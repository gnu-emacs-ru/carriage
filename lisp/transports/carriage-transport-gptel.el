;;; carriage-transport-gptel.el --- gptel transport adapter  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (gptel "0.1"))
;; Version: 0.1
;; Keywords: transport, gptel
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/llm-transport-v2.org
;;   spec/tool-contracts-v2.org
;;   spec/logging-v2.org
;;   spec/observability-v2.org
;;   spec/golden-fixtures-v2.org
;;
;;; Commentary:
;; Adapter for streaming gptel backends.
;;
;;; Code:

;; Minimal reference adapter that maps Carriage transport events to gptel-request.
;; - Streams chunks to *carriage-traffic* and accumulates response.
;; - On completion feeds accumulated text to carriage-accept-llm-response.
;; - Registers an abort handler that stops the underlying gptel request.

(require 'cl-lib)
(require 'subr-x)
(require 'gptel nil t)        ;; optional at load time; allow tests without gptel
(require 'carriage-logging)
(require 'carriage-ui)
(require 'carriage-transport)
(require 'carriage-errors)
(require 'carriage-llm-registry)
;; Avoid hard dependency on carriage-mode to break cycles at load time.
(declare-function carriage-register-abort-handler "carriage-mode" (fn))
(declare-function carriage-insert-stream-chunk "carriage-mode" (string &optional type))
(declare-function carriage-begin-reasoning "carriage-mode" ())
(declare-function carriage-end-reasoning "carriage-mode" ())
(declare-function carriage-stream-finalize "carriage-mode" (&optional errorp mark-last-iteration))

(defgroup carriage-transport-gptel nil
  "GPTel transport adapter for Carriage."
  :group 'carriage)

(defun carriage--gptel--normalize-model (model)
  "Return a symbol for GPTel's `gptel-model' from MODEL string/symbol.

Accepts forms:
- \"gptel:PROVIDER:MODEL\" → MODEL
- \"PROVIDER:MODEL\"       → MODEL
- \"MODEL\"                → MODEL
- symbol → as-is

If MODEL cannot be interned meaningfully, return it unchanged."
  (cond
   ((symbolp model) model)
   ((stringp model)
    (let* ((last (car (last (split-string model ":" t)))))
      (if (and last (not (string-empty-p last)))
          (intern last)
        (intern model))))
   (t model)))

(defun carriage--gptel--strip-carriage-markers (text)
  "Remove Carriage doc-state and per-send marker lines from TEXT (best-effort).

This is a hard safety belt: even if callers pass a pre-built prompt that still
contains markers, transports must never leak them to the LLM."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let ((case-fold-search t))
      ;; Legacy/state blocks
      (while (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t)
        (let ((beg (match-beginning 0)))
          (if (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)
              (let ((end (line-end-position)))
                (delete-region beg end)
                (when (looking-at "\n") (delete-char 1)))
            (delete-region beg (point-max)))))
      ;; Doc-state and iteration id in Org property headers
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+\\(CARRIAGE_STATE\\|CARRIAGE_ITERATION_ID\\)\\b.*$" nil t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))
        (goto-char (line-beginning-position)))
      ;; Per-send inline markers
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+\\(CARRIAGE_FINGERPRINT\\|CARRIAGE_ITERATION_ID\\)\\b.*$" nil t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))
        (goto-char (line-beginning-position))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun carriage--gptel--prompt (source buffer mode)
  "Build prompt string for GPTel from SOURCE and BUFFER in MODE.
Strips any #+begin_carriage … #+end_carriage blocks from the outgoing text."
  (with-current-buffer buffer
    (let* ((raw
            (pcase source
              ('subtree
               (if (eq mode 'org-mode)
                   (save-excursion
                     (require 'org)
                     (ignore-errors (org-back-to-heading t))
                     (let ((beg (save-excursion (org-back-to-heading t) (point)))
                           (end (save-excursion (org-end-of-subtree t t) (point))))
                       (buffer-substring-no-properties beg end)))
                 (buffer-substring-no-properties (point-min) (point-max))))
              (_ (buffer-substring-no-properties (point-min) (point-max))))))
      (carriage--gptel--strip-carriage-markers raw))))

(defun carriage--gptel--maybe-open-logs ()
  "Open log/traffic buffers if user prefs demand and we are interactive."
  (when (and (boundp 'carriage-mode-auto-open-log)
             carriage-mode-auto-open-log
             (not (bound-and-true-p noninteractive)))
    (ignore-errors (carriage-show-log)))
  (when (and (boundp 'carriage-mode-auto-open-traffic)
             carriage-mode-auto-open-traffic
             (not (bound-and-true-p noninteractive)))
    (ignore-errors (carriage-show-traffic))))

(defun carriage--gptel--classify (response)
  "Normalize GPTel RESPONSE into a plist with :kind and payload.
Kinds: 'text 'reasoning 'both 'reasoning-end 'tool 'done 'abort 'unknown.
When both reasoning and text are present, returns :kind 'both with keys :thinking and :text."
  (cond
   ;; Plain text piece
   ((stringp response)
    (list :kind 'text :text response))
   ;; End
   ((eq response t)
    (list :kind 'done))
   ;; Abort/error
   ((or (null response) (eq response 'abort))
    (list :kind 'abort))
   ;; Reasoning stream (classic pair: (reasoning . CHUNK|t))
   ((and (consp response) (eq (car response) 'reasoning))
    (let ((chunk (cdr response)))
      (cond
       ((eq chunk t) (list :kind 'reasoning-end))
       ((stringp chunk) (list :kind 'reasoning :text chunk))
       (t (list :kind 'unknown)))))
   ;; Tool events
   ((and (consp response) (memq (car response) '(tool-call tool-result)))
    (list :kind 'tool))
   ;; Plist/alist with :thinking and/or :content/:delta
   ((listp response)
    (let* ((th  (or (plist-get response :thinking)
                    (and (fboundp 'alist-get) (alist-get :thinking response))))
           (txt (or (plist-get response :content)
                    (plist-get response :delta)
                    (and (fboundp 'alist-get) (alist-get :content response))
                    (and (fboundp 'alist-get) (alist-get :delta response)))))
      (cond
       ;; End of reasoning
       ((eq th t) (list :kind 'reasoning-end))
       ;; Both thinking and text present
       ((and (stringp th) (stringp txt))
        (list :kind 'both :thinking th :text txt))
       ;; Only thinking
       ((stringp th)
        (list :kind 'reasoning :text th))
       ;; Only text
       ((stringp txt)
        (list :kind 'text :text txt))
       (t (list :kind 'unknown)))))
   (t (list :kind 'unknown))))

(defun carriage--gptel--ensure-backend (backend buffer)
  "Ensure BACKEND is 'gptel or signal error and complete in BUFFER."
  (unless (memq (if (symbolp backend) backend (intern (format "%s" backend)))
                '(gptel))
    (carriage-log "Transport[gptel]: backend mismatch (%s), dropping" backend)
    (with-current-buffer buffer
      (condition-case _
          (signal (carriage-error-symbol 'LLM_E_BACKEND)
                  (list (format "Unknown transport backend: %s" backend)))
        (error nil))
      (carriage-transport-complete t))
    (user-error "No transport adapter for backend: %s" backend))
  t)

(defun carriage--gptel--state-waiting (buffer model provider)
  "Set UI waiting state for BUFFER and initialize progress metadata."
  (with-current-buffer buffer
    (carriage-ui-set-state 'waiting)
    (ignore-errors
      (carriage-ui-note-stream-progress
       (list :model (format "%s" model)
             :provider provider
             :time-start (float-time)
             :time-last (float-time))))))

(defun carriage--gptel--register-abort (buffer)
  "Register abort handler for ongoing GPTel request tied to BUFFER."
  (with-current-buffer buffer
    (carriage-register-abort-handler
     (lambda ()
       (condition-case e
           (gptel-abort buffer)
         (error (carriage-log "Transport[gptel]: abort error: %s"
                              (error-message-string e))))))))

(defun carriage--gptel--open-and-log (buffer args prompt system model)
  "Prepare UI/logging for BUFFER and record outbound request."
  (with-current-buffer buffer
    (carriage--gptel--maybe-open-logs))
  (carriage--gptel--register-abort buffer)
  (carriage-traffic-log-request buffer
                                :backend 'gptel
                                :model model
                                :system system
                                :prompt prompt
                                :context (plist-get args :context))
  (carriage-traffic-log 'out "gptel request: model=%s source=%s bytes=%d"
                        model (plist-get args :source) (length prompt))
  (carriage--gptel--state-waiting buffer model (plist-get args :provider)))

(defun carriage--gptel--summary-init ()
  "Initialize response summary accumulator."
  (list :head "" :tail "" :bytes 0))

(defun carriage--gptel--summary-update (summary text head-limit tail-limit)
  "Update SUMMARY with TEXT using HEAD-LIMIT and TAIL-LIMIT.
Returns cons (UPDATED-SUMMARY . REMAINDER-TEXT-AFTER-HEAD)."
  (let* ((head (or (plist-get summary :head) ""))
         (tail (or (plist-get summary :tail) ""))
         (bytes (or (plist-get summary :bytes) 0))
         (s (or text "")))
    (setq bytes (+ bytes (string-bytes s)))
    (when (< (length head) head-limit)
      (let* ((need (max 0 (- head-limit (length head))))
             (take (min need (length s))))
        (setq head (concat head (substring s 0 take)))
        (setq s (substring s take))))
    (when (> (length s) 0)
      (let* ((concatd (concat tail s))
             (len (length concatd)))
        (setq tail (if (<= len tail-limit)
                       concatd
                     (substring concatd (- len tail-limit) len)))))
    (cons (list :head head :tail tail :bytes bytes) s)))

(defun carriage--gptel--summary-string (summary)
  "Return compact summary string for SUMMARY plist."
  (let ((head (or (plist-get summary :head) ""))
        (tail (or (plist-get summary :tail) ""))
        (bytes (or (plist-get summary :bytes) 0)))
    (concat head
            (when (> bytes (+ (string-bytes head) (string-bytes tail)))
              "…")
            tail)))

(defun carriage--gptel--on-done (buffer summary)
  "Handle completion: log summary and finalize BUFFER."
  (carriage-traffic-log 'in "gptel: done")
  (with-current-buffer buffer
    (ignore-errors (carriage-ui-note-stream-progress (list :time-last (float-time)))))
  (carriage-traffic-log-response-summary buffer (carriage--gptel--summary-string summary))
  (with-current-buffer buffer
    (carriage-stream-finalize nil t)
    (carriage-transport-complete nil buffer)))

(defun carriage--gptel--on-abort (buffer info summary)
  "Handle abort/error with INFO: log and finalize BUFFER with error, include SUMMARY."
  (let ((msg (or (plist-get info :status) "error/abort")))
    (carriage-traffic-log 'in "gptel: %s" msg)
    (with-current-buffer buffer
      (ignore-errors
        (carriage-ui-note-error
         (list :code 'LLM_ABORT :message msg :source 'transport)))))
  (carriage-traffic-log-response-summary buffer (carriage--gptel--summary-string summary))
  (with-current-buffer buffer
    (carriage-stream-finalize t nil)
    (carriage-transport-complete t buffer)))

(defun carriage--gptel--cb--ensure-reasoning (gptel-buffer state)
  "Ensure UI reasoning state on first event. Return updated STATE."
  (if (plist-get state :first-event)
      (progn
        (with-current-buffer gptel-buffer
          (carriage-ui-set-state 'reasoning))
        (plist-put state :first-event nil))
    state))

(defun carriage--gptel--cb--ensure-streaming (gptel-buffer state)
  "Ensure streaming state on first event. Return updated STATE."
  (if (plist-get state :first-event)
      (progn
        (with-current-buffer gptel-buffer
          (carriage-transport-streaming))
        (plist-put state :first-event nil))
    state))

(defun carriage--gptel--cb--update-summary (state text)
  "Update STATE's summary with TEXT. Returns cons (NEW-STATE . REMAINDER)."
  (let* ((hl (or (plist-get state :head-limit) 4096))
         (tl (or (plist-get state :tail-limit) 4096))
         (sum (or (plist-get state :summary) (carriage--gptel--summary-init)))
         (upd (carriage--gptel--summary-update sum (or text "") hl tl)))
    (cons (plist-put state :summary (car upd)) (cdr upd))))

(defun carriage--gptel--cb-handle-reasoning (gptel-buffer state text)
  "Handle reasoning CHUNK TEXT. Return updated STATE."
  (setq state (carriage--gptel--cb--ensure-reasoning gptel-buffer state))
  (if (plist-get state :any-text-seen)
      (carriage-traffic-log 'in "[reasoning] %s" (or text ""))
    (with-current-buffer gptel-buffer
      (carriage-insert-stream-chunk (or text "") 'reasoning)
      (ignore-errors (carriage-ui-note-reasoning-chunk (or text "")))))
  state)

(defun carriage--gptel--cb-handle-both (gptel-buffer state thinking text)
  "Handle event with both THINKING and TEXT. Return updated STATE."
  (setq state (carriage--gptel--cb--ensure-reasoning gptel-buffer state))
  (when (and (stringp thinking) (not (plist-get state :any-text-seen)))
    (with-current-buffer gptel-buffer
      (carriage-insert-stream-chunk thinking 'reasoning))
    (carriage-traffic-log 'in "[reasoning] %s" thinking))
  (when (stringp text)
    (setq state (carriage--gptel--cb--ensure-streaming gptel-buffer state))
    (with-current-buffer gptel-buffer
      (carriage-insert-stream-chunk text 'text)
      (ignore-errors
        (carriage-ui-note-stream-progress (list :inc-chunk t :time-last (float-time))))))
  (let* ((res (carriage--gptel--cb--update-summary state text)))
    (setq state (car res))
    (let ((remainder (cdr res)))
      (setq state (plist-put state :any-text-seen t))
      (carriage-traffic-log 'in "%s" remainder)))
  state)

(defun carriage--gptel--cb-handle-reasoning-end (gptel-buffer state)
  "Handle end of reasoning. Return STATE unchanged."
  (with-current-buffer gptel-buffer
    (ignore-errors (carriage-end-reasoning)))
  (carriage-traffic-log 'in "[reasoning] end")
  state)

(defun carriage--gptel--cb-handle-text (gptel-buffer state text)
  "Handle plain TEXT chunk. Return updated STATE."
  (setq state (carriage--gptel--cb--ensure-streaming gptel-buffer state))
  (when (stringp text)
    (with-current-buffer gptel-buffer
      (carriage-insert-stream-chunk text 'text)
      (ignore-errors
        (carriage-ui-note-stream-progress (list :inc-chunk t :time-last (float-time))))))
  (let* ((res (carriage--gptel--cb--update-summary state text)))
    (setq state (car res))
    (let ((remainder (cdr res)))
      (setq state (plist-put state :any-text-seen t))
      (when (stringp remainder)
        (carriage-traffic-log 'in "%s" remainder))))
  state)

(defun carriage--gptel--cb-handle-tool (state)
  "Handle tool events. Return STATE unchanged."
  (carriage-traffic-log 'in "[tool] ...")
  state)

(defun carriage--gptel--make-callback (gptel-buffer args head-limit tail-limit)
  "Return GPTel stream callback for GPTEL-BUFFER with limits."
  (let ((state (list :first-event t
                     :any-text-seen nil
                     :summary (carriage--gptel--summary-init)
                     :head-limit head-limit
                     :tail-limit tail-limit)))
    (lambda (response info)
      (condition-case qerr
          (let* ((cls (carriage--gptel--classify response))
                 (kind (plist-get cls :kind))
                 (text (plist-get cls :text))
                 (thinking (plist-get cls :thinking)))
            (pcase kind
              ('reasoning
               (setq state (carriage--gptel--cb-handle-reasoning gptel-buffer state text)))
              ('both
               (setq state (carriage--gptel--cb-handle-both gptel-buffer state thinking text)))
              ('reasoning-end
               (setq state (carriage--gptel--cb-handle-reasoning-end gptel-buffer state)))
              ('text
               (setq state (carriage--gptel--cb-handle-text gptel-buffer state text)))
              ('tool
               (setq state (carriage--gptel--cb-handle-tool state)))
              ('done
               (carriage--gptel--on-done gptel-buffer (plist-get state :summary)))
              ('abort
               (carriage--gptel--on-abort gptel-buffer info (plist-get state :summary)))
              (_
               (carriage-traffic-log 'in "[unknown] %S" response))))
        (error
         (carriage-traffic-log 'in "gptel callback error: %s" (error-message-string qerr))
         (carriage--gptel--on-abort gptel-buffer (list :status (error-message-string qerr))
                                    (plist-get state :summary)))))))

(defun carriage-transport-gptel-dispatch (&rest args)
  "Dispatch Carriage request via GPTel when backend is 'gptel.

ARGS is a plist with at least :backend, :model, :source, :buffer, :mode.
Optionally accepts :prompt and :system to override prompt construction.
When :insert-marker is a live marker, insert accepted blocks at its position.
Streams chunks to *carriage-traffic*, accumulates text, and on completion
invokes =carriage-accept-llm-response' in the originating buffer/marker.

On backend mismatch, logs and completes with error."
  (let* ((backend (plist-get args :backend))
         (model   (plist-get args :model))
         (source  (plist-get args :source))
         (buffer  (or (plist-get args :buffer) (current-buffer)))
         (mode    (or (plist-get args :mode)
                      (buffer-local-value 'major-mode buffer)))
         (ins-marker (plist-get args :insert-marker)))
    (carriage--gptel--ensure-backend backend buffer)
    (let* ((prompt (or (plist-get args :prompt)
                       (carriage--gptel--prompt source buffer (intern (format "%s" mode)))))
           (system (plist-get args :system))
           (gptel-buffer buffer)
           (prov (or (plist-get args :provider)
                     (and (boundp 'carriage-mode-provider) carriage-mode-provider)))
           (resolved (ignore-errors (carriage-llm-resolve-model 'gptel prov model)))
           (gptel-model (carriage--gptel--normalize-model (or resolved model)))
           (head-limit (or (and (boundp 'carriage-traffic-summary-head-bytes)
                                carriage-traffic-summary-head-bytes) 4096))
           (tail-limit (or (and (boundp 'carriage-traffic-summary-tail-bytes)
                                carriage-traffic-summary-tail-bytes) 4096)))
      (carriage--gptel--open-and-log gptel-buffer args prompt system gptel-model)
      (condition-case err
          (gptel-request prompt
            :system system
            :stream t
            :callback (carriage--gptel--make-callback gptel-buffer args head-limit tail-limit))
        (error
         (carriage-traffic-log 'in "gptel: request error: %s" (error-message-string err))
         (carriage--gptel--on-abort gptel-buffer (list :status (error-message-string err)) (carriage--gptel--summary-init)))))))

;; Entry-point: carriage-transport-gptel-dispatch

;; Assist (schema-locked) minimal APIs and validators
(require 'carriage-assist)

(provide 'carriage-transport-gptel)
;;; carriage-transport-gptel.el ends here
