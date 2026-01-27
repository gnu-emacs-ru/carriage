;;; carriage-transport-gptel.el --- GPTel transport adapter (v2)  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.2
;; Keywords: transport, llm, gptel
;;
;; Specifications:
;;   spec/llm-transport-v2.org
;;   spec/errors-v2.org
;;   spec/logging-v2.org
;;   spec/security-v2.org
;;   spec/observability-v2.org
;;
;;; Commentary:
;; Small, predictable GPTel transport for Carriage.
;;
;; Design principles:
;; - Hot path is callback-driven only (no keepalive, no SSE trailer parsing, no advices).
;; - Self-healing: watchdog timeout + abort + cleanup.
;; - Diagnostics: on every finalize we log response/info shape and process inventory.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-ui)
(require 'carriage-transport)

;; Avoid hard dependency cycle with carriage-mode at load time.
(declare-function carriage-register-abort-handler "carriage-mode" (fn))
(declare-function carriage-insert-stream-chunk "carriage-mode" (string &optional type))
(declare-function carriage-stream-finalize "carriage-mode" (&optional errorp mark-last-iteration))
(declare-function carriage-fingerprint-note-usage-and-cost "carriage-mode" (usage &optional backend provider model))

;; GPTel entry-points
(declare-function gptel-request "gptel-request" (&optional prompt &rest args))
(declare-function gptel-abort "gptel-request" (buf))
(declare-function gptel-fsm-info "gptel-request" (fsm))

(defgroup carriage-transport-gptel nil
  "GPTel transport adapter (v2) for Carriage."
  :group 'carriage)

(defcustom carriage-transport-gptel-timeout-seconds 30
  "Watchdog startup timeout in seconds.
If no callback activity is observed for this long, the request is treated as hung:
we log TIMEOUT, call `gptel-abort' and perform cleanup.

Note: after the first streamed content chunk arrives, the transport MAY extend
the watchdog to a longer \"silence timeout\" to avoid aborting valid long generations."
  :type 'number
  :group 'carriage-transport-gptel)

(defcustom carriage-transport-gptel-cleanup-delay 0.2
  "Delay (seconds) before the second cleanup pass after abort/error."
  :type 'number
  :group 'carriage-transport-gptel)

(defcustom carriage-transport-gptel-cleanup-policy 'aggressive-on-error
  "Process cleanup policy for GPTel transport.

- nil: never delete processes (only call gptel-abort on errors).
- aggressive-on-error: on ERROR/ABORT/TIMEOUT, delete all live processes whose
  names start with \"gptel-curl\".

NOTE: This is global (may kill non-Carriage GPTel requests). This is allowed
by the user policy \"reliability over isolation\", and MUST be logged."
  :type '(choice (const :tag "No process deletion" nil)
                 (const :tag "Kill all gptel-curl on error/abort/timeout" aggressive-on-error))
  :group 'carriage-transport-gptel)

(defcustom carriage-transport-gptel-diagnostics t
  "When non-nil, log diagnostic details on finalize, including snippets and process inventory."
  :type 'boolean
  :group 'carriage-transport-gptel)

;; ---------------------------------------------------------------------
;; Utilities

(defvar carriage-transport-gptel--seq 0)

(defun carriage-transport-gptel--new-id ()
  "Return a new request id string."
  (format "gptel-%d" (cl-incf carriage-transport-gptel--seq)))

(defun carriage-transport-gptel--plist-keys (pl)
  "Return keys of plist PL (best-effort)."
  (when (listp pl)
    (let ((keys '()) (tail pl) (n 0))
      (while (and (consp tail) (< n 40))
        (let ((k (car tail)))
          (when (or (keywordp k) (symbolp k))
            (push k keys)))
        (setq tail (cddr tail))
        (setq n (1+ n)))
      (nreverse keys))))

(defun carriage-transport-gptel--snip (obj &optional max)
  "Return a clipped printed representation of OBJ."
  (let* ((max (or max 600))
         (s (condition-case _e
                (cond
                 ((stringp obj) obj)
                 (t (prin1-to-string obj)))
              (error (format "%S" obj)))))
    (if (> (length s) max)
        (concat (substring s 0 max) "…")
      s)))

(defun carriage-transport-gptel--response-kind (resp)
  "Return short kind name for RESP."
  (cond
   ((eq resp t) "t")
   ((null resp) "nil")
   ((stringp resp) "string")
   ((symbolp resp) (format "sym:%s" resp))
   ((consp resp) "cons")
   (t (format "%s" (type-of resp)))))

(defun carriage-transport-gptel--live-gptel-curl-procs ()
  "Return list of live processes whose names start with \"gptel-curl\"."
  (cl-loop for p in (process-list)
           when (and (processp p)
                     (memq (process-status p) '(run open connect))
                     (string-prefix-p "gptel-curl" (process-name p)))
           collect p))

(defun carriage-transport-gptel--proc-desc (p)
  "Return one-line description for process P."
  (format "%s pid=%s status=%s buf=%s"
          (process-name p)
          (or (ignore-errors (process-id p)) "?")
          (process-status p)
          (let ((b (process-buffer p)))
            (if (buffer-live-p b) (buffer-name b) "-"))))

(defun carriage-transport-gptel--log-procs (tag id procs)
  "Log process inventory PROCS under TAG."
  (when carriage-transport-gptel-diagnostics
    (carriage-log "Transport[gptel] %s id=%s procs=%d%s"
                  tag id (length procs)
                  (if (null procs) "" ":"))
    (dolist (p procs)
      (carriage-log "Transport[gptel] %s id=%s - %s"
                    tag id (carriage-transport-gptel--proc-desc p)))))

(defun carriage-transport-gptel--maybe-gptel-request-alist-size ()
  "Return length of gptel--request-alist when available, else nil."
  (when (boundp 'gptel--request-alist)
    (condition-case _e
        (length gptel--request-alist)
      (error nil))))

(defun carriage-transport-gptel--gptel-fsm-for-buffer (buf)
  "Return the first live GPTel FSM whose :buffer equals BUF, or nil (best-effort)."
  (when (and (buffer-live-p buf)
             (boundp 'gptel--request-alist)
             (listp gptel--request-alist))
    (cl-loop
     for entry in gptel--request-alist
     for fsm = (ignore-errors (cadr entry))
     for info = (and fsm (ignore-errors (gptel-fsm-info fsm)))
     when (and (listp info) (eq (plist-get info :buffer) buf))
     return fsm)))

(defun carriage-transport-gptel--gptel-http-status-known-p (buf)
  "Return non-nil when GPTel already knows HTTP status for BUF (best-effort)."
  (when-let* ((fsm (carriage-transport-gptel--gptel-fsm-for-buffer buf))
              (info (ignore-errors (gptel-fsm-info fsm))))
    (and (listp info)
         (or (plist-get info :http-status)
             (plist-get info :http_status)
             (plist-get info :status)))))

(defun carriage-transport-gptel--cleanup-request-alist (origin-buffer id)
  "Remove any gptel--request-alist entries whose FSM belongs to ORIGIN-BUFFER.

This is a scoped cleanup to avoid \"dirty\" GPTel state after early aborts.
Returns number of removed entries (best-effort)."
  (if (not (and (buffer-live-p origin-buffer) (boundp 'gptel--request-alist)))
      0
    (let ((removed 0)
          (before (ignore-errors (length gptel--request-alist))))
      (dolist (entry (copy-sequence gptel--request-alist))
        (let* ((proc (car entry))
               (fsm (ignore-errors (cadr entry)))
               (info (and fsm (ignore-errors (gptel-fsm-info fsm))))
               (buf (and (listp info) (plist-get info :buffer))))
          (when (eq buf origin-buffer)
            (ignore-errors
              (setf (alist-get proc gptel--request-alist nil 'remove) nil))
            (setq removed (1+ removed)))))
      (when (and carriage-transport-gptel-diagnostics (> removed 0))
        (carriage-log "Transport[gptel] CLEAN id=%s removed-request-alist=%d before=%s after=%s"
                      id
                      removed
                      (if (numberp before) before "—")
                      (ignore-errors (length gptel--request-alist))))
      removed)))

(defun carriage-transport-gptel--classify-final (resp info watchdog-fired)
  "Return classification code symbol for finalization."
  (cond
   (watchdog-fired 'LLM_E_TIMEOUT)
   ((eq resp 'abort) 'LLM_E_ABORT)
   ((and (null resp) (listp info) (or (plist-get info :error) (plist-get info :http-status)))
    'LLM_E_PROVIDER)
   ((null resp) 'LLM_E_NETWORK)
   (t 'LLM_E_UNKNOWN)))

(defun carriage-transport-gptel--maybe-backtrace-string ()
  "Return a best-effort backtrace string."
  (condition-case _e
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (backtrace))
        (buffer-string))
    (error nil)))

(defun carriage-transport-gptel--cleanup-kill-procs (procs)
  "Delete PROCS best-effort and return count killed."
  (let ((k 0))
    (dolist (p procs)
      (when (process-live-p p)
        (ignore-errors (delete-process p))
        (setq k (1+ k))))
    k))

(defun carriage-transport-gptel--schedule (delay fn)
  "Run FN after DELAY seconds (best-effort)."
  (run-at-time (max 0 (or delay 0)) nil fn))

;; ---------------------------------------------------------------------
;; Watchdog / finalization guards (buffer-local)

(defvar-local carriage-transport-gptel--finalized nil
  "Non-nil once this buffer's current GPTel request has been finalized by the transport.")

(defvar-local carriage-transport-gptel--finalized-kind nil
  "Final kind symbol used for the last finalize in this buffer.")

(defvar-local carriage-transport-gptel--wd-timer nil)
(defvar-local carriage-transport-gptel--wd-last-activity 0.0)
(defvar-local carriage-transport-gptel--wd-fired nil)
(defvar-local carriage-transport-gptel--wd-timeout nil
  "Current watchdog timeout (seconds).
Starts as startup timeout, then may be extended after first stream content arrives.")
(defvar-local carriage-transport-gptel--wd-extended nil
  "Non-nil once watchdog timeout has been extended after first stream content arrives.")

(defun carriage-transport-gptel--wd-stop ()
  "Stop watchdog timer."
  (when (timerp carriage-transport-gptel--wd-timer)
    (cancel-timer carriage-transport-gptel--wd-timer))
  (setq carriage-transport-gptel--wd-timer nil))

(defun carriage-transport-gptel--wd-touch ()
  "Record activity timestamp."
  (setq carriage-transport-gptel--wd-last-activity (float-time)))

(defun carriage-transport-gptel--wd-extended-timeout ()
  "Return silence timeout (seconds) to use after first stream content arrives.

Policy: keep only one user-facing knob (`carriage-transport-gptel-timeout-seconds`)
as the startup timeout, but never let the silence timeout drop below 60s to avoid
aborting valid long generations."
  (max (float (or carriage-transport-gptel-timeout-seconds 10)) 60.0))

(defun carriage-transport-gptel--wd-extend ()
  "Extend watchdog timeout after first stream content arrives (idempotent)."
  (unless carriage-transport-gptel--wd-extended
    (setq carriage-transport-gptel--wd-extended t
          carriage-transport-gptel--wd-timeout (carriage-transport-gptel--wd-extended-timeout))
    (when carriage-transport-gptel-diagnostics
      (carriage-log "Transport[gptel] WD: extend silence-timeout=%.3fs"
                    carriage-transport-gptel--wd-timeout))))

(defun carriage-transport-gptel--wd-start (origin-buffer id on-timeout)
  "Start watchdog for ORIGIN-BUFFER.

ON-TIMEOUT is a zero-arg function called when the watchdog fires.
It MUST be idempotent."
  (with-current-buffer origin-buffer
    (setq carriage-transport-gptel--wd-fired nil
          carriage-transport-gptel--wd-extended nil
          carriage-transport-gptel--wd-timeout
          (max 0.1 (float (or carriage-transport-gptel-timeout-seconds 10))))
    (carriage-transport-gptel--wd-stop)
    (carriage-transport-gptel--wd-touch)
    (let ((buf origin-buffer))
      (setq carriage-transport-gptel--wd-timer
            (run-at-time
             0.5 0.5
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let* ((timeout (or carriage-transport-gptel--wd-timeout
                                       (max 0.1 (float (or carriage-transport-gptel-timeout-seconds 30))))))
                     ;; If GPTel already observed HTTP response headers/status for this buffer,
                     ;; treat it as activity and switch to (longer) silence-timeout.
                     (when (and (not carriage-transport-gptel--finalized)
                                (carriage-transport-gptel--gptel-http-status-known-p buf))
                       (carriage-transport-gptel--wd-touch)
                       (carriage-transport-gptel--wd-extend))
                     (let ((dt (- (float-time) (or carriage-transport-gptel--wd-last-activity 0.0))))
                       (when (and (not carriage-transport-gptel--wd-fired)
                                  (not carriage-transport-gptel--finalized)
                                  (> dt timeout))
                         (setq carriage-transport-gptel--wd-fired t)
                         (carriage-log "Transport[gptel] TIMEOUT id=%s phase=%s dt=%.3fs timeout=%.3fs"
                                       id
                                       (if carriage-transport-gptel--wd-extended "silence" "startup")
                                       dt timeout)
                         (ignore-errors (funcall on-timeout)))))))))))))

;; ---------------------------------------------------------------------
;; Finalization + cleanup

(defun carriage-transport-gptel--finalize
    (origin-buffer id final resp info &optional internal-error)
  "Finalize request and update UI. FINAL is one of 'done 'abort 'error 'timeout."
  (with-current-buffer origin-buffer
    (carriage-transport-gptel--wd-stop)
    (if carriage-transport-gptel--finalized
        (progn
          (when carriage-transport-gptel-diagnostics
            (carriage-log "Transport[gptel] DUP-FINALIZE id=%s final=%s ignored"
                          id final))
          t)
      (setq carriage-transport-gptel--finalized t
            carriage-transport-gptel--finalized-kind final)
      (let* ((watchdog-fired (and (boundp 'carriage-transport-gptel--wd-fired)
                                  carriage-transport-gptel--wd-fired))
             (code (if internal-error
                       'LLM_E_INTERNAL
                     (carriage-transport-gptel--classify-final resp info watchdog-fired)))
             (status (and (listp info) (plist-get info :status)))
             (http (and (listp info) (or (plist-get info :http-status)
                                         (plist-get info :http_status))))
             (reqn (carriage-transport-gptel--maybe-gptel-request-alist-size))
             (resp-kind (carriage-transport-gptel--response-kind resp))
             (procs0 (carriage-transport-gptel--live-gptel-curl-procs))
             (abort-called nil)
             (killed0 0)
             (killed1 0))
        ;; Always log a single END line with key attributes
        (carriage-log
         "Transport[gptel] END id=%s final=%s code=%s status=%s http=%s resp=%s req-alist=%s procs0=%d"
         id final code
         (if status (carriage-transport-gptel--snip status 200) "—")
         (if http (format "%s" http) "—")
         resp-kind
         (if (numberp reqn) (number-to-string reqn) "—")
         (length procs0))

        (when (and carriage-transport-gptel-diagnostics
                   (memq final '(abort error timeout)))
          (carriage-log "Transport[gptel] LAST id=%s resp=%s" id
                        (carriage-transport-gptel--snip resp 600))
          (carriage-log "Transport[gptel] LAST id=%s info-keys=%S" id
                        (carriage-transport-gptel--plist-keys info))
          (carriage-log "Transport[gptel] LAST id=%s info=%s" id
                        (carriage-transport-gptel--snip info 600)))

        (when internal-error
          (carriage-log "Transport[gptel] INTERNAL id=%s err=%s" id
                        (carriage-transport-gptel--snip internal-error 600))
          (when-let* ((bt (carriage-transport-gptel--maybe-backtrace-string)))
            (carriage-log "Transport[gptel] INTERNAL id=%s backtrace:\n%s" id bt)))

        ;; Process handling: only on non-successful finals
        (when (and (eq carriage-transport-gptel-cleanup-policy 'aggressive-on-error)
                   (memq final '(abort error timeout)))
          (setq abort-called t)
          (ignore-errors (gptel-abort origin-buffer))
          ;; Scoped cleanup: remove any gptel request state tied to this origin buffer.
          (ignore-errors (carriage-transport-gptel--cleanup-request-alist origin-buffer id))
          (carriage-transport-gptel--log-procs "PROCS-BEFORE" id procs0)
          (setq killed0 (carriage-transport-gptel--cleanup-kill-procs procs0))
          ;; second pass after delay
          (carriage-transport-gptel--schedule
           (or carriage-transport-gptel-cleanup-delay 0.2)
           (lambda ()
             (when (buffer-live-p origin-buffer)
               (with-current-buffer origin-buffer
                 (let ((procs1 (carriage-transport-gptel--live-gptel-curl-procs)))
                   (setq killed1 (carriage-transport-gptel--cleanup-kill-procs procs1))
                   (ignore-errors (carriage-transport-gptel--cleanup-request-alist origin-buffer id))
                   (carriage-transport-gptel--log-procs "PROCS-AFTER" id
                                                        (carriage-transport-gptel--live-gptel-curl-procs))
                   (carriage-log "Transport[gptel] ACTION id=%s abort=%s kill0=%d kill1=%d"
                                 id (if abort-called "t" "nil") killed0 killed1)))))))

        ;; Finalize stream and transport UI
        (with-demoted-errors "carriage-stream-finalize error: %S"
          (pcase final
            ('done (carriage-stream-finalize nil t))
            (_     (carriage-stream-finalize t nil))))
        (carriage-transport-complete (memq final '(abort error timeout)) origin-buffer)
        t))))

;; ---------------------------------------------------------------------
;; Callback

(defun carriage-transport-gptel--make-callback (origin-buffer id)
  "Return GPTel callback for ORIGIN-BUFFER."
  (let ((finished nil)
        (first-stream nil)
        (last-info nil)
        (last-resp nil))
    (lambda (resp info)
      (with-current-buffer origin-buffer
        (carriage-transport-gptel--wd-touch))
      (setq last-info info)
      (setq last-resp resp)
      (condition-case err
          (progn
            (when finished
              ;; Late callback after finalize: never signal.
              (when carriage-transport-gptel-diagnostics
                (carriage-log "Transport[gptel] LATE id=%s resp=%s"
                              id (carriage-transport-gptel--snip resp 200)))
              ;; Return quietly, do not throw/catch.
              nil)

            (cond
             ;; Stream text
             ((stringp resp)
              (unless first-stream
                (setq first-stream t)
                (with-current-buffer origin-buffer
                  (carriage-transport-gptel--wd-extend))
                (carriage-transport-streaming origin-buffer))
              (with-current-buffer origin-buffer
                (carriage-insert-stream-chunk resp 'text)))

             ;; Reasoning stream (best-effort, use carriage-mode policy at insertion layer)
             ((and (consp resp) (eq (car resp) 'reasoning))
              (let ((val (cdr resp)))
                (cond
                 ((stringp val)
                  (unless first-stream
                    (setq first-stream t)
                    (carriage-transport-streaming origin-buffer))
                  (with-current-buffer origin-buffer
                    (carriage-insert-stream-chunk val 'reasoning)))
                 ((eq val t)
                  ;; end reasoning marker: insertion layer handles closing
                  nil)
                 (t nil))))

             ;; Done
             ((eq resp t)
              (setq finished t)
              (carriage-transport-gptel--finalize origin-buffer id 'done resp info))

             ;; Abort (explicit)
             ((eq resp 'abort)
              (setq finished t)
              (carriage-transport-gptel--finalize origin-buffer id 'abort resp info))

             ;; Error (nil response)
             ((null resp)
              (setq finished t)
              (carriage-transport-gptel--finalize origin-buffer id 'error resp info))

             ;; Unknown event kind: log and ignore (do not crash)
             (t
              (when carriage-transport-gptel-diagnostics
                (carriage-log "Transport[gptel] UNKNOWN id=%s resp=%s info=%s"
                              id
                              (carriage-transport-gptel--snip resp 400)
                              (carriage-transport-gptel--snip info 400)))
              nil)))
        (error
         ;; Internal crash in our callback handler
         (setq finished t)
         (carriage-transport-gptel--finalize
          origin-buffer id 'error last-resp last-info err))))))

;; ---------------------------------------------------------------------
;; Dispatch entry-point

;;;###autoload
(defun carriage-transport-gptel-v2-dispatch (&rest args)
  "Dispatch Carriage request via GPTel.

ARGS is a plist with keys like:
:backend :model :source :buffer :mode :prompt :system :insert-marker

This implementation is minimal and callback-driven, with watchdog + cleanup."
  (let* ((backend (plist-get args :backend))
         (model   (plist-get args :model))
         (source  (or (plist-get args :source) 'buffer))
         (buffer  (or (plist-get args :buffer) (current-buffer)))
         (prompt  (plist-get args :prompt))
         (system  (plist-get args :system))
         (_mode   (plist-get args :mode))
         (id      (carriage-transport-gptel--new-id)))
    (unless (eq (if (symbolp backend) backend (intern (format "%s" backend))) 'gptel)
      (carriage-log "Transport[gptel] backend mismatch: %S" backend)
      (carriage-transport-complete t buffer)
      (user-error "No transport adapter for backend: %s" backend))

    (unless (require 'gptel-request nil t)
      (carriage-log "Transport[gptel] gptel-request not available")
      (carriage-transport-complete t buffer)
      (user-error "GPTel is not available"))

    (with-current-buffer buffer
      (let* ((cb (carriage-transport-gptel--make-callback buffer id))
             (abort-fn
              (lambda ()
                (carriage-log "Transport[gptel] ABORT requested id=%s" id)
                (ignore-errors (gptel-abort buffer))
                ;; If GPTel never delivers terminal callback, force-finalize as abort.
                (carriage-transport-gptel--schedule
                 0.6
                 (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (unless carriage-transport-gptel--finalized
                         (carriage-transport-gptel--finalize
                          buffer id 'abort 'abort (list :status "abort-timeout") nil))))))))
             (start-procs (carriage-transport-gptel--live-gptel-curl-procs)))
        ;; Reset per-request guards for this buffer
        (setq carriage-transport-gptel--finalized nil
              carriage-transport-gptel--finalized-kind nil)
        (carriage-log "Transport[gptel] START id=%s source=%s model=%s prompt-bytes=%s"
                      id source (or model "—")
                      (if (stringp prompt) (number-to-string (string-bytes prompt)) "—"))
        (carriage-transport-gptel--log-procs "PROCS-START" id start-procs)

        ;; Register abort handler for UI (C-c e k)
        (carriage-register-abort-handler abort-fn)

        ;; Start watchdog (watchdog calls on-timeout).
        ;; UI state (sending) is managed by the caller via `carriage-transport-begin'.
        (carriage-transport-gptel--wd-start
         buffer id
         (lambda ()
           (funcall abort-fn)
           (carriage-transport-gptel--finalize
            buffer id 'timeout nil (list :status "timeout") nil)))

        ;; Invoke gptel-request in the origin buffer.
        ;;
        ;; We intentionally avoid touching internal gptel logging or curl buffers.
        ;; Everything we need for diagnostics is logged from callback and process inventory.
        (condition-case err
            (let ((gptel-backend (and (boundp 'gptel-backend) gptel-backend))
                  (gptel-model   (and (boundp 'gptel-model) gptel-model)))
              ;; If model is provided as string, gptel expects symbol; but we don't mutate global.
              ;; Let gptel sanitize if needed; keep this adapter minimal.
              (ignore gptel-backend)
              (ignore gptel-model)
              (gptel-request
                  prompt
                :callback cb
                :buffer buffer
                :stream t
                :system system))
          (error
           ;; Request could not be started at all.
           (carriage-log "Transport[gptel] START-ERROR id=%s err=%s" id (error-message-string err))
           (carriage-transport-gptel--finalize buffer id 'error nil (list :status "start-error") err))))
      t)))

(provide 'carriage-transport-gptel)
;;; carriage-transport-gptel.el ends here
