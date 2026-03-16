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
;; - OpenAI-like attachments semantics in this adapter:
;;   * image/* attachments are sent as media via `gptel-context'
;;   * text/document attachments are sent as inline file contents via :textfile
;;   * generic binary files are not advertised as real attachments and stay metadata-only.
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
(defvar gptel-model)

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

(defcustom carriage-transport-gptel-result-include-model nil
  "When non-nil, include :CAR_MODEL into #+CARRIAGE_RESULT.
Default is nil to avoid duplication with FINGERPRINT and reduce UI drift."
  :type 'boolean
  :group 'carriage-transport-gptel)

(defcustom carriage-transport-gptel-trace-chunks nil
  "When non-nil, log every GPTel callback event (kind + size) for Carriage requests.

This is intentionally very verbose. Use it to debug cases where:
- streaming is announced, but no chunks reach `carriage-insert-stream-chunk',
- only reasoning boundary events arrive, or
- the request appears to \"finish ok\" and then \"finish error\" due to a race."
  :type 'boolean
  :group 'carriage-transport-gptel)

(defun carriage-transport-gptel--trace-callback (id resp info &optional note)
  "Trace GPTel callback events for request ID when enabled. Never signals."
  (when carriage-transport-gptel-trace-chunks
    (condition-case _e
        (let* ((kind (carriage-transport-gptel--response-kind resp))
               (len (cond
                     ((stringp resp) (string-bytes resp))
                     ((and (consp resp) (stringp (cdr resp))) (string-bytes (cdr resp)))
                     (t 0)))
               (http (and (listp info)
                          (or (plist-get info :http-status)
                              (plist-get info :http_status))))
               (status (and (listp info) (plist-get info :status)))
               (status1 (and (stringp status)
                             (if (> (length status) 120) (substring status 0 120) status))))
          (carriage-log "Transport[gptel] TRACE id=%s kind=%s len=%s http=%s status=%s%s"
                        id kind len
                        (or http "—")
                        (or status1 "—")
                        (if note (format " note=%s" note) "")))
      (error nil))))

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
  "Return a clipped printed representation of OBJ (safe).
Ensures multibyte escaping to avoid inserting raw byte-code into logs."
  (let* ((print-escape-multibyte t)
         (max (or max 600))
         (s (condition-case _e
                (cond
                 ;; Always print strings via prin1 with multibyte escaping to avoid
                 ;; embedding raw bytes into logs; also coerce to safe UTF-8 first.
                 ((stringp obj)
                  (prin1-to-string (carriage-transport-gptel--safe-utf8 obj)))
                 (t (prin1-to-string obj)))
              (error (format "%S" obj)))))
    (if (> (length s) max)
        (concat (substring s 0 max) "…")
      s)))

(defun carriage-transport-gptel--redact (obj &optional max)
  "Return a safe, redacted single-line preview for OBJ suitable for logs.
- Functions/closures/byte-code are replaced with \"#<fn>\".
- Large strings are truncated; multibyte is escaped.
- Plists/alists/vectors are walked shallowly."
  (let ((print-escape-multibyte t)
        (max (or max 300)))
    (cl-labels
        ((safe-val (v)
           (cond
            ;; redact any function/closure/subr/byte-code
            ((functionp v) "#<fn>")
            ((and (symbolp v) (fboundp v)) "#<fn>")
            ((and (fboundp 'byte-code-function-p) (byte-code-function-p v)) "#<fn>")
            ;; strings: trim and escape
            ((stringp v)
             (let ((s v))
               (when (> (length s) max)
                 (setq s (concat (substring s 0 max) "…")))
               (with-temp-buffer
                 (let ((print-escape-multibyte t))
                   (prin1 s (current-buffer))
                   (buffer-substring-no-properties (point-min) (point-max))))))
            ;; numbers, symbols, booleans
            ((or (numberp v) (symbolp v) (null v) (eq v t)) (format "%S" v))
            ;; vectors: shallow map
            ((vectorp v)
             (format "[%s]" (mapconcat (lambda (x) (safe-val x))
                                       (append v nil) " ")))
            ;; cons/plist/alist (shallow)
            ((consp v)
             (let* ((is-plist (keywordp (car v)))
                    (pairs
                     (if is-plist
                         (cl-loop for (k val) on v by #'cddr
                                  collect (cons k (safe-val val)))
                       ;; alist-ish
                       (cl-loop for it in v
                                for k = (car-safe it)
                                for val = (cdr-safe it)
                                collect (cons k (safe-val val))))))
               (if is-plist
                   (format ":%s" (mapconcat (lambda (kv)
                                              (format "%s=%s" (car kv) (cdr kv)))
                                            pairs " "))
                 (format "{%s}" (mapconcat (lambda (kv)
                                             (format "%s=%s" (car kv) (cdr kv)))
                                           pairs " ")))))
            ;; fallback
            (t (carriage-transport-gptel--snip v max)))))
      (safe-val obj))))

(defun carriage-transport-gptel--safe-utf8 (s)
  "Return S coerced to a safe UTF-8 string, replacing invalid bytes.
When S is not a string, return it unchanged."
  (if (stringp s)
      (let* ((enc (condition-case _e
                      (encode-coding-string s 'utf-8 'noerror)
                    (error s))))
        (condition-case _e
            (decode-coding-string enc 'utf-8 'noerror)
          (error s)))
    s))

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

(defun carriage-transport-gptel--info-http-ok-p (info)
  "Return non-nil when INFO indicates successful HTTP response."
  (let* ((http (and (listp info)
                    (or (plist-get info :http-status)
                        (plist-get info :http_status))))
         (status (and (listp info) (plist-get info :status))))
    (or (member http '("200" "100" 200 100))
        (and (stringp status)
             (string-match-p "\\bHTTP/[0-9.]+ +\\(200\\|100\\)\\b" status)))))

(defun carriage-transport-gptel--info-has-error-p (info)
  "Return non-nil when INFO carries provider/transport error."
  (and (listp info)
       (plist-get info :error)))

(defun carriage-transport-gptel--classify-final (resp info watchdog-fired)
  "Return classification code symbol for finalization."
  (cond
   (watchdog-fired 'LLM_E_TIMEOUT)
   ((eq resp 'abort) 'LLM_E_ABORT)
   ((carriage-transport-gptel--info-has-error-p info) 'LLM_E_PROVIDER)
   ((and (null resp) (carriage-transport-gptel--info-http-ok-p info)) 'LLM_E_UNKNOWN)
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

;; Pricing helper: compute and log detailed status in one place.
(defun carriage-transport-gptel--pricing-compute-and-log (origin-buffer model-str usage)
  "Compute pricing best-effort and log a single 'pricing:' line indicating status.
Returns COST plist (or nil) with keys like :cost-total-u."
  (let ((cost nil)
        (canon (and (stringp model-str) (concat "gptel:" model-str))))
    (cond
     ;; No model string — log skip with usage snapshot
     ((not (stringp model-str))
      (carriage-log "pricing: skip reason=no-model usage-in=%s usage-out=%s"
                    (or (plist-get usage :tokens-in) "—")
                    (or (plist-get usage :tokens-out) "—")))
     ;; Pricing system not available
     ((not (require 'carriage-pricing nil t))
      (carriage-log "pricing: skip reason=require-failed model=%s" (or canon "")))
     (t
      ;; Try loading table and computing cost; log any errors explicitly.
      (let* ((table nil)
             (table-ok nil))
        (condition-case e
            (progn
              (setq table (carriage-pricing-load-table
                           (with-current-buffer origin-buffer default-directory)))
              (setq table-ok t))
          (error
           (carriage-log "pricing: table-error model=%s err=%s"
                         canon (carriage-transport-gptel--snip e 200))))
        (when table-ok
          (condition-case e2
              (setq cost (carriage-pricing-compute canon usage table))
            (error
             (carriage-log "pricing: compute-error model=%s err=%s"
                           canon (carriage-transport-gptel--snip e2 200))))
          (let* ((tin (plist-get usage :tokens-in))
                 (tout (plist-get usage :tokens-out))
                 (ci  (and cost (plist-get cost :cost-in-u)))
                 (co  (and cost (plist-get cost :cost-out-u)))
                 (cai (and cost (plist-get cost :cost-audio-in-u)))
                 (cao (and cost (plist-get cost :cost-audio-out-u)))
                 (tt  (and cost (plist-get cost :cost-total-u)))
                 (rub (and (integerp tt) (/ tt 1000000.0))))
            (carriage-log "pricing: model=%s known=%s tokens-in=%s tokens-out=%s audio-in=%s audio-out=%s total-u=%s total=%s"
                          canon (if (and cost (integerp tt)) "t" "nil")
                          (or tin "—") (or tout "—") (or cai "—") (or cao "—")
                          (or tt "—") (if rub (format "₽%.2f" rub) "—")))))))
    cost))

;; ---------------------------------------------------------------------
;; Watchdog / finalization guards (buffer-local)

(defvar-local carriage-transport-gptel--finalized nil
  "Non-nil once this buffer's current GPTel request has been finalized by the transport.")

(defvar-local carriage-transport-gptel--finalized-kind nil
  "Final kind symbol used for the last finalize in this buffer.")

;; Chunk diagnostics (per-request, per-buffer)
(defvar-local carriage-transport-gptel--chunks-text 0
  "Number of plain text chunks observed in GPTel callback for current request.")
(defvar-local carriage-transport-gptel--bytes-text 0
  "Total bytes of plain text chunks observed in GPTel callback for current request.")
(defvar-local carriage-transport-gptel--chunks-reasoning 0
  "Number of reasoning chunks observed in GPTel callback for current request.")
(defvar-local carriage-transport-gptel--bytes-reasoning 0
  "Total bytes of reasoning chunks observed in GPTel callback for current request.")
(defvar-local carriage-transport-gptel--chunk-first-at nil
  "Float-time of first callback event observed for current request, or nil.")
(defvar-local carriage-transport-gptel--chunk-last-at nil
  "Float-time of last callback event observed for current request, or nil.")
(defvar-local carriage-transport-gptel--chunk-last-kind nil
  "Last callback event kind for current request: symbol like 'text/'reasoning/'done/...")

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

(defun carriage-transport-gptel--chunk-reset ()
  "Reset per-request callback/chunk counters in current buffer."
  (setq carriage-transport-gptel--chunks-text 0
        carriage-transport-gptel--bytes-text 0
        carriage-transport-gptel--chunks-reasoning 0
        carriage-transport-gptel--bytes-reasoning 0
        carriage-transport-gptel--chunk-first-at nil
        carriage-transport-gptel--chunk-last-at nil
        carriage-transport-gptel--chunk-last-kind nil))

(defun carriage-transport-gptel--chunk-note (kind payload)
  "Record a callback event of KIND with PAYLOAD (string or nil) into counters."
  (let* ((now (float-time))
         (bytes (if (stringp payload) (string-bytes payload) 0)))
    (unless (numberp carriage-transport-gptel--chunk-first-at)
      (setq carriage-transport-gptel--chunk-first-at now))
    (setq carriage-transport-gptel--chunk-last-at now)
    (setq carriage-transport-gptel--chunk-last-kind kind)
    (pcase kind
      ('text
       (setq carriage-transport-gptel--chunks-text (1+ (or carriage-transport-gptel--chunks-text 0)))
       (setq carriage-transport-gptel--bytes-text (+ (or carriage-transport-gptel--bytes-text 0) bytes)))
      ('reasoning
       (setq carriage-transport-gptel--chunks-reasoning (1+ (or carriage-transport-gptel--chunks-reasoning 0)))
       (setq carriage-transport-gptel--bytes-reasoning (+ (or carriage-transport-gptel--bytes-reasoning 0) bytes)))
      (_ nil))))

(defun carriage-transport-gptel--gptel-proc+buf-for-origin (origin-buffer)
  "Return (PROC . PROC-BUF) for GPTel request whose :buffer is ORIGIN-BUFFER, or nil."
  (when (and (buffer-live-p origin-buffer)
             (boundp 'gptel--request-alist)
             (listp gptel--request-alist))
    (cl-loop
     for entry in gptel--request-alist
     for proc = (car entry)
     for fsm = (ignore-errors (cadr entry))
     for info = (and fsm (ignore-errors (gptel-fsm-info fsm)))
     when (and (processp proc)
               (listp info)
               (eq (plist-get info :buffer) origin-buffer))
     return (cons proc (process-buffer proc)))))

(defun carriage-transport-gptel--log-gptel-proc-buffer (origin-buffer id tag)
  "Log snapshot of GPTel curl process buffer for ORIGIN-BUFFER (best-effort)."
  (when carriage-transport-gptel-diagnostics
    (when-let* ((pb (carriage-transport-gptel--gptel-proc+buf-for-origin origin-buffer))
                (proc (car pb))
                (buf (cdr pb))
                ((buffer-live-p buf)))
      (with-current-buffer buf
        (let* ((sz (buffer-size))
               (tail (buffer-substring-no-properties
                      (max (point-min) (- (point-max) 500))
                      (point-max))))
          (carriage-log "Transport[gptel] %s id=%s proc=%s buf=%s size=%d tail=%s"
                        (or tag "PROC-SNAP")
                        id
                        (process-name proc)
                        (buffer-name buf)
                        sz
                        (carriage-transport-gptel--snip tail 320)))))))

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

(defun carriage-transport-gptel--finalize--log-end (id final code info resp reqn procs0)
  "Log the canonical END line for request ID."
  (let* ((status (and (listp info) (plist-get info :status)))
         (http (and (listp info)
                    (or (plist-get info :http-status)
                        (plist-get info :http_status))))
         (resp-kind (carriage-transport-gptel--response-kind resp)))
    (carriage-log
     "Transport[gptel] END id=%s final=%s code=%s status=%s http=%s resp=%s req-alist=%s procs0=%d chunks{text=%d/%dB reasoning=%d/%dB last=%s}"
     id final code
     (if status (carriage-transport-gptel--snip status 200) "—")
     (if http (format "%s" http) "—")
     resp-kind
     (if (numberp reqn) (number-to-string reqn) "—")
     (length procs0)
     (or carriage-transport-gptel--chunks-text 0)
     (or carriage-transport-gptel--bytes-text 0)
     (or carriage-transport-gptel--chunks-reasoning 0)
     (or carriage-transport-gptel--bytes-reasoning 0)
     (or carriage-transport-gptel--chunk-last-kind '-))
    (when (fboundp 'carriage-traffic-log)
      (carriage-traffic-log
       'in
       "gptel: end id=%s final=%s code=%s http=%s status=%s err=%s chunks{text=%d/%dB reasoning=%d/%dB last=%s}"
       id final code
       (if http (format "%s" http) "—")
       (if status (carriage-transport-gptel--snip status 120) "—")
       (carriage-transport-gptel--snip (and (listp info) (plist-get info :error)) 120)
       (or carriage-transport-gptel--chunks-text 0)
       (or carriage-transport-gptel--bytes-text 0)
       (or carriage-transport-gptel--chunks-reasoning 0)
       (or carriage-transport-gptel--bytes-reasoning 0)
       (or carriage-transport-gptel--chunk-last-kind '-)))))

(defun carriage-transport-gptel--finalize--log-last (id final resp info)
  "Log LAST diagnostics for FINAL request kinds."
  (when (and carriage-transport-gptel-diagnostics
             (memq final '(abort error timeout)))
    (carriage-log "Transport[gptel] LAST id=%s resp=%s" id
                  (carriage-transport-gptel--snip resp 600))
    (carriage-log "Transport[gptel] LAST id=%s info-keys=%S" id
                  (carriage-transport-gptel--plist-keys info))
    (carriage-log "Transport[gptel] LAST id=%s info-redacted=%s" id
                  (carriage-transport-gptel--redact info 300))))

(defun carriage-transport-gptel--finalize--log-internal-error (id internal-error)
  "Log INTERNAL diagnostics for INTERNAL-ERROR."
  (when internal-error
    (carriage-log "Transport[gptel] INTERNAL id=%s err=%s" id
                  (carriage-transport-gptel--snip internal-error 600))
    (when-let* ((bt (carriage-transport-gptel--maybe-backtrace-string)))
      (carriage-log "Transport[gptel] INTERNAL id=%s backtrace:\n%s" id bt))))

(defun carriage-transport-gptel--finalize--note-error-class (origin-buffer final code info)
  "Update buffer-local last error classification fields for UI (best-effort)."
  (when (buffer-live-p origin-buffer)
    (with-current-buffer origin-buffer
      (if (eq final 'done)
          (progn
            (setq-local carriage--last-error-class nil)
            (setq-local carriage--last-error-detail nil))
        (let* ((st (and (fboundp 'carriage-transport-gptel--status->code+text)
                        (listp info)
                        (carriage-transport-gptel--status->code+text info)))
               (http (and (listp st) (plist-get st :code)))
               (http-s (and (integerp http) (number-to-string http)))
               (detail
                (or http-s
                    (pcase final
                      ('timeout "timeout")
                      ('abort "abort")
                      (_ nil))
                    (pcase code
                      ('LLM_E_NETWORK "network")
                      ('LLM_E_PROVIDER "provider")
                      ('LLM_E_INTERNAL "internal")
                      ('LLM_E_TIMEOUT "timeout")
                      ('LLM_E_ABORT "abort")
                      (_ "error")))))
          (setq-local carriage--last-error-class code)
          (setq-local carriage--last-error-detail detail)
          ;; Ensure http status vars even when GPTel callback never fired.
          (when http-s
            (setq-local carriage--last-http-status http-s))
          (when (and (listp st) (stringp (plist-get st :text)))
            (setq-local carriage--last-http-status-text (plist-get st :text)))
          ;; Backend error string best-effort.
          (when (and (fboundp 'carriage-transport-gptel--error->string)
                     (listp info))
            (let ((e (carriage-transport-gptel--error->string info)))
              (when (stringp e)
                (setq-local carriage--last-backend-error e))))
          ;; Ensure modeline reflects updated error details promptly (best-effort).
          (when (fboundp 'carriage-ui--invalidate-ml-cache)
            (ignore-errors (carriage-ui--invalidate-ml-cache)))
          (ignore-errors (force-mode-line-update t)))))))

(defun carriage-transport-gptel--finalize--maybe-cleanup (origin-buffer id final procs0)
  "Abort and cleanup processes/state best-effort for non-successful FINAL.
Returns killed0 count (or 0)."
  (if (not (and (eq carriage-transport-gptel-cleanup-policy 'aggressive-on-error)
                (memq final '(abort error timeout))))
      0
    (ignore-errors (gptel-abort origin-buffer))
    (ignore-errors (carriage-transport-gptel--cleanup-request-alist origin-buffer id))
    (carriage-transport-gptel--log-procs "PROCS-BEFORE" id procs0)
    (let ((killed0 (carriage-transport-gptel--cleanup-kill-procs procs0)))
      (carriage-transport-gptel--schedule
       (or carriage-transport-gptel-cleanup-delay 0.2)
       (lambda ()
         (when (buffer-live-p origin-buffer)
           (with-current-buffer origin-buffer
             (let* ((procs1 (carriage-transport-gptel--live-gptel-curl-procs))
                    (killed1 (carriage-transport-gptel--cleanup-kill-procs procs1)))
               (ignore-errors (carriage-transport-gptel--cleanup-request-alist origin-buffer id))
               (carriage-transport-gptel--log-procs "PROCS-AFTER" id
                                                    (carriage-transport-gptel--live-gptel-curl-procs))
               (carriage-log "Transport[gptel] ACTION id=%s abort=t kill0=%d kill1=%d"
                             id killed0 killed1))))))
      killed0)))

(defun carriage-transport-gptel--finalize--stream-and-complete (origin-buffer final)
  "Finalize stream UI and call transport completion for ORIGIN-BUFFER."
  (with-demoted-errors "carriage-stream-finalize error: %S"
    (pcase final
      ('done (carriage-stream-finalize nil t))
      (_     (carriage-stream-finalize t nil))))
  (carriage-transport-complete (memq final '(abort error timeout)) origin-buffer))

(defun carriage-transport-gptel--numberish->nat-int (v)
  "Coerce V into a non-negative integer, or nil."
  (let ((n (cond
            ((integerp v) v)
            ((numberp v) (truncate v))
            ((and (stringp v) (string-match-p "\\`[0-9]+\\'" v)) (string-to-number v))
            (t nil))))
    (when (and (integerp n) (>= n 0)) n)))

(defun carriage-transport-gptel--usage-from-info (info)
  "Extract usage plist (:tokens-in N :tokens-out M) from INFO, or nil."
  (let* ((uobj (and (listp info) (plist-get info :usage)))
         (raw-out
          (and (listp info)
               (or (plist-get info :output-tokens)
                   (plist-get info :completion-tokens)
                   (plist-get info :tokens-out)
                   (plist-get info :output_tokens)
                   (plist-get info :tokens_out)
                   (and (listp uobj)
                        (or (plist-get uobj :output-tokens)
                            (plist-get uobj :completion-tokens)
                            (plist-get uobj :tokens-out)
                            (plist-get uobj :completion_tokens)
                            (plist-get uobj :output_tokens)
                            (plist-get uobj :tokens_out))))))
         (raw-in
          (and (listp info)
               (or (plist-get info :input-tokens)
                   (plist-get info :prompt-tokens)
                   (plist-get info :tokens-in)
                   (plist-get info :input_tokens)
                   (plist-get info :tokens_in)
                   (and (listp uobj)
                        (or (plist-get uobj :input-tokens)
                            (plist-get uobj :prompt-tokens)
                            (plist-get uobj :tokens-in)
                            (plist-get uobj :prompt_tokens)
                            (plist-get uobj :input_tokens)
                            (plist-get uobj :tokens_in))))))
         (out (carriage-transport-gptel--numberish->nat-int raw-out))
         (in  (carriage-transport-gptel--numberish->nat-int raw-in)))
    (let (u)
      (when (integerp in)  (setq u (plist-put u :tokens-in in)))
      (when (integerp out) (setq u (plist-put u :tokens-out out)))
      u)))

(defun carriage-transport-gptel--usage-from-fsm (origin-buffer)
  "Best-effort extract usage from GPTel FSM for ORIGIN-BUFFER."
  (when-let* ((fsm (carriage-transport-gptel--gptel-fsm-for-buffer origin-buffer))
              (info2 (ignore-errors (gptel-fsm-info fsm))))
    (carriage-transport-gptel--usage-from-info info2)))

(defun carriage-transport-gptel--finalize--extract-usage (origin-buffer info)
  "Return best-effort usage plist from INFO, with FSM fallback.
Also merges bytes-in/out captured by our `gptel-request' wrapper when available."
  (let* ((u0 (carriage-transport-gptel--usage-from-info info))
         (u1 (if (or (plist-get u0 :tokens-in) (plist-get u0 :tokens-out))
                 u0
               (carriage-transport-gptel--usage-from-fsm origin-buffer)))
         (bytes
          (when (buffer-live-p origin-buffer)
            (with-current-buffer origin-buffer
              (and (boundp 'carriage--last-usage)
                   (listp carriage--last-usage)
                   carriage--last-usage))))
         (tin  (and (listp u1) (plist-get u1 :tokens-in)))
         (tout (and (listp u1) (plist-get u1 :tokens-out)))
         (bin  (and (listp bytes) (plist-get bytes :bytes-in)))
         (bout (and (listp bytes) (plist-get bytes :bytes-out))))
    ;; IMPORTANT:
    ;; Always return a plist with these keys (values may be nil).
    ;; This ensures fingerprint/result upserts can still write placeholders,
    ;; so UI/folds don't end up “empty” when usage/cost is unknown.
    (list :tokens-in tin
          :tokens-out tout
          :bytes-in (and (integerp bin) bin)
          :bytes-out (and (integerp bout) bout))))

(defun carriage-transport-gptel--finalize--insert-result-line (origin-buffer id status model-str usage cost)
  "Insert #+CARRIAGE_RESULT line into ORIGIN-BUFFER."
  (with-current-buffer origin-buffer
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (let* ((pl (list
                  :CAR_REQ_ID id
                  :CAR_STATUS status
                  :CAR_BACKEND 'gptel
                  :CAR_PROVIDER nil))
             (pl (if carriage-transport-gptel-result-include-model
                     (plist-put pl :CAR_MODEL (or model-str ""))
                   pl))
             ;; Always include usage keys (even when nil).
             ;;
             ;; Display/UX policy:
             ;; - When token usage is unavailable, also fill in/out with bytes so
             ;;   result summaries don't stay empty ("in:- out:-").
             ;; - Preserve raw bytes in dedicated keys regardless.
             (pl
              (let* ((tin (plist-get usage :tokens-in))
                     (tout (plist-get usage :tokens-out))
                     (bin (plist-get usage :bytes-in))
                     (bout (plist-get usage :bytes-out))
                     (unit (cond
                            ((or (integerp tin) (integerp tout)) 'tokens)
                            ((or (integerp bin) (integerp bout)) 'bytes)
                            (t nil)))
                     (tin2 (if (integerp tin) tin (and (integerp bin) bin)))
                     (tout2 (if (integerp tout) tout (and (integerp bout) bout))))
                (setq pl (plist-put pl :CAR_TOKENS_IN tin2))
                (setq pl (plist-put pl :CAR_TOKENS_OUT tout2))
                (setq pl (plist-put pl :CAR_BYTES_IN bin))
                (setq pl (plist-put pl :CAR_BYTES_OUT bout))
                (when unit
                  (setq pl (plist-put pl :CAR_USAGE_UNIT unit)))
                pl))
             (pl (if (and cost (plist-get cost :cost-in-u))
                     (plist-put pl :CAR_COST_IN_U (plist-get cost :cost-in-u))
                   pl))
             (pl (if (and cost (plist-get cost :cost-out-u))
                     (plist-put pl :CAR_COST_OUT_U (plist-get cost :cost-out-u))
                   pl))
             (pl (if (and cost (plist-get cost :cost-audio-in-u))
                     (plist-put pl :CAR_COST_AUDIO_IN_U (plist-get cost :cost-audio-in-u))
                   pl))
             (pl (if (and cost (plist-get cost :cost-audio-out-u))
                     (plist-put pl :CAR_COST_AUDIO_OUT_U (plist-get cost :cost-audio-out-u))
                   pl))
             (pl (plist-put pl :CAR_COST_TOTAL_U (and cost (plist-get cost :cost-total-u))))
             (pl (let ((tt (and cost (plist-get cost :cost-total-u))))
                   (plist-put pl :CAR_COST_KNOWN (and (integerp tt) t))))
             (pl (plist-put pl :CAR_TS (float-time))))
        (insert (format "#+CARRIAGE_RESULT: %S\n" pl))))))

(defun carriage-transport-gptel--finalize--post-ui-refresh (origin-buffer)
  "Best-effort refresh UI elements affected by a finalize."
  (when (fboundp 'carriage-ui-doc-cost-schedule-refresh)
    (ignore-errors
      (with-current-buffer origin-buffer
        (carriage-ui-doc-cost-schedule-refresh 0.05))))
  (when (fboundp 'carriage-doc-state-summary-refresh)
    (ignore-errors
      (with-current-buffer origin-buffer
        (carriage-doc-state-summary-refresh origin-buffer)))))

(defun carriage-transport-gptel--finalize--record-result (origin-buffer id final info requested-model)
  "Best-effort record usage/model/cost result for request."
  (ignore-errors
    (let* ((model-str (carriage-transport-gptel--model-string-from-info info requested-model))
           (usage (carriage-transport-gptel--finalize--extract-usage origin-buffer info))
           (status (pcase final ('done 'done) ('abort 'abort) ('timeout 'timeout) (_ 'error)))
           (cost nil))
      (setq cost (carriage-transport-gptel--pricing-compute-and-log origin-buffer model-str usage))
      ;; Persist last request metrics for UI (modeline req-cost/status tooltips).
      ;; Keep "unknown" values as nil (never coerce missing tokens/cost to 0).
      (ignore-errors
        (with-current-buffer origin-buffer
          ;; Model id (best-effort stable string for UI).
          (setq-local carriage--last-model-id model-str)
          ;; Merge extracted token usage into the last usage snapshot to preserve
          ;; bytes-in/bytes-out that are captured by our gptel-request wrapper.
          (let* ((u0 (and (boundp 'carriage--last-usage) carriage--last-usage))
                 (u (if (listp u0) (copy-sequence u0) nil)))
            (when (plist-get usage :tokens-in)
              (setq u (plist-put u :tokens-in (plist-get usage :tokens-in))))
            (when (plist-get usage :tokens-out)
              (setq u (plist-put u :tokens-out (plist-get usage :tokens-out))))
            (setq-local carriage--last-usage u))
          ;; Store cost as-is from pricing (may be nil / unknown).
          ;; Add a conservative :known flag for UI renderers when not present.
          (let* ((c (and (listp cost) (copy-sequence cost)))
                 (tt (and (listp c) (plist-get c :cost-total-u))))
            (when (and (listp c) (not (plist-member c :known)))
              (setq c (plist-put c :known (and (integerp tt) (> tt 0)))))
            (setq-local carriage--last-cost c))
          (when (fboundp 'carriage-ui--invalidate-ml-cache)
            (carriage-ui--invalidate-ml-cache))
          (force-mode-line-update t)))
      ;; Always try to upsert fingerprint with usage/bytes (even when all values are nil),
      ;; so the fingerprint line gains stable keys for UI/fold renderers.
      (when (fboundp 'carriage-fingerprint-note-usage-and-cost)
        (ignore-errors
          (carriage-fingerprint-note-usage-and-cost usage 'gptel nil model-str)))
      (carriage-transport-gptel--finalize--insert-result-line
       origin-buffer id status model-str usage cost)
      (carriage-transport-gptel--finalize--post-ui-refresh origin-buffer))))

(defun carriage-transport-gptel--finalize--enter (id final)
  "Mark current buffer as finalized for FINAL.
Returns non-nil if already finalized (caller should stop)."
  (carriage-transport-gptel--wd-stop)
  (if carriage-transport-gptel--finalized
      (progn
        (when carriage-transport-gptel-diagnostics
          (carriage-log "Transport[gptel] DUP-FINALIZE id=%s final=%s ignored"
                        id final))
        t)
    (setq carriage-transport-gptel--finalized t
          carriage-transport-gptel--finalized-kind final)
    nil))

(defun carriage-transport-gptel--finalize--collect (resp info internal-error)
  "Collect finalize-time diagnostics from current buffer and arguments.
Returns plist with :watchdog-fired :code :reqn :procs0."
  (let* ((watchdog-fired (and (boundp 'carriage-transport-gptel--wd-fired)
                              carriage-transport-gptel--wd-fired))
         (code (if internal-error
                   'LLM_E_INTERNAL
                 (carriage-transport-gptel--classify-final resp info watchdog-fired)))
         (reqn (carriage-transport-gptel--maybe-gptel-request-alist-size))
         (procs0 (carriage-transport-gptel--live-gptel-curl-procs)))
    (list :watchdog-fired watchdog-fired
          :code code
          :reqn reqn
          :procs0 procs0)))

(defun carriage-transport-gptel--finalize--run (origin-buffer id final resp info requested-model internal-error data)
  "Run finalize actions using DATA from `carriage-transport-gptel--finalize--collect'."
  (let ((code (plist-get data :code))
        (reqn (plist-get data :reqn))
        (procs0 (plist-get data :procs0)))
    (carriage-transport-gptel--finalize--log-end id final code info resp reqn procs0)
    (carriage-transport-gptel--finalize--log-last id final resp info)
    (carriage-transport-gptel--finalize--log-internal-error id internal-error)
    (carriage-transport-gptel--finalize--note-error-class origin-buffer final code info)
    (carriage-transport-gptel--finalize--maybe-cleanup origin-buffer id final procs0)
    (carriage-transport-gptel--finalize--stream-and-complete origin-buffer final)
    (carriage-transport-gptel--finalize--record-result origin-buffer id final info requested-model)))

(defun carriage-transport-gptel--finalize--maybe-run
    (origin-buffer id final resp info requested-model internal-error)
  "Finalize unless already finalized in ORIGIN-BUFFER."
  (with-current-buffer origin-buffer
    (unless (carriage-transport-gptel--finalize--enter id final)
      (let ((data (carriage-transport-gptel--finalize--collect resp info internal-error)))
        (carriage-transport-gptel--finalize--run
         origin-buffer id final resp info requested-model internal-error data)))
    t))

(defun carriage-transport-gptel--finalize--valid-final-p (final)
  "Return non-nil when FINAL is a supported finalize kind."
  (memq final '(done abort error timeout)))

(defun carriage-transport-gptel--finalize--normalize-final (id final)
  "Return FINAL when valid, otherwise coerce to 'error (and log warning)."
  (if (carriage-transport-gptel--finalize--valid-final-p final)
      final
    (carriage-log "Transport[gptel] WARN id=%s invalid-final=%S coerced=error" id final)
    'error))

(defun carriage-transport-gptel--finalize--call
    (origin-buffer id final resp info requested-model internal-error)
  "Internal finalize call (small wrapper around `carriage-transport-gptel--finalize--maybe-run')."
  (carriage-transport-gptel--finalize--maybe-run
   origin-buffer id final resp info requested-model internal-error))

(defun carriage-transport-gptel--finalize--normalize-and-call
    (origin-buffer id final resp info requested-model internal-error)
  "Normalize FINAL and call internal finalize implementation."
  (let ((final2 (carriage-transport-gptel--finalize--normalize-final id final)))
    (carriage-transport-gptel--finalize--call
     origin-buffer id final2 resp info requested-model internal-error)))

(defun carriage-transport-gptel--finalize
    (origin-buffer id final resp info requested-model &optional internal-error)
  "Finalize request and update UI. FINAL is one of 'done 'abort 'error 'timeout."
  (carriage-transport-gptel--finalize--normalize-and-call
   origin-buffer id final resp info requested-model internal-error))

;; ---------------------------------------------------------------------
;; Callback

(defun carriage-transport-gptel--make-callback (origin-buffer id requested-model)
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
      (when carriage-transport-gptel-trace-chunks
        (carriage-transport-gptel--trace-callback
         id resp info
         (cond
          ((and (stringp resp) (string-empty-p resp)) "empty-string")
          ((and (consp resp) (eq (car resp) 'reasoning) (stringp (cdr resp)) (string-empty-p (cdr resp))) "empty-reasoning")
          ((and (consp resp) (eq (car resp) 'reasoning) (eq (cdr resp) t)) "reasoning-end")
          ((eq resp t) "done")
          ((eq resp 'abort) "abort")
          ((null resp) "nil")
          (t nil))))
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
              (with-current-buffer origin-buffer
                (carriage-transport-gptel--chunk-note 'text resp))
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
                  (with-current-buffer origin-buffer
                    (carriage-transport-gptel--chunk-note 'reasoning val))
                  (unless first-stream
                    (setq first-stream t)
                    (carriage-transport-streaming origin-buffer))
                  (with-current-buffer origin-buffer
                    (carriage-insert-stream-chunk val 'reasoning)))
                 ((eq val t)
                  (with-current-buffer origin-buffer
                    (carriage-transport-gptel--chunk-note 'reasoning-end nil))
                  ;; end reasoning marker: insertion layer handles closing
                  nil)
                 (t nil))))

             ;; Done
             ((eq resp t)
              (with-current-buffer origin-buffer
                (carriage-transport-gptel--chunk-note 'done nil))
              (setq finished t)
              (carriage-transport-gptel--finalize origin-buffer id 'done resp info requested-model))

             ;; Abort (explicit)
             ((eq resp 'abort)
              (with-current-buffer origin-buffer
                (carriage-transport-gptel--chunk-note 'abort nil))
              (setq finished t)
              (carriage-transport-gptel--finalize origin-buffer id 'abort resp info requested-model))

             ;; Empty terminal response: if HTTP already succeeded and provider error is absent,
             ;; treat this as a successful completion with empty textual content.
             ((null resp)
              (with-current-buffer origin-buffer
                (carriage-transport-gptel--chunk-note 'nil nil))
              (setq finished t)
              (when carriage-transport-gptel-diagnostics
                (carriage-log "Transport[gptel] NIL-RESP id=%s http-ok=%s info-error=%s status=%s"
                              id
                              (if (carriage-transport-gptel--info-http-ok-p info) "t" "nil")
                              (if (carriage-transport-gptel--info-has-error-p info) "t" "nil")
                              (carriage-transport-gptel--snip
                               (and (listp info) (plist-get info :status))
                               200)))
              (carriage-transport-gptel--finalize
               origin-buffer id
               (if (and (carriage-transport-gptel--info-http-ok-p info)
                        (not (carriage-transport-gptel--info-has-error-p info)))
                   'done
                 'error)
               resp info requested-model))

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
          origin-buffer id 'error last-resp last-info requested-model err))))))

;; ---------------------------------------------------------------------
;; Attachments (manual + auto)

(defgroup carriage-transport-gptel-attachments nil
  "Attachments support for Carriage GPTel transport."
  :group 'carriage)

(defcustom carriage-transport-gptel-openai-like-attachment-mode 'inline-text-and-images
  "How to send attachments over OpenAI-like GPTel transports.

Supported values:

- `inline-text-and-images' (default):
  - text-like files are inlined into the prompt/context as text;
  - image/* files are sent as media via `gptel-context';
  - unsupported binary files are omitted from transport and mentioned only in diagnostics.

- `note-only':
  keep the old conservative behavior: do not inline text attachments, only append
  a system note for files that are not transported natively.

This variable exists because current gptel OpenAI-compatible code supports
text, image_url/media and textfile-style inline content, but not native generic
file attachments/file_id workflow."
  :type '(choice
          (const :tag "Inline text files and send images as media" inline-text-and-images)
          (const :tag "Do not inline, mention only in system note" note-only))
  :group 'carriage-transport-gptel-attachments)

(defcustom carriage-transport-gptel-auto-attach-size-threshold (* 1024 1024)
  "Auto-attach files from #+begin_context when their size exceeds this threshold (bytes).

This is in addition to always auto-attaching binary files."
  :type 'integer
  :group 'carriage-transport-gptel-attachments)

(defcustom carriage-transport-gptel-inline-text-max-bytes (* 200 1024)
  "Maximum bytes to inline from a single text-like attachment.

Inlining very large files can stall Emacs for a few seconds (disk I/O + huge
string concatenation) before the request is even started. Files larger than this
limit are NOT inlined and are instead mentioned in the \"metadata only\" note.

Set to nil to disable the limit (not recommended)."
  :type '(choice (const :tag "No limit" nil)
                 integer)
  :group 'carriage-transport-gptel-attachments)

(defun carriage-transport-gptel--attachment-too-large-to-inline-p (attachment)
  "Return non-nil when ATTACHMENT is too large to inline as text."
  (let ((max carriage-transport-gptel-inline-text-max-bytes)
        (sz (plist-get attachment :size-bytes)))
    (and (integerp max)
         (> max 0)
         (integerp sz)
         (> sz max))))

(defun carriage-transport-gptel--read-block-path-lines (buffer begin-rx end-rx)
  "Collect trimmed non-empty path lines between BEGIN-RX and END-RX in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search t)
              (paths '()))
          (goto-char (point-min))
          (while (re-search-forward begin-rx nil t)
            (let ((body-beg (min (point-max) (1+ (line-end-position)))))
              (if (re-search-forward end-rx nil t)
                  (let ((body-end (line-beginning-position)))
                    (save-excursion
                      (goto-char body-beg)
                      (while (< (point) body-end)
                        (let* ((ln (string-trim
                                    (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position)))))
                          (unless (or (string-empty-p ln)
                                      (string-prefix-p "#" ln)
                                      (string-prefix-p ";;" ln))
                            (push ln paths)))
                        (forward-line 1))))
                ;; Missing end marker: consume till EOF.
                (goto-char (point-max)))))
          (nreverse (delete-dups (delq nil paths))))))))

(defun carriage-transport-gptel--buffer-root (buffer)
  "Return best-effort project root for BUFFER (string)."
  (with-current-buffer buffer
    (or (and (fboundp 'carriage-project-root)
             (ignore-errors (carriage-project-root)))
        default-directory)))

(defun carriage-transport-gptel--resolve-path (buffer p &optional root)
  "Resolve P (string) relative to ROOT when provided; otherwise use BUFFER root.
Return truename or nil if remote/non-existent."
  (when (and (stringp p) (not (string-empty-p (string-trim p))))
    (let* ((p (string-trim p)))
      (unless (file-remote-p p)
        (let* ((root (or root (carriage-transport-gptel--buffer-root buffer)))
               (abs (if (file-name-absolute-p p)
                        p
                      (expand-file-name p (or root default-directory))))
               (tru (ignore-errors (file-truename abs))))
          (when (and (stringp tru) (file-exists-p tru))
            tru))))))

(defun carriage-transport-gptel--file-binary-p (path)
  "Return non-nil if PATH looks like a binary file (NUL byte in first chunk)."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents-literally path nil 0 512)
        (goto-char (point-min))
        (search-forward (string 0) nil t))
    (error nil)))

(defun carriage-transport-gptel--file-size (path)
  "Return size in bytes for PATH, or nil."
  (condition-case nil
      (file-attribute-size (file-attributes path))
    (error nil)))

(defun carriage-transport-gptel--collect-attachments (buffer)
  "Collect attachments from BUFFER.

Manual:
- all paths listed in #+begin_attachments blocks.

Auto:
- paths from #+begin_context blocks only if binary or larger than
  `carriage-transport-gptel-auto-attach-size-threshold'."
  (let* ((root (carriage-transport-gptel--buffer-root buffer))
         ;; Per-dispatch caches: avoid repeating I/O (file-attributes / reads).
         (size-cache (make-hash-table :test 'equal))
         (bin-cache (make-hash-table :test 'equal))
         (get-size
          (lambda (p)
            (let ((v (gethash p size-cache :missing)))
              (if (eq v :missing)
                  (let ((sz (carriage-transport-gptel--file-size p)))
                    (puthash p sz size-cache)
                    sz)
                v))))
         (get-bin
          (lambda (p)
            (let ((v (gethash p bin-cache :missing)))
              (if (eq v :missing)
                  (let ((b (carriage-transport-gptel--file-binary-p p)))
                    (puthash p b bin-cache)
                    b)
                v))))
         (resolve (lambda (p) (carriage-transport-gptel--resolve-path buffer p root)))
         (manual-lines
          (carriage-transport-gptel--read-block-path-lines
           buffer
           "^[ \t]*#\\+begin_attachments\\b.*$"
           "^[ \t]*#\\+end_attachments\\b.*$"))
         (context-lines
          (carriage-transport-gptel--read-block-path-lines
           buffer
           "^[ \t]*#\\+begin_context\\b.*$"
           "^[ \t]*#\\+end_context\\b.*$"))
         (manual-paths
          (delq nil (mapcar resolve manual-lines)))
         (context-paths
          (delq nil (mapcar resolve context-lines)))
         (auto-paths
          (let ((acc '()))
            (dolist (p context-paths)
              (let* ((sz (funcall get-size p))
                     (bin (funcall get-bin p)))
                (when (or bin
                          (and (numberp sz)
                               (numberp carriage-transport-gptel-auto-attach-size-threshold)
                               (> sz carriage-transport-gptel-auto-attach-size-threshold)))
                  (push p acc))))
            (nreverse acc)))
         (all (delete-dups (append manual-paths auto-paths))))
    (carriage-log "Transport[gptel] ATTACH-COLLECT root=%s manual-lines=%d manual-paths=%d context-paths=%d auto-paths=%d total=%d"
                  (or root "—")
                  (length manual-lines) (length manual-paths)
                  (length context-paths) (length auto-paths)
                  (length all))
    (cl-loop
     for p in all
     for sz = (funcall get-size p)
     for bin = (funcall get-bin p)
     collect (list :path p
                   :mime (carriage-transport-gptel--attachment-mime p)
                   :size-bytes (and (numberp sz) sz)
                   :reason (cond
                            ((member p manual-paths) 'manual)
                            (bin 'binary)
                            (t 'size-limit))))))

(defun carriage-transport-gptel--attachment-mime (path)
  "Return MIME type for PATH, or nil.

IMPORTANT: do not force `(require 'mailcap)` on the hot path, because loading and
parsing mailcap may cause a noticeable stall on the first request. Media upload
path (`carriage-transport-gptel--attachments->gptel-context`) will `require`
mailcap when needed."
  (and (stringp path)
       (fboundp 'mailcap-file-name-to-mime-type)
       (mailcap-file-name-to-mime-type path)))

(defun carriage-transport-gptel--attachment-image-p (mime)
  "Return non-nil when MIME is an image type."
  (and (stringp mime) (string-prefix-p "image/" mime)))

(defun carriage-transport-gptel--attachment-text-p (mime path)
  "Return non-nil when attachment with MIME and PATH should be inlined as text."
  (or (and (stringp mime) (string-prefix-p "text/" mime))
      (and (stringp mime)
           (member mime '("application/json"
                          "application/xml"
                          "application/yaml"
                          "application/x-yaml"
                          "application/toml")))
      (and (stringp path)
           (let ((ext (downcase (or (file-name-extension path) ""))))
             (member ext '("org" "md" "markdown" "txt" "text"
                           "json" "jsonl" "yaml" "yml" "toml" "xml"
                           "el" "py" "js" "ts" "tsx" "c" "h" "cc" "cpp"
                           "hpp" "java" "go" "rs" "rb" "sh" "bash" "zsh"
                           "html" "css" "csv"))))))

(defun carriage-transport-gptel--attachments-inline-text (attachments)
  "Return attachments context string for text-like ATTACHMENTS, or nil."
  (let ((chunks '()))
    (dolist (a attachments)
      (let* ((p (plist-get a :path))
             (mime (or (plist-get a :mime)
                       (carriage-transport-gptel--attachment-mime p))))
        (when (and (stringp p)
                   (file-exists-p p)
                   (carriage-transport-gptel--attachment-text-p mime p))
          (carriage-log "Transport[gptel] ATTACH-INLINE path=%s mime=%s"
                        p (or mime "-"))
          (push (format "In attachment `%s`:\n\n```\n%s\n```"
                        p
                        (with-temp-buffer
                          (insert-file-contents p)
                          (buffer-substring-no-properties (point-min) (point-max))))
                chunks))))
    (when chunks
      (mapconcat #'identity (nreverse chunks) "\n\n"))))

(defun carriage-transport-gptel--attachments-inline-text-enabled-p ()
  "Return non-nil when text attachments should be inlined for OpenAI-like transports."
  (eq carriage-transport-gptel-openai-like-attachment-mode 'inline-text-and-images))

(defun carriage-transport-gptel--attachments-inline-text-blocks (attachments)
  "Return inline prompt blocks for text-like ATTACHMENTS, or nil."
  (let ((inline-text (and (carriage-transport-gptel--attachments-inline-text-enabled-p)
                          (carriage-transport-gptel--attachments-inline-text attachments))))
    (when (and (stringp inline-text)
               (not (string-empty-p (string-trim inline-text))))
      (concat "Attached files content:\n\n" inline-text))))

(defun carriage-transport-gptel--attachments-note-attachments (attachments)
  "Return attachments from ATTACHMENTS that should remain note-only."
  (let ((inline-text-p (carriage-transport-gptel--attachments-inline-text-enabled-p)))
    (cl-loop
     for a in attachments
     for p = (plist-get a :path)
     for mime = (or (plist-get a :mime)
                    (carriage-transport-gptel--attachment-mime p))
     unless (or (and (stringp mime)
                     (carriage-transport-gptel--attachment-image-p mime))
                (and inline-text-p
                     (stringp mime)
                     (carriage-transport-gptel--attachment-text-p mime p)))
     collect a)))

(defun carriage-transport-gptel--attachments-note-string (attachments)
  "Return note text for unsupported ATTACHMENTS, or nil."
  (let ((unsupported (and (listp attachments)
                          (carriage-transport-gptel--attachments-note-attachments attachments))))
    (when unsupported
      (dolist (a unsupported)
        (carriage-log "Transport[gptel] ATTACH-NOTE-ONLY path=%s mime=%s reason=%s"
                      (or (plist-get a :path) "-")
                      (or (plist-get a :mime)
                          (carriage-transport-gptel--attachment-mime (plist-get a :path))
                          "-")
                      (or (plist-get a :reason) "-")))
      (concat
       "Untransported attachments (metadata only):\n"
       (mapconcat
        (lambda (a)
          (let* ((p (plist-get a :path))
                 (r (plist-get a :reason))
                 (mime (or (plist-get a :mime)
                           (carriage-transport-gptel--attachment-mime p))))
            (format "- %s%s [mime=%s]"
                    (or p "")
                    (if r (format " (%s)" r) "")
                    (or mime "-"))))
        unsupported
        "\n")))))

(defun carriage-transport-gptel--maybe-append-attachments-note (system attachments)
  "Append attachment note for ATTACHMENTS that cannot be transported natively."
  (let ((note (carriage-transport-gptel--attachments-note-string attachments)))
    (if (not note)
        system
      (concat
       (or system "")
       (when (and (stringp system) (not (string-empty-p (string-trim system))))
         "\n\n")
       note))))

(defun carriage-transport-gptel--attachments->gptel-context (attachments model)
  "Convert ATTACHMENTS to `gptel-context' entries for image/media only.

Text-like attachments are inlined directly into the prompt by Carriage.
Generic non-image files are omitted from `gptel-context'."
  (when (and (listp attachments)
             (require 'mailcap nil t))
    (let ((media-capable (and (fboundp 'gptel--model-capable-p)
                              (gptel--model-capable-p 'media model))))
      (cl-loop
       for a in attachments
       for p = (plist-get a :path)
       for mime = (or (plist-get a :mime)
                      (and (stringp p) (mailcap-file-name-to-mime-type p)))
       if (and (stringp p)
               (stringp mime)
               (carriage-transport-gptel--attachment-image-p mime)
               media-capable
               (fboundp 'gptel--model-mime-capable-p)
               (gptel--model-mime-capable-p mime model))
       collect (list p :mime mime)
       else do
       (carriage-log "Transport[gptel] ATTACH-OMIT path=%s mime=%s reason=%s"
                     (or p "-")
                     (or mime "-")
                     (cond
                      ((and (stringp mime)
                            (carriage-transport-gptel--attachment-text-p mime p))
                       "text-inline")
                      (t "unsupported-openai-like-attachment")))))))

;; ---------------------------------------------------------------------
;; Dispatch entry-point

(defun carriage-transport-gptel--dispatch-validate (backend buffer)
  "Validate BACKEND and GPTel availability, else signal a user-error and complete BUFFER."
  (unless (eq (if (symbolp backend) backend (intern (format "%s" backend))) 'gptel)
    (carriage-log "Transport[gptel] backend mismatch: %S" backend)
    (carriage-transport-complete t buffer)
    (user-error "No transport adapter for backend: %s" backend))
  (unless (require 'gptel-request nil t)
    (carriage-log "Transport[gptel] gptel-request not available")
    (carriage-transport-complete t buffer)
    (user-error "GPTel is not available"))
  t)

(defun carriage-transport-gptel--dispatch-make-abort (buffer id)
  "Return an abort function bound to BUFFER and request ID."
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
              buffer id 'abort 'abort (list :status "abort-timeout") requested-model nil))))))))

(defun carriage-transport-gptel--dispatch-init (buffer id model source prompt abort-fn)
  "Initialize per-request state, logging, and abort handler."
  (let ((start-procs (carriage-transport-gptel--live-gptel-curl-procs)))
    (setq carriage-transport-gptel--finalized nil
          carriage-transport-gptel--finalized-kind nil)
    (carriage-transport-gptel--chunk-reset)
    (carriage-log "Transport[gptel] START id=%s source=%s model=%s prompt-bytes=%s"
                  id source (or model "—")
                  (if (stringp prompt) (number-to-string (string-bytes prompt)) "—"))
    (carriage-transport-gptel--log-procs "PROCS-START" id start-procs)
    (carriage-register-abort-handler abort-fn)))

(defun carriage-transport-gptel--dispatch-start-watchdog (buffer id abort-fn)
  "Start watchdog timers for BUFFER and ID using ABORT-FN."
  (carriage-transport-gptel--wd-start
   buffer id
   (lambda ()
     (ignore-errors
       (carriage-transport-gptel--log-gptel-proc-buffer buffer id "WD-TIMEOUT"))
     (funcall abort-fn)
     (carriage-transport-gptel--finalize
      buffer id 'timeout nil (list :status "timeout") nil))))


(defun carriage-transport-gptel--dispatch-prepare-payload (prompt system)
  "Sanitize payload and return (CONS PROMPT2 . SYSTEM2).

Transport must not inject typedblocks guidance; prompt structure is owned by
prompt builder."
  (let* ((prompt0 (or prompt ""))
         (system0 (or system ""))
         (prompt1 (carriage-transport--strip-internal-lines prompt0))
         (system1 (carriage-transport--strip-internal-lines system0))
         (prompt2 (carriage-transport-gptel--safe-utf8 prompt1))
         (system2 (carriage-transport-gptel--safe-utf8 system1)))
    (when (or (not (string= prompt0 prompt2))
              (not (string= system0 system2)))
      (carriage-log "Transport[gptel] WARN: payload sanitized (internal log lines or invalid bytes removed)")
      (carriage-traffic-log 'out "payload: sanitized: prompt-diff=%s system-diff=%s"
                            (if (string= prompt0 prompt2) "no" "yes")
                            (if (string= system0 system2) "no" "yes")))
    (carriage-log "Transport[gptel] PAYLOAD prepared prompt-bytes=%d system-bytes=%d mode=%s"
                  (string-bytes prompt2)
                  (string-bytes system2)
                  carriage-transport-gptel-openai-like-attachment-mode)
    (carriage-traffic-log 'out
                          "payload: begin_map present: system=%s prompt=%s"
                          (if (and (stringp system2)
                                   (string-match-p "#\\+begin_map\\b" system2))
                              "yes" "no")
                          (if (and (stringp prompt2)
                                   (string-match-p "#\\+begin_map\\b" prompt2))
                              "yes" "no"))
    (carriage-traffic-log 'out
                          "payload: bytes prompt=%d system=%d"
                          (string-bytes prompt2)
                          (string-bytes system2))
    (cons prompt2 system2)))

(defun carriage-transport-gptel--normalize-model (model)
  "Return MODEL normalized for GPTel as a symbol, or nil.
If MODEL is in \"backend[:provider]:model\" form, keep only the last segment."
  (let* ((raw (cond
               ((symbolp model) (symbol-name model))
               ((stringp model) model)
               ((null model) nil)
               (t (format "%s" model)))))
    (when (and (stringp raw) (not (string-empty-p (string-trim raw))))
      (let* ((s (string-trim raw))
             (parts (split-string s ":" t))
             (name (if (and parts (> (length parts) 0))
                       (car (last parts))
                     s)))
        (when (and (stringp name) (not (string-empty-p name)))
          (intern name))))))

(defun carriage-transport-gptel--model-string-from-info (info fallback-model)
  "Return a stable model string from INFO or FALLBACK-MODEL."
  (let* ((cand
          (or (and (listp info)
                   (or (plist-get info :model)
                       (plist-get info :requested-model)
                       (plist-get info :model-name)))
              fallback-model))
         (s (cond
             ((symbolp cand) (symbol-name cand))
             ((stringp cand) cand)
             ((null cand) nil)
             (t (format "%s" cand)))))
    (when (and (stringp s) (not (string-empty-p (string-trim s))))
      (string-trim s))))

(defun carriage-transport-gptel--dispatch--compose-prompt (prompt2 attachments)
  "Return composed prompt payload as plist.

Keys:
- :prompt (string) final prompt
- :inline-text (string or nil) inlined file contents block
- :note (string or nil) note-only attachments block"
  (let* ((inline-text (carriage-transport-gptel--attachments-inline-text-blocks attachments))
         (attachments-note (carriage-transport-gptel--attachments-note-string attachments))
         (prompt3 (concat
                   (or prompt2 "")
                   (when (and (stringp inline-text)
                              (not (string-empty-p (string-trim inline-text))))
                     (concat
                      (when (not (string-empty-p (string-trim (or prompt2 ""))))
                        "\n\n")
                      inline-text))
                   (when (and (stringp attachments-note)
                              (not (string-empty-p (string-trim attachments-note))))
                     (concat
                      (when (or (not (string-empty-p (string-trim (or prompt2 ""))))
                                (and (stringp inline-text)
                                     (not (string-empty-p (string-trim inline-text)))))
                        "\n\n")
                      attachments-note)))))
    (list :prompt prompt3 :inline-text inline-text :note attachments-note)))

(defun carriage-transport-gptel--dispatch--compose-context (attachments model)
  "Return gptel-context value with supported media ATTACHMENTS added (best-effort)."
  (if (boundp 'gptel-context)
      (append gptel-context
              (carriage-transport-gptel--attachments->gptel-context attachments model))
    nil))

(defun carriage-transport-gptel--dispatch--log-model (id model model-sym effective-model)
  "Log model selection details for request ID."
  (when carriage-transport-gptel-diagnostics
    (carriage-log "Transport[gptel] MODEL id=%s requested=%s normalized=%s effective=%s"
                  id
                  (carriage-transport-gptel--snip model 120)
                  (if model-sym (symbol-name model-sym) "nil")
                  (carriage-transport-gptel--snip effective-model 120)))
  (carriage-traffic-log 'out
                        "gptel-request: requested-model=%s normalized=%s effective=%s"
                        (carriage-transport-gptel--snip model 120)
                        (if model-sym (symbol-name model-sym) "nil")
                        (carriage-transport-gptel--snip effective-model 120)))

(defun carriage-transport-gptel--dispatch--attachments-summary (attachments inline-text-p)
  "Return human-readable one-line summary string for ATTACHMENTS."
  (mapconcat
   (lambda (a)
     (let* ((p (plist-get a :path))
            (mime (or (plist-get a :mime)
                      (and (stringp p)
                           (carriage-transport-gptel--attachment-mime p))))
            (mode (cond
                   ((and (stringp mime)
                         (carriage-transport-gptel--attachment-image-p mime))
                    "image/media")
                   ((and inline-text-p
                         (stringp mime)
                         (carriage-transport-gptel--attachment-text-p mime p))
                    "text-inline")
                   ((and (not inline-text-p)
                         (stringp mime)
                         (carriage-transport-gptel--attachment-text-p mime p))
                    "text/note-only")
                   (t "unsupported"))))
       (format "%s[%s]" (file-name-nondirectory (or p "-")) mode)))
   attachments
   ", "))

(defun carriage-transport-gptel--dispatch--log-attachments (id attachments inline-text attachments-note)
  "Log attachments details for request ID."
  (when (and carriage-transport-gptel-diagnostics (listp attachments) attachments)
    (let ((inline-text-p (carriage-transport-gptel--attachments-inline-text-enabled-p)))
      (carriage-log "Transport[gptel] ATTACH id=%s n=%d details=%s inline-bytes=%d note-bytes=%d"
                    id
                    (length attachments)
                    (carriage-transport-gptel--dispatch--attachments-summary attachments inline-text-p)
                    (if (stringp inline-text) (string-bytes inline-text) 0)
                    (if (stringp attachments-note) (string-bytes attachments-note) 0)))))

(defun carriage-transport-gptel--dispatch--call-gptel (prompt system buffer cb)
  "Call `gptel-request' with PROMPT/SYSTEM, writing into BUFFER, using callback CB."
  (gptel-request
      prompt
    :callback cb
    :buffer buffer
    :stream t
    :system system))

(defun carriage-transport-gptel--dispatch--effective-model (model)
  "Return plist (:model-sym SYM :effective MODEL) for MODEL."
  (let* ((model-sym (carriage-transport-gptel--normalize-model model))
         (effective (or model-sym gptel-model)))
    (list :model-sym model-sym :effective effective)))

(defun carriage-transport-gptel--dispatch--build-request (prompt2 system2 attachments effective-model)
  "Return request plist for dispatch.

Keys:
- :prompt (string)
- :system (string)
- :context (value for `gptel-context')
- :inline-text (string or nil)
- :note (string or nil)"
  (let* ((payload (carriage-transport-gptel--dispatch--compose-prompt prompt2 attachments))
         (prompt3 (plist-get payload :prompt))
         (inline-text (plist-get payload :inline-text))
         (attachments-note (plist-get payload :note))
         (context (carriage-transport-gptel--dispatch--compose-context attachments effective-model)))
    (list :prompt prompt3
          :system system2
          :context context
          :inline-text inline-text
          :note attachments-note)))

(defun carriage-transport-gptel--dispatch--log-request (id model model-sym effective-model request attachments)
  "Log all dispatch request attributes."
  (carriage-transport-gptel--dispatch--log-model id model model-sym effective-model)
  (carriage-transport-gptel--dispatch--log-attachments
   id attachments
   (plist-get request :inline-text)
   (plist-get request :note))
  (carriage-traffic-log 'out
                        "gptel-request: inline-attachments prompt-bytes=%d system-bytes=%d"
                        (string-bytes (or (plist-get request :prompt) ""))
                        (string-bytes (or (plist-get request :system) ""))))

(defun carriage-transport-gptel--dispatch--with-effective-model (model fn)
  "Call FN with (MODEL-SYM EFFECTIVE-MODEL) and with `gptel-model' bound.
FN is called as (FN MODEL-SYM EFFECTIVE-MODEL)."
  (let* ((sel (carriage-transport-gptel--dispatch--effective-model model))
         (model-sym (plist-get sel :model-sym))
         ;; Dynamic let-binding makes gptel-request use Carriage-selected model.
         (gptel-model (plist-get sel :effective)))
    (funcall fn model-sym gptel-model)))

(defun carriage-transport-gptel--dispatch--invoke-run (buffer id prompt2 system2 cb model attachments)
  "Internal body of `carriage-transport-gptel--dispatch-invoke'."
  (with-current-buffer buffer
    (carriage-transport-gptel--dispatch--with-effective-model
     model
     (lambda (model-sym effective-model)
       (let* ((t0 (float-time))
              (request (carriage-transport-gptel--dispatch--build-request
                        prompt2 system2 attachments effective-model))
              (t1 (float-time))
              ;; Best-effort: inject supported image/media attachments into gptel-context.
              (gptel-context (plist-get request :context)))
         (carriage-transport-gptel--dispatch--log-request
          id model model-sym effective-model request attachments)
         (carriage-transport-gptel--dispatch--call-gptel
          (plist-get request :prompt)
          (plist-get request :system)
          buffer
          cb)
         (when carriage-transport-gptel-diagnostics
           (carriage-log "Transport[gptel] PERF id=%s build-request=%.3fs call-gptel=%.3fs"
                         id
                         (- t1 t0)
                         (- (float-time) t1))))))))

(defun carriage-transport-gptel--dispatch-invoke--try (buffer id prompt2 system2 cb model attachments)
  "Attempt to start a GPTel request. Errors are handled by caller."
  (carriage-transport-gptel--dispatch--invoke-run
   buffer id prompt2 system2 cb model attachments))

(defun carriage-transport-gptel--dispatch-invoke--handle-start-error (buffer id model err)
  "Handle ERR when GPTel request could not be started."
  (carriage-log "Transport[gptel] START-ERROR id=%s err=%s" id (error-message-string err))
  (carriage-transport-gptel--finalize buffer id 'error nil (list :status "start-error") model err))

(defun carriage-transport-gptel--dispatch-invoke (buffer id prompt2 system2 cb model attachments)
  "Invoke GPTel request with prepared PROMPT2/SYSTEM2 and requested MODEL.
Finalizes with error on startup failures."
  (condition-case err
      (carriage-transport-gptel--dispatch-invoke--try
       buffer id prompt2 system2 cb model attachments)
    (error
     (carriage-transport-gptel--dispatch-invoke--handle-start-error buffer id model err)))
  t)

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
    (carriage-transport-gptel--dispatch-validate backend buffer)
    (with-current-buffer buffer
      (let* ((t0 (float-time))
             (cb (carriage-transport-gptel--make-callback buffer id model))
             (abort-fn (carriage-transport-gptel--dispatch-make-abort buffer id))
             (t1 (float-time))
             (attachments (carriage-transport-gptel--collect-attachments buffer))
             (t2 (float-time))
             (pair (carriage-transport-gptel--dispatch-prepare-payload prompt system))
             (t3 (float-time))
             (prompt2 (car pair))
             (system2 (cdr pair))
             (system3 (carriage-transport-gptel--maybe-append-attachments-note
                       system2 attachments)))
        (when carriage-transport-gptel-diagnostics
          (carriage-log "Transport[gptel] PERF id=%s make-cb+abort=%.3fs collect-attachments=%.3fs prepare-payload=%.3fs preinvoke-total=%.3fs"
                        id
                        (- t1 t0)
                        (- t2 t1)
                        (- t3 t2)
                        (- t3 t0)))
        (carriage-transport-gptel--dispatch-init buffer id model source prompt abort-fn)
        (carriage-transport-gptel--dispatch-start-watchdog buffer id abort-fn)
        (carriage-transport-gptel--dispatch-invoke buffer id prompt2 system3 cb model attachments)))
    t))

;; --- gptel accounting hooks --------------------------------------------------
;; We avoid changing gptel internals and instead wrap callbacks used by Carriage
;; buffers (carriage-mode).  This lets UI show:
;; - cost when known,
;; - otherwise tokens or bytes,
;; - and HTTP error code/text in status tooltip.

(require 'cl-lib)
(require 'subr-x)

(defun carriage-transport-gptel--int-or-nil (x)
  "Convert X to non-negative integer, or nil when unknown."
  (cond
   ((null x) nil)
   ((eq x :null) nil)
   ((integerp x) (and (>= x 0) x))
   ((stringp x)
    (when (string-match-p "\\`[0-9]+\\'" x)
      (string-to-number x)))
   (t nil)))

(defun carriage-transport-gptel--prompt-bytes (prompt)
  "Best-effort byte size of PROMPT (string/list), or nil."
  (cond
   ((stringp prompt) (string-bytes prompt))
   ((consp prompt)
    (cl-loop for s in prompt
             when (stringp s) sum (string-bytes s) into n
             finally return (and (numberp n) n)))
   (t nil)))

(defun carriage-transport-gptel--status->code+text (info)
  "Best-effort parse of HTTP status code/text from GPTel INFO.
Returns plist (:code INT|nil :text STRING|nil)."
  (let* ((code-raw (plist-get info :http-status))
         (status (plist-get info :status))
         (code (carriage-transport-gptel--int-or-nil code-raw))
         (text nil))
    (when (and (null code) (stringp status))
      (when (string-match "\\b\\([0-9][0-9][0-9]\\)\\b\\(.*\\)\\'" status)
        (setq code (carriage-transport-gptel--int-or-nil (match-string 1 status)))
        (setq text (string-trim (match-string 2 status)))))
    (when (and (null text) (stringp status))
      (setq text (string-trim status)))
    (list :code code :text (and (stringp text) (not (string-empty-p text)) text))))

(defun carriage-transport-gptel--error->string (info)
  "Extract a short backend error/status message from INFO, or nil."
  (let ((e (plist-get info :error))
        (s (plist-get info :status)))
    (cond
     ((and (stringp e) (not (string-empty-p e))) e)
     ((and e (not (stringp e))) (format "%S" e))
     ((and (stringp s) (not (string-empty-p s))) s)
     (t nil))))

(defun carriage-transport-gptel--model-id-from-info (info)
  "Extract model id/name from gptel INFO (best-effort), return string or nil."
  (when-let* ((data-buf (plist-get info :data))
              ((bufferp data-buf))
              (m (with-current-buffer data-buf
                   (and (boundp 'gptel-model) gptel-model))))
    (format "%s" m)))

(defun carriage-transport-gptel--note-metrics (buf response info bytes-in)
  "Update BUF-local accounting vars from RESPONSE/INFO (best-effort)."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (bound-and-true-p carriage-mode)
        ;; HTTP status/error
        (let* ((st (carriage-transport-gptel--status->code+text info))
               (code (plist-get st :code))
               (text (plist-get st :text))
               (err (carriage-transport-gptel--error->string info)))
          (setq-local carriage--last-http-status (and code (number-to-string code)))
          (setq-local carriage--last-http-status-text text)
          (setq-local carriage--last-backend-error err))
        ;; Model id (for tooltip/debug)
        (setq-local carriage--last-model-id (carriage-transport-gptel--model-id-from-info info))
        ;; Bytes out accounting (stream-safe)
        (cond
         ((stringp response)
          (setq-local carriage--last-bytes-out-acc
                      (+ (or carriage--last-bytes-out-acc 0)
                         (string-bytes response))))
         ((eq response t)
          ;; end of streaming: nothing to add, accumulator already has the total
          nil)
         (t nil))
        ;; Tokens (if gptel provides them; keep nil when unknown)
        (let* ((tin  (or (carriage-transport-gptel--int-or-nil (plist-get info :input-tokens))
                         (carriage-transport-gptel--int-or-nil (plist-get info :prompt-tokens))))
               (tout (or (carriage-transport-gptel--int-or-nil (plist-get info :output-tokens))
                         (carriage-transport-gptel--int-or-nil (plist-get info :completion-tokens))))
               (bout (cond
                      ((stringp response) (string-bytes response))
                      ((eq response t) (carriage-transport-gptel--int-or-nil carriage--last-bytes-out-acc))
                      (t nil)))
               (usage (list :tokens-in tin
                            :tokens-out tout
                            :bytes-in (carriage-transport-gptel--int-or-nil bytes-in)
                            :bytes-out (carriage-transport-gptel--int-or-nil bout))))
          (setq-local carriage--last-usage usage))))))

(defun carriage-transport-gptel--wrap-callback (orig-cb buf bytes-in)
  "Wrap ORIG-CB so we can capture metrics for BUF.
BYTES-IN is the prompt byte size estimate (may be nil)."
  (lambda (response info)
    ;; Capture metrics first, then delegate.
    (condition-case _e
        (carriage-transport-gptel--note-metrics buf response info bytes-in)
      (error nil))
    (when (functionp orig-cb)
      (funcall orig-cb response info))))

(with-eval-after-load 'gptel-request
  (when (fboundp 'gptel-request)
    (unless (advice-member-p 'carriage-transport-gptel--advice-gptel-request
                             'gptel-request)
      (defun carriage-transport-gptel--advice-gptel-request (orig &rest args)
        "Around-advice for `gptel-request' to capture metrics for Carriage buffers."
        (let* ((prompt (car args))
               (rest (cdr args))
               (buf (or (plist-get rest :buffer) (current-buffer)))
               (cb  (plist-get rest :callback)))
          ;; Only wrap when Carriage is active AND a callback is provided.
          ;; (If we force a callback when CB is nil, we would break gptel's default insertion.)
          (when (and (buffer-live-p buf)
                     (functionp cb)
                     (with-current-buffer buf (bound-and-true-p carriage-mode)))
            ;; Reset streaming accumulator at request start.
            (with-current-buffer buf
              (setq-local carriage--last-bytes-out-acc 0))
            (let ((bytes-in (carriage-transport-gptel--prompt-bytes prompt)))
              (setq rest (plist-put rest :callback
                                    (carriage-transport-gptel--wrap-callback cb buf bytes-in)))))
          (apply orig (append (list prompt) rest))))
      (advice-add 'gptel-request :around
                  #'carriage-transport-gptel--advice-gptel-request))))

;;;###autoload
(defun carriage-transport-gptel-emergency-cleanup (&optional kill-all reason)
  "Emergency cleanup of GPTel processes/state (best-effort).

When KILL-ALL is non-nil, delete all live processes whose names start with
\"gptel-curl\" and kill their process buffers. Also clears `gptel--request-alist'
when it exists.

REASON is an optional short string used for diagnostics/logging.

Return a plist:
  (:killed N :killed-buffers M :cleared-request-alist BOOL :reason REASON)

WARNING:
This is a global cleanup and may affect non-Carriage GPTel requests."
  (interactive "P")
  (let ((killed 0)
        (killed-bufs 0)
        (cleared nil)
        (why (cond
              ((stringp reason) reason)
              (reason (format "%s" reason))
              (t nil))))
    (condition-case _e
        (when kill-all
          (dolist (p (process-list))
            (when (and (processp p)
                       (memq (process-status p) '(run open connect))
                       (string-prefix-p "gptel-curl" (process-name p)))
              (let ((pb (process-buffer p)))
                (ignore-errors (delete-process p))
                (setq killed (1+ killed))
                (when (buffer-live-p pb)
                  (ignore-errors (kill-buffer pb))
                  (setq killed-bufs (1+ killed-bufs))))))
          (when (boundp 'gptel--request-alist)
            (setq gptel--request-alist nil)
            (setq cleared t)))
      (error nil))
    (ignore-errors
      (when (fboundp 'carriage-log)
        (carriage-log "Transport[gptel] emergency-cleanup kill-all=%s killed=%d killed-bufs=%d cleared-request-alist=%s reason=%s"
                      (if kill-all "t" "nil")
                      killed killed-bufs
                      (if cleared "t" "nil")
                      (or why "-"))))
    (list :killed killed
          :killed-buffers killed-bufs
          :cleared-request-alist (and cleared t)
          :reason why)))

(provide 'carriage-transport-gptel)
;;; carriage-transport-gptel.el ends here
