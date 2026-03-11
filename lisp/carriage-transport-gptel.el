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

(defun carriage-transport-gptel--finalize--log-end (id final code info resp reqn procs0)
  "Log the canonical END line for request ID."
  (let* ((status (and (listp info) (plist-get info :status)))
         (http (and (listp info)
                    (or (plist-get info :http-status)
                        (plist-get info :http_status))))
         (resp-kind (carriage-transport-gptel--response-kind resp)))
    (carriage-log
     "Transport[gptel] END id=%s final=%s code=%s status=%s http=%s resp=%s req-alist=%s procs0=%d"
     id final code
     (if status (carriage-transport-gptel--snip status 200) "—")
     (if http (format "%s" http) "—")
     resp-kind
     (if (numberp reqn) (number-to-string reqn) "—")
     (length procs0))))

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
  "Return best-effort usage plist from INFO, with FSM fallback."
  (let ((u (carriage-transport-gptel--usage-from-info info)))
    (if (or (plist-get u :tokens-in) (plist-get u :tokens-out))
        u
      (carriage-transport-gptel--usage-from-fsm origin-buffer))))

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
             (pl (if (plist-get usage :tokens-in)
                     (plist-put pl :CAR_TOKENS_IN (plist-get usage :tokens-in))
                   pl))
             (pl (if (plist-get usage :tokens-out)
                     (plist-put pl :CAR_TOKENS_OUT (plist-get usage :tokens-out))
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
      (when (and (fboundp 'carriage-fingerprint-note-usage-and-cost)
                 (or (plist-get usage :tokens-in) (plist-get usage :tokens-out)))
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

(defun carriage-transport-gptel--finalize
    (origin-buffer id final resp info requested-model &optional internal-error)
  "Finalize request and update UI. FINAL is one of 'done 'abort 'error 'timeout."
  (let ((final2 (carriage-transport-gptel--finalize--normalize-final id final)))
    (carriage-transport-gptel--finalize--call
     origin-buffer id final2 resp info requested-model internal-error)))

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
              (carriage-transport-gptel--finalize origin-buffer id 'done resp info requested-model))

             ;; Abort (explicit)
             ((eq resp 'abort)
              (setq finished t)
              (carriage-transport-gptel--finalize origin-buffer id 'abort resp info requested-model))

             ;; Empty terminal response: if HTTP already succeeded and provider error is absent,
             ;; treat this as a successful completion with empty textual content.
             ((null resp)
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

(defun carriage-transport-gptel--resolve-path (buffer p)
  "Resolve P (string) relative to project root when possible; otherwise to buffer's default-directory.
Return truename or nil if remote/non-existent."
  (when (and (stringp p) (not (string-empty-p (string-trim p))))
    (let* ((p (string-trim p)))
      (unless (file-remote-p p)
        (let* ((root (with-current-buffer buffer
                       (or (and (fboundp 'carriage-project-root)
                                (ignore-errors (carriage-project-root)))
                           default-directory)))
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
  (let* ((manual-lines
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
          (delq nil (mapcar (lambda (p) (carriage-transport-gptel--resolve-path buffer p))
                            manual-lines)))
         (auto-paths
          (cl-loop
           for p in (delq nil (mapcar (lambda (x) (carriage-transport-gptel--resolve-path buffer x))
                                      context-lines))
           for sz = (carriage-transport-gptel--file-size p)
           when (or (carriage-transport-gptel--file-binary-p p)
                    (and (numberp sz)
                         (numberp carriage-transport-gptel-auto-attach-size-threshold)
                         (> sz carriage-transport-gptel-auto-attach-size-threshold)))
           collect p))
         (all (delete-dups (append manual-paths auto-paths))))
    (carriage-log "Transport[gptel] ATTACH-COLLECT manual-lines=%d manual-paths=%d auto-paths=%d total=%d"
                  (length manual-lines) (length manual-paths) (length auto-paths) (length all))
    (cl-loop
     for p in all
     for sz = (carriage-transport-gptel--file-size p)
     collect (list :path p
                   :mime (carriage-transport-gptel--attachment-mime p)
                   :size-bytes (and (numberp sz) sz)
                   :reason (cond
                            ((member p manual-paths) 'manual)
                            ((carriage-transport-gptel--file-binary-p p) 'binary)
                            (t 'size-limit))))))

(defun carriage-transport-gptel--attachment-mime (path)
  "Return MIME type for PATH, or nil."
  (and (stringp path)
       (require 'mailcap nil t)
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
       (let* ((request (carriage-transport-gptel--dispatch--build-request
                        prompt2 system2 attachments effective-model))
              ;; Best-effort: inject supported image/media attachments into gptel-context.
              (gptel-context (plist-get request :context)))
         (carriage-transport-gptel--dispatch--log-request
          id model model-sym effective-model request attachments)
         (carriage-transport-gptel--dispatch--call-gptel
          (plist-get request :prompt)
          (plist-get request :system)
          buffer
          cb))))))

(defun carriage-transport-gptel--dispatch-invoke (buffer id prompt2 system2 cb model attachments)
  "Invoke GPTel request with prepared PROMPT2/SYSTEM2 and requested MODEL.
Finalizes with error on startup failures."
  (condition-case err
      (carriage-transport-gptel--dispatch--invoke-run
       buffer id prompt2 system2 cb model attachments)
    (error
     ;; Request could not be started at all.
     (carriage-log "Transport[gptel] START-ERROR id=%s err=%s" id (error-message-string err))
     (carriage-transport-gptel--finalize buffer id 'error nil (list :status "start-error") model err)))
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
        (let* ((cb (carriage-transport-gptel--make-callback buffer id model))
               (abort-fn (carriage-transport-gptel--dispatch-make-abort buffer id))
               (attachments (carriage-transport-gptel--collect-attachments buffer))
               (pair (carriage-transport-gptel--dispatch-prepare-payload prompt system))
               (prompt2 (car pair))
               (system2 (cdr pair))
               (system3 (carriage-transport-gptel--maybe-append-attachments-note
                         system2 attachments)))
          (carriage-transport-gptel--dispatch-init buffer id model source prompt abort-fn)
          (carriage-transport-gptel--dispatch-start-watchdog buffer id abort-fn)
          (carriage-transport-gptel--dispatch-invoke buffer id prompt2 system3 cb model attachments)))
      t))

  (provide 'carriage-transport-gptel)
;;; carriage-transport-gptel.el ends here
