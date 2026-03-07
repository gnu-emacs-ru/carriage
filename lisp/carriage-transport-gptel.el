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
(require 'carriage-typedblocks)

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
          ;; Log only keys and a redacted summary for info to avoid raw byte-code in logs
          (carriage-log "Transport[gptel] LAST id=%s info-keys=%S" id
                        (carriage-transport-gptel--plist-keys info))
          (carriage-log "Transport[gptel] LAST id=%s info-redacted=%s" id
                        (carriage-transport-gptel--redact info 300)))

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
        ;; Record result line with usage/model/cost for doc-cost aggregation
        ;; Pre-log pricing to ensure visibility even if later block errors out early.
        ;; We log with whatever model is known and no usage; the main block below will
        ;; still compute full usage/cost and may log another 'pricing:' line.
        (let* ((mdl-raw (and (listp info) (plist-get info :model)))
               (model-str (and mdl-raw (format "%s" mdl-raw))))
          (ignore-errors
            (carriage-transport-gptel--pricing-compute-and-log origin-buffer model-str nil)))
        (ignore-errors
          (let* ((mdl-raw (and (listp info) (plist-get info :model)))
                 (model-str (and mdl-raw (format "%s" mdl-raw)))
                 ;; Robust token extraction: consider multiple possible INFO/usage keys,
                 ;; accept numbers or numeric strings and sanitize to integers.
                 (uobj (and (listp info) (plist-get info :usage)))
                 (raw-out
                  (let ((v (and (listp info)
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
                                             (plist-get uobj :tokens_out)))))))
                    (cond
                     ((numberp v) v)
                     ((and (stringp v) (string-match-p "\\`[0-9]+\\'" v)) (string-to-number v))
                     (t nil))))
                 (raw-in
                  (let ((v (and (listp info)
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
                                             (plist-get uobj :tokens_in)))))))
                    (cond
                     ((numberp v) v)
                     ((and (stringp v) (string-match-p "\\`[0-9]+\\'" v)) (string-to-number v))
                     (t nil))))
                 (out (and (numberp raw-out) (>= raw-out 0) (truncate raw-out)))
                 (in  (and (numberp raw-in)  (>= raw-in 0)  (truncate raw-in)))
                 (usage (let (u)
                          (when (integerp in)  (setq u (plist-put u :tokens-in in)))
                          (when (integerp out) (setq u (plist-put u :tokens-out out)))
                          u))
                 ;; Fallback: if usage is missing, try to extract from GPTel FSM info.
                 (dummy-fallback
                  (when (and (not (plist-get usage :tokens-in))
                             (not (plist-get usage :tokens-out)))
                    (when-let* ((fsm (carriage-transport-gptel--gptel-fsm-for-buffer origin-buffer))
                                (info2 (ignore-errors (gptel-fsm-info fsm))))
                      (let* ((uobj2 (and (listp info2) (plist-get info2 :usage)))
                             (vout (and (listp info2)
                                        (or (plist-get info2 :output-tokens)
                                            (plist-get info2 :completion-tokens)
                                            (plist-get info2 :tokens-out)
                                            (plist-get info2 :output_tokens)
                                            (plist-get info2 :tokens_out)
                                            (and (listp uobj2)
                                                 (or (plist-get uobj2 :output-tokens)
                                                     (plist-get uobj2 :completion-tokens)
                                                     (plist-get uobj2 :tokens-out)
                                                     (plist-get uobj2 :completion_tokens)
                                                     (plist-get uobj2 :output_tokens)
                                                     (plist-get uobj2 :tokens_out))))))
                             (vin  (and (listp info2)
                                        (or (plist-get info2 :input-tokens)
                                            (plist-get info2 :prompt-tokens)
                                            (plist-get info2 :tokens-in)
                                            (plist-get info2 :input_tokens)
                                            (plist-get info2 :tokens_in)
                                            (and (listp uobj2)
                                                 (or (plist-get uobj2 :input-tokens)
                                                     (plist-get uobj2 :prompt-tokens)
                                                     (plist-get uobj2 :tokens-in)
                                                     (plist-get uobj2 :prompt_tokens)
                                                     (plist-get uobj2 :input_tokens)
                                                     (plist-get uobj2 :tokens_in)))))))
                        (setq out (and (numberp vout) (>= vout 0) (truncate vout)))
                        (setq in  (and (numberp vin)  (>= vin 0)  (truncate vin)))
                        (setq usage nil)
                        (when (integerp in)  (setq usage (plist-put usage :tokens-in in)))
                        (when (integerp out) (setq usage (plist-put usage :tokens-out out)))))))
                 (status (pcase final ('done 'done) ('abort 'abort) ('timeout 'timeout) (_ 'error)))
                 (cost nil))
            ;; Best-effort pricing (no network). If pricing is unavailable, cost remains nil.
            ;; Always produce a 'pricing:' line with status (or explicit skip reason) for diagnostics.
            (setq cost (carriage-transport-gptel--pricing-compute-and-log origin-buffer model-str usage))
            ;; Best-effort: also write usage+cost into fingerprint for doc-cost aggregation
            (when (and (fboundp 'carriage-fingerprint-note-usage-and-cost)
                       (or (plist-get usage :tokens-in) (plist-get usage :tokens-out)))
              (ignore-errors
                (carriage-fingerprint-note-usage-and-cost usage 'gptel nil model-str)))
            ;; Insert #+CARRIAGE_RESULT near end of buffer
            (with-current-buffer origin-buffer
              (save-excursion
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (let* ((pl (list
                            :CAR_REQ_ID id
                            :CAR_STATUS status
                            :CAR_BACKEND 'gptel
                            :CAR_PROVIDER nil))
                       ;; Include model only when user opts in (avoid duplication with fingerprint)
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
                       ;; KNOWN = true only when total cost is computed (integer)
                       (pl (let ((tt (and cost (plist-get cost :cost-total-u))))
                             (plist-put pl :CAR_COST_KNOWN (and (integerp tt) t))))
                       (pl (plist-put pl :CAR_TS (float-time))))
                  (insert (format "#+CARRIAGE_RESULT: %S\n" pl)))))
            ;; Trigger doc-cost refresh (debounced)
            (when (fboundp 'carriage-ui-doc-cost-schedule-refresh)
              (ignore-errors
                (with-current-buffer origin-buffer
                  (carriage-ui-doc-cost-schedule-refresh 0.05))))
            ;; Refresh doc-state fold overlays (best-effort; show CARRIAGE_RESULT nicely)
            (when (fboundp 'carriage-doc-state-summary-refresh)
              (ignore-errors
                (with-current-buffer origin-buffer
                  (carriage-doc-state-summary-refresh origin-buffer)))))))
      t)))

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
              buffer id 'abort 'abort (list :status "abort-timeout") nil))))))))

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
  "Build system prompt fragment, sanitize payload and return (CONS PROMPT2 . SYSTEM2)."
  (let* ((use-typedblocks-hint
          (and (boundp 'carriage-mode-typedblocks-structure-hint)
               carriage-mode-typedblocks-structure-hint))
         (sys-frag
          (and use-typedblocks-hint
               (fboundp 'carriage-typedblocks-prompt-fragment-v1)
               (carriage-typedblocks-prompt-fragment-v1)))
         ;; Avoid double-injecting when the prompt builder already included it.
         (sys-frag
          (if (and (stringp system)
                   (string-match-p "Org Typed Blocks v1" system))
              nil
            sys-frag))
         (system* (cond
                   ((and (stringp system) (> (length system) 0))
                    (if sys-frag (concat sys-frag "\n\n" system) system))
                   (sys-frag sys-frag)
                   (t system))))
    (when (and sys-frag (not (equal system* system)))
      (carriage-log "Transport[gptel] inject typedblocks-v1 fragment into :system"))
    (let* ((prompt0 (or prompt ""))
           (system0 (or system* ""))
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
      (cons prompt2 system2))))

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

(defun carriage-transport-gptel--dispatch-invoke (buffer id prompt2 system2 cb model)
  "Invoke GPTel request with prepared PROMPT2/SYSTEM2 and requested MODEL.
Finalizes with error on startup failures."
  (condition-case err
      (let* ((model-sym (carriage-transport-gptel--normalize-model model))
             (gptel-backend (and (boundp 'gptel-backend) gptel-backend))
             ;; Dynamic let-binding makes gptel-request use Carriage-selected model.
             (gptel-model (or model-sym gptel-model)))
        (ignore gptel-backend)
        (when carriage-transport-gptel-diagnostics
          (carriage-log "Transport[gptel] MODEL id=%s requested=%s normalized=%s"
                        id
                        (carriage-transport-gptel--snip model 120)
                        (if model-sym (symbol-name model-sym) "nil")))
        (gptel-request
            prompt2
          :callback cb
          :buffer buffer
          :stream t
          :system system2))
    (error
     ;; Request could not be started at all.
     (carriage-log "Transport[gptel] START-ERROR id=%s err=%s" id (error-message-string err))
     (carriage-transport-gptel--finalize buffer id 'error nil (list :status "start-error") err)))
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
      (let* ((cb (carriage-transport-gptel--make-callback buffer id))
             (abort-fn (carriage-transport-gptel--dispatch-make-abort buffer id))
             (pair (carriage-transport-gptel--dispatch-prepare-payload prompt system buffer))
             (prompt2 (car pair))
             (system2 (cdr pair)))
        (carriage-transport-gptel--dispatch-init buffer id model source prompt abort-fn)
        (carriage-transport-gptel--dispatch-start-watchdog buffer id abort-fn)
        (carriage-transport-gptel--dispatch-invoke buffer id prompt2 system2 cb model)))
    t))

(provide 'carriage-transport-gptel)
;;; carriage-transport-gptel.el ends here
