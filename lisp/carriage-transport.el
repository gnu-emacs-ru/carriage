;;; carriage-transport.el --- Transport dispatcher and events  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: transport, llm
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/llm-transport-v2.org
;;   spec/tool-contracts-v2.org
;;   spec/logging-v2.org
;;   spec/security-v2.org
;;   spec/observability-v2.org
;;   spec/golden-fixtures-v2.org
;;
;;; Commentary:
;; Event-based transport dispatcher used by Carriage-mode.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-ui)
(require 'carriage-logging)
(require 'carriage-errors)
(require 'carriage-transport-watchdog)
(require 'carriage-transport-payload)
;; Break circular dependency with carriage-mode: call its fns via declare-function.
(declare-function carriage-register-abort-handler "carriage-mode" (fn))
(declare-function carriage-clear-abort-handler "carriage-mode" ())
(declare-function carriage--preloader-start "carriage-mode" ())
(declare-function carriage--preloader-stop "carriage-mode" ())

(defvar carriage--transport-loading-adapter nil
  "Guard to prevent recursive/layered adapter loading in transport dispatcher.")

;; -----------------------------------------------------------------------------
;; Request watchdog + diagnostics (transport-level, adapter-agnostic)
;;
;; Symptom addressed: UI stuck forever in 'sending/'streaming when adapter never
;; reaches `carriage-transport-complete' or when no progress arrives.
;;
;; Design:
;; - Watchdog is owned by transport layer, not by adapters.
;; - It tracks per-buffer request-id (rid) and last-progress time.
;; - It can be fed by:
;;     - `carriage-transport-streaming' (stream start),
;;     - `carriage-transport-dispatch' (request dispatched),
;;     - `carriage-transport-note-progress' (adapters/stream insertion),
;;     - `carriage-transport-complete' (finalization).
;; - On stall timeout it attempts abort-fn (if present) and *guarantees*
;;   collapsing the request into error via `carriage-transport-complete'.

(defgroup carriage-transport-watchdog nil
  "Watchdog and diagnostics for Carriage transport."
  :group 'carriage)

(defcustom carriage-transport-watchdog-enabled t
  "When non-nil, enable transport-level watchdog for stuck requests."
  :type 'boolean
  :group 'carriage-transport-watchdog)

(defcustom carriage-transport-watchdog-stall-seconds 90.0
  "Seconds of no progress after which the watchdog fails the request.

Progress is recorded via `carriage-transport-note-progress' and via
transport begin/dispatch/streaming/complete events."
  :type 'number
  :group 'carriage-transport-watchdog)

(defcustom carriage-transport-watchdog-check-interval 1.0
  "How often (seconds) the watchdog checks for stalling."
  :type 'number
  :group 'carriage-transport-watchdog)

(defcustom carriage-transport-watchdog-debug nil
  "When non-nil, emit extra diagnostics for watchdog/completion paths.

This is intended for debugging cases like:
- streaming announced but no chunks arrive,
- \"double finalize\" (same rid completed as ok and then error),
- watchdog abort interactions with adapter sentinels."
  :type 'boolean
  :group 'carriage-transport-watchdog)

(defcustom carriage-transport-progress-trace nil
  "When non-nil, log every `carriage-transport-note-progress' call with rid and timestamp.

This is very verbose; enable only for diagnosing stalls / watchdog timeouts."
  :type 'boolean
  :group 'carriage-transport-watchdog)

(defcustom carriage-transport-debug-in-file-log nil
  "When non-nil, log a short summary of `In file <path>:' sections present in SYSTEM text."
  :type 'boolean
  :group 'carriage-transport-watchdog)

(defun carriage-transport--short-backtrace (&optional max-lines)
  "Return a short backtrace string (best-effort) limited to MAX-LINES.
Never signals."
  (let ((max-lines (or max-lines 18)))
    (condition-case _e
        (with-temp-buffer
          (let ((standard-output (current-buffer)))
            (backtrace))
          (goto-char (point-min))
          (let ((i 0))
            (while (and (< i max-lines) (not (eobp)))
              (forward-line 1)
              (setq i (1+ i)))
            (buffer-substring-no-properties (point-min) (point))))
      (error nil))))

(defvar carriage-transport--request-counter 0
  "Monotonic counter used to generate transport request ids (rid).")

(defvar-local carriage-transport--request-id nil
  "Current transport request id (rid) for this buffer, or nil.")

(defvar-local carriage-transport--last-progress-at nil
  "Float-time of the last observed progress for this buffer's current request.")

(defvar-local carriage-transport--watchdog-timer nil
  "Timer object for transport watchdog in this buffer, or nil.")

(defvar-local carriage-transport--watchdog-fired nil
  "Non-nil if watchdog has fired for the current request.")

(defvar-local carriage-transport--watchdog-abort-fn nil
  "Abort function captured at `carriage-transport-begin' for watchdog use.")

(defvar-local carriage-transport--watchdog-abort-rid nil
  "RID for which `carriage-transport--watchdog-abort-fn' is valid.")

(defvar-local carriage-transport--watchdog-abort-rid nil
  "RID for which `carriage-transport--watchdog-abort-fn' is valid.")

(defvar-local carriage-transport--last-complete nil
  "Last completion record for this buffer (best-effort).
Plist keys: :ts :rid :errorp.")

(defvar-local carriage-transport--complete-count 0
  "Number of times `carriage-transport-complete' was called in this buffer.")

(defun carriage-transport--send-entry-current-p (entry-id &optional buffer)
  "Return non-nil when ENTRY-ID is still the current send owner for BUFFER.
This is used to ignore stale delayed/reentry callbacks from send-preparation."
  (with-current-buffer (or buffer (current-buffer))
    (and (stringp entry-id)
         (boundp 'carriage--current-send-entry-id)
         (stringp carriage--current-send-entry-id)
         (string= entry-id carriage--current-send-entry-id))))

(defun carriage-transport-invalidate-send-owner (&optional buffer)
  "Invalidate current send owner token for BUFFER.
This prevents stale preflight/reentry callbacks from starting a new send lifecycle
after the request has already been completed or aborted."
  (with-current-buffer (or buffer (current-buffer))
    (when (boundp 'carriage--current-send-entry-id)
      (setq carriage--current-send-entry-id nil))
    t))

(defun carriage-transport--rid-next ()
  "Generate a new transport request id (rid)."
  (setq carriage-transport--request-counter (1+ (or carriage-transport--request-counter 0)))
  (format "rid-%d" carriage-transport--request-counter))

(defun carriage-transport-current-request-id (&optional buffer)
  "Return current transport request id for BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    carriage-transport--request-id))

(defun carriage-transport-note-progress (&optional what buffer)
  "Record progress for watchdog in BUFFER (or current buffer).

WHAT is a diagnostic tag (symbol/string) used only for optional logging.
This function MUST be safe on hot paths (stream chunks) and MUST never signal."
  (condition-case _e
      (with-current-buffer (or buffer (current-buffer))
        (let ((ts (float-time))
              ;; Read rid directly from buffer-local bookkeeping (fast path).
              (rid (and (boundp 'carriage-transport--request-id)
                        carriage-transport--request-id)))
          (setq carriage-transport--last-progress-at ts)
          (when carriage-transport-progress-trace
            (carriage-log "Transport: progress rid=%s what=%s ts=%.3f buf=%s"
                          (or rid "-")
                          (if what (format "%s" what) "-")
                          (max 0.0 ts)
                          (buffer-name (current-buffer)))))
        t)
    (error nil)))

(defun carriage-transport--watchdog-stop (&optional buffer)
  "Stop watchdog timer for BUFFER (or current buffer). Best-effort."
  (with-current-buffer (or buffer (current-buffer))
    (when (timerp carriage-transport--watchdog-timer)
      (ignore-errors (cancel-timer carriage-transport--watchdog-timer)))
    (setq carriage-transport--watchdog-timer nil)
    t))

(defun carriage-transport--watchdog-trigger (rid idle stall &optional buffer)
  "Trigger watchdog failure for RID due to IDLE>=STALL in BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (unless carriage-transport--watchdog-fired
      (setq carriage-transport--watchdog-fired t)
      (carriage-log "Transport: watchdog timeout rid=%s idle=%.3fs stall=%.3fs (forcing error finalize)"
                    (or rid "-") (max 0.0 (or idle 0.0)) (max 0.0 (or stall 0.0)))
      (when carriage-transport-watchdog-debug
        (let ((st (and (boundp 'carriage--ui-state) carriage--ui-state))
              (lp (and (boundp 'carriage-transport--last-progress-at)
                       carriage-transport--last-progress-at)))
          (carriage-log "Transport: watchdog diag rid=%s buf=%s state=%s last-progress=%s req-id=%s"
                        (or rid "-")
                        (buffer-name (current-buffer))
                        (or st "-")
                        (if (numberp lp) (format "%.3f" lp) "-")
                        (or carriage-transport--request-id "-")))
        (when-let* ((bt (carriage-transport--short-backtrace 14)))
          (carriage-log "Transport: watchdog backtrace rid=%s\n%s" (or rid "-") bt)))
      ;; Try abort first (best-effort), then *guarantee* completion state change.
      (if (and (functionp carriage-transport--watchdog-abort-fn)
               (stringp rid)
               (stringp carriage-transport--watchdog-abort-rid)
               (string= rid carriage-transport--watchdog-abort-rid))
          (condition-case e
              (funcall carriage-transport--watchdog-abort-fn)
            (error
             (carriage-log "Transport: watchdog abort-fn error rid=%s: %s"
                           (or rid "-") (error-message-string e))))
        (when carriage-transport-watchdog-debug
          (carriage-log "Transport: watchdog abort-fn missing/stale rid=%s abort-rid=%s"
                        (or rid "-")
                        (or carriage-transport--watchdog-abort-rid "-"))))
      ;; Ensure timer does not re-fire while completion is running.
      (carriage-transport--watchdog-stop (current-buffer))
      (when carriage-transport-watchdog-debug
        (carriage-log "Transport: watchdog -> complete(error) rid=%s current-rid=%s"
                      (or rid "-")
                      (or (carriage-transport-current-request-id (current-buffer)) "-")))
      (ignore-errors (carriage-transport-complete t (current-buffer))))))

(defun carriage-transport--watchdog-start (&optional buffer)
  "Start watchdog timer for BUFFER (or current buffer) if enabled."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-transport--watchdog-stop (current-buffer))
    (when (and carriage-transport-watchdog-enabled
               (numberp carriage-transport-watchdog-stall-seconds)
               (> carriage-transport-watchdog-stall-seconds 0)
               (numberp carriage-transport-watchdog-check-interval)
               (> carriage-transport-watchdog-check-interval 0))
      (let* ((buf (current-buffer))
             (stall (float carriage-transport-watchdog-stall-seconds))
             (interval (float carriage-transport-watchdog-check-interval)))
        (setq carriage-transport--watchdog-timer
              (run-at-time
               interval interval
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let* ((rid carriage-transport--request-id)
                            (lp (or carriage-transport--last-progress-at (float-time)))
                            (idle (- (float-time) lp)))
                       ;; If request-id is gone, stop watchdog.
                       (when (or (null rid) (string-empty-p (format "%s" rid)))
                         (carriage-transport--watchdog-stop buf))
                       (when (and (stringp rid) (>= idle stall))
                         (carriage-transport--watchdog-trigger rid idle stall buf))))))))))
    t))

(defun carriage-transport--begin-new-request (abort-fn)
  "Initialize new request bookkeeping in current buffer."
  (carriage-transport--watchdog-stop (current-buffer))
  (setq carriage-transport--request-id (carriage-transport--rid-next))
  (setq carriage-transport--watchdog-fired nil)
  (setq carriage-transport--watchdog-abort-fn abort-fn)
  (setq carriage-transport--watchdog-abort-rid carriage-transport--request-id)
  (carriage-transport-note-progress 'begin (current-buffer))
  (carriage-transport--watchdog-start (current-buffer))
  carriage-transport--request-id)

(defun carriage-transport--debug-summarize-in-file (text)
  "Return a list of paths found in `In file <path>:' headers inside TEXT."
  (when (stringp text)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((acc '()))
        (while (re-search-forward "^In file \\(.+\\):[ \t]*$" nil t)
          (push (match-string-no-properties 1) acc))
        (nreverse (delete-dups acc))))))

;; Traffic logging (per-buffer buffers, summaries, and optional file logging)

(defgroup carriage-traffic nil
  "Traffic logging for Carriage (per-buffer buffers, summaries, file logging)."
  :group 'carriage-traffic)

;; Batch flush coordination (declared in carriage-traffic-batch.el; referenced here)
(defvar carriage-traffic-batch--flushing nil)
(defvar carriage-traffic-batch--flushed-buffers nil)

;; Global GC guard during streaming (set in transport-begin/complete)
(defvar carriage--gc-guard-active nil)
(defvar carriage--gc-guard-prev-threshold nil)

(defcustom carriage-traffic-summary-head-bytes 4096
  "Max bytes from the beginning of the response to include in summary."
  :type 'integer :group 'carriage-traffic)

(defcustom carriage-traffic-summary-tail-bytes 4096
  "Max bytes from the end of the response to include in summary."
  :type 'integer :group 'carriage-traffic)

(defcustom carriage-traffic-max-bytes 10048576
  "Approximate max size (in characters) for a traffic buffer before trimming from the top."
  :type 'integer :group 'carriage-traffic)

(defcustom carriage-traffic-log-to-file nil
  "When non-nil, also append traffic log lines to a file (see `carriage-traffic-log-file')."
  :type 'boolean :group 'carriage-traffic)

(defcustom carriage-traffic-log-file "carriage-traffic.log"
  "Path to traffic log file. Relative path is resolved against origin buffer's default-directory."
  :type 'string :group 'carriage-traffic)

(defcustom carriage-traffic-trim-on-append t
  "When non-nil, trim traffic buffers immediately on each append.
When nil, callers (e.g., batch flush) are responsible for triggering trim explicitly."
  :type 'boolean :group 'carriage-traffic)

(defun carriage--traffic--origin-name (origin-buffer)
  "Return a short human name for ORIGIN-BUFFER (file basename or buffer-name)."
  (with-current-buffer origin-buffer
    (or (and buffer-file-name (file-name-nondirectory buffer-file-name))
        (buffer-name origin-buffer)
        "buffer")))

(defun carriage--traffic-buffer-name (origin-buffer)
  "Return per-origin traffic buffer name."
  (format "*carriage-traffic:%s*" (carriage--traffic--origin-name origin-buffer)))

(defun carriage--traffic--ensure-buffer (origin-buffer)
  "Return the per-origin traffic buffer, creating it if necessary."
  (let* ((name (carriage--traffic-buffer-name origin-buffer))
         (buf  (get-buffer name)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create name))
      (with-current-buffer buf
        (special-mode)
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Carriage Traffic — %s\n\n" (carriage--traffic--origin-name origin-buffer))))))
    buf))

(defun carriage--traffic--trim-if-needed (buf)
  "Trim BUF from the beginning if it exceeds `carriage-traffic-max-bytes' (approx by chars)."
  (when (and (integerp carriage-traffic-max-bytes)
             (> carriage-traffic-max-bytes 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (when (> (buffer-size) carriage-traffic-max-bytes)
          (save-excursion
            (goto-char (point-min))
            (forward-char (max 0 (- (buffer-size) carriage-traffic-max-bytes)))
            (delete-region (point-min) (point))))))))

(defun carriage--traffic-append (origin-buffer text)
  "Append TEXT to per-origin traffic buffer and trim if needed. Also write to file if enabled.

When batching is active (carriage-traffic-batch--flushing), defer trimming to the
end of the batch and record the target buffer in `carriage-traffic-batch--flushed-buffers'."
  (let ((buf (carriage--traffic--ensure-buffer origin-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)
        (unless (string-suffix-p "\n" text) (insert "\n"))))
    (if carriage-traffic-batch--flushing
        (progn
          (when (buffer-live-p buf)
            (push buf carriage-traffic-batch--flushed-buffers)))
      (carriage--traffic--trim-if-needed buf)))
  (when carriage-traffic-log-to-file
    (condition-case _e
        (let* ((root (with-current-buffer origin-buffer default-directory))
               (path (if (file-name-absolute-p carriage-traffic-log-file)
                         carriage-traffic-log-file
                       (expand-file-name carriage-traffic-log-file root))))
          (make-directory (file-name-directory path) t)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-buffer
              (insert text)
              (unless (string-suffix-p "\n" text) (insert "\n"))
              (append-to-file (point-min) (point-max) path))))
      (error nil))))

(defun carriage-traffic-log-local (origin-buffer type fmt &rest args)
  "Log a formatted traffic line to ORIGIN-BUFFER's traffic buffer (and optional file).
TYPE is a symbol like 'out or 'in, used only as a prefix marker."
  (let ((prefix (pcase type ('out ">> ") ('in "<< ") (_ "-- "))))
    (carriage--traffic-append origin-buffer
                              (concat prefix (apply #'format fmt args)))))

(defun carriage-traffic-log-request (origin-buffer &rest plist)
  "Log a structured request to ORIGIN-BUFFER's traffic buffer.
PLIST keys: :backend :model :system :prompt :context (list of paths or structures)."
  (let* ((backend (plist-get plist :backend))
         (model   (plist-get plist :model))
         (system  (plist-get plist :system))
         (prompt  (plist-get plist :prompt))
         (context (plist-get plist :context)))
    (carriage--traffic-append origin-buffer (format "==== BEGIN REQUEST (%s:%s) ====" backend model))
    (when (stringp system)
      (carriage--traffic-append origin-buffer "---- SYSTEM ----")
      (carriage--traffic-append origin-buffer system))
    (when (stringp prompt)
      (carriage--traffic-append origin-buffer "---- PROMPT ----")
      (carriage--traffic-append origin-buffer prompt))
    ;; Diagnostic: make begin_map presence obvious in the request log.
    (let* ((has-map-sys (and (stringp system) (string-match-p "#\\+begin_map\\b" system)))
           (has-map-pr  (and (stringp prompt)  (string-match-p "#\\+begin_map\\b" prompt))))
      (carriage--traffic-append origin-buffer
                                (format "---- PROJECT MAP ---- begin_map: system=%s prompt=%s"
                                        (if has-map-sys "t" "nil")
                                        (if has-map-pr "t" "nil"))))
    (when context
      (carriage--traffic-append origin-buffer "---- CONTEXT FILES (content omitted) ----")
      (dolist (c context)
        (let* ((p (cond
                   ((stringp c) c)
                   ((and (listp c) (plist-get c :path)) (plist-get c :path))
                   ((and (listp c) (alist-get :path c)) (alist-get :path c))
                   (t (format "%S" c)))))
          (carriage--traffic-append origin-buffer (format "- %s (…)" p)))))
    (carriage--traffic-append origin-buffer "==== END REQUEST ====")))

(defun carriage-traffic-log-response-summary (origin-buffer total-text)
  "Log a summary (head…tail) for TOTAL-TEXT into ORIGIN-BUFFER's traffic buffer."
  (let* ((head-limit (max 0 (or carriage-traffic-summary-head-bytes 0)))
         (tail-limit (max 0 (or carriage-traffic-summary-tail-bytes 0)))
         (len (length (or total-text "")))
         (head (substring (or total-text "") 0 (min len head-limit)))
         (tail (if (> len tail-limit) (substring (or total-text "") (- len (min len tail-limit)) len) "")))
    (carriage--traffic-append origin-buffer "==== BEGIN RESPONSE (summary) ====")
    (when (> (length head) 0)
      (carriage--traffic-append origin-buffer "---- HEAD ----")
      (carriage--traffic-append origin-buffer head))
    (when (and (> (length tail) 0) (> len (+ (length head) (length tail))))
      (carriage--traffic-append origin-buffer "…"))
    (when (> (length tail) 0)
      (carriage--traffic-append origin-buffer "---- TAIL ----")
      (carriage--traffic-append origin-buffer tail))
    (carriage--traffic-append origin-buffer
                              (format "==== END RESPONSE (bytes=%d) ====" (string-bytes (or total-text ""))))))


;;;###autoload
(defun carriage-transport-begin (&optional abort-fn buffer)
  "Signal beginning of an async request: set UI to 'sending and install ABORT-FN.
ABORT-FN should be a zero-arg function that cancels the ongoing request.
Returns an unregister lambda that clears the handler when called.
When BUFFER is non-nil, operate in that buffer."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      ;; Always clear any previous abort handler (e.g., send-prep cancel handler)
      ;; so it cannot leak into the transport/request phase.
      (carriage-clear-abort-handler)
      (when (functionp abort-fn)
        (carriage-register-abort-handler abort-fn))
      (let ((rid (carriage-transport--begin-new-request abort-fn)))
        (carriage-log "Transport: begin rid=%s entry=%s (abort=%s)"
                      rid
                      (if (boundp 'carriage--current-send-entry-id)
                          (or carriage--current-send-entry-id "-")
                        "-")
                      (if (functionp abort-fn) "installed" "none")))
      ;; GC guard: raise threshold during request to reduce pauses.
      (unless carriage--gc-guard-active
        (setq carriage--gc-guard-prev-threshold gc-cons-threshold)
        (setq gc-cons-threshold most-positive-fixnum)
        (setq carriage--gc-guard-active t))
      (carriage-ui-set-state 'sending)
      ;; Start buffer preloader (if available) at the insertion point (guard duplicate overlays)
      (when (and (fboundp 'carriage--preloader-start)
                 (not (and (boundp 'carriage--preloader-overlay)
                           (overlayp carriage--preloader-overlay))))
        (ignore-errors (carriage--preloader-start))))
    ;; Return unregister lambda (bound to origin buffer)
    (lambda ()
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (carriage-clear-abort-handler)
          (carriage-transport--watchdog-stop buf)
          (carriage-log "Transport: unregister abort handler rid=%s"
                        (or (carriage-transport-current-request-id buf) "-")))))))

;;;###autoload
(defun carriage-transport-streaming (&optional buffer)
  "Signal that transport has progressed to streaming: update UI state (do not stop preloader).

When BUFFER is non-nil, operate in that buffer."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (carriage-transport-note-progress 'streaming buf)
      (carriage-log "Transport: streaming rid=%s"
                    (or (carriage-transport-current-request-id buf) "-"))
      (carriage-ui-set-state 'streaming)
      ;; Ensure spinner overlay is alive when streaming starts; render a frame if needed (best-effort).
      (when (and (fboundp 'carriage--preloader-start)
                 (boundp 'carriage-mode-preloader-enabled)
                 carriage-mode-preloader-enabled)
        (ignore-errors
          ;; If overlay/timer were not started for any reason, start now.
          (unless (and (boundp 'carriage--preloader-overlay)
                       (overlayp carriage--preloader-overlay))
            (carriage--preloader-start))
          ;; Draw a frame immediately to avoid waiting for timer tick.
          (when (fboundp 'carriage--preloader--render)
            (let* ((pos (or (and (boundp 'carriage--stream-end-marker)
                                 (markerp carriage--stream-end-marker)
                                 (marker-position carriage--stream-end-marker))
                            (and (boundp 'carriage--stream-origin-marker)
                                 (markerp carriage--stream-origin-marker)
                                 (marker-position carriage--stream-origin-marker))
                            (point))))
              (when (numberp pos)
                (ignore-errors (carriage--preloader--render pos))))))))))

;;;###autoload
(defun carriage-transport-complete (&optional errorp buffer)
  "Signal completion of an async request: clear abort handler and set UI state.
If ERRORP non-nil, set state to 'error; otherwise flash 'done then return to 'idle.
When BUFFER is non-nil, operate in that buffer (default is current buffer)."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (cl-block carriage-transport-complete
        (let ((rid0 (and (boundp 'carriage-transport--request-id)
                         carriage-transport--request-id))
              (st0 (and (boundp 'carriage--ui-state) carriage--ui-state))
              (wf0 (and (boundp 'carriage-transport--watchdog-fired)
                        carriage-transport--watchdog-fired)))
          ;; Always maintain lightweight completion bookkeeping to catch races:
          ;; watchdog finalize vs adapter finalize (ok→error for same rid, etc).
          (let* ((now (float-time))
                 (prev carriage-transport--last-complete)
                 (prev-ts (and (listp prev) (plist-get prev :ts)))
                 (prev-rid (and (listp prev) (plist-get prev :rid)))
                 (prev-err (and (listp prev) (plist-get prev :errorp)))
                 (dt (and (numberp prev-ts) (- now prev-ts))))
            (setq carriage-transport--complete-count (1+ (or carriage-transport--complete-count 0)))
            (setq carriage-transport--last-complete (list :ts now :rid rid0 :errorp (and errorp t)))

            ;; Case 1: complete called after rid already cleared (typical for late finalize
            ;; from a previous request). Log it, but do not mutate current UI/state.
            (when (null rid0)
              (carriage-log "Transport: complete(NO-RID) errorp=%s prev-rid=%s prev-errorp=%s dt=%.3fs state=%s wfired=%s buf=%s"
                            (if errorp "t" "nil")
                            (or prev-rid "-")
                            (if prev-err "t" "nil")
                            (if (numberp dt) (max 0.0 dt) 0.0)
                            (or st0 "-")
                            (if wf0 "t" "nil")
                            (buffer-name buf))
              (when-let* ((bt (carriage-transport--short-backtrace 10)))
                (carriage-log "Transport: complete(NO-RID) backtrace\n%s" bt))
              ;; Late completion from an already-finished or aborted request: diagnostics only.
              ;; Must not touch current UI/request flags.
              (carriage-transport--watchdog-stop buf)
              (cl-return-from carriage-transport-complete t))

            ;; Case 2: duplicate complete for the same rid (ok→error or error→ok).
            ;; The first finalize wins; later calls are diagnostics-only.
            (when (and (stringp rid0) (stringp prev-rid) (string= rid0 prev-rid))
              (carriage-log "Transport: DUP complete rid=%s prev-errorp=%s new-errorp=%s dt=%.3fs state=%s wfired=%s buf=%s"
                            rid0
                            (if prev-err "t" "nil")
                            (if errorp "t" "nil")
                            (if (numberp dt) (max 0.0 dt) 0.0)
                            (or st0 "-")
                            (if wf0 "t" "nil")
                            (buffer-name buf))
              (when-let* ((bt (carriage-transport--short-backtrace 10)))
                (carriage-log "Transport: complete(DUP) backtrace\n%s" bt))
              ;; Duplicate completion for the active rid must not re-run cleanup/state flip.
              (carriage-transport--watchdog-stop buf)
              (cl-return-from carriage-transport-complete t)))

          ;; Keep existing verbose watchdog debug logs as-is.
          (when (and carriage-transport-watchdog-debug (null rid0))
            (carriage-log "Transport: complete called with no active rid (errorp=%s state=%s watchdog-fired=%s buf=%s)"
                          (if errorp "t" "nil") (or st0 "-") (if wf0 "t" "nil") (buffer-name buf)))
          (when carriage-transport-watchdog-debug
            (when-let* ((bt (carriage-transport--short-backtrace 14)))
              (carriage-log "Transport: complete backtrace rid=%s errorp=%s state=%s watchdog-fired=%s\n%s"
                            (or rid0 "-") (if errorp "t" "nil") (or st0 "-") (if wf0 "t" "nil") bt))))

        ;; Stop watchdog first to prevent late re-fire during finalization.
        (carriage-transport--watchdog-stop buf)
        (carriage-transport-note-progress 'complete buf)
        (let* ((rid (or (carriage-transport-current-request-id buf) "-"))
               (lp (or carriage-transport--last-progress-at (float-time)))
               (idle (- (float-time) lp)))
          (carriage-clear-abort-handler)
          ;; Ensure preloader is stopped on finalize.
          (when (fboundp 'carriage--preloader-stop)
            (ignore-errors (carriage--preloader-stop)))
          ;; Last-resort: when GPTel request ends in error, force an emergency cleanup of gptel/curl
          ;; to avoid \"stuck until Emacs restart\" due to lingering gptel-curl processes.
          (when errorp
            (ignore-errors
              (when (require 'carriage-transport-gptel nil t)
                (when (fboundp 'carriage-transport-gptel-emergency-cleanup)
                  (carriage-transport-gptel-emergency-cleanup t "transport-complete/error")))))
          ;; Restore GC threshold and schedule a light GC after completion.
          (when carriage--gc-guard-active
            (setq gc-cons-threshold (or carriage--gc-guard-prev-threshold gc-cons-threshold))
            (setq carriage--gc-guard-prev-threshold nil)
            (setq carriage--gc-guard-active nil)
            (run-at-time 0.2 nil (lambda () (ignore-errors (garbage-collect)))))
          (if errorp
              (carriage-ui-set-state 'error)
            (carriage-ui-set-state 'idle))
          (carriage-log "Transport: complete rid=%s entry=%s (status=%s idle=%.3fs)"
                        rid
                        (if (boundp 'carriage--current-send-entry-id)
                            (or carriage--current-send-entry-id "-")
                          "-")
                        (if errorp "error" "ok")
                        (max 0.0 idle))
          ;; Clear request bookkeeping (so stale rid can't be reused).
          (setq carriage-transport--request-id nil)
          (setq carriage-transport--watchdog-abort-fn nil)
          (setq carriage-transport--watchdog-abort-rid nil)
          ;; Invalidate send ownership before reopening the gate, so stale
          ;; send-prepare/reentry callbacks cannot resurrect the just-finished send.
          (carriage-transport-invalidate-send-owner buf)
          ;; Update conversation state for multi-turn awareness
          ;; This tracks modified files so next iteration's prompt includes state summary
          (when (boundp 'carriage-transport--conversation-state)
            (ignore-errors
              (carriage-transport--update-conversation-state
               (let ((rep (and (boundp 'carriage--last-apply-report) carriage--last-apply-report)))
                 rep)
               buf)))
          ;; Allow the next send only after the current request is fully finalized.
          (when (boundp 'carriage--send-in-flight)
            (setq carriage--send-in-flight nil))

          (when (boundp 'carriage--send-dispatch-scheduled)
            (setq carriage--send-dispatch-scheduled nil))
          (when (boundp 'carriage--active-send-generation)
            (setq carriage--active-send-generation nil))
          (when (boundp 'carriage--apply-entry-id)
            (setq carriage--apply-entry-id nil))
          (when (boundp 'carriage--apply-entry-log-count)
            (setq carriage--apply-entry-log-count 0))
          t)))))

;;;###autoload
(defun carriage-transport-dispatch (&rest args)
  "Dispatch request ARGS to transport adapter with safe lazy loading.

Injects conversation state summary into system prompt to prevent LLM from
re-patching already-modified files in multi-turn conversations."
  ;; Inject conversation state summary into system prompt
  (let* ((system (plist-get args :system))
         (buffer (plist-get args :buffer)))
    (when (stringp system)
      (let ((injected (carriage-transport--inject-conversation-summary system)))
        (setq args (plist-put args :system injected)))))
  "Dispatch request ARGS to transport adapter with safe lazy loading.

Contract:
- Prefer direct entry-point call (carriage-transport-<backend>-dispatch) when fboundp.
- If missing, attempt one-shot lazy load of adapter (guarded), then call entry-point.
- No recursion, no reliance on function cell replacement."
  (carriage-transport-note-progress 'dispatch (plist-get args :buffer))
  (when carriage-transport-debug-in-file-log
    (let ((paths (carriage-transport--debug-summarize-in-file (plist-get args :system))))
      (carriage-log "Transport: In file sections in SYSTEM: %S" paths)))
  (carriage-traffic-log 'out "dispatch request rid=%s: %S"
                        (or (carriage-transport-current-request-id (plist-get args :buffer)) "-")
                        args)
  (let* ((backend (plist-get args :backend))
         (bsym (cond
                ((symbolp backend) backend)
                ((stringp backend) (intern backend))
                (t (ignore-errors (intern (format "%s" backend)))))))
    (pcase bsym
      ;; GPTel backend
      ('gptel
       (cond
        ((fboundp 'carriage-transport-gptel-v2-dispatch)
         (apply #'carriage-transport-gptel-v2-dispatch args))
        ((not carriage--transport-loading-adapter)
         (let* ((carriage--transport-loading-adapter t))
           (when (and (require 'gptel nil t)
                      (require 'carriage-transport-gptel nil t))
             (carriage-log "Transport: gptel adapter loaded on demand"))
           (if (fboundp 'carriage-transport-gptel-v2-dispatch)
               (apply #'carriage-transport-gptel-v2-dispatch args)
             (carriage-log "Transport: no gptel entry-point; request dropped")
             (carriage-transport-complete t)
             (user-error "No transport adapter installed (gptel)"))))
        (t
         (carriage-log "Transport: adapter loading already in progress; dropping")
         (carriage-transport-complete t)
         (user-error "No transport adapter installed (gptel)"))))
      ;; Echo backend (dev)
      ('echo
       (cond
        ((fboundp 'carriage-transport-echo-dispatch)
         (apply #'carriage-transport-echo-dispatch args))
        ((not carriage--transport-loading-adapter)
         (let* ((carriage--transport-loading-adapter t))
           (when (require 'carriage-transport-echo nil t)
             (carriage-log "Transport: echo adapter loaded on demand"))
           (if (fboundp 'carriage-transport-echo-dispatch)
               (apply #'carriage-transport-echo-dispatch args)
             (carriage-log "Transport: no echo entry-point; request dropped")
             (carriage-transport-complete t)
             (user-error "No transport adapter installed (echo)"))))
        (t
         (carriage-log "Transport: adapter loading already in progress; dropping")
         (carriage-transport-complete t)
         (user-error "No transport adapter installed (echo)"))))
      ;; Unknown backend
      (_
       (carriage-log "Transport: unknown backend=%s" bsym)
       (signal (carriage-error-symbol 'LLM_E_BACKEND) (list (format "Unknown transport backend: %s" bsym)))
       (carriage-transport-complete t)
       (user-error "Unknown transport backend: %s" bsym)))))

(defun carriage-transport--payload-summarize-patch-blocks-fallback (text)
  "Transport-local fallback: summarize #+begin_patch blocks into one-line history markers.

Used only when the canonical `carriage--payload-summarize-patch-blocks'
is unavailable. Must never signal."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let ((case-fold-search t)
          (rx-end "^[ \t]*#\\+end_patch\\b"))
      (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
        (let* ((beg (line-beginning-position))
               (hdr-line (buffer-substring-no-properties beg (line-end-position)))
               (hdr nil)
               (keep nil))
          ;; Parse optional header plist from the begin_patch line.
          (when (string-match "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" hdr-line)
            (let* ((sexp (match-string 1 hdr-line))
                   (obj (condition-case _e
                            (car (read-from-string sexp))
                          (error nil))))
              (when (listp obj)
                (setq hdr obj)
                (setq keep (plist-get obj :keep)))))
          (if keep
              ;; Keep block as-is; just jump past its end (or EOF).
              (progn
                (forward-line 1)
                (if (re-search-forward rx-end nil t)
                    (forward-line 1)
                  (goto-char (point-max))))
            ;; Replace the whole block with a one-line marker.
            (let* ((end
                    (save-excursion
                      (goto-char (1+ (line-end-position)))
                      (if (re-search-forward rx-end nil t)
                          (min (point-max) (1+ (line-end-position)))
                        (point-max))))
                   (desc0 (or (and (listp hdr)
                                   (or (plist-get hdr :description)
                                       (plist-get hdr :result)))
                              (and (listp hdr) (plist-get hdr :applied) "Applied")
                              "(no description)"))
                   (desc (string-trim (format "%s" desc0)))
                   (desc (if (string-empty-p desc) "(no description)" desc))
                   (target
                    (cond
                     ((and (listp hdr) (plist-get hdr :path)) (plist-get hdr :path))
                     ((and (listp hdr) (plist-get hdr :file)) (plist-get hdr :file))
                     ((and (listp hdr)
                           (or (plist-get hdr :from) (plist-get hdr :to)))
                      (let ((f (plist-get hdr :from))
                            (t2 (plist-get hdr :to)))
                        (string-trim
                         (format "%s → %s"
                                 (or (and (stringp f) f) "-")
                                 (or (and (stringp t2) t2) "-")))))
                     (t "-")))
                   (line (format ";; patch history: %s — %s" target desc)))
              (delete-region beg end)
              (goto-char beg)
              (insert line "\n"))))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defvar carriage-transport--conversation-state nil
  "Conversation state summary for multi-turn awareness.
Plist keys:
  :iterations — number of send iterations in current conversation
  :modified-files — list of paths modified in last iteration
  :last-ts — timestamp of last completion

This is injected into system prompt to prevent LLM from re-patching already-modified files.")

(defun carriage-transport--update-conversation-state (report &optional buffer)
  "Update conversation state from apply/dry-run REPORT.
Called after each completion to track modified files for next iteration."
  (let* ((items (and (listp report) (plist-get report :items)))
         (modified
          (when (listp items)
            (delq nil
                  (mapcar
                   (lambda (it)
                     (when (eq (plist-get it :status) 'ok)
                       (or (plist-get it :path) (plist-get it :file))))
                   items)))))
    (setq carriage-transport--conversation-state
          (list :iterations (1+ (or (and (listp carriage-transport--conversation-state)
                                         (plist-get carriage-transport--conversation-state :iterations))
                                    0))
                :modified-files modified
                :last-ts (format-time-string "%Y-%m-%d %H:%M:%S")
                :buffer (or buffer (current-buffer))))))

(defun carriage-transport--format-conversation-summary ()
  "Format conversation state into LLM-facing summary string.
Returns nil if no conversation state exists."
  (when (and (listp carriage-transport--conversation-state)
             (plist-get carriage-transport--conversation-state :modified-files))
    (let* ((state carriage-transport--conversation-state)
           (iter (plist-get state :iterations))
           (files (plist-get state :modified-files))
           (ts (plist-get state :last-ts))
           (file-list (mapconcat (lambda (f) (format "- %s" f)) files "\n")))
      (concat ";; Conversation state:\n"
              (format ";; - Iteration: %d\n" iter)
              (format ";; - Last modified: %s\n" ts)
              ";; - Files modified in previous iteration(s):\n"
              file-list
              "\n;; IMPORTANT: Do not re-patch files listed above. Their content has already been updated.\n"))))

(defun carriage-transport--inject-conversation-summary (text)
  "Inject conversation state summary into TEXT (system prompt).
If no conversation state exists, returns TEXT unchanged."
  (let ((summary (carriage-transport--format-conversation-summary)))
    (if (and summary (stringp text))
        (concat summary text)
      text)))

(defun carriage-transport--strip-internal-lines (text)
  "Remove internal Carriage marker lines from TEXT (best-effort, centralized).

Policy: applied patch bodies must never be sent to the LLM (only headers/metadata may remain).

Strips:
- Org property headers for CARRIAGE_STATE / CARRIAGE_FINGERPRINT
- Inline CARRIAGE_FINGERPRINT lines"
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let ((case-fold-search t))
      ;; Doc-state / fingerprint / iteration in Org property headers
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+\\(CARRIAGE_STATE\\|CARRIAGE_FINGERPRINT\\)\\b.*$" nil t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position)))))
      ;; Per-send inline fingerprint/iteration
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+\\(CARRIAGE_FINGERPRINT\\)\\b.*$" nil t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position)))))

      ;; Patch blocks must never leak into prompts (history is summarized to one-line markers).
      ;; Prefer full patch-history collapse (all patches) when available; fall back
      ;; to applied-only collapse on older builds.
      (let ((s (buffer-substring-no-properties (point-min) (point-max))))
        (cond
         ((fboundp 'carriage--payload-summarize-patch-blocks)
          (erase-buffer)
          (insert (carriage--payload-summarize-patch-blocks s)))
         ((fboundp 'carriage-transport--payload-summarize-patch-blocks-fallback)
          (erase-buffer)
          (insert (carriage-transport--payload-summarize-patch-blocks-fallback s)))
         ((fboundp 'carriage--payload-summarize-applied-patches)
          (erase-buffer)
          (insert (carriage--payload-summarize-applied-patches s))))))
    ;; Also strip any accidentally pasted transport diagnostic lines to avoid polluting prompts.
    ;; Example: \"Transport[gptel] …\" lines copied from *carriage-log*/*carriage-traffic*.
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*Transport\\[[^]\n]+\\].*$" nil t)
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position)))))
    (buffer-substring-no-properties (point-min) (point-max))))


(provide 'carriage-transport)
;;; carriage-transport.el ends here
