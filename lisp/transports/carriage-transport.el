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
;; Break circular dependency with carriage-mode: call its fns via declare-function.
(declare-function carriage-register-abort-handler "carriage-mode" (fn))
(declare-function carriage-clear-abort-handler "carriage-mode" ())
(declare-function carriage--preloader-start "carriage-mode" ())
(declare-function carriage--preloader-stop "carriage-mode" ())

(defvar carriage--transport-loading-adapter nil
  "Guard to prevent recursive/layered adapter loading in transport dispatcher.")

;; Traffic logging (per-buffer buffers, summaries, and optional file logging)

(defgroup carriage-traffic nil
  "Traffic logging for Carriage (per-buffer buffers, summaries, file logging)."
  :group 'carriage)

(defcustom carriage-traffic-summary-head-bytes 4096
  "Max bytes from the beginning of the response to include in summary."
  :type 'integer :group 'carriage-traffic)

(defcustom carriage-traffic-summary-tail-bytes 4096
  "Max bytes from the end of the response to include in summary."
  :type 'integer :group 'carriage-traffic)

(defcustom carriage-traffic-max-bytes 1048576
  "Approximate max size (in characters) for a traffic buffer before trimming from the top."
  :type 'integer :group 'carriage-traffic)

(defcustom carriage-traffic-log-to-file nil
  "When non-nil, also append traffic log lines to a file (see `carriage-traffic-log-file')."
  :type 'boolean :group 'carriage-traffic)

(defcustom carriage-traffic-log-file "carriage-traffic.log"
  "Path to traffic log file. Relative path is resolved against origin buffer's default-directory."
  :type 'string :group 'carriage-traffic)

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
  "Append TEXT to per-origin traffic buffer and trim if needed. Also write to file if enabled."
  (let ((buf (carriage--traffic--ensure-buffer origin-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)
        (unless (string-suffix-p "\n" text) (insert "\n"))))
    (carriage--traffic--trim-if-needed buf))
  (when carriage-traffic-log-to-file
    (condition-case _e
        (let* ((root (with-current-buffer origin-buffer default-directory))
               (path (if (file-name-absolute-p carriage-traffic-log-file)
                         carriage-traffic-log-file
                       (expand-file-name carriage-traffic-log-file root))))
          (make-directory (file-name-directory path) t)
          (with-temp-buffer
            (insert text)
            (unless (string-suffix-p "\n" text) (insert "\n"))
            (append-to-file (point-min) (point-max) path)))
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
         (head (substring total-text 0 (min len head-limit)))
         (tail (if (> len tail-limit) (substring total-text (- len (min len tail-limit)) len) "")))
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
      (when (functionp abort-fn)
        (carriage-register-abort-handler abort-fn))
      (carriage-log "Transport: begin (abort=%s)" (if (functionp abort-fn) "installed" "none"))
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
          (carriage-log "Transport: unregister abort handler"))))))

;;;###autoload
(defun carriage-transport-streaming (&optional buffer)
  "Signal that transport has progressed to streaming: update UI state (do not stop preloader).

When BUFFER is non-nil, operate in that buffer."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (carriage-log "Transport: streaming")
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
      (carriage-clear-abort-handler)
      ;; Ensure preloader is stopped on finalize.
      (when (fboundp 'carriage--preloader-stop)
        (ignore-errors (carriage--preloader-stop)))
      (if errorp
          (carriage-ui-set-state 'error)
        (carriage-ui-set-state 'idle))
      (carriage-log "Transport: complete (status=%s)" (if errorp "error" "ok"))
      t)))

;;;###autoload
(defun carriage-transport-dispatch (&rest args)
  "Dispatch request ARGS to transport adapter with safe lazy loading.

Contract:
- Prefer direct entry-point call (carriage-transport-<backend>-dispatch) when fboundp.
- If missing, attempt one-shot lazy load of adapter (guarded), then call entry-point.
- No recursion, no reliance on function cell replacement."
  (carriage-traffic-log 'out "dispatch request: %S" args)
  (let* ((backend (plist-get args :backend))
         (bsym (cond
                ((symbolp backend) backend)
                ((stringp backend) (intern backend))
                (t (ignore-errors (intern (format "%s" backend)))))))
    (pcase bsym
      ;; GPTel backend
      ('gptel
       (cond
        ;; Fast path: entry-point present
        ((fboundp 'carriage-transport-gptel-dispatch)
         (apply #'carriage-transport-gptel-dispatch args))
        ;; One-shot lazy load guarded
        ((not carriage--transport-loading-adapter)
         (let* ((carriage--transport-loading-adapter t))
           (when (and (require 'gptel nil t)
                      (require 'carriage-transport-gptel nil t))
             (carriage-log "Transport: gptel adapter loaded on demand"))
           (if (fboundp 'carriage-transport-gptel-dispatch)
               (apply #'carriage-transport-gptel-dispatch args)
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

(defun carriage-transport--strip-internal-lines (text)
  "Remove internal Carriage marker lines from TEXT (best-effort, centralized).

Strips:
- #+begin_carriage … #+end_carriage blocks
- Org property headers for CARRIAGE_STATE / CARRIAGE_ITERATION_ID / CARRIAGE_FINGERPRINT
- Inline CARRIAGE_FINGERPRINT / CARRIAGE_ITERATION_ID lines"
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
      ;; Doc-state / fingerprint / iteration in Org property headers
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+\\(CARRIAGE_STATE\\|CARRIAGE_ITERATION_ID\\|CARRIAGE_FINGERPRINT\\)\\b.*$" nil t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position)))))
      ;; Per-send inline fingerprint/iteration
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+\\(CARRIAGE_FINGERPRINT\\|CARRIAGE_ITERATION_ID\\)\\b.*$" nil t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))))
    (buffer-substring-no-properties (point-min) (point-max))))


(provide 'carriage-transport)
;;; carriage-transport.el ends here
