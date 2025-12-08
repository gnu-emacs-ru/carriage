;;; carriage-logging.el --- Logging and traffic buffers  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: logging, traffic
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/logging-v2.org
;;   spec/security-v2.org
;;   spec/observability-v2.org
;;
;;; Commentary:
;; Logging helpers and carriage-traffic buffer management.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage nil
  "Carriage-mode customization group."
  :group 'tools)

(defcustom carriage-mode-log-max-lines 2000
  "Maximum number of lines to keep in *carriage-log* and report buffers."
  :type 'integer
  :group 'carriage)

(defcustom carriage-mode-traffic-max-lines 1000
  "Maximum number of lines in *carriage-traffic* buffer."
  :type 'integer
  :group 'carriage)

(defcustom carriage-mode-aux-window-side 'right
  "Side for auxiliary buffers (*carriage-log*, *carriage-traffic*)."
  :type '(choice (const left) (const right) (const top) (const bottom))
  :group 'carriage)

(defcustom carriage-mode-aux-window-size 0.33
  "Relative size of the side window for auxiliary buffers.
For left/right sides this is window-width; for top/bottom — window-height."
  :type 'number
  :group 'carriage)

(defcustom carriage-mode-aux-window-reuse t
  "Reuse an existing window that already displays the auxiliary buffer."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-use-tab-line-in-aux nil
  "When non-nil, use tab-line-mode in Carriage auxiliary buffers (Log/Traffic).
When nil (default), use a lightweight header-line tabs to reduce redisplay cost."
  :type 'boolean
  :group 'carriage)

(defconst carriage--log-buffer-name "*carriage-log*")
(defconst carriage--traffic-buffer-name "*carriage-traffic*")

(defun carriage-log-buffer ()
  "Return the log buffer, creating it if necessary. Ensure read-only special mode with 'q' to close."
  (let* ((buf (get-buffer-create carriage--log-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'carriage-aux-mode)
        (carriage-aux-mode))
      (setq-local header-line-format (carriage--aux-header-line 'log))
      (carriage--aux-ensure-tabline))
    buf))

(defun carriage-traffic-buffer ()
  "Return the traffic buffer, creating it if necessary. Ensure read-only special mode with 'q' to close."
  (let* ((buf (get-buffer-create carriage--traffic-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'carriage-aux-mode)
        (carriage-aux-mode))
      (setq-local header-line-format (carriage--aux-header-line 'traffic))
      (carriage--aux-ensure-tabline))
    buf))

(defun carriage--buffer-line-count (buffer)
  "Count lines in BUFFER."
  (with-current-buffer buffer
    (count-lines (point-min) (point-max))))

(defun carriage--trim-buffer-lines (buffer max-lines)
  "Trim BUFFER to MAX-LINES from the end."
  (with-current-buffer buffer
    (when (> (carriage--buffer-line-count buffer) max-lines)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (forward-line (- max-lines))
        (delete-region (point-min) (point))))))

(defun carriage--append-line-capped (buffer string max-lines)
  "Append STRING and newline to BUFFER, cap to MAX-LINES.
STRING may be any object; it will be coerced to a string via format."
  (let* ((s (if (stringp string) string (format "%s" string))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert s)
        (unless (or (string-empty-p s)
                    (eq (aref s (1- (length s))) ?\n))
          (insert "\n"))))
    (carriage--trim-buffer-lines buffer max-lines)
    buffer))


(defun carriage-log (fmt &rest args)
  "Log a formatted message FMT with ARGS to the general log."
  (let* ((buf (carriage-log-buffer)))
    (carriage--append-line-capped buf (apply #'format fmt args) carriage-mode-log-max-lines)))

(defun carriage-traffic-log (dir fmt &rest args)
  "Log traffic DIR ('in|'out) with FMT and ARGS."
  (let* ((tag (format "[%s] " (if (eq dir 'in) "IN " "OUT")))
         (buf (carriage-traffic-buffer)))
    (carriage--append-line-capped buf (concat tag (apply #'format fmt args)) carriage-mode-traffic-max-lines)))

(defun carriage-clear-logs ()
  "Clear log and traffic buffers."
  (dolist (b (list (carriage-log-buffer) (carriage-traffic-buffer)))
    (with-current-buffer b
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (message "Carriage logs cleared.")
  t)

;; -------------------------------------------------------------------
;; Unified stacked (tabbed) aux window: one window with [Log|Traffic] tabs
;; - Prefer tab-line-mode when available (Emacs 27+).
;; - Fallback to a clickable header-line tabs if tab-line is unavailable.

(defun carriage--aux-tab-buffers ()
  "Return the list of aux buffers in tab order."
  (delq nil (list (get-buffer carriage--log-buffer-name)
                  (get-buffer carriage--traffic-buffer-name))))

(defun carriage--aux-tab-name (buffer &rest _)
  "Return a short tab name for BUFFER. Accept extra args for newer Emacs."
  (let ((n (buffer-name buffer)))
    (cond
     ((string= n carriage--log-buffer-name) "Log")
     ((string= n carriage--traffic-buffer-name) "Traffic")
     (t n))))

(defun carriage--aux-ensure-tabline ()
  "Enable tab-line with only Log/Traffic buffers when available.
Respects `carriage-mode-use-tab-line-in-aux'. When nil, do not enable tab-line
and rely on the lightweight header-line tabs.
Also explicitly disables tab-line in these buffers to avoid global tab-line-mode overhead."
  (if (and carriage-mode-use-tab-line-in-aux
           (boundp 'tab-line-tabs-function))
      (progn
        (setq-local tab-line-tabs-function (lambda ()
                                             (carriage--aux-tab-buffers)))
        (setq-local tab-line-tab-name-function #'carriage--aux-tab-name)
        (when (fboundp 'tab-line-mode)
          (tab-line-mode 1)))
    (when (fboundp 'tab-line-mode)
      (tab-line-mode 0))))

(defun carriage--aux-header-line (active)
  "Build a minimal header-line with clickable [Log|Traffic] tabs as fallback.
ACTIVE is 'log or 'traffic to highlight current tab."
  (let* ((mk (lambda (label which)
               (let* ((on (eq active which))
                      (txt (format "[%s]" label))
                      (cmd (pcase which
                             ('log #'carriage-show-log)
                             ('traffic #'carriage-show-traffic)
                             (_ #'ignore)))
                      (map (let ((m (make-sparse-keymap)))
                             ;; Bind both press and click to improve compatibility.
                             ;; Support both header-line qualified events and plain mouse events
                             ;; to accommodate different Emacs builds.
                             (define-key m [header-line mouse-1] cmd)
                             (define-key m [header-line down-mouse-1] cmd)
                             (define-key m [header-line mouse-2] cmd)
                             (define-key m [header-line down-mouse-2] cmd)
                             (define-key m [mouse-1] cmd)
                             (define-key m [down-mouse-1] cmd)
                             (define-key m [mouse-2] cmd)
                             (define-key m [down-mouse-2] cmd)
                             m)))
                 (propertize txt
                             'mouse-face 'mode-line-highlight
                             'help-echo (format "Switch to %s" label)
                             ;; Use 'keymap for header-line strings; some builds ignore 'local-map here.
                             'keymap map
                             'follow-link t
                             'pointer 'hand
                             'face (when on 'mode-line-emphasis))))))
    (concat "Carriage: "
            (funcall mk "Log" 'log)
            " "
            (funcall mk "Traffic" 'traffic))))

(defun carriage--aux-find-window ()
  "Return an existing window showing either log or traffic buffer, or nil."
  (or (get-buffer-window carriage--log-buffer-name t)
      (get-buffer-window carriage--traffic-buffer-name t)))

(defun carriage--display-aux-buffer (buffer &optional side size reuse slot)
  "Display BUFFER in a side window without replacing the current window.
SIDE defaults to =carriage-mode-aux-window-side'. SIZE defaults to
=carriage-mode-aux-window-size'. When REUSE (or
=carriage-mode-aux-window-reuse') is non-nil, reuse the single aux window
(and switch its buffer) to keep one stacked tabbed panel."
  (let* ((side (or side (and (boundp 'carriage-mode-aux-window-side)
                             carriage-mode-aux-window-side)
                   'right))
         (size (or size (and (boundp 'carriage-mode-aux-window-size)
                             carriage-mode-aux-window-size)
                   0.33))
         (reuse (if (boundp 'carriage-mode-aux-window-reuse)
                    carriage-mode-aux-window-reuse
                  t))
         ;; Always use a single slot for both buffers to avoid creating two windows.
         (slot (or slot 0))
         (win (and reuse (carriage--aux-find-window))))
    (when (and win
               (let ((ws (window-parameter win 'window-side)))
                 (not (eq ws side))))
      (setq win nil))
    (save-selected-window
      (let* ((inhibit-switch-frame t))
        (cond
         (win
          ;; Reuse the existing aux window, just switch its buffer.
          (when (window-live-p win)
            (set-window-buffer win buffer)))
         (t
          ;; Show new side window
          (display-buffer buffer
                          `((display-buffer-reuse-window display-buffer-in-side-window)
                            (inhibit-same-window . t)
                            (side . ,side)
                            ,@(if (memq side '(left right))
                                  `((window-width . ,size))
                                `((window-height . ,size)))
                            (slot . ,slot)
                            (window-parameters . ((no-delete-other-windows . t)))))))))))

(defun carriage-show-log ()
  "Display the Carriage log in a single right-side window with tabs."
  (interactive)
  (carriage--display-aux-buffer (carriage-log-buffer)
                                carriage-mode-aux-window-side
                                carriage-mode-aux-window-size
                                carriage-mode-aux-window-reuse
                                0))

(defun carriage-show-traffic ()
  "Display the Carriage traffic in a single right-side window with tabs."
  (interactive)
  (carriage--display-aux-buffer (carriage-traffic-buffer)
                                carriage-mode-aux-window-side
                                carriage-mode-aux-window-size
                                carriage-mode-aux-window-reuse
                                0))

;;;###autoload
(defun carriage-show-log-and-traffic ()
  "Open a single side window (right) with tabs [Log|Traffic].
Click tabs (or call carriage-show-log / carriage-show-traffic) to switch."
  (interactive)
  ;; Default to Log on open; tabs allow switching instantly.
  (carriage-show-log))

;; Auxiliary buffers major mode (read-only, 'q' to close window)
(defvar carriage-aux-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'quit-window)
    ;; Fallback: bind C-c e q directly in aux buffers, in addition to keyspec.
    (define-key map (kbd "C-c e q") #'quit-window)
    map)
  "Keymap for Carriage auxiliary buffers (*carriage-log*, *carriage-traffic*).")

(defun carriage-aux-mode (&optional _arg)
  "Major mode for Carriage auxiliary buffers."
  (interactive)
  (kill-all-local-variables)
  (use-local-map carriage-aux-mode-map)
  (setq major-mode 'carriage-aux-mode)
  (setq mode-name "Carriage-Aux")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;; Ensure tab-line fallback header reflects current buffer
  (setq-local header-line-format
              (carriage--aux-header-line (if (string= (buffer-name) carriage--log-buffer-name) 'log 'traffic)))
  (run-mode-hooks 'carriage-aux-mode-hook)
  ;; Ensure keyspec bindings (e.g., C-c e q) are present when the mode is enabled.
  (when (fboundp 'carriage-keys-apply-known-keymaps)
    (ignore-errors (carriage-keys-apply-known-keymaps))))

;; Ensure keyspec bindings are applied to carriage-aux-mode-map (log/traffic contexts).
;; Apply immediately if keyspec is loaded, and also after it loads.
(when (fboundp 'carriage-keys-apply-known-keymaps)
  (ignore-errors (carriage-keys-apply-known-keymaps)))
(with-eval-after-load 'carriage-keyspec
  (when (fboundp 'carriage-keys-apply-known-keymaps)
    (ignore-errors (carriage-keys-apply-known-keymaps))))

(defvar carriage--metrics-events nil
  "In-memory list of lightweight metrics events (each is a plist).")

;;;###autoload
(defun carriage-metrics-note (key value &optional meta)
  "Record lightweight metric KEY → VALUE with optional META plist.
Stores event in `carriage--metrics-events' and logs via `carriage-log' when available.
Returns the recorded event plist."
  (let* ((ev (list :time (float-time) :key key :value value :meta meta)))
    (setq carriage--metrics-events (cons ev carriage--metrics-events))
    (when (fboundp 'carriage-log)
      (ignore-errors
        (carriage-log "metrics: %s=%s %s"
                      key value (if meta (format "%S" meta) ""))))
    ev))

;;;###autoload
(defun carriage-metrics-events ()
  "Return a shallow copy of collected metrics events."
  (copy-sequence (or carriage--metrics-events '())))

(provide 'carriage-logging)
;;; carriage-logging.el ends here
