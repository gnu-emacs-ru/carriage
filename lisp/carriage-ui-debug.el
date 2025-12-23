;;; carriage-ui-debug.el --- Debug helpers: log UI/doc-state interactions  -*- lexical-binding: t; -*-
;;
;; Small instrumentation helpers to trace why a buffer ends up with header-line
;; but no mode-line (useful to debug CARRIAGE_STATE-driven UI choices).
;;
;; Usage:
;;  - (carriage-ui-debug-install)      ; enable advices/watchers
;;  - (carriage-ui-debug-uninstall)    ; remove advices/watchers
;;  - (carriage-ui-debug-dump)         ; write current buffer UI state to log
;;
;; This module is best-effort and never signals in advice hooks.
;; It relies on carriage-log (carriage-logging), but otherwise is optional.
;;
;;; Code:

(require 'subr-x)
(require 'carriage-logging nil t)

(defvar-local carriage-ui-debug--watchers-installed nil
  "Non-nil when variable watchers are installed in the current buffer.")

(defvar carriage-ui-debug--global-watchers nil
  "List of symbols for global variable watchers installed by carriage-ui-debug.")

(defun carriage-ui-debug--safe-prin1-to-string (v)
  "Safe printer for V."
  (condition-case _e
      (prin1-to-string v)
    (error (format "<unprintable-%s>" (type-of v)))))

(defun carriage-ui-debug--dump-buffer-state (&optional buffer)
  "Return a plist describing UI-related state for BUFFER (or current buffer)."
  (let* ((buf (or buffer (current-buffer)))
         (res (with-current-buffer buf
                (list
                 :buffer (buffer-name buf)
                 :major-mode major-mode
                 :mode-line-local (local-variable-p 'mode-line-format)
                 :mode-line-value (if (local-variable-p 'mode-line-format)
                                      (carriage-ui-debug--safe-prin1-to-string mode-line-format)
                                    (carriage-ui-debug--safe-prin1-to-string (default-value 'mode-line-format)))
                 :header-line-local (local-variable-p 'header-line-format)
                 :header-line-value (if (local-variable-p 'header-line-format)
                                        (carriage-ui-debug--safe-prin1-to-string header-line-format)
                                      (carriage-ui-debug--safe-prin1-to-string (default-value 'header-line-format)))
                 :invisible (and (boundp 'buffer-invisibility-spec) buffer-invisibility-spec)
                 :carriage-mode (and (boundp 'carriage-mode) (bound-and-true-p carriage-mode))
                 :carriage-state (when (fboundp 'carriage-doc-state-read)
                                   (condition-case _e
                                       (carriage-doc-state-read buf)
                                     (error nil)))))))
    res))

(defun carriage-ui-debug--log-buffer-state (&optional buffer msg)
  "Log UI state for BUFFER (or current) with optional MSG prefix."
  (ignore-errors
    (let* ((st (carriage-ui-debug--dump-buffer-state buffer))
           (prefix (if (stringp msg) msg "carriage-ui-debug")))
      (carriage-log "%s: %s" prefix (prin1-to-string st)))))

;;; Advices / hooks

(defun carriage-ui-debug--advice-doc-state-restore (&rest _args)
  "Advice run after `carriage-doc-state-restore'.
Logs buffer UI state. Silently ignores errors."
  (ignore-errors
    (when (called-interactively-p 'interactive)
      (carriage-log "carriage-ui-debug: doc-state restore (interactive)"))
    (carriage-ui-debug--log-buffer-state (current-buffer) "doc-state-restore")))

(defun carriage-ui-debug--advice-carriage-mode (arg &rest _)
  "Advice after `carriage-mode' toggles. ARG is the numeric arg given to the command."
  (ignore-errors
    ;; run in current buffer (caller)
    (carriage-ui-debug--log-buffer-state (current-buffer) (format "carriage-mode toggled arg=%s" arg))
    ;; also dump if doc-state available
    (when (fboundp 'carriage-doc-state-read)
      (carriage-ui-debug--log-buffer-state (current-buffer) "post-toggle doc-state"))))

;;; Variable watchers

(defun carriage-ui-debug--var-watcher (sym newval op where)
  "Variable watcher that logs changes to SYM.
NEWVAL is the new value; OP/WHERE are unused (kept for compatibility)."
  (ignore-errors
    (let ((buf (current-buffer)))
      (carriage-log "carriage-ui-debug: var %s changed in %s → %s" sym (buffer-name buf)
                    (carriage-ui-debug--safe-prin1-to-string newval)))))

(defun carriage-ui-debug--install-watchers ()
  "Install buffer-local watchers for mode/header line changes."
  (when (not carriage-ui-debug--watchers-installed)
    (add-variable-watcher 'mode-line-format #'carriage-ui-debug--var-watcher)
    (add-variable-watcher 'header-line-format #'carriage-ui-debug--var-watcher)
    (setq carriage-ui-debug--watchers-installed t)))

(defun carriage-ui-debug--remove-watchers ()
  "Remove variable watchers installed by this module."
  (when carriage-ui-debug--watchers-installed
    (ignore-errors
      (remove-variable-watcher 'mode-line-format #'carriage-ui-debug--var-watcher)
      (remove-variable-watcher 'header-line-format #'carriage-ui-debug--var-watcher))
    (setq carriage-ui-debug--watchers-installed nil)))

;;; Public install/uninstall helpers

;;;###autoload
(defun carriage-ui-debug-install ()
  "Enable carriage UI debug advices and watchers globally (best-effort).
This installs advices on `carriage-doc-state-restore' and `carriage-mode' and
adds variable watchers for mode/header line changes."
  (interactive)
  (ignore-errors
    (when (fboundp 'advice-add)
      (with-eval-after-load 'carriage-doc-state
        (advice-add 'carriage-doc-state-restore :after #'carriage-ui-debug--advice-doc-state-restore))
      (with-eval-after-load 'carriage-mode
        (advice-add 'carriage-mode :after #'carriage-ui-debug--advice-carriage-mode)))
    ;; global watchers: attach to current and future buffers via minor-mode hook
    ;; Install per-buffer watchers for already existing buffers where carriage-mode is active.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (boundp 'carriage-mode)
          (carriage-ui-debug--install-watchers))))
    ;; Hook to install watchers when carriage-mode is enabled in a buffer
    (add-hook 'carriage-mode-hook #'carriage-ui-debug--install-watchers)
    (carriage-log "carriage-ui-debug: installed")))

;;;###autoload
(defun carriage-ui-debug-uninstall ()
  "Disable carriage UI debug advices and watchers (best-effort)."
  (interactive)
  (ignore-errors
    (when (fboundp 'advice-remove)
      (when (advice-member-p #'carriage-ui-debug--advice-doc-state-restore #'carriage-doc-state-restore)
        (advice-remove 'carriage-doc-state-restore #'carriage-ui-debug--advice-doc-state-restore))
      (when (advice-member-p #'carriage-ui-debug--advice-carriage-mode #'carriage-mode)
        (advice-remove 'carriage-mode #'carriage-ui-debug--advice-carriage-mode)))
    (remove-hook 'carriage-mode-hook #'carriage-ui-debug--install-watchers)
    ;; Remove watchers from all buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (carriage-ui-debug--remove-watchers)))
    (carriage-log "carriage-ui-debug: uninstalled")))

;;;###autoload
(defun carriage-ui-debug-dump (&optional buffer)
  "Interactively dump UI/doc-state info for BUFFER (or current)."
  (interactive)
  (carriage-ui-debug--log-buffer-state (or buffer (current-buffer)) "carriage-ui-debug-dump")
  (message "carriage-ui-debug: dumped state for %s" (buffer-name (or buffer (current-buffer)))))

(provide 'carriage-ui-debug)
;;; carriage-ui-debug.el ends here
