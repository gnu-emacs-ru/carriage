;;; carriage-global-mode.el --- Global prefix for Carriage -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: global, keybindings, convenience
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/ui-v2.org
;;   spec/keyspec-v2.org
;;   spec/i18n-v2.org
;;
;;; Commentary:
;; Global minor mode that installs the Carriage prefix/menu (C-c e) when
;; enabled. Integrates with keyspec/which-key and exposes a fallback prefix.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-ui nil t)
(require 'carriage-keyspec)

(defgroup carriage-global nil
  "Global integration for Carriage (global prefix/menu)."
  :group 'applications
  :prefix "carriage-global-")

;;;###autoload
(define-minor-mode carriage-global-mode
  "Global minor mode enabling the Carriage prefix keymap globally.

Policy (keyspec v3):
- `carriage-keys-prefix' is ALWAYS a prefix keymap (never a menu command).
- Menu/help live under the prefix:
  - C-c e SPC → `carriage-menu-open'
  - C-c e ?   → `carriage-menu-help'
- When enabled, this mode binds the prefix(es) globally to `carriage-prefix-map'
  via `carriage-keys-global-enable'. When disabled, restores prior bindings."
  :global t
  :group 'carriage-global
  (if carriage-global-mode
      (progn
        (when (require 'carriage-keyspec nil t)
          (ignore-errors (carriage-keys-install-known-keymaps))
          (ignore-errors (carriage-keys-global-enable)))
        ;; Auto-enable carriage-mode on visiting Org files when CAR_MODE=t
        (when (require 'carriage-doc-state nil t)
          (add-hook 'find-file-hook #'carriage-doc-state-auto-enable))
        (message "carriage-global-mode enabled"))
    (when (featurep 'carriage-doc-state)
      (remove-hook 'find-file-hook #'carriage-doc-state-auto-enable))
    (when (require 'carriage-keyspec nil t)
      (ignore-errors (carriage-keys-global-disable)))
    (message "carriage-global-mode disabled")))

;; Auto-enable carriage-mode and fold CARRIAGE_STATE summary on open (best-effort).
;;
;; Goal: when an Org buffer contains `#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t ...)`,
;; opening the file should automatically:
;;  - enable `carriage-mode` (if not enabled yet),
;;  - fold `#+PROPERTY: CARRIAGE_STATE ...` into icon/summary view (overlay),
;; so users see badges immediately without manual commands.
(defun carriage-global-mode--maybe-auto-enable-doc-state ()
  "Best-effort: auto-enable carriage-mode from CARRIAGE_STATE and fold summary."
  (when (derived-mode-p 'org-mode)
    (when (require 'carriage-doc-state nil t)
      ;; Enable carriage-mode if the document requests it.
      (ignore-errors (carriage-doc-state-auto-enable (current-buffer)))
      ;; Always try to fold CARRIAGE_STATE into summary when present.
      ;; (Works even if carriage-mode isn't enabled; it is just a visual layer.)
      (ignore-errors
        (carriage-doc-state-hide (current-buffer)))
      ;; Minimal, safe anti-race: one deferred ensure so late mode-line rewrites
      ;; (during/after org-mode-hook) don't hide Carriage.
      (let ((buf (current-buffer)))
        (run-at-time
         0 nil
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when (and (bound-and-true-p carriage-mode)
                          (fboundp 'carriage-mode--modeline-ensure-once))
                 (ignore-errors (carriage-mode--modeline-ensure-once buf)))))))))))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'carriage-global-mode--maybe-auto-enable-doc-state))

(provide 'carriage-global-mode)
;;; carriage-global-mode.el ends here
