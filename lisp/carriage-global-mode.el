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
;;   spec/keyspec-v2.org
;;   spec/ui-v2.org
;;
;;; Commentary:
;; Global minor mode that installs the Carriage prefix/menu (C-c e) when
;; enabled. Integrates with keyspec/which-key and exposes a fallback prefix.
;;
;;; Code:
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/ui-v2.org
;;   spec/keyspec-v2.org
;;   spec/i18n-v2.org

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-keyspec)

(defgroup carriage-global nil
  "Global integration for Carriage (global prefix/menu)."
  :group 'applications
  :prefix "carriage-global-")

(defcustom carriage-global-use-transient t
  "When non-nil, the global Carriage prefix opens the Carriage menu (transient or fallback).
When nil, the global Carriage prefix acts as a prefix key with suffixes from :contexts (global)."
  :type 'boolean
  :group 'carriage-global)

(defvar carriage-global--prefix-map nil
  "Global prefix keymap installed by carriage-global-mode when
carriage-global-use-transient is nil. Holds suffixes from :contexts (global).")

;;;###autoload
(define-minor-mode carriage-global-mode
  "Global minor mode enabling the Carriage prefix/menu for :contexts (global).
When enabled:
- If =carriage-global-use-transient' is non-nil, bind the base prefix (from =carriage-keys-prefix') globally to =carriage-keys-open-menu'.
- If nil, install a global prefix keymap under the base prefix and populate suffixes from :contexts (global) via keyspec.
which-key hints are registered if available."
  :global t
  :group 'carriage-global
  (let* ((prefixes (carriage-keys-prefixes)))
    (dolist (pref prefixes)
      (define-key global-map (kbd pref) nil))
    (when (keymapp carriage-global--prefix-map)
      (setq carriage-global--prefix-map nil))
    (if carriage-global-mode
        (progn
          (dolist (pref prefixes)
            (define-key global-map (kbd pref) #'carriage-keys-open-menu))
          ;; Auto-enable carriage-mode on visiting Org files when CAR_MODE=t
          (when (require 'carriage-doc-state nil t)
            (add-hook 'find-file-hook #'carriage-doc-state-auto-enable))
          (message "carriage-global-mode enabled (menu)"))
      (when (featurep 'carriage-doc-state)
        (remove-hook 'find-file-hook #'carriage-doc-state-auto-enable))
      (message "carriage-global-mode disabled"))))

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
        (carriage-doc-state-hide (current-buffer))))))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'carriage-global-mode--maybe-auto-enable-doc-state))

(provide 'carriage-global-mode)
;;; carriage-global-mode.el ends here
