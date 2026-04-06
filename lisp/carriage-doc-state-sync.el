;;; carriage-doc-state-sync.el --- Save hooks and sync for document state  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Save hooks and sync for CARRIAGE_STATE.
;; Extracted from carriage-doc-state.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-doc-state-read "carriage-doc-state" (&optional buffer))
(declare-function carriage-doc-state-write "carriage-doc-state" (&optional state buffer))
(declare-function carriage-doc-state-write-current "carriage-doc-state" (&optional buffer))
(declare-function carriage-doc-state-restore "carriage-doc-state" (&optional buffer))
(declare-function carriage-doc-state-summary-enable "carriage-doc-state" (&optional buffer))
(declare-function carriage-doc-state-summary-refresh "carriage-doc-state" (&optional buffer))

(defcustom carriage-doc-state-save-on-save t
  "When non-nil, normalize and save CARRIAGE_STATE on buffer save."
  :type 'boolean
  :group 'carriage-doc-state)

(defvar-local carriage-doc-state--save-hook-installed nil
  "Non-nil when before-save hook is installed for this buffer.")

(defun carriage-doc-state--before-save ()
  "Before-save hook handler: normalize and save CARRIAGE_STATE."
  (when carriage-doc-state-save-on-save
    (ignore-errors (carriage-doc-state-restore (current-buffer)))
    (ignore-errors (carriage-doc-state-write-current (current-buffer)))
    (ignore-errors
      (when (derived-mode-p 'org-mode)
        (carriage-doc-state-summary-enable)
        (carriage-doc-state-summary-refresh (current-buffer))))))

;;;###autoload
(defun carriage-doc-state-install-save-hook (&optional buffer)
  "Install buffer-local before-save hook for doc-state normalization."
  (with-current-buffer (or buffer (current-buffer))
    (when (and carriage-doc-state-save-on-save
               (not carriage-doc-state--save-hook-installed))
      (add-hook 'before-save-hook #'carriage-doc-state--before-save nil t)
      (setq carriage-doc-state--save-hook-installed t)
      t)))

(provide 'carriage-doc-state-sync)
;;; carriage-doc-state-sync.el ends here
