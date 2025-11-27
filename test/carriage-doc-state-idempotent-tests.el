;;; carriage-doc-state-idempotent-tests.el --- Idempotent before-save normalization tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Ensure before-save normalization creates/updates #+begin_carriage exactly once
;; and stays idempotent across multiple save-hook invocations.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'carriage-doc-state)

(defun carriage--cds--count-begin-carriage ()
  "Return number of begin_carriage blocks in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((n 0)
          (case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t)
        (setq n (1+ n)))
      n)))

(ert-deftest carriage-doc-state/idempotent-before-save-normalizes-begin-carriage ()
  "Before-save hook creates normalized #+begin_carriage once and is idempotent."
  (with-temp-buffer
    (org-mode)
    ;; Seed file-level properties (legacy path) to trigger normalization.
    (insert "#+title: Demo\n"
            "#+PROPERTY: CARRIAGE_MODE t\n"
            "#+PROPERTY: CARRIAGE_INTENT Code\n\n"
            "* Note\nBody\n")
    ;; Install per-buffer hook and simulate two saves.
    (carriage-doc-state-install-save-hook)
    (run-hooks 'before-save-hook)
    (should (= (carriage--cds--count-begin-carriage) 1))
    ;; Run again → still one block
    (run-hooks 'before-save-hook)
    (should (= (carriage--cds--count-begin-carriage) 1))
    ;; Check that normalized keys appear inside the block
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t)
      (let ((ok (progn
                  (goto-char (point-min))
                  (or (re-search-forward "^CARRIAGE_MODE[ \t]+t\\b" nil t)
                      (progn
                        (goto-char (point-min))
                        (re-search-forward "^CAR_MODE[ \t]+t\\b" nil t))))))
        (should ok)))))

(provide 'carriage-doc-state-idempotent-tests)
;;; carriage-doc-state-idempotent-tests.el ends here
