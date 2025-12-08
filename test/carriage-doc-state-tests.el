;;; carriage-doc-state-tests.el --- Tests for doc-state normalization and begin_carriage -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Ensure before-save normalization creates/updates #+begin_carriage and folds overlays idempotently.

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-doc-state)

(ert-deftest carriage-doc-state/before-save-normalizes-begin-carriage ()
  "Installing the before-save hook normalizes storage: writes #+begin_carriage."
  (with-temp-buffer
    (org-mode)
    ;; Seed a minimal buffer with a file-level property to be normalized
    (insert "#+title: Demo\n#+PROPERTY: CARRIAGE_MODE t\n\n* Note\nBody\n")
    ;; Install hook and simulate save
    (carriage-doc-state-install-save-hook)
    (run-hooks 'before-save-hook)
    ;; Expect begin_carriage present with normalized keys
    (save-excursion
      (goto-char (point-min))
      (should (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t))
      (should (re-search-forward "^CARRIAGE_MODE[ \t]+t\\b" nil t))
      (should (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)))))

(ert-deftest carriage-doc-state/relocates-begin-carriage-under-properties ()
  "Existing begin_carriage block is moved to the very top under #+PROPERTY."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n#+PROPERTY: A x\n#+PROPERTY: B y\n\n* Note\nBody\n\n")
    ;; Put block far below (wrong place)
    (insert "#+begin_carriage\nCARRIAGE_MODE t\n#+end_carriage\n\n")
    ;; Writing should relocate it to the top slot
    (carriage-doc-state-write '(("CAR_MODE" . "t")))
    (save-excursion
      (goto-char (point-min))
      (should (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t))
      (forward-line -1)
      ;; Immediately above begin_carriage must be a PROPERTY (last header line)
      (should (looking-at "^[ \t]*#\\+PROPERTY:")))))

(ert-deftest carriage-doc-state/inserts-at-top-when-absent ()
  "When absent, begin_carriage is inserted at the top slot under #+PROPERTY."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n#+PROPERTY: P q\n\n* Note\n")
    (carriage-doc-state-write '(("CAR_MODE" . "t")))
    (save-excursion
      (goto-char (point-min))
      (should (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t))
      (forward-line -1)
      (should (looking-at "^[ \t]*#\\+PROPERTY:")))))

(ert-deftest carriage-doc-state/idempotent-top-placement ()
  "Second write keeps the block in place and produces identical buffer."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n#+PROPERTY: X 1\n#+PROPERTY: Y 2\n\n* Note\n")
    (carriage-doc-state-write '(("CAR_MODE" . "t")))
    (let ((s1 (buffer-substring-no-properties (point-min) (point-max))))
      (carriage-doc-state-write '(("CAR_MODE" . "t")))
      (let ((s2 (buffer-substring-no-properties (point-min) (point-max))))
        (should (equal s1 s2))))))

(provide 'carriage-doc-state-tests)
;;; carriage-doc-state-tests.el ends here
