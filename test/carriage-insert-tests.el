;;; carriage-insert-tests.el --- Tests for insert actions (single undo group) -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Verify that insert actions perform edits within a single undo group:
;; - After a single call to carriage-insert-*, one (undo) should restore buffer to original content.

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-task)

(defun carriage--tests--with-temp-org (initial thunk)
  "Run THUNK in a temporary org-mode buffer initialized with INITIAL string."
  (with-temp-buffer
    (org-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (or initial ""))
      (goto-char (point-max))
      (setq buffer-undo-list nil)        ; start clean
      (funcall thunk))))

(defun carriage--tests--undo-restores-p (after-fn)
  "Return non-nil if AFTER-FN inserts content that can be fully undone by a single (undo 1)."
  (let ((restored nil))
    (carriage--tests--with-temp-org
     "* Heading\n\nBody\n"
     (lambda ()
       (let ((before (buffer-substring-no-properties (point-min) (point-max))))
         (funcall after-fn)
         ;; Single undo should restore the original state (guard when no undo info).
         (when (listp buffer-undo-list)
           (ignore-errors (undo 1)))
         (setq restored
               (string=
                before
                (buffer-substring-no-properties (point-min) (point-max)))))))
    restored))

(ert-deftest carriage-insert/plan-single-undo ()
  "Insert Plan should be revertible by a single undo step."
  (should (carriage--tests--undo-restores-p #'carriage-insert-plan-section)))

(ert-deftest carriage-insert/step-single-undo ()
  "Insert Step should be revertible by a single undo step."
  (should (carriage--tests--undo-restores-p #'carriage-insert-step-section)))

(ert-deftest carriage-insert/test-single-undo ()
  "Insert Test should be revertible by a single undo step."
  (should (carriage--tests--undo-restores-p #'carriage-insert-test-section)))

(ert-deftest carriage-insert/retro-single-undo ()
  "Insert Retro should be revertible by a single undo step."
  (should (carriage--tests--undo-restores-p #'carriage-insert-retro-section)))

(provide 'carriage-insert-tests)
;;; carriage-insert-tests.el ends here
