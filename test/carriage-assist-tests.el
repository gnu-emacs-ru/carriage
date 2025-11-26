;;; carriage-assist-tests.el --- Tests for Assist schema validators -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; ERT tests for Assist schema validators:
;; - Suggest list validator
;; - Context-Delta validator

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'carriage-assist)

(ert-deftest carriage-assist/suggest-valid ()
  "Validator accepts empty or well-formed suggestion lists; rejects malformed."
  ;; Empty list is acceptable (no suggestions)
  (should (carriage-assist--suggest-valid-p '()))
  ;; Well-formed single suggestion
  (should (carriage-assist--suggest-valid-p
           (list (list :id 'insert-plan :label "Insert Plan" :reason "" :weight 0))))
  ;; Missing required keys → invalid
  (should-not (carriage-assist--suggest-valid-p
               (list (list :id 'insert-plan :label "x"))))
  (should-not (carriage-assist--suggest-valid-p
               (list (list :label "x" :reason "" :weight 1)))))

(ert-deftest carriage-assist/context-delta-valid ()
  "Validator accepts {:add :remove :why}; rejects incomplete forms."
  (should (carriage-assist--ctx-delta-valid-p (list :add '() :remove '() :why "")))
  ;; Missing :remove
  (should-not (carriage-assist--ctx-delta-valid-p (list :add '() :why "")))
  ;; Wrong types for add/remove
  (should-not (carriage-assist--ctx-delta-valid-p (list :add "x" :remove '() :why "")))
  (should-not (carriage-assist--ctx-delta-valid-p (list :add '() :remove "x" :why ""))))

;; UI-level guard: invalid Assist delta must not modify buffers or prompt.
(ert-deftest carriage-assist/ui-context-delta-invalid-no-change ()
  "UI command refuses invalid Assist delta and keeps buffer intact without prompting."
  (with-temp-buffer
    (require 'org)
    (require 'cl-lib)
    (require 'carriage-ui)
    (org-mode)
    ;; Seed minimal context block
    (insert "#+title: Demo\n\n#+begin_context\nREADME.md\n#+end_context\n")
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (cl-letf (((symbol-function 'carriage-assist-context-delta)
                 (lambda (_ctx)
                   ;; Malformed schema: :add is not a list, :why wrong type
                   (list :add "not-a-list" :remove '() :why 42)))
                ;; Ensure the command never tries to prompt on invalid schema
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) (error "Should not prompt on invalid schema"))))
        (should-not (carriage-ui-context-delta-assist))
        (should (string=
                 before
                 (buffer-substring-no-properties (point-min) (point-max))))))))

;; Positive path: valid Assist delta → applies only after confirmation
(ert-deftest carriage-assist/ui-context-delta-valid-apply ()
  "UI command applies a valid Assist delta only after confirmation."
  (with-temp-buffer
    (require 'org)
    (require 'cl-lib)
    (require 'carriage-ui)
    (org-mode)
    ;; Seed minimal context block
    (insert "#+title: Demo\n\n#+begin_context\nREADME.md\n#+end_context\n")
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      (cl-letf (((symbol-function 'carriage-assist-context-delta)
                 (lambda (_ctx)
                   ;; Valid schema: add one file, remove one (README.md)
                   (list :add (list "lisp/carriage-task.el")
                         :remove (list "README.md")
                         :why "test-apply")))
                ;; Auto-confirm
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) t)))
        (should (carriage-ui-context-delta-assist))
        (let* ((after (buffer-substring-no-properties (point-min) (point-max))))
          (with-temp-buffer
            (insert after)
            (goto-char (point-min))
            ;; Removed
            (should-not (re-search-forward "^README\\.md$" nil t))
            ;; Added
            (goto-char (point-min))
            (should (re-search-forward "^lisp/carriage-task\\.el$" nil t))))))))

(provide 'carriage-assist-tests)
;;; carriage-assist-tests.el ends here
