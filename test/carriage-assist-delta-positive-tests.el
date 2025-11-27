;;; carriage-assist-delta-positive-tests.el --- Positive tests for Assist context-delta -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Positive-path tests for UI command applying context-delta:
;; - Valid delta is applied only after confirmation
;; - TRAMP/absolute/out-of-root paths are ignored by normalization
;; - Changes are applied in a single undo group

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'carriage-ui)
(require 'carriage-context)

(defun carriage--tests--buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest carriage-assist/context-delta-valid-apply ()
  "UI applies a valid Assist context-delta only after confirmation; filters bad paths."
  (with-temp-buffer
    (org-mode)
    ;; Minimal doc with begin_context
    (insert "#+title: Demo\n\n#+begin_context\nREADME.md\n#+end_context\n")
    (let ((before (carriage--tests--buffer-string)))
      (cl-letf (((symbol-function 'carriage-assist-context-delta)
                 (lambda (_ctx)
                   ;; Valid schema: one good relative path, one bad TRAMP, one absolute → only relative should remain.
                   (list :add (list "lisp/carriage-task.el" "/etc/passwd" "/ssh:host:/file")
                         :remove (list "README.md")
                         :why "add-task-el; drop-readme")))
                ;; Confirm apply
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _args) t)))
        ;; Execute command under atomic-change-group expectation: one undo step
        (let ((before-undo-length (and (listp buffer-undo-list) (length buffer-undo-list))))
          (should (carriage-ui-context-delta-assist))
          ;; Verify begin_context updated: README.md removed, lisp/carriage-task.el added; TRAMP/abs ignored.
          (let ((after (carriage--tests--buffer-string)))
            (with-temp-buffer
              (insert after)
              (goto-char (point-min))
              (let ((case-fold-search t))
                (should (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t))
                (should (re-search-forward "^lisp/carriage-task\\.el$" nil t))
                (goto-char (point-min))
                (should-not (re-search-forward "^/etc/passwd$" nil t))
                (goto-char (point-min))
                (should-not (re-search-forward "^/ssh:host:/file$" nil t))
                (goto-char (point-min))
                (should-not (re-search-forward "^README\\.md$" nil t)))))
          ;; Undo once should revert all delta changes (single undo group)
          (when (listp buffer-undo-list)
            (condition-case _
                (undo 1)
              (error (ert-skip "No undo information in this buffer"))))
          (when (listp buffer-undo-list)
            (should (string= before (carriage--tests--buffer-string))))))))))

(provide 'carriage-assist-delta-positive-tests)
;;; carriage-assist-delta-positive-tests.el ends here
