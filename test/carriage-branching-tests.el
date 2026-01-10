;;; carriage-branching-tests.el --- Tests for branching provenance and context blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; ERT tests for:
;; - Writing provenance via doc-state (#+PROPERTY: CARRIAGE_STATE).
;; - Inserting begin_context blocks with given paths.

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-task)
(require 'carriage-doc-state)

(ert-deftest carriage-branching/insert-begin-context-with-paths ()
  "Insert a begin_context block with provided paths at a sensible location."
  (with-temp-buffer
    (org-mode)
    ;; Insert a minimal header and Context section to anchor insertion
    (insert "#+title: Demo\n\n* Context\n\n* Plan\n")
    (let* ((paths '("lisp/carriage-task.el" "spec/document-branching-and-templates-v1.org")))
      (carriage-task--insert-begin-context-in-buffer (current-buffer) paths))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (should (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t))
      (should (re-search-forward "^lisp/carriage-task\\.el$" nil t))
      (should (re-search-forward "^spec/document-branching-and-templates-v1\\.org$" nil t))
      (should (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)))))

(provide 'carriage-branching-tests)
;;; carriage-branching-tests.el ends here
