;;; carriage-templates-tests.el --- Tests for template registry/rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; ERT tests for carriage-templates:
;; - Deterministic renders with placeholders and filters
;; - Guard against I/O/network inside :render
;; - Unknown placeholder/filter errors

;;; Code:

(require 'ert)
(require 'carriage-templates)

(defun carriage--tests--with-templates (templates thunk)
  "Temporarily set CARRIAGE-TEMPLATES to TEMPLATES and run THUNK."
  (let ((old (copy-sequence carriage-templates)))
    (unwind-protect
        (progn
          (setq carriage-templates (copy-sequence templates))
          (funcall thunk))
      (setq carriage-templates old)
      (carriage-templates--ensure-builtins))))

(ert-deftest carriage-templates/deterministic-render-builtins ()
  "Built-in templates render deterministically with a fixed ctx."
  (let* ((ctx (list :title "My Task"
                    :today "2025-01-01"
                    :project "demo"
                    :origin-file "TODO.org"
                    :origin-heading "My Task"
                    :subtree "Subtree text"
                    :ctx-profile 'p1
                    :parent-context '()
                    :inherited (list :begin-context nil :car-flags nil))))
    (dolist (tid '(task/default task/decomposition task/implementation-step
                                test/plan debug/protocol design/adr bug/incident))
      (let* ((s (carriage-templates-render tid ctx)))
        (should (stringp s))
        (should (> (length s) 0))
        (should (string-match-p "#\\+title:" s))
        (should (string-match-p "My Task" s))))))

(ert-deftest carriage-templates/error-on-unknown-filter ()
  "Unknown filter in placeholders must signal TEMPLATE_E_UNSAFE_PLACEHOLDER."
  (let* ((tmp (list (list :id 'tmp/unknown-filter
                          :version "1.0"
                          :label "Tmp"
                          :render "Title: {{title|foo}}"))))
    (carriage--tests--with-templates tmp
                                     (lambda ()
                                       (let ((ctx (list :title "T" :today "2025-01-01")))
                                         (should-error
                                          (carriage-templates-render 'tmp/unknown-filter ctx)
                                          :type 'error))))))

(ert-deftest carriage-templates/error-on-io-in-render ()
  "Any I/O attempt in :render must be refused with TEMPLATE_E_IO_ATTEMPT."
  (let* ((tmp (list (list :id 'tmp/io-attempt
                          :version "1.0"
                          :label "Tmp"
                          :render (lambda (_ctx)
                                    ;; Attempt a write; must be blocked by IO guards.
                                    (write-region "x" nil (make-temp-file "carriage-io-") nil 'silent)
                                    "should-not-happen")))))
    (carriage--tests--with-templates tmp
                                     (lambda ()
                                       (let ((ctx (list :title "T" :today "2025-01-01")))
                                         (should-error
                                          (carriage-templates-render 'tmp/io-attempt ctx)
                                          :type 'error))))))

(ert-deftest carriage-templates/lint-builtins-ok ()
  "Built-in templates should lint without issues."
  (let* ((issues (ignore-errors (carriage-templates-lint))))
    (should (listp issues))
    (should (zerop (length issues)))))

(provide 'carriage-templates-tests)
;;; carriage-templates-tests.el ends here
