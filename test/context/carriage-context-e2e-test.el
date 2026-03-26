;;; carriage-context-e2e-test.el --- E2E test: context file bodies reach payload  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; License: GPL-3+

;;; Commentary:
;; End-to-end test verifying that file bodies from begin_context blocks
;; actually reach the final prompt payload sent to LLM.
;;
;; This test catches the bug where context is collected but not passed
;; to carriage-build-prompt as :context-text.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'carriage-mode)
(require 'carriage-context)
(require 'carriage-suite)
(require 'carriage)

(ert-deftest carriage-context-e2e-file-bodies-reach-prompt ()
  "File bodies from begin_context must reach carriage-build-prompt as :context-text."
  (let* ((root (make-temp-file "carriage-e2e-" t))
         (default-directory root)
         (rel "sample.el")
         (abs (expand-file-name rel root))
         (sample-content "(defun sample-fn () :ok)\n")
         ctx ctx-text)
    (unwind-protect
        (progn
          ;; Create sample file
          (with-temp-file abs
            (insert sample-content))
          ;; Create buffer with begin_context
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_context\n")
            (insert rel "\n")
            (insert "#+end_context\n")
            ;; Collect context
            (setq ctx (carriage-context-collect (current-buffer) root))
            (should ctx)
            (should (plist-get ctx :files))
            ;; Verify file content was collected
            (let* ((files (plist-get ctx :files))
                   (entry (cl-find-if (lambda (e) (equal (plist-get e :rel) rel)) files)))
              (should entry)
              (should (stringp (plist-get entry :content)))
              (should (string-match-p (regexp-quote sample-content)
                                      (plist-get entry :content))))
            ;; Format context
            (setq ctx-text (carriage-context-format ctx :where 'system))
            (should (stringp ctx-text))
            ;; Verify formatted output contains In file section with body
            (should (string-match-p (format "In file %s:" (regexp-quote rel)) ctx-text))
            (should (string-match-p (regexp-quote sample-content) ctx-text))
            (should (string-match-p "CURRENT TEXT PRESENT IN THIS REQUEST" ctx-text))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-context-e2e-build-prompt-receives-context-text ()
  "carriage-build-prompt must receive :context-text with file bodies."
  (let* ((root (make-temp-file "carriage-e2e-" t))
         (default-directory root)
         (rel "test-file.el")
         (abs (expand-file-name rel root))
         (sample-content ";; Test content\n(message \"hello\")\n")
         result system-prompt)
    (unwind-protect
        (progn
          ;; Create sample file
          (with-temp-file abs
            (insert sample-content))
          ;; Create buffer with begin_context
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_context\n")
            (insert rel "\n")
            (insert "#+end_context\n")
            ;; Call build-prompt with context
            (let* ((ctx (carriage-context-collect (current-buffer) root))
                   (ctx-text (carriage-context-format ctx :where 'system))
                   (payload (list :payload "Test request"
                                  :context-text ctx-text
                                  :context-target 'system)))
              (setq result (carriage-build-prompt 'Code 'sre payload))
              (setq system-prompt (plist-get result :system))))
          ;; Verify system prompt contains file body
          (should (stringp system-prompt))
          (should (string-match-p (format "In file %s:" (regexp-quote rel)) system-prompt))
          (should (string-match-p (regexp-quote sample-content) system-prompt))
          (should (string-match-p "CURRENT TEXT" system-prompt)))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-context-e2e-state-manifest-and-file-body-consistent ()
  "begin_state_manifest has_text must match presence of In file body."
  (let* ((root (make-temp-file "carriage-e2e-" t))
         (default-directory root)
         (rel "manifest-test.el")
         (abs (expand-file-name rel root))
         (sample-content "(defun test () t)\n")
         ctx ctx-text)
    (unwind-protect
        (progn
          (with-temp-file abs
            (insert sample-content))
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_context\n")
            (insert rel "\n")
            (insert "#+end_context\n")
            (setq ctx (carriage-context-collect (current-buffer) root))
            (setq ctx-text (carriage-context-format ctx :where 'system)))
          ;; Verify manifest says has_text=true when content is present
          (should (string-match-p (format "%s|true|true" (regexp-quote rel)) ctx-text))
          ;; Verify In file section exists with body
          (should (string-match-p (format "In file %s:" (regexp-quote rel)) ctx-text))
          (should (string-match-p (regexp-quote sample-content) ctx-text)))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-context-e2e-omitted-file-marks-has-text-false ()
  "When file is omitted (size limit), has_text should be false and no body in output."
  (let* ((root (make-temp-file "carriage-e2e-" t))
         (default-directory root)
         (rel "large-file.el")
         (abs (expand-file-name rel root))
         ;; Create content larger than typical include limit
         (sample-content (concat (make-string 500000 ?x) "\n"))
         ctx ctx-text)
    (unwind-protect
        (progn
          (with-temp-file abs
            (insert sample-content))
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_context\n")
            (insert rel "\n")
            (insert "#+end_context\n")
            (let ((carriage-context-max-bytes 100000)) ; Lower limit for test
              (setq ctx (carriage-context-collect (current-buffer) root))
              (setq ctx-text (carriage-context-format ctx :where 'system))))
          ;; Verify manifest says has_text=false when omitted
          (should (string-match-p (format "%s|true|false" (regexp-quote rel)) ctx-text))
          ;; Verify no literal body in output (may have [content omitted] marker)
          (should-not (string-match-p (regexp-quote (substring sample-content 0 100)) ctx-text)))
      (ignore-errors (delete-directory root t)))))

(provide 'carriage-context-e2e-test)
;;; carriage-context-e2e-test.el ends here
