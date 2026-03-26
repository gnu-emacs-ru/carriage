;;; carriage-context-includes-text-test.el --- Tests for context file text inclusion  -*- lexical-binding: t; -*-
;; Specifications:
;;   spec/context-integration-v2.org
;;   spec/code-style-v2.org

(require 'ert)
(require 'cl-lib)

;;; Code:

(defun carriage-context-test--with-temp-file (content fn)
  "Create temp file with CONTENT, call FN with path."
  (let ((tmp (make-temp-file "carriage-ctx-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert content)
            (write-region (point-min) (point-max) tmp nil 'silent))
          (funcall fn tmp))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(ert-deftest carriage-context-collect-includes-file-text ()
  "Verify that carriage-context-collect includes actual file text, not just paths."
  (let* ((root (make-temp-file "carriage-ctx-root-" t))
         (default-directory root)
         (test-content "Line 1\nLine 2\nLine 3\n")
         (rel "sample.txt")
         (abs (expand-file-name rel root)))
    (unwind-protect
        (progn
          (with-temp-file abs
            (insert test-content))
          (with-temp-buffer
            (insert "#+begin_context\nsample.txt\n#+end_context\n")
            (let* ((result (carriage-context-collect (current-buffer) root))
                   (files (plist-get result :files))
                   (entry (cl-find-if (lambda (it)
                                        (equal (plist-get it :rel) rel))
                                      files)))
              (should (listp files))
              (should entry)
              (should (string= (plist-get entry :content) test-content)))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-context-collect-marks-has-text-true ()
  "Verify that formatted state manifest marks included file text with has_text=true."
  (let* ((ctx (list :files (list (list :rel "sample.txt"
                                       :true "/tmp/sample.txt"
                                       :content "hello\n"))
                    :warnings nil
                    :omitted 0
                    :stats (list :total-bytes 6 :included 1 :skipped 0)))
         (out (carriage-context-format ctx :where 'system)))
    (should (stringp out))
    (should (string-match-p "#\\+begin_state_manifest" out))
    (should (string-match-p "path|exists|has_text" out))
    (should (string-match-p "sample\\.txt|true|true" out))))

(ert-deftest carriage-context-collect-omits-content-for-large-files ()
  "Verify that files exceeding size limits are included as path-only with size-limit reason."
  (let* ((root (make-temp-file "carriage-ctx-root-" t))
         (default-directory root)
         (rel "big.txt")
         (abs (expand-file-name rel root))
         (big (make-string 128 ?A)))
    (unwind-protect
        (progn
          (with-temp-file abs
            (insert big))
          (with-temp-buffer
            (insert "#+begin_context\nbig.txt\n#+end_context\n")
            (let ((carriage-mode-context-max-total-bytes 16))
              (let* ((result (carriage-context-collect (current-buffer) root))
                     (files (plist-get result :files))
                     (entry (cl-find-if (lambda (it)
                                          (equal (plist-get it :rel) rel))
                                        files)))
                (should entry)
                (should (null (plist-get entry :content)))
                (should (eq (plist-get entry :reason) 'size-limit))))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-context-state-manifest-includes-path ()
  "Verify that state manifest rows include path, exists, has_text columns."
  (let* ((ctx (list :files (list (list :rel "a.txt" :true "/tmp/a.txt" :content "A")
                                 (list :rel "visible:/buf" :true nil :content nil))
                    :warnings nil
                    :omitted 0
                    :stats (list :total-bytes 1 :included 1 :skipped 1)))
         (out (carriage-context-format ctx :where 'system)))
    (should (string-match-p "path|exists|has_text" out))
    (should (string-match-p "a\\.txt|true|true" out))
    (should (string-match-p "visible:/buf|false|false" out))))

(ert-deftest carriage-context-normalizes-paths-relative-to-repo ()
  "Verify that collected file paths are repo-relative, not absolute."
  (let* ((root (make-temp-file "carriage-ctx-root-" t))
         (default-directory root)
         (rel "dir/sample.txt")
         (abs (expand-file-name rel root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory abs) t)
          (with-temp-file abs
            (insert "data\n"))
          (with-temp-buffer
            (insert "#+begin_context\n./dir/sample.txt\n#+end_context\n")
            (let* ((result (carriage-context-collect (current-buffer) root))
                   (files (plist-get result :files))
                   (entry (cl-find-if (lambda (it)
                                        (equal (plist-get it :rel) rel))
                                      files)))
              (should entry)
              (should (stringp (plist-get entry :rel)))
              (should-not (file-name-absolute-p (plist-get entry :rel)))
              (should-not (string-match-p "\\.\\." (plist-get entry :rel))))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-context-format-in-file-section-carries-current-text-contract ()
  "Formatted context must make file-body visibility explicit for the model."
  (let* ((ctx (list :files (list (list :rel "lisp/x.el"
                                       :true "/tmp/lisp/x.el"
                                       :content "(message \"hi\")\n"))
                    :warnings nil
                    :omitted 0
                    :stats (list :total-bytes 15 :included 1 :skipped 0)))
         (out (carriage-context-format ctx :where 'system)))
    (should (stringp out))
    (should (string-match-p "In file lisp/x\\.el:" out))
    (should (string-match-p "CURRENT TEXT PRESENT IN THIS REQUEST" out))
    (should (string-match-p "#\\+begin_src emacs-lisp" out))
    (should (string-match-p "(message \"hi\")" out))))

(ert-deftest carriage-context-format-in-file-section-marks-authoritative-current-text ()
  "Formatted context must explicitly say that the In file body is authoritative and not missing."
  (let* ((ctx (list :files (list (list :rel "lisp/x.el"
                                       :true "/tmp/lisp/x.el"
                                       :content "(message \"hi\")\n"))
                    :warnings nil
                    :omitted 0
                    :stats (list :total-bytes 15 :included 1 :skipped 0)))
         (out (carriage-context-format ctx :where 'system)))
    (should (stringp out))
    (should (string-match-p "AUTHORITATIVE CURRENT TEXT FOR THIS PATH" out))
    (should (string-match-p "MUST NOT CLAIM THAT THIS FILE TEXT IS MISSING" out))))

(ert-deftest carriage-context-format-in-file-section-marks-body-as-visible-to-model ()
  "Formatted context must explicitly say that the full file body is visible to the model."
  (let* ((ctx (list :files (list (list :rel "lisp/x.el"
                                       :true "/tmp/lisp/x.el"
                                       :content "(message \"hi\")\n"))
                    :warnings nil
                    :omitted 0
                    :stats (list :total-bytes 15 :included 1 :skipped 0)))
         (out (carriage-context-format ctx :where 'system)))
    (should (stringp out))
    (should (string-match-p "THIS FULL FILE BODY IS VISIBLE TO THE MODEL IN THE CURRENT REQUEST" out))
    (should (string-match-p "THIS MEANS has_text=true FOR THIS PATH IN THE CURRENT REQUEST" out))))

(ert-deftest carriage-context-format-in-file-section-says-text-is-already-present-in-context ()
  "Formatted context must explicitly say that seeing In file with body means text is already present."
  (let* ((ctx (list :files (list (list :rel "lisp/x.el"
                                       :true "/tmp/lisp/x.el"
                                       :content "(message \"hi\")\n"))
                    :warnings nil
                    :omitted 0
                    :stats (list :total-bytes 15 :included 1 :skipped 0)))
         (out (carriage-context-format ctx :where 'system)))
    (should (stringp out))
    (should (string-match-p "IF YOU CAN SEE THIS `In file <path>:` SECTION WITH A BODY, THEN THE FILE TEXT IS PRESENT IN CONTEXT" out))
    (should (string-match-p "DO NOT ASK FOR begin_context FOR THIS SAME PATH" out))))

(ert-deftest carriage-context-format-in-file-section-says-body-is-real-current-file-text ()
  "Formatted context must explicitly say that the In file body is the real current file contents."
  (let* ((ctx (list :files (list (list :rel "lisp/x.el"
                                       :true "/tmp/lisp/x.el"
                                       :content "(message \"hi\")\n"))
                    :warnings nil
                    :omitted 0
                    :stats (list :total-bytes 15 :included 1 :skipped 0)))
         (out (carriage-context-format ctx :where 'system)))
    (should (stringp out))
    (should (string-match-p "THIS `In file <path>:` BODY IS THE REAL CURRENT FILE TEXT" out))
    (should (string-match-p "EDITS FOR THIS EXACT PATH ARE ALLOWED TO RELY ON THIS BODY AS VISIBLE CURRENT TEXT" out))))

(provide 'carriage-context-includes-text-test)
;;; carriage-context-includes-text-test.el ends here
