;;; patched-files-detection-test.el --- Tests for detecting files from applied patch blocks  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-context--patched-files-from-applied-begin-patch ()
  "carriage-context--patched-files should extract file paths from applied begin_patch blocks."
  (unless (require 'org nil t)
    (ert-skip "org not available"))
  (unless (require 'carriage-context nil t)
    (ert-skip "carriage-context not available"))
  (with-temp-buffer
    (org-mode)
    ;; Applied unified-diff patch: expect :path to be collected
    (insert "#+begin_patch (:version \"1\" :op \"patch\" :strip 1 :applied t :path \"foo.txt\")\n")
    (insert "@@ -1 +1 @@\n-old\n+new\n")
    (insert "#+end_patch\n\n")
    ;; Applied SRE patch: expect :file to be collected
    (insert "#+begin_patch (:version \"1\" :op \"sre\" :applied t :file \"bar.txt\")\n")
    (insert "#+begin_from\nX\n#+end_from\n#+begin_to\nY\n#+end_to\n")
    (insert "#+end_patch\n")
    (let ((files (carriage-context--patched-files (current-buffer))))
      (should (member "foo.txt" files))
      (should (member "bar.txt" files)))))
