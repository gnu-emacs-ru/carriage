;;; annotate-applied-patch-blocks-test.el --- Tests for annotating applied begin_patch headers  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-annotate-applied-blocks--sets-applied-flag ()
  "carriage--annotate-applied-blocks-in-report should mark header with :applied t."
  (unless (require 'org nil t)
    (ert-skip "org not available"))
  (unless (require 'carriage-apply nil t)
    (ert-skip "carriage-apply not available"))
  (with-temp-buffer
    (org-mode)
    ;; Minimal begin_patch without :applied
    (insert "#+begin_patch (:version \"1\" :op \"patch\" :strip 1 :path \"foo.txt\")\n")
    (insert "@@ -1,1 +1,1 @@\n-old\n+new\n")
    (insert "#+end_patch\n")
    ;; Prepare markers surrounding the block
    (goto-char (point-min))
    (let* ((mb (save-excursion
                 (search-forward "#+begin_patch" nil t)
                 (beginning-of-line)
                 (point-marker)))
           (me (save-excursion
                 (search-forward "#+end_patch" nil t)
                 (end-of-line)
                 (point-marker)))
           (plan (list :_buffer (current-buffer) :_beg-marker mb :_end-marker me))
           (row  (list :status 'ok :details "Applied" :engine 'git :_plan plan))
           (report (list :items (list row))))
      ;; Run annotator
      (carriage--annotate-applied-blocks-in-report report)
      ;; Validate header line now contains :applied t and preserves other keys
      (goto-char (marker-position mb))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (should (string-match-p "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" line))
        (should (string-match-p ":applied[ \t]+t" line))
        (should (string-match-p ":path[ \t]+\"foo.txt\"" line))))))
