;;; payload-summarize-patch-blocks-keep-test.el --- Tests for patch-block summarization  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-transport)

(ert-deftest carriage-payload-summarize-patch-blocks-keep-t ()
  "Non-:keep patch blocks must collapse into one-line markers; :keep t blocks must survive."
  (let* ((input
          (concat
           "hello\n"
           "#+begin_patch (:version \"1\" :op \"aibo\" :file \"a.txt\" :description \"D\")\n"
           "#+begin_from\nold\n#+end_from\n"
           "#+begin_to\nnew\n#+end_to\n"
           "#+end_patch\n"
           "mid\n"
           "#+begin_patch (:version \"1\" :op \"aibo\" :file \"b.txt\" :keep t :description \"K\")\n"
           "BODY\n"
           "#+end_patch\n"
           "tail\n"))
         (out (carriage--payload-summarize-patch-blocks input)))
    ;; Collapsed block: no markers remain, but history line exists.
    (should (string-match-p ";; patch history: aibo a\\.txt — D" out))
    (should-not (string-match-p "#\\+begin_patch.*a\\.txt" out))
    (should-not (string-match-p "#\\+begin_from\\b" out))
    (should-not (string-match-p "#\\+begin_to\\b" out))
    ;; Kept block remains intact.
    (should (string-match-p "#\\+begin_patch.*b\\.txt" out))
    (should (string-match-p "BODY" out))
    (should (string-match-p "#\\+end_patch" out))))

(ert-deftest carriage-payload-summarize-patch-blocks-unterminated ()
  "Unterminated patch blocks must not crash and should collapse to a history marker."
  (let* ((input
          (concat
           "x\n"
           "#+begin_patch (:version \"1\" :op \"aibo\" :file \"u.txt\" :description \"U\")\n"
           "some body\n"
           "EOF\n"))
         (out (carriage--payload-summarize-patch-blocks input)))
    (should (string-match-p ";; patch history: aibo u\\.txt — U" out))
    (should-not (string-match-p "#\\+begin_patch\\b" out))))
(provide 'payload-summarize-patch-blocks-keep-test)
;;; payload-summarize-patch-blocks-keep-test.el ends here
