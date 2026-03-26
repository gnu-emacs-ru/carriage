;;; payload-patch-history-test.el --- tests for patch history payload sanitization -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'carriage-mode)

(ert-deftest carriage-payload-summarize-patch-blocks-basic ()
  (let* ((input
          (concat
           "hello\n"
           "#+begin_patch (:version \"1\" :op \"aibo\" :file \"a.txt\" :description \"d1\")\n"
           "#+pair (:occur first :expect 1)\n"
           "#+begin_from\nA\n#+end_from\n"
           "#+begin_to\nB\n#+end_to\n"
           "#+end_patch\n"
           "world\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"b.txt\" :description \"d2\" :applied t)\n"
           "content\n"
           "#+end_patch\n"))
         (out (carriage--payload-summarize-patch-blocks input))
         (lines (split-string out "\n" nil)))
    (should (string-match-p "hello" out))
    (should (string-match-p "world" out))
    (should-not (string-match-p "#\\+begin_patch\\b" out))
    (should-not (string-match-p "#\\+end_patch\\b" out))
    (should (string-match-p "a.txt" out))
    (should (string-match-p "b.txt" out))
    (should (= (cl-count-if (lambda (s) (string-match-p "^;; patch history:" s)) lines)
               2))))

(ert-deftest carriage-payload-summarize-patch-blocks-keep ()
  (let* ((input
          (concat
           "#+begin_patch (:version \"1\" :op \"create\" :file \"keep.txt\" :keep t :description \"k\")\n"
           "X\n"
           "#+end_patch\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"drop.txt\" :description \"d\")\n"
           "Y\n"
           "#+end_patch\n"))
         (out (carriage--payload-summarize-patch-blocks input))
         (lines (split-string out "\n" nil)))
    ;; Kept block still contains begin_patch, dropped one is summarized.
    (should (string-match-p "#\\+begin_patch\\b" out))
    (should (= (cl-count-if (lambda (s) (string-match-p "^[ \t]*#\\+begin_patch\\b" s)) lines)
               1))
    (should (string-match-p "keep.txt" out))
    (should (string-match-p "drop.txt" out))
    (should (string-match-p ";; patch history:" out))))
