;;; payload-summarize-patch-blocks-regression-test.el --- regression tests  -*- lexical-binding: t; -*-

(require 'ert)

(require 'carriage-mode)

(ert-deftest carriage-payload-summarize-patch-blocks--collapses-all ()
  (let* ((in (concat
              "before\n"
              "#+begin_patch (:version \"1\" :op \"patch\" :path \"a.txt\" :description \"d1\")\n"
              "PATCHBODY1\n"
              "#+end_patch\n"
              "middle\n"
              "#+begin_patch (:version \"1\" :op \"create\" :file \"b.txt\")\n"
              "CONTENT2\n"
              "#+end_patch\n"
              "after\n"))
         (out (carriage--payload-summarize-patch-blocks in)))
    (should (not (string-match-p "#\\+begin_patch\\b" out)))
    (should (not (string-match-p "#\\+end_patch\\b" out)))
    (should (string-match-p (regexp-quote ";; patch history: a.txt — d1") out))
    (should (string-match-p (regexp-quote ";; patch history: b.txt — (no description)") out))))

(ert-deftest carriage-payload-summarize-patch-blocks--keep-preserves-block ()
  (let* ((in (concat
              "#+begin_patch (:version \"1\" :op \"patch\" :path \"a.txt\" :keep t :description \"k\")\n"
              "PATCHBODY\n"
              "#+end_patch\n"))
         (out (carriage--payload-summarize-patch-blocks in)))
    (should (string-match-p "#\\+begin_patch\\b" out))
    (should (string-match-p "#\\+end_patch\\b" out))
    (should (not (string-match-p ";; patch history:" out)))))

(ert-deftest carriage-payload-summarize-patch-blocks--unterminated-collapses-to-eof ()
  (let* ((in (concat
              "x\n"
              "#+begin_patch (:version \"1\" :op \"patch\" :path \"a.txt\")\n"
              "BROKEN\n"))
         (out (carriage--payload-summarize-patch-blocks in)))
    (should (not (string-match-p "#\\+begin_patch\\b" out)))
    (should (string-match-p (regexp-quote ";; patch history: a.txt — (no description)") out))))

(provide 'payload-summarize-patch-blocks-regression-test)
;;; payload-summarize-patch-blocks-regression-test.el ends here
