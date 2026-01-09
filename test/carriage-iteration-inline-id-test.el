;;; carriage-iteration-inline-id-test.el  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-iteration-read-id-inline-fallback ()
  (with-temp-buffer
    (org-mode)
    (insert "* Header\nSome text\n")
    (insert "#+CARRIAGE_FINGERPRINT: (:CAR_TS 0)\n")
    (insert "#+begin_patch (:version \"1\" :op \"sre\" :file \"a.txt\")\n#+begin_from\na\n#+end_from\n#+begin_to\nb\n#+end_to\n#+end_patch\n")
    (goto-char (point-min))
    (let ((id (progn (require 'carriage-iteration) (carriage-iteration-read-id))))
      (should (string= id "87105ad1c6eea77df26686308df00116")))))
