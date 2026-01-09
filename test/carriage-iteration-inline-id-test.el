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

(ert-deftest carriage-iteration-read-id-prefers-property-over-inline ()
  (with-temp-buffer
    (org-mode)
    (insert "#+PROPERTY: CARRIAGE_ITERATION_ID deadbeefdeadbeefdeadbeefdeadbeef\n")
    (insert "#+CARRIAGE_ITERATION_ID: 87105ad1c6eea77df26686308df00116\n")
    (let ((id (progn (require 'carriage-iteration) (carriage-iteration-read-id))))
      (should (string= id "deadbeefdeadbeefdeadbeefdeadbeef")))))
