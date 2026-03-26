;;; strip-patch-blocks-test.el --- sanitize payload: strip patch blocks  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'seq)

(require 'carriage-mode)

(ert-deftest carriage-strip-patch-blocks-in-payload-test ()
  "Outgoing payload must not contain begin_patch markers; they become one-line history markers."
  (let* ((input
          (string-join
           '("#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t)"
             "#+CARRIAGE_FINGERPRINT: (:CAR_TS 0)"
             "plain"
             "#+begin_patch (:version \"1\" :op \"create\" :file \"a.txt\" :description \"mk\")"
             "hello"
             "#+end_patch"
             "mid"
             "#+begin_patch (:version \"1\" :op \"patch\" :path \"b.txt\" :description \"edit\")"
             "--- a/b.txt"
             "+++ b/b.txt"
             "@@ -1 +1 @@"
             "-x"
             "+y"
             "#+end_patch"
             "tail")
           "\n"))
         (out (carriage--sanitize-payload-for-llm input)))
    ;; Non-patch content remains.
    (should (string-match-p "\\bplain\\b" out))
    (should (string-match-p "\\bmid\\b" out))
    (should (string-match-p "\\btail\\b" out))
    ;; Internal markers removed.
    (should-not (string-match-p "^#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b" out))
    (should-not (string-match-p "^#\\+CARRIAGE_FINGERPRINT\\b" out))
    ;; Patch markers removed.
    (should-not (string-match-p "^[ \t]*#\\+begin_patch\\b" out))
    (should-not (string-match-p "^[ \t]*#\\+end_patch\\b" out))
    ;; Exactly 2 history markers inserted.
    (let* ((lines (split-string out "\n" t))
           (markers
            (seq-filter (lambda (ln)
                          (string-prefix-p ";; patch history:" (string-trim-left ln)))
                        lines)))
      (should (= (length markers) 2))
      (should (seq-some (lambda (ln) (string-match-p "a\\.txt" ln)) markers))
      (should (seq-some (lambda (ln) (string-match-p "b\\.txt" ln)) markers)))))

(ert-deftest carriage-sanitize-payload-for-llm-keeps-in-file-bodies ()
  "Sanitize must preserve In file sections and their src bodies."
  (let* ((input
          (string-join
           '("In file lisp/carriage-mode.el:"
             ";; CURRENT TEXT PRESENT IN THIS REQUEST"
             "#+begin_src emacs-lisp"
             "(defun carriage-test () t)"
             "#+end_src")
           "\n"))
         (out (carriage--sanitize-payload-for-llm input)))
    (should (string-match-p "In file lisp/carriage-mode\\.el:" out))
    (should (string-match-p "CURRENT TEXT PRESENT IN THIS REQUEST" out))
    (should (string-match-p "(defun carriage-test () t)" out))
    (should (string-match-p "#\\+begin_src emacs-lisp" out))
    (should (string-match-p "#\\+end_src" out))))

(provide 'strip-patch-blocks-test)
;;; strip-patch-blocks-test.el ends here
