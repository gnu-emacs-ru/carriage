;;; carriage-aibo-sanitize-test.el --- AIBO sanitize/validation tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)

(require 'carriage)

(ignore-errors (require 'carriage-op-aibo))
(require 'carriage-errors)

(defun carriage-aibo--tmp-root ()
  (make-temp-file "carriage-aibo-" t))

(ert-deftest carriage-aibo-disallows-match-key ()
  "AIBO must reject any :match key (literal-only)."
  (let* ((root (carriage-aibo--tmp-root))
         (hdr  '(:version "1" :op "aibo" :file "x.txt"))
         (body (mapconcat #'identity
                          '("#+pair (:match regex)"
                            "#+begin_from"
                            "A"
                            "#+end_from"
                            "#+begin_to"
                            "B"
                            "#+end_to")
                          "\n")))
    (unwind-protect
        (should-error
         (carriage-parse-aibo hdr body root)
         :type (carriage-error-symbol 'SRE_E_REGEX_SYNTAX))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-aibo-occur-all-requires-expect ()
  "AIBO uses SRE parser; :occur all must require :expect."
  (let* ((root (carriage-aibo--tmp-root))
         (hdr  '(:version "1" :op "aibo" :file "x.txt"))
         (body (mapconcat #'identity
                          '("#+pair (:occur all)"
                            "#+begin_from"
                            "foo"
                            "#+end_from"
                            "#+begin_to"
                            "bar"
                            "#+end_to")
                          "\n")))
    (unwind-protect
        (should-error
         (carriage-parse-aibo hdr body root)
         :type (carriage-error-symbol 'SRE_E_OCCUR_EXPECT))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-aibo-forces-literal-match ()
  "AIBO must force :match 'literal on pairs without :match."
  (let* ((root (carriage-aibo--tmp-root))
         (hdr  '(:version "1" :op "aibo" :file "x.txt"))
         (body (mapconcat #'identity
                          '("#+begin_from"
                            "hello"
                            "#+end_from"
                            "#+begin_to"
                            "world"
                            "#+end_to")
                          "\n"))
         (plan (carriage-parse-aibo hdr body root))
         (pairs (alist-get :pairs plan))
         (opts  (alist-get :opts (car pairs))))
    (unwind-protect
        (progn
          (should (eq (plist-get opts :match) 'literal))
          (should (memq (plist-get opts :occur) '(nil first all))) ;; default is 'first
          (should (or (null (plist-get opts :occur))
                      (eq (plist-get opts :occur) 'first))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-sanitize-create-body-strips-legacy-markers ()
  "Create-body sanitizer should remove legacy delimiters and stray end markers."
  (should (equal (carriage--sanitize-create-body "<<abcdef\nline1\nline2\n:abcdef\n#+end\n")
                 "line1\nline2")))

(ert-deftest carriage-sanitize-patch-header-drops-delim-for-create ()
  "Create patch headers should drop legacy :delim fields."
  (should (equal (carriage--sanitize-patch-header '(:op create :delim "abcdef" :file "x.txt"))
                 '(:op create :file "x.txt"))))

(provide 'carriage-aibo-sanitize-test)
;;; carriage-aibo-sanitize-test.el ends here
