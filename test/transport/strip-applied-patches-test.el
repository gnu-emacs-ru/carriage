;;; strip-applied-patches-test.el --- Tests for stripping applied blocks from payload  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-build-context--strips-applied-begin-patch ()
  "Applied begin_patch block contents must not be included in outgoing payload."
  (unless (require 'org nil t)
    (ert-skip "org not available"))
  (unless (require 'carriage-mode nil t)
    (ert-skip "carriage-mode not available"))
  (with-temp-buffer
    (org-mode)
    ;; Insert some preface text that should stay
    (insert "* Title\nSome intro text.\n\n")
    ;; Insert an applied begin_patch block
    (insert "#+begin_patch (:version \"1\" :op \"patch\" :strip 1 :applied t :path \"foo.txt\" :description \"Applied demo\")\n")
    (insert "@@ -1,1 +1,1 @@\nUNIQUE_BODY_SHOULD_NOT_LEAK\n")
    (insert "#+end_patch\n\n")
    ;; Insert some trailing text that should stay
    (insert "Tail text.\n")
    ;; Build context payload directly from helper
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (payload (plist-get ctx :payload)))
      (should (stringp payload))
      ;; The unique marker from applied block must be stripped
      (should-not (string-match-p "UNIQUE_BODY_SHOULD_NOT_LEAK" payload))
      ;; Non-patch text remains present
      (should (string-match-p "Some intro text" payload))
      (should (string-match-p "Tail text" payload)))))
