(require 'ert)

(require 'carriage-mode)

(ert-deftest carriage-payload-summarize-patch-blocks/collapse-and-keep ()
  (let* ((txt
          (concat
           "Preamble\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"a.txt\" :description \"Add A\")\n"
           "AAA\n"
           "#+end_patch\n"
           "\n"
           "#+begin_patch (:version \"1\" :op \"rename\" :from \"old.txt\" :to \"new.txt\" :description \"Rename\")\n"
           "IGNORED\n"
           "#+end_patch\n"
           "\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"keep.txt\" :description \"Keep me\" :keep t)\n"
           "BODY-KEPT\n"
           "#+end_patch\n"
           "Tail\n"))
         (out (carriage--payload-summarize-patch-blocks txt)))
    (should (string-match-p ";; patch history: a\\.txt — Add A" out))
    (should (string-match-p ";; patch history: old\\.txt → new\\.txt — Rename" out))
    (should-not (string-match-p "AAA" out))
    (should-not (string-match-p "IGNORED" out))
    (should (string-match-p "#\\+begin_patch\\b.*:keep t" out))
    (should (string-match-p "BODY-KEPT" out))))

(ert-deftest carriage-payload-summarize-patch-blocks/collapse-and-keep ()
  (let* ((txt
          (concat
           "Preamble\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"a.txt\" :description \"Add A\")\n"
           "AAA\n"
           "#+end_patch\n"
           "\n"
           "#+begin_patch (:version \"1\" :op \"rename\" :from \"old.txt\" :to \"new.txt\" :description \"Rename\")\n"
           "IGNORED\n"
           "#+end_patch\n"
           "\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"keep.txt\" :description \"Keep me\" :keep t)\n"
           "BODY-KEPT\n"
           "#+end_patch\n"
           "Tail\n"))
         (out (carriage--payload-summarize-patch-blocks txt)))
    ;; Collapsed blocks: no begin/end markers for those.
    (should (string-match-p ";; patch history: a\\.txt — Add A" out))
    (should (string-match-p ";; patch history: old\\.txt → new\\.txt — Rename" out))
    (should-not (string-match-p "AAA" out))
    (should-not (string-match-p "IGNORED" out))
    ;; Kept block remains intact (escape hatch).
    (should (string-match-p "#\\+begin_patch\\b.*:keep t" out))
    (should (string-match-p "BODY-KEPT" out))))

(ert-deftest carriage-payload-summarize-patch-blocks/unterminated-block ()
  (let* ((txt
          (concat
           "X\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"a.txt\" :description \"Add A\")\n"
           "AAA\n"))
         (out (carriage--payload-summarize-patch-blocks txt)))
    (should (string-match-p ";; patch history: a\\.txt — Add A" out))
    (should-not (string-match-p "#\\+begin_patch" out))
    (should-not (string-match-p "AAA" out))))

(ert-deftest carriage-sanitize-payload-for-llm/collapses-all-patches-except-keep ()
  (let* ((txt
          (concat
           "Preamble\n"
           "#+PROPERTY: CARRIAGE_STATE something\n"
           "#+CARRIAGE_FINGERPRINT: (:X 1)\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"a.txt\" :description \"Add A\")\n"
           "AAA\n"
           "#+end_patch\n"
           "\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"keep.txt\" :keep t)\n"
           "BODY-KEPT\n"
           "#+end_patch\n"
           "Tail\n"))
         (out (carriage--sanitize-payload-for-llm txt)))
    (should-not (string-match-p "#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b" out))
    (should-not (string-match-p "#\\+CARRIAGE_FINGERPRINT\\b" out))
    (should (string-match-p ";; patch history: a\\.txt — Add A" out))
    (should-not (string-match-p "AAA" out))
    (should-not (string-match-p "#\\+begin_patch\\b.*a\\.txt" out))
    (should (string-match-p "#\\+begin_patch\\b.*:keep t" out))
    (should (string-match-p "BODY-KEPT" out))))

(ert-deftest carriage-sanitize-payload-for-llm/collapses-all-patches-except-keep ()
  (let* ((txt
          (concat
           "Preamble\n"
           "#+PROPERTY: CARRIAGE_STATE something\n"
           "#+CARRIAGE_FINGERPRINT: (:X 1)\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"a.txt\" :description \"Add A\")\n"
           "AAA\n"
           "#+end_patch\n"
           "\n"
           "#+begin_patch (:version \"1\" :op \"create\" :file \"keep.txt\" :keep t)\n"
           "BODY-KEPT\n"
           "#+end_patch\n"
           "Tail\n"))
         (out (carriage--sanitize-payload-for-llm txt)))
    (should-not (string-match-p "#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b" out))
    (should-not (string-match-p "#\\+CARRIAGE_FINGERPRINT\\b" out))
    ;; Collapsed patch: history marker present, body removed, and no begin_patch mentioning a.txt.
    (should (string-match-p ";; patch history: a\\.txt — Add A" out))
    (should-not (string-match-p "AAA" out))
    (should-not (string-match-p "#\\+begin_patch\\b.*a\\.txt" out))
    ;; Kept block remains intact.
    (should (string-match-p "#\\+begin_patch\\b.*:keep t" out))
    (should (string-match-p "BODY-KEPT" out))))
