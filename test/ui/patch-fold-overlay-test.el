;;; patch-fold-overlay-test.el --- Tests for folding applied begin_patch blocks  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-patch-fold--folds-applied-blocks ()
  "Ensure applied begin_patch blocks are folded with an overlay placeholder."
  (unless (require 'org nil t)
    (ert-skip "org not available"))
  (unless (require 'carriage-patch-fold nil t)
    (ert-skip "carriage-patch-fold not available"))
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_patch (:version \"1\" :op \"patch\" :strip 1 :applied t :description \"Demo change\" :path \"foo.txt\")\n")
    (insert "@@ -1,1 +1,1 @@\n-old\n+new\n")
    (insert "#+end_patch\n")
    (carriage-patch-fold-enable (current-buffer))
    ;; Trigger refresh explicitly
    (ignore-errors (carriage-patch-fold-refresh))
    ;; Implementation detail: module maintains overlays list buffer-locally
    (let ((ovs (and (boundp 'carriage-patch-fold--overlays)
                    carriage-patch-fold--overlays)))
      (should (consp ovs))
      (should (overlayp (car ovs)))
      ;; Placeholder must be present (before-string)
      (should (overlay-get (car ovs) 'before-string)))
    (carriage-patch-fold-disable (current-buffer))))
