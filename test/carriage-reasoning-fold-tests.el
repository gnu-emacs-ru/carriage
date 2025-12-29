;;; carriage-reasoning-fold-tests.el --- Tests for reasoning folding  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'carriage-reasoning-fold)
(require 'carriage-mode)

(defun carriage-test--count-reasoning-fold-overlays ()
  (let ((n 0))
    (dolist (ov (overlays-in (point-min) (point-max)) n)
      (when (eq (overlay-get ov 'category) 'carriage-reasoning-fold)
        (setq n (1+ n))))))

(ert-deftest carriage-reasoning-fold--folds-all-on-enable ()
  "Enabling carriage-mode folds all begin_reasoning blocks in the buffer."
  (with-temp-buffer
    (org-mode)
    (insert "* T\n"
            "#+begin_reasoning\nA\n#+end_reasoning\n\n"
            "Body\n\n"
            "#+begin_reasoning\nB\n#+end_reasoning\n")
    (goto-char (point-min))
    (carriage-mode 1)
    (should (= 2 (carriage-test--count-reasoning-fold-overlays)))
    (let ((ovs (cl-remove-if-not
                (lambda (ov) (eq (overlay-get ov 'category) 'carriage-reasoning-fold))
                (overlays-in (point-min) (point-max)))))
      (should (cl-every (lambda (ov) (overlay-get ov 'invisible)) ovs)))))

(ert-deftest carriage-reasoning-fold--open-block-is-folded-and_updates_on_end ()
  "A reasoning block without end marker is folded, and after end marker appears overlay stays folded."
  (with-temp-buffer
    (org-mode)
    (insert "* T\n#+begin_reasoning\nA\n")
    (goto-char (point-min))
    (carriage-reasoning-fold-enable)
    (carriage-reasoning-fold-refresh-now)
    (should (= 1 (carriage-test--count-reasoning-fold-overlays)))
    (let* ((ovs (cl-remove-if-not
                 (lambda (ov) (eq (overlay-get ov 'category) 'carriage-reasoning-fold))
                 (overlays-in (point-min) (point-max))))
           (ov (car ovs)))
      (should (overlayp ov))
      (should (overlay-get ov 'invisible))
      ;; Now close the block and refresh; it should remain folded.
      (goto-char (point-max))
      (insert "#+end_reasoning\n")
      (carriage-reasoning-fold-refresh-now)
      (should (= 1 (carriage-test--count-reasoning-fold-overlays)))
      (let* ((ovs2 (cl-remove-if-not
                    (lambda (ov2) (eq (overlay-get ov2 'category) 'carriage-reasoning-fold))
                    (overlays-in (point-min) (point-max))))
             (ov2 (car ovs2)))
        (should (overlayp ov2))
        (should (overlay-get ov2 'invisible))))))

(provide 'carriage-reasoning-fold-tests)
;;; carriage-reasoning-fold-tests.el ends here
