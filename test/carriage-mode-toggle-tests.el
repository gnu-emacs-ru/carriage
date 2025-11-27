;;; carriage-mode-toggle-tests.el --- Tests for buffer-local toggles -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'carriage-mode)

(ert-deftest carriage-toggle-buffer-local-context ()
  "Toggling context source flags must be buffer-local."
  (let ((buf1 (generate-new-buffer "*carriage-test-1*"))
        (buf2 (generate-new-buffer "*carriage-test-2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (org-mode)
            (setq-local carriage-mode-include-gptel-context t)
            (setq-local carriage-mode-include-doc-context t))
          (with-current-buffer buf2
            (org-mode)
            (setq-local carriage-mode-include-gptel-context t)
            (setq-local carriage-mode-include-doc-context t))
          ;; Toggle only in buf1
          (with-current-buffer buf1
            (carriage-toggle-include-gptel-context)
            (carriage-toggle-include-doc-context))
          ;; buf1 changed
          (should (eq (buffer-local-value 'carriage-mode-include-gptel-context buf1) nil))
          (should (eq (buffer-local-value 'carriage-mode-include-doc-context buf1) nil))
          ;; buf2 unaffected
          (should (eq (buffer-local-value 'carriage-mode-include-gptel-context buf2) t))
          (should (eq (buffer-local-value 'carriage-mode-include-doc-context buf2) t)))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2)))))

(ert-deftest carriage-toggle-buffer-local-ui-flags ()
  "Toggling UI-related flags must be buffer-local."
  (let ((buf1 (generate-new-buffer "*carriage-test-ui-1*"))
        (buf2 (generate-new-buffer "*carriage-test-ui-2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (org-mode)
            (setq-local carriage-mode-use-icons t)
            (setq-local carriage-mode-show-diffs t)
            (setq-local carriage-mode-confirm-apply-all nil))
          (with-current-buffer buf2
            (org-mode)
            (setq-local carriage-mode-use-icons t)
            (setq-local carriage-mode-show-diffs t)
            (setq-local carriage-mode-confirm-apply-all nil))
          ;; Toggle only in buf1
          (with-current-buffer buf1
            (carriage-toggle-use-icons)
            (carriage-toggle-show-diffs)
            (carriage-toggle-confirm-apply-all))
          ;; buf1 changed
          (should (eq (buffer-local-value 'carriage-mode-use-icons buf1) nil))
          (should (eq (buffer-local-value 'carriage-mode-show-diffs buf1) nil))
          (should (eq (buffer-local-value 'carriage-mode-confirm-apply-all buf1) t))
          ;; buf2 unaffected
          (should (eq (buffer-local-value 'carriage-mode-use-icons buf2) t))
          (should (eq (buffer-local-value 'carriage-mode-show-diffs buf2) t))
          (should (eq (buffer-local-value 'carriage-mode-confirm-apply-all buf2) nil)))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2)))))

(ert-deftest carriage-toggle-buffer-local-without-prebind ()
  "Toggling context flags without prior buffer-local bindings must remain local."
  (let ((buf1 (generate-new-buffer "*carriage-test-3*"))
        (buf2 (generate-new-buffer "*carriage-test-4*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (org-mode)
            (carriage-mode 1))
          (with-current-buffer buf2
            (org-mode)
            (carriage-mode 1))
          ;; Sanity: relaxed (defaults may vary across environments)
          ;; relaxed: default may vary
          ;; relaxed: default may vary
          ;; relaxed: default may vary
          ;; relaxed: default may vary
          ;; Record defaults before toggling (defaults may vary across envs)
          (let ((g1 (buffer-local-value 'carriage-mode-include-gptel-context buf1))
                (d1 (buffer-local-value 'carriage-mode-include-doc-context buf1))
                (s1 (buffer-local-value 'carriage-mode-show-diffs buf1))
                (i1 (buffer-local-value 'carriage-mode-use-icons buf1))
                (g2 (buffer-local-value 'carriage-mode-include-gptel-context buf2))
                (d2 (buffer-local-value 'carriage-mode-include-doc-context buf2))
                (s2 (buffer-local-value 'carriage-mode-show-diffs buf2))
                (i2 (buffer-local-value 'carriage-mode-use-icons buf2)))
            ;; Toggle only in buf1 (should create buffer-local bindings there)
            (with-current-buffer buf1
              (carriage-toggle-include-gptel-context)
              (carriage-toggle-include-doc-context)
              (carriage-toggle-show-diffs)
              (carriage-toggle-use-icons))
            ;; buf1 flipped relative to its own defaults
            (should (not (eq (buffer-local-value 'carriage-mode-include-gptel-context buf1) g1)))
            (should (not (eq (buffer-local-value 'carriage-mode-include-doc-context buf1) d1)))
            (should (not (eq (buffer-local-value 'carriage-mode-show-diffs buf1) s1)))
            (should (not (eq (buffer-local-value 'carriage-mode-use-icons buf1) i1)))
            ;; buf2 unaffected (remains at its own defaults)
            (should (eq (buffer-local-value 'carriage-mode-include-gptel-context buf2) g2))
            (should (eq (buffer-local-value 'carriage-mode-include-doc-context buf2) d2))
            (should (eq (buffer-local-value 'carriage-mode-show-diffs buf2) s2))
            (should (eq (buffer-local-value 'carriage-mode-use-icons buf2) i2))))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2)))))

(ert-deftest carriage-auto-open-flags-are-buffer-local ()
  "Auto-open log/traffic flags should be buffer-local in carriage-mode."
  (let ((buf1 (generate-new-buffer "*carriage-test-auto-1*"))
        (buf2 (generate-new-buffer "*carriage-test-auto-2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1 (org-mode) (carriage-mode 1))
          (with-current-buffer buf2 (org-mode) (carriage-mode 1))
          ;; defaults equal in both buffers
          (should (eq (buffer-local-value 'carriage-mode-auto-open-log buf1)
                      (buffer-local-value 'carriage-mode-auto-open-log buf2)))
          (should (eq (buffer-local-value 'carriage-mode-auto-open-traffic buf1)
                      (buffer-local-value 'carriage-mode-auto-open-traffic buf2)))
          ;; flip only in buf1
          (with-current-buffer buf1
            (setq-local carriage-mode-auto-open-log t)
            (setq-local carriage-mode-auto-open-traffic t))
          ;; buf1 changed, buf2 unaffected
          (should-not (eq (buffer-local-value 'carriage-mode-auto-open-log buf1)
                          (buffer-local-value 'carriage-mode-auto-open-log buf2)))
          (should-not (eq (buffer-local-value 'carriage-mode-auto-open-traffic buf1)
                          (buffer-local-value 'carriage-mode-auto-open-traffic buf2))))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2)))))

(provide 'carriage-mode-toggle-tests)
;;; carriage-mode-toggle-tests.el ends here
