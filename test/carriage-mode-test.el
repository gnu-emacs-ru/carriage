;;; carriage-mode-test.el --- Mode/UI presence tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(ert-deftest carriage-smoke-package-loads-and-provides-mode ()
  (require 'carriage)
  (should (featurep 'carriage))
  (should (fboundp 'carriage-mode))
  (should (boundp 'carriage-mode-map)))

(ert-deftest carriage-mode-present ()
  (should (fboundp 'carriage-mode))
  (should (boundp 'carriage-mode-map)))

(ert-deftest carriage-report-present ()
  (should (fboundp 'carriage-report-open))
  (should (fboundp 'carriage-report-render)))

(ert-deftest carriage-auto-apply-guard-allows-distinct-single-items ()
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (let ((carriage--current-send-entry-id "carriage-send-buffer-14")
          (item-a '(:op aibo :file "lisp/carriage-mode.el"))
          (item-b '(:op aibo :file "test/carriage-transport-test.el")))
      (setq carriage--apply-entry-id nil
            carriage--apply-entry-log-count 0
            carriage--apply-response-fingerprint nil
            carriage--apply-claim-keys nil)
      (should (carriage--auto-apply-guard-claim 'single-item nil item-a))
      (should-not (carriage--auto-apply-guard-claim 'single-item nil item-a))
      (should (carriage--auto-apply-guard-claim 'single-item nil item-b)))))

(ert-deftest carriage-auto-apply-guard-allows-same-item-with-different-fingerprint ()
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (let ((carriage--current-send-entry-id "carriage-send-buffer-17")
          (item '(:op aibo :file "lisp/carriage-mode.el")))
      (setq carriage--apply-entry-id nil
            carriage--apply-entry-log-count 0
            carriage--apply-response-fingerprint nil
            carriage--apply-claim-keys nil)
      (should (carriage--auto-apply-guard-claim 'single-item "fp-a" item))
      (should-not (carriage--auto-apply-guard-claim 'single-item "fp-a" item))
      (should (carriage--auto-apply-guard-claim 'single-item "fp-b" item)))))

(ert-deftest carriage-auto-apply-guard-separates-group-sources ()
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (let ((carriage--current-send-entry-id "carriage-send-buffer-18")
          (item '(:op aibo :file "lisp/carriage-mode.el")))
      (setq carriage--apply-entry-id nil
            carriage--apply-entry-log-count 0
            carriage--apply-response-fingerprint nil
            carriage--apply-claim-keys nil)
      (should (carriage--auto-apply-guard-claim 'single-item nil item))
      (should (carriage--auto-apply-guard-claim 'last-iteration nil nil))
      (should-not (carriage--auto-apply-guard-claim 'last-iteration nil nil)))))
