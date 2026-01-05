;;; carriage-ui-ctx-additional-tests.el --- Additional tests for Ctx/modeline behavior -*- lexical-binding: t; -*-

;; Extra ERT tests that verify toggle handling and gptel-version invalidation
;; for the context badge logic.

(require 'ert)
(require 'carriage-context)
(require 'carriage-ui)

(ert-deftest carriage-context-count-respects-gptel-toggle ()
  "When `carriage-mode-include-gptel-context' is buffer-local and nil, the toggle snapshot reports gptel OFF."
  (with-temp-buffer
    ;; make sure buffer-local var exists and is explicitly nil
    (setq-local carriage-mode-include-gptel-context nil)
    (setq-local carriage-mode-include-doc-context t)
    (let ((tog (carriage-context--count-root+toggles (current-buffer))))
      (should (eq (plist-get tog :inc-gpt) nil))
      (should (eq (plist-get tog :inc-doc) t)))))

(ert-deftest carriage-ui-ctx-cache-invalidates-on-gptel-version-bump ()
  "Cached Ctx badge must be considered invalid when the global gptel-context version changes."
  (with-temp-buffer
    (let* ((buf (current-buffer))
           (tick 100)
           (now  10.0)
           ;; ensure a known snapshot of toggles
           (toggles (progn
                      (setq-local carriage-mode-include-doc-context t)
                      (setq-local carriage-mode-include-gptel-context t)
                      (carriage-ui--context-toggle-states)))
           (cache (carriage-ui--ctx-build-cache toggles tick now (cons "Ctx:0" "tip"))))
      ;; cache valid for current gptel version (default 0)
      (should (carriage-ui--ctx-cache-valid-p cache toggles tick now nil))
      ;; bump global gptel-context version and ensure cache becomes invalid
      (setq carriage-ui--gptel-context-version (1+ (or carriage-ui--gptel-context-version 0)))
      (let ((tog2 (carriage-ui--context-toggle-states)))
        (should-not (carriage-ui--ctx-cache-valid-p cache tog2 tick now nil))))))


(ert-deftest carriage-ui-context-toggle-visible-reflects-buffer-local ()
  "Ensure `carriage-ui--context-toggle-states' reflects buffer-local visible toggle."
  (with-temp-buffer
    (setq-local carriage-mode-include-visible-context t)
    (let ((tog (carriage-ui--context-toggle-states)))
      (should (eq (plist-get tog :vis) t)))))

(provide 'carriage-ui-ctx-additional-tests)
;;; carriage-ui-ctx-additional-tests.el ends here
