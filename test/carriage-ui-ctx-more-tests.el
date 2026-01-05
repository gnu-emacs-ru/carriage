;;; carriage-ui-ctx-more-tests.el --- Extra ctx tests -*- lexical-binding: t; -*-

;; Additional ERT tests for context badge logic: toggles, gptel-version invalidation,
;; and visible-buffers inclusion. Designed to be lightweight and robust.

(require 'ert)
(require 'carriage-context)
(require 'carriage-ui)

(ert-deftest carriage-ui-ctx-inc-gpt-off-when-var-nil ()
  "When `carriage-mode-include-gptel-context' is explicitly nil, inc-gpt is nil."
  (with-temp-buffer
    (setq-local carriage-mode-include-gptel-context nil)
    (setq-local carriage-mode-include-doc-context t)
    (let ((tog (carriage-context--count-root+toggles (current-buffer))))
      (should (null (plist-get tog :inc-gpt)))
      (should (eq (plist-get tog :inc-doc) t)))))

(ert-deftest carriage-ui-ctx-cache-invalidated-by-gptel-version ()
  "Cached Ctx badge must be considered invalid when the global gptel-context version changes."
  (with-temp-buffer
    (let* ((buf (current-buffer))
           ;; ensure toggles exist and are on for the test
           (toggle (progn
                     (setq-local carriage-mode-include-doc-context t)
                     (setq-local carriage-mode-include-gptel-context t)
                     (carriage-ui--context-toggle-states)))
           (tick 100)
           (now  10.0)
           (cache (carriage-ui--ctx-build-cache toggle tick now (cons "Ctx:0" "tip"))))
      ;; cache valid for current gptel version (default 0)
      (should (carriage-ui--ctx-cache-valid-p cache toggle tick now nil))
      ;; bump global gptel-context version and ensure cache becomes invalid
      (let ((orig (or carriage-ui--gptel-context-version 0)))
        (setq carriage-ui--gptel-context-version (1+ orig))
        (let ((tog2 (carriage-ui--context-toggle-states)))
          (should-not (carriage-ui--ctx-cache-valid-p cache tog2 tick now nil)))
        (setq carriage-ui--gptel-context-version orig)))))

(ert-deftest carriage-context-count-includes-visible-buffers ()
  "When visible-context is enabled, visible buffers contribute to the count.
This test makes a simple visible buffer and ensures the counter accounts for it."
  (let ((orig-win (selected-window)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (let ((car-buf (current-buffer)))
              (with-current-buffer car-buf
                (setq-local carriage-mode-include-visible-context t)
                ;; create a visible buffer in another window
                (let ((b (get-buffer-create "*ctx-vis-test*")))
                  (with-current-buffer b (insert "hello-visible"))
                  ;; display the buffer so it becomes visible for collection
                  (let ((w (display-buffer b)))
                    ;; run count in car-buf
                    (with-current-buffer car-buf
                      (let ((res (carriage-context-count (current-buffer) nil)))
                        (should (and (listp res) (numberp (plist-get res :count))))
                        ;; ensure at least one item when visible-context is on
                        (should (>= (plist-get res :count) 1)))
                      t)
                    ;; cleanup display
                    (when (window-live-p w) (when (not (eq w orig-win)) (delete-window w)))))))))
      (when (window-live-p orig-win) (select-window orig-win)))))

(provide 'carriage-ui-ctx-more-tests)
;;; carriage-ui-ctx-more-tests.el ends here
