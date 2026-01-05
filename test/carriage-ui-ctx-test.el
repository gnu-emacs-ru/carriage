;;; carriage-ui-ctx-test.el --- Tests for carriage-ui ctx cache invalidation  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(let* ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))

(require 'carriage-ui)

;; Define toggles used by carriage-ui--context-toggle-states in case carriage-mode isn't loaded.
(defvar carriage-mode-include-doc-context t)
(defvar carriage-mode-include-gptel-context nil)
(defvar carriage-mode-include-visible-context nil)
(defvar carriage-mode-include-patched-files nil)
(defvar carriage-doc-context-scope 'all)

(make-variable-buffer-local 'carriage-mode-include-doc-context)
(make-variable-buffer-local 'carriage-mode-include-gptel-context)
(make-variable-buffer-local 'carriage-mode-include-visible-context)
(make-variable-buffer-local 'carriage-mode-include-patched-files)
(make-variable-buffer-local 'carriage-doc-context-scope)

(ert-deftest carriage-ui-ctx-cache-invalidated-when-gptel-version-changes ()
  "Ctx cache must become invalid when gptel-context version changes."
  (with-temp-buffer
    ;; Make sure these are buffer-local for the test.
    (setq-local carriage-mode-include-doc-context t)
    (setq-local carriage-mode-include-gptel-context t)
    (setq-local carriage-mode-include-visible-context nil)
    (setq-local carriage-mode-include-patched-files nil)
    (setq-local carriage-doc-context-scope 'all)

    (let* ((tick 42)
           (now  1.0))
      (setq carriage-ui--gptel-context-version 0)
      (let* ((toggles (carriage-ui--context-toggle-states))
             (cache (carriage-ui--ctx-build-cache toggles tick now (cons "Ctx:0" "tip"))))
        (should (carriage-ui--ctx-cache-valid-p cache toggles tick now nil))

        ;; Simulate external change in gptel-context; this must invalidate cache.
        (setq carriage-ui--gptel-context-version 1)
        (let ((toggles2 (carriage-ui--context-toggle-states)))
          (should-not (carriage-ui--ctx-cache-valid-p cache toggles2 tick now nil)))))))

(provide 'carriage-ui-ctx-test)
;;; carriage-ui-ctx-test.el ends here
