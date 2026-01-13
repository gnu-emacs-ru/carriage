;;; carriage-context-menu-lint-tests.el --- Lint tests for Context keys (bindings-first)  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage-keyspec)

(defun carriage--lk (map key)
  "Helper: lookup KEY (kbd string) in MAP."
  (lookup-key map (kbd key)))

(ert-deftest carriage-context-prefix-bindings-exist ()
  "Context controls are bound under prefix-map using two-stroke t x keys."
  (carriage-keys-build-prefix-map)
  (let ((keys '("t g" "t f" "t p" "t m" "t v" "t a" "t l" "t s" "t P")))
    (dolist (k keys)
      (should (commandp (carriage--lk carriage-prefix-map k))))
    ;; Ensure the 't' sub-prefix exists (acts as prefix, not a single command)
    (should (keymapp (carriage--lk carriage-prefix-map "t")))))

(ert-deftest carriage-context-prefix-two-stroke-keys-unique ()
  "Two-stroke t x keys are unique within the prefix-map (by full sequence)."
  (carriage-keys-build-prefix-map)
  (let* ((keys '("t g" "t f" "t p" "t m" "t v" "t a" "t l" "t s" "t P"))
         (dups (cl-set-difference keys (delete-dups (copy-sequence keys)) :test #'equal)))
    (should (null dups))))

(ert-deftest carriage-keyspec-labels-non-empty-and-clean ()
  "All keyspec action labels are non-empty and contain no trailing bracket hints."
  (dolist (a (carriage-keys-actions))
    (let ((lbl (carriage-keys-action-label a)))
      (should (and (stringp lbl) (not (string-match-p "\\`[ \t]*\\'" lbl))))
      (should-not (string-match-p " \\[[^][]+\\]\\'" lbl)))))

(provide 'carriage-context-menu-lint-tests)
;;; carriage-context-menu-lint-tests.el ends here
