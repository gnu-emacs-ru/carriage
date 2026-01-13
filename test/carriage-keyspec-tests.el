;;; carriage-keyspec-tests.el --- Tests for keyspec v3 (bindings-first)  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage-keyspec)
(require 'carriage-mode)

(defun carriage--lk (map key)
  "Helper: lookup KEY (kbd string) in MAP."
  (lookup-key map (kbd key)))

(ert-deftest carriage-keyspec-prefix-map-present ()
  "Prefix map exists and is populated."
  (should (boundp 'carriage-prefix-map))
  (should (keymapp carriage-prefix-map))
  ;; Menu/help inside prefix
  (should (eq (carriage--lk carriage-prefix-map "SPC") #'carriage-menu-open))
  (should (eq (carriage--lk carriage-prefix-map "?")   #'carriage-menu-help)))

(ert-deftest carriage-keyspec-context-keys-in-prefix-map ()
  "Context controls are real bindings in prefix map (two-stroke t x)."
  (let ((expect
         '(("t g" . carriage-toggle-include-gptel-context)
           ("t f" . carriage-toggle-include-doc-context)
           ("t p" . carriage-toggle-include-patched-files)
           ("t m" . carriage-toggle-include-project-map)
           ("t v" . carriage-toggle-include-visible-context)
           ("t a" . carriage-select-doc-context-all)
           ("t l" . carriage-select-doc-context-last)
           ("t s" . carriage-toggle-doc-context-scope)
           ("t P" . carriage-toggle-context-profile))))
    (dolist (pr expect)
      (should (eq (carriage--lk carriage-prefix-map (car pr))
                  (cdr pr))))
    ;; 't is a prefix in the map (not a command)
    (should (keymapp (carriage--lk carriage-prefix-map "t")))))

(ert-deftest carriage-keyspec-installs-into-carriage-mode-map ()
  "Keyspec installs prefix and direct bindings into carriage-mode-map."
  (carriage-keys-install-known-keymaps)
  ;; Prefix binding(s)
  (dolist (pref (carriage-keys-prefixes))
    (should (eq (lookup-key carriage-mode-map (kbd pref)) carriage-prefix-map)))
  ;; Direct bindings (must not depend on menu provider)
  (should (eq (lookup-key carriage-mode-map (kbd "C-c C-c")) #'carriage-ctrl-c-ctrl-c))
  (should (eq (lookup-key carriage-mode-map (kbd "C-c !"))   #'carriage-apply-last-iteration)))

(ert-deftest carriage-keyspec-action-labels-non-empty ()
  "All action labels are non-empty and do not include trailing bracket hints."
  (dolist (a (carriage-keys-actions))
    (let ((lbl (carriage-keys-action-label a)))
      (should (and (stringp lbl) (not (string-match-p "\\`[ \t]*\\'" lbl))))
      (should-not (string-match-p " \\[[^][]+\\]\\'" lbl)))))

(provide 'carriage-keyspec-tests)
;;; carriage-keyspec-tests.el ends here
