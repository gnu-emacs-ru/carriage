;;; carriage-prompt-builder-test.el --- ERT tests for prompt builder -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-suite)
(require 'carriage-intent-registry)
(require 'carriage-mode)

(ert-deftest carriage-prompt-builder-unknown-suite-errors ()
  "Unknown suite should raise MODE_E_DISPATCH."
  (should-error
   (carriage-build-prompt 'Code 'unknown '(:payload "X"))
   :type (carriage-error-symbol 'MODE_E_DISPATCH)))

(ert-deftest carriage-prompt-builder-intent-override-wins ()
  "Intent override replaces registry fragment."
  (let* ((carriage-intent-fragment-overrides '((Hybrid . "OVERRIDE HYBRID")))
    (ret (carriage-build-prompt 'Hybrid 'sre '(:payload "Z")))
    (sys (plist-get ret :system)))
  (should (string-match-p "OVERRIDE HYBRID" sys))))

(ert-deftest carriage-prompt-builder-context-text-preserves-in-file-visibility-contract ()
  "Explicit formatted context must carry strong wording that file bodies are visible current text."
  (let* ((ctx-text
          (concat
           "In file lisp/carriage-mode.el:\n"
           ";; CURRENT TEXT PRESENT IN THIS REQUEST\n"
           ";; THIS FULL FILE BODY IS VISIBLE TO THE MODEL IN THE CURRENT REQUEST.\n"
           ";; TREAT THIS FILE BODY AS THE AUTHORITATIVE CURRENT TEXT FOR THIS PATH.\n"
           ";; THIS MEANS has_text=true FOR THIS PATH IN THE CURRENT REQUEST.\n"
           ";; THE MODEL MUST USE THIS BODY AS THE CURRENT FILE TEXT AND MUST NOT CLAIM IT IS MISSING.\n"
           "#+begin_src emacs-lisp\n"
           "(defun example () t)\n"
           "#+end_src\n"))
    (ret (carriage-build-prompt 'Code 'sre
                                (list :payload "Q"
                                      :context-text ctx-text
                                      :context-target 'system)))
    (sys (plist-get ret :system)))
  (should (stringp sys))
  (should (string-match-p "THIS FULL FILE BODY IS VISIBLE TO THE MODEL IN THE CURRENT REQUEST" sys))
  (should (string-match-p "THIS MEANS has_text=true FOR THIS PATH IN THE CURRENT REQUEST" sys))
  (should (string-match-p "MUST USE THIS BODY AS THE CURRENT FILE TEXT" sys))))

(ert-deftest carriage-prompt-builder-system-with-in-file-body-keeps-actual-file-text-visible ()
  "When system context includes an In file body, the final system prompt must retain that body verbatim."
  (let* ((ctx-text
          (concat
           "#+begin_state_manifest\n"
           "path|exists|has_text\n"
           "lisp/carriage-mode.el|true|true\n"
           "#+end_state_manifest\n\n"
           "In file lisp/carriage-mode.el:\n"
           ";; CURRENT TEXT PRESENT IN THIS REQUEST\n"
           ";; THIS FULL FILE BODY IS VISIBLE TO THE MODEL IN THE CURRENT REQUEST.\n"
           "#+begin_src emacs-lisp\n"
           "(defun visible-example () t)\n"
           "#+end_src\n"))
    (ret (carriage-build-prompt 'Code 'sre
                                (list :payload "Q"
                                      :context-text ctx-text
                                      :context-target 'system)))
    (sys (plist-get ret :system)))
  (should (stringp sys))
  (should (string-match-p "In file lisp/carriage-mode\\.el:" sys))
  (should (string-match-p "THIS FULL FILE BODY IS VISIBLE TO THE MODEL IN THE CURRENT REQUEST" sys))
  (should (string-match-p "(defun visible-example () t)" sys))))

(provide 'carriage-prompt-builder-test)
;;; carriage-prompt-builder-test.el ends here