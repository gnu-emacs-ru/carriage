;;; carriage-prompt-builder-test.el --- ERT tests for prompt builder -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-suite)
(require 'carriage-intent-registry)

(ert-deftest carriage-prompt-builder-unknown-suite-errors ()
  "Unknown suite should raise MODE_E_DISPATCH."
  (should-error
   (carriage-build-prompt 'Code 'unknown '(:payload "X"))
   :type (carriage-error-symbol 'MODE_E_DISPATCH)))

(ert-deftest carriage-prompt-builder-sre-excludes-udiff-markers ()
  "Suite=sre must not include unified diff markers."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Do SRE")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should-not (string-match-p "^---\\s-+a/" sys))
    (should-not (string-match-p "^\\+\\+\\+\\s-+b/" sys))
    (should-not (string-match-p "^diff --git" sys))
    (should-not (string-match-p "unified diff" sys))))

(ert-deftest carriage-prompt-builder-udiff-excludes-sre-markers ()
  "Suite=udiff must not include SRE markers."
  (let* ((ret (carriage-build-prompt 'Code 'udiff '(:payload "Do patch")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should-not (string-match-p "#\\+begin_from" sys))
    (should-not (string-match-p "#\\+begin_to" sys))))

(ert-deftest carriage-prompt-builder-intent-override-wins ()
  "Intent override replaces registry fragment."
  (let* ((carriage-intent-fragment-overrides '((Hybrid . "OVERRIDE HYBRID")))
         (ret (carriage-build-prompt 'Hybrid 'sre '(:payload "Z")))
         (sys (plist-get ret :system)))
    (should (string-match-p "OVERRIDE HYBRID" sys))))

(ert-deftest carriage-prompt-builder-op-override-wins ()
  "Op override replaces op module fragment."
  (let* ((carriage-op-fragment-overrides '((patch . "OVERRIDE PATCH FRAGMENT")))
         (ret (carriage-build-prompt 'Code 'udiff '(:payload "P")))
         (sys (plist-get ret :system)))
    (should (string-match-p "OVERRIDE PATCH FRAGMENT" sys))))

(ert-deftest carriage-prompt-builder-unknown-intent-errors ()
  "Unknown intent should raise MODE_E_DISPATCH."
  (should-error
   (carriage-build-prompt 'Unknown 'sre '(:payload "X"))
   :type (carriage-error-symbol 'MODE_E_DISPATCH)))

(ert-deftest carriage-prompt-builder-context-injection-system ()
  "When :context-target is 'system, context goes to :system, not :prompt."
  (let* ((ret (carriage-build-prompt 'Hybrid 'sre '(:payload "P"
                                                             :context-text "CTX"
                                                             :context-target system)))
         (sys (plist-get ret :system))
         (pr  (plist-get ret :prompt)))
    (should (stringp sys))
    (should (string-match-p "CTX" sys))
    (should (string= pr "P"))))

(ert-deftest carriage-prompt-builder-context-injection-user ()
  "When :context-target is 'user, context goes to :prompt prefix, not :system."
  (let* ((ret (carriage-build-prompt 'Hybrid 'sre '(:payload "P"
                                                             :context-text "CTX"
                                                             :context-target user)))
         (sys (plist-get ret :system))
         (pr  (plist-get ret :prompt)))
    (should (stringp sys))
    (should (string-prefix-p "CTX\n" pr))
    (should (string-match-p "P\\'" pr))))

(ert-deftest carriage-prompt-builder-includes-aibo-fragment ()
  "Suite=sre system should include AIBO fragment when op module is available."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "X")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should (string-match-p "AIBO (literal-only" sys))))

(ert-deftest carriage-prompt-builder-ask-includes-org-formatting ()
  "Intent=Ask system prompt must enforce Org-only formatting (never Markdown)."
  (let* ((ret (carriage-build-prompt 'Ask 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should (string-match-p "Formatting (Org-mode required):" sys))
    (should (string-match-p "not Markdown" sys))
    (should (string-match-p "never '#'" sys))
    (should (string-match-p "never triple backticks" sys))))

(ert-deftest carriage-prompt-builder-hybrid-includes-org-formatting ()
  "Intent=Hybrid system prompt must enforce Org-only prose formatting (never Markdown)."
  (let* ((ret (carriage-build-prompt 'Hybrid 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should (string-match-p "Formatting (Org-mode required):" sys))
    (should (string-match-p "not Markdown" sys))
    (should (string-match-p "never '#'" sys))
    (should (string-match-p "never triple backticks" sys))
    (should (string-match-p "Default behavior: reply with Org prose only\\." sys))))

(ert-deftest carriage-prompt-builder-code-does-not-include-org-formatting-fragment ()
  "Intent=Code system prompt should not include the Org prose formatting fragment."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should-not (string-match-p "Formatting (Org-mode required):" sys))))

(provide 'carriage-prompt-builder-test)
;;; carriage-prompt-builder-test.el ends here
