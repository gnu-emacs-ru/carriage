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

(ert-deftest carriage-prompt-builder-recognizes-in-file-sections-as-current-text ()
  "Prompt must state explicitly that `In file <path>:` sections provide current file text."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should (string-match-p "In file <path>:" sys))
    (should (string-match-p "CURRENT TEXT of that file is present" sys))))

(ert-deftest carriage-prompt-builder-in-file-body-is-authoritative-current-text ()
  "Prompt must state explicitly that the body of `In file <path>:` is authoritative current text."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should (string-match-p "authoritative current file text" sys))
    (should (string-match-p "MUST be treated as visible context" sys))))

(ert-deftest carriage-prompt-builder-does-not-claim-missing-text-when-in-file-body-is-present ()
  "Prompt must explicitly forbid claiming missing file text when `In file <path>:` already carries the file body."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should (string-match-p "Do NOT say that file text is absent" sys))
    (should (string-match-p "`In file <path>:` section" sys))))

(ert-deftest carriage-prompt-builder-code-does-not-include-org-formatting-fragment ()
  "Intent=Code system prompt should not include the Org prose formatting fragment."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should-not (string-match-p "Formatting (Org-mode required):" sys))))

(ert-deftest carriage-prompt-builder-context-text-goes-to-system-with-in-file-body ()
  "System prompt must preserve explicit file-body context sections."
  (let* ((ctx-text
          (concat
           "#+begin_state_manifest\n"
           "path|exists|has_text\n"
           "lisp/carriage-mode.el|true|true\n"
           "#+end_state_manifest\n\n"
           "In file lisp/carriage-mode.el:\n"
           ";; CURRENT TEXT PRESENT IN THIS REQUEST\n"
           "#+begin_src emacs-lisp\n"
           "(defun example () t)\n"
           "#+end_src\n"))
         (ret (carriage-build-prompt 'Code 'sre
                                     (list :payload "Q"
                                           :context-text ctx-text
                                           :context-target 'system)))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should (string-match-p "In file lisp/carriage-mode\\.el:" sys))
    (should (string-match-p "CURRENT TEXT PRESENT IN THIS REQUEST" sys))
    (should (string-match-p "(defun example () t)" sys))))

(ert-deftest carriage-prompt-builder-system-contract-explicitly-trusts-in-file-bodies ()
  "System contract must explicitly say that matching In file bodies satisfy current-text visibility."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should
     (string-match-p
      "the request may provide the matching current file body under `In file <path>:`"
      sys))
    (should
     (string-match-p
      "there is no current file body for it in this request"
      sys))))

(ert-deftest carriage-prompt-builder-system-contract-forbids-ignoring-present-file-bodies ()
  "System contract must explicitly forbid claiming missing text when In file body is present."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should
     (string-match-p
      "Do NOT claim that file text is missing when this request already contains an `In file <path>:` section with the file body"
      sys))
    (should
     (string-match-p
      "When a file body is present in such an `In file <path>:` section, treat that file as having current text available"
      sys))))

(ert-deftest carriage-prompt-builder-system-contract-in-file-body-overrides-missing-text-uncertainty ()
  "System contract must explicitly say that a visible In file body makes text available."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should
     (string-match-p
      "If this request contains an `In file <path>:` section with the file body for a path, you MUST treat that file text as visible/present"
      sys))
    (should
     (string-match-p
      "even if begin_state_manifest was omitted, stale, or says has_text=false"
      sys))))

(ert-deftest carriage-prompt-builder-system-contract-in-file-body-implies-has-text-true ()
  "System contract must explicitly say that visible file bodies imply has_text=true for this request."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should
     (string-match-p
      "that file already has current text available in this request and must be treated as has_text=true"
      sys))))

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



(ert-deftest carriage-prompt-builder-context-text-explicitly-says-in-file-body-means-text-is-present ()
  "Prompt must explicitly say that visible In file sections mean text is already present in context."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should
     (string-match-p
      "Seeing an `In file <path>:` section with a body means the file text is already present in context"
      sys))
    (should
     (string-match-p
      "Do NOT ask for begin_context for a path whose full current body is already present"
      sys))))



(ert-deftest carriage-prompt-builder-context-text-explicitly-says-in-file-body-is-real-current-contents ()
  "Prompt must explicitly say that an In file body is the real current file contents, not just a hint."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should
     (string-match-p
      "An `In file <path>:` body is the actual current contents of that file in this request"
      sys))
    (should
     (string-match-p
      "you already have the required current text and must proceed from that body instead of claiming missing context"
      sys))))

(ert-deftest carriage-prompt-builder-typedblocks-guidance-on ()
  "Hybrid intent MUST include Typed Blocks guidance when typedblocks-structure-hint is enabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Hybrid 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Formatting (Org-mode required):" sys)))))

(ert-deftest carriage-prompt-builder-typedblocks-guidance-off ()
  "Hybrid intent MUST NOT include Typed Blocks guidance when typedblocks-structure-hint is disabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint nil)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Hybrid 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Formatting (Org-mode required):" sys))
      (should
       (string-match-p
        "Current text is considered present when the matching `In file <path>:` section with the file body is present in this request"
        sys)))))

(ert-deftest carriage-prompt-builder-ask-typedblocks-guidance-on ()
  "Ask intent MUST include Typed Blocks guidance only when the toggle is enabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Ask 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Do NOT produce any begin_patch blocks" sys)))))

(ert-deftest carriage-prompt-builder-ask-typedblocks-guidance-off ()
  "Ask intent MUST NOT include Typed Blocks guidance when the toggle is disabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint nil)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Ask 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Do NOT produce any begin_patch blocks" sys)))))

(ert-deftest carriage-prompt-builder-code-ignores-typedblocks-guidance ()
  "Code intent MUST ignore Typed Blocks guidance flag entirely."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Code 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys)))))

(ert-deftest carriage-prompt-builder-typedblocks-guidance-on ()
  "Hybrid intent MUST include Typed Blocks guidance when typedblocks-structure-hint is enabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Hybrid 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Formatting (Org-mode required):" sys)))))

(ert-deftest carriage-prompt-builder-typedblocks-guidance-off ()
  "Hybrid intent MUST NOT include Typed Blocks guidance when typedblocks-structure-hint is disabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint nil)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Hybrid 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Formatting (Org-mode required):" sys))
      (should
       (string-match-p
        "Current text is considered present when the matching `In file <path>:` section with the file body is present in this request"
        sys)))))

(ert-deftest carriage-prompt-builder-ask-typedblocks-guidance-on ()
  "Ask intent MUST include Typed Blocks guidance only when the toggle is enabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Ask 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Do NOT produce any begin_patch blocks" sys)))))

(ert-deftest carriage-prompt-builder-ask-typedblocks-guidance-off ()
  "Ask intent MUST NOT include Typed Blocks guidance when the toggle is disabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint nil)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Ask 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Do NOT produce any begin_patch blocks" sys)))))

(ert-deftest carriage-prompt-builder-code-ignores-typedblocks-guidance ()
  "Code intent MUST ignore Typed Blocks guidance flag entirely."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Code 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys)))))

(ert-deftest carriage-prompt-builder-typedblocks-guidance-on ()
  "Hybrid intent MUST include Typed Blocks guidance when typedblocks-structure-hint is enabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Hybrid 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Formatting (Org-mode required):" sys)))))

(ert-deftest carriage-prompt-builder-typedblocks-guidance-off ()
  "Hybrid intent MUST NOT include Typed Blocks guidance when typedblocks-structure-hint is disabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint nil)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Hybrid 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Formatting (Org-mode required):" sys))
      (should
       (string-match-p
        "Current text is considered present when the matching `In file <path>:` section with the file body is present in this request"
        sys)))))

(ert-deftest carriage-prompt-builder-ask-typedblocks-guidance-on ()
  "Ask intent MUST include Typed Blocks guidance only when the toggle is enabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Ask 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Do NOT produce any begin_patch blocks" sys)))))

(ert-deftest carriage-prompt-builder-ask-typedblocks-guidance-off ()
  "Ask intent MUST NOT include Typed Blocks guidance when the toggle is disabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint nil)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Ask 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Do NOT produce any begin_patch blocks" sys)))))

(ert-deftest carriage-prompt-builder-code-ignores-typedblocks-guidance ()
  "Code intent MUST ignore Typed Blocks guidance flag entirely."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Code 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys)))))

(ert-deftest carriage-prompt-builder-explicitly-forbids-claiming-in-file-text-is-invisible ()
  "Prompt must explicitly forbid saying that file text is unseen when In file body is present."
  (let* ((ret (carriage-build-prompt 'Code 'sre '(:payload "Q")))
         (sys (plist-get ret :system)))
    (should (stringp sys))
    (should
     (string-match-p
      "If the current request visibly contains `In file <path>:` with a real file body, you already see that file's text in context right now"
      sys))
    (should
     (string-match-p
      "Never say \"I do not see the file text\" or equivalent"
      sys))))

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

(ert-deftest carriage-prompt-builder-hybrid-typedblocks-guidance-on ()
  "Hybrid includes Typed Blocks guidance only when the toggle is enabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Hybrid 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "begin_task" sys)))))

(ert-deftest carriage-prompt-builder-hybrid-typedblocks-guidance-off ()
  "Hybrid omits Typed Blocks guidance when the toggle is disabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint nil)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Hybrid 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Formatting (Org-mode required):" sys)))))

(ert-deftest carriage-prompt-builder-ask-typedblocks-guidance-on ()
  "Ask includes Typed Blocks guidance only when the toggle is enabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Ask 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Do NOT produce any begin_patch blocks" sys)))))

(ert-deftest carriage-prompt-builder-ask-typedblocks-guidance-off ()
  "Ask omits Typed Blocks guidance when the toggle is disabled."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint nil)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Ask 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys))
      (should (string-match-p "Do NOT produce any begin_patch blocks" sys)))))

(ert-deftest carriage-prompt-builder-code-ignores-typedblocks-guidance-flag ()
  "Code intent ignores the typed-blocks toggle and never emits that guidance."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-typedblocks-structure-hint t)
    (let* ((ctx (carriage--build-context 'buffer (current-buffer)))
           (ret (carriage-build-prompt 'Code 'sre ctx))
           (sys (plist-get ret :system)))
      (should (stringp sys))
      (should-not (string-match-p "Typed Blocks (v1):" sys)))))

(provide 'carriage-prompt-builder-test)
;;; carriage-prompt-builder-test.el ends here
