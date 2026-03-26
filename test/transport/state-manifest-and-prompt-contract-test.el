;;; state-manifest-and-prompt-contract-test.el --- Tests for state manifest and prompt contract -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-context)
(require 'carriage-intent-registry)

(ert-deftest carriage-context-state-manifest-format-basic ()
  (let* ((ctx (list :files
                    (list
                     (list :rel "a.txt" :true "/tmp/a.txt" :content "A")
                     (list :rel "b.txt" :true "/tmp/b.txt" :content nil :reason 'size-limit))))
         (s (carriage-context--state-manifest-format ctx)))
    (should (string-match-p "#\\+begin_state_manifest" s))
    (should (string-match-p "path|exists|has_text" s))
    (should (string-match-p "a.txt|true|true" s))
    (should (string-match-p "b.txt|true|false" s))
    (should (string-match-p "#\\+end_state_manifest" s))))

(ert-deftest carriage-intent-code-contract-mentions-begin-context-and-state-manifest ()
  (let ((s (carriage--intent-frag-code nil)))
    (should (string-match-p "begin_context" s))
    (should (string-match-p "begin_state_manifest" s))
    (should (string-match-p "has_text=true" s))
    (should (string-match-p ":op create is forbidden" s))
    (should (string-match-p "NEVER use :from/:to header keys for SRE/AIBO" s))))

(provide 'state-manifest-and-prompt-contract-test)
