;;; carriage-file-ops-prompt-test.el --- Prompt policy tests for file ops -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-op-file)

(ert-deftest carriage-create-prompt-fragment-forbids-duplicate-create ()
  "Create prompt fragment should explicitly forbid duplicate create for existing files."
  (let ((s (carriage-op-create-prompt-fragment nil)))
    (should (string-match-p "use :op create ONLY when the file does not already exist" s))
    (should (string-match-p "begin_context, begin_map, or applied patch history" s))
    (should (string-match-p "For an existing file, use patch/sre/aibo" s))))

(provide 'carriage-file-ops-prompt-test)
;;; carriage-file-ops-prompt-test.el ends here
