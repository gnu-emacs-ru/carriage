;;; typedblocks-prompt-fragment-tests.el --- Tests for Typed Blocks v1 prompt fragment -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage-typedblocks)

(ert-deftest typedblocks/prompt-fragment-present-and-strict ()
  "The canonical Typed Blocks v1 fragment exists and encodes strict rules."
  (should (fboundp 'carriage-typedblocks-prompt-fragment-v1))
  (let* ((s (carriage-typedblocks-prompt-fragment-v1)))
    (should (stringp s))
    (should (string-match-p "#\\+begin_" s))     ;; mentions correct marker
    (should (string-match-p "Forbidden" s))      ;; contains prohibitions
    ;; must discourage asterisk headlines (textual check, not literal pattern-only)
    (should (or (string-match-p "asterisk" s)
                (string-match-p "\\*begin_" s)))))

(provide 'typedblocks-prompt-fragment-tests)
;;; typedblocks-prompt-fragment-tests.el ends here
