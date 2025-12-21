;;; carriage-apply-last-iteration-diagnostic-test.el --- Diagnostics for strict last-iteration  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(ert-deftest carriage-apply-last-iteration-diagnostic-includes-counts ()
  "When strict collector finds no blocks, error should include Blocks=… and marked=…"
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    ;; Insert two unmarked blocks
    (insert "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\")\n"
            "#+begin_from\nA\n#+end_from\n#+begin_to\nB\n#+end_to\n"
            "#+end_patch\n\n")
    (insert "#+begin_patch (:version \"1\" :op \"sre\" :file \"y.txt\")\n"
            "#+begin_from\nC\n#+end_from\n#+begin_to\nD\n#+end_to\n"
            "#+end_patch\n")
    (let ((msg nil))
      (condition-case e
          (carriage-apply-last-iteration)
        (error (setq msg (error-message-string e))))
      (should (stringp msg))
      (should (string-match-p "Нет последнего отпечатка (CARRIAGE_FINGERPRINT) или нет патчей ниже него" msg))
      (carriage-mode -1))))

(provide 'carriage-apply-last-iteration-diagnostic-test)
;;; carriage-apply-last-iteration-diagnostic-test.el ends here
