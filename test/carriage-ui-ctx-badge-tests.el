;;; carriage-ui-ctx-badge-tests.el --- ERT tests for [Ctx:N] tooltip profile line  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage-ui)

(ert-deftest carriage-ui/ctx-badge-profile-line-p3 ()
  "Tooltip for [Ctx:N] must include profile P3 info (and budgets line) when profile is P3."
  (with-temp-buffer
    (org-mode)
    ;; Ensure no real context counting happens during test.
    (cl-letf (((symbol-function 'carriage-context-count)
               (lambda (&rest _)
                 (list :count 0 :items '() :warnings '() :stats '()))))
      (let* ((carriage-ui-context-async-refresh nil)
             ;; Simulate toggles values; only doc on to avoid off-case.
             (inc-doc t) (inc-gpt nil) (inc-vis nil) (inc-patched nil))
        (setq-local carriage-doc-context-profile 'p3)
        (let* ((badge (carriage-ui--compute-context-badge inc-doc inc-gpt inc-vis inc-patched))
               (help (and (consp badge) (cdr badge))))
          (should (stringp help))
          ;; Russian tooltip line includes profile and budgets; check for «Профиль: P3».
          (should (string-match-p "Профиль: P3" help)))))))

(ert-deftest carriage-ui/ctx-badge-profile-line-p1 ()
  "Tooltip for [Ctx:N] must include profile P1 info when profile is P1."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'carriage-context-count)
               (lambda (&rest _)
                 (list :count 0 :items '() :warnings '() :stats '()))))
      (let* ((carriage-ui-context-async-refresh nil)
             (inc-doc t) (inc-gpt nil) (inc-vis nil) (inc-patched nil))
        (setq-local carriage-doc-context-profile 'p1)
        (let* ((badge (carriage-ui--compute-context-badge inc-doc inc-gpt inc-vis inc-patched))
               (help (and (consp badge) (cdr badge))))
          (should (stringp help))
          (should (string-match-p "Профиль: P1" help)))))))

(provide 'carriage-ui-ctx-badge-tests)
;;; carriage-ui-ctx-badge-tests.el ends here
