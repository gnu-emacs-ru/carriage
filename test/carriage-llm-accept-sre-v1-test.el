;;; carriage-llm-accept-sre-v1-test.el --- Accept SRE v1 (begin_from/to) tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(ert-deftest carriage-accept-llm-response-sre-v1-ok ()
  "Accept begin_from/begin_to SRE block and parse it as a plan item."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (setq-local carriage-mode-auto-open-report t)
    (let* ((blk
            (concat
             "#+begin_patch (:version \"1\" :op \"sre\" :file \"x.txt\")\n"
             "#+begin_from\nfoo\n#+end_from\n"
             "#+begin_to\nbar\n#+end_to\n"
             "#+end_patch\n"))
           (before (buffer-string)))
      (should (stringp before))
      (carriage-accept-llm-response blk)
      ;; Ensure last-iteration blocks are present and parsable
      (goto-char (point-min))
      (let* ((root default-directory)
             (plan (carriage-collect-last-iteration-blocks root)))
        (should (listp plan))
        (should (= (length plan) 1))
        (let* ((it (car plan)))
          (should (eq (alist-get :op it) 'sre))
          (should (string= (alist-get :file it) "x.txt"))
          (should (listp (alist-get :pairs it)))
          (should (= (length (alist-get :pairs it)) 1)))))))
(provide 'carriage-llm-accept-sre-v1-test)
;;; carriage-llm-accept-sre-v1-test.el ends here
