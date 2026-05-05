;;; carriage-git-wip-async-test.el --- Async WIP branch helpers smoke tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(defun carriage-git-wip-async-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-git-wip-async-test--git-out (dir &rest args)
  "Run git ARGS in DIR and return trimmed stdout."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (apply #'call-process "git" nil t nil args)
      (string-trim (buffer-string)))))

(ert-deftest carriage-git-checkout-wip-async-unborn-head-creates-commit ()
  "carriage-git-checkout-wip-async should create WIP and ensure HEAD exists when repo has no commits."
  (let ((dir (make-temp-file "carriage-wip-async-unborn-" t)))
    (unwind-protect
        (progn
          (should (zerop (carriage-git-wip-async-test--git dir "init")))
          (should (zerop (carriage-git-wip-async-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-git-wip-async-test--git dir "config" "user.name" "Tester")))
          ;; Start async checkout
          (let ((done nil)
                (res nil))
            (carriage-git-checkout-wip-async
             dir nil
             (lambda (r) (setq res r done t))
             (lambda (e) (setq res e done t)))
            ;; wait until done (small spin)
            (let ((deadline (+ (float-time) 5)))
              (while (and (not done) (< (float-time) deadline))
                (sleep-for 0 50)))
            (should done)
            ;; HEAD should now be valid and branch should be carriage/WIP
            (let ((branch (carriage-git-wip-async-test--git-out dir "rev-parse" "--abbrev-ref" "HEAD")))
              (should (string= branch "carriage/WIP")))
            (let ((head (carriage-git-wip-async-test--git-out dir "rev-parse" "HEAD")))
              (should (not (string-empty-p head))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest carriage-git-create-ephemeral-branch-async-creates-branch ()
  "carriage-git-create-ephemeral-branch-async should create a new ephemeral branch and check it out."
  (let ((dir (make-temp-file "carriage-ephemeral-async-" t)))
    (unwind-protect
        (progn
          (should (zerop (carriage-git-wip-async-test--git dir "init")))
          (should (zerop (carriage-git-wip-async-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-git-wip-async-test--git dir "config" "user.name" "Tester")))
          ;; Create initial commit to allow branch create
          (with-temp-file (expand-file-name "README.md" dir) (insert "# tmp\n"))
          (should (zerop (carriage-git-wip-async-test--git dir "add" "README.md")))
          (should (zerop (carriage-git-wip-async-test--git dir "commit" "-m" "init")))
          (let ((done nil)
                (res nil))
            (carriage-git-create-ephemeral-branch-async
             dir
             (lambda (r) (setq res r done t))
             (lambda (e) (setq res e done t)))
            (let ((deadline (+ (float-time) 5)))
              (while (and (not done) (< (float-time) deadline))
                (sleep-for 0 50)))
            (should done)
            (should (string-match-p "^carriage/tmp" (or (plist-get res :branch) "")))
            (let ((branch (carriage-git-wip-async-test--git-out dir "rev-parse" "--abbrev-ref" "HEAD")))
              (should (string-match-p "^carriage/tmp" branch)))))
      (ignore-errors (delete-directory dir t)))))

;;; carriage-git-wip-async-test.el ends here
