;;; carriage-git-branch-policy-test.el --- Branch policy: dry-run does not switch branches -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage)
(require 'carriage-apply)

(defun carriage-bp--call (dir &rest args)
  "Run git ARGS in DIR and return plist (:exit :stdout :stderr)."
  (let* ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let* ((out (current-buffer))
             (stderr-file (make-temp-file "carriage-bp-stderr-"))
             (code (unwind-protect
                       (apply #'call-process "git" nil (list out stderr-file) nil args)
                     (ignore-errors (set-buffer out)))))
        (prog1
            (list :exit code
                  :stdout (buffer-substring-no-properties (point-min) (point-max))
                  :stderr (prog1
                              (with-temp-buffer
                                (insert-file-contents stderr-file)
                                (buffer-substring-no-properties (point-min) (point-max)))
                            (ignore-errors (delete-file stderr-file))))
          (erase-buffer))))))

(defun carriage-bp--with-temp-repo (fn)
  "Create a temporary git repo and call FN with its root."
  (let ((root (make-temp-file "carriage-bp-repo-" t)))
    (carriage-bp--call root "init")
    (carriage-bp--call root "config" "user.email" "ci@example.invalid")
    (carriage-bp--call root "config" "user.name" "CI")
    (let ((f (expand-file-name "foo.txt" root)))
      (with-temp-file f (insert "hello\n")))
    (carriage-bp--call root "add" "-A")
    (carriage-bp--call root "commit" "-m" "init" "--no-gpg-sign" "--no-verify")
    (funcall fn root)))

(defun carriage-bp--current-branch (root)
  (string-trim (plist-get (carriage-bp--call root "rev-parse" "--abbrev-ref" "HEAD") :stdout)))

(defun carriage-bp--make-diff (old new)
  (format (concat
           "diff --git a/foo.txt b/foo.txt\n"
           "index 0000000..0000001 100644\n"
           "--- a/foo.txt\n"
           "+++ b/foo.txt\n"
           "@@ -1,1 +1,1 @@\n"
           "-%s\n"
           "+%s\n")
          (string-trim-right old) (string-trim-right new)))

(ert-deftest carriage-git-dry-run-does-not-switch-branches ()
  "Dry-run for patch must not switch or create branches under any branch policy."
  (carriage-bp--with-temp-repo
   (lambda (root)
     (let* ((before (carriage-bp--current-branch root))
            (plan-item (list (cons :version "1")
                             (cons :op 'patch)
                             (cons :apply 'git-apply)
                             (cons :strip 1)
                             (cons :path "foo.txt")
                             (cons :diff (carriage-bp--make-diff "hello" "hello world")))))
       ;; Force engine to 'git for this test
       (let ((carriage-apply-stage-policy 'none))
         (let* ((rep (carriage-dry-run-plan (list plan-item) root))
                (it (car (plist-get rep :items))))
           (should (eq (plist-get it :status) 'ok))))
       (let ((after (carriage-bp--current-branch root)))
         (should (string= before after)))))))

(ert-deftest carriage-git-dry-run-without-manifest-uses-trusted-project-state ()
  "Internal dry-run without explicit manifest should use trusted project state."
  (carriage-bp--with-temp-repo
   (lambda (root)
     (let* ((plan-item (list (cons :version "1")
                             (cons :op 'patch)
                             (cons :apply 'git-apply)
                             (cons :strip 1)
                             (cons :path "foo.txt")
                             (cons :diff (carriage-bp--make-diff "hello" "hello world"))))
            (rep (carriage-dry-run-plan (list plan-item) root))
            (it (car (plist-get rep :items))))
       (should (eq (plist-get it :status) 'ok))))))

(ert-deftest carriage-git-apply-without-manifest-uses-trusted-project-state ()
  "Internal apply without explicit manifest should use trusted project state."
  (carriage-bp--with-temp-repo
   (lambda (root)
     (let* ((plan-item (list (cons :version "1")
                             (cons :op 'patch)
                             (cons :apply 'git-apply)
                             (cons :strip 1)
                             (cons :path "foo.txt")
                             (cons :diff (carriage-bp--make-diff "hello" "hello world"))))
            (rep (carriage-apply-plan (list plan-item) root))
            (it (car (plist-get rep :items))))
       (should (eq (plist-get it :status) 'ok))
       (should (string= "hello world\n"
                        (with-temp-buffer
                          (insert-file-contents (expand-file-name "foo.txt" root))
                          (buffer-string))))))))

(ert-deftest carriage-git-dry-run-explicit-empty-manifest-fails-closed ()
  "Explicit empty state manifest must disable trusted filesystem fallback for patch."
  (carriage-bp--with-temp-repo
   (lambda (root)
     (with-temp-buffer
       (insert "#+begin_state_manifest\n")
       (insert "path|exists|has_text\n")
       (insert "#+end_state_manifest\n")
       (let* ((plan-item (list (cons :version "1")
                               (cons :op 'patch)
                               (cons :apply 'git-apply)
                               (cons :strip 1)
                               (cons :path "foo.txt")
                               (cons :diff (carriage-bp--make-diff "hello" "hello world"))))
              (rep (carriage-dry-run-plan (list plan-item) root))
              (it (car (plist-get rep :items))))
         (should (eq (plist-get it :status) 'fail))
         (should (string-match-p "State-sensitive op rejected: path missing from current request state"
                                 (or (plist-get it :details) ""))))))))

(provide 'carriage-git-branch-policy-test)
;;; carriage-git-branch-policy-test.el ends here
