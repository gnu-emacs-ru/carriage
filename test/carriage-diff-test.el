;;; carriage-diff-test.el --- Unified diff tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-op-patch)

(defun carriage-diff-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-diff-test--write (dir rel content)
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(defun carriage-diff-test--read (dir rel)
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel dir))
    (buffer-string)))

(ert-deftest carriage-diff-dry-run-and-apply ()
  "Dry-run git apply --check should pass; then apply the patch and verify content."
  (let* ((dir (make-temp-file "carriage-diff-" t)))
    ;; init repo
    (should (zerop (carriage-diff-test--git dir "init")))
    (should (zerop (carriage-diff-test--git dir "config" "user.email" "tester@example.com")))
    (should (zerop (carriage-diff-test--git dir "config" "user.name" "Tester")))
    ;; initial file and commit
    (carriage-diff-test--write dir "a.txt" "old\n")
    (should (zerop (carriage-diff-test--git dir "add" "--" "a.txt")))
    (should (zerop (carriage-diff-test--git dir "commit" "-m" "init")))
    ;; make a simple unified diff (old -> new)
    (let* ((diff (concat
                  (mapconcat #'identity
                             '("diff --git a/a.txt b/a.txt"
                               "index 0000000..0000001 100644"
                               "--- a/a.txt"
                               "+++ b/a.txt"
                               "@@ -1,1 +1,1 @@"
                               "-old"
                               "+new")
                             "\n")
                  "\n"))
           (item `(:version "1" :op patch :apply git-apply :strip 1
                            :path "a.txt" :diff ,diff)))
      ;; dry-run should be ok (plan-level)
      (let* ((rep (carriage-dry-run-plan (list item) dir))
             (row (car (plist-get rep :items))))
        (when (eq (plist-get row :status) 'fail)
          ;; Helpful diagnostics when git --check fails in CI
          (message "carriage-diff-test: dry-run failed, details=%s"
                   (plist-get row :details)))
        (should (eq (plist-get row :status) 'ok))
        (should (numberp (plist-get row :pid)))
        (should (numberp (plist-get row :elapsed-ms)))
        (should (eq (plist-get row :engine) 'git)))
      ;; apply should update file content (plan-level)
      (let* ((ap (carriage-apply-plan (list item) dir))
             (row (car (plist-get ap :items))))
        (should (eq (plist-get row :status) 'ok))
        (should (numberp (plist-get row :pid)))
        (should (numberp (plist-get row :elapsed-ms)))
        (should (eq (plist-get row :engine) 'git))
        (should (string= (carriage-diff-test--read dir "a.txt") "new\n"))))))

(ert-deftest carriage-diff-missing-headers-signals-diff-syntax ()
  "Parsing a body without ---/+++ headers must signal PATCH_E_DIFF_SYNTAX."
  (let* ((hdr '(:version "1" :op "patch"))
         (body "this is not a unified diff")
         (sym  (carriage-error-symbol 'PATCH_E_DIFF_SYNTAX)))
    (should-error
     (carriage-parse-diff hdr body default-directory)
     :type sym)))


(ert-deftest carriage-patch-apply-header-ignored ()
  "Header :apply is informational and must be ignored; engine must remain default ('git)."
  (let* ((dir (make-temp-file "carriage-diff-" t)))
    ;; init repo
    (should (zerop (carriage-diff-test--git dir "init")))
    (should (zerop (carriage-diff-test--git dir "config" "user.email" "tester@example.com")))
    (should (zerop (carriage-diff-test--git dir "config" "user.name" "Tester")))
    ;; initial file and commit
    (carriage-diff-test--write dir "a.txt" "A\n")
    (should (zerop (carriage-diff-test--git dir "add" "--" "a.txt")))
    (should (zerop (carriage-diff-test--git dir "commit" "-m" "init")))
    ;; build unified diff
    (let* ((diff (concat
                  (mapconcat #'identity
                             '("diff --git a/a.txt b/a.txt"
                               "index 0000000..0000001 100644"
                               "--- a/a.txt"
                               "+++ b/a.txt"
                               "@@ -1,1 +1,1 @@"
                               "-A"
                               "+B")
                             "\n")
                  "\n"))
           ;; deliberately set a non-default :apply to verify it's ignored
           (item `(:version "1" :op patch :apply echo :strip 1
                            :path "a.txt" :diff ,diff)))
      ;; dry-run must use default engine ('git), regardless of header :apply
      (let* ((rep (carriage-dry-run-plan (list item) dir))
             (row (car (plist-get rep :items))))
        (should (eq (plist-get row :status) 'ok))
        (should (eq (plist-get row :engine) 'git))
        (should (numberp (plist-get row :pid)))
        (should (numberp (plist-get row :elapsed-ms)))))))

(ert-deftest carriage-diff-invalid-strip-signals-error ()
  "If header :strip is present and invalid, signal PATCH_E_STRIP."
  (let* ((body (concat
                (mapconcat #'identity
                           '("diff --git a/a.txt b/a.txt"
                             "--- a/a.txt"
                             "+++ b/a.txt"
                             "@@ -1,1 +1,1 @@"
                             "-x"
                             "+y")
                           "\n")
                "\n"))
         (sym  (carriage-error-symbol 'PATCH_E_STRIP)))
    ;; Non-integer value
    (let ((hdr '(:version "1" :op "patch" :strip "oops")))
      (should-error
       (carriage-parse-diff hdr body default-directory)
       :type sym))
    ;; Negative integer
    (let ((hdr '(:version "1" :op "patch" :strip -1)))
      (should-error
       (carriage-parse-diff hdr body default-directory)
       :type sym))))

(ert-deftest carriage-diff-dry-run-explicit-manifest-without-text-fails-closed ()
  "Explicit request-state manifest with has_text=false must reject patch dry-run."
  (let* ((dir (make-temp-file "carriage-diff-" t)))
    (should (zerop (carriage-diff-test--git dir "init")))
    (should (zerop (carriage-diff-test--git dir "config" "user.email" "tester@example.com")))
    (should (zerop (carriage-diff-test--git dir "config" "user.name" "Tester")))
    (carriage-diff-test--write dir "a.txt" "old\n")
    (should (zerop (carriage-diff-test--git dir "add" "--" "a.txt")))
    (should (zerop (carriage-diff-test--git dir "commit" "-m" "init")))
    (let* ((diff (concat
                  (mapconcat #'identity
                             '("diff --git a/a.txt b/a.txt"
                               "index 0000000..0000001 100644"
                               "--- a/a.txt"
                               "+++ b/a.txt"
                               "@@ -1,1 +1,1 @@"
                               "-old"
                               "+new")
                             "\n")
                  "\n"))
           (item `(:version "1" :op patch :apply git-apply :strip 1
                            :path "a.txt" :diff ,diff)))
      (with-temp-buffer
        (insert "#+begin_state_manifest\n")
        (insert "path|exists|has_text\n")
        (insert "a.txt|true|false\n")
        (insert "#+end_state_manifest\n")
        (let* ((rep (carriage-dry-run-plan (list item) dir))
               (row (car (plist-get rep :items))))
          (should (eq (plist-get row :status) 'fail))
          (should (string-match-p "file text is not present in current request context"
                                  (or (plist-get row :details) ""))))))))

(ert-deftest carriage-create-explicit-manifest-existing-path-fails-closed ()
  "Explicit request-state manifest with exists=true must reject create."
  (let* ((dir (make-temp-file "carriage-diff-" t)))
    (carriage-diff-test--write dir "a.txt" "old\n")
    (with-temp-buffer
      (insert "#+begin_state_manifest\n")
      (insert "path|exists|has_text\n")
      (insert "a.txt|true|true\n")
      (insert "#+end_state_manifest\n")
      (let* ((rep (carriage-dry-run-plan
                   (list '(:version "1" :op create :file "a.txt" :content "new\n"))
                   dir))
             (row (car (plist-get rep :items))))
        (should (eq (plist-get row :status) 'fail))
        (should (string-match-p "Create forbidden"
                                (or (plist-get row :details) "")))))))

;;; carriage-diff-test.el ends here
