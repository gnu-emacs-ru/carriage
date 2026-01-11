;;; carriage-context-plan-test.el --- Tests for begin_context_plan  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

(require 'carriage-context-plan)

(defun carriage-test--git (dir &rest args)
  "Run git ARGS in DIR, return stdout (string). Signal on non-zero exit."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let ((exit (apply #'process-file "git" nil (list (current-buffer) t) nil args)))
        (unless (and (integerp exit) (zerop exit))
          (error "git failed (dir=%s args=%S): %s" dir args (buffer-string)))
        (buffer-string)))))

(defun carriage-test--write-file (root rel content)
  "Write CONTENT to ROOT/REL, creating parent directories."
  (let* ((abs (expand-file-name rel root))
         (dir (file-name-directory abs)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))
    (with-temp-file abs
      (insert (or content "")))
    abs))

(defun carriage-test--make-temp-git-repo ()
  "Create and return a temporary git repo directory."
  (let* ((root (make-temp-file "carriage-ctxplan-" t)))
    (carriage-test--git root "init")
    ;; Files for tests
    (carriage-test--write-file root "a.el" ";; a\n")
    (carriage-test--write-file root "src/b.el" ";; b\n")
    (carriage-test--write-file root "src/nested/c.el" ";; c\n")
    ;; Ignored files/dirs
    (carriage-test--write-file root ".gitignore" "ignored.el\nignored-dir/\n")
    (carriage-test--write-file root "ignored.el" ";; ignored\n")
    (carriage-test--write-file root "ignored-dir/x.el" ";; ignored dir file\n")
    root))

(defun carriage-test--ctxplan-clear-cache ()
  (when (boundp 'carriage-context-plan--repo-files-cache)
    (clrhash carriage-context-plan--repo-files-cache)))

(defun carriage-test--count-begin-context-blocks ()
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (n 0))
      (while (re-search-forward "^[ \t]*#\\+begin_context[ \t]*$" nil t)
        (setq n (1+ n)))
      n)))

(defun carriage-test--materialized-context-lines ()
  "Return list of lines from the materialized begin_context right after the plan."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (re-search-forward "^[ \t]*#\\+end_context_plan\\b" nil t)
      (forward-line 1)
      (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
        (forward-line 1))
      (should (looking-at-p "^[ \t]*#\\+begin_context\\b"))
      (forward-line 1)
      (let ((beg (point)))
        (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
        (split-string (buffer-substring-no-properties beg (line-beginning-position))
                      "\n" t)))))

(defun carriage-test--with-plan-buffer (root plan-body &optional tail)
  "Create an org buffer with a plan body and optional TAIL text after it."
  (let ((buf (generate-new-buffer " *carriage-ctxplan-test*")))
    (with-current-buffer buf
      (setq default-directory (file-name-as-directory (expand-file-name root)))
      (org-mode)
      (insert "#+begin_context_plan\n")
      (insert plan-body)
      (unless (string-suffix-p "\n" plan-body) (insert "\n"))
      (insert "#+end_context_plan\n")
      (when tail
        (insert tail))
      ;; Place point inside the plan block by default.
      (goto-char (point-min))
      (re-search-forward "^[ \t]*#\\+begin_context_plan\\b" nil t)
      (forward-line 1))
    buf))

(ert-deftest carriage-ctxplan-glob-star-el-is-recursive-and-matches-root ()
  (carriage-test--ctxplan-clear-cache)
  (let* ((root (carriage-test--make-temp-git-repo)))
    (unwind-protect
        (let ((buf (carriage-test--with-plan-buffer root "*.el\n" "\nAfter\n")))
          (unwind-protect
              (with-current-buffer buf
                (let* ((res (carriage-context-plan-compile (current-buffer)))
                       (lines (carriage-test--materialized-context-lines)))
                  (should (plist-get res :ok))
                  ;; Must include root-level file too (a.el), not only under dirs.
                  (should (equal lines '("a.el" "src/b.el" "src/nested/c.el")))
                  ;; Exactly one begin_context created
                  (should (= (carriage-test--count-begin-context-blocks) 1))))
            (kill-buffer buf)))
      (delete-directory root t))))

(ert-deftest carriage-ctxplan-glob-src-only ()
  (carriage-test--ctxplan-clear-cache)
  (let* ((root (carriage-test--make-temp-git-repo)))
    (unwind-protect
        (let ((buf (carriage-test--with-plan-buffer root "GLOB src/**/*.el\n" nil)))
          (unwind-protect
              (with-current-buffer buf
                (carriage-context-plan-compile (current-buffer))
                (should (equal (carriage-test--materialized-context-lines)
                               '("src/b.el" "src/nested/c.el"))))
            (kill-buffer buf)))
      (delete-directory root t))))

(ert-deftest carriage-ctxplan-gitignore-excluded ()
  (carriage-test--ctxplan-clear-cache)
  (let* ((root (carriage-test--make-temp-git-repo)))
    (unwind-protect
        (let ((buf (carriage-test--with-plan-buffer root "*.el\n" nil)))
          (unwind-protect
              (with-current-buffer buf
                (carriage-context-plan-compile (current-buffer))
                (let ((lines (carriage-test--materialized-context-lines)))
                  (should (not (member "ignored.el" lines)))
                  (should (not (member "ignored-dir/x.el" lines)))))
            (kill-buffer buf)))
      (delete-directory root t))))

(ert-deftest carriage-ctxplan-upsert-immediate-only-and-idempotent ()
  (carriage-test--ctxplan-clear-cache)
  (let* ((root (carriage-test--make-temp-git-repo))
         (tail
          (concat
           "\n"
           "#+begin_context\n"
           "DUMMY\n"
           "#+end_context\n"
           "\nSome text\n"
           "#+begin_context\n"
           "SHOULDSTAY\n"
           "#+end_context\n")))
    (unwind-protect
        (let ((buf (carriage-test--with-plan-buffer root "*.el\n" tail)))
          (unwind-protect
              (with-current-buffer buf
                ;; First compile updates immediate begin_context (replaces DUMMY).
                (carriage-context-plan-compile (current-buffer))
                (should (equal (carriage-test--materialized-context-lines)
                               '("a.el" "src/b.el" "src/nested/c.el")))
                (should (= (carriage-test--count-begin-context-blocks) 2))
                ;; The later begin_context must be untouched.
                (save-excursion
                  (goto-char (point-min))
                  (should (re-search-forward "^SHOULDSTAY$" nil t)))

                ;; Idempotency: second compile doesn't change buffer text.
                (let ((s1 (buffer-string)))
                  (carriage-context-plan-compile (current-buffer))
                  (should (equal s1 (buffer-string))))

                ;; Now change plan and compile: still only the immediate block is updated.
                (save-excursion
                  (goto-char (point-min))
                  (re-search-forward "^[ \t]*#\\+begin_context_plan\\b" nil t)
                  (forward-line 1)
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert "GLOB src/**/*.el"))
                (carriage-context-plan-compile (current-buffer))
                (should (equal (carriage-test--materialized-context-lines)
                               '("src/b.el" "src/nested/c.el")))
                (should (= (carriage-test--count-begin-context-blocks) 2))
                (save-excursion
                  (goto-char (point-min))
                  (should (re-search-forward "^SHOULDSTAY$" nil t))))
            (kill-buffer buf)))
      (delete-directory root t))))

(ert-deftest carriage-ctxplan-truncation-deterministic ()
  (carriage-test--ctxplan-clear-cache)
  (let* ((root (carriage-test--make-temp-git-repo)))
    (unwind-protect
        (let ((buf (carriage-test--with-plan-buffer root "*.el\n" nil)))
          (unwind-protect
              (with-current-buffer buf
                (let ((carriage-context-plan-max-files 2))
                  (let* ((res (carriage-context-plan-compile (current-buffer)))
                         (lines (carriage-test--materialized-context-lines))
                         (warnings (plist-get res :warnings))
                         (stats (plist-get res :stats)))
                    (should (equal lines '("a.el" "src/b.el")))
                    (should (string-match-p "CTXPLAN_W_LIMIT" (mapconcat #'identity warnings "\n")))
                    (should (= (plist-get stats :truncated) 1)))))
            (kill-buffer buf)))
      (delete-directory root t))))

(ert-deftest carriage-ctxplan-parse-errors ()
  (carriage-test--ctxplan-clear-cache)
  (let* ((root (carriage-test--make-temp-git-repo)))
    (unwind-protect
        (let ((buf (carriage-test--with-plan-buffer root "GLOB\n" nil)))
          (unwind-protect
              (with-current-buffer buf
                (let ((err (should-error (carriage-context-plan-compile (current-buffer))
                                         :type 'user-error)))
                  (should (string-match-p "CTXPLAN_E_PARSE" (cadr err)))))
            (kill-buffer buf)))
      (delete-directory root t))))

(ert-deftest carriage-ctxplan-non-git-repo-errors ()
  (carriage-test--ctxplan-clear-cache)
  (let* ((root (make-temp-file "carriage-ctxplan-nogit-" t)))
    (unwind-protect
        (let ((buf (carriage-test--with-plan-buffer root "*.el\n" nil)))
          (unwind-protect
              (with-current-buffer buf
                (let ((err (should-error (carriage-context-plan-compile (current-buffer))
                                         :type 'user-error)))
                  (should (string-match-p "CTXPLAN_E_NOT_GIT" (cadr err)))))
            (kill-buffer buf)))
      (delete-directory root t))))

(ert-deftest carriage-ctxplan-no-content-reads ()
  (carriage-test--ctxplan-clear-cache)
  (let* ((root (carriage-test--make-temp-git-repo)))
    (unwind-protect
        (let ((buf (carriage-test--with-plan-buffer root "*.el\n" nil)))
          (unwind-protect
              (with-current-buffer buf
                (let ((guard (lambda (&rest _args)
                               (ert-fail "compile must not read file contents via insert-file-contents"))))
                  (advice-add 'insert-file-contents :around guard)
                  (advice-add 'insert-file-contents-literally :around guard)
                  (unwind-protect
                      (progn
                        (carriage-context-plan-compile (current-buffer))
                        (should (equal (carriage-test--materialized-context-lines)
                                       '("a.el" "src/b.el" "src/nested/c.el"))))
                    (advice-remove 'insert-file-contents guard)
                    (advice-remove 'insert-file-contents-literally guard)))))
          (kill-buffer buf)))
    (delete-directory root t)))

(ert-deftest carriage-ctxplan-symlink-escape-excluded ()
  (carriage-test--ctxplan-clear-cache)
  (let* ((root (carriage-test--make-temp-git-repo))
         (outside-dir (make-temp-file "carriage-ctxplan-outside-" t))
         (outside-file (expand-file-name "outside.el" outside-dir)))
    (unwind-protect
        (progn
          (carriage-test--write-file outside-dir "outside.el" ";; outside\n")
          (let* ((link (expand-file-name "link.el" root)))
            (condition-case _e
                (make-symbolic-link outside-file link t)
              (error
               (ert-skip "Symlinks not supported on this platform/user")))
            (let ((buf (carriage-test--with-plan-buffer root "PATH link.el\n" nil)))
              (unwind-protect
                  (with-current-buffer buf
                    (let* ((res (carriage-context-plan-compile (current-buffer)))
                           (lines (carriage-test--materialized-context-lines))
                           (warnings (plist-get res :warnings)))
                      (should (equal lines '()))
                      (should (string-match-p "CTXPLAN_W_OUTSIDE_ROOT"
                                              (mapconcat #'identity warnings "\n")))))
                (kill-buffer buf)))))
      (delete-directory outside-dir t)
      (delete-directory root t))))

(provide 'carriage-context-plan-test)
;;; carriage-context-plan-test.el ends here
