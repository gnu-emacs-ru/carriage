;;; carriage-branching-e2e-tests.el --- E2E branching tests (P1 default, P3+inherit)  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(require 'carriage-task)
(require 'carriage-doc-state)
(require 'carriage-context)

(defun carriage--test--make-temp-dir ()
  "Return a fresh temporary directory path (with trailing slash)."
  (file-name-as-directory (make-temp-file "carriage-e2e-" t)))

(defun carriage--test--write-file (path content)
  "Write CONTENT to PATH, creating directories as needed."
  (make-directory (file-name-directory (expand-file-name path)) t)
  (with-temp-file (expand-file-name path)
    (insert content)))

(defun carriage--test--first-org-created (root)
  "Return path of the first created org doc under ROOT/org matching NN.N-*.org, or nil."
  (let* ((dir (expand-file-name "org" (file-name-as-directory root)))
         (files (and (file-directory-p dir)
                     (directory-files dir t "^[0-9]+\\.[0-9]+-.*\\.org\\'"))))
    (car files)))

(defun carriage--test--buffer-string-of (file)
  "Return contents of FILE as string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest carriage-branching/e2e-default-p1 ()
  "E2E: Branching from TODO heading with default P1 profile produces begin_context and CAR_*."
  (unless (getenv "CARRIAGE_RUN_E2E")
  (ert-skip "Set CARRIAGE_RUN_E2E=1 to run E2E branching tests"))
  (let* ((root (carriage--test--make-temp-dir))
         (default-directory root)
         (todo (expand-file-name "TODO.org" root))
         ;; Minimal TODO with one heading
         (todo-content "* TODO Implement feature\n\nSome notes here.\n"))
    (carriage--test--write-file todo todo-content)
    ;; Open TODO and place point at heading
    (let ((buf (find-file-noselect todo)))
      (with-current-buffer buf
        (org-mode)
        (goto-char (point-min))
        ;; Call create-from-template (defaults to task/default and P1)
        (let ((carriage-task-branch-default-profile 'p1))
          (carriage-task-create-doc-from-template)))
      ;; Assert created file exists
      (let* ((created (carriage--test--first-org-created root)))
        (should (stringp created))
        (let* ((s (carriage--test--buffer-string-of created)))
          ;; begin_context is present (may be empty body)
          (should (string-match-p "^[ \t]*#\\+begin_context\\b" s))
          (should (string-match-p "^[ \t]*#\\+end_context\\b" s))
          ;; begin_carriage contains provenance and profile P1
          (should (string-match-p "^[ \t]*#\\+begin_carriage\\b" s))
          (should (string-match-p "^CARRIAGE_TEMPLATE_ID[ \t]+" s))
          (should (string-match-p "^CARRIAGE_TEMPLATE_VER[ \t]+" s))
          (should (string-match-p "^CARRIAGE_CONTEXT_PROFILE[ \t]+P1\\b" s)))))))

(ert-deftest carriage-branching/e2e-p3-inherit-dedup ()
  "E2E: Branching with P3 + inherit copies parent's begin_context with dedup and sets CAR_CONTEXT_PROFILE=P3."
  (skip-unless (getenv "CARRIAGE_RUN_E2E"))
  (let* ((root (carriage--test--make-temp-dir))
         (default-directory root)
         (origin (expand-file-name "parent.org" root))
         ;; Parent document with duplicated begin_context paths
         (parent-content "#+title: Parent\n\n#+begin_context\nfoo/bar.el\nfoo/bar.el\nspec/doc.org\n#+end_context\n\n* Task Head\nBody.\n"))
    (carriage--test--write-file origin parent-content)
    (let ((buf (find-file-noselect origin)))
      (with-current-buffer buf
        (org-mode)
        ;; Go to heading
        (goto-char (point-min))
        (re-search-forward "^\\*\\s-+Task Head\\b")
        ;; Create from template with explicit P3 + inherit begin_context and flags
        ;; Signature: (carriage-task-create-doc-from-template &optional template-id profile inherit-begin inherit-flags)
        (carriage-task-create-doc-from-template 'task/default 'p3 t t)))
    (let* ((created (carriage--test--first-org-created root)))
      (should (stringp created))
      (let* ((s (carriage--test--buffer-string-of created)))
        ;; Context block present and deduped (foo/bar.el only once)
        (should (string-match-p "^[ \t]*#\\+begin_context\\b" s))
        (let* ((lines (split-string s "\n"))
               (ctx-start (cl-position-if (lambda (ln) (string-match-p "^[ \t]*#\\+begin_context\\b" ln)) lines))
               (ctx-end   (cl-position-if (lambda (ln) (string-match-p "^[ \t]*#\\+end_context\\b" ln)) lines)))
          (should (and ctx-start ctx-end (< ctx-start ctx-end)))
          (let* ((body (cl-subseq lines (1+ ctx-start) ctx-end))
                 (trim (cl-remove-if (lambda (ln) (or (string-match-p "^[ \t]*$" ln)
                                                 (string-match-p "^[ \t]*#" ln)))
                                     body)))
            ;; foo/bar.el appears at most once
            (should (<= (cl-count "foo/bar.el" trim :test #'string=) 1))
            ;; spec/doc.org propagated
            (should (member "spec/doc.org" trim))))
        ;; begin_carriage has P3
        (should (string-match-p "^[ \t]*#\\+begin_carriage\\b" s))
        (should (string-match-p "^CARRIAGE_CONTEXT_PROFILE[ \t]+P3\\b" s))
        (should (string-match-p "^CARRIAGE_TEMPLATE_ID[ \t]+" s))
        (should (string-match-p "^CARRIAGE_TEMPLATE_VER[ \t]+" s))))))

(provide 'carriage-branching-e2e-tests)
;;; carriage-branching-e2e-tests.el ends here
