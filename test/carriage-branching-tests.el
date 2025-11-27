;;; carriage-branching-tests.el --- Tests for branching provenance and context blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; ERT tests for:
;; - Writing provenance into #+begin_carriage and reading it back via carriage-doc-state.
;; - Inserting begin_context blocks with given paths.

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-task)
(require 'carriage-doc-state)

(ert-deftest carriage-branching/provenance-write-and-read ()
  "Write provenance into begin_carriage and read it back via carriage-doc-state-read."
  (with-temp-buffer
    (org-mode)
    ;; Ensure empty buffer; write provenance like carriage-task-create-from-template would do.
    (carriage-task--write-carriage-provenance
     (current-buffer)
     'task/default "1.0" 'p1 (list :begin t :flags t))
    ;; Now read via doc-state; it prefers begin_carriage.
    (let* ((pl (carriage-doc-state-read (current-buffer)))
           (tid (plist-get pl :CAR_TEMPLATE_ID))
           (tver (plist-get pl :CAR_TEMPLATE_VER))
           (prof (plist-get pl :CAR_CONTEXT_PROFILE))
           (inh (plist-get pl :CAR_INHERITED)))
      (should (stringp tid))
      (should (stringp tver))
      (should (stringp prof))
      (should (stringp inh))
      (should (string-match-p "\\`task/default\\'" tid))
      (should (equal tver "1.0"))
      (should (equal prof "P1"))
      (should (member inh '("BOTH" "BEGIN" "FLAGS" "NONE"))))))

(ert-deftest carriage-branching/insert-begin-context-with-paths ()
  "Insert a begin_context block with provided paths at a sensible location."
  (with-temp-buffer
    (org-mode)
    ;; Insert a minimal header and Context section to anchor insertion
    (insert "#+title: Demo\n\n* Context\n\n* Plan\n")
    (let* ((paths '("lisp/carriage-task.el" "spec/document-branching-and-templates-v1.org")))
      (carriage-task--insert-begin-context-in-buffer (current-buffer) paths))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (should (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t))
      (should (re-search-forward "^lisp/carriage-task\\.el$" nil t))
      (should (re-search-forward "^spec/document-branching-and-templates-v1\\.org$" nil t))
      (should (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)))))

;; Integration tests for create-from-template (P1/P3, inheritance, dedup)
(ert-deftest carriage-branching/create-from-template-p1-and-p3 ()
  "Create docs from template with P1 and P3; verify begin_context and CAR_*."
  (unless (getenv "CARRIAGE_RUN_E2E")
  (ert-skip "Set CARRIAGE_RUN_E2E=1 to run branching transient/e2e tests"))
  (let* ((tmp (make-temp-file "carriage-branch-" t))  ;; temp project root
         (default-directory (file-name-as-directory tmp)))
    ;; Prepare origin TODO with a heading and a begin_context (with duplicates)
    (let* ((todo (expand-file-name "TODO.org" default-directory)))
      (with-current-buffer (find-file-noselect todo)
        (org-mode)
        (erase-buffer)
        (insert "* Test Task\n\n")
        (insert "#+begin_context\nspec/index.org\nspec/index.org\nlisp/carriage-task.el\n#+end_context\n")
        (save-buffer)))
    ;; P1 (no inheritance) — should create org/NN.* file with begin_context present (empty block allowed)
    (let* ((origin-buf (find-file-noselect (expand-file-name "TODO.org" default-directory))))
      (with-current-buffer origin-buf
        (goto-char (point-min))
        (re-search-forward "^\\* Test Task$")
        ;; Create with P1, no inheritance
        (carriage-task-create-doc-from-template 'task/default 'p1 nil nil))
      (let* ((org-dir (expand-file-name "org" default-directory))
             (created (car (directory-files org-dir t "^[0-9]+\\..*-.*\\.org\\'")))
             (content (with-temp-buffer
                        (insert-file-contents created)
                        (buffer-substring-no-properties (point-min) (point-max)))))
        (should (string-match-p "^[ \t]*#\\+begin_context" content))
        (should (string-match-p "^[ \t]*#\\+end_context" content))
        (should (string-match-p "CARRIAGE_TEMPLATE_ID[ \t]+task/default" content))
        (should (string-match-p "CARRIAGE_CONTEXT_PROFILE[ \t]+P1" content))))
    ;; P3 (inherit begin_context) — should dedup paths and mark inherited; profile P3 in CAR_*.
    (let* ((origin-buf (find-file-noselect (expand-file-name "TODO.org" default-directory))))
      (with-current-buffer origin-buf
        (goto-char (point-min))
        (re-search-forward "^\\* Test Task$")
        (carriage-task-create-doc-from-template 'task/default 'p3 t t))
      (let* ((org-dir (expand-file-name "org" default-directory))
             ;; pick newest by mtime
             (cands (directory-files org-dir t "^[0-9]+\\..*-.*\\.org\\'"))
             (created (car (last (sort cands (lambda (a b)
                                               (time-less-p (nth 5 (file-attributes a))
                                                            (nth 5 (file-attributes b))))))))
             (content (with-temp-buffer
                        (insert-file-contents created)
                        (buffer-substring-no-properties (point-min) (point-max)))))
        (should (string-match-p "CARRIAGE_CONTEXT_PROFILE[ \t]+P3" content))
        ;; Inherited content deduped: each path should appear once
        (should (string-match-p "^[ \t]*#\\+begin_context" content))
        (let* ((lines (with-temp-buffer
                        (insert content)
                        (goto-char (point-min))
                        (re-search-forward "^[ \t]*#\\+begin_context\\b")
                        (let ((beg (line-end-position)))
                          (re-search-forward "^[ \t]*#\\+end_context\\b")
                          (let ((end (line-beginning-position)))
                            (narrow-to-region (1+ beg) end)
                            (goto-char (point-min))
                            (let (acc)
                              (while (not (eobp))
                                (let ((s (string-trim (buffer-substring (line-beginning-position)
                                                                       (line-end-position)))))
                                  (unless (string-empty-p s) (push s acc)))
                                (forward-line 1))
                              (nreverse acc)))))))
          ;; spec/index.org should be unique
          (should (= (length (seq-filter (lambda (s) (string= s "spec/index.org")) lines)) 1))))))
  t)

;; Fallback transient path (no transient available): prompts and creates via create-from-template
(ert-deftest carriage-branching/transient-fallback ()
  "carriage-branching-transient should work without transient.el by falling back to prompts.
Creates a doc from default template with P1 profile and no inheritance."
  (skip-unless (getenv "CARRIAGE_RUN_E2E"))
  (skip-unless (getenv "CARRIAGE_RUN_E2E"))
  (let* ((tmp (make-temp-file "carriage-branch-transient-" t))
         (default-directory (file-name-as-directory tmp))
         (todo (expand-file-name "TODO.org" default-directory)))
    ;; Prepare origin TODO with a heading
    (with-current-buffer (find-file-noselect todo)
      (org-mode)
      (erase-buffer)
      (insert "* Test Task\n\nSome body\n")
      (save-buffer))
    ;; Visit heading and invoke transient (fallback prompts)
    (with-current-buffer (find-file-noselect todo)
      (goto-char (point-min))
      (re-search-forward "^\\* Test Task$")
      ;; Stub require to pretend transient is unavailable; stub completing-read and y-or-n-p.
      (cl-letf (((symbol-function 'require)
                 (lambda (feat &rest args)
                   (if (eq feat 'transient) nil (apply #'require feat args))))
                ;; Choose first candidate label in the list
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (cond
                    ;; template label pick
                    ((and (listp choices) (stringp (car choices))) (car choices))
                    (t (car choices)))))
                ;; All y-or-n-p prompts → 'no' to keep P1 and no inheritance
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (carriage-branching-transient)))
    ;; Validate result
    (let* ((org-dir (expand-file-name "org" default-directory))
           (created (car (directory-files org-dir t "^[0-9]+\\..*-.*\\.org\\'")))
           (content (with-temp-buffer
                      (insert-file-contents created)
                      (buffer-substring-no-properties (point-min) (point-max)))))
      (should (string-match-p "^[ \t]*#\\+begin_context" content))
      (should (string-match-p "^[ \t]*#\\+end_context" content))
      (should (string-match-p "CARRIAGE_TEMPLATE_ID[ \t]+task/default" content))
      (should (string-match-p "CARRIAGE_CONTEXT_PROFILE[ \t]+P1" content)))))

(ert-deftest carriage-branching/transient-created-doc-has-begin-context ()
  "Transient fallback creates a new doc with an explicit begin_context block."
  (skip-unless (getenv "CARRIAGE_RUN_E2E"))
  (let* ((tmp (make-temp-file "carriage-branch-transient-" t))
         (default-directory (file-name-as-directory tmp))
         (todo (expand-file-name "TODO.org" default-directory)))
    ;; Prepare origin TODO with a heading
    (with-current-buffer (find-file-noselect todo)
      (org-mode)
      (erase-buffer)
      (insert "* Test Task\n\nSome body\n")
      (save-buffer))
    ;; Visit heading and invoke transient (fallback prompts)
    (with-current-buffer (find-file-noselect todo)
      (goto-char (point-min))
      (re-search-forward "^\\* Test Task$")
      (cl-letf (((symbol-function 'require)
                 (lambda (feat &rest args)
                   (if (eq feat 'transient) nil (apply #'require feat args))))
                ;; Choose first candidate label in the list
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (cond
                    ((and (listp choices) (stringp (car choices))) (car choices))
                    (t (car choices)))))
                ;; All y-or-n-p prompts → 'no' to keep P1 and no inheritance
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (carriage-branching-transient)))
    ;; Validate result
    (let* ((org-dir (expand-file-name "org" default-directory))
           (created (car (directory-files org-dir t "^[0-9]+\\..*-.*\\.org\\'")))
           (content (with-temp-buffer
                      (insert-file-contents created)
                      (buffer-substring-no-properties (point-min) (point-max)))))
      ;; Must contain explicit begin_context/end_context
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (should (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t))
        (should (re-search-forward "^[ \t]*#\\+end_context\\b" nil t))))))

(ert-deftest carriage-branching/transient-p3-inherit-dedup ()
  "End-to-end transient fallback: choose P3 + inherit; verify CAR_CONTEXT_PROFILE and deduped begin_context."
  (skip-unless (getenv "CARRIAGE_RUN_E2E"))
  (let* ((tmp (make-temp-file "carriage-branch-transient2-" t))
         (default-directory (file-name-as-directory tmp))
         (todo (expand-file-name "TODO.org" default-directory)))
    ;; Seed parent TODO with a heading and begin_context (with duplicates)
    (with-current-buffer (find-file-noselect todo)
      (org-mode)
      (erase-buffer)
      (insert "* Demo Task\n\n")
      (insert "#+begin_context\nspec/index.org\nspec/index.org\nlisp/carriage-task.el\n#+end_context\n")
      (save-buffer))
    ;; Visit heading and invoke transient in fallback path (transient not available).
    (with-current-buffer (find-file-noselect todo)
      (goto-char (point-min))
      (re-search-forward "^\\* Demo Task$")
      (let ((answers (list t t t))) ;; P3? yes; inherit begin? yes; inherit CAR_*? yes
        (cl-letf
            (((symbol-function 'require)
              (lambda (feat &rest args)
                (if (eq feat 'transient) nil (apply #'require feat args))))
             ;; Choose first candidate string in the completing-read choices
             ((symbol-function 'completing-read)
              (lambda (_prompt choices &rest _)
                (cond
                 ((and (listp choices) (stringp (car choices))) (car choices))
                 (t (car choices)))))
             ;; Sequential y-or-n-p answers for prompts
             ((symbol-function 'y-or-n-p)
              (lambda (&rest _)
                (prog1 (car answers) (setq answers (cdr answers))))))
          (carriage-branching-transient))))
    ;; Validate result
    (let* ((org-dir (expand-file-name "org" default-directory))
           (created (car (directory-files org-dir t "^[0-9]+\\..*-.*\\.org\\'")))
           (content (with-temp-buffer
                      (insert-file-contents created)
                      (buffer-substring-no-properties (point-min) (point-max)))))
      ;; Must contain begin_context with deduped paths
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (should (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t))
        (should (re-search-forward "^[ \t]*#\\+end_context\\b" nil t))
        ;; Dedup check: spec/index.org appears exactly once
        (goto-char (point-min))
        (let ((cnt 0))
          (while (re-search-forward "^spec/index\\.org$" nil t)
            (setq cnt (1+ cnt)))
          (should (= cnt 1))))
      ;; CAR_CONTEXT_PROFILE P3 recorded in #+begin_carriage
      (should (string-match-p "^CARRIAGE_CONTEXT_PROFILE[ \t]+P3\\b" content)))))

(provide 'carriage-branching-tests)
;;; carriage-branching-tests.el ends here
