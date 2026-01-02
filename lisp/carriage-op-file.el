;;; carriage-op-file.el --- File ops: create/replace/delete/rename  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: ops, files
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/file-header-format-v2.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/file-ops-v2.org
;;   spec/parser-impl-v2.org
;;   spec/parser-registry-v2.org
;;
;;; Commentary:
;; Implementation of create/replace/delete/rename plan items.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-format-registry)

;;;; Prompt fragments

(defun carriage-op-create-prompt-fragment (_ctx)
  "Prompt fragment for :op create (no delimiter markers in v1)."
  (concat
   "CREATE:\n"
   "#+begin_patch (:version \"1\" :op \"create\" :file \"RELATIVE/PATH\")\n"
   "FILE CONTENTS\n"
   "#+end_patch\n"
   "- Requirements: :version \"1\"; :file (not :path). The file content is the raw body between begin/end.\n"
   "- No delimiter markers (<<DELIM/:DELIM) are used in v1.\n"
   "- Aliases like write/create_file/delete_file/rename_file are forbidden; use create/delete/rename/patch.\n"))

(defun carriage-op-delete-prompt-fragment (_ctx)
  "Prompt fragment for :op delete."
  (concat
   "DELETE:\n"
   "#+begin_patch (:version \"1\" :op \"delete\" :file \"RELATIVE/PATH\")\n"
   "#+end_patch\n"
   "- Requirements: :version \"1\"; :file (not :path). No content between begin/end.\n"
   "- Do not use *_file aliases.\n"))

(defun carriage-op-rename-prompt-fragment (_ctx)
  "Prompt fragment for :op rename."
  (concat
   "RENAME:\n"
   "#+begin_patch (:version \"1\" :op \"rename\" :from \"OLD/RELATIVE/PATH\" :to \"NEW/RELATIVE/PATH\")\n"
   "#+end_patch\n"
   "- Requirements: :version \"1\"; :from/:to must be relative paths (not :path).\n"
   "- Do not use *_file aliases.\n"))

;;;; Parse

(defun carriage-parse-create (header body repo-root)
  "Parse :op create from HEADER/BODY under REPO-ROOT. Return plan item alist.

In v1, no delimiter markers are used. The file content is the raw BODY between
#+begin_patch and #+end_patch."
  (let* ((file (plist-get header :file))
         (_v (plist-get header :version))
         (mkdir (if (plist-member header :mkdir)
                    (plist-get header :mkdir) t))
         (ensure-final (if (plist-member header :ensure-final-newline)
                           (plist-get header :ensure-final-newline) t)))
    (unless (and (stringp file) (not (string-empty-p file)))
      (signal (carriage-error-symbol 'OPS_E_PATH) (list file)))
    (let* ((norm (carriage-normalize-path repo-root file)))
      (list (cons :version "1")
            (cons :op 'create)
            (cons :file (file-relative-name norm repo-root))
            (cons :content body)
            (cons :mkdir mkdir)
            (cons :ensure-final-newline ensure-final)))))

(defun carriage-parse-delete (header _body repo-root)
  "Parse :op delete from HEADER under REPO-ROOT."
  (let* ((file (plist-get header :file)))
    (unless (and (stringp file) (not (string-empty-p file)))
      (signal (carriage-error-symbol 'OPS_E_PATH) (list file)))
    (let* ((norm (carriage-normalize-path repo-root file)))
      (list (cons :version "1") (cons :op 'delete) (cons :file (file-relative-name norm repo-root))))))

(defun carriage-parse-rename (header _body repo-root)
  "Parse :op rename from HEADER under REPO-ROOT."
  (let* ((from (plist-get header :from))
         (to   (plist-get header :to)))
    (dolist (p (list from to))
      (unless (and (stringp p) (not (string-empty-p p)))
        (signal (carriage-error-symbol 'OPS_E_PATH) (list p))))
    (let* ((norm-from (carriage-normalize-path repo-root from))
           (norm-to   (carriage-normalize-path repo-root to)))
      (list (cons :version "1")
            (cons :op 'rename)
            (cons :from (file-relative-name norm-from repo-root))
            (cons :to   (file-relative-name norm-to repo-root))))))

;;;; Dry-run

(defun carriage-dry-run-create (plan-item repo-root)
  "Validate create preconditions."
  (let* ((file (alist-get :file plan-item))
         (abs (carriage-normalize-path repo-root file)))
    (if (file-exists-p abs)
        (list :op 'create :status 'fail :file file :details "Already exists")
      (list :op 'create :status 'ok :file file
            :details (format "Will create (%d bytes)"
                             (length (or (alist-get :content plan-item) "")))))))

(defun carriage-dry-run-delete (plan-item repo-root)
  "Validate delete preconditions."
  (let* ((file (alist-get :file plan-item))
         (abs (carriage-normalize-path repo-root file)))
    (if (not (file-exists-p abs))
        (list :op 'delete :status 'fail :file file :details "Not found")
      (list :op 'delete :status 'ok :file file :details "Will delete"))))

(defun carriage-dry-run-rename (plan-item repo-root)
  "Validate rename preconditions."
  (let* ((from (alist-get :from plan-item))
         (to   (alist-get :to plan-item))
         (abs-from (carriage-normalize-path repo-root from))
         (abs-to   (carriage-normalize-path repo-root to)))
    (cond
     ((not (file-exists-p abs-from))
      (list :op 'rename :status 'fail :file from :details "Source not found"))
     ((file-exists-p abs-to)
      (list :op 'rename :status 'fail :file to :details "Target exists"))
     (t (list :op 'rename :status 'ok :file (format "%s -> %s" from to) :details "Will rename")))))

;;;; Apply

(defun carriage-apply-create (plan-item repo-root)
  "Create a new file. Commit is not performed here; optional staging per policy."
  (let* ((file (alist-get :file plan-item))
         (content (alist-get :content plan-item))
         (mkdir (alist-get :mkdir plan-item))
         (ensure-cell (assq :ensure-final-newline plan-item))
         (ensure-final (if ensure-cell (cdr ensure-cell) t))
         (abs (carriage-normalize-path repo-root file))
         (stage (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy)))
    (let ((payload (or content "")))
      (when (and ensure-final
                 (> (length payload) 0)
                 (not (eq (aref payload (1- (length payload))) ?\n)))
        (setq payload (concat payload "\n")))
      (carriage-write-file-string abs payload mkdir))
    (when (and (eq stage 'index)
               (fboundp 'carriage-apply-engine)
               (eq (carriage-apply-engine) 'git))
      (carriage-git-add repo-root file))
    (list :op 'create :status 'ok :file file :details "Created")))

(defun carriage-apply-delete (plan-item repo-root)
  "Delete file. For staging policy 'index use git rm; otherwise delete from FS. No commit."
  (let* ((file (alist-get :file plan-item))
         (stage (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy))
         (abs (carriage-normalize-path repo-root file)))
    (cond
     ((and (eq stage 'index)
           (fboundp 'carriage-apply-engine)
           (eq (carriage-apply-engine) 'git))
      (carriage-git-rm repo-root file)
      (list :op 'delete :status 'ok :file file :details "Deleted (staged)"))
     (t
      (if (file-exists-p abs)
          (progn (delete-file abs)
                 (list :op 'delete :status 'ok :file file :details "Deleted"))
        (list :op 'delete :status 'fail :file file :details "Not found"))))))

(defun carriage-apply-rename (plan-item repo-root)
  "Rename file. For staging policy 'index use git mv; otherwise rename on FS. No commit."
  (let* ((from (alist-get :from plan-item))
         (to   (alist-get :to plan-item))
         (abs-from (carriage-normalize-path repo-root from))
         (abs-to   (carriage-normalize-path repo-root to))
         (to-dir (file-name-directory abs-to))
         (stage (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy)))
    (when (and to-dir (not (file-directory-p to-dir)))
      (make-directory to-dir t))
    (if (and (eq stage 'index)
             (fboundp 'carriage-apply-engine)
             (eq (carriage-apply-engine) 'git))
        (let ((mvres (carriage-git-mv repo-root from to)))
          (if (and (plist-get mvres :exit) (zerop (plist-get mvres :exit)))
              (list :op 'rename :status 'ok :file (format "%s -> %s" from to) :details "Renamed (staged)")
            (list :op 'rename :status 'fail :file (format "%s -> %s" from to)
                  :details "git mv failed"
                  :extra (list :exit (plist-get mvres :exit)
                               :stderr (plist-get mvres :stderr)
                               :stdout (plist-get mvres :stdout)))))
      (progn
        (rename-file abs-from abs-to)
        (list :op 'rename :status 'ok :file (format "%s -> %s" from to) :details "Renamed")))))

;;;; Registration

(carriage-format-register 'create "1"
                          :parse #'carriage-parse-create
                          :dry-run #'carriage-dry-run-create
                          :apply #'carriage-apply-create
                          :prompt-fragment #'carriage-op-create-prompt-fragment)

(carriage-format-register 'delete "1"
                          :parse #'carriage-parse-delete
                          :dry-run #'carriage-dry-run-delete
                          :apply #'carriage-apply-delete
                          :prompt-fragment #'carriage-op-delete-prompt-fragment)

(carriage-format-register 'rename "1"
                          :parse #'carriage-parse-rename
                          :dry-run #'carriage-dry-run-rename
                          :apply #'carriage-apply-rename
                          :prompt-fragment #'carriage-op-rename-prompt-fragment)

(provide 'carriage-op-file)
;;; carriage-op-file.el ends here
