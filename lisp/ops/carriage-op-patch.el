;;; carriage-op-patch.el --- Unified diff patch op (one file)  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: ops, patch
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/file-header-format-v2.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/patch-unified-diff-v2.org
;;   spec/parser-impl-v2.org
;;   spec/parser-registry-v2.org
;;
;;; Commentary:
;; Parsing and validation for unified diff blocks (single-file patch).
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)

(require 'carriage-format-registry)

(defun carriage--patch--reject-rename-copy (text)
  "Signal when TEXT contains rename/copy prelude forbidden in v1."
  (when (or (string-match-p "^rename from " text)
            (string-match-p "^rename to " text)
            (string-match-p "^copy from " text)
            (string-match-p "^copy to " text))
    (signal (carriage-error-symbol 'PATCH_E_RENAME_COPY)
            (list "rename/copy prelude not supported in v1"))))

(defun carriage--patch--reject-binary (text)
  "Signal when TEXT contains binary patch markers forbidden in v1."
  (when (or (string-match-p "^GIT[ \t]+binary[ \t]+patch" text)
            (string-match-p "^Binary files .+ differ" text))
    (signal (carriage-error-symbol 'PATCH_E_BINARY)
            (list "binary patches are not supported in v1"))))

(defun carriage--patch--extract-paths (text)
  "Return plist (:old OLD :new NEW) from TEXT using ---/+++ lines.
OLD/NEW are raw header payloads after \"--- \" and \"+++ \"."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let* ((old nil) (new nil))
      (when (re-search-forward "^---[ \t]+\\(.+\\)$" nil t)
        (setq old (string-trim (match-string 1))))
      (goto-char (point-min))
      (when (re-search-forward "^\\+\\+\\+[ \t]+\\(.+\\)$" nil t)
        (setq new (string-trim (match-string 1))))
      (list :old old :new new))))

(defun carriage--patch--single-file-p (text)
  "Return non-nil if TEXT contains at most one pair of ---/+++ headers."
  (let ((count 0))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^---[ \t]+" nil t)
        (setq count (1+ count))))
    (<= count 1)))

(defun carriage--patch--normalize-path (root old new)
  "From OLD/NEW header payloads compute RELPATH for single-file patch.
Supports (/dev/null,b/path) for create and (a/path,/dev/null) for delete.
Returns RELPATH string. Signals PATCH_E_PATH_MISMATCH when a/b paths differ."
  (cond
   ;; Create
   ((and (stringp old) (stringp new)
         (string-prefix-p "/dev/null" old)
         (string-prefix-p "b/" new))
    (let* ((rel (substring new 2)))
      ;; Validate path is safe and within repo; signal on violations.
      (carriage-normalize-path root rel)
      rel))
   ;; Delete
   ((and (stringp old) (stringp new)
         (string-prefix-p "a/" old)
         (string-prefix-p "/dev/null" new))
    (let* ((rel (substring old 2)))
      ;; Validate path is safe and within repo; signal on violations.
      (carriage-normalize-path root rel)
      rel))
   ;; Modify existing
   ((and (stringp old) (stringp new)
         (string-prefix-p "a/" old)
         (string-prefix-p "b/" new))
    (let* ((r1 (substring old 2))
           (r2 (substring new 2)))
      (unless (string= r1 r2)
        (signal (carriage-error-symbol 'PATCH_E_PATH_MISMATCH)
                (list (format "Paths differ: a/%s vs b/%s" r1 r2))))
      ;; Validate path is safe and within repo; signal on violations.
      (carriage-normalize-path root r1)
      r1))
   (t
    (signal (carriage-error-symbol 'PATCH_E_MULTI_FILE)
            (list (format "Unsupported ---/+++ pair: %S / %S" old new))))))

(defun carriage-parse-diff (header body repo-root)
  "Parse unified diff (v1).
Ignores header :apply key entirely; determines :path from ---/+++ lines.
Returns a plan item alist: (:version \"1\" :op 'patch :strip N :path REL :diff BODY)."
  (let* ((version (carriage--plan-kv header :version))
         (op (carriage--plan-kv header :op))
         ;; Ignore any :apply key in header (v1 behavior). Detect in plist or alist.
         (has-apply (or (plist-member header :apply) (assq :apply header)))
         (_ignored-apply (and has-apply (carriage--plan-kv header :apply)))
         ;; :strip defaults to 1; if provided, must be integer ≥ 0; otherwise error.
         ;; Be robust to plist/alist and to cases where value is non-nil even if plist-member/assq fail.
         (strip-raw (carriage--plan-kv header :strip))
         (strip-present (or (plist-member header :strip)
                            (assq :strip header)
                            (not (null strip-raw))))
         (strip (if strip-present
                    (if (and (integerp strip-raw) (>= strip-raw 0)) strip-raw
                      (signal (carriage-error-symbol 'PATCH_E_STRIP)
                              (list (format "Invalid :strip (expected integer>=0): %S" strip-raw))))
                  1)))
    (unless (string= version "1")
      (signal (carriage-error-symbol 'PATCH_E_VERSION) (list version)))
    (unless (member (format "%s" op) '("patch" :patch patch))
      (signal (carriage-error-symbol 'PATCH_E_OP) (list op)))
    ;; Hard limits and forbidden preludes
    (when (> (string-bytes body) (* 4 1024 1024))
      (signal (carriage-error-symbol 'PATCH_E_LIMITS)
              (list "Patch body exceeds 4MiB")))
    (carriage--patch--reject-rename-copy body)
    (carriage--patch--reject-binary body)
    ;; Ensure single-file diff (one ---/+++ pair)
    (unless (carriage--patch--single-file-p body)
      (signal (carriage-error-symbol 'PATCH_E_MULTI_FILE) (list "Multiple files in one patch")))
    (let* ((paths (carriage--patch--extract-paths body))
           (old (plist-get paths :old))
           (new (plist-get paths :new)))
      (unless (and old new)
        (signal (carriage-error-symbol 'PATCH_E_DIFF_SYNTAX) (list "Missing ---/+++ headers")))
      ;; Validate :strip against conventional a/ b/ prefixes
      (when (and (string-prefix-p "a/" (or old "")) (string-prefix-p "b/" (or new ""))
                 (not (= strip 1)))
        (signal (carriage-error-symbol 'PATCH_E_STRIP)
                (list (format "strip mismatch for a/b prefixes: %s" strip))))
      (let* ((rel
              (condition-case e
                  (carriage--patch--normalize-path repo-root old new)
                (error
                 (let ((sym (car e)))
                   (cond
                    ;; Map path-related errors to PATCH_E_PATH
                    ((memq sym (list (carriage-error-symbol 'OPS_E_PATH)
                                     (carriage-error-symbol 'IO_E_PATH)))
                     (signal (carriage-error-symbol 'PATCH_E_PATH) (cdr e)))
                    (t (signal sym (cdr e)))))))))
        (list (cons :version "1")
              (cons :op 'patch)
              (cons :strip strip)
              (cons :path rel)
              (cons :diff body))))))

(defun carriage--plan-kv (item key)
  "Return KEY from ITEM that may be a plist or an alist."
  (if (plist-member item key) (plist-get item key) (alist-get key item)))





;;; Prompt fragment
(defun carriage-op-patch-prompt-fragment (_ctx)
  "Prompt fragment for :op patch (unified diff, single file)."
  (concat
   "PATCH (unified diff, single file):\n"
   "#+begin_patch (:version \"1\" :op \"patch\" :strip 1 :description \"Короткое описание\")\n"
   "--- a/RELATIVE/PATH\n"
   "+++ b/RELATIVE/PATH\n"
   "@@ -N,M +N,M @@\n"
   "-old\n"
   "+new\n"
   "#+end_patch\n"
   "- Requirements: exactly ONE file (one ---/+++ pair); a/ and b/ paths MUST match; :strip=1 for a/b.\n"
   "- Forbidden: binary patches, rename/copy preludes, multi-file diffs.\n"))

;;; Registration
(carriage-format-register 'patch "1"
                          :parse   #'carriage-parse-diff
                          :prompt-fragment #'carriage-op-patch-prompt-fragment)

(provide 'carriage-op-patch)
;;; carriage-op-patch.el ends here
