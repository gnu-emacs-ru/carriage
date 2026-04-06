;;; carriage-context-collect.el --- Document context collection for context  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Document context (#+begin_context) collection and patched files detection.
;; Extracted from carriage-context.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom carriage-doc-context-scope 'all
  "Scope for document (#+begin_context) collection: 'all or 'last."
  :type '(choice (const all) (const last))
  :group 'carriage-context)
(make-variable-buffer-local 'carriage-doc-context-scope)

(defcustom carriage-mode-include-patched-files nil
  "When non-nil, include files referenced by #+begin_patch blocks in the current buffer into the document context."
  :type 'boolean
  :group 'carriage-context)
(make-variable-buffer-local 'carriage-mode-include-patched-files)

(defconst carriage-context--re-begin-context-line
  "^[ \t]*#\\+begin_context[ \t]*$"
  "Regexp matching a *directive-only* begin_context marker line.")

(defconst carriage-context--re-end-context-line
  "^[ \t]*#\\+end_context[ \t]*$"
  "Regexp matching a *directive-only* end_context marker line.")

(defvar-local carriage-context--doc-paths-cache nil
  "Cache of doc-context paths for the current buffer.")

(defvar-local carriage-context--doc-paths-dirty t
  "When non-nil, doc-context paths cache must be recomputed for the current buffer.")

(defvar-local carriage-context--patched-files-cache nil
  "Cache of patched-files paths (from applied begin_patch blocks) for the current buffer.")

(defvar-local carriage-context--patched-files-dirty t
  "When non-nil, patched-files cache must be recomputed for the current buffer.")

(defun carriage-context--pos-in-doc-context-block-p (pos)
  "Return non-nil when POS is inside a #+begin_context...#+end_context block."
  (when (number-or-marker-p pos)
    (save-excursion
      (let ((case-fold-search t))
        (goto-char pos)
        (let ((b (re-search-backward carriage-context--re-begin-context-line nil t))
              (e (re-search-backward carriage-context--re-end-context-line nil t)))
          (and b (or (null e) (> b e))))))))

(defun carriage-context--doc-paths-mark-dirty (beg end _len)
  "Mark doc-paths cache dirty if edit touches begin/end_context lines or occurs inside a begin_context block."
  (when (and (number-or-marker-p beg) (number-or-marker-p end))
    (save-excursion
      (let ((case-fold-search t)
            (hit nil))
        (goto-char beg)
        (beginning-of-line)
        (setq hit (or (looking-at-p carriage-context--re-begin-context-line)
                      (looking-at-p carriage-context--re-end-context-line)
                      (carriage-context--pos-in-doc-context-block-p (point))))
        (unless hit
          (goto-char end)
          (beginning-of-line)
          (setq hit (or (looking-at-p carriage-context--re-begin-context-line)
                        (looking-at-p carriage-context--re-end-context-line)
                        (carriage-context--pos-in-doc-context-block-p (point)))))
        (when hit
          (setq carriage-context--doc-paths-dirty t))))))

(defun carriage-context--patched-files-mark-dirty (beg end _len)
  "Mark patched-files cache dirty if edit touches begin_patch/end_patch lines."
  (when (and (number-or-marker-p beg) (number-or-marker-p end))
    (save-excursion
      (let ((case-fold-search t)
            (hit nil))
        (goto-char beg)
        (beginning-of-line)
        (setq hit (or (looking-at-p "^[ \t]*#\\+begin_patch\\b")
                      (looking-at-p "^[ \t]*#\\+end_patch\\b")))
        (unless hit
          (goto-char end)
          (beginning-of-line)
          (setq hit (or (looking-at-p "^[ \t]*#\\+begin_patch\\b")
                        (looking-at-p "^[ \t]*#\\+end_patch\\b"))))
        (when hit
          (setq carriage-context--patched-files-dirty t)))))

(defun carriage-context--doc-blocks-in-region (beg end)
  "Parse begin_context blocks within BEG..END and return plist:
  (:paths PATHS :warnings WARNS :blocks N :unterminated M)."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((paths '())
            (warns '())
            (blocks 0)
            (unterminated 0))
        (while (re-search-forward carriage-context--re-begin-context-line nil t)
          (setq blocks (1+ blocks))
          (let ((b (match-beginning 0)))
            (if (re-search-forward carriage-context--re-end-context-line nil t)
                (let ((e (match-beginning 0)))
                  (goto-char b)
                  (forward-line 1)
                  (while (< (point) e)
                    (when (looking-at "^[ \t]*#\\+context:[ \t]*\\(.*\\)$")
                      (push (match-string 1) paths))
                    (forward-line 1)))
              (setq unterminated (1+ unterminated))
              (push (format "unterminated block at %s" b) warns))))
        (list :paths (nreverse paths)
              :warnings warns
              :blocks blocks
              :unterminated unterminated)))))

(defun carriage-context--doc-warnings (buffer)
  "Return warnings for #+begin_context blocks in BUFFER."
  (let ((state (carriage-context--doc-blocks-in-region (point-min) (point-max))))
    (plist-get state :warnings)))

(defun carriage-context--doc-paths (buffer)
  "Return list of file paths from #+begin_context blocks in BUFFER."
  (with-current-buffer buffer
    (if (and (not carriage-context--doc-paths-dirty)
             (listp carriage-context--doc-paths-cache)
             (listp (plist-get carriage-context--doc-paths-cache :paths))
             (eq (plist-get carriage-context--doc-paths-cache :scope)
                 carriage-doc-context-scope))
        (plist-get carriage-context--doc-paths-cache :paths)
      (let* ((scope carriage-doc-context-scope)
             (all-paths '())
             (case-fold-search t))
        (goto-char (point-min))
        (while (re-search-forward carriage-context--re-begin-context-line nil t)
          (let* ((beg (match-beginning 0))
                 (end (if (re-search-forward carriage-context--re-end-context-line nil t)
                          (match-beginning 0)
                        (point-max)))
                 (block (carriage-context--doc-blocks-in-region beg end)))
            (setq all-paths (append all-paths (plist-get block :paths)))
            (when (eq scope 'last)
              (goto-char (point-max)))))
        (setq all-paths (delete-dups (delq nil all-paths)))
        (setq carriage-context--doc-paths-cache (list :paths all-paths :scope scope))
        (setq carriage-context--doc-paths-dirty nil)
        all-paths))))

;;;###autoload
(defun carriage-toggle-include-patched-files ()
  "Toggle inclusion of files referenced by applied begin_patch blocks for this buffer."
  (interactive)
  (setq-local carriage-mode-include-patched-files (not carriage-mode-include-patched-files))
  (when (fboundp 'carriage-ui--reset-context-cache)
    (carriage-ui--reset-context-cache))
  (when (fboundp 'carriage-ui--invalidate-ml-cache)
    (carriage-ui--invalidate-ml-cache))
  (force-mode-line-update))

(defun carriage-context--patched-files (buffer)
  "Return list of file paths referenced by #+begin_patch headers in BUFFER."
  (with-current-buffer buffer
    (if (and (not carriage-context--patched-files-dirty)
             (listp carriage-context--patched-files-cache)
             (listp (plist-get carriage-context--patched-files-cache :paths)))
        (plist-get carriage-context--patched-files-cache :paths)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t)
              (acc '()))
          (while (re-search-forward "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" nil t)
            (let* ((sexp-str (match-string 1))
                   (plist (condition-case _e
                              (car (read-from-string sexp-str))
                            (error nil)))
                   (op (and (listp plist) (plist-get plist :op)))
                   (opstr (and op (replace-regexp-in-string "^:" "" (format "%s" op)))))
              (when (listp plist)
                (pcase opstr
                  ("patch"
                   (let ((p (plist-get plist :path)))
                     (when (and (stringp p) (not (string-empty-p p)))
                       (push p acc))))
                  ("rename"
                   (dolist (k '(:from :to))
                     (let ((p (plist-get plist k)))
                       (when (and (stringp p) (not (string-empty-p p)))
                         (push p acc)))))
                  (_
                   (let ((p (plist-get plist :file)))
                     (when (and (stringp p) (not (string-empty-p p)))
                       (push p acc))))))))
          (setq acc (nreverse (delete-dups acc)))
          (setq carriage-context--patched-files-cache (list :paths acc))
          (setq carriage-context--patched-files-dirty nil)
          acc)))))

(provide 'carriage-context-collect)
;;; carriage-context-collect.el ends here
