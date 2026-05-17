;;; carriage-context-patches.el --- Applied patch metadata helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Helpers for parsing and summarizing applied #+begin_patch blocks.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar-local carriage-context--applied-patches-cache nil
  "Cache of applied patch metadata for the current buffer.
Plist keys:
  :patches — list of plists (:path PATH :ts TIMESTAMP :op OP)
  :updated — float-time of last update.

This is used to inject applied patch state into context for LLM awareness.")

(defvar-local carriage-context--applied-patches-dirty t
  "When non-nil, applied patches cache must be recomputed.")

(defun carriage-context--applied-patches-mark-dirty (&rest _args)
  "Mark applied patches cache dirty.
Designed to be called after apply operations complete."
  (setq carriage-context--applied-patches-dirty t))

(add-hook 'carriage-mode-hook
          (lambda ()
            ;; Invalidate applied patches cache when patch blocks are modified
            (add-hook 'after-change-functions
                      #'carriage-context--applied-patches-mark-dirty
                      nil t)))

(defun carriage-context--collect-applied-patches (buffer)
  "Collect applied patch metadata from BUFFER.
Returns list of plists: (:path PATH :ts TIMESTAMP :op OP :result RESULT)."
  (with-current-buffer buffer
    (if (and (not carriage-context--applied-patches-dirty)
             (listp carriage-context--applied-patches-cache)
             (listp (plist-get carriage-context--applied-patches-cache :patches)))
        (plist-get carriage-context--applied-patches-cache :patches)
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
                   (applied (and (listp plist) (plist-get plist :applied)))
                   (ts (and (listp plist) (plist-get plist :applied_at)))
                   (result (and (listp plist) (plist-get plist :result))))
              (when (and (listp plist) applied)
                (let ((target
                       (cond
                        ((eq op 'rename)
                         (let ((a (plist-get plist :from))
                               (b (plist-get plist :to)))
                           (format "%s → %s"
                                   (or (and (stringp a) a) "-")
                                   (or (and (stringp b) b) "-"))))
                        ((eq op 'patch) (plist-get plist :path))
                        (t (plist-get plist :file)))))
                  (when (and (stringp target) (not (string-empty-p target)))
                    (push (list :path target
                                :ts (or ts (format-time-string "%Y-%m-%d %H:%M"))
                                :op (symbol-name op)
                                :result (or result "Applied"))
                          acc))))))
          (setq acc (nreverse acc))
          (setq carriage-context--applied-patches-cache (list :patches acc))
          (setq carriage-context--applied-patches-dirty nil)
          acc)))))

(defun carriage-context--format-applied-patches-summary (patches)
  "Format applied patches list into LLM-facing summary string."
  (when (and (listp patches) (> (length patches) 0))
    (let ((lines
           (mapcar
            (lambda (p)
              (format ";; - %s (%s) — %s @%s"
                      (plist-get p :path)
                      (plist-get p :op)
                      (plist-get p :result)
                      (plist-get p :ts)))
            patches)))
      (concat ";; Applied patches in this session:\n"
              (mapconcat #'identity lines "\n")
              "\n"))))

(defun carriage-context--collapse-applied-patches-in-text (text)
  "Return TEXT with applied #+begin_patch blocks replaced by one-line history comments.

Policy:
- For blocks whose header plist contains :applied non-nil:
  - Remove the whole begin_patch…end_patch block (including markers).
  - Insert one comment line:
      ;; applied patch: <target> — <description|result|Applied>
- Non-applied patch blocks remain unchanged.

This preserves history for the LLM while avoiding begin_patch markers that can bias it."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" nil t)
        (let* ((beg-line-beg (line-beginning-position))
               (sexp-str (match-string 1))
               (plist (condition-case _e
                          (car (read-from-string sexp-str))
                        (error nil))))
          (if (and (listp plist) (plist-get plist :applied))
              (let* ((desc (or (plist-get plist :description)
                               (plist-get plist :result)
                               "Applied"))
                     (desc (string-trim (format "%s" desc)))
                     (desc (if (string-empty-p desc) "Applied" desc))
                     (target
                      (cond
                       ((eq (plist-get plist :op) 'rename)
                        (let ((a (plist-get plist :from))
                              (b (plist-get plist :to)))
                          (string-trim
                           (format "%s → %s"
                                   (or (and (stringp a) a) "-")
                                   (or (and (stringp b) b) "-")))))
                       ((stringp (plist-get plist :path)) (plist-get plist :path))
                       ((stringp (plist-get plist :file)) (plist-get plist :file))
                       (t "-")))
                     (summary (format ";; applied patch: %s — %s\n"
                                      (string-trim (format "%s" target))
                                      desc))
                     (block-end
                      (save-excursion
                        (goto-char (line-end-position))
                        (forward-line 1)
                        (if (re-search-forward "^[ \t]*#\\+end_patch\\b.*$" nil t)
                            (min (point-max) (1+ (line-end-position)))
                          (point-max)))))
                (delete-region beg-line-beg block-end)
                (goto-char beg-line-beg)
                (insert summary)
                (goto-char (min (point-max) (+ beg-line-beg (length summary)))))
            ;; Not applied: continue scanning from next line to avoid loops.
            (goto-char (min (point-max) (1+ (line-end-position))))))))
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'carriage-context-patches)
;;; carriage-context-patches.el ends here
