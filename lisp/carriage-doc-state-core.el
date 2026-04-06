;;; carriage-doc-state-core.el --- Core read/write for document state  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Core read/write functions for CARRIAGE_STATE property.
;; Extracted from carriage-doc-state.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom carriage-doc-state--property-key "CARRIAGE_STATE"
  "Property key used for storing Carriage state in Org buffers."
  :type 'string
  :group 'carriage-doc-state)

(defcustom carriage-doc-state-save-on-save t
  "When non-nil, normalize and save CARRIAGE_STATE on buffer save."
  :type 'boolean
  :group 'carriage-doc-state)

(defvar-local carriage-doc-state--save-hook-installed nil
  "Non-nil when before-save hook is installed for this buffer.")

(defun carriage-doc-state--begin-block-line-p ()
  "Non-nil if point is on a #+begin_ line."
  (looking-at-p "^[ \t]*#\\+begin_"))

(defun carriage-doc-state--header-insertion-point ()
  "Return position for inserting canonical CARRIAGE_STATE line.
Searches backward for last #+title or first #+begin_src, or uses point-min."
  (save-excursion
    (goto-char (point-min))
    (let ((title nil)
          (first-block nil))
      (while (re-search-forward "^[ \t]*#\\+\\([^:]+\\)" nil t)
        (cond
         ((looking-at "title:")
          (setq title (match-beginning 0)))
         ((and (not first-block)
               (looking-at "begin_"))
          (setq first-block (match-beginning 0)))))
      (or title first-block (point-min)))))

(defun carriage-doc-state--normalize-state (state)
  "Normalize STATE (plist or alist) into canonical plist form."
  (cond
   ((null state) '())
   ((listp state)
    (if (keywordp (car state))
        state
      (cl-loop for (k . v) in state
               collect (if (keywordp k) k (intern (format ":%s" k)))
               collect v)))
   (t '())))

(defun carriage-doc-state--sexp-read (s)
  "Read STATE string S into Emacs Lisp, returning plist or nil."
  (when (and (stringp s) (not (string-empty-p s)))
    (ignore-errors
      (car (read-from-string s)))))

(defun carriage-doc-state--find-state-lines ()
  "Find all CARRIAGE_STATE property lines in buffer, return list of (BEG . END)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (lines '()))
        (while (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b" nil t)
          (push (cons (match-beginning 0) (line-end-position)) lines))
        (nreverse lines)))))

(defun carriage-doc-state--remove-state-lines ()
  "Remove all CARRIAGE_STATE property lines from current buffer."
  (let ((inhibit-read-only t))
    (dolist (rng (carriage-doc-state--find-state-lines))
      (delete-region (car rng) (cdr rng)))))

;;;###autoload
(defun carriage-doc-state-read (&optional buffer)
  "Read CARRIAGE_STATE from BUFFER (or current buffer) and return a plist."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b\\(.*\\)$" nil t)
            (let* ((tail (string-trim (or (match-string 1) "")))
                   (val (if (string-match-p "^[ \t]*$" tail)
                            ""
                          (string-trim-left tail)))
                   (sexp (carriage-doc-state--sexp-read val)))
              (when sexp
                (carriage-doc-state--normalize-state sexp)))))))))

(defun carriage-doc-state--upsert-state-line (plist)
  "Upsert exactly one canonical CARRIAGE_STATE line using PLIST."
  (let* ((inhibit-read-only t)
         (sexp (prin1-to-string (or plist '())))
         (line (format "#+PROPERTY: %s %s\n" carriage-doc-state--property-key sexp)))
    (carriage-doc-state--remove-state-lines)
    (let ((ip (carriage-doc-state--header-insertion-point)))
      (goto-char ip)
      (unless (bolp) (insert "\n"))
      (insert line))
    t))

;;;###autoload
(defun carriage-doc-state-write (state &optional buffer)
  "Write STATE into BUFFER as canonical CARRIAGE_STATE property line."
  (with-current-buffer (or buffer (current-buffer))
    (condition-case _e
        (save-excursion
          (save-restriction
            (widen)
            (let ((pl (carriage-doc-state--normalize-state state)))
              (carriage-doc-state--upsert-state-line pl)
              (ignore-errors
                (when (and (boundp 'carriage-doc-state-summary-enable)
                           carriage-doc-state-summary-enable
                           (fboundp 'carriage-doc-state-summary-refresh))
                  (carriage-doc-state-summary-enable)
                  (carriage-doc-state-summary-refresh (current-buffer))))
              t)))
      (error nil))))

(defun carriage-doc-state--collect-var (sym)
  "Return value of SYM if bound, otherwise nil."
  (when (boundp sym) (symbol-value sym)))

(defun carriage-doc-state--collect-current ()
  "Collect current buffer-local Carriage state as a plist (:CAR_* ...)."
  (condition-case _e
      (let ((pl '()))
        (cl-labels
            ((put (k v) (setq pl (plist-put pl k v)))
             (b (sym) (carriage-doc-state--collect-var sym)))
          (put :CAR_MODE (and (boundp 'carriage-mode) (bound-and-true-p carriage-mode)))
          (put :CAR_INTENT  (b 'carriage-mode-intent))
          (put :CAR_SUITE   (b 'carriage-mode-suite))
          (put :CAR_MODEL   (b 'carriage-mode-model))
          (put :CAR_BACKEND (b 'carriage-mode-backend))
          (put :CAR_PROVIDER (b 'carriage-mode-provider))
          (put :CAR_CTX_GPTEL   (b 'carriage-mode-include-gptel-context))
          (put :CAR_CTX_DOC     (b 'carriage-mode-include-doc-context))
          (put :CAR_CTX_VISIBLE (b 'carriage-mode-include-visible-context))
          (put :CAR_CTX_MAP     (b 'carriage-mode-include-project-map))
          (put :CAR_CTX_PATCHED (b 'carriage-mode-include-patched-files))
          (put :CAR_CTX_MAX_FILES (b 'carriage-mode-context-max-files))
          (put :CAR_CTX_MAX_BYTES (b 'carriage-mode-context-max-total-bytes))
          (put :CAR_STRUCTURE_HINT (b 'carriage-mode-org-structure-hint))
          (put :CAR_TYPEDBLOCKS   (b 'carriage-mode-typedblocks-structure-hint))
          pl))
    (error nil)))

;;;###autoload
(defun carriage-doc-state-write-current (&optional buffer)
  "Write current buffer-local Carriage state to BUFFER."
  (interactive)
  (carriage-doc-state-write (carriage-doc-state--collect-current) buffer))

(defun carriage-doc-state--apply-if-bound (var val)
  "Set buffer-local VAR to VAL if VAR is bound."
  (when (and (boundp var) (not (eq var 'carriage-mode)))
    (set (intern (symbol-name var)) val)))

(defun carriage-doc-state--as-symbol (v)
  "Normalize V into a symbol when possible."
  (cond
   ((symbolp v) v)
   ((stringp v) (intern v))
   (t nil)))

(defun carriage-doc-state--as-string (v)
  "Normalize V into a string."
  (cond
   ((null v) "")
   ((stringp v) v)
   (t (format "%s" v))))

(defun carriage-doc-state--bool (v)
  "Normalize V to boolean."
  (cond
   ((eq v t) t)
   ((eq v nil) nil)
   ((stringp v) (not (member (downcase v) '("" "nil" "false" "no"))))
   (t t)))

;;;###autoload
(defun carriage-doc-state-restore (&optional buffer)
  "Restore buffer-local Carriage state from CARRIAGE_STATE in BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((pl (ignore-errors (carriage-doc-state-read (current-buffer)))))
      (when (listp pl)
        (cl-dolist (cell (plist-member pl :CAR_MODE))
          (carriage-doc-state--apply-if-bound (car cell) (cdr cell))))
        pl))))

;;;###autoload
(defun carriage-doc-state-auto-enable (&optional buffer)
  "Auto-enable `carriage-mode' in BUFFER if CARRIAGE_STATE requests it."
  (with-current-buffer (or buffer (current-buffer))
    (let ((pl (ignore-errors (carriage-doc-state-read (current-buffer)))))
      (when (and (listp pl)
                 (plist-member pl :CAR_MODE)
                 (carriage-doc-state--bool (plist-get pl :CAR_MODE))
                 (fboundp 'carriage-mode)
                 (boundp 'carriage-mode)
                 (not (bound-and-true-p carriage-mode)))
        (ignore-errors (carriage-mode 1))))))

(provide 'carriage-doc-state-core)
;;; carriage-doc-state-core.el ends here
