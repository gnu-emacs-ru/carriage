;;; carriage-parser.el --- Parse patch blocks and build plan  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: parser, ops
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/parser-impl-v2.org
;;   spec/parser-registry-v2.org
;;   spec/sre-v2.org
;;   spec/patch-unified-diff-v2.org
;;   spec/file-ops-v2.org
;;
;;; Commentary:
;; Block parser and plan builder for different :op formats.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-utils)
(require 'carriage-logging)
(require 'carriage-format-registry)

;; Forward declaration: buffer-local id of the "last iteration".
(defvar carriage--last-iteration-id nil
  "Identifier of the last iteration in the current buffer (if any).")

;; Fallback when Customize not loaded: limit for SRE pairs (see spec/index.org FREEZE)
(defvar carriage-mode-max-batch-pairs 200
  "Fallback maximum number of pairs allowed in an SRE block when Customize is not loaded.")

(defun carriage-parse (op header-plist body-text repo-root)
  "Dispatch parse via registry by OP for HEADER-PLIST and BODY-TEXT under REPO-ROOT.

Strict v1 behavior:
- Алиасы :op не поддерживаются (MODE_E_DISPATCH).
- Нет автокоррекции :strip; валидация в парсерах.
- :path не принимается вместо :file (никаких скрытых преобразований).

Если обработчик не зарегистрирован, выполняется ленивый load нужного ops-модуля и повторная попытка."
  (cl-block carriage-parse
    (let* ((op-sym0 (if (symbolp op) op (intern (format "%s" op))))
           (op-sym (progn
                     (pcase op-sym0
                       ((or 'replace 'diff 'write 'create_file 'delete_file 'rename_file)
                        (signal (carriage-error-symbol 'MODE_E_DISPATCH)
                                (list (format "Alias op not supported in v1: %S" op-sym0))))
                       (_ nil))
                     op-sym0))
           (hdr1 header-plist)
           (rec (and (fboundp 'carriage-format-get) (carriage-format-get op-sym "1")))
           (fn  (and rec (plist-get rec :parse))))
      (unless (functionp fn)
        ;; Лениво грузим ops-модуль и повторяем поиск
        (pcase op-sym
          ('sre                  (load "ops/carriage-op-sre" t t))
          ('aibo                 (load "ops/carriage-op-aibo" t t))
          ('patch                (load "ops/carriage-op-patch" t t))
          ((or 'create 'delete 'rename) (load "ops/carriage-op-file" t t))
          (_ nil))
        (setq rec (and (fboundp 'carriage-format-get) (carriage-format-get op-sym "1"))
              fn  (and rec (plist-get rec :parse))))
      (if (functionp fn)
          (funcall fn hdr1 body-text repo-root)
        (signal (carriage-error-symbol 'MODE_E_DISPATCH)
                (list (format "Unknown op or unregistered handler: %S" op-sym)))))))

;;;; Ops-specific parsing moved to ops modules (lisp/ops/*).
;;;; This file now contains only registry dispatch and Org scanning helpers.

;;;; Org buffer helpers

(defun carriage--bounds-of-patch-block-at-point ()
  "Return (BEG . END) bounds of the current #+begin_patch ... #+end_patch block.
Move point is not changed. Return nil if not found."
  (save-excursion
    (let ((beg nil) (end nil))
      ;; Find begin (scan backward by literal search, verify bol whitespace)
      (save-excursion
        (while (and (not beg)
                    (search-backward "#+begin_patch" nil t))
          (let* ((p (point))
                 (bol (line-beginning-position))
                 (ok (save-excursion
                       (goto-char bol)
                       (skip-chars-forward " \t")
                       (let* ((here (point)))
                         (and (<= here p)
                              (string-prefix-p "#+begin_patch"
                                               (buffer-substring-no-properties here (min (+ here 13) (line-end-position)))))))))
            (when ok (setq beg bol)))))
      ;; Find end (scan forward, verify bol whitespace)
      (save-excursion
        (while (and (not end)
                    (search-forward "#+end_patch" nil t))
          (let* ((p (- (point) (length "#+end_patch")))
                 (bol (save-excursion (goto-char p) (line-beginning-position)))
                 (ok (save-excursion
                       (goto-char bol)
                       (skip-chars-forward " \t")
                       (let* ((here (point)))
                         (and (<= here p)
                              (string-prefix-p "#+end_patch"
                                               (buffer-substring-no-properties here (min (+ here 11) (line-end-position)))))))))
            (when ok (setq end bol)))))
      (when (and (number-or-marker-p beg) (number-or-marker-p end) (> end beg))
        (cons beg end)))))


(defun carriage--read-patch-header-at (pos)
  "Parse patch header plist at line around POS. Return plist or signal error."
  (save-excursion
    (goto-char pos)
    (let* ((beg (line-beginning-position))
           (end (line-end-position))
           (line (buffer-substring-no-properties beg end)))
      (require 'cl-lib)
      (let* ((kw "#+begin_patch")
             (i (cl-search kw line :test #'char-equal)))
        (unless (and i)
          (signal (carriage-error-symbol 'MODE_E_DISPATCH) (list "Invalid begin_patch header")))
        (let* ((j (+ i (length kw))))
          ;; skip spaces/tabs
          (while (and (< j (length line))
                      (memq (aref line j) '(?\s ?\t)))
            (setq j (1+ j)))
          (unless (and (< j (length line)) (eq (aref line j) ?\())
            (signal (carriage-error-symbol 'MODE_E_DISPATCH) (list "Missing header plist")))
          ;; find matching closing ')': use last ')' on line
          (let* ((k (cl-position ?\) line :from-end t)))
            (unless (and k (>= k j))
              (signal (carriage-error-symbol 'MODE_E_DISPATCH) (list "Unclosed header plist")))
            (let* ((hdr-str (substring line j (1+ k)))
                   (parsed (car (read-from-string hdr-str))))
              parsed)))))))

(defun carriage-parse-block-at-point (repo-root)
  "Parse current org patch block at point into a plan item under REPO-ROOT."
  (let* ((bounds (carriage--bounds-of-patch-block-at-point)))
    (unless bounds
      (signal (carriage-error-symbol 'MODE_E_DISPATCH) (list "No patch block at point")))
    (let* ((beg (car bounds))
           (end-bol (cdr bounds))
           (end-eol (save-excursion (goto-char end-bol) (line-end-position)))
           (header-plist (save-excursion
                           (goto-char beg)
                           (carriage--read-patch-header-at beg)))
           (body (save-excursion
                   (goto-char beg)
                   (forward-line 1)
                   (let ((body-beg (point)))
                     (goto-char end-bol)
                     ;; end-bol points to beginning of #+end_patch line
                     (buffer-substring-no-properties body-beg (line-beginning-position)))))
           (plan (carriage-parse (plist-get header-plist :op) header-plist body repo-root)))
      ;; Attach buffer and live markers for later replacement (# +patch_done)
      (let ((mb (copy-marker beg))
            (me (copy-marker end-eol)))
        (if (listp plan)
            (append plan (list (cons :_buffer (current-buffer))
                               (cons :_beg-marker mb)
                               (cons :_end-marker me)))
          plan)))))

;;;; Region/group parsing

(defun carriage-parse-blocks-in-region (beg end repo-root)
  "Parse all #+begin_patch blocks between BEG and END into a PLAN under REPO-ROOT.
Return a list of plan items in buffer order."
  (save-excursion
    (goto-char beg)
    (let ((plan '()))
      (while (and (< (point) end)
                  (search-forward "#+begin_patch" end t))
        ;; Verify marker at BOL with optional spaces
        (let* ((p (- (point) (length "#+begin_patch")))
               (line-beg (save-excursion (goto-char p) (line-beginning-position)))
               (ok (save-excursion
                     (goto-char line-beg)
                     (skip-chars-forward " \t")
                     (let ((here (point)))
                       (and (<= here p)
                            (string-prefix-p "#+begin_patch"
                                             (buffer-substring-no-properties here (min (+ here 13) (line-end-position)))))))))
          (unless ok
            (forward-line 1)
            (cl-return-from carriage-parse-blocks-in-region
              (append (carriage-parse-blocks-in-region (point) end repo-root) plan)))
          (let* ((start line-beg)
                 (body-beg (save-excursion (goto-char start) (forward-line 1) (point)))
                 (header-plist (carriage--read-patch-header-at start))
                 (block-end
                  (save-excursion
                    (goto-char body-beg)
                    (let ((found nil))
                      (while (and (not found)
                                  (search-forward "#+end_patch" end t))
                        (let* ((q (- (point) (length "#+end_patch")))
                               (bol (save-excursion (goto-char q) (line-beginning-position)))
                               (ok2 (save-excursion
                                      (goto-char bol)
                                      (skip-chars-forward " \t")
                                      (let ((here (point)))
                                        (and (<= here q)
                                             (string-prefix-p "#+end_patch"
                                                              (buffer-substring-no-properties here (min (+ here 11) (line-end-position)))))))))
                          (when ok2 (setq found (line-beginning-position)))))
                      (unless found
                        (signal (carriage-error-symbol 'SRE_E_UNCLOSED_BLOCK)
                                (list "Unclosed #+begin_patch block")))
                      found)))
                 (body (buffer-substring-no-properties body-beg block-end))
                 (op (plist-get header-plist :op)))
            ;; Diagnostics (best-effort, simplified to avoid read-time issues)
            (ignore-errors
              (carriage-log "group-parse: op=%s file=%s"
                            op
                            (plist-get header-plist :file)))
            (let* ((it (carriage-parse op header-plist body repo-root))
                   (mb (copy-marker start))
                   (me (save-excursion (goto-char block-end) (line-end-position)))
                   (it2 (append it (list (cons :_buffer (current-buffer))
                                         (cons :_beg-marker mb)
                                         (cons :_end-marker me)))))
              (push it2 plan))
            (goto-char block-end)
            (forward-line 1))))
      (nreverse plan))))

(defun carriage-collect-last-iteration-blocks (&optional repo-root)
  "Collect blocks of the last iteration in current buffer and parse to a PLAN.
If the buffer-local variable =carriage--last-iteration-id' is set, collect only blocks
annotated with that id (text property =carriage-iteration-id' on the #+begin_patch line).
Otherwise, return all patch blocks in the buffer.

If REPO-ROOT is nil, detect via =carriage-project-root' or use =default-directory'."
  (let* ((root (or repo-root (carriage-project-root) default-directory))
         (id   (and (boundp 'carriage--last-iteration-id) carriage--last-iteration-id)))
    (carriage-log "collect-last-iteration root=%s id=%s"
                  (or root "<nil>")
                  (and id (substring id 0 (min 8 (length id)))))
    (if (not id)
        (carriage-parse-blocks-in-region (point-min) (point-max) root)
      (save-excursion
        (goto-char (point-min))
        (let ((plan '()))
          (while (search-forward "#+begin_patch" nil t)
            (let* ((p (- (point) (length "#+begin_patch")))
                   (line-beg (save-excursion (goto-char p) (line-beginning-position)))
                   (ok (save-excursion
                         (goto-char line-beg)
                         (skip-chars-forward " \t")
                         (let ((here (point)))
                           (and (<= here p)
                                (string-prefix-p "#+begin_patch"
                                                 (buffer-substring-no-properties here (min (+ here 13) (line-end-position))))))))
                   (prop (and ok (get-text-property line-beg 'carriage-iteration-id))))
              (when ok
                (let* ((body-beg (save-excursion (goto-char line-beg) (forward-line 1) (point)))
                       (header-plist (carriage--read-patch-header-at line-beg))
                       (block-end
                        (save-excursion
                          (goto-char body-beg)
                          (let ((found nil))
                            (while (and (not found)
                                        (search-forward "#+end_patch" nil t))
                              (let* ((q (- (point) (length "#+end_patch")))
                                     (bol (save-excursion (goto-char q) (line-beginning-position)))
                                     (ok2 (save-excursion
                                            (goto-char bol)
                                            (skip-chars-forward " \t")
                                            (let ((here (point)))
                                              (and (<= here q)
                                                   (string-prefix-p "#+end_patch"
                                                                    (buffer-substring-no-properties here (min (+ here 11) (line-end-position)))))))))
                                (when ok2 (setq found (line-beginning-position)))))
                            (unless found
                              (signal (carriage-error-symbol 'SRE_E_UNCLOSED_BLOCK)
                                      (list "Unclosed #+begin_patch block")))
                            found)))
                       (after (save-excursion (goto-char block-end) (forward-line 1) (point))))
                  (carriage-log "iter-collect: begin@%d prop=%s id=%s match=%s"
                                line-beg prop id (if (equal prop id) "yes" "no"))
                  (when (equal prop id)
                    (let* ((body (buffer-substring-no-properties body-beg block-end))
                           (op (plist-get header-plist :op)))
                      (let* ((it (carriage-parse op header-plist body root))
                             (mb (copy-marker line-beg))
                             (me (save-excursion (goto-char block-end) (line-end-position)))
                             (it2 (append it (list (cons :_buffer (current-buffer))
                                                   (cons :_beg-marker mb)
                                                   (cons :_end-marker me)))))
                        (push it2 plan))
                      (carriage-log "iter-collect: pushed op=%s file=%s"
                                    op (plist-get header-plist :file))))
                  (goto-char after)
                  (carriage-log "iter-collect: advanced to %d (after end_patch)" (point))))))
          (setq plan (nreverse plan))
          (carriage-log "iter-collect done matched=%d" (length plan))
          (if plan
              plan
            (carriage-parse-blocks-in-region (point-min) (point-max) root)))))))


(defun carriage-collect-last-iteration-blocks-strict (&optional repo-root)
  "Collect blocks of the last iteration strictly; return nil when no id is present.

This strict collector relies only on:
- buffer-local variable `carriage--last-iteration-id'
- text property `carriage-iteration-id' on the #+begin_patch line

It NEVER falls back to collecting all blocks in the buffer."
  (let* ((root (or repo-root (carriage-project-root) default-directory))
         (id   (and (boundp 'carriage--last-iteration-id) carriage--last-iteration-id)))
    (carriage-log "collect-last-iteration STRICT root=%s id=%s"
                  (or root "<nil>")
                  (and id (substring id 0 (min 8 (length id)))))
    (unless id
      (carriage-log "no last-iteration id; strict collector returns nil")
      (cl-return-from carriage-collect-last-iteration-blocks-strict nil))
    (save-excursion
      (goto-char (point-min))
      (let ((plan '()))
        (while (search-forward "#+begin_patch" nil t)
          (let* ((p (- (point) (length "#+begin_patch")))
                 (line-beg (save-excursion (goto-char p) (line-beginning-position)))
                 (ok (save-excursion
                       (goto-char line-beg)
                       (skip-chars-forward " \t")
                       (let ((here (point)))
                         (and (<= here p)
                              (string-prefix-p "#+begin_patch"
                                               (buffer-substring-no-properties
                                                here
                                                (min (+ here 13) (line-end-position))))))))
                 (prop (and ok (get-text-property line-beg 'carriage-iteration-id))))
            (when ok
              (let* ((body-beg (save-excursion (goto-char line-beg) (forward-line 1) (point)))
                     (header-plist (carriage--read-patch-header-at line-beg))
                     (block-end
                      (save-excursion
                        (goto-char body-beg)
                        (let ((found nil))
                          (while (and (not found)
                                      (search-forward "#+end_patch" nil t))
                            (let* ((q (- (point) (length "#+end_patch")))
                                   (bol (save-excursion (goto-char q) (line-beginning-position)))
                                   (ok2 (save-excursion
                                          (goto-char bol)
                                          (skip-chars-forward " \t")
                                          (let ((here (point)))
                                            (and (<= here q)
                                                 (string-prefix-p "#+end_patch"
                                                                  (buffer-substring-no-properties
                                                                   here
                                                                   (min (+ here 11) (line-end-position)))))))))
                              (when ok2 (setq found (line-beginning-position)))))
                          (unless found
                            (signal (carriage-error-symbol 'SRE_E_UNCLOSED_BLOCK)
                                    (list "Unclosed #+begin_patch block")))
                          found)))
                     (after (save-excursion (goto-char block-end) (forward-line 1) (point))))
                (when (equal prop id)
                  (let* ((body (buffer-substring-no-properties body-beg block-end))
                         (op (plist-get header-plist :op))
                         (it (carriage-parse op header-plist body root))
                         (mb (copy-marker line-beg))
                         (me (save-excursion (goto-char block-end) (line-end-position)))
                         (it2 (append it (list (cons :_buffer (current-buffer))
                                               (cons :_beg-marker mb)
                                               (cons :_end-marker me)))))
                    (push it2 plan)))
                (goto-char after)))))
        (nreverse plan)))))

(provide 'carriage-parser)
;;; carriage-parser.el ends here
