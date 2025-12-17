;;; carriage-block-fold.el --- Reusable overlay-based folding for begin_<kind> blocks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Carriage team
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: org, folding, overlays, convenience
;;
;;; Commentary:
;;
;; Generic, reusable overlay-based folding for Org-style blocks:
;;   #+begin_<kind>
;;   ...
;;   #+end_<kind>
;;
;; Features:
;; - No use of org-fold; blocks never auto-unfold by isearch or fragile logic.
;; - Hidden using an overlay with a compact summary (before-string).
;; - Auto-reveal on cursor enter; auto-hide on cursor leave.
;; - Idempotent ensure; lightweight refresh after edits.
;; - Extensible to multiple kinds via `carriage-block-fold-kinds'.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-block-fold nil
  "Overlay-based folding for begin_<kind> blocks."
  :group 'applications
  :prefix "carriage-block-fold-")

(defface carriage-block-fold-summary-face
  '((t :inherit shadow :slant italic :height 0.9))
  "Face for the one-line placeholder of folded begin_<kind> blocks."
  :group 'carriage-block-fold)

(defcustom carriage-block-fold-kinds nil
  "Alist of (KIND . ENABLED) for overlay-based folding of begin_<kind> blocks.

Example:
  ((context . t) (reasoning . nil))

No kinds are enabled by default. Enable kinds explicitly when needed."
  :type '(alist :key-type (choice (const carriage) (symbol))
                :value-type boolean)
  :group 'carriage-block-fold)

(defcustom carriage-block-fold-invisible-symbol 'carriage-doc-state
  "Symbol used in `buffer-invisibility-spec' for folded overlays.
Defaults to 'carriage-doc-state to integrate with doc-state toggles."
  :type 'symbol
  :group 'carriage-block-fold)

(defvar-local carriage-block-fold--overlays nil
  "Alist of (KIND . OVERLAY) for folded begin_<kind> blocks in this buffer.")

(defvar-local carriage-block-fold--overlay-refresh-timer nil
  "Idle timer used to coalesce overlay refresh after edits.")

(defun carriage-block-fold--block-range-of (kind)
  "Return (BEG . END) inclusive line bounds of the begin_<KIND>…end_<KIND> block, or nil."
  (let* ((name (symbol-name kind)))
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let* ((case-fold-search t)
                 (rb (and (re-search-forward (format "^[ \t]*#\\+begin_%s\\b" (regexp-quote name)) nil t)
                          (line-beginning-position)))
                 (re (and rb (progn
                               (when (re-search-forward (format "^[ \t]*#\\+end_%s\\b" (regexp-quote name)) nil t)
                                 (line-end-position))))))
            (when (and rb re (> re rb)) (cons rb re))))))))

(defun carriage-block-fold--summary-string (kind beg end)
  "Return a one-line summary string for a folded block KIND covering BEG..END."
  (let* ((arrow (if (display-graphic-p) "▸" ">"))
         (nm (format "begin_%s" (symbol-name kind)))
         (lines (max 1 (count-lines beg end)))
         (txt (format "%s %s (%d lines) — mouse-1: toggle" arrow nm lines))
         (s (propertize txt 'face 'carriage-block-fold-summary-face)))
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1]
        (lambda () (interactive)
          (let ((ov (alist-get kind carriage-block-fold--overlays)))
            (if (and (overlayp ov) (overlay-get ov 'invisible))
                (carriage-block-fold-reveal kind)
              (carriage-block-fold-hide kind)))))
      (add-text-properties
       0 (length s)
       (list 'local-map map
             'mouse-face 'mode-line-highlight
             'help-echo (format "Toggle %s block visibility" nm))
       s))
    s))

(defun carriage-block-fold--ensure-overlay (kind)
  "Idempotently ensure an overlay hiding begin_<KIND>…end_<KIND> with a summary line.
Returns the overlay or nil if block is absent."
  (let* ((rg (carriage-block-fold--block-range-of kind)))
    (when rg
      (let* ((beg (car rg)) (end (cdr rg))
             (ov (alist-get kind carriage-block-fold--overlays)))
        (cond
         ((and (overlayp ov)
               (= (overlay-start ov) beg)
               (= (overlay-end   ov) end))
          (unless (overlay-get ov 'carriage-block-revealed)
            (overlay-put ov 'before-string (carriage-block-fold--summary-string kind beg end))
            (overlay-put ov 'invisible carriage-block-fold-invisible-symbol))
          (unless (member carriage-block-fold-invisible-symbol buffer-invisibility-spec)
            (add-to-invisibility-spec carriage-block-fold-invisible-symbol))
          ov)
         (t
          (when (overlayp ov) (delete-overlay ov))
          (let ((new (make-overlay beg end)))
            (overlay-put new 'evaporate t)
            (overlay-put new 'category 'carriage-block-fold)
            (overlay-put new 'carriage-block-kind kind)
            (overlay-put new 'carriage-block-revealed nil)
            (overlay-put new 'before-string (carriage-block-fold--summary-string kind beg end))
            (overlay-put new 'invisible carriage-block-fold-invisible-symbol)
            (add-to-invisibility-spec carriage-block-fold-invisible-symbol)
            (setf (alist-get kind carriage-block-fold--overlays) new)
            new)))))))

(defun carriage-block-fold-ensure-overlay (kind &optional buffer)
  "Public wrapper to ensure overlay for KIND in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-block-fold--ensure-overlay kind)))

(defun carriage-block-fold-reveal (kind &optional buffer)
  "Reveal begin_<KIND> block if folded; keeps overlay for later re-hiding."
  (with-current-buffer (or buffer (current-buffer))
    (let ((ov (alist-get kind carriage-block-fold--overlays)))
      (when (overlayp ov)
        (overlay-put ov 'before-string nil)
        (overlay-put ov 'invisible nil)
        (overlay-put ov 'carriage-block-revealed t))
      ov)))

(defun carriage-block-fold-hide (kind &optional buffer)
  "Hide begin_<KIND> block with a placeholder; creates overlay if missing."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((ov (or (alist-get kind carriage-block-fold--overlays)
                   (carriage-block-fold--ensure-overlay kind))))
      (when (overlayp ov)
        (let ((beg (overlay-start ov))
              (end (overlay-end ov)))
          (overlay-put ov 'before-string (carriage-block-fold--summary-string kind beg end))
          (overlay-put ov 'invisible carriage-block-fold-invisible-symbol)
          (overlay-put ov 'carriage-block-revealed nil)
          (unless (member carriage-block-fold-invisible-symbol buffer-invisibility-spec)
            (add-to-invisibility-spec carriage-block-fold-invisible-symbol))))
      ov)))

(defun carriage-block-fold--cursor-ensure-visibility ()
  "Reveal folded blocks when point enters them; hide back when point leaves.
Avoid full rescans on every command: prefer existing overlays and only schedule
a debounced refresh when overlays are missing."
  (when (and (derived-mode-p 'org-mode)
             (listp carriage-block-fold-kinds))
    (dolist (cell carriage-block-fold-kinds)
      (let* ((kind (car cell))
             (enabled (cdr cell)))
        (when enabled
          (let ((ov (alist-get kind carriage-block-fold--overlays)))
            (cond
             ;; Fast path: use existing overlay bounds (O(1))
             ((overlayp ov)
              (let* ((pos (point))
                     (beg (overlay-start ov))
                     (end (overlay-end ov)))
                (when (and (numberp beg) (numberp end))
                  (if (and (>= pos beg) (<= pos end))
                      (carriage-block-fold-reveal kind)
                    (carriage-block-fold-hide kind)))))
             ;; Overlay missing — avoid synchronous rescan; schedule a debounced refresh
             (t
              (carriage-block-fold-schedule-overlay-refresh 0.1)))))))))

(defun carriage-block-fold-install-cursor-watch (&optional buffer)
  "Install buffer-local watcher to auto-reveal folded blocks on cursor enter."
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'post-command-hook #'carriage-block-fold--cursor-ensure-visibility nil t)))

(defun carriage-block-fold--refresh-overlays (&optional kinds)
  "Rescan and (re)ensure overlays for enabled KINDS (or all from defcustom).
Deletes stale overlays when corresponding blocks disappear."
  (when (derived-mode-p 'org-mode)
    (let ((ks (or kinds (mapcar #'car carriage-block-fold-kinds))))
      (dolist (k ks)
        (let* ((enabled (alist-get k carriage-block-fold-kinds))
               (rg (and enabled (carriage-block-fold--block-range-of k)))
               (ov (alist-get k carriage-block-fold--overlays)))
          (cond
           ((and enabled rg)
            (ignore-errors (carriage-block-fold--ensure-overlay k)))
           (ov
            (when (overlayp ov) (delete-overlay ov))
            (setf (alist-get k carriage-block-fold--overlays) nil))))))
    t))

(defun carriage-block-fold-schedule-overlay-refresh (&optional delay buffer)
  "Schedule a near-future overlays refresh (debounced).
Optional DELAY in seconds (default 0.1) and BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (when (timerp carriage-block-fold--overlay-refresh-timer)
      (cancel-timer carriage-block-fold--overlay-refresh-timer))
    (let* ((d (or delay 0.1))
           (buf (current-buffer)))
      (setq carriage-block-fold--overlay-refresh-timer
            (run-at-time d nil
                         (lambda (b)
                           (when (buffer-live-p b)
                             (with-current-buffer b
                               (ignore-errors (carriage-block-fold--refresh-overlays))
                               (setq carriage-block-fold--overlay-refresh-timer nil))))
                         buf)))
    t))

(defun carriage-block-fold--after-change (_beg _end _len)
  "After-change hook to keep block overlays in sync with buffer edits."
  (when (derived-mode-p 'org-mode)
    (carriage-block-fold-schedule-overlay-refresh 0.1)))

(defun carriage-block-fold-install-change-watch (&optional buffer)
  "Install buffer-local after-change watcher to refresh folded overlays."
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'after-change-functions #'carriage-block-fold--after-change nil t)))

(defun carriage-block-fold-remove-change-watch (&optional buffer)
  "Remove buffer-local after-change watcher."
  (with-current-buffer (or buffer (current-buffer))
    (remove-hook 'after-change-functions #'carriage-block-fold--after-change t)))

(defun carriage-block-fold-ensure-for-buffer (&optional buffer)
  "Ensure folded overlays for all enabled kinds in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'org-mode)
      (dolist (cell carriage-block-fold-kinds)
        (when (cdr cell)
          (ignore-errors (carriage-block-fold--ensure-overlay (car cell))))))
    t))

(provide 'carriage-block-fold)
;;; carriage-block-fold.el ends here
