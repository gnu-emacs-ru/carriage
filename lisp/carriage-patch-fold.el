;;; carriage-patch-fold.el --- Fold applied begin_patch blocks with overlays -*- lexical-binding: t; -*-

;; Hide contents of applied #+begin_patch ... #+end_patch blocks (header has :applied t)
;; Show a compact one-line placeholder:
;;   ✓ <file> — <description|result>
;; No auto-reveal by cursor: applied patches stay folded by default.
;; User toggles visibility only via TAB on the placeholder.
;;
;; Integration:
;; - Enabled automatically when carriage-mode is active and
;;   carriage-mode-hide-applied-patches is non-nil.
;; - Can be called manually: (carriage-patch-fold-enable) / (carriage-patch-fold-disable)

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-patch-fold nil
  "Overlay-based folding of applied #+begin_patch blocks."
  :group 'applications
  :prefix "carriage-patch-fold-")

(defface carriage-patch-fold-placeholder-face
  '((t :inherit shadow :slant italic :height 0.95))
  "Face for the folded placeholder line."
  :group 'carriage-patch-fold)

(defcustom carriage-patch-fold-invisible-symbol 'carriage-patch-fold
  "Symbol used in buffer-invisibility-spec for folded patch overlays."
  :type 'symbol :group 'carriage-patch-fold)


(defvar-local carriage-patch-fold--overlays nil
  "List of overlays currently managed by carriage-patch-fold in this buffer.")

(defvar-local carriage-patch-fold--refresh-timer nil
  "Idle timer used to coalesce rescans after edits.")





(defun carriage-patch-fold--org-hide-at (beg)
  "Hide the Org block at BEG using org-fold/org-hide (best-effort)."
  (when (and (numberp beg) (derived-mode-p 'org-mode))
    (ignore-errors
      (require 'org)
      (save-excursion
        (goto-char beg)
        (cond
         ((fboundp 'org-fold-hide-drawer-or-block)
          (org-fold-hide-drawer-or-block t))
         ((fboundp 'org-hide-block-toggle)
          (org-hide-block-toggle t))
         ((featurep 'org-fold)
          (let ((body-beg (progn (forward-line 1) (point)))
                (body-end (save-excursion
                            (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                              (forward-line -1))
                            (line-end-position))))
            (when (< body-beg body-end)
              (org-fold-region body-beg body-end t))))
         (t nil))))))


(defun carriage-patch-fold--org-suppress-placeholders (beg end)
  "Best-effort: keep Org folding but suppress Org's own ellipsis/placeholder overlays.

When we fold applied patches we want *one* visible placeholder (Carriage's overlay).
Org folding may also add its own ellipsis via overlay `before-string'/`display'.
This function removes those presentation strings while keeping invisibility."
  (when (and (numberp beg) (numberp end) (< beg end)
             (derived-mode-p 'org-mode))
    (ignore-errors
      (dolist (ov (overlays-in beg end))
        (when (overlayp ov)
          (let ((inv (overlay-get ov 'invisible))
                (cat (overlay-get ov 'category)))
            (when (or (eq inv 'org-fold)
                      (eq inv 'org-hide-block)
                      (eq cat 'org-fold)
                      (eq cat 'org-hide-block))
              (overlay-put ov 'before-string nil)
              (overlay-put ov 'after-string nil)
              (overlay-put ov 'display nil))))))))



(defun carriage-patch-fold--parse-header-plist (line)
  "Return plist parsed from begin_patch LINE or nil."
  (let ((case-fold-search t))
    (when (string-match "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" line)
      (let* ((sexp (match-string 1 line)))
        (condition-case _e
            (let ((obj (car (read-from-string sexp))))
              (and (listp obj) obj))
          (error nil))))))

(defun carriage-patch-fold--block-bounds-at (start)
  "Given point START at beginning of a begin_patch line, return cons (BEG . END) for the block."
  (save-excursion
    (goto-char start)
    (let ((beg (line-beginning-position))
          (case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
        (cons beg (line-end-position))))))

(defun carriage-patch-fold--scan-applied ()
  "Scan buffer for applied begin_patch blocks; return list of cells:
  ((:beg BEG :end END :plist PLIST) ...)"
  (let ((acc '())
        (case-fold-search t))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
          (let* ((hdr-line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                 (pl (carriage-patch-fold--parse-header-plist hdr-line))
                 (applied (and (listp pl) (plist-get pl :applied))))
            (when applied
              (let* ((b (line-beginning-position))
                     (rg (carriage-patch-fold--block-bounds-at b)))
                (when (consp rg)
                  (push (list :beg (car rg) :end (cdr rg) :plist pl) acc))))))))
    (nreverse acc)))

(defun carriage-patch-fold--toggle-keymap ()
  "Return keymap for toggling applied-patch visibility with TAB."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'carriage-patch-fold-toggle-at)
    (define-key map [tab] #'carriage-patch-fold-toggle-at)
    map))

(defun carriage-patch-fold--placeholder (pl)
  "Build placeholder string from header PL."
  (let* ((tick (if (display-graphic-p) "✓" "OK"))
         (path (or (plist-get pl :path) (plist-get pl :file) "-"))
         (desc (or (plist-get pl :description)
                   (plist-get pl :result)
                   "Applied"))
         (txt (format "%s %s — %s" tick path desc))
         (s (propertize txt 'face 'carriage-patch-fold-placeholder-face)))
    (let ((map (carriage-patch-fold--toggle-keymap)))
      (add-text-properties 0 (length s)
                           (list 'local-map map
                                 'help-echo "Toggle visibility (TAB)")
                           s))
    s))

(defun carriage-patch-fold--make (beg end pl)
  "Create overlay covering BEG..END (inclusive line bounds) with header PL."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'category 'carriage-patch-fold)
    (overlay-put ov 'carriage-patch-plist pl)
    (overlay-put ov 'carriage-patch-revealed nil)
    (overlay-put ov 'carriage-patch-auto-revealed nil)
    (overlay-put ov 'before-string (carriage-patch-fold--placeholder pl))
    (overlay-put ov 'invisible carriage-patch-fold-invisible-symbol)
    (add-to-invisibility-spec carriage-patch-fold-invisible-symbol)
    (push ov carriage-patch-fold--overlays)
    ov))

(defun carriage-patch-fold--clear-overlays ()
  "Delete all managed overlays in current buffer.

Also purges any stale overlays left behind due to older bugs or lost bookkeeping,
to guarantee a single placeholder per applied patch block."
  ;; First, delete overlays we know about.
  (when (listp carriage-patch-fold--overlays)
    (dolist (ov carriage-patch-fold--overlays)
      (when (overlayp ov) (delete-overlay ov))))
  (setq carriage-patch-fold--overlays nil)
  ;; Defensive cleanup: if overlays list was stale/cleared incorrectly, remove all
  ;; patch-fold overlays still present in the buffer.
  (ignore-errors
    (save-restriction
      (widen)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (and (overlayp ov)
                   (overlay-buffer ov)
                   (or (overlay-get ov 'carriage-patch-plist)
                       (eq (overlay-get ov 'category) 'carriage-patch-fold)))
          (delete-overlay ov))))))

(defun carriage-patch-fold--refresh-now ()
  "Rescan buffer and (re)create overlays for applied patches.

Invariant for applied patches (like reasoning blocks):
- The block is folded by Org (org-fold / org-hide-block).
- The entire block is also hidden by Carriage's placeholder overlay for compact display.

To avoid duplicate ellipses/placeholders, we suppress Org's own presentation
strings inside the folded region, leaving only Carriage's placeholder visible."
  (carriage-patch-fold--clear-overlays)
  (when (derived-mode-p 'org-mode)
    (dolist (cell (carriage-patch-fold--scan-applied))
      (let ((beg (plist-get cell :beg))
            (end (plist-get cell :end))
            (pl  (plist-get cell :plist)))
        (when (and (numberp beg) (numberp end) (< beg end))
          (carriage-patch-fold--org-hide-at beg)
          (carriage-patch-fold--org-suppress-placeholders beg end)
          (carriage-patch-fold--make beg end pl))))))

(defun carriage-patch-fold--schedule-refresh (&optional delay)
  "Schedule debounced refresh with optional DELAY seconds (default 0.1)."
  (when (timerp carriage-patch-fold--refresh-timer)
    (cancel-timer carriage-patch-fold--refresh-timer))
  (let* ((d (or delay 0.1))
         (buf (current-buffer)))
    (setq carriage-patch-fold--refresh-timer
          (run-at-time d nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (setq carriage-patch-fold--refresh-timer nil)
                             (ignore-errors (carriage-patch-fold--refresh-now)))))))))

(defun carriage-patch-fold--after-change (_beg _end _len)
  "After-change hook to coalesce refresh."
  (carriage-patch-fold--schedule-refresh 0.1))


;;; Public API

;;;###autoload
(defun carriage-patch-fold-refresh-now (&optional buffer)
  "Public: refresh applied patch overlays now in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-patch-fold--refresh-now)))

(defun carriage-patch-fold-refresh (&optional buffer)
  "Backward-compatible alias for `carriage-patch-fold-refresh-now'."
  (carriage-patch-fold-refresh-now buffer))

(defun carriage-patch-fold-toggle-at (pos)
  "Toggle fold state of an overlay covering POS."
  (interactive "d")
  (let ((hit nil))
    (dolist (ov carriage-patch-fold--overlays)
      (when (and (overlayp ov)
                 (>= pos (overlay-start ov))
                 (<= pos (overlay-end ov)))
        (setq hit ov)))
    (when (overlayp hit)
      (if (overlay-get hit 'carriage-patch-revealed)
          (progn
            ;; Manual fold
            (overlay-put hit 'carriage-patch-auto-revealed nil)
            (overlay-put hit 'before-string (carriage-patch-fold--placeholder
                                             (overlay-get hit 'carriage-patch-plist)))
            (overlay-put hit 'invisible carriage-patch-fold-invisible-symbol)
            (overlay-put hit 'carriage-patch-revealed nil))
        ;; Manual reveal
        (overlay-put hit 'carriage-patch-auto-revealed nil)
        (overlay-put hit 'before-string nil)
        (overlay-put hit 'invisible nil)
        (overlay-put hit 'carriage-patch-revealed t)))))

;;;###autoload
(defun carriage-patch-fold-enable (&optional buffer)
  "Enable folding of applied patch blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'after-change-functions #'carriage-patch-fold--after-change nil t)
    (carriage-patch-fold--refresh-now)
    t))

;;;###autoload
(defun carriage-patch-fold-disable (&optional buffer)
  "Disable folding of applied patch blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (remove-hook 'after-change-functions #'carriage-patch-fold--after-change t)
    (when (timerp carriage-patch-fold--refresh-timer)
      (cancel-timer carriage-patch-fold--refresh-timer))
    (setq carriage-patch-fold--refresh-timer nil)
    (carriage-patch-fold--clear-overlays)
    t))

;; Auto-enable for carriage-mode when user preferences demand.
(with-eval-after-load 'carriage-mode
  (add-hook 'carriage-mode-hook
            (lambda ()
              (when (and (boundp 'carriage-mode-hide-applied-patches)
                         carriage-mode-hide-applied-patches)
                (ignore-errors (carriage-patch-fold-enable))))))

(provide 'carriage-patch-fold)
;;; carriage-patch-fold.el ends here
