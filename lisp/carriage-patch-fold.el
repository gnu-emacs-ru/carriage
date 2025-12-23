;;; carriage-patch-fold.el --- Fold applied begin_patch blocks with overlays -*- lexical-binding: t; -*-

;; Hide contents of applied #+begin_patch ... #+end_patch blocks (header has :applied t)
;; Show a compact one-line placeholder:
;;   ✓ <file> — <description|result>
;; Auto-reveal original text when cursor enters the block; auto-hide when cursor leaves.
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

(defvar-local carriage-patch-fold--saved-ignore-invis nil
  "Saved value of `line-move-ignore-invisible' to restore on disable.")

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

(defun carriage-patch-fold--placeholder (pl)
  "Build placeholder string from header PL."
  (let* ((tick (if (display-graphic-p) "✓" "OK"))
         (path (or (plist-get pl :path) (plist-get pl :file) "-"))
         (desc (or (plist-get pl :description)
                   (plist-get pl :result)
                   "Applied"))
         (txt (format "%s %s — %s  (mouse-1: toggle)" tick path desc))
         (s (propertize txt 'face 'carriage-patch-fold-placeholder-face)))
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1]
        (lambda () (interactive)
          (carriage-patch-fold-toggle-at (point))))
      (add-text-properties 0 (length s)
                           (list 'local-map map
                                 'mouse-face 'mode-line-highlight
                                 'help-echo "Toggle visibility")
                           s))
    s))

(defun carriage-patch-fold--make (beg end pl)
  "Create overlay covering BEG..END (inclusive line bounds) with header PL."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'category 'carriage-patch-fold)
    (overlay-put ov 'carriage-patch-plist pl)
    (overlay-put ov 'carriage-patch-revealed nil)
    (overlay-put ov 'before-string (carriage-patch-fold--placeholder pl))
    (overlay-put ov 'invisible carriage-patch-fold-invisible-symbol)
    (add-to-invisibility-spec carriage-patch-fold-invisible-symbol)
    (push ov carriage-patch-fold--overlays)
    ov))

(defun carriage-patch-fold--clear-overlays ()
  "Delete all managed overlays in current buffer."
  (when (listp carriage-patch-fold--overlays)
    (dolist (ov carriage-patch-fold--overlays)
      (when (overlayp ov) (delete-overlay ov))))
  (setq carriage-patch-fold--overlays nil))

(defun carriage-patch-fold--refresh-now ()
  "Rescan buffer and (re)create overlays for applied patches."
  (carriage-patch-fold--clear-overlays)
  (when (derived-mode-p 'org-mode)
    (dolist (cell (carriage-patch-fold--scan-applied))
      (let ((beg (plist-get cell :beg))
            (end (plist-get cell :end))
            (pl  (plist-get cell :plist)))
        (when (and (numberp beg) (numberp end) (< beg end))
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

(defun carriage-patch-fold--cursor-ensure-visibility ()
  "Reveal when point enters an overlay; hide when it leaves."
  (let ((pos (point)))
    (dolist (ov carriage-patch-fold--overlays)
      (when (overlayp ov)
        (let ((b (overlay-start ov))
              (e (overlay-end ov)))
          (when (and (numberp b) (numberp e))
            (if (and (>= pos b) (<= pos e))
                (progn
                  (overlay-put ov 'before-string nil)
                  (overlay-put ov 'invisible nil)
                  (overlay-put ov 'carriage-patch-revealed t))
              (overlay-put ov 'before-string (carriage-patch-fold--placeholder
                                              (overlay-get ov 'carriage-patch-plist)))
              (overlay-put ov 'invisible carriage-patch-fold-invisible-symbol)
              (overlay-put ov 'carriage-patch-revealed nil))))))))

;;; Public API

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
            (overlay-put hit 'before-string (carriage-patch-fold--placeholder
                                             (overlay-get hit 'carriage-patch-plist)))
            (overlay-put hit 'invisible carriage-patch-fold-invisible-symbol)
            (overlay-put hit 'carriage-patch-revealed nil))
        (overlay-put hit 'before-string nil)
        (overlay-put hit 'invisible nil)
        (overlay-put hit 'carriage-patch-revealed t)))))

;;;###autoload
(defun carriage-patch-fold-enable (&optional buffer)
  "Enable folding of applied patch blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-patch-fold--saved-ignore-invis line-move-ignore-invisible)
    ;; Let point move into invisible text so our reveal logic can trigger.
    (setq-local line-move-ignore-invisible nil)
    (add-hook 'after-change-functions #'carriage-patch-fold--after-change nil t)
    (add-hook 'post-command-hook #'carriage-patch-fold--cursor-ensure-visibility nil t)
    (carriage-patch-fold--refresh-now)
    t))

;;;###autoload
(defun carriage-patch-fold-disable (&optional buffer)
  "Disable folding of applied patch blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (remove-hook 'after-change-functions #'carriage-patch-fold--after-change t)
    (remove-hook 'post-command-hook #'carriage-patch-fold--cursor-ensure-visibility t)
    (when (timerp carriage-patch-fold--refresh-timer)
      (cancel-timer carriage-patch-fold--refresh-timer))
    (setq carriage-patch-fold--refresh-timer nil)
    (when (local-variable-p 'line-move-ignore-invisible)
      (setq-local line-move-ignore-invisible carriage-patch-fold--saved-ignore-invis))
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
