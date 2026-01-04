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

(defvar-local carriage-patch-fold--saved-ignore-invis nil
  "Saved value of `line-move-ignore-invisible' to restore on disable.")

(defvar-local carriage-patch-fold--hover-active nil
  "When non-nil, hover-mode is active for an applied patch block.
Plist keys:
  :beg begin position (beginning of #+begin_patch line)
  :end end position (end of #+end_patch line)
  :pl  parsed header plist used to restore the placeholder overlay.")

(defvar-local carriage-patch-fold--hover-inhibit nil
  "Internal guard to avoid re-entrancy in hover post-command handler.")

(defun carriage-patch-fold--overlay-at (pos)
  "Return the managed applied-patch overlay covering POS, or nil."
  (let ((hit nil))
    (dolist (ov carriage-patch-fold--overlays)
      (when (and (overlayp ov)
                 (numberp (overlay-start ov))
                 (numberp (overlay-end ov))
                 (>= pos (overlay-start ov))
                 (<= pos (overlay-end ov)))
        (setq hit ov)))
    hit))

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

(defun carriage-patch-fold--org-show-at (beg)
  "Show the Org block at BEG (undo folding) best-effort."
  (when (and (numberp beg) (derived-mode-p 'org-mode))
    (ignore-errors
      (require 'org)
      (save-excursion
        (goto-char beg)
        (cond
         ((fboundp 'org-fold-show-drawer-or-block)
          (org-fold-show-drawer-or-block))
         ((featurep 'org-fold)
          (let ((body-beg (progn (forward-line 1) (point)))
                (body-end (save-excursion
                            (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                              (forward-line -1))
                            (line-end-position))))
            (when (< body-beg body-end)
              (org-fold-region body-beg body-end nil))))
         ((fboundp 'org-hide-block-toggle)
          (org-hide-block-toggle nil))
         (t nil))))))

(defun carriage-patch-fold--hover-enter (ov)
  "Enter hover-mode for applied patch overlay OV: remove placeholder and use org-fold."
  (when (overlayp ov)
    (let* ((beg (overlay-start ov))
           (pl  (overlay-get ov 'carriage-patch-plist))
           (rg  (and (numberp beg) (carriage-patch-fold--block-bounds-at beg)))
           (end (and (consp rg) (cdr rg))))
      (setq carriage-patch-fold--overlays (delq ov carriage-patch-fold--overlays))
      (ignore-errors (delete-overlay ov))
      (carriage-patch-fold--org-hide-at beg)
      (setq carriage-patch-fold--hover-active (list :beg beg :end (or end beg) :pl pl)))))

(defun carriage-patch-fold--hover-exit ()
  "Exit hover-mode if active: restore placeholder overlay immediately.

Important: do NOT keep org-fold under our placeholder overlay.

Keeping org-fold overlays inside our `invisible' overlay may render a second
ellipsis/placeholder (looks like a duplicate) because org-fold uses overlays
with before-string/display inside the hidden region."
  (when (and (listp carriage-patch-fold--hover-active)
             (numberp (plist-get carriage-patch-fold--hover-active :beg)))
    (let* ((beg (plist-get carriage-patch-fold--hover-active :beg))
           (pl  (plist-get carriage-patch-fold--hover-active :pl))
           (rg  (and (numberp beg) (carriage-patch-fold--block-bounds-at beg)))
           (end (and (consp rg) (cdr rg))))
      ;; Remove org-fold created for hover-mode to avoid double placeholders.
      (carriage-patch-fold--org-show-at beg)
      (when (and (numberp beg) (numberp end) (< beg end) (listp pl))
        (carriage-patch-fold--make beg end pl)))
    (setq carriage-patch-fold--hover-active nil)))

(defun carriage-patch-fold--post-command ()
  "Hover behavior: when point enters a placeholder overlay, replace it with org-folded block;
when point leaves that block, restore placeholder overlay immediately."
  (when (and (derived-mode-p 'org-mode)
             (not carriage-patch-fold--hover-inhibit))
    (let ((carriage-patch-fold--hover-inhibit t)
          (pos (point)))
      (condition-case _e
          (let* ((active carriage-patch-fold--hover-active)
                 (abeg (and (listp active) (plist-get active :beg)))
                 (aend (and (listp active) (plist-get active :end))))
            (cond
             ;; Active hover block and point left it → exit
             ((and (numberp abeg) (numberp aend)
                   (or (< pos abeg) (> pos aend)))
              (carriage-patch-fold--hover-exit)
              ;; After exit, maybe immediately enter another overlay
              (let ((ov2 (carriage-patch-fold--overlay-at pos)))
                (when (overlayp ov2)
                  (carriage-patch-fold--hover-enter ov2))))
             ;; No active hover block → entering overlay activates hover
             ((null active)
              (let ((ov (carriage-patch-fold--overlay-at pos)))
                (when (overlayp ov)
                  (carriage-patch-fold--hover-enter ov)))))
            nil)
        (error nil)))))

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
         (txt (format "%s %s — %s" tick path desc))
         (s (propertize txt 'face 'carriage-patch-fold-placeholder-face)))
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "TAB") #'carriage-patch-fold-toggle-at)
      (define-key map [tab] #'carriage-patch-fold-toggle-at)
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
  "Rescan buffer and (re)create overlays for applied patches.

Avoid duplicate placeholders:
- When hover-mode is active for a block, do NOT recreate the placeholder overlay
  for that same block (otherwise we get: placeholder overlay + org-fold ellipsis).
- Before creating a placeholder overlay for other blocks, best-effort remove any
  org-fold at their begin line."
  (carriage-patch-fold--clear-overlays)
  (when (derived-mode-p 'org-mode)
    (let* ((hover-beg (and (listp carriage-patch-fold--hover-active)
                           (plist-get carriage-patch-fold--hover-active :beg))))
      (dolist (cell (carriage-patch-fold--scan-applied))
        (let ((beg (plist-get cell :beg))
              (end (plist-get cell :end))
              (pl  (plist-get cell :plist)))
          (when (and (numberp beg) (numberp end) (< beg end))
            (unless (and (numberp hover-beg) (= beg hover-beg))
              (carriage-patch-fold--org-show-at beg)
              (carriage-patch-fold--make beg end pl))))))))

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
    ;; Let point move into invisible text so user can land on the placeholder and press TAB.
    (setq-local line-move-ignore-invisible nil)
    (add-hook 'post-command-hook #'carriage-patch-fold--post-command nil t)
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
