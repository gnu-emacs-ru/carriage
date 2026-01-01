;;; carriage-reasoning-fold.el --- Fold begin_reasoning blocks with overlays -*- lexical-binding: t; -*-

;; Fold Org-style reasoning blocks:
;;   #+begin_reasoning
;;   ...
;;   #+end_reasoning
;;
;; Requirements:
;; - All existing reasoning blocks should be folded when carriage-mode is enabled.
;; - Newly streamed reasoning should be folded immediately (as early as possible).
;; - When reasoning is completed (#+end_reasoning inserted), it should remain folded.
;; - No auto-reveal by cursor: blocks stay folded by default.
;; - User toggles visibility only via TAB on the placeholder.
;;
;; Implementation:
;; - Overlay-based fold (invisible + before-string placeholder), similar to carriage-patch-fold.
;; - Supports multiple reasoning blocks per buffer.
;; - Debounced rescan after edits to keep overlay ranges in sync.

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-reasoning-fold nil
  "Overlay-based folding of begin_reasoning blocks."
  :group 'applications
  :prefix "carriage-reasoning-fold-")

(defface carriage-reasoning-fold-placeholder-face
  '((t :inherit shadow :slant italic :height 0.95))
  "Face for the folded reasoning placeholder line."
  :group 'carriage-reasoning-fold)

(defcustom carriage-reasoning-fold-invisible-symbol 'carriage-reasoning-fold
  "Symbol used in `buffer-invisibility-spec' for folded reasoning overlays."
  :type 'symbol
  :group 'carriage-reasoning-fold)


(defvar-local carriage-reasoning-fold--enabled nil
  "Non-nil when reasoning folding is enabled in the current buffer.")

(defvar-local carriage-reasoning-fold--overlays nil
  "List of overlays currently managed by carriage-reasoning-fold in this buffer.")

(defvar-local carriage-reasoning-fold--refresh-timer nil
  "Idle timer used to coalesce rescans after edits.")



(defvar-local carriage-reasoning-fold--hover-active nil
  "When non-nil, hover-mode is active for a reasoning block.
Plist keys:
  :beg   begin position (beginning of #+begin_reasoning line)
  :end   last known end position (end of #+end_reasoning line, or point-max if open).")

(defvar-local carriage-reasoning-fold--hover-inhibit nil
  "Internal guard to avoid re-entrancy in hover post-command handler.")

(defun carriage-reasoning-fold--overlay-at (pos)
  "Return the managed reasoning overlay covering POS, or nil."
  (let ((hit nil))
    (dolist (ov carriage-reasoning-fold--overlays)
      (when (and (overlayp ov)
                 (numberp (overlay-start ov))
                 (numberp (overlay-end ov))
                 (>= pos (overlay-start ov))
                 (<= pos (overlay-end ov)))
        (setq hit ov)))
    hit))

(defun carriage-reasoning-fold--org-hide-at (beg)
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
          ;; Best effort: fold only the body
          (let ((body-beg (progn (forward-line 1) (point)))
                (body-end (save-excursion
                            (when (re-search-forward "^[ \t]*#\\+end_reasoning\\b" nil t)
                              (forward-line -1))
                            (line-end-position))))
            (when (< body-beg body-end)
              (org-fold-region body-beg body-end t))))
         (t nil))))))

(defun carriage-reasoning-fold--org-show-at (beg)
  "Show the Org block at BEG (undo folding) best-effort."
  (when (and (numberp beg) (derived-mode-p 'org-mode))
    (ignore-errors
      (require 'org)
      (save-excursion
        (goto-char beg)
        (cond
         ((fboundp 'org-fold-show-drawer-or-block)
          (org-fold-show-drawer-or-block))
         ;; Fallback: show the body region if org-fold exists
         ((featurep 'org-fold)
          (let ((body-beg (progn (forward-line 1) (point)))
                (body-end (save-excursion
                            (when (re-search-forward "^[ \t]*#\\+end_reasoning\\b" nil t)
                              (forward-line -1))
                            (line-end-position))))
            (when (< body-beg body-end)
              (org-fold-region body-beg body-end nil))))
         ;; Last resort: try to toggle to "shown" state (may still toggle on some versions)
         ((fboundp 'org-hide-block-toggle)
          (org-hide-block-toggle nil))
         (t nil))))))

(defun carriage-reasoning-fold--hover-enter (ov)
  "Enter hover-mode for reasoning overlay OV: remove placeholder and use org-fold.

Important: does NOT reveal block contents. It only removes the placeholder overlay
so the user sees a normal Org-folded block header."
  (when (overlayp ov)
    (let* ((beg (overlay-start ov))
           (cell (and (numberp beg) (carriage-reasoning-fold--parse-block-at beg)))
           (end  (and (listp cell) (plist-get cell :end)))
           (openp (and (listp cell) (plist-get cell :openp))))
      ;; For open/unfinished blocks there may be no #+end_reasoning yet; org-fold is unreliable.
      ;; Keep placeholder overlay until the block is complete.
      (unless openp
        ;; Remove placeholder overlay so begin/end lines can be seen.
        (setq carriage-reasoning-fold--overlays (delq ov carriage-reasoning-fold--overlays))
        (ignore-errors (delete-overlay ov))
        ;; Ensure the block remains folded by Org.
        (carriage-reasoning-fold--org-hide-at beg)
        (setq carriage-reasoning-fold--hover-active (list :beg beg :end (or end (point-max))))))))

(defun carriage-reasoning-fold--hover-exit ()
  "Exit hover-mode if active: show org-folded block, restore placeholder overlay (folded)."
  (when (and (listp carriage-reasoning-fold--hover-active)
             (numberp (plist-get carriage-reasoning-fold--hover-active :beg)))
    (let* ((beg (plist-get carriage-reasoning-fold--hover-active :beg))
           (cell (and (numberp beg) (carriage-reasoning-fold--parse-block-at beg)))
           (end  (and (listp cell) (plist-get cell :end)))
           (openp (and (listp cell) (plist-get cell :openp))))
      ;; Ensure org-fold is not left behind (otherwise placeholder-toggle would never reveal body).
      (carriage-reasoning-fold--org-show-at beg)
      ;; Restore placeholder overlay (folded by default).
      (when (and (numberp beg) (numberp end) (< beg end))
        (carriage-reasoning-fold--make beg end openp)))
    (setq carriage-reasoning-fold--hover-active nil)))

(defun carriage-reasoning-fold--post-command ()
  "Hover behavior: when point enters a placeholder overlay, replace it with org-folded block;
when point leaves that block, restore placeholder overlay immediately."
  (when (and carriage-reasoning-fold--enabled
             (derived-mode-p 'org-mode)
             (not carriage-reasoning-fold--hover-inhibit))
    (let ((carriage-reasoning-fold--hover-inhibit t)
          (pos (point)))
      (condition-case _e
          (let* ((active carriage-reasoning-fold--hover-active)
                 (abeg (and (listp active) (plist-get active :beg)))
                 (aend (and (listp active) (plist-get active :end))))
            (cond
             ;; If we have an active hover block and point left it, exit hover-mode.
             ((and (numberp abeg) (numberp aend)
                   (or (< pos abeg) (> pos aend)))
              (carriage-reasoning-fold--hover-exit)
              ;; After exit, we may have entered another overlay at POS in the same command.
              (let ((ov2 (carriage-reasoning-fold--overlay-at pos)))
                (when (overlayp ov2)
                  (carriage-reasoning-fold--hover-enter ov2))))
             ;; No active hover block: entering an overlay activates hover-mode.
             ((null active)
              (let ((ov (carriage-reasoning-fold--overlay-at pos)))
                (when (overlayp ov)
                  (carriage-reasoning-fold--hover-enter ov)))))
            nil)
        (error nil)))))

(defun carriage-reasoning-fold--parse-block-at (beg)
  "At BEG (beginning of line), return plist (:beg :end :openp) for reasoning block, or nil.
When no end marker is found, END is point-max and OPENP is t."
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search t))
      (when (looking-at "^[ \t]*#\\+begin_reasoning\\b")
        (let ((rb (line-beginning-position)))
          (if (re-search-forward "^[ \t]*#\\+end_reasoning\\b" nil t)
              (list :beg rb :end (line-end-position) :openp nil)
            (list :beg rb :end (point-max) :openp t)))))))

(defun carriage-reasoning-fold--scan ()
  "Scan buffer for reasoning blocks; return list of plists (:beg :end :openp)."
  (let ((acc '())
        (case-fold-search t))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_reasoning\\b" nil t)
          (let* ((b (line-beginning-position))
                 (cell (carriage-reasoning-fold--parse-block-at b)))
            (when (and (listp cell)
                       (numberp (plist-get cell :beg))
                       (numberp (plist-get cell :end))
                       (< (plist-get cell :beg) (plist-get cell :end)))
              (push cell acc)))
          ;; Move forward at least one line to avoid edge loops.
          (forward-line 1))))
    (nreverse acc)))

(defun carriage-reasoning-fold--placeholder (beg end openp)
  "Build placeholder string for a folded reasoning block BEG..END.
OPENP indicates an unfinished (still streaming) block."
  (let* ((arrow (if (display-graphic-p) "▸" ">"))
         (lines (max 1 (count-lines beg end)))
         (state (if openp "streaming…" "done"))
         (txt (format "%s reasoning (%s, %d lines) — TAB: toggle" arrow state lines))
         (s (propertize txt 'face 'carriage-reasoning-fold-placeholder-face)))
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "TAB") #'carriage-reasoning-fold-toggle-at)
      (define-key map [tab] #'carriage-reasoning-fold-toggle-at)
      (add-text-properties 0 (length s)
                           (list 'local-map map
                                 'keymap map
                                 'mouse-face 'highlight
                                 'help-echo "Toggle reasoning visibility (TAB)")
                           s))
    s))

(defun carriage-reasoning-fold--make (beg end openp)
  "Create a folded overlay covering BEG..END for a reasoning block."
  (let ((ov (make-overlay beg end nil nil t))) ;; rear-advance=t so END grows as streaming appends
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'category 'carriage-reasoning-fold)
    (overlay-put ov 'carriage-reasoning-openp openp)
    (overlay-put ov 'carriage-reasoning-revealed nil)
    (overlay-put ov 'before-string (carriage-reasoning-fold--placeholder beg end openp))
    (overlay-put ov 'invisible carriage-reasoning-fold-invisible-symbol)
    (add-to-invisibility-spec carriage-reasoning-fold-invisible-symbol)
    (push ov carriage-reasoning-fold--overlays)
    ov))

(defun carriage-reasoning-fold--clear-overlays ()
  "Delete all managed overlays in current buffer."
  (when (listp carriage-reasoning-fold--overlays)
    (dolist (ov carriage-reasoning-fold--overlays)
      (when (overlayp ov) (delete-overlay ov))))
  (setq carriage-reasoning-fold--overlays nil))

(defun carriage-reasoning-fold--refresh-now ()
  "Rescan buffer and (re)create overlays for reasoning blocks."
  (carriage-reasoning-fold--clear-overlays)
  (when (and carriage-reasoning-fold--enabled
             (derived-mode-p 'org-mode))
    (dolist (cell (carriage-reasoning-fold--scan))
      (let ((beg (plist-get cell :beg))
            (end (plist-get cell :end))
            (openp (plist-get cell :openp)))
        (when (and (numberp beg) (numberp end) (< beg end))
          (carriage-reasoning-fold--make beg end openp)))))
  ;; No auto-reveal by cursor; keep overlays folded after refresh.
  )

(defun carriage-reasoning-fold-refresh-now (&optional buffer)
  "Public: refresh reasoning overlays now in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-reasoning-fold--refresh-now)))

(defun carriage-reasoning-fold--schedule-refresh (&optional delay)
  "Schedule debounced refresh with optional DELAY seconds (default 0.1)."
  (when (timerp carriage-reasoning-fold--refresh-timer)
    (cancel-timer carriage-reasoning-fold--refresh-timer))
  (let* ((d (or delay 0.1))
         (buf (current-buffer)))
    (setq carriage-reasoning-fold--refresh-timer
          (run-at-time d nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (setq carriage-reasoning-fold--refresh-timer nil)
                             (ignore-errors (carriage-reasoning-fold--refresh-now)))))))))

(defun carriage-reasoning-fold--after-change (_beg _end _len)
  "After-change hook to coalesce refresh."
  (when carriage-reasoning-fold--enabled
    (carriage-reasoning-fold--schedule-refresh 0.1)))


(defun carriage-reasoning-fold-toggle-at (pos)
  "Toggle fold state of a reasoning overlay covering POS."
  (interactive "d")
  (let ((hit nil))
    (dolist (ov carriage-reasoning-fold--overlays)
      (when (and (overlayp ov)
                 (>= pos (overlay-start ov))
                 (<= pos (overlay-end ov)))
        (setq hit ov)))
    (when (overlayp hit)
      (if (overlay-get hit 'carriage-reasoning-revealed)
          (progn
            (overlay-put hit 'before-string
                         (carriage-reasoning-fold--placeholder
                          (overlay-start hit) (overlay-end hit)
                          (overlay-get hit 'carriage-reasoning-openp)))
            (overlay-put hit 'invisible carriage-reasoning-fold-invisible-symbol)
            (overlay-put hit 'carriage-reasoning-revealed nil))
        (overlay-put hit 'before-string nil)
        (overlay-put hit 'invisible nil)
        (overlay-put hit 'carriage-reasoning-revealed t)))))

(defun carriage-reasoning-fold-hide-all (&optional buffer)
  "Hide all reasoning overlays in BUFFER (or current), including the one containing point."
  (with-current-buffer (or buffer (current-buffer))
    (when carriage-reasoning-fold--enabled
      (dolist (ov carriage-reasoning-fold--overlays)
        (when (overlayp ov)
          (let ((b (overlay-start ov)) (e (overlay-end ov)))
            (when (and (numberp b) (numberp e))
              (overlay-put ov 'before-string
                           (carriage-reasoning-fold--placeholder
                            b e (overlay-get ov 'carriage-reasoning-openp)))
              (overlay-put ov 'invisible carriage-reasoning-fold-invisible-symbol)
              (overlay-put ov 'carriage-reasoning-revealed nil))))))))

;;;###autoload
(defun carriage-reasoning-fold-enable (&optional buffer)
  "Enable folding of reasoning blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-reasoning-fold--enabled t)
    (setq carriage-reasoning-fold--saved-ignore-invis line-move-ignore-invisible)
    ;; Allow point to move into invisible text so users can enter the folded region.
    (setq-local line-move-ignore-invisible nil)
    (add-hook 'post-command-hook #'carriage-reasoning-fold--post-command nil t)
    (add-hook 'after-change-functions #'carriage-reasoning-fold--after-change nil t)
    (carriage-reasoning-fold--refresh-now)
    t))

;;;###autoload
(defun carriage-reasoning-fold-disable (&optional buffer)
  "Disable folding of reasoning blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-reasoning-fold--enabled nil)
    (remove-hook 'after-change-functions #'carriage-reasoning-fold--after-change t)
    (when (timerp carriage-reasoning-fold--refresh-timer)
      (cancel-timer carriage-reasoning-fold--refresh-timer))
    (setq carriage-reasoning-fold--refresh-timer nil)
    (when (local-variable-p 'line-move-ignore-invisible)
      (setq-local line-move-ignore-invisible carriage-reasoning-fold--saved-ignore-invis))
    (carriage-reasoning-fold--clear-overlays)
    t))

(provide 'carriage-reasoning-fold)
;;; carriage-reasoning-fold.el ends here
