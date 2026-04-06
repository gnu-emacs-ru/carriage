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

(defvar-local carriage-reasoning-fold--active-overlay nil
  "Currently revealed reasoning overlay under point, or nil.")

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
        (let ((end nil))
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
           (t nil))
          ;; Org may leave its own ellipsis/placeholder overlays behind after reveal.
          ;; Purge them in the block range so hover reveal shows the full block body.
          (save-excursion
            (goto-char beg)
            (when (re-search-forward "^[ \t]*#\\+end_reasoning\\b" nil t)
              (setq end (line-end-position))
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
                      (overlay-put ov 'display nil)
                      (overlay-put ov 'invisible nil))))))))))))

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
         (txt (format "%s reasoning (%s, %d lines)" arrow state lines))
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
  (let* ((ov (make-overlay beg end nil nil t)) ;; rear-advance=t so END grows as streaming appends
         (ph (carriage-reasoning-fold--placeholder beg end openp)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'category 'carriage-reasoning-fold)
    ;; Marker property for external reveal hooks (carriage-mode).
    (overlay-put ov 'carriage-reasoning-fold t)

    (overlay-put ov 'carriage-reasoning-openp openp)
    (overlay-put ov 'carriage-reasoning-revealed nil)

    ;; Keep a stable placeholder and "saved" state so carriage-mode can restore fold
    ;; even if this overlay is revealed by a background refresh during streaming.
    (overlay-put ov 'carriage-reasoning-placeholder ph)
    (overlay-put ov 'carriage--saved-invisible carriage-reasoning-fold-invisible-symbol)
    (overlay-put ov 'carriage--saved-before-string ph)
    (overlay-put ov 'carriage--saved-display nil)

    (overlay-put ov 'before-string ph)
    (overlay-put ov 'invisible carriage-reasoning-fold-invisible-symbol)
    (add-to-invisibility-spec carriage-reasoning-fold-invisible-symbol)
    ;; Ensure cursor can enter the folded region (reveal hooks rely on it).
    (overlay-put ov 'intangible nil)
    (overlay-put ov 'cursor-intangible nil)
    (push ov carriage-reasoning-fold--overlays)
    ov))

(defun carriage-reasoning-fold--clear-overlays ()
  "Delete all managed overlays in current buffer.

Also purges any stale overlays left behind due to older bugs or lost bookkeeping,
to guarantee a single placeholder per reasoning block."
  ;; First, delete overlays we know about.
  (when (listp carriage-reasoning-fold--overlays)
    (dolist (ov carriage-reasoning-fold--overlays)
      (when (overlayp ov) (delete-overlay ov))))
  (setq carriage-reasoning-fold--overlays nil)
  (setq carriage-reasoning-fold--active-overlay nil)
  ;; Defensive cleanup: if overlays list was stale/cleared incorrectly, remove all
  ;; reasoning-fold overlays still present in the buffer.
  (ignore-errors
    (save-restriction
      (widen)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (and (overlayp ov)
                   (overlay-buffer ov)
                   (or (overlay-get ov 'carriage-reasoning-fold)
                       (overlay-get ov 'carriage-reasoning)
                       (eq (overlay-get ov 'category) 'carriage-reasoning-fold)))
          (delete-overlay ov))))))

(defun carriage-reasoning-fold--point-inside-ov-p (ov)
  "Return non-nil when point is inside OV.
Treat overlay end as inside too, to keep reveal stable at EOL."
  (when (overlayp ov)
    (let ((beg (overlay-start ov))
          (end (overlay-end ov))
          (pt (point)))
      (and (number-or-marker-p beg)
           (number-or-marker-p end)
           (<= beg pt)
           (<= pt end)))))

(defun carriage-reasoning-fold--refresh-now ()
  "Rescan buffer and (re)create overlays for reasoning blocks.

Hover reveal is preserved across refreshes for the currently active overlay only,
which keeps cursor-driven behavior stable while avoiding full overlay churn per command."
  (let ((pt (point))
        (reveal-range nil))
    (when (overlayp carriage-reasoning-fold--active-overlay)
      (setq reveal-range
            (cons (overlay-start carriage-reasoning-fold--active-overlay)
                  (overlay-end carriage-reasoning-fold--active-overlay))))
    (carriage-reasoning-fold--clear-overlays)
    (when (and carriage-reasoning-fold--enabled
               (derived-mode-p 'org-mode))
      (dolist (cell (carriage-reasoning-fold--scan))
        (let ((beg (plist-get cell :beg))
              (end (plist-get cell :end))
              (openp (plist-get cell :openp)))
          (when (and (numberp beg) (numberp end) (< beg end))
            (let ((ov (carriage-reasoning-fold--make beg end openp)))
              (when (and (consp reveal-range)
                         (= beg (car reveal-range))
                         (= end (cdr reveal-range))
                         (<= beg pt) (<= pt end))
                (carriage-reasoning-fold--org-show-at beg)
                (overlay-put ov 'before-string nil)
                (overlay-put ov 'invisible nil)
                (overlay-put ov 'carriage-reasoning-revealed t)
                (setq carriage-reasoning-fold--active-overlay ov)))))))))

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

(defun carriage-reasoning-fold--post-command ()
  "Reveal reasoning overlay under point; fold it back after point leaves.
Touches only one active overlay per command to minimize interference with vertical motion."
  (when (and carriage-reasoning-fold--enabled
             (derived-mode-p 'org-mode)
             (get-buffer-window (current-buffer) t))
    (let ((active carriage-reasoning-fold--active-overlay))
      ;; Fold previously active overlay when point left it.
      (when (overlayp active)
        (if (carriage-reasoning-fold--point-inside-ov-p active)
            nil
          (carriage-reasoning-fold--org-hide-at (overlay-start active))
          (overlay-put active 'before-string
                       (carriage-reasoning-fold--placeholder
                        (overlay-start active)
                        (overlay-end active)
                        (overlay-get active 'carriage-reasoning-openp)))
          (overlay-put active 'invisible carriage-reasoning-fold-invisible-symbol)
          (overlay-put active 'carriage-reasoning-revealed nil)
          (setq carriage-reasoning-fold--active-overlay nil)))

      ;; Reveal current overlay exactly under point.
      (unless (overlayp carriage-reasoning-fold--active-overlay)
        (let ((pt (point))
              (hit nil))
          (dolist (ov (overlays-at pt))
            (when (and (overlayp ov)
                       (eq (overlay-get ov 'category) 'carriage-reasoning-fold)
                       (carriage-reasoning-fold--point-inside-ov-p ov))
              (setq hit ov)))
          (unless hit
            (let ((lo (max (point-min) (1- pt)))
                  (hi (min (point-max) (1+ pt))))
              (dolist (ov (overlays-in lo hi))
                (when (and (overlayp ov)
                           (eq (overlay-get ov 'category) 'carriage-reasoning-fold)
                           (carriage-reasoning-fold--point-inside-ov-p ov))
                  (setq hit ov)))))
          (when (overlayp hit)
            (carriage-reasoning-fold--org-show-at (overlay-start hit))
            (overlay-put hit 'before-string nil)
            (overlay-put hit 'invisible nil)
            (overlay-put hit 'carriage-reasoning-revealed t)
            (setq carriage-reasoning-fold--active-overlay hit)))))))

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
            (carriage-reasoning-fold--org-hide-at (overlay-start hit))
            (overlay-put hit 'before-string
                         (carriage-reasoning-fold--placeholder
                          (overlay-start hit) (overlay-end hit)
                          (overlay-get hit 'carriage-reasoning-openp)))
            (overlay-put hit 'invisible carriage-reasoning-fold-invisible-symbol)
            (overlay-put hit 'carriage-reasoning-revealed nil))
        (carriage-reasoning-fold--org-show-at (overlay-start hit))
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
    (setq carriage-reasoning-fold--active-overlay nil)
    (add-hook 'after-change-functions #'carriage-reasoning-fold--after-change nil t)
    (add-hook 'post-command-hook #'carriage-reasoning-fold--post-command nil t)
    (when (derived-mode-p 'org-mode)
      (dolist (cell (carriage-reasoning-fold--scan))
        (let ((beg (plist-get cell :beg))
              (end (plist-get cell :end)))
          (when (and (numberp beg) (numberp end) (< beg end))
            (carriage-reasoning-fold--org-hide-at beg)))))
    (carriage-reasoning-fold--refresh-now)
    t))

;;;###autoload
(defun carriage-reasoning-fold-disable (&optional buffer)
  "Disable folding of reasoning blocks in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-reasoning-fold--enabled nil)
    (setq carriage-reasoning-fold--active-overlay nil)
    (remove-hook 'after-change-functions #'carriage-reasoning-fold--after-change t)
    (remove-hook 'post-command-hook #'carriage-reasoning-fold--post-command t)
    (when (timerp carriage-reasoning-fold--refresh-timer)
      (cancel-timer carriage-reasoning-fold--refresh-timer))
    (setq carriage-reasoning-fold--refresh-timer nil)
    (carriage-reasoning-fold--clear-overlays)
    t))

;; Internal helper: check if overlay is a reasoning fold overlay
(defun carriage-reasoning-fold--overlay-p (ov)
  "Return non-nil when OV looks like a reasoning-fold overlay."
  (and (overlayp ov)
       (overlay-buffer ov)
       (or (overlay-get ov 'carriage-reasoning-fold)
           (overlay-get ov 'carriage-reasoning)
           (eq (overlay-get ov 'category) 'carriage-reasoning-fold))
       (not (eq (overlay-get ov 'category) 'carriage-doc-state-fold))
       (not (overlay-get ov 'carriage-fold-summary))
       (not (overlay-get ov 'carriage-fold-tooltip))))

;; Internal helper: sanitize display property
(defun carriage-reasoning-fold--sanitize-display (disp)
  "Return DISP with noisy hints removed (e.g., \"(TAB: toggle …)\")."
  (if (not (stringp disp))
      disp
    (let* ((s disp))
      (setq s (replace-regexp-in-string
               "(\\s-*\\(TAB\\|Tab\\):\\s-*toggle\\([^)]*\\))" "" s t))
      (setq s (replace-regexp-in-string
               "\\s-*\\(TAB\\|Tab\\):\\s-*toggle\\b.*\\'" "" s t))
      (setq s (replace-regexp-in-string "[ \t][ \t]+" " " s t t))
      (setq s (string-trim s))
      (if (string-empty-p s) "…" s))))

;; Internal helper: reveal overlay
(defun carriage-reasoning-fold--reveal (ov)
  "Reveal OV by removing hiding properties, saving them for restoration."
  (when (overlayp ov)
    (unless (overlay-get ov 'carriage--saved-invisible)
      (overlay-put ov 'carriage--saved-invisible (overlay-get ov 'invisible)))
    (unless (overlay-get ov 'carriage--saved-display)
      (overlay-put ov 'carriage--saved-display (overlay-get ov 'display)))
    (unless (overlay-get ov 'carriage--saved-before-string)
      (overlay-put ov 'carriage--saved-before-string (overlay-get ov 'before-string)))
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'display nil)
    (overlay-put ov 'before-string nil)
    (overlay-put ov 'intangible nil)
    (overlay-put ov 'cursor-intangible nil)))

;; Internal helper: restore overlay
(defun carriage-reasoning-fold--restore (ov)
  "Restore OV hiding properties if previously saved."
  (when (overlayp ov)
    (when (overlay-get ov 'carriage--saved-invisible)
      (overlay-put ov 'invisible (overlay-get ov 'carriage--saved-invisible)))
    (when (overlay-get ov 'carriage--saved-display)
      (overlay-put ov 'display (carriage-reasoning-fold--sanitize-display
                               (overlay-get ov 'carriage--saved-display))))
    (when (overlay-get ov 'carriage--saved-before-string)
      (overlay-put ov 'before-string (carriage-reasoning-fold--sanitize-display
                                      (overlay-get ov 'carriage--saved-before-string))))
    (overlay-put ov 'intangible nil)
    (overlay-put ov 'cursor-intangible nil)))

(defvar-local carriage-reasoning-fold--revealed-ovs nil
  "Internal list of reasoning fold overlays currently revealed due to point being inside.")

;; Public: sanitize all reasoning fold overlays
(defun carriage-reasoning-fold-sanitize-all (&optional buffer)
  "Best-effort sanitize all reasoning-fold overlays in BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (carriage-reasoning-fold--overlay-p ov)
        (ignore-errors
          (let ((d (overlay-get ov 'display)))
            (when d
              (overlay-put ov 'display (carriage-reasoning-fold--sanitize-display d))))
          (let ((b (overlay-get ov 'before-string)))
            (when b
              (overlay-put ov 'before-string (carriage-reasoning-fold--sanitize-display b))))
          (overlay-put ov 'intangible nil)
          (overlay-put ov 'cursor-intangible nil))))
    t))

;; Public: post-command hook to reveal folded reasoning block at point
(defun carriage-reasoning-fold-post-command ()
  "Post-command hook to reveal folded reasoning block at point and refold on leave."
  (condition-case _e
      (let* ((pt (point))
             (lo (max (point-min) (1- pt)))
             (hi (min (point-max) (1+ pt)))
             (near (cl-remove-if-not #'carriage-reasoning-fold--overlay-p
                                     (overlays-in lo hi)))
             (cur  (cl-remove-if-not #'carriage-reasoning-fold--overlay-p
                                     (overlays-at pt)))
             (prev (or carriage-reasoning-fold--revealed-ovs '())))
         (dolist (ov near)
           (ignore-errors
             (overlay-put ov 'intangible nil)
             (overlay-put ov 'cursor-intangible nil)
             (let ((d (overlay-get ov 'display))
                   (b (overlay-get ov 'before-string)))
               (when d (overlay-put ov 'display (carriage-reasoning-fold--sanitize-display d)))
               (when b (overlay-put ov 'before-string (carriage-reasoning-fold--sanitize-display b))))))
         (dolist (ov prev)
           (unless (memq ov cur)
             (ignore-errors (carriage-reasoning-fold--restore ov))))
         (dolist (ov cur)
           (unless (memq ov prev)
             (ignore-errors (carriage-reasoning-fold--reveal ov))))
         (setq carriage-reasoning-fold--revealed-ovs cur))
    (error nil)))

(provide 'carriage-reasoning-fold)
;;; carriage-reasoning-fold.el ends here
