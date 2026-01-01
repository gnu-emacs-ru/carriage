;;; carriage-ui-ctx-fast.el --- Fast (≤1Hz) context count for modeline  -*- lexical-binding: t; -*-

;; Purpose:
;; - Fix Ctx:?/Ctx:0 flicker by providing a single stable computation path
;; - Never blocks redisplay: only schedules recompute, returns cached string
;; - Counts doc-context candidates "from current point" (scope-aware)
;;
;; This module is intentionally self-contained and does NOT read file contents.
;; It may use file existence/attributes at most (currently: existence only).

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-ui-ctx-fast nil
  "Fast context badge computation for Carriage UI."
  :group 'carriage)

(defcustom carriage-ui-ctx-refresh-interval 1.0
  "Minimum seconds between context badge recomputations.
Used to keep the UI responsive while still staying reasonably up-to-date."
  :type 'number
  :group 'carriage-ui-ctx-fast)

(defvar-local carriage-ui-ctx--cache nil
  "Cached propertized modeline segment string for Ctx badge (e.g., \"Ctx:3\").")

(defvar-local carriage-ui-ctx--cache-tooltip nil
  "Cached tooltip string for Ctx badge.")

(defvar-local carriage-ui-ctx--last-update-ts 0.0
  "float-time of the last successful recomputation of the context badge.")

(defvar-local carriage-ui-ctx--pending-timer nil
  "Timer object for a scheduled recomputation, or nil.")

(defvar-local carriage-ui-ctx--last-error nil
  "Last error string observed during recomputation (for tooltip).")

(defun carriage-ui-ctx--now ()
  (float-time))

(defun carriage-ui-ctx--doc-scope ()
  "Return effective doc-context scope for current buffer."
  (or (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
      (and (boundp 'carriage-mode-doc-context-scope) carriage-mode-doc-context-scope)
      'all))

(defun carriage-ui-ctx--doc-enabled-p ()
  "Return non-nil when doc-context is enabled in current buffer."
  (and (boundp 'carriage-mode-include-doc-context)
       carriage-mode-include-doc-context))

(defun carriage-ui-ctx--line->path (line)
  "Parse a candidate path from LINE, or nil.
Ignores empty lines and comments starting with '#'."
  (let* ((s (string-trim (or line ""))))
    (cond
     ((string-empty-p s) nil)
     ((string-prefix-p "#" s) nil)
     (t s))))

(defun carriage-ui-ctx--block-lines-between (beg end)
  "Return list of raw lines between BEG and END (buffer positions), excluding markers."
  (save-excursion
    (goto-char beg)
    (let ((lines '()))
      (while (< (point) end)
        (let ((ln (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
          (push ln lines))
        (forward-line 1))
      (nreverse lines))))

(defun carriage-ui-ctx--doc-lines-from-point (&optional pt scope)
  "Collect doc-context lines (raw strings) from begin_context blocks.
PT defaults to point. SCOPE defaults to `carriage-ui-ctx--doc-scope'.

SCOPE:
- 'all  : all begin_context blocks in buffer
- 'last : the nearest begin_context block above PT (by begin marker position)"
  (let ((pt (or pt (point)))
        (scope (or scope (carriage-ui-ctx--doc-scope))))
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (save-restriction
          (widen)
          (let ((case-fold-search t)
                (rx-beg "^[ \t]*#\\+begin_context\\b")
                (rx-end "^[ \t]*#\\+end_context\\b"))
            (pcase scope
              ('last
               (goto-char (min (max (point-min) pt) (point-max)))
               (when (re-search-backward rx-beg nil t)
                 (let ((body-beg (progn (forward-line 1) (point)))
                       (body-end (and (re-search-forward rx-end nil t)
                                      (match-beginning 0))))
                   (when (and (numberp body-end) (> body-end body-beg))
                     (carriage-ui-ctx--block-lines-between body-beg body-end)))))
              (_
               (goto-char (point-min))
               (let ((acc '()))
                 (while (re-search-forward rx-beg nil t)
                   (let ((body-beg (progn (forward-line 1) (point)))
                         (body-end (and (re-search-forward rx-end nil t)
                                        (match-beginning 0))))
                     (when (and (numberp body-end) (> body-end body-beg))
                       (setq acc (nconc acc (carriage-ui-ctx--block-lines-between body-beg body-end)))))
                   ;; continue searching after end marker (or current line if malformed)
                   (forward-line 1))
                 acc)))))))))

(defun carriage-ui-ctx--normalize-doc-paths (lines)
  "Turn raw context LINES into a list of normalized existing file paths (strings).
This is a cheap approximation of what will be sent: we only count candidates
that exist locally and are not TRAMP."
  (let* ((root (or (and (fboundp 'carriage-project-root) (carriage-project-root))
                   default-directory))
         (out '()))
    (dolist (ln lines)
      (let ((p (carriage-ui-ctx--line->path ln)))
        (when (and (stringp p) (not (string-empty-p p)))
          (condition-case _e
              (let* ((abs (if (file-name-absolute-p p)
                              (expand-file-name p)
                            (if (fboundp 'carriage-normalize-path)
                                (carriage-normalize-path root p)
                              (expand-file-name p root)))))
                (when (and (stringp abs)
                           (not (file-remote-p abs))
                           (file-exists-p abs)
                           (file-regular-p abs))
                  ;; Use truename for stable de-dup (still cheap; no content IO).
                  (push (file-truename abs) out)))
            (error nil)))))
    (delete-dups (nreverse out))))

(defun carriage-ui-ctx--compute-count ()
  "Compute the current context count for the modeline (doc-context only for now).
Returns plist: (:count N :tooltip STRING). Never signals."
  (condition-case e
      (let* ((doc-enabled (carriage-ui-ctx--doc-enabled-p))
             (scope (carriage-ui-ctx--doc-scope))
             (pt (point))
             (doc-lines (if doc-enabled
                            (carriage-ui-ctx--doc-lines-from-point pt scope)
                          '()))
             (doc-files (carriage-ui-ctx--normalize-doc-paths (or doc-lines '())))
             (n (length doc-files))
             (tip (concat
                   (format "Carriage context (fast, ≤%.1fs)\n" (or carriage-ui-ctx-refresh-interval 1.0))
                   (format "doc-context: %s\n" (if doc-enabled "on" "off"))
                   (format "scope: %s\n" scope)
                   (format "candidates: %d\n" n)
                   (when (> n 0)
                     (concat "\nFiles:\n"
                             (mapconcat (lambda (s) (concat " - " s))
                                        (cl-subseq doc-files 0 (min 50 n))
                                        "\n")
                             (when (> n 50) "\n - …"))))))
        (list :count n :tooltip tip))
    (error
     (list :count 0
           :tooltip (format "Carriage context compute error: %s" (error-message-string e))
           :error (error-message-string e)))))

(defun carriage-ui-ctx--set-cache (count tooltip &optional err)
  "Update cache and also best-effort mirror into carriage-ui internal vars if present."
  (let* ((label (format "Ctx:%d" (max 0 (or count 0))))
         (tip (or tooltip ""))
         (seg (propertize label 'help-echo tip)))
    (setq carriage-ui-ctx--cache seg
          carriage-ui-ctx--cache-tooltip tip
          carriage-ui-ctx--last-error err
          carriage-ui-ctx--last-update-ts (carriage-ui-ctx--now))
    ;; Mirror into carriage-ui internal variables when present, to reduce flicker
    ;; if other parts still read them.
    (when (boundp 'carriage-ui--ctx-badge-cache)
      (setq carriage-ui--ctx-badge-cache seg))
    (when (boundp 'carriage-ui--ctx-badge-cache-sig)
      (setq carriage-ui--ctx-badge-cache-sig (list :ts carriage-ui-ctx--last-update-ts)))
    (when (boundp 'carriage-ui--ctx-badge-version)
      (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0))))
    seg))

(defun carriage-ui-ctx--refresh-now (&optional buffer)
  "Compute and update cache in BUFFER (or current buffer). Never signals."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((res (carriage-ui-ctx--compute-count))
           (n (plist-get res :count))
           (tip (plist-get res :tooltip))
           (err (plist-get res :error)))
      (carriage-ui-ctx--set-cache n tip err)
      (force-mode-line-update t)
      t)))

(defun carriage-ui-ctx-invalidate (&optional buffer)
  "Invalidate cached ctx badge in BUFFER (or current) and schedule refresh soon."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-ui-ctx--last-update-ts 0.0)
    (setq carriage-ui-ctx--cache nil)
    (carriage-ui-ctx-schedule-refresh)))

(defun carriage-ui-ctx-schedule-refresh (&optional buffer)
  "Schedule a context badge refresh for BUFFER (or current), throttled to ≤1Hz."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((now (carriage-ui-ctx--now))
           (interval (max 0.2 (or carriage-ui-ctx-refresh-interval 1.0)))
           (age (- now (or carriage-ui-ctx--last-update-ts 0.0)))
           (need (or (null carriage-ui-ctx--cache)
                     (>= age interval))))
      (when (and need (not (timerp carriage-ui-ctx--pending-timer)))
        (let ((buf (current-buffer)))
          (setq carriage-ui-ctx--pending-timer
                (run-at-time
                 0.05 nil
                 (lambda ()
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (setq carriage-ui-ctx--pending-timer nil)
                       (ignore-errors (carriage-ui-ctx--refresh-now buf)))))))))))
  t)

(defun carriage-ui-ctx--badge-string ()
  "Return cached ctx badge string; schedule refresh when stale/missing."
  (unless (stringp carriage-ui-ctx--cache)
    ;; When cache is empty, show placeholder but schedule immediate refresh.
    (carriage-ui-ctx-schedule-refresh))
  (or carriage-ui-ctx--cache
      (propertize "Ctx:?" 'help-echo "Ctx badge pending recompute…")))

(with-eval-after-load 'carriage-ui
  ;; Hard override: ensure a single consistent getter, no synchronous computation.
  (when (fboundp 'carriage-ui--context-badge)
    (defalias 'carriage-ui--context-badge #'carriage-ui-ctx--badge-string))
  ;; Compatibility: if something still calls compute directly, return cached string and schedule.
  (when (fboundp 'carriage-ui--compute-context-badge)
    (defalias 'carriage-ui--compute-context-badge #'carriage-ui-ctx--badge-string))
  ;; Ensure invalidation entrypoint exists and uses our scheduler.
  (unless (fboundp 'carriage-ui--ctx-invalidate)
    (defalias 'carriage-ui--ctx-invalidate #'carriage-ui-ctx-invalidate))
  t)

(provide 'carriage-ui-ctx-fast)
;;; carriage-ui-ctx-fast.el ends here
