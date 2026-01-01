;;; carriage-context-fast.el --- Fast context counting for UI (no file reads) -*- lexical-binding: t; -*-
;;
;; Purpose:
;; - Provide a fast, UI-safe context counter for the modeline badge.
;; - MUST NOT read file contents.
;; - Uses idle/debounce + buffer-local cache to keep UI responsive.
;;
;; This module is intentionally conservative: it focuses on doc-context and
;; visible buffers (file-visiting) as cheap sources. It returns a plist:
;;   (:count N :items LIST :warnings LIST)
;;
;; Items are plists at minimum containing:
;;   :path STRING
;;   :source SYMBOL  ; 'doc | 'visible | 'doc+visible
;;   :omitted BOOL   ; optional
;;   :why STRING     ; optional
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-project-root "carriage-utils" ())
(declare-function carriage-ui--ctx-invalidate "carriage-ui" ())

(defvar-local carriage-context-fast--cache nil
  "Cached result plist for `carriage-context-count-fast'.")

(defvar-local carriage-context-fast--cache-tick nil
  "buffer-chars-modified-tick corresponding to `carriage-context-fast--cache'.")

(defvar-local carriage-context-fast--refresh-timer nil
  "Idle timer used to coalesce recomputation of context count.")

(defun carriage-context-fast--project-root (buffer)
  "Return expanded project root for BUFFER (best-effort)."
  (with-current-buffer buffer
    (file-name-as-directory
     (expand-file-name
      (or (and (fboundp 'carriage-project-root)
               (ignore-errors (carriage-project-root)))
          default-directory)))))

(defun carriage-context-fast--normalize-path (s root)
  "Normalize S against ROOT.
Return a repo-relative path string, or nil if invalid/outside-root/remote."
  (let* ((s1 (and (stringp s) (string-trim s))))
    (cond
     ((or (null s1) (string-empty-p s1)) nil)
     ((file-remote-p s1) nil)
     ((file-name-absolute-p s1)
      (let* ((abs (ignore-errors (expand-file-name s1)))
             (root1 (file-name-as-directory (expand-file-name root))))
        (when (and (stringp abs)
                   (string-prefix-p root1 abs))
          (file-relative-name abs root1))))
     (t
      ;; Treat as repo-relative already (UI layer should not expand outside root).
      (when (and (not (string-prefix-p "../" s1))
                 (not (string-prefix-p "..\\" s1))
                 (not (string-prefix-p "/" s1)))
        s1)))))

(defun carriage-context-fast--scan-context-blocks (buffer scope)
  "Return list of raw (un-normalized) lines from begin_context blocks in BUFFER.
SCOPE is 'all or 'last."
  (with-current-buffer buffer
    (if (not (derived-mode-p 'org-mode))
        '()
      (save-excursion
        (save-restriction
          (widen)
          (let ((case-fold-search t)
                (blocks '()))
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t)
              (let* ((body-beg (save-excursion (forward-line 1) (point)))
                     (body-end (if (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
                                   (line-beginning-position)
                                 (point-max))))
                (push (cons body-beg body-end) blocks)))
            (setq blocks (nreverse blocks))
            (when (and (eq scope 'last) blocks)
              (setq blocks (list (car (last blocks)))))
            (let ((acc '()))
              (dolist (rg blocks)
                (save-excursion
                  (goto-char (car rg))
                  (while (< (point) (cdr rg))
                    (let* ((ln (buffer-substring-no-properties (line-beginning-position)
                                                              (line-end-position)))
                           (tln (string-trim ln)))
                      (unless (or (string-empty-p tln)
                                  (string-prefix-p "#" tln)
                                  (string-prefix-p ";" tln))
                        (push tln acc)))
                    (forward-line 1))))
              (nreverse acc))))))))

(defun carriage-context-fast--visible-file-paths (buffer)
  "Return list of absolute file names for buffers visible in any window.
Only includes file-visiting buffers; ignores non-file buffers."
  (with-current-buffer buffer
    (let ((acc '()))
      (dolist (w (window-list nil 'no-mini))
        (when (window-live-p w)
          (let* ((b (window-buffer w))
                 (fn (and (buffer-live-p b)
                          (buffer-local-value 'buffer-file-name b))))
            (when (and (stringp fn) (not (file-remote-p fn)) (not (string-empty-p fn)))
              (push fn acc)))))
      (nreverse (delete-dups acc)))))

(defun carriage-context-fast--compute (buffer)
  "Compute a fast context count for BUFFER (no file reads)."
  (let* ((root (carriage-context-fast--project-root buffer))
         (inc-doc (with-current-buffer buffer
                    (and (boundp 'carriage-mode-include-doc-context)
                         carriage-mode-include-doc-context)))
         (inc-vis (with-current-buffer buffer
                    (and (boundp 'carriage-mode-include-visible-context)
                         carriage-mode-include-visible-context)))
         (scope (with-current-buffer buffer
                  (if (and (boundp 'carriage-doc-context-scope)
                           (eq carriage-doc-context-scope 'last))
                      'last
                    'all)))
         (warnings '())
         (src (make-hash-table :test 'equal)))

    (when inc-doc
      (dolist (ln (carriage-context-fast--scan-context-blocks buffer scope))
        (let* ((rel (carriage-context-fast--normalize-path ln root)))
          (if (not (stringp rel))
              (push (format "doc: ignored invalid/out-of-root path: %s" ln) warnings)
            (puthash rel (if (gethash rel src) 'doc+visible 'doc) src)))))

    (when inc-vis
      (dolist (abs (carriage-context-fast--visible-file-paths buffer))
        (let* ((rel (carriage-context-fast--normalize-path abs root)))
          (when (stringp rel)
            (puthash rel (if (gethash rel src) 'doc+visible 'visible) src)))))

    (let ((items '())
          (count 0))
      (maphash
       (lambda (path source)
         (push (list :path path :source source) items)
         (setq count (1+ count)))
       src)
      ;; Deterministic ordering for stable UI tooltip.
      (setq items (sort items (lambda (a b)
                                (string< (plist-get a :path) (plist-get b :path)))))
      (list :count count :items items :warnings (nreverse warnings)))))

(defun carriage-context-fast--schedule-refresh (buffer &optional delay)
  "Schedule cached recomputation for BUFFER and invalidate UI context badge."
  (with-current-buffer buffer
    (when (timerp carriage-context-fast--refresh-timer)
      (ignore-errors (cancel-timer carriage-context-fast--refresh-timer)))
    (let ((buf buffer)
          (d (or delay 0.15)))
      (setq carriage-context-fast--refresh-timer
            (run-with-idle-timer
             d nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq carriage-context-fast--refresh-timer nil)
                   (setq carriage-context-fast--cache
                         (carriage-context-fast--compute buf))
                   (setq carriage-context-fast--cache-tick (buffer-chars-modified-tick))
                   (when (fboundp 'carriage-ui--ctx-invalidate)
                     (ignore-errors (carriage-ui--ctx-invalidate)))
                   (force-mode-line-update t)))))))))

(defun carriage-context-fast--after-change (_beg _end _len)
  "After-change hook: schedule refresh (coalesced)."
  (carriage-context-fast--schedule-refresh (current-buffer) 0.2))

;;;###autoload
(defun carriage-context-count-fast (&optional buffer _point)
  "Fast context counter for UI. Returns plist (:count N :items LIST :warnings LIST).

BUFFER defaults to current buffer. POINT is currently ignored (kept for API compatibility).
This function never reads file contents.

Caching:
- If cache matches current buffer tick, return cached value.
- If stale and cache exists, schedule an idle refresh and return cached value.
- If no cache yet, compute once synchronously (best-effort) and cache it."
  (let* ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      ;; Ensure change watch is installed once per buffer (lightweight).
      (unless (memq #'carriage-context-fast--after-change after-change-functions)
        (add-hook 'after-change-functions #'carriage-context-fast--after-change nil t))
      (let ((tick (buffer-chars-modified-tick)))
        (cond
         ((and (listp carriage-context-fast--cache)
               (integerp carriage-context-fast--cache-tick)
               (= tick carriage-context-fast--cache-tick))
          carriage-context-fast--cache)
         ((listp carriage-context-fast--cache)
          ;; Stale cache: return immediately, refresh on idle.
          (carriage-context-fast--schedule-refresh buf 0.15)
          carriage-context-fast--cache)
         (t
          ;; First compute: do it now (context blocks are typically small).
          (setq carriage-context-fast--cache (carriage-context-fast--compute buf))
          (setq carriage-context-fast--cache-tick tick)
          carriage-context-fast--cache))))))

(provide 'carriage-context-fast)
;;; carriage-context-fast.el ends here
