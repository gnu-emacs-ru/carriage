;;; carriage-doc-state.el --- Persist/restore Carriage buffer state via Org property -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+
;;
;;; Commentary:
;;
;; Doc-state v2:
;; - Canonical storage is a single file-level Org property:
;;     #+PROPERTY: CARRIAGE_STATE <sexp>
;; - <sexp> is a readable Emacs Lisp plist (preferred) with :CAR_* keys
;;   (or an alist convertible to such plist).
;; - Legacy storages (begin_carriage blocks, drawers, multi-line CARRIAGE_* props)
;;   are not supported by this module.
;;
;; Goals:
;; - Deterministic header placement: right after the last top-of-file #+PROPERTY line
;;   if any exist in the header, otherwise after the last top-of-file #+KEY line.
;; - Never insert inside/after any #+begin_* block.
;; - Idempotent write/normalization.
;; - Robustness: invalid/unreadable CARRIAGE_STATE must not break anything; restore becomes
;;   best-effort and defaults remain active.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-doc-state nil
  "Persist/restore Carriage document state in Org buffers."
  :group 'applications
  :prefix "carriage-doc-state-")

(defcustom carriage-doc-state-save-on-save nil
  "When non-nil, install a buffer-local before-save hook to normalize CARRIAGE_STATE.
Opt-in; disabled by default."
  :type 'boolean
  :group 'carriage-doc-state)

(defcustom carriage-doc-state-sync-on-change t
  "When non-nil, persist document state after commands that change Carriage settings.
This is implemented via advices in carriage-mode and must be best-effort."
  :type 'boolean
  :group 'carriage-doc-state)

(defcustom carriage-doc-state-invisibility-symbol 'carriage-doc-state
  "Symbol used in `buffer-invisibility-spec' to hide the CARRIAGE_STATE property line."
  :type 'symbol
  :group 'carriage-doc-state)

(defvar-local carriage-doc-state--overlay nil
  "Overlay used to hide the CARRIAGE_STATE property line(s) in the current buffer.")

(defvar-local carriage-doc-state--save-hook-installed nil
  "Non-nil when the doc-state before-save hook is installed in the current buffer.")

(defconst carriage-doc-state--property-key "CARRIAGE_STATE"
  "Org property key used for canonical Carriage doc-state storage.")

(defun carriage-doc-state--begin-block-line-p ()
  "Return non-nil when current line starts an Org begin_<kind> block."
  (looking-at-p "^[ \t]*#\\+begin_\\b"))

(defun carriage-doc-state--header-insertion-point ()
  "Return canonical insertion point for the CARRIAGE_STATE property line.

Placement:
- In the initial header area (leading run of #+KEY: lines).
- Directly after the last top-of-file #+PROPERTY: line if any exist in that header,
  otherwise after the last top-of-file #+KEY: line.
- Never insert inside/after the first begin_* block."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (seen-hdr nil)
            (last-key-eol nil)
            (last-prop-eol nil)
            (done nil))
        (while (and (not done) (not (eobp)))
          (cond
           ;; Skip leading blanks before the header starts.
           ((looking-at-p "^[ \t]*$")
            (if seen-hdr
                (setq done t)
              (forward-line 1)))

           ;; Do not cross begin_* blocks: header ends before them.
           ((carriage-doc-state--begin-block-line-p)
            (setq done t))

           ;; File keyword lines
           ((looking-at-p "^[ \t]*#\\+PROPERTY:")
            (setq seen-hdr t)
            (setq last-key-eol (line-end-position))
            (setq last-prop-eol (line-end-position))
            (forward-line 1))
           ((looking-at-p "^[ \t]*#\\+[A-Za-z0-9_]+:")
            (setq seen-hdr t)
            (setq last-key-eol (line-end-position))
            (forward-line 1))

           ;; Other #+something lines are treated as header terminators.
           ((looking-at-p "^[ \t]*#\\+")
            (setq done t))

           ;; Comment lines: allow only before header begins.
           ((looking-at-p "^[ \t]*#\\([^+]\\|$\\)")
            (if seen-hdr
                (setq done t)
              (forward-line 1)))

           ;; Any other content ends header scan.
           (t
            (setq done t))))

        (let ((eol (or last-prop-eol last-key-eol)))
          (if (numberp eol)
              (progn
                (goto-char eol)
                (forward-line 1)
                (point))
            (point-min)))))))

(defun carriage-doc-state--normalize-state (state)
  "Normalize STATE to a plist with :CAR_* keys.
STATE may be nil, a plist, or an alist."
  (condition-case _e
      (cond
       ((null state) '())
       ;; plist: (:K1 V1 :K2 V2 ...)
       ((and (listp state)
             (or (null state) (keywordp (car state))))
        state)
       ;; alist: ((:K . V) ...)
       ((and (listp state)
             (consp (car state))
             (keywordp (caar state)))
        (let ((pl '()))
          (dolist (cell state (nreverse pl))
            (when (consp cell)
              (setq pl (cons (cdr cell) (cons (car cell) pl)))))))
       (t '()))
    (error '())))

(defun carriage-doc-state--sexp-read (s)
  "Read sexp from string S; return nil on any read error."
  (condition-case _e
      (let* ((txt (string-trim (or s ""))))
        (when (> (length txt) 0)
          (car (read-from-string txt))))
    (error nil)))

(defun carriage-doc-state--find-state-lines ()
  "Return (BEG . END) covering all CARRIAGE_STATE property lines, or nil.
BEG is beginning of the first matching line, END is end position after the last matching line."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (beg nil)
            (end nil))
        (while (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b.*$" nil t)
          (setq beg (or beg (line-beginning-position)))
          (setq end (min (point-max) (1+ (line-end-position)))))
        (when (and (numberp beg) (numberp end) (> end beg))
          (cons beg end))))))

(defun carriage-doc-state--remove-state-lines ()
  "Delete all CARRIAGE_STATE property lines in the current buffer.
Return number of deleted lines."
  (let ((n 0))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search t)
              (inhibit-read-only t))
          (while (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b.*$" nil t)
            (setq n (1+ n))
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position))))
            (goto-char (line-beginning-position))))))
    n))

;;;###autoload
(defun carriage-doc-state-read (&optional buffer)
  "Read CARRIAGE_STATE from BUFFER (or current buffer) and return a plist.
Returns nil when missing or unreadable."
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
  "Upsert exactly one canonical CARRIAGE_STATE line using PLIST (already normalized)."
  (let* ((inhibit-read-only t)
         (sexp (prin1-to-string (or plist '())))
         (line (format "#+PROPERTY: %s %s\n" carriage-doc-state--property-key sexp)))
    ;; Remove all existing occurrences (anywhere, including inside begin_* blocks).
    (carriage-doc-state--remove-state-lines)
    ;; Insert at canonical header slot.
    (let ((ip (carriage-doc-state--header-insertion-point)))
      (goto-char ip)
      (unless (bolp) (insert "\n"))
      (insert line))
    t))

;;;###autoload
(defun carriage-doc-state-write (state &optional buffer)
  "Write STATE into BUFFER (or current buffer) as canonical CARRIAGE_STATE property line.
STATE may be a plist or an alist. Always writes exactly one line.
Never signals on parse/format errors; returns non-nil on success."
  (with-current-buffer (or buffer (current-buffer))
    (condition-case _e
        (save-excursion
          (save-restriction
            (widen)
            (let ((pl (carriage-doc-state--normalize-state state)))
              (carriage-doc-state--upsert-state-line pl))))
      (error nil))))

(defun carriage-doc-state--collect-var (sym)
  "Return value of SYM if bound, otherwise nil."
  (when (boundp sym) (symbol-value sym)))

(defun carriage-doc-state--collect-current ()
  "Collect current buffer-local Carriage state as a plist (:CAR_* ...).
This function must never signal."
  (condition-case _e
      (let ((pl '()))
        (cl-labels
            ((put (k v) (setq pl (plist-put pl k v)))
             (b (sym) (carriage-doc-state--collect-var sym)))
          ;; Core UI state
          (put :CAR_MODE (and (boundp 'carriage-mode) (bound-and-true-p carriage-mode)))
          (put :CAR_INTENT  (b 'carriage-mode-intent))
          (put :CAR_SUITE   (b 'carriage-mode-suite))
          (put :CAR_MODEL   (b 'carriage-mode-model))
          (put :CAR_BACKEND (b 'carriage-mode-backend))
          (put :CAR_PROVIDER (b 'carriage-mode-provider))

          ;; Context toggles/sources
          (put :CAR_CTX_GPTEL   (b 'carriage-mode-include-gptel-context))
          (put :CAR_CTX_DOC     (b 'carriage-mode-include-doc-context))
          (put :CAR_CTX_VISIBLE (b 'carriage-mode-include-visible-context))
          (put :CAR_CTX_INJECTION (b 'carriage-mode-context-injection))
          (put :CAR_CTX_MAX_FILES (b 'carriage-mode-context-max-files))
          (put :CAR_CTX_MAX_BYTES (b 'carriage-mode-context-max-total-bytes))

          ;; UI/behaviour toggles
          (put :CAR_SHOW_DIFFS (b 'carriage-mode-show-diffs))
          (put :CAR_CONFIRM_APPLY_ALL (b 'carriage-mode-confirm-apply-all))
          (put :CAR_CONFIRM_APPLY (b 'carriage-mode-confirm-apply))
          (put :CAR_USE_ICONS (b 'carriage-mode-use-icons))
          (put :CAR_USE_SUITE_ICON (b 'carriage-mode-use-suite-icon))
          (put :CAR_USE_ENGINE_ICON (b 'carriage-mode-use-engine-icon))
          (put :CAR_FLASH_PATCHES (b 'carriage-mode-flash-patches))
          (put :CAR_AUDIO_NOTIFY (b 'carriage-mode-audio-notify))
          (put :CAR_REPORT_OPEN_POLICY (b 'carriage-mode-report-open-policy))
          (put :CAR_AUTO_OPEN_LOG (b 'carriage-mode-auto-open-log))
          (put :CAR_AUTO_OPEN_TRAFFIC (b 'carriage-mode-auto-open-traffic))
          (put :CAR_AUTO_OPEN_REPORT (b 'carriage-mode-auto-open-report))

          ;; Apply engine (function-based)
          (when (fboundp 'carriage-apply-engine)
            (put :CAR_APPLY_ENGINE (ignore-errors (carriage-apply-engine))))

          ;; Doc-context scope / presets (best-effort; these vars may or may not exist)
          (dolist (cell '((:CAR_DOC_CTX_SCOPE . carriage-doc-context-scope)
                          (:CAR_DOC_CTX_SCOPE . carriage-mode-doc-context-scope)
                          (:CAR_DOC_CTX_SCOPE . carriage-doc-context-scope)
                          (:CAR_CTX_PROFILE . carriage-context-profile)
                          (:CAR_CTX_PROFILE . carriage-mode-context-profile)))
            (let ((k (car cell)) (v (cdr cell)))
              (when (boundp v) (put k (symbol-value v))))))
        pl)
    (error '())))

;;;###autoload
(defun carriage-doc-state-write-current (&optional buffer)
  "Collect current state and write it into BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-doc-state-write (carriage-doc-state--collect-current) (current-buffer))))

(defun carriage-doc-state--apply-if-bound (var val)
  "Set buffer-local VAR to VAL if VAR is bound.
Never signals."
  (when (boundp var)
    (condition-case _e
        (set (make-local-variable var) val)
      (error nil))))

;;;###autoload
(defun carriage-doc-state-restore (&optional buffer)
  "Restore Carriage buffer-local variables from CARRIAGE_STATE in BUFFER.
Best-effort: invalid/unreadable state results in no changes (defaults remain)."
  (with-current-buffer (or buffer (current-buffer))
    (let ((pl (ignore-errors (carriage-doc-state-read (current-buffer)))))
      (when (and (listp pl) (plist-member pl :CAR_MODE))
        ;; Never toggle carriage-mode here; just restore variables.
        (carriage-doc-state--apply-if-bound 'carriage-mode-intent  (plist-get pl :CAR_INTENT))
        (carriage-doc-state--apply-if-bound 'carriage-mode-suite   (plist-get pl :CAR_SUITE))
        (carriage-doc-state--apply-if-bound 'carriage-mode-model   (plist-get pl :CAR_MODEL))
        (carriage-doc-state--apply-if-bound 'carriage-mode-backend (plist-get pl :CAR_BACKEND))
        (carriage-doc-state--apply-if-bound 'carriage-mode-provider (plist-get pl :CAR_PROVIDER))

        (carriage-doc-state--apply-if-bound 'carriage-mode-include-gptel-context (plist-get pl :CAR_CTX_GPTEL))
        (carriage-doc-state--apply-if-bound 'carriage-mode-include-doc-context   (plist-get pl :CAR_CTX_DOC))
        (carriage-doc-state--apply-if-bound 'carriage-mode-include-visible-context (plist-get pl :CAR_CTX_VISIBLE))
        (carriage-doc-state--apply-if-bound 'carriage-mode-context-injection (plist-get pl :CAR_CTX_INJECTION))
        (carriage-doc-state--apply-if-bound 'carriage-mode-context-max-files (plist-get pl :CAR_CTX_MAX_FILES))
        (carriage-doc-state--apply-if-bound 'carriage-mode-context-max-total-bytes (plist-get pl :CAR_CTX_MAX_BYTES))

        (carriage-doc-state--apply-if-bound 'carriage-mode-show-diffs (plist-get pl :CAR_SHOW_DIFFS))
        (carriage-doc-state--apply-if-bound 'carriage-mode-confirm-apply-all (plist-get pl :CAR_CONFIRM_APPLY_ALL))
        (carriage-doc-state--apply-if-bound 'carriage-mode-confirm-apply (plist-get pl :CAR_CONFIRM_APPLY))
        (carriage-doc-state--apply-if-bound 'carriage-mode-use-icons (plist-get pl :CAR_USE_ICONS))
        (carriage-doc-state--apply-if-bound 'carriage-mode-use-suite-icon (plist-get pl :CAR_USE_SUITE_ICON))
        (carriage-doc-state--apply-if-bound 'carriage-mode-use-engine-icon (plist-get pl :CAR_USE_ENGINE_ICON))
        (carriage-doc-state--apply-if-bound 'carriage-mode-flash-patches (plist-get pl :CAR_FLASH_PATCHES))
        (carriage-doc-state--apply-if-bound 'carriage-mode-audio-notify (plist-get pl :CAR_AUDIO_NOTIFY))
        (carriage-doc-state--apply-if-bound 'carriage-mode-report-open-policy (plist-get pl :CAR_REPORT_OPEN_POLICY))
        (carriage-doc-state--apply-if-bound 'carriage-mode-auto-open-log (plist-get pl :CAR_AUTO_OPEN_LOG))
        (carriage-doc-state--apply-if-bound 'carriage-mode-auto-open-traffic (plist-get pl :CAR_AUTO_OPEN_TRAFFIC))
        (carriage-doc-state--apply-if-bound 'carriage-mode-auto-open-report (plist-get pl :CAR_AUTO_OPEN_REPORT))

        ;; Doc-context scope / profiles (best-effort; vars may be defined in other modules)
        (carriage-doc-state--apply-if-bound 'carriage-doc-context-scope (plist-get pl :CAR_DOC_CTX_SCOPE))
        (carriage-doc-state--apply-if-bound 'carriage-mode-doc-context-scope (plist-get pl :CAR_DOC_CTX_SCOPE))
        (carriage-doc-state--apply-if-bound 'carriage-context-profile (plist-get pl :CAR_CTX_PROFILE))
        (carriage-doc-state--apply-if-bound 'carriage-mode-context-profile (plist-get pl :CAR_CTX_PROFILE))

        t))))

;;;###autoload
(defun carriage-doc-state-auto-enable (&optional buffer)
  "Auto-enable `carriage-mode' in BUFFER if CARRIAGE_STATE requests it.
Invalid/unreadable state is ignored."
  (with-current-buffer (or buffer (current-buffer))
    (let ((pl (ignore-errors (carriage-doc-state-read (current-buffer)))))
      (when (and (listp pl)
                 (plist-member pl :CAR_MODE)
                 (plist-get pl :CAR_MODE)
                 (fboundp 'carriage-mode)
                 (boundp 'carriage-mode)
                 (not (bound-and-true-p carriage-mode)))
        (ignore-errors (carriage-mode 1))))))

(defun carriage-doc-state--before-save ()
  "Before-save hook handler: normalize and (optionally) hide CARRIAGE_STATE.
Must be a no-op when `carriage-doc-state-save-on-save' is nil."
  (when carriage-doc-state-save-on-save
    ;; Best-effort: invalid state must not break saving.
    (ignore-errors (carriage-doc-state-restore (current-buffer)))
    (ignore-errors (carriage-doc-state-write-current (current-buffer)))
    (ignore-errors (carriage-doc-state-hide (current-buffer)))))

;;;###autoload
(defun carriage-doc-state-install-save-hook (&optional buffer)
  "Install buffer-local before-save hook for doc-state normalization."
  (with-current-buffer (or buffer (current-buffer))
    (when (and carriage-doc-state-save-on-save
               (not carriage-doc-state--save-hook-installed))
      (add-hook 'before-save-hook #'carriage-doc-state--before-save nil t)
      (setq carriage-doc-state--save-hook-installed t)
      t)))

(defun carriage-doc-state--delete-overlay ()
  "Delete doc-state overlay if present."
  (when (overlayp carriage-doc-state--overlay)
    (delete-overlay carriage-doc-state--overlay))
  (setq carriage-doc-state--overlay nil))

;;;###autoload
(defun carriage-doc-state-hide (&optional buffer)
  "Hide CARRIAGE_STATE property line(s) in BUFFER using an overlay."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-doc-state--delete-overlay)
    (let ((rg (carriage-doc-state--find-state-lines)))
      (when (consp rg)
        (let ((ov (make-overlay (car rg) (cdr rg) (current-buffer) t t)))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'category 'carriage-doc-state)
          (overlay-put ov 'invisible carriage-doc-state-invisibility-symbol)
          (setq carriage-doc-state--overlay ov)
          (unless (member carriage-doc-state-invisibility-symbol buffer-invisibility-spec)
            (add-to-invisibility-spec carriage-doc-state-invisibility-symbol))
          t)))))

;;;###autoload
(defun carriage-doc-state-show (&optional buffer)
  "Show (unhide) CARRIAGE_STATE property line(s) in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-doc-state--delete-overlay)
    t))

;;;###autoload
(defun carriage-doc-state-toggle-visibility (&optional buffer)
  "Toggle visibility of CARRIAGE_STATE property line(s) in BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (if (overlayp carriage-doc-state--overlay)
        (carriage-doc-state-show (current-buffer))
      (carriage-doc-state-hide (current-buffer)))))

(provide 'carriage-doc-state)
;;; carriage-doc-state.el ends here
