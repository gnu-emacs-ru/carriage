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

(defcustom carriage-doc-state-summary-enable t
  "When non-nil, fold `#+PROPERTY: CARRIAGE_STATE ...` into a compact summary via overlay.

Behavior:
- When point is NOT on the CARRIAGE_STATE line, the original line is hidden and a short
  badge/icon summary is shown (similar to the modeline).
- When point enters the line, the original text is revealed automatically.
- Tooltip (help-echo) shows detailed state, including context budgets."
  :type 'boolean
  :group 'carriage-doc-state)

(defcustom carriage-doc-state-summary-debounce-seconds 0.15
  "Idle debounce (seconds) for refreshing CARRIAGE_STATE summary overlay after edits."
  :type 'number
  :group 'carriage-doc-state)

(defvar-local carriage-doc-state--overlay nil
  "Overlay used to render the CARRIAGE_STATE property line(s) as a compact summary when folded.

When `carriage-doc-state-summary-enable' is non-nil, this overlay uses:
- `invisible' to hide the original line(s),
- `before-string' to show summary badges/icons,
- auto-reveal when point enters the overlay and auto-hide when point leaves.")

(defvar-local carriage-doc-state--summary-hooks-installed nil
  "Non-nil when post-command/after-change hooks for the CARRIAGE_STATE summary are installed.")

(defvar-local carriage-doc-state--summary-refresh-timer nil
  "Idle timer used to debounce refresh of the CARRIAGE_STATE summary overlay.")

(defvar-local carriage-doc-state--summary-folded nil
  "Non-nil when the CARRIAGE_STATE overlay is currently in folded (summary) mode.")

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
BEG is beginning of the first matching line, END is end position of the last matching line.

Important: END intentionally does NOT include the trailing newline. This keeps
line navigation (C-p/C-n) stable when a summary overlay is applied."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (beg nil)
            (end nil))
        (while (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b.*$" nil t)
          (setq beg (or beg (line-beginning-position)))
          (setq end (line-end-position)))
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
              (carriage-doc-state--upsert-state-line pl)
              ;; Refresh summary overlay (best-effort). This is purely a UI layer and must not signal.
              (ignore-errors
                (when (and (boundp 'carriage-doc-state-summary-enable)
                           carriage-doc-state-summary-enable
                           (fboundp 'carriage-doc-state-summary-refresh))
                  ;; Ensure hooks are installed so fold/reveal follows point automatically,
                  ;; even when callers only write state (without calling hide/enable explicitly).
                  (carriage-doc-state--summary-install-hooks)
                  (carriage-doc-state-summary-refresh (current-buffer))))
              t)))
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
          (put :CAR_CTX_PATCHED (b 'carriage-mode-include-patched-files))
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
        (carriage-doc-state--apply-if-bound 'carriage-mode-include-patched-files (plist-get pl :CAR_CTX_PATCHED))
        (carriage-doc-state--apply-if-bound 'carriage-mode-context-injection (plist-get pl :CAR_CTX_INJECTION))
        (carriage-doc-state--apply-if-bound 'carriage-mode-context-max-files (plist-get pl :CAR_CTX_MAX_FILES))
        (carriage-doc-state--apply-if-bound 'carriage-mode-context-max-total-bytes (plist-get pl :CAR_CTX_MAX_BYTES))

        (carriage-doc-state--apply-if-bound 'carriage-mode-show-diffs (plist-get pl :CAR_SHOW_DIFFS))
        (carriage-doc-state--apply-if-bound 'carriage-mode-confirm-apply-all (plist-get pl :CAR_CONFIRM_APPLY_ALL))
        (carriage-doc-state--apply-if-bound 'carriage-mode-confirm-apply (plist-get pl :CAR_CONFIRM_APPLY))
        ;; UI toggles: apply only when explicitly present, to avoid clobbering defaults.
        (when (plist-member pl :CAR_USE_ICONS)
          (carriage-doc-state--apply-if-bound 'carriage-mode-use-icons (plist-get pl :CAR_USE_ICONS)))
        (when (plist-member pl :CAR_USE_SUITE_ICON)
          (carriage-doc-state--apply-if-bound 'carriage-mode-use-suite-icon (plist-get pl :CAR_USE_SUITE_ICON)))
        (when (plist-member pl :CAR_USE_ENGINE_ICON)
          (carriage-doc-state--apply-if-bound 'carriage-mode-use-engine-icon (plist-get pl :CAR_USE_ENGINE_ICON)))
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

        ;; Best-effort: if summary folding is enabled, fold the line immediately
        ;; so users see badges by default after restore (e.g., on buffer open).
        (ignore-errors
          (when (and (boundp 'carriage-doc-state-summary-enable)
                     carriage-doc-state-summary-enable
                     (derived-mode-p 'org-mode))
            (carriage-doc-state-hide (current-buffer))))
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
  (setq carriage-doc-state--overlay nil)
  (setq carriage-doc-state--summary-folded nil))

(defun carriage-doc-state--summary-cancel-timer ()
  "Cancel pending debounce timer for summary refresh, if any."
  (when (timerp carriage-doc-state--summary-refresh-timer)
    (ignore-errors (cancel-timer carriage-doc-state--summary-refresh-timer)))
  (setq carriage-doc-state--summary-refresh-timer nil))

(defun carriage-doc-state--summary-install-hooks ()
  "Install buffer-local hooks for auto folding/revealing of the CARRIAGE_STATE overlay."
  (unless carriage-doc-state--summary-hooks-installed
    (setq carriage-doc-state--summary-hooks-installed t)
    (add-hook 'post-command-hook #'carriage-doc-state--summary-post-command nil t)
    (add-hook 'after-change-functions #'carriage-doc-state--summary-after-change nil t)))

(defun carriage-doc-state--summary-remove-hooks ()
  "Remove buffer-local hooks for the CARRIAGE_STATE overlay."
  (when carriage-doc-state--summary-hooks-installed
    (setq carriage-doc-state--summary-hooks-installed nil)
    (remove-hook 'post-command-hook #'carriage-doc-state--summary-post-command t)
    (remove-hook 'after-change-functions #'carriage-doc-state--summary-after-change t)))

(defun carriage-doc-state--as-symbol (v)
  "Normalize V into a symbol when possible (best-effort)."
  (cond
   ((symbolp v) v)
   ((stringp v) (intern v))
   (t nil)))

(defun carriage-doc-state--as-string (v)
  "Normalize V into a string (best-effort)."
  (cond
   ((null v) "")
   ((stringp v) v)
   ((symbolp v) (symbol-name v))
   (t (format "%s" v))))

(defun carriage-doc-state--bool (v)
  "Normalize V into boolean."
  (and v (not (eq v :json-false))))

(defun carriage-doc-state--important-plist (pl)
  "Extract a stable \"response/context-shaping\" subset of PL for summary rendering.
Budgets are intentionally NOT included in the summary subset (they go to tooltip)."
  (let* ((intent (carriage-doc-state--as-symbol (plist-get pl :CAR_INTENT)))
         (suite (carriage-doc-state--as-symbol (plist-get pl :CAR_SUITE)))
         (backend (carriage-doc-state--as-symbol (plist-get pl :CAR_BACKEND)))
         (provider (carriage-doc-state--as-string (plist-get pl :CAR_PROVIDER)))
         (model (carriage-doc-state--as-string (plist-get pl :CAR_MODEL)))
         (ctx-doc (carriage-doc-state--bool (plist-get pl :CAR_CTX_DOC)))
         (ctx-gptel (carriage-doc-state--bool (plist-get pl :CAR_CTX_GPTEL)))
         (ctx-vis (carriage-doc-state--bool (plist-get pl :CAR_CTX_VISIBLE)))
         (ctx-patched (carriage-doc-state--bool (plist-get pl :CAR_CTX_PATCHED)))
         (scope (carriage-doc-state--as-symbol (plist-get pl :CAR_DOC_CTX_SCOPE)))
         (profile (carriage-doc-state--as-symbol (plist-get pl :CAR_CTX_PROFILE)))
         (inj (carriage-doc-state--as-symbol (plist-get pl :CAR_CTX_INJECTION))))
    (list :CAR_INTENT intent
          :CAR_SUITE suite
          :CAR_BACKEND backend
          :CAR_PROVIDER provider
          :CAR_MODEL model
          :CAR_CTX_DOC ctx-doc
          :CAR_CTX_GPTEL ctx-gptel
          :CAR_CTX_VISIBLE ctx-vis
          :CAR_CTX_PATCHED ctx-patched
          :CAR_DOC_CTX_SCOPE scope
          :CAR_CTX_PROFILE profile
          :CAR_CTX_INJECTION inj)))

(defun carriage-doc-state--llm-display-name (backend provider model)
  "Return a compact model label from BACKEND/PROVIDER/MODEL (best-effort)."
  (let* ((raw (string-trim (or model "")))
         (id (cond
              ((and (fboundp 'carriage-llm-make-full-id)
                    (or (and (symbolp backend) backend) (and (stringp provider) (> (length provider) 0))))
               (ignore-errors
                 (carriage-llm-make-full-id backend provider raw)))
              (t raw)))
         (disp (if (and (fboundp 'carriage-llm-display-name) (stringp id))
                   (ignore-errors (carriage-llm-display-name id))
                 id)))
    (if (and (stringp disp) (not (string-empty-p disp)))
        disp
      (or raw "-"))))

(defun carriage-doc-state--ui-icon (key fallback)
  "Return icon string for KEY using carriage-ui when available; otherwise FALLBACK."
  (condition-case _e
      (if (and (require 'carriage-ui nil t)
               (fboundp 'carriage-ui--icons-available-p)
               (carriage-ui--icons-available-p)
               (fboundp 'carriage-ui--icon))
          (or (carriage-ui--icon key) fallback)
        fallback)
    (error fallback)))

(defun carriage-doc-state--badge (s &optional face)
  "Build a small badge string S with FACE.

Important: preserve any existing face/font properties on S (e.g. all-the-icons).
We apply FACE to the bracket chrome, and for the content we only apply FACE to
runs that have no explicit `face' property, so icon glyphs keep their font/face."
  (let* ((content (cond
                   ((null s) "-")
                   ((stringp s) s)
                   (t (format "%s" s))))
         (content2 (if (stringp content) (copy-sequence content) content))
         (lb (if face (propertize "[" 'face face) "["))
         (rb (if face (propertize "]" 'face face) "]")))
    (when (and face (stringp content2))
      (let ((i 0)
            (len (length content2)))
        (while (< i len)
          (let* ((next (or (next-single-property-change i 'face content2) len))
                 (f (get-text-property i 'face content2)))
            (unless f
              (put-text-property i next 'face face content2))
            (setq i next)))))
    (concat lb content2 rb)))

(defun carriage-doc-state--ctx-flag-badge (label on &optional icon-key)
  "Badge for a context source flag."
  (let* ((ic (and icon-key (carriage-doc-state--ui-icon icon-key nil)))
         ;; Important: preserve icon text-properties (all-the-icons).
         (name (if ic (concat ic label) label))
         (face (if on 'success 'shadow)))
    (carriage-doc-state--badge name face)))

(defun carriage-doc-state--summary-string (pl)
  "Return compact summary string (badges/icons) for CARRIAGE_STATE plist PL."
  (let* ((imp (carriage-doc-state--important-plist pl))
         (intent (plist-get imp :CAR_INTENT))
         (suite  (plist-get imp :CAR_SUITE))
         (backend (plist-get imp :CAR_BACKEND))
         (provider (plist-get imp :CAR_PROVIDER))
         (model (plist-get imp :CAR_MODEL))
         (ctx-doc (plist-get imp :CAR_CTX_DOC))
         (ctx-gptel (plist-get imp :CAR_CTX_GPTEL))
         (ctx-vis (plist-get imp :CAR_CTX_VISIBLE))
         (ctx-patched (plist-get imp :CAR_CTX_PATCHED))
         (scope (plist-get imp :CAR_DOC_CTX_SCOPE))
         (profile (plist-get imp :CAR_CTX_PROFILE))
         (inj (plist-get imp :CAR_CTX_INJECTION))
         (intent-ic (pcase intent
                      ('Ask    (carriage-doc-state--ui-icon 'ask "A"))
                      ('Code   (carriage-doc-state--ui-icon 'patch "C"))
                      ('Hybrid (carriage-doc-state--ui-icon 'hybrid "H"))
                      (_       (carriage-doc-state--ui-icon 'ask "A"))))
         (suite-ic (carriage-doc-state--ui-icon 'suite nil))
         (model-ic (carriage-doc-state--ui-icon 'model nil))
         ;; IMPORTANT: do not use `format' with icon strings, it drops text properties
         ;; (all-the-icons font/face). Use `concat' to preserve icon properties.
         (intent-b (carriage-doc-state--badge (or intent-ic "-") 'mode-line-emphasis))
         (suite-b  (carriage-doc-state--badge (concat (or suite-ic "") (carriage-doc-state--as-string (or suite "-"))) 'shadow))
         (model-b  (carriage-doc-state--badge (concat (or model-ic "")
                                                      (carriage-doc-state--llm-display-name backend provider model))
                                              'mode-line-emphasis))
         (ctx-b (string-join
                 (delq nil
                       (list
                        (carriage-doc-state--ctx-flag-badge "Doc" ctx-doc)
                        (carriage-doc-state--ctx-flag-badge "Gpt" ctx-gptel)
                        (carriage-doc-state--ctx-flag-badge "Vis" ctx-vis)
                        (when (plist-member imp :CAR_CTX_PATCHED)
                          (carriage-doc-state--ctx-flag-badge "Pat" ctx-patched))))
                 " "))
         (scope-b (when (and scope (not (eq scope nil)))
                    (carriage-doc-state--badge (format "%s" scope) 'shadow)))
         (profile-b (when (and profile (not (eq profile nil)))
                      (carriage-doc-state--badge (format "%s" profile) 'shadow)))
         (inj-b (when (and inj (not (eq inj nil)))
                  (carriage-doc-state--badge (format "%s" inj) 'shadow))))
    (string-join (delq nil (list intent-b suite-b model-b ctx-b scope-b profile-b inj-b)) " ")))

(defun carriage-doc-state--tooltip-string (pl)
  "Return detailed tooltip text for CARRIAGE_STATE plist PL (includes budgets)."
  (let* ((intent (carriage-doc-state--as-string (plist-get pl :CAR_INTENT)))
         (suite  (carriage-doc-state--as-string (plist-get pl :CAR_SUITE)))
         (backend (carriage-doc-state--as-string (plist-get pl :CAR_BACKEND)))
         (provider (carriage-doc-state--as-string (plist-get pl :CAR_PROVIDER)))
         (model (carriage-doc-state--as-string (plist-get pl :CAR_MODEL)))
         (ctx-doc (carriage-doc-state--bool (plist-get pl :CAR_CTX_DOC)))
         (ctx-gptel (carriage-doc-state--bool (plist-get pl :CAR_CTX_GPTEL)))
         (ctx-vis (carriage-doc-state--bool (plist-get pl :CAR_CTX_VISIBLE)))
         (ctx-patched (carriage-doc-state--bool (plist-get pl :CAR_CTX_PATCHED)))
         (scope (carriage-doc-state--as-string (plist-get pl :CAR_DOC_CTX_SCOPE)))
         (profile (carriage-doc-state--as-string (plist-get pl :CAR_CTX_PROFILE)))
         (inj (carriage-doc-state--as-string (plist-get pl :CAR_CTX_INJECTION)))
         (mf (plist-get pl :CAR_CTX_MAX_FILES))
         (mb (plist-get pl :CAR_CTX_MAX_BYTES)))
    (string-join
     (delq nil
           (list
            "CARRIAGE_STATE"
            (format "Intent: %s" (or intent "-"))
            (format "Suite: %s" (or suite "-"))
            (format "Model: %s:%s:%s" (or backend "-") (or provider "-") (or model "-"))
            (format "Context sources: doc=%s gptel=%s visible=%s patched=%s"
                    (if ctx-doc "on" "off")
                    (if ctx-gptel "on" "off")
                    (if ctx-vis "on" "off")
                    (if ctx-patched "on" "off"))
            (when (or (not (string-empty-p scope)) (not (string-empty-p profile)))
              (format "Scope/Profile: scope=%s profile=%s"
                      (if (string-empty-p scope) "-" scope)
                      (if (string-empty-p profile) "-" profile)))
            (when (not (string-empty-p inj))
              (format "Context injection: %s" inj))
            (when (or mf mb)
              (format "Budgets: max-files=%s max-bytes=%s"
                      (if (numberp mf) mf (or mf "-"))
                      (if (numberp mb) mb (or mb "-"))))
            (format "Raw: %s" (prin1-to-string (or pl '())))))
     "\n")))

(defun carriage-doc-state--summary-range ()
  "Return (BEG . END) range for the CARRIAGE_STATE property line(s), or nil."
  (carriage-doc-state--find-state-lines))

(defun carriage-doc-state--summary-point-inside-p (ov)
  "Return non-nil if point is within OV bounds."
  (and (overlayp ov)
       (numberp (overlay-start ov))
       (numberp (overlay-end ov))
       (>= (point) (overlay-start ov))
       (<= (point) (overlay-end ov))))

(defun carriage-doc-state--summary-fold (ov summary tooltip)
  "Fold OV: show SUMMARY via overlay display, keep line navigable, attach TOOLTIP."
  (when (overlayp ov)
    (overlay-put ov 'help-echo tooltip)
    ;; Use `display' instead of `invisible'+`before-string' to preserve line navigation (C-p/C-n)
    ;; and allow point to enter the line for editing (reveal).
    (overlay-put ov 'display summary)
    ;; Ensure we don't also inject a phantom line.
    (overlay-put ov 'before-string nil)
    (overlay-put ov 'invisible nil)
    (setq carriage-doc-state--summary-folded t)))

(defun carriage-doc-state--summary-reveal (ov tooltip)
  "Reveal OV: show original text, hide summary placeholder."
  (when (overlayp ov)
    (overlay-put ov 'help-echo tooltip)
    (overlay-put ov 'before-string nil)
    (overlay-put ov 'invisible nil)
    (setq carriage-doc-state--summary-folded nil)))

(defun carriage-doc-state-summary-refresh (&optional buffer)
  "Rebuild/refresh CARRIAGE_STATE summary overlay in BUFFER (or current buffer)."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'org-mode)
      (let ((rg (carriage-doc-state--summary-range)))
        (if (not (consp rg))
            (progn
              (carriage-doc-state--delete-overlay)
              nil)
          (let* ((pl (or (ignore-errors (carriage-doc-state-read (current-buffer))) '()))
                 (summary (ignore-errors (carriage-doc-state--summary-string pl)))
                 (tooltip (ignore-errors (carriage-doc-state--tooltip-string pl)))
                 (beg (car rg))
                 (end (cdr rg))
                 (ov carriage-doc-state--overlay))
            (unless (and (overlayp ov)
                         (= (overlay-start ov) beg)
                         (= (overlay-end ov) end))
              (when (overlayp ov) (delete-overlay ov))
              (setq ov (make-overlay beg end (current-buffer) t t))
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'category 'carriage-doc-state)
              (overlay-put ov 'carriage-doc-state-summary t)
              (setq carriage-doc-state--overlay ov))
            ;; Fold/reveal depending on point location.
            (if (carriage-doc-state--summary-point-inside-p ov)
                (carriage-doc-state--summary-reveal ov tooltip)
              (carriage-doc-state--summary-fold ov summary tooltip))
            t))))))

(defun carriage-doc-state--summary-refresh-debounced ()
  "Schedule a debounced refresh of the summary overlay."
  (carriage-doc-state--summary-cancel-timer)
  (let ((buf (current-buffer))
        (d (max 0.01 (or carriage-doc-state-summary-debounce-seconds 0.15))))
    (setq carriage-doc-state--summary-refresh-timer
          (run-at-time
           d nil
           (lambda (b)
             (when (buffer-live-p b)
               (with-current-buffer b
                 (setq carriage-doc-state--summary-refresh-timer nil)
                 (ignore-errors (carriage-doc-state-summary-refresh b)))))
           buf))))

(defun carriage-doc-state--summary-after-change (_beg _end _len)
  "After-change hook: refresh summary overlay (debounced)."
  (when (and (boundp 'carriage-doc-state-summary-enable)
             carriage-doc-state-summary-enable)
    (carriage-doc-state--summary-refresh-debounced)))

(defun carriage-doc-state--summary-post-command ()
  "post-command hook: auto-reveal on cursor enter and auto-hide on cursor leave."
  (when (and (boundp 'carriage-doc-state-summary-enable)
             carriage-doc-state-summary-enable
             (overlayp carriage-doc-state--overlay))
    (ignore-errors
      (carriage-doc-state-summary-refresh (current-buffer)))))

;;;###autoload
(defun carriage-doc-state-summary-enable (&optional buffer)
  "Enable compact summary overlay for CARRIAGE_STATE in BUFFER (or current buffer)."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-doc-state-summary-enable t)
    (carriage-doc-state--summary-install-hooks)
    (carriage-doc-state-summary-refresh (current-buffer))))

;;;###autoload
(defun carriage-doc-state-summary-disable (&optional buffer)
  "Disable compact summary overlay for CARRIAGE_STATE in BUFFER (or current buffer)."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-doc-state-summary-enable nil)
    (carriage-doc-state--summary-cancel-timer)
    (carriage-doc-state--summary-remove-hooks)
    (carriage-doc-state--delete-overlay)
    t))

;;;###autoload
(defun carriage-doc-state-hide (&optional buffer)
  "Hide CARRIAGE_STATE property line(s) in BUFFER.
If `carriage-doc-state-summary-enable' is non-nil, show compact badges/icons instead."
  (with-current-buffer (or buffer (current-buffer))
    (if (and (boundp 'carriage-doc-state-summary-enable)
             carriage-doc-state-summary-enable)
        (progn
          (carriage-doc-state--summary-install-hooks)
          (carriage-doc-state-summary-refresh (current-buffer)))
      ;; Legacy: pure hide without summary
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
            t))))))

;;;###autoload
(defun carriage-doc-state-show (&optional buffer)
  "Show (unfold) the raw CARRIAGE_STATE property line(s) in BUFFER (disable summary overlay)."
  (with-current-buffer (or buffer (current-buffer))
    ;; Disable the summary hooks and remove overlay entirely: this is an explicit \"show raw\".
    (carriage-doc-state--summary-cancel-timer)
    (carriage-doc-state--summary-remove-hooks)
    (carriage-doc-state--delete-overlay)
    t))

;;;###autoload
(defun carriage-doc-state-toggle-visibility (&optional buffer)
  "Toggle visibility of CARRIAGE_STATE property line(s) in BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (if (and (overlayp carriage-doc-state--overlay)
             (or carriage-doc-state--summary-folded
                 (overlay-get carriage-doc-state--overlay 'invisible)))
        (carriage-doc-state-show (current-buffer))
      (carriage-doc-state-hide (current-buffer)))))

(provide 'carriage-doc-state)
;;; carriage-doc-state.el ends here
