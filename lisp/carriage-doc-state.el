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
;; - UI: folded summary uses overlay 'display (no invisible/before-string); reveal on point.
;;
;;; Code:
;; Display-based fold: overlays show badges when cursor is away; reveal original when cursor enters the line.

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


(defcustom carriage-doc-state-summary-enable t
  "When non-nil, fold `#+PROPERTY: CARRIAGE_STATE ...` into a compact summary via overlay.

Behavior:
- When point is NOT on the CARRIAGE_STATE line, the original line is hidden and a short
  badge/icon summary is shown (similar to the modeline).
- When point enters the line, the original text is revealed automatically.
- Tooltip (help-echo) shows detailed state, including context budgets."
  :type 'boolean
  :group 'carriage-doc-state)

(defcustom carriage-doc-state-summary-debounce-seconds 0.4
  "Idle debounce (seconds) for refreshing CARRIAGE_STATE summary overlay after edits.

Note: doc-state refresh may touch many fingerprint overlays; a slightly larger debounce
reduces input lag during continuous typing."
  :type 'number
  :group 'carriage-doc-state)

(defcustom carriage-state-badge-overlay-minimal nil
  "When non-nil, render CARRIAGE_STATE fold badge in minimal form (model only).
When nil (default), render a richer badge (suite/model/context toggles, etc.)."
  :type 'boolean
  :group 'carriage-doc-state)

(defcustom carriage-fingerprint-badge-overlay-minimal nil
  "When non-nil, render CARRIAGE_FINGERPRINT fold badge in minimal form (model only).
When nil (default), render a richer badge (suite/model/context toggles, etc.)."
  :type 'boolean
  :group 'carriage-doc-state)

(defcustom carriage-doc-state-tooltip-verbosity 'brief
  "Verbosity policy for doc-state fold tooltips (CARRIAGE_STATE/CARRIAGE_FINGERPRINT).

- 'brief (default): tooltip is lightweight and does NOT render full raw plist/details.
  Intended for performance (safe during frequent refreshes).
- 'full: tooltip renders the full details (may be expensive in buffers with many fingerprints)."
  :type '(choice (const :tag "Brief (fast)" brief)
                 (const :tag "Full (verbose; may be slow)" full))
  :group 'carriage-doc-state)

(defcustom carriage-doc-state-fingerprint-fold-max 30
  "Maximum number of CARRIAGE_FINGERPRINT lines to fold with overlays.

This directly bounds the number of persistent folding overlays in the buffer and
reduces redisplay/input overhead in long-running file-chat buffers.

Set to nil to disable limiting (fold all fingerprints)."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max folded fingerprints"))
  :group 'carriage-doc-state)

(defvar-local carriage-doc-state--overlay nil
  "Overlay used to render the CARRIAGE_STATE property line as a compact summary (display-based fold).

Implementation details:
- Uses only overlay 'display to replace the visible text with a compact summary.
- When point is on the line, 'display is nil (original text is shown and editable).
- When point leaves the line, 'display is set back to the summary.")

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
                  (carriage-doc-state-summary-enable)
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
          (put :CAR_CTX_MAP     (b 'carriage-mode-include-project-map))
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
      (when (and (listp pl) (plist-member pl :CAR_MODE)
                 (carriage-doc-state--bool (plist-get pl :CAR_MODE)))
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
        (carriage-doc-state--apply-if-bound 'carriage-mode-include-project-map (plist-get pl :CAR_CTX_MAP))
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
            (carriage-doc-state-summary-enable)
            (carriage-doc-state-summary-refresh (current-buffer))))
        t))))

;;;###autoload
(defun carriage-doc-state-auto-enable (&optional buffer)
  "Auto-enable `carriage-mode' in BUFFER if CARRIAGE_STATE requests it.
Invalid/unreadable state is ignored."
  (with-current-buffer (or buffer (current-buffer))
    (let ((pl (ignore-errors (carriage-doc-state-read (current-buffer)))))
      (when (and (listp pl)
                 (plist-member pl :CAR_MODE)
                 (carriage-doc-state--bool (plist-get pl :CAR_MODE))
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
    (ignore-errors
      (when (derived-mode-p 'org-mode)
        (carriage-doc-state-summary-enable)
        (carriage-doc-state-summary-refresh (current-buffer))))))

;;;###autoload
(defun carriage-doc-state-install-save-hook (&optional buffer)
  "Install buffer-local before-save hook for doc-state normalization."
  (with-current-buffer (or buffer (current-buffer))
    (when (and carriage-doc-state-save-on-save
               (not carriage-doc-state--save-hook-installed))
      (add-hook 'before-save-hook #'carriage-doc-state--before-save nil t)
      (setq carriage-doc-state--save-hook-installed t)
      t)))





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
  "Normalize V into boolean (accept symbol/string/number/json false)."
  (cond
   ((or (null v) (eq v :json-false)) nil)
   ((eq v t) t)
   ((numberp v) (not (zerop v)))
   ((symbolp v) (memq v '(t yes on true)))
   ((stringp v)
    (let ((s (downcase (string-trim v))))
      (cond
       ((member s '("t" "true" "yes" "on" "1")) t)
       ((member s '("nil" "false" "no" "off" "0" "")) nil)
       (t t))))
   (t t)))

(defun carriage-doc-state--money-symbol ()
  "Return currency symbol string for UI rendering (best-effort)."
  (cond
   ((and (boundp 'carriage-pricing-currency-symbol)
         (stringp carriage-pricing-currency-symbol))
    carriage-pricing-currency-symbol)
   (t "₽")))

(defun carriage-doc-state--format-money-suffix (amount-u &optional currency-symbol)
  "Format AMOUNT-U (µ₽ integer) as \"12.34₽\" with half-up kopeck rounding.
When AMOUNT-U is nil/non-integer, return \"—\"."
  (if (not (integerp amount-u))
      "—"
    (let* ((sym (or (and (stringp currency-symbol) currency-symbol)
                    (carriage-doc-state--money-symbol)))
           (kopecks (/ (+ amount-u 5000) 10000)) ;; 1 коп. = 10_000 µ₽, half-up
           (rub (/ kopecks 100))
           (kop (% kopecks 100)))
      (format "%d.%02d%s" rub kop (or sym "")))))

(defun carriage-doc-state--important-plist (pl)
  "Extract a stable \"response/context-shaping\" subset of PL for summary rendering.
Budgets are intentionally NOT included in the summary subset (they go to tooltip).

:CAR_CTX_INJECTION (system/user) больше не показывается в summary (оверлее)."
  (let* ((intent (carriage-doc-state--as-symbol (plist-get pl :CAR_INTENT)))
         (suite (carriage-doc-state--as-symbol (plist-get pl :CAR_SUITE)))
         (backend (carriage-doc-state--as-symbol (plist-get pl :CAR_BACKEND)))
         (provider (carriage-doc-state--as-string (plist-get pl :CAR_PROVIDER)))
         (model (carriage-doc-state--as-string (plist-get pl :CAR_MODEL)))
         (ctx-doc (carriage-doc-state--bool (plist-get pl :CAR_CTX_DOC)))
         (ctx-gptel (carriage-doc-state--bool (plist-get pl :CAR_CTX_GPTEL)))
         (ctx-vis (carriage-doc-state--bool (plist-get pl :CAR_CTX_VISIBLE)))
         (ctx-plain (carriage-doc-state--bool (plist-get pl :CAR_CTX_PLAIN)))
         (ctx-patched (carriage-doc-state--bool (plist-get pl :CAR_CTX_PATCHED)))
         (ctx-map (carriage-doc-state--bool (plist-get pl :CAR_CTX_MAP)))
         (scope (carriage-doc-state--as-symbol (plist-get pl :CAR_DOC_CTX_SCOPE)))
         (profile (carriage-doc-state--as-symbol (plist-get pl :CAR_CTX_PROFILE))))
    (list :CAR_INTENT intent
          :CAR_SUITE suite
          :CAR_BACKEND backend
          :CAR_PROVIDER provider
          :CAR_MODEL model
          :CAR_CTX_DOC ctx-doc
          :CAR_CTX_GPTEL ctx-gptel
          :CAR_CTX_VISIBLE ctx-vis
          :CAR_CTX_PLAIN ctx-plain
          :CAR_CTX_PATCHED ctx-patched
          :CAR_CTX_MAP ctx-map
          :CAR_DOC_CTX_SCOPE scope
          :CAR_CTX_PROFILE profile)))

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
  "Return icon string for KEY using carriage-ui when available; otherwise FALLBACK.

Important: when `carriage-mode' isn't loaded yet (e.g. file just opened and
carriage-mode auto-enable hasn't run), `carriage-mode-use-icons' may be unbound.
We best-effort load carriage-mode so the icon availability checks work and
default to enabled."
  (condition-case _e
      (progn
        ;; Ensure `carriage-mode-use-icons' and icon params exist before calling carriage-ui.
        (require 'carriage-mode nil t)
        (if (and (require 'carriage-ui nil t)
                 (fboundp 'carriage-ui--icons-available-p)
                 (carriage-ui--icons-available-p)
                 (fboundp 'carriage-ui--icon))
            (or (carriage-ui--icon key) fallback)
          fallback))
    (error fallback)))

(defun carriage-doc-state--badge (s &optional face)
  "Build a small segment string S with FACE (no brackets).

Important: preserve any existing face/font properties on S (e.g. all-the-icons).
We apply FACE only to runs that have no explicit `face' property, so icon glyphs
keep their font/face."
  (let* ((content (cond
                   ((null s) "")
                   ((stringp s) s)
                   (t (format "%s" s))))
         (content2 (if (stringp content) (copy-sequence content) content)))
    (when (and face (stringp content2))
      (let ((i 0)
            (len (length content2)))
        (while (< i len)
          (let* ((next (or (next-single-property-change i 'face content2) len))
                 (f (get-text-property i 'face content2)))
            (unless f
              (put-text-property i next 'face face content2))
            (setq i next)))))
    content2))

(defun carriage-doc-state--ctx-flag-badge (label on &optional icon-key)
  "Return icon-only badge for a context flag when ON; hide when OFF.

- When ON and an icon is available for ICON-KEY, return that icon (no label).
- When ON but no icon is available, fall back to LABEL (text).
- When OFF (nil), return nil (do not show a disabled toggle at all)."
  (when on
    (let* ((ic (and icon-key (carriage-doc-state--ui-icon icon-key nil)))
           ;; Icons keep their own face; for fallback text we apply a visible face.
           (name (or ic label)))
      (carriage-doc-state--badge name 'mode-line-emphasis))))

(defun carriage-doc-state--icon-gap (&optional pixels)
  "Return a small fixed-size gap for separating an icon and its label.
Uses `display' spacing in pixels when available; degrades gracefully in TTY."
  (propertize " " 'display (list 'space :width (cons 'pixels (or pixels 4)))))

(defun carriage-doc-state--ctx-flag-badge-with-label (label on icon-key)
  "Return \"icon + label\" badge for a context flag when ON; hide when OFF.

- When ON and an icon is available for ICON-KEY, render: ICON + GAP + LABEL.
- When ON but no icon is available, fall back to LABEL.
- When OFF (nil), return nil (do not show disabled toggles at all).

Important: use `concat' (not `format') to preserve icon text properties."
  (when on
    (let* ((ic (and icon-key (carriage-doc-state--ui-icon icon-key nil)))
           (lbl (carriage-doc-state--badge label 'mode-line-emphasis)))
      (if (and (stringp ic) (> (length ic) 0))
          (concat ic (carriage-doc-state--icon-gap) lbl)
        lbl))))

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
         (ctx-plain (plist-get imp :CAR_CTX_PLAIN))
         (ctx-patched (plist-get imp :CAR_CTX_PATCHED))
         (ctx-map (plist-get imp :CAR_CTX_MAP))
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
         (suite-b  (carriage-doc-state--badge
                    (let ((txt (carriage-doc-state--as-string (or suite "-"))))
                      (if (and (stringp suite-ic) (> (length suite-ic) 0))
                          (concat suite-ic (carriage-doc-state--icon-gap) txt)
                        txt))
                    'shadow))
         (model-b  (carriage-doc-state--badge
                    (let* ((ic (or model-ic ""))
                           (gap (if (and (stringp ic) (> (length ic) 0))
                                    (carriage-doc-state--icon-gap)
                                  "")))
                      (concat ic gap (carriage-doc-state--llm-display-name backend provider model)))
                    'mode-line-emphasis))
         (ctx-b (string-join
                 (delq nil
                       (list
                        (carriage-doc-state--ctx-flag-badge "Plain" ctx-plain 'plain)
                        (carriage-doc-state--ctx-flag-badge "Doc" ctx-doc 'files)
                        (carriage-doc-state--ctx-flag-badge "Gpt" ctx-gptel 'ctx)
                        (carriage-doc-state--ctx-flag-badge "Vis" ctx-vis 'visible)
                        (when (plist-member imp :CAR_CTX_PATCHED)
                          (carriage-doc-state--ctx-flag-badge "Pat" ctx-patched 'patched))
                        (when (plist-member imp :CAR_CTX_MAP)
                          (carriage-doc-state--ctx-flag-badge "Map" ctx-map 'map))))
                 " "))
         (scope-b (when (and scope (not (eq scope nil)))
                    (carriage-doc-state--badge
                     (let* ((ic (pcase scope
                                  ('all  (carriage-doc-state--ui-icon 'scope-all nil))
                                  ('last (carriage-doc-state--ui-icon 'scope-last nil))
                                  (_     (carriage-doc-state--ui-icon 'scope nil))))
                            ;; When scope is all/last and we have an icon, render icon-only (no "all"/"last" label).
                            (txt (unless (and (memq scope '(all last))
                                              (stringp ic) (> (length ic) 0))
                                   (carriage-doc-state--as-string scope)))
                            (gap (if (and (stringp ic) (> (length ic) 0)
                                          (stringp txt) (> (length txt) 0))
                                     (carriage-doc-state--icon-gap)
                                   "")))
                       (concat (or ic "") gap (or txt "")))
                     'shadow)))
         (profile-b (when (and profile (not (eq profile nil)))
                      (carriage-doc-state--badge
                       (let* ((ic (or (carriage-doc-state--ui-icon 'profile nil) ""))
                              (gap (if (and (stringp ic) (> (length ic) 0))
                                       (carriage-doc-state--icon-gap)
                                     "")))
                         (concat ic gap (carriage-doc-state--as-string profile)))
                       'shadow)))
         (inj-b (when (and inj (not (eq inj nil)))
                  (carriage-doc-state--badge
                   (let* ((ic (or (carriage-doc-state--ui-icon 'inject nil) ""))
                          (gap (if (and (stringp ic) (> (length ic) 0))
                                   (carriage-doc-state--icon-gap)
                                 "")))
                     (concat ic gap (carriage-doc-state--as-string inj)))
                   'shadow))))
    (string-join (delq nil (list intent-b suite-b model-b ctx-b scope-b profile-b inj-b)) " ")))

(defun carriage-doc-state--summary-string-fingerprint (pl)
  "Return compact summary string (badges/icons) for CARRIAGE_FINGERPRINT plist PL.

Differs from `carriage-doc-state--summary-string' by rendering context flags as
icons-only (like the modeline). Labels like \"Doc\"/\"Pat\" are not shown.

Also shows total request cost when present (as the last badge):
- integer :CAR_COST_TOTAL_U → \"22.76₽\"
- explicit nil (key present) → \"—\"
- absent key → omitted (typically before usage/cost upsert)."
  (let* ((imp (carriage-doc-state--important-plist pl))
         (intent (plist-get imp :CAR_INTENT))
         (suite  (plist-get imp :CAR_SUITE))
         (backend (plist-get imp :CAR_BACKEND))
         (provider (plist-get imp :CAR_PROVIDER))
         (model (plist-get imp :CAR_MODEL))
         (ctx-doc (plist-get imp :CAR_CTX_DOC))
         (ctx-gptel (plist-get imp :CAR_CTX_GPTEL))
         (ctx-vis (plist-get imp :CAR_CTX_VISIBLE))
         (ctx-plain (plist-get imp :CAR_CTX_PLAIN))
         (ctx-patched (plist-get imp :CAR_CTX_PATCHED))
         (ctx-map (plist-get imp :CAR_CTX_MAP))
         (scope (plist-get imp :CAR_DOC_CTX_SCOPE))
         (profile (plist-get imp :CAR_CTX_PROFILE))
         ;; IMPORTANT: cost keys are not part of important-plist; read them from PL directly.
         (cost-u (and (listp pl) (plist-get pl :CAR_COST_TOTAL_U)))
         (cost-b
          (cond
           ((integerp cost-u)
            (carriage-doc-state--badge (carriage-doc-state--format-money-suffix cost-u) 'shadow))
           ((and (listp pl) (plist-member pl :CAR_COST_TOTAL_U))
            (carriage-doc-state--badge "—" 'shadow))
           (t nil)))
         (intent-ic (pcase intent
                      ('Ask    (carriage-doc-state--ui-icon 'ask "A"))
                      ('Code   (carriage-doc-state--ui-icon 'patch "C"))
                      ('Hybrid (carriage-doc-state--ui-icon 'hybrid "H"))
                      (_       (carriage-doc-state--ui-icon 'ask "A"))))
         (suite-ic (carriage-doc-state--ui-icon 'suite nil))
         (model-ic (carriage-doc-state--ui-icon 'model nil))
         (intent-b (carriage-doc-state--badge (or intent-ic "-") 'mode-line-emphasis))
         (suite-b  (carriage-doc-state--badge
                    (let* ((ic (or suite-ic ""))
                           (gap (if (and (stringp ic) (> (length ic) 0))
                                    (carriage-doc-state--icon-gap)
                                  "")))
                      (concat ic gap (carriage-doc-state--as-string (or suite "-"))))
                    'shadow))
         (model-b  (carriage-doc-state--badge
                    (let* ((ic (or model-ic ""))
                           (gap (if (and (stringp ic) (> (length ic) 0))
                                    (carriage-doc-state--icon-gap)
                                  "")))
                      (concat ic gap (carriage-doc-state--llm-display-name backend provider model)))
                    'mode-line-emphasis))
         (ctx-b (string-join
                 (delq nil
                       (list
                        (carriage-doc-state--ctx-flag-badge "Plain" ctx-plain 'plain)
                        (carriage-doc-state--ctx-flag-badge "Doc" ctx-doc 'files)
                        (carriage-doc-state--ctx-flag-badge "Gpt" ctx-gptel 'ctx)
                        (carriage-doc-state--ctx-flag-badge "Vis" ctx-vis 'visible)
                        (when (plist-member imp :CAR_CTX_PATCHED)
                          (carriage-doc-state--ctx-flag-badge "Pat" ctx-patched 'patched))
                        (when (plist-member imp :CAR_CTX_MAP)
                          (carriage-doc-state--ctx-flag-badge "Map" ctx-map 'map))))
                 " "))
         (scope-b (when (and scope (not (eq scope nil)))
                    (carriage-doc-state--badge
                     (let* ((ic (pcase scope
                                  ('all  (carriage-doc-state--ui-icon 'scope-all nil))
                                  ('last (carriage-doc-state--ui-icon 'scope-last nil))
                                  (_     (carriage-doc-state--ui-icon 'scope nil))))
                            ;; When scope is all/last and we have an icon, render icon-only (no "all"/"last" label).
                            (txt (unless (and (memq scope '(all last))
                                              (stringp ic) (> (length ic) 0))
                                   (carriage-doc-state--as-string scope)))
                            (gap (if (and (stringp ic) (> (length ic) 0)
                                          (stringp txt) (> (length txt) 0))
                                     (carriage-doc-state--icon-gap)
                                   "")))
                       (concat (or ic "") gap (or txt "")))
                     'shadow)))
         (profile-b (when (and profile (not (eq profile nil)))
                      (carriage-doc-state--badge
                       (let* ((ic (or (carriage-doc-state--ui-icon 'profile nil) ""))
                              (gap (if (and (stringp ic) (> (length ic) 0))
                                       (carriage-doc-state--icon-gap)
                                     "")))
                         (concat ic gap (carriage-doc-state--as-string profile)))
                       'shadow))))
    (string-join (delq nil (list intent-b suite-b model-b ctx-b scope-b profile-b cost-b)) " ")))

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
         (ctx-map (carriage-doc-state--bool (plist-get pl :CAR_CTX_MAP)))
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
            (format "Context sources: doc=%s gptel=%s visible=%s plain=%s patched=%s map=%s"
                    (if ctx-doc "on" "off")
                    (if ctx-gptel "on" "off")
                    (if ctx-vis "on" "off")
                    (if ctx-plain "on" "off")
                    (if ctx-patched "on" "off")
                    (if ctx-map "on" "off"))
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





(defun carriage-doc-state-summary-refresh (&optional buffer)
  "Rebuild/refresh overlays for state/fingerprint in BUFFER (or current buffer).

Unified, display-based fold engine:
- No invisible/before-string usage.
- Always applies fold/reveal according to current point at the end.
Compatibility: mirrors overlay into `carriage-doc-state--overlay' for legacy tests."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'org-mode)
      ;; Ensure fold engine is enabled, then refresh overlays and apply fold/reveal.
      (setq carriage-doc-state--fold-enabled t)
      ;; Force a full rescan: summary-refresh can be called outside after-change,
      ;; so `carriage-doc-state--fold--pending-kind' may still be nil even though
      ;; new fingerprint lines were just inserted. Without this, fast-path may skip
      ;; rescanning and the fingerprint overlay won't be created.
      (setq carriage-doc-state--fold--pending-kind 'both)
      (ignore-errors (carriage-doc-state--fold--refresh-overlays))
      ;; Back-compat for older tests expecting `carriage-doc-state--overlay'.
      (setq carriage-doc-state--overlay carriage-doc-state--fold-state-ov)
      t)))




;; ---------------------------------------------------------------------------
;; Harmonious fold UI for CARRIAGE_STATE and CARRIAGE_FINGERPRINT
;;
;; Invariants:
;; - Never uses `invisible`/`before-string` for these lines (navigation must work).
;; - Uses only overlay `display` to replace the visible representation.
;; - If point is on the line: show original text (display=nil) and allow editing.
;; - If point leaves the line: restore summary badges (display=summary).
;; - After any refresh (write/after-change/open): fold/reveal is re-applied for point.

(defgroup carriage-doc-state-fold nil
  "Fold UI for Carriage state/fingerprint lines."
  :group 'carriage)



(defvar-local carriage-doc-state--fold-state-ov nil)
(defvar-local carriage-doc-state--fold-fp-ovs nil)
(defvar-local carriage-doc-state--fold--active-fp-ov nil
  "Fingerprint overlay currently revealed due to point being inside (perf cache; O(1) post-command).")

(defvar-local carriage-doc-state--fold--pending-kind nil
  "Which doc-state overlays need a refresh.
Nil means full refresh.
Possible values: 'state, 'fingerprint, 'both.")

(defvar-local carriage-doc-state--fold-refresh-timer nil)
(defvar-local carriage-doc-state--fold--refresh-pending nil
  "Non-nil when a fold overlay refresh is pending (coalesced by a single timer).")
(defvar-local carriage-doc-state--fold-enabled nil)

(defvar-local carriage-doc-state--fold--last-scan-tick nil
  "Last `buffer-chars-modified-tick' value when fold overlays were fully rescanned.")

(defvar-local carriage-doc-state--fold--last-scan-had-lines :unknown
  "Whether the last full rescan found any state/fingerprint lines.
Values:
- t        — at least one line was found
- nil      — no lines were found
- :unknown — never scanned (or state was reset).")

(defvar-local carriage-doc-state--fold--last-scan-env nil
  "Last environment signature used to render fold summaries.

Used to force a rescan/refresh when UI-affecting toggles change (e.g. minimal badge
mode) without any buffer text edits.")

(defun carriage-doc-state--fold--line-range-at-point ()
  "Return (BEG . END) for current logical line, excluding trailing newline."
  (save-excursion
    (cons (line-beginning-position)
          (line-end-position))))

(defun carriage-doc-state--fold--point-inside-ov-p (ov)
  "Non-nil if point is inside OV. End position is treated as inside (EOL)."
  (when (overlayp ov)
    (let ((beg (overlay-start ov))
          (end (overlay-end ov)))
      (and (number-or-marker-p beg)
           (number-or-marker-p end)
           (<= beg (point))
           (<= (point) end)))))

(defun carriage-doc-state--fold--ov-set-folded (ov)
  (when (overlayp ov)
    (overlay-put ov 'display (overlay-get ov 'carriage-fold-summary))
    (overlay-put ov 'help-echo (overlay-get ov 'carriage-fold-tooltip))
    (overlay-put ov 'mouse-face 'highlight)))

(defun carriage-doc-state--fold--ov-set-revealed (ov)
  (when (overlayp ov)
    (overlay-put ov 'display nil)
    (overlay-put ov 'help-echo (overlay-get ov 'carriage-fold-tooltip))
    (overlay-put ov 'mouse-face nil)))

(defun carriage-doc-state--fold--apply-for-point ()
  "Apply reveal/fold depending on current point for fold overlays.

Performance invariant:
- Must be O(1) on every command (post-command-hook).
- Must NOT traverse all fingerprint overlays (those can grow unbounded over time)."
  (when (and carriage-doc-state--fold-enabled
             ;; Skip work for buffers that are not visible; avoids background churn.
             (get-buffer-window (current-buffer) t))
    ;; ---------------------------------------------------------------------
    ;; State line overlay (single; safe to handle every time)
    (when (overlayp carriage-doc-state--fold-state-ov)
      (let ((beg (overlay-start carriage-doc-state--fold-state-ov))
            (end (overlay-end carriage-doc-state--fold-state-ov)))
        (if (and (number-or-marker-p beg) (number-or-marker-p end))
            (if (carriage-doc-state--fold--point-inside-ov-p carriage-doc-state--fold-state-ov)
                (carriage-doc-state--fold--ov-set-revealed carriage-doc-state--fold-state-ov)
              (carriage-doc-state--fold--ov-set-folded carriage-doc-state--fold-state-ov))
          (delete-overlay carriage-doc-state--fold-state-ov)
          (setq carriage-doc-state--fold-state-ov nil))))

    ;; ---------------------------------------------------------------------
    ;; Fingerprint overlays (many): ONLY touch at most one "active" fp overlay.
    ;;
    ;; Old behavior walked the whole list on every command, which makes cursor moves
    ;; progressively slower as the buffer accumulates fingerprints.
    (let ((active carriage-doc-state--fold--active-fp-ov))
      ;; Drop stale active overlay (killed/evaporated/moved to other buffer).
      (when (and (overlayp active)
                 (not (eq (overlay-buffer active) (current-buffer))))
        (setq active nil)
        (setq carriage-doc-state--fold--active-fp-ov nil))

      ;; If we have an active fp overlay, fold/reveal it depending on point.
      (when (overlayp active)
        (if (carriage-doc-state--fold--point-inside-ov-p active)
            (carriage-doc-state--fold--ov-set-revealed active)
          (carriage-doc-state--fold--ov-set-folded active)
          ;; We are no longer inside; clear active so we can pick a new one quickly.
          (setq active nil)
          (setq carriage-doc-state--fold--active-fp-ov nil)))

      ;; Find fingerprint overlay near point (boundary-safe) and reveal only that one.
      ;; Avoid allocating (append (overlays-at ...) (overlays-in ...)) on every command.
      (let* ((pt (point))
             (fp
              (cl-loop for ov in (overlays-at pt)
                       when (and (overlayp ov)
                                 (eq (overlay-get ov 'category) 'carriage-doc-state-fold)
                                 (not (eq ov carriage-doc-state--fold-state-ov))
                                 (overlay-get ov 'carriage-fold-summary))
                       return ov)))
        (unless (overlayp fp)
          (let* ((lo (max (point-min) (1- pt)))
                 (hi (min (point-max) (1+ pt))))
            (setq fp
                  (cl-loop for ov in (overlays-in lo hi)
                           when (and (overlayp ov)
                                     (eq (overlay-get ov 'category) 'carriage-doc-state-fold)
                                     (not (eq ov carriage-doc-state--fold-state-ov))
                                     (overlay-get ov 'carriage-fold-summary))
                           return ov))))
        (when (and (overlayp fp)
                   (not (eq fp active)))
          ;; Fold previously active fp overlay (if any), then reveal new one.
          (when (overlayp active)
            (carriage-doc-state--fold--ov-set-folded active))
          (setq carriage-doc-state--fold--active-fp-ov fp)
          (carriage-doc-state--fold--ov-set-revealed fp)))))
  t)

(defun carriage-doc-state--fold--maybe-summary-from-plist (pl kind)
  "Render summary badges from PL for KIND ('state or 'fingerprint).

By default, renders a \"rich\" badge (suite/model/context toggles, etc.).
When minimal mode is enabled for the given KIND, renders only the model badge."
  (let* ((pl2 (if (fboundp 'carriage-doc-state--important-plist)
                  (carriage-doc-state--important-plist pl)
                pl))
         ;; Global minimal toggle (when present) overrides per-kind settings.
         (global-min (and (boundp 'carriage-badge-overlay-minimal)
                          carriage-badge-overlay-minimal))
         (kind-min
          (pcase kind
            ('state (and (boundp 'carriage-state-badge-overlay-minimal)
                         carriage-state-badge-overlay-minimal))
            ('fingerprint (and (boundp 'carriage-fingerprint-badge-overlay-minimal)
                               carriage-fingerprint-badge-overlay-minimal))
            (_ nil)))
         (minimal (or global-min kind-min))
         (backend (carriage-doc-state--as-symbol (plist-get pl2 :CAR_BACKEND)))
         (provider (carriage-doc-state--as-string (plist-get pl2 :CAR_PROVIDER)))
         (model (carriage-doc-state--as-string (plist-get pl2 :CAR_MODEL)))
         (model-ic (carriage-doc-state--ui-icon 'model nil))
         (model-b (carriage-doc-state--badge
                   (concat (or model-ic "")
                           (carriage-doc-state--llm-display-name backend provider model))
                   'mode-line-emphasis)))
    (if minimal
        (let* ((intent (carriage-doc-state--as-symbol (plist-get pl2 :CAR_INTENT)))
               (intent-ic (pcase intent
                            ('Ask    (carriage-doc-state--ui-icon 'ask "A"))
                            ('Code   (carriage-doc-state--ui-icon 'patch "C"))
                            ('Hybrid (carriage-doc-state--ui-icon 'hybrid "H"))
                            (_       (carriage-doc-state--ui-icon 'ask "A"))))
               (intent-b (carriage-doc-state--badge (or intent-ic "-") 'mode-line-emphasis)))
          (string-join (delq nil (list intent-b model-b)) " "))
      ;; Rich badges: state keeps icon-only ctx flags; fingerprint renders icon+label for ctx flags.
      ;; IMPORTANT: fingerprint summary needs access to pricing keys (e.g. :CAR_COST_TOTAL_U),
      ;; which are intentionally not part of `carriage-doc-state--important-plist`.
      ;; Therefore, for fingerprint overlays pass the full PL, not PL2.
      (pcase kind
        ('fingerprint (carriage-doc-state--summary-string-fingerprint pl))
        (_            (carriage-doc-state--summary-string pl2))))))

(defun carriage-doc-state--fold--tooltip (raw-line pl kind)
  "Build tooltip text for RAW-LINE + PL (always brief/fast).

Perf invariant:
- Never renders full raw plist (no `pp-to-string`, no `carriage-doc-state--tooltip-string`).
- Intended to be safe to call during redisplay without causing input lag."
  (let* ((raw (string-trim-right (or raw-line "")))
         (pl0 (or pl '()))
         (imp (if (fboundp 'carriage-doc-state--important-plist)
                  (ignore-errors (carriage-doc-state--important-plist pl0))
                pl0))
         (backend (carriage-doc-state--as-symbol (plist-get imp :CAR_BACKEND)))
         (provider (carriage-doc-state--as-string (plist-get imp :CAR_PROVIDER)))
         (model (carriage-doc-state--as-string (plist-get imp :CAR_MODEL)))
         (ctx-doc (carriage-doc-state--bool (plist-get imp :CAR_CTX_DOC)))
         (ctx-gptel (carriage-doc-state--bool (plist-get imp :CAR_CTX_GPTEL)))
         (ctx-vis (carriage-doc-state--bool (plist-get imp :CAR_CTX_VISIBLE)))
         (ctx-patched (carriage-doc-state--bool (plist-get imp :CAR_CTX_PATCHED)))
         (ctx-plain (carriage-doc-state--bool (plist-get imp :CAR_CTX_PLAIN)))
         (scope (carriage-doc-state--as-string (plist-get imp :CAR_DOC_CTX_SCOPE)))
         (profile (carriage-doc-state--as-string (plist-get imp :CAR_CTX_PROFILE)))
         (cost-u (and (eq kind 'CARRIAGE_FINGERPRINT)
                      (listp pl0)
                      (plist-get pl0 :CAR_COST_TOTAL_U)))
         (cost-line
          (when (eq kind 'CARRIAGE_FINGERPRINT)
            (cond
             ((integerp cost-u)
              (format "Cost: %s" (carriage-doc-state--format-money-suffix cost-u)))
             ((and (listp pl0) (plist-member pl0 :CAR_COST_TOTAL_U))
              "Cost: —")
             (t nil)))))
    (string-join
     (delq nil
           (list
            (format "%s" kind)
            raw
            (format "Model: %s" (carriage-doc-state--llm-display-name backend provider model))
            cost-line
            (format "Context: doc=%s gptel=%s visible=%s plain=%s patched=%s"
                    (if ctx-doc "on" "off")
                    (if ctx-gptel "on" "off")
                    (if ctx-vis "on" "off")
                    (if ctx-plain "on" "off")
                    (if ctx-patched "on" "off"))
            (when (or (not (string-empty-p scope))
                      (not (string-empty-p profile)))
              (format "Scope/Profile: scope=%s profile=%s"
                      (if (string-empty-p scope) "-" scope)
                      (if (string-empty-p profile) "-" profile)))))
     "\n")))

(defun carriage-doc-state--fold--parse-sexp (s)
  (when (and s (not (string-empty-p (string-trim s))))
    (condition-case _err
        (read s)
      (error nil))))

(defun carriage-doc-state--fold--scan-state-line ()
  "Return plist describing the CARRIAGE_STATE line, or nil.
Returned plist keys: :beg :end :raw :pl."
  (save-excursion
    (goto-char (point-min))
    (let ((rx "^#\\+PROPERTY:\\s-+CARRIAGE_STATE\\s-+\\(.*\\)$"))
      (when (re-search-forward rx nil t)
        (let* ((beg (line-beginning-position))
               (end (line-end-position))
               (raw (buffer-substring-no-properties beg end))
               (sexp (match-string-no-properties 1))
               (pl (carriage-doc-state--fold--parse-sexp sexp)))
          (list :beg beg :end end :raw raw :pl pl))))))

(defun carriage-doc-state--fold--scan-fingerprint-lines ()
  "Return list of plists describing all fingerprint lines.
Supported canonical format (no backwards compatibility): `#+CARRIAGE_FINGERPRINT: <sexp>`."
  (save-excursion
    (goto-char (point-min))
    (let ((rx "^[ \t]*#\\+CARRIAGE_FINGERPRINT:\\s-*\\(.*\\)$")
          (acc nil))
      (while (re-search-forward rx nil t)
        (let* ((beg (line-beginning-position))
               (end (line-end-position))
               (raw (buffer-substring-no-properties beg end))
               (sexp (match-string-no-properties 1))
               (pl (carriage-doc-state--fold--parse-sexp sexp)))
          (push (list :beg beg :end end :raw raw :pl pl) acc)))
      (let* ((res (nreverse acc))
             (mx (and (boundp 'carriage-doc-state-fingerprint-fold-max)
                      carriage-doc-state-fingerprint-fold-max)))
        (if (and (integerp mx) (> mx 0) (> (length res) mx))
            (cl-subseq res (- (length res) mx))
          res)))))

(defun carriage-doc-state--fold--ov-upsert (ov beg end summary tooltip)
  (unless (overlayp ov)
    (setq ov (make-overlay beg end nil t t))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'priority 9000)
    (overlay-put ov 'category 'carriage-doc-state-fold))
  (move-overlay ov beg end)
  (overlay-put ov 'carriage-fold-summary summary)
  (overlay-put ov 'carriage-fold-tooltip tooltip)
  ;; Always start folded unless point is inside (apply-for-point fixes it).
  (carriage-doc-state--fold--ov-set-folded ov)
  ov)

(defun carriage-doc-state--fold--refresh-overlays ()
  "Rescan buffer and rebuild fold overlays, then apply fold/reveal for point.

Perf:
- Full rescan is performed only when the buffer text changed since the last scan
  (`buffer-chars-modified-tick') or when overlays are missing.
- On window/point moves (e.g. windmove) with no buffer edits, we only run
  `carriage-doc-state--fold--apply-for-point' (O(1)).
- UI-affecting toggles (e.g. minimal badge mode) MUST force refresh even when the
  buffer text did not change."
  (cl-block nil
    (when carriage-doc-state--fold-enabled
      (let* ((tick (buffer-chars-modified-tick))
             (env (list
                   (and (boundp 'carriage-badge-overlay-minimal)
                        carriage-badge-overlay-minimal)
                   (and (boundp 'carriage-state-badge-overlay-minimal)
                        carriage-state-badge-overlay-minimal)
                   (and (boundp 'carriage-fingerprint-badge-overlay-minimal)
                        carriage-fingerprint-badge-overlay-minimal)))
             (have-state-ov (overlayp carriage-doc-state--fold-state-ov))
             (have-fp-ovs (cl-some #'overlayp carriage-doc-state--fold-fp-ovs))
             (have-any-ov (or have-state-ov have-fp-ovs))
             (same-tick (and (numberp carriage-doc-state--fold--last-scan-tick)
                             (= tick carriage-doc-state--fold--last-scan-tick)))
             (same-env (equal env carriage-doc-state--fold--last-scan-env)))
        ;; Fast paths:
        ;; - If buffer text/env did not change since last scan → only apply fold/reveal (O(1)).
        ;; - If buffer changed but the edit did NOT touch doc-state lines (pending-kind=nil)
        ;;   and env is unchanged → do NOT rescan the whole buffer; overlays track moves automatically.
        (when (and same-env have-any-ov
                   (or same-tick
                       (null carriage-doc-state--fold--pending-kind)))
          (setq carriage-doc-state--fold--last-scan-tick tick)
          (carriage-doc-state--fold--apply-for-point)
          (cl-return-from nil t))

        (let* ((st  (carriage-doc-state--fold--scan-state-line))
               (fps (carriage-doc-state--fold--scan-fingerprint-lines)))

          ;; State line (single).
          (if (not st)
              (when (overlayp carriage-doc-state--fold-state-ov)
                (delete-overlay carriage-doc-state--fold-state-ov)
                (setq carriage-doc-state--fold-state-ov nil))
            (let* ((pl (plist-get st :pl))
                   (raw (plist-get st :raw))
                   (summary (carriage-doc-state--fold--maybe-summary-from-plist pl 'state))
                   (tooltip (carriage-doc-state--fold--tooltip raw pl 'CARRIAGE_STATE)))
              (setq carriage-doc-state--fold-state-ov
                    (carriage-doc-state--fold--ov-upsert
                     carriage-doc-state--fold-state-ov
                     (plist-get st :beg) (plist-get st :end)
                     summary tooltip))))

          ;; Fingerprints (many).
          (let* ((wanted (length fps))
                 (existing (cl-remove-if-not #'overlayp carriage-doc-state--fold-fp-ovs)))
            ;; Trim extra overlays.
            (when (> (length existing) wanted)
              (dolist (ov (nthcdr wanted existing))
                (when (overlayp ov) (delete-overlay ov)))
              (setq existing (cl-subseq existing 0 wanted)))
            ;; Extend overlays list.
            (while (< (length existing) wanted)
              (push nil existing))
            (setq existing (nreverse existing))

            ;; Upsert/move each overlay.
            (setq carriage-doc-state--fold-fp-ovs
                  (cl-loop for fp in fps
                           for ov in existing
                           collect
                           (let* ((pl (plist-get fp :pl))
                                  (raw (plist-get fp :raw))
                                  (summary (carriage-doc-state--fold--maybe-summary-from-plist pl 'fingerprint))
                                  (tooltip (carriage-doc-state--fold--tooltip raw pl 'CARRIAGE_FINGERPRINT)))
                             (carriage-doc-state--fold--ov-upsert
                              ov (plist-get fp :beg) (plist-get fp :end)
                              summary tooltip)))))

          ;; Record scan outcome for fast-path early return.
          (setq carriage-doc-state--fold--last-scan-tick tick)
          (setq carriage-doc-state--fold--last-scan-env env)
          (setq carriage-doc-state--fold--last-scan-had-lines
                (if (or st (and (listp fps) (> (length fps) 0))) t nil)))))

    ;; Critical: restore folded/revealed according to current point after refresh.
    (carriage-doc-state--fold--apply-for-point)
    t))

(defun carriage-doc-state--fold--schedule-refresh (beg end _len)
  "Coalesced schedule of fold overlay refresh (perf critical).

Hard performance contract:
- Runs on EVERY edit, often per character => MUST be O(1).
- MUST NOT trigger refresh for ordinary typing in the body.
- Schedules refresh ONLY when the edit touches doc-state lines:
  - #+PROPERTY: CARRIAGE_STATE ...
  - #+CARRIAGE_FINGERPRINT: ...

Debounce/coalescing semantics:
- Never cancel+recreate timers on each keystroke.
- If a timer exists, only postpone its idle time and update `carriage-doc-state--fold--pending-kind'.
- Timer callback does ONE refresh for visible buffers, then clears pending state."
  (when (and carriage-doc-state--fold-enabled
             (number-or-marker-p beg)
             (number-or-marker-p end))
    (let* ((secs (or carriage-doc-state-summary-debounce-seconds 0.4))
           (kind
            (save-excursion
              (let ((case-fold-search t)
                    (hit-state nil)
                    (hit-fp nil))
                (goto-char beg)
                (beginning-of-line)
                (setq hit-state (or hit-state (looking-at-p "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b")))
                (setq hit-fp    (or hit-fp    (looking-at-p "^[ \t]*#\\+CARRIAGE_FINGERPRINT\\b")))
                (goto-char end)
                (beginning-of-line)
                (setq hit-state (or hit-state (looking-at-p "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b")))
                (setq hit-fp    (or hit-fp    (looking-at-p "^[ \t]*#\\+CARRIAGE_FINGERPRINT\\b")))
                (cond
                 ((and hit-state hit-fp) 'both)
                 (hit-state 'state)
                 (hit-fp 'fingerprint)
                 (t nil))))))
      (when kind
        ;; Mark what needs refresh. This is consumed by refresh engine to avoid
        ;; expensive rescans/rebuilds when only one kind changed.
        (setq carriage-doc-state--fold--pending-kind
              (cond
               ;; Anything + both => both
               ((or (eq carriage-doc-state--fold--pending-kind 'both)
                    (eq kind 'both))
                'both)
               ;; First hit wins
               ((null carriage-doc-state--fold--pending-kind)
                kind)
               ;; Same kind stays as-is
               ((eq carriage-doc-state--fold--pending-kind kind)
                kind)
               ;; Different kinds coalesce to both
               (t 'both)))
        ;; Never do heavy work for invisible buffers.
        (when (get-buffer-window (current-buffer) t)
          (cond
           ((timerp carriage-doc-state--fold-refresh-timer)
            (ignore-errors
              (when (fboundp 'timer-set-idle-time)
                (timer-set-idle-time carriage-doc-state--fold-refresh-timer secs))))
           (t
            (let ((buf (current-buffer)))
              (setq carriage-doc-state--fold-refresh-timer
                    (run-with-idle-timer
                     secs nil
                     (lambda (&rest _ignored)
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (setq carriage-doc-state--fold-refresh-timer nil)
                           ;; Consume pending-kind; if another edit comes in, hook will set it again.
                           ;; Avoid running heavy refresh for hidden buffers.
                           (when (and carriage-doc-state--fold-enabled
                                      (get-buffer-window (current-buffer) t)
                                      carriage-doc-state--fold--pending-kind)
                             ;; IMPORTANT: do not clear pending-kind before refresh.
                             ;; `carriage-doc-state--fold--refresh-overlays' uses it to decide
                             ;; whether a full rescan is necessary. Clearing it early may skip
                             ;; rescans on fingerprint/state edits, leaving them without overlays.
                             (ignore-errors (carriage-doc-state--fold--refresh-overlays))
                             (setq carriage-doc-state--fold--pending-kind nil)))))
                     nil))))))))))

(defun carriage-doc-state-summary-enable ()
  "Enable summary folding for `CARRIAGE_STATE` and `CARRIAGE_FINGERPRINT` lines in current buffer."
  (interactive)
  (setq carriage-doc-state--fold-enabled t)
  (add-hook 'post-command-hook #'carriage-doc-state--fold--apply-for-point nil t)
  (add-hook 'after-change-functions #'carriage-doc-state--fold--schedule-refresh nil t)
  (carriage-doc-state--fold--refresh-overlays))

(defun carriage-doc-state-summary-disable ()
  "Disable summary folding in current buffer and remove fold overlays."
  (interactive)
  (setq carriage-doc-state--fold-enabled nil)
  (remove-hook 'post-command-hook #'carriage-doc-state--fold--apply-for-point t)
  (remove-hook 'after-change-functions #'carriage-doc-state--fold--schedule-refresh t)
  (when (timerp carriage-doc-state--fold-refresh-timer)
    (cancel-timer carriage-doc-state--fold-refresh-timer))
  (setq carriage-doc-state--fold-refresh-timer nil)
  (when (overlayp carriage-doc-state--fold-state-ov)
    (delete-overlay carriage-doc-state--fold-state-ov))
  (setq carriage-doc-state--fold-state-ov nil)
  (dolist (ov carriage-doc-state--fold-fp-ovs)
    (when (overlayp ov) (delete-overlay ov)))
  (setq carriage-doc-state--fold-fp-ovs nil))

;; Best-effort auto-enable: if carriage-mode is on (or user wants it), allow folding (display-based).
(add-hook 'carriage-mode-hook
          (lambda ()
            (when carriage-doc-state-summary-enable
              (ignore-errors (carriage-doc-state-summary-enable)))))

;; Legacy shim: keep old entry-point name working with the new fold UI.
;;;###autoload
(defun carriage-doc-state-hide (&optional buffer)
  "Legacy alias: enable summary fold UI and refresh.
Best-effort: safe to call even when overlays are already installed."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (ignore-errors (carriage-doc-state-summary-enable))
    (ignore-errors (carriage-doc-state-summary-refresh (current-buffer)))
    t))

(provide 'carriage-doc-state)
;;; carriage-doc-state.el ends here
