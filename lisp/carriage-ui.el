;;; carriage-ui.el --- UI widgets, header-line and mode-line helpers  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: ui, modeline
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/ui-v2.org
;;   spec/keyspec-v2.org
;;   spec/i18n-v2.org
;;   spec/document-branching-and-templates-v1.org
;;   spec/logging-v2.org
;;
;;; Commentary:
;; Header-line, mode-line and transient/menu UI helpers.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-llm-registry)
(require 'carriage-pricing nil t)
(require 'carriage-perf nil t)
(require 'carriage-transient-async nil t)
(declare-function carriage-select-apply-engine "carriage-apply-engine" (&optional engine))

(defgroup carriage-ui nil
  "UI components for Carriage."
  :group 'applications
  :prefix "carriage-")

(defcustom carriage-mode-icon-v-adjust -0.00
  "Vertical offset for all-the-icons in Carriage modeline/header-line.
Negative values move icons up; positive move them down."
  :type 'number
  :group 'carriage-ui)

(defcustom carriage-mode-icon-height 0.82
  "Uniform icon height scale for all-the-icons in mode-line/header-line."
  :type 'number
  :group 'carriage-ui)

;; Debug logging controls
(defcustom carriage-ui-debug nil
  "When non-nil, log debug info about icon faces/colors in modeline."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-ui-debug-max 200
  "Max number of debug log entries to emit per session."
  :type 'integer
  :group 'carriage-ui)

(defvar carriage-ui--debug-count 0
  "Counter of emitted debug log entries.")

(defun carriage-ui--dbg (fmt &rest args)
  "Internal logger with throttling for Carriage UI."
  (when (and carriage-ui-debug
             (or (not (numberp carriage-ui-debug-max))
                 (< carriage-ui--debug-count carriage-ui-debug-max)))
    (setq carriage-ui--debug-count (1+ carriage-ui--debug-count))
    (apply #'message (concat "[carriage-ui] " fmt) args)))

(defun carriage-ui--faceprop-foreground (fp)
  "Extract foreground color from face property FP (symbol/list/plist)."
  (cond
   ((null fp) nil)
   ((symbolp fp) (ignore-errors (face-attribute fp :foreground nil 'default)))
   ((and (listp fp) (keywordp (car fp))) (plist-get fp :foreground))
   ((listp fp)
    (let* ((fg nil))
      (dolist (el fp)
        (unless fg
          (setq fg (carriage-ui--faceprop-foreground el))))
      fg))
   (t nil)))

(defun carriage-ui--log-face-prop (tag fp)
  "Log face property FP with TAG."
  (carriage-ui--dbg "%s face=%s fg=%s"
                    tag
                    (prin1-to-string fp)
                    (or (carriage-ui--faceprop-foreground fp) "-")))

(defun carriage-ui--log-face-of-string (tag s)
  "Log face property of string S with TAG."
  (when (and (stringp s) (> (length s) 0))
    (carriage-ui--log-face-prop tag (get-text-property 0 'face s))))

;; Harmonious palette with explicit foregrounds (avoid theme desaturation to gray)
(defface carriage-ui-accent-blue-face
  '((t :inherit nil :foreground "#6fa8dc"))
  "Accent face (blue-ish) for UI icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-green-face
  '((t :inherit nil :foreground "#93c47d"))
  "Accent face (green) for success/apply icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-yellow-face
  '((t :inherit nil :foreground "#f1c232"))
  "Accent face (yellow) for caution/WIP icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-red-face
  '((t :inherit nil :foreground "#e06666"))
  "Accent face (red) for abort/error icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-purple-face
  '((t :inherit nil :foreground "#8e7cc3"))
  "Accent face (purple) for code/report/confirm icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-orange-face
  '((t :inherit nil :foreground "#f6b26b"))
  "Accent face (orange) for diff/dry icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-cyan-face
  '((t :inherit nil :foreground "#76a5af"))
  "Accent face (cyan/teal) for model/reset/icons."
  :group 'carriage-ui)

(defface carriage-ui-muted-face
  '((t :inherit nil :foreground "#9e9e9e"))
  "Muted face for disabled toggle icons."
  :group 'carriage-ui)

;; State faces (UI v1.3)
(defface carriage-ui-state-idle-face
  '((t :inherit nil :foreground "#268bd2"))
  "Mode-line face for idle state (blue)."
  :group 'carriage-ui)

(defface carriage-ui-state-sending-face
  '((t :inherit nil :foreground "#93c47d"))
  "Mode-line face for sending/streaming states (green)."
  :group 'carriage-ui)

(defface carriage-ui-state-error-face
  '((t :inherit nil :foreground "#e06666"))
  "Mode-line face for error state (red)."
  :group 'carriage-ui)

;; New state faces for refined color mapping
(defface carriage-ui-state-success-face
  '((t :inherit nil :foreground "#93c47d"))
  "Mode-line face for success/idle/done states (green)."
  :group 'carriage-ui)

(defface carriage-ui-state-active-face
  '((t :inherit nil :foreground "#f6b26b"))
  "Mode-line face for active reasoning/waiting/streaming/dispatch states (orange)."
  :group 'carriage-ui)

;; Faces for patch block highlighting (spec/ui-v2.org)
(defface carriage-patch-valid-face
  '((t :inherit nil :background "#203a24"))
  "Face for visually marking valid patch blocks."
  :group 'carriage-ui)

(defface carriage-patch-warning-face
  '((t :inherit nil :background "#3a2f20"))
  "Face for visually marking suspicious patch blocks."
  :group 'carriage-ui)

(defface carriage-patch-error-face
  '((t :inherit nil :background "#3a2020"))
  "Face for visually marking erroneous patch blocks."
  :group 'carriage-ui)

;; Faces for report rows (OK/WARN/ERR)
(defface carriage-report-ok-face
  '((t :inherit success))
  "Face for OK rows in report."
  :group 'carriage-ui)

(defface carriage-report-warn-face
  '((t :inherit warning))
  "Face for WARN rows in report."
  :group 'carriage-ui)

(defface carriage-report-err-face
  '((t :inherit error))
  "Face for ERR rows in report."
  :group 'carriage-ui)

;; carriage-mode-map moved to carriage-mode.el (UI must not define keymaps; keys go via keyspec)

(defvar-local carriage--ui-state 'idle
  "Current UI state: one of 'idle 'sending 'streaming 'dispatch 'waiting 'reasoning 'done 'error.

NOTE: In v2 UI we treat this as *request/transport* state. Local apply/dry-run
status is tracked separately and rendered in a separate modeline badge
`apply-status' to avoid mixing request errors with apply errors.")

;; Separate apply/dry-run status (must not mix with request state).
(defvar-local carriage--ui-apply-state 'none
  "Last known apply/dry-run UI state.
One of:
- 'none (no runs yet)
- 'running
- 'dry-ok | 'dry-fail
- 'apply-ok | 'apply-fail
- 'aborted")

(defvar-local carriage--ui-apply-tooltip nil
  "Cached tooltip string for apply-status badge (help-echo).")

(defvar-local carriage--ui-apply-last-report nil
  "Last dry-run/apply report plist used to populate apply-status badge (best-effort).")

(defvar-local carriage-ui--apply-badge-version 0
  "Monotonic version for apply-status badge; used to invalidate modeline cache.")

;; -----------------------------------------------------------------------------
;; Doc-cost cache (sum of per-request costs from fingerprint lines).
;; Public API is required by spec/ui-cost-v2.org and ERT tests.

(defvar-local carriage-ui--doc-cost-cache nil
  "Cached snapshot plist for document cost.
Keys: :known-total-u :known-count :unknown-count :ts")

(defvar-local carriage-ui--doc-cost-version 0
  "Monotonic version of doc-cost cache; bump on refresh to invalidate modeline cache.")

(defvar-local carriage-ui--doc-cost-refresh-timer nil
  "Debounce timer for doc-cost refresh (buffer-local).")

(defun carriage-ui-doc-cost-get ()
  "Return cached document cost snapshot plist.

Plist keys:
- :known-total-u integer (µ₽) or 0
- :known-count integer
- :unknown-count integer
- :ts float-time (last refresh)"
  (or carriage-ui--doc-cost-cache
      (list :known-total-u 0 :known-count 0 :unknown-count 0 :ts nil)))

(defun carriage-ui-doc-cost-refresh-now (&optional buffer)
  "Recompute and cache document cost for BUFFER (or current buffer).
May scan buffer text; must not be called from redisplay."
  (with-current-buffer (or buffer (current-buffer))
    (let ((sum 0)
          (known 0)
          (unknown 0)
          (case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT:[ \t]*\\(.*\\)$" nil t)
          (let* ((s (match-string 1))
                 (obj (condition-case _e
                          (car (read-from-string s))
                        (error nil)))
                 (v (and (listp obj) (plist-get obj :CAR_COST_TOTAL_U))))
            (cond
             ((integerp v)
              (setq sum (+ sum v))
              (setq known (1+ known)))
             ((and (listp obj) (plist-member obj :CAR_COST_TOTAL_U))
              ;; Explicit nil (unknown cost)
              (setq unknown (1+ unknown)))
             (t nil)))))
      (setq carriage-ui--doc-cost-cache
            (list :known-total-u sum
                  :known-count known
                  :unknown-count unknown
                  :ts (float-time)))
      (setq carriage-ui--doc-cost-version (1+ (or carriage-ui--doc-cost-version 0)))
      (carriage-ui--invalidate-ml-cache)
      (force-mode-line-update t)
      t)))

(defun carriage-ui-doc-cost-schedule-refresh (&optional delay)
  "Schedule a debounced doc-cost refresh for the current buffer."
  (when (timerp carriage-ui--doc-cost-refresh-timer)
    (ignore-errors (cancel-timer carriage-ui--doc-cost-refresh-timer)))
  (let* ((buf (current-buffer))
         (d (max 0.01 (float (or delay 0.08)))))
    (setq carriage-ui--doc-cost-refresh-timer
          (run-at-time
           d nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq carriage-ui--doc-cost-refresh-timer nil)
                 (ignore-errors (carriage-ui-doc-cost-refresh-now buf))))))
          ))
  t)

;; Spinner state (buffer-local)
(defvar-local carriage--ui-spinner-timer nil
  "Buffer-local spinner timer for Carriage mode-line.")
(defvar-local carriage--ui-spinner-index 0
  "Current spinner frame index (buffer-local).")
(defconst carriage--ui-spinner-frames-unicode
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Unicode spinner frames.")
(defconst carriage--ui-spinner-frames-ascii
  ["-" "\\" "|" "/"]
  "ASCII spinner frames (TTY fallback).")

(defun carriage-ui--spinner-frames ()
  "Return vector of spinner frames appropriate for current display."
  (if (display-graphic-p)
      carriage--ui-spinner-frames-unicode
    carriage--ui-spinner-frames-ascii))

(defun carriage-ui--spinner-char ()
  "Return current spinner frame as string."
  (let* ((frames (carriage-ui--spinner-frames))
         (n (length frames))
         (i (mod (or carriage--ui-spinner-index 0) (max 1 n))))
    (aref frames i)))

(defun carriage-ui--spinner-tick (buf)
  "Advance spinner in BUF and update mode-line.
Update only when BUF is visible; avoid forcing window repaints."
  (let ((wins (and (buffer-live-p buf) (get-buffer-window-list buf t t))))
    (when wins
      (with-current-buffer buf
        (setq carriage--ui-spinner-index (1+ carriage--ui-spinner-index))
        ;; Local refresh only; avoid repainting windows explicitly.
        (force-mode-line-update)))))

(defun carriage-ui--spinner-start ()
  "Start buffer-local spinner timer if not running."
  (unless (or (bound-and-true-p noninteractive)
              carriage--ui-spinner-timer
              (not carriage-ui-enable-spinner))
    (setq carriage--ui-spinner-index 0)
    (let* ((buf (current-buffer))
           (interval (or (and (boundp 'carriage-mode-spinner-interval)
                              carriage-mode-spinner-interval)
                         0.08)))
      (setq carriage--ui-spinner-timer
            (run-at-time 0 interval
                         (lambda ()
                           (carriage-ui--spinner-tick buf)))))))

(defun carriage-ui--spinner-stop (&optional reset)
  "Stop buffer-local spinner timer. When RESET, also zero index."
  (when (timerp carriage--ui-spinner-timer)
    (cancel-timer carriage--ui-spinner-timer))
  (setq carriage--ui-spinner-timer nil)
  (when reset
    (setq carriage--ui-spinner-index 0))
  ;; Local refresh only; avoid repainting all windows.
  (force-mode-line-update))

(defun carriage-ui-reset-timers ()
  "Cancel Carriage UI timers/overlays in this buffer and refresh the modeline.
Useful when input feels frozen (e.g., during rapid cursor movement)."
  (interactive)
  ;; Also cancel any stale ctx timers created by older Carriage versions (may have wrong arglists).
  (ignore-errors
    (when (fboundp 'carriage-ui--purge-stale-ctx-timers)
      (carriage-ui--purge-stale-ctx-timers)))
  (condition-case _e
      (progn
        ;; Context refresh timer (buffer-local)
        (when (timerp carriage-ui--ctx-refresh-timer)
          (cancel-timer carriage-ui--ctx-refresh-timer))
        (setq carriage-ui--ctx-refresh-timer nil)
        ;; Context badge refresh timer (final impl; buffer-local).
        ;; If a timer was cancelled elsewhere but pending flag wasn't cleared, the badge can
        ;; get stuck at [Ctx:?] forever. Reset both timer and pending state here.
        (when (boundp 'carriage-ui--ctx-badge-refresh-timer)
          (when (timerp carriage-ui--ctx-badge-refresh-timer)
            (cancel-timer carriage-ui--ctx-badge-refresh-timer))
          (setq carriage-ui--ctx-badge-refresh-timer nil))
        (when (boundp 'carriage-ui--ctx-badge-pending)
          (setq carriage-ui--ctx-badge-pending nil))
        (when (boundp 'carriage-ui--ctx-badge-last-time)
          (setq carriage-ui--ctx-badge-last-time 0.0))
        ;; Patch-count refresh timer (buffer-local)
        (when (timerp carriage-ui--patch-refresh-timer)
          (cancel-timer carriage-ui--patch-refresh-timer))
        (setq carriage-ui--patch-refresh-timer nil)
        ;; Spinner timer (buffer-local)
        (when (timerp carriage--ui-spinner-timer)
          (cancel-timer carriage--ui-spinner-timer))
        (setq carriage--ui-spinner-timer nil
              carriage--ui-spinner-index 0)
        ;; Header-line idle timer (buffer-local)
        (when (and (boundp 'carriage-ui--headerline-idle-timer)
                   (timerp carriage-ui--headerline-idle-timer))
          (cancel-timer carriage-ui--headerline-idle-timer))
        (setq carriage-ui--headerline-idle-timer nil)
        ;; Preloader timer/overlay from carriage-mode (if present)
        (when (boundp 'carriage--preloader-timer)
          (when (timerp carriage--preloader-timer)
            (cancel-timer carriage--preloader-timer))
          (setq carriage--preloader-timer nil))
        (when (and (boundp 'carriage--preloader-overlay)
                   (overlayp carriage--preloader-overlay))
          (delete-overlay carriage--preloader-overlay)
          (setq carriage--preloader-overlay nil))
        (carriage-ui--invalidate-ml-cache)
        (force-mode-line-update)
        t)
    (error nil)))

(defun carriage-ui-set-state (state &optional tooltip)
  "Set UI STATE symbol for mode-line visuals and manage spinner.

When TOOLTIP is a string, set it as help-echo for the [STATE] segment.
State change invalidates modeline cache and updates spinner lifecycle."
  (setq carriage--ui-state (or state 'idle))
  ;; Invalidate modeline cache on state changes to avoid stale render
  (setq carriage-ui--ml-cache nil
        carriage-ui--ml-cache-key nil)
  ;; Optional tooltip update
  (when (and (stringp tooltip) (> (length tooltip) 0))
    (setq carriage--ui-state-tooltip tooltip))
  (pcase carriage--ui-state
    ;; spinner for active phases (include reasoning/waiting/streaming/dispatch/sending)
    ((or 'sending 'streaming 'dispatch 'waiting 'reasoning)
     (carriage-ui--spinner-start))
    ;; no spinner for preparation/done/others
    ((or 'prompt 'context 'done)
     (carriage-ui--spinner-stop nil))
    (_
     (carriage-ui--spinner-stop t)))
  (force-mode-line-update))

(defun carriage-ui-state-lighter ()
  "Return a short lighter suffix for current =carriage--ui-state'."
  (pcase carriage--ui-state
    ('idle "")
    ('context " prep")
    ('prompt " prep")
    ('dispatch " req")
    ('waiting " wait")
    ('sending " snd")
    ('reasoning " rzn")
    ('streaming " str")
    ('done " done")

    ('error " ERR")
    (_ "")))

(defun carriage-ui--state-label (state)
  "Return human-readable label for STATE."
  (pcase state
    ('idle "Idle")
    ;; Preparation phases
    ('context "Prepare")
    ('prompt "Prepare")
    ;; Network phases
    ('dispatch "Request")
    ('waiting "Waiting")
    ('sending "Sending")
    ;; Reasoning/stream phases
    ('reasoning "Reasoning")
    ('streaming "Streaming")
    ;; Completion
    ('done "Done")
    ('error "Error")
    (_ (capitalize (symbol-name (or state 'idle))))))

(defcustom carriage-ui-context-cache-ttl nil
  "Maximum age in seconds for cached context badge computations in the mode-line.
Set to 0 to recompute on every redisplay; set to nil to keep values until other cache
keys change (buffer content, toggle states)."
  :type '(choice (const :tag "Disable caching" 0)
                 (number :tag "TTL (seconds)")
                 (const :tag "Unlimited (until buffer changes)" nil))
  :group 'carriage-ui)

(defcustom carriage-ui-context-async-refresh t
  "When non-nil, compute the [Ctx:N] badge off the redisplay path on an idle timer.
The modeline uses the last cached value and schedules a lightweight refresh."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-ui-context-refresh-delay 0.05
  "Idle delay (seconds) before recomputing context badge when cache is stale."
  :type 'number
  :group 'carriage-ui)

(defcustom carriage-ui-context-min-refresh-interval 1.0
  "Minimum interval in seconds between actual context badge recomputations per buffer.

This is a hard throttle on top of debounce and guarantees recomputation frequency
is at most 1 Hz (default)."
  :type 'number
  :group 'carriage-ui)

(defcustom carriage-ui-context-tooltip-max-items 50
  "Maximum number of context items to list in the [Ctx:N] tooltip.
When more are present, the tooltip shows a tail line like \"… (+K more)\"."
  :type 'integer
  :group 'carriage-ui)

(defcustom carriage-ui-enable-spinner nil
  "When non-nil, animate a spinner in the [State] segment and tick the mode-line.
Disabling this eliminates periodic redisplay work during active phases."
  :type 'boolean
  :group 'carriage-ui)

(defconst carriage-ui--modeline-default-blocks
  '(
    intent
    state
    apply-status
    doc-cost
    abort
    all
    context
    toggle-visible
    toggle-ctx
    toggle-patched
    toggle-files
    doc-scope-all
    doc-scope-last
    model
    ;; report
    ;; patch
    ;; branch
    ;; save
    ;; settings
    )
  "Default order of Carriage modeline blocks.")

(defun carriage-ui--invalidate-icon-cache-all-buffers ()
  "Invalidate icon caches in all live buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (boundp 'carriage-ui--icon-cache-env)
          (setq carriage-ui--icon-cache nil)
          (setq carriage-ui--icon-cache-env nil))
        ;; Drop modeline cache as well (theme/face/icon env changed)
        (when (boundp 'carriage-ui--ml-cache)
          (setq carriage-ui--ml-cache nil)
          (setq carriage-ui--ml-cache-key nil))
        ;; Drop header-line cache too (icons/theme affect it)
        (when (boundp 'carriage-ui--hl-cache)
          (setq carriage-ui--hl-cache nil)
          (setq carriage-ui--hl-cache-key nil))))))

(defun carriage-ui--set-modeline-blocks (sym val)
  "Setter for `carriage-ui-modeline-blocks' that refreshes modelines everywhere."
  (set-default sym val)
  (carriage-ui--invalidate-icon-cache-all-buffers)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (force-mode-line-update t)))))

(defun carriage-ui-reset-modeline-blocks ()
  "Reset `carriage-ui-modeline-blocks' to defaults and refresh modelines."
  (interactive)
  (customize-set-variable 'carriage-ui-modeline-blocks carriage-ui--modeline-default-blocks))

(defcustom carriage-ui-modeline-blocks carriage-ui--modeline-default-blocks
  "List of symbols describing the Carriage modeline blocks and their order.

Recognized block symbols:
- `suite' — suite selector.
- `engine' — apply engine selector.
- `branch' — current VCS branch badge.
- `model' — model/backend selector.
- `intent' — Ask/Code/Hybrid intent toggle.
- `state' — Request/transport state indicator with spinner.
- `apply-status' — last dry-run/apply status badge (separate from request state).
- `context' — context badge.
- `patch' — patch block counter.
- `all' — Apply last iteration button.
- `abort' — Abort button.
- `report' — Report buffer shortcut.
- `toggle-ctx' — GPT context toggle.
- `toggle-files' — doc context toggle.
- `doc-scope-all' — select doc-context scope 'all.
- `doc-scope-last' — select doc-context scope 'last.
- `toggle-visible' — include visible buffers.
- `settings' — settings/menu button.
carriage-ui--set-modeline-blocks
Unknown symbols are ignored."
  :type '(repeat (choice (const :tag "Suite selector" suite)
                         (const :tag "Engine selector" engine)
                         (const :tag "Branch badge" branch)
                         (const :tag "Model selector" model)
                         (const :tag "Intent toggle" intent)
                         (const :tag "Request state indicator" state)
                         (const :tag "Apply/Dry-run status" apply-status)
                         (const :tag "Context badge" context)
                         (const :tag "Patch counter" patch)
                         (const :tag "Apply last iteration button" all)
                         (const :tag "Diff button" diff)
                         (const :tag "Ediff button" ediff)
                         (const :tag "Abort button" abort)
                         (const :tag "Report button" report)
                         (const :tag "GPT context toggle" toggle-ctx)
                         (const :tag "Doc context toggle" toggle-files)
                         (const :tag "Doc scope: all" doc-scope-all)
                         (const :tag "Doc scope: last" doc-scope-last)
                         (const :tag "Patched files toggle" toggle-patched)
                         (const :tag "Visible-buffers toggle" toggle-visible)
                         (const :tag "Settings button" settings)))
  :set #'carriage-ui--set-modeline-blocks
  :group 'carriage-ui)

(defvar-local carriage-ui--model-block-cache nil
  "Cached (label . help) tuple for the mode-line model segment.")

(defvar-local carriage-ui--model-block-cache-key nil
  "Key signature used to compute `carriage-ui--model-block-cache'.")

(defvar-local carriage-ui--ctx-cache nil
  "Buffer-local cache for context badge computation.
Plist keys: :doc :gpt :tick :time :value.")

(defvar-local carriage-ui--ctx-refresh-timer nil
  "Timer for asynchronous context badge refresh (buffer-local).")

(defvar-local carriage-ui--ctx-pending-toggles nil
  "Last requested toggles snapshot for a pending async context badge refresh.")

(defvar-local carriage-ui--ctx-pending-tick nil
  "Last requested buffer tick for a pending async context badge refresh.")

(defvar-local carriage-ui--ctx-badge-version 0
  "Monotonic version of context badge state; bumps on real changes to avoid work on redisplay.")

(defvar-local carriage-ui--ctx-last-placeholder-time 0
  "Timestamp (float seconds) when a [Ctx:?] placeholder was last returned for this buffer.")

(defvar-local carriage-ui--ctx-placeholder-count 0
  "How many consecutive times [Ctx:?] was returned without a successful refresh.")

(defvar-local carriage-ui--ml-cache nil
  "Cached rendered modeline string for Carriage (buffer-local).")

(defvar-local carriage-ui--ml-cache-key nil
  "Signature (key) of the last rendered modeline for quick short-circuit.")

(defvar-local carriage-ui--button-cache nil
  "Cache of clickable modeline button strings keyed by (label help fn).")

(defvar-local carriage--mode-modeline-string nil
  "Precomputed Carriage modeline string for non-:eval mode-line segment.")

(defvar-local carriage-ui--ml-stale-p t
  "When non-nil, the precomputed modeline string is considered stale and must be rebuilt.")

(defun carriage-ui--invalidate-ml-cache ()
  "Invalidate cached modeline/button strings for the current buffer."
  (setq carriage-ui--ml-cache nil
        carriage-ui--ml-cache-key nil)
  (setq carriage-ui--button-cache (make-hash-table :test 'equal))
  (setq carriage-ui--ml-stale-p t))

(defun carriage-ui--reset-context-cache (&optional buffer)
  "Mark cached context badge for BUFFER (or current buffer) as stale and bump version.

Important UX/perf rule:
- Do NOT drop the last known badge value (avoid Ctx:? flicker).
- Instead, keep :value and force a refresh by setting :tick to -1."
  (let ((do-reset
         (lambda ()
           (let* ((old carriage-ui--ctx-cache)
                  (val (and (listp old) (plist-get old :value)))
                  (tog (carriage-ui--context-toggle-states)))
             ;; Preserve the last rendered value if present, but make cache invalid.
             (setq carriage-ui--ctx-cache
                   (if val
                       (carriage-ui--ctx-build-cache tog -1 0.0 val)
                     nil))
             (setq carriage-ui--ctx-placeholder-count 0
                   carriage-ui--ctx-last-placeholder-time 0)
             (when (timerp carriage-ui--ctx-refresh-timer)
               (ignore-errors (cancel-timer carriage-ui--ctx-refresh-timer)))
             (setq carriage-ui--ctx-refresh-timer nil)
             (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))))))
    (if buffer
        (with-current-buffer buffer (funcall do-reset))
      (funcall do-reset))))

(defun carriage-ui--ctx-invalidate ()
  "Invalidate context badge cache, bump version and refresh the modeline."
  (interactive)
  (carriage-ui--reset-context-cache)
  ;; Schedule a refresh explicitly so toggles/scope changes update Ctx even if redisplay
  ;; path avoids rescheduling (perf invariant).
  (let* ((toggles (carriage-ui--context-toggle-states))
         (tick (buffer-chars-modified-tick))
         (delay (or (and (boundp 'carriage-ui-context-refresh-delay)
                         carriage-ui-context-refresh-delay)
                    0.05)))
    (when (fboundp 'carriage-ui--ctx-schedule-refresh)
      (ignore-errors (carriage-ui--ctx-schedule-refresh toggles tick delay))))
  (carriage-ui--invalidate-ml-cache)
  (force-mode-line-update))

(defun carriage-ui--context-item->line (item)
  "Format ITEM from carriage-context-count into a single tooltip line."
  (let* ((p   (plist-get item :path))
         (src   (pcase (plist-get item :source)
                  ('doc "doc") ('gptel "gptel") ('both "both") ('visible "visible") (_ "?")))
         (inc (plist-get item :included))
         (rsn (plist-get item :reason)))
    (concat " - [" src "] " (or p "-")
            (unless inc
              (format " (content omitted%s)" (if rsn (format ": %s" rsn) ""))))))

(defun carriage-ui--context-build-tooltip (inc-doc inc-gpt cnt items warns limit)
  "Build multi-line tooltip text for [Ctx:N].
INC-DOC/INC-GPT are toggle states for doc/gptel sources.
CNT is the count from collector, ITEMS/WARNS from carriage-context-count.
LIMIT controls max number of items shown; nil or <=0 means no limit."
  (let* ((n (length items))
         (shown (if (and (integerp limit) (> limit 0))
                    (cl-subseq items 0 (min n limit))
                  items))
         (more (max 0 (- n (length shown))))
         (header (format "Контекст: файлов=%d — источники: doc=%s, gptel=%s"
                         cnt (if inc-doc "on" "off") (if inc-gpt "on" "off")))
         (warn-lines (when warns
                       (cons "Предупреждения:"
                             (mapcar (lambda (w) (concat " - " w)) warns))))
         (item-lines (when shown
                       (cons "Элементы:"
                             (append (mapcar #'carriage-ui--context-item->line shown)
                                     (when (> more 0)
                                       (list (format "… (+%d more)" more))))))))
    (mapconcat #'identity
               (delq nil (list header
                               (and warn-lines (mapconcat #'identity warn-lines "\n"))
                               (and item-lines (mapconcat #'identity item-lines "\n"))))
               "\n")))

(defun carriage-ui--compute-context-badge (inc-doc inc-gpt inc-vis inc-patched)
  "Compute context badge (LABEL . HINT) for INC-DOC/INC-GPT/INC-VIS/INC-PATCHED toggles.
 Основано на carriage-context-count — показывает ровно то, что уйдёт в запрос."
  (require 'carriage-context nil t)
  (let* ((off (not (or inc-doc inc-gpt inc-vis inc-patched))))
    (if off
        (cons "Ctx:-" "Контекст выключен (doc=off, gptel=off, vis=off, patched=off)")
      ;; Ensure a fast, UI-safe context counter is available.
      ;; MUST NOT read file contents; see `carriage-context-count-fast'.
      (ignore-errors (require 'carriage-context-fast nil t))
      (ignore-errors (require 'carriage-context nil t))
      (let* ((count-fn (cond
                        ((fboundp 'carriage-context-count-fast) #'carriage-context-count-fast)
                        ((fboundp 'carriage-context-count)      #'carriage-context-count)
                        (t nil)))
             (res (and count-fn
                       (ignore-errors
                         ;; Point must not affect the context count; doc-scope='last is tail-most.
                         (funcall count-fn (current-buffer) nil))))
             (cnt  (or (and (listp res) (plist-get res :count)) 0))
             (items (or (and (listp res) (plist-get res :items)) '()))
             (warns (or (and (listp res) (plist-get res :warnings)) '()))
             (limit (or (and (boundp 'carriage-ui-context-tooltip-max-items)
                             carriage-ui-context-tooltip-max-items)
                        50))
             (n (length items))
             (shown (if (and (integerp limit) (> limit 0))
                        (cl-subseq items 0 (min n limit))
                      items))
             (more (max 0 (- n (length shown))))
             (head-line (format "Контекст: файлов=%d — источники: doc=%s, gptel=%s, vis=%s, patched=%s, scope=%s, profile=%s"
                                cnt
                                (if inc-doc "on" "off")
                                (if inc-gpt "on" "off")
                                (if inc-vis "on" "off")
                                (if inc-patched "on" "off")
                                (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                                  (if (eq sc 'last) "last" "all"))
                                (let ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)))
                                  (if (eq pr 'p3) "P3" "P1"))))
             (warn-lines (when warns
                           (cons "Предупреждения:"
                                 (mapcar (lambda (w) (concat " - " w)) warns))))
             (item-lines (when shown
                           (cons "Элементы:"
                                 (append (mapcar #'carriage-ui--context-item->line shown)
                                         (when (> more 0)
                                           (list (format "… (+%d more)" more))))))))
        (carriage-ui--dbg "Ctx badge: inc-doc=%s inc-gpt=%s inc-vis=%s inc-patched=%s scope=%s cnt=%s items=%s warns=%s"
                          inc-doc inc-gpt inc-vis inc-patched
                          (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                            (if (eq sc 'last) "last" "all"))
                          cnt n (length warns))
        (cons (format "Ctx:%d" cnt)
              (mapconcat #'identity
                         (delq nil (list head-line
                                         ;; Profile/limits line with explicit warning for P3
                                         (let* ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile))
                                                (mf (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files))
                                                (mb (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes)))
                                           (format "Профиль: %s — лимиты: файлы=%s байты=%s%s"
                                                   (if (eq pr 'p3) "P3-debug" "P1-core")
                                                   (or mf "-") (or mb "-")
                                                   (if (eq pr 'p3) " (внимание: расширенный бюджет — выше стоимость/шум)" "")))
                                         (and warn-lines (mapconcat #'identity warn-lines "\n"))
                                         (and item-lines (mapconcat #'identity item-lines "\n"))))
                         "\n"))))))

(defun carriage-ui--context-toggle-states ()
  "Return plist of current context toggle states and scope.
Keys: :doc :gpt :vis :patched :scope

Note: Avoids requiring carriage-context in the redisplay path; relies on boundp with defaults."
  (list
   :doc (if (boundp 'carriage-mode-include-doc-context)
            carriage-mode-include-doc-context
          t)
   :gpt (if (boundp 'carriage-mode-include-gptel-context)
            carriage-mode-include-gptel-context
          nil)
   :vis (and (boundp 'carriage-mode-include-visible-context)
             carriage-mode-include-visible-context)
   :patched (and (boundp 'carriage-mode-include-patched-files)
                 carriage-mode-include-patched-files)
   :gptver (if (boundp 'carriage-ui--gptel-context-version)
               carriage-ui--gptel-context-version
             0)
   :scope (and (boundp 'carriage-doc-context-scope)
               carriage-doc-context-scope)))

(defun carriage-ui--ctx-cache-ttl-ok-p (cache now ttl)
  "Return non-nil when CACHE age is within TTL. NIL TTL means unlimited."
  (or (null ttl)
      (and (numberp ttl) (> ttl 0)
           cache
           (< (- now (or (plist-get cache :time) 0)) ttl))))

(defun carriage-ui--ctx-cache-valid-p (cache toggles tick now ttl)
  "Return non-nil when CACHE matches TOGGLES, TICK and TTL.

Robustness: older experimental code may accidentally store a string/cons in CACHE.
Never signal from redisplay/modeline; treat non-plists as invalid."
  (and (listp cache)
       (listp toggles)
       (eq (plist-get toggles :doc)     (plist-get cache :doc))
       (eq (plist-get toggles :gpt)     (plist-get cache :gpt))
       (eq (plist-get toggles :vis)     (plist-get cache :vis))
       (eq (plist-get toggles :scope)   (plist-get cache :scope))
       (eq (plist-get toggles :patched) (plist-get cache :patched))
       (=  (or (plist-get toggles :gptver) 0)
           (or (plist-get cache :gptver) 0))
       (=  tick (or (plist-get cache :tick) -1))
       (carriage-ui--ctx-cache-ttl-ok-p cache now ttl)))

(defun carriage-ui--ctx-build-cache (toggles tick time value)
  "Build a new context cache plist from TOGGLES, TICK, TIME and VALUE."
  (list :doc (plist-get toggles :doc)
        :gpt (plist-get toggles :gpt)
        :vis (plist-get toggles :vis)
        :scope (plist-get toggles :scope)
        :patched (plist-get toggles :patched)
        :gptver (or (plist-get toggles :gptver) 0)
        :tick tick :time time :value value))

(defun carriage-ui--ctx-schedule-refresh (toggles tick delay)
  "Schedule a near-future context badge recomputation with TOGGLES and TICK after DELAY.

Debounce semantics:
- Repeated calls overwrite `carriage-ui--ctx-pending-toggles' and `carriage-ui--ctx-pending-tick'.
- Only one timer should fire: we cancel any existing timer and reschedule.
- Uses `run-at-time' (not idle-only) to avoid starvation during continuous interaction."
  ;; Always keep the latest request.
  (setq carriage-ui--ctx-pending-toggles toggles)
  (setq carriage-ui--ctx-pending-tick tick)
  ;; Strict debounce: cancel previous timer and reschedule with the newest args.
  (when (timerp carriage-ui--ctx-refresh-timer)
    (ignore-errors (cancel-timer carriage-ui--ctx-refresh-timer))
    (setq carriage-ui--ctx-refresh-timer nil))
  (let* ((buf (current-buffer))
         (dl  (max 0.0 (or delay 0.05))))
    (setq carriage-ui--ctx-refresh-timer
          (run-at-time
           dl nil
           (lambda (&optional _ignored)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 ;; Always clear the timer first to avoid a stuck pending state.
                 (setq carriage-ui--ctx-refresh-timer nil)
                 ;; Skip background work for hidden buffers; keep last badge until visible again.
                 (when (get-buffer-window buf t)
                   (condition-case _e
                       (let* ((tog (or carriage-ui--ctx-pending-toggles
                                       (carriage-ui--context-toggle-states)))
                              (tk  (or carriage-ui--ctx-pending-tick
                                       (buffer-chars-modified-tick)))
                              (doc (plist-get tog :doc))
                              (gpt (plist-get tog :gpt))
                              (vis (plist-get tog :vis))
                              (pt  (plist-get tog :patched))
                              (val (carriage-ui--compute-context-badge doc gpt vis pt))
                              (t2 (float-time)))
                         (setq carriage-ui--ctx-cache
                               (carriage-ui--ctx-build-cache tog tk t2 val))
                         (setq carriage-ui--ctx-placeholder-count 0
                               carriage-ui--ctx-last-placeholder-time 0)
                         (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
                         (carriage-ui--invalidate-ml-cache))
                     (error nil))
                   (force-mode-line-update t)))))))))

(defun carriage-ui--ctx-force-sync (toggles tick time)
  "Synchronously recompute context badge for TOGGLES/TICK at TIME, update cache and return value."
  (let* ((doc (plist-get toggles :doc))
         (gpt (plist-get toggles :gpt))
         (vis (plist-get toggles :vis))
         (pt  (plist-get toggles :patched))
         (val (carriage-ui--compute-context-badge doc gpt vis pt))
         (old (and (listp carriage-ui--ctx-cache)
                   (plist-get carriage-ui--ctx-cache :value)))
         (changed (not (equal val old))))
    (setq carriage-ui--ctx-cache (carriage-ui--ctx-build-cache toggles tick time val))
    (setq carriage-ui--ctx-placeholder-count 0
          carriage-ui--ctx-last-placeholder-time 0
          carriage-ui--ctx-refresh-timer nil)
    (when changed
      (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
      (carriage-ui--invalidate-ml-cache))
    val))

(defun carriage-ui--context-badge ()
  "Return cached (LABEL . HINT) for [Ctx:N]; refresh asynchronously when stale.
Heavy recomputation is moved off the redisplay path to reduce churn.
Robustness: if async refresh stalls, fall back to a synchronous recompute after
several placeholder frames or a time threshold."
  (let* ((toggles (carriage-ui--context-toggle-states))
         (tick (buffer-chars-modified-tick))
         (now (float-time))
         (ttl carriage-ui-context-cache-ttl)
         (cache carriage-ui--ctx-cache)
         (valid (carriage-ui--ctx-cache-valid-p cache toggles tick now ttl)))
    (cond
     ;; Valid cache → return instantly
     (valid
      (when carriage-ui-debug
        (let* ((val (plist-get cache :value))
               (lbl (and (consp val) (car val))))
          (carriage-ui--dbg "ctx-badge: cache HIT (doc=%s gpt=%s vis=%s patched=%s tick=%s) → %s"
                            (plist-get toggles :doc)
                            (plist-get toggles :gpt)
                            (plist-get toggles :vis)
                            (plist-get toggles :patched)
                            tick lbl)))
      (plist-get cache :value))

     ;; Stale cache and async mode → schedule refresh, return last known value.
     ;; IMPORTANT: do not cancel/reschedule on every redisplay; schedule only once while a timer is pending.
     ((and carriage-ui-context-async-refresh cache)
      (unless (timerp carriage-ui--ctx-refresh-timer)
        (carriage-ui--ctx-schedule-refresh toggles tick carriage-ui-context-refresh-delay))
      (plist-get cache :value))

     ;; No cache yet and async mode → schedule refresh, show placeholder.
     ;; IMPORTANT (perf): never do a synchronous recompute from redisplay/modeline.
     (carriage-ui-context-async-refresh
      (let* ((t0 now)
             (cnt  (or carriage-ui--ctx-placeholder-count 0))
             (base-delay (max 0.0 (or carriage-ui-context-refresh-delay 0.05))))
        ;; IMPORTANT: avoid timer churn on redisplay; if a timer is already pending, just return placeholder.
        (unless (timerp carriage-ui--ctx-refresh-timer)
          (carriage-ui--ctx-schedule-refresh toggles tick base-delay))
        (setq carriage-ui--ctx-last-placeholder-time t0
              carriage-ui--ctx-placeholder-count (1+ cnt))
        (cons "Ctx:?" "Вычисление контекста…")))

     ;; Synchronous recompute fallback
     (t
      (carriage-ui--ctx-force-sync toggles tick now)))))

(defun carriage-ui-refresh-context-badge ()
  "Force refresh of the [Ctx:N] badge synchronously and update modeline."
  (interactive)
  (let* ((toggles (carriage-ui--context-toggle-states))
         (tick (buffer-chars-modified-tick))
         (now (float-time)))
    (carriage-ui--ctx-force-sync toggles tick now)
    (force-mode-line-update)))

(defun carriage-ui--ctx-after-change (_beg _end _len)
  "After-change hook: schedule a debounced context badge refresh.

Important: does NOT clear caches immediately and does not compute on the redisplay path.
Keeps the previous badge visible until the scheduled refresh completes."
  (when (and (boundp 'carriage-mode) (bound-and-true-p carriage-mode))
    (let* ((toggles (carriage-ui--context-toggle-states))
           (doc (plist-get toggles :doc))
           (gpt (plist-get toggles :gpt))
           (vis (plist-get toggles :vis))
           (pt  (plist-get toggles :patched)))
      (when (or doc gpt vis pt)
        (carriage-ui--ctx-schedule-refresh
         toggles
         (buffer-chars-modified-tick)
         (or (and (boundp 'carriage-ui-context-refresh-delay)
                  carriage-ui-context-refresh-delay)
             0.05))))))


;; Lightweight event-driven invalidation: update context badge on save/revert.
(defun carriage-ui--after-save-refresh ()
  "After-save/revert hook: invalidate context badge and refresh modeline for this buffer."
  ;; NOTE: this hook is installed globally; keep it effectively no-op unless
  ;; carriage-mode is actually active in this buffer.
  (when (and (bound-and-true-p carriage-mode)
             (boundp 'carriage-ui--ctx-cache))
    (carriage-ui--reset-context-cache)
    (carriage-ui--invalidate-ml-cache)
    (force-mode-line-update)))

;; NOTE: Hooks are installed buffer-locally by `carriage-mode` to avoid global overhead.

;; -------------------------------------------------------------------
;; Header-line and Mode-line builders (M3: icons (optional) + outline click)

(defun carriage-ui--truncate-middle (s max)
  "Truncate string S to MAX chars with a middle ellipsis if needed."
  (let ((len (length (or s ""))))
    (if (or (<= max 0) (<= len max))
        (or s "")
      (let* ((keep (max 1 (/ (- max 1) 2)))
             (left (substring s 0 keep))
             (right (substring s (- len keep))))
        (concat left "…" right)))))

(defun carriage-ui--project-name ()
  "Return cached project name (directory name of project root) or \"-\"."
  (or carriage-ui--project-name-cached
      (let* ((root (or (carriage-project-root) default-directory))
             (dir  (file-name-nondirectory (directory-file-name root)))
             (name (or (and dir (not (string-empty-p dir)) dir) "-")))
        (setq carriage-ui--project-name-cached name)
        name)))

(defun carriage-ui--org-outline-path ()
  "Return org outline path (A › B › C) at point, or nil if not available.
Prefer using Org's cache (use-cache=t); our hooks keep the value fresh."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-get-outline-path))
    ;; Use cache=t for speed; we recompute on heading changes via our hook.
    (let* ((path (ignore-errors (org-get-outline-path t nil))))
      (when (and path (listp path) (> (length path) 0))
        (mapconcat (lambda (s) (if (stringp s) s (format "%s" s))) path " › ")))))

(defun carriage-ui-goto-outline (&optional _event)
  "Go to the current org heading (best-effort) when clicking outline in header-line."
  (interactive "e")
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-find-exact-headline-in-buffer)
             (fboundp 'org-get-outline-path))
    (let* ((path (ignore-errors (org-get-outline-path t t)))
           (title (and path (car (last path))))
           (pos (and title (ignore-errors (org-find-exact-headline-in-buffer title)))))
      (when (number-or-marker-p pos)
        (goto-char pos)
        (recenter 1)))))

(defvar carriage-ui--icons-lib-available (featurep 'all-the-icons)
  "Cached availability of all-the-icons library.")

(defvar-local carriage-ui--icon-cache nil
  "Buffer-local cache of generated icon strings keyed by KEY or (toggle KEY ONP).")

(defvar-local carriage-ui--icon-cache-env nil
  "Environment snapshot for the icon cache to detect invalidation.
List of (gui use-icons height v-adjust themes).")

(defun carriage-ui--icon-cache-env-current ()
  "Return current environment signature for icon rendering."
  (list (display-graphic-p)
        (and (boundp 'carriage-mode-use-icons) carriage-mode-use-icons)
        carriage-mode-icon-height
        carriage-mode-icon-v-adjust
        custom-enabled-themes))

(defun carriage-ui--invalidate-icon-cache ()
  "Invalidate icon cache for the current buffer."
  (setq carriage-ui--icon-cache nil)
  (setq carriage-ui--icon-cache-env (carriage-ui--icon-cache-env-current)))

(defun carriage-ui--maybe-refresh-icon-cache-env ()
  "Ensure icon cache environment matches current UI; reset cache if not."
  (let ((cur (carriage-ui--icon-cache-env-current)))
    (unless (equal cur carriage-ui--icon-cache-env)
      (setq carriage-ui--icon-cache (make-hash-table :test 'equal))
      (setq carriage-ui--icon-cache-env cur))))



(defvar carriage-ui--icon-theme-hook-installed nil
  "Internal flag to install theme-change advice once.")

(unless carriage-ui--icon-theme-hook-installed
  (setq carriage-ui--icon-theme-hook-installed t)
  (advice-add 'load-theme :after (lambda (&rest _)
                                   (carriage-ui--invalidate-icon-cache-all-buffers))))

(defun carriage-ui--icons-available-p ()
  "Return non-nil when icons can be used in modeline."
  (let* ((use-flag (and (boundp 'carriage-mode-use-icons) carriage-mode-use-icons))
         (gui (display-graphic-p)))
    (unless carriage-ui--icons-lib-available
      (setq carriage-ui--icons-lib-available (require 'all-the-icons nil t)))
    (and use-flag gui carriage-ui--icons-lib-available)))

(defun carriage-ui--accent-hex (face)
  "Return final hexadecimal foreground color for FACE."
  (or (ignore-errors (face-foreground face nil 'default))
      (face-attribute face :foreground nil 'default)
      "#aaaaaa"))
(defun carriage-ui--icon (key)
  "Return icon string for KEY using all-the-icons, or nil if unavailable.
Results are cached per-buffer and invalidated when theme or UI parameters change."
  (when (carriage-ui--icons-available-p)
    (carriage-ui--maybe-refresh-icon-cache-env)
    (let* ((cache (or carriage-ui--icon-cache
                      (setq carriage-ui--icon-cache (make-hash-table :test 'equal))))
           (hit (gethash key cache)))
      (if (stringp hit)
          hit
        (let ((res
               (pcase key
                 ;; Intent
                 ('ask  (when (fboundp 'all-the-icons-material)
                          (all-the-icons-material "chat"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face)))))
                 ('patch (when (fboundp 'all-the-icons-material)
                           (all-the-icons-material "code"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('hybrid (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "merge_type"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ;; Model/backend (prefer Material; fallback to Octicon CPU)
                 ('model (cond
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "memory"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "cpu"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                          (t nil)))
                 ;; Header-line sections
                 ('project (cond
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "repo"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "folder"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            (t nil)))
                 ('file    (cond
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "file-text"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "description"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            (t nil)))
                 ('heading (cond
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "chevron_right"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face))))
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "bookmark"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face))))
                            (t nil)))
                 ('suite (cond
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "package"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "category"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          (t nil)))
                 ('engine (cond
                           ((fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "gear"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                           ((fboundp 'all-the-icons-material)
                            (all-the-icons-material "build"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                           (t nil)))
                 ;; Actions
                 ('dry    (when (fboundp 'all-the-icons-faicon)
                            (all-the-icons-faicon "flask"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-orange-face)))))
                 ('apply  (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "check_circle"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-green-face)))))
                 ('all    (cond
                           ((fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "rocket"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                           ((fboundp 'all-the-icons-material)
                            (all-the-icons-material "play_arrow"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                           (t nil)))
                 ('abort  (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "stop"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-red-face)))))
                 ('report (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "file-text"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('save   (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "publish"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
                 ('wip    (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "git-branch"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('commit (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "git-commit"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('reset  (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "history"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
                 ;; Icons for doc-state/fingerprint summary context flags
                 ;; (used by carriage-doc-state overlay fold badges).
                 ('files (when (fboundp 'all-the-icons-material)
                           (all-the-icons-material "featured_play_list"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.14)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('ctx (when (fboundp 'all-the-icons-material)
                         (all-the-icons-material "toc"
                                                 :height carriage-mode-icon-height
                                                 :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                 :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face)))))
                 ('visible (cond
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "visibility"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "eye"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            (t nil)))
                 ('patched (cond
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "code"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "checklist"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            (t nil)))
                 ;; Additional icons for doc-state badges (scope/profile/injection)
                 ;; Keep generic 'scope for other UI bits, and add explicit icons for modeline All/Last scope.
                 ('scope-all (when (fboundp 'all-the-icons-material)
                               (all-the-icons-material "layers"
                                                       :height carriage-mode-icon-height
                                                       :v-adjust carriage-mode-icon-v-adjust
                                                       :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('scope-all-off (when (fboundp 'all-the-icons-material)
                                   (all-the-icons-material "layers"
                                                           :height carriage-mode-icon-height
                                                           :v-adjust carriage-mode-icon-v-adjust
                                                           :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-muted-face)))))
                 ('scope-last (when (fboundp 'all-the-icons-material)
                                ;; NOTE: avoid glyphs that embed dots/digits (e.g. filter_1/looks_one).
                                ;; Use a clean arrow for "last/nearest" scope marker.
                                (all-the-icons-material "filter_1"
                                                        :height carriage-mode-icon-height
                                                        :v-adjust carriage-mode-icon-v-adjust
                                                        :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('scope-last-off (when (fboundp 'all-the-icons-material)
                                    (all-the-icons-material "filter_1"
                                                            :height carriage-mode-icon-height
                                                            :v-adjust (- carriage-mode-icon-v-adjust 0.2)
                                                            :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-muted-face)))))
                 ('scope  (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "layers"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust (- carriage-mode-icon-v-adjust 0.2)
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('profile (when (fboundp 'all-the-icons-material)
                             (all-the-icons-material "account_tree"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('inject (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "call_split"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
                 (_ nil))))
          (when (stringp res)
            (puthash key res cache))
          res)))))

;; Header-line helpers (split from carriage-ui--header-line)

(defvar-local carriage-ui--rendering-window nil
  "Window currently being rendered for header-line (:eval). Set by carriage-ui--header-line-for.")

(defun carriage-ui--hl-sep ()
  "Separator used between header-line segments."
  " › ")

(defun carriage-ui--left-pad (&optional pixels)
  "Return a left padding spacer of PIXELS (default 3) using a display property."
  (let ((px (or pixels 3)))
    (propertize " " 'display (list 'space :width (cons 'pixels px)))))

(defun carriage-ui--icon-gap (&optional pixels)
  "Return a small fixed-size gap between an icon and its label.

Uses `display' spacing in pixels when available; degrades gracefully in TTY."
  (propertize " " 'display (list 'space :width (cons 'pixels (or pixels 4)))))

(defun carriage-ui--hl-build-seg (label icon)
  "Build a segment from LABEL and optional ICON, preserving icon face."
  (if icon (concat icon (carriage-ui--icon-gap) label) label))

(defun carriage-ui--hl-mute-tail (s)
  "Apply muted face to text after the icon separator.
Skips leading padding spaces so the icon face is preserved even when the
segment starts with a padded space. If no separator is found, mute whole string."
  (if (not (stringp s)) s
    (let* ((start (let ((m (string-match "[^ ]" s))) (if (integerp m) m 0))) ; first non-space (skip left pad)
           (idx   (string-match " " s (1+ start))))                          ; space after the first glyph (icon)
      (if (and (integerp idx) (< idx (length s)))
          (let ((head (substring s 0 (1+ idx)))
                (tail (substring s (1+ idx))))
            (concat head (propertize tail 'face 'carriage-ui-muted-face)))
        (propertize (copy-sequence s) 'face 'carriage-ui-muted-face)))))

(defun carriage-ui--hl-clickable-outline (s)
  "Make outline string S clickable to jump to the heading."
  (let* ((omap (let ((m (make-sparse-keymap)))
                 (define-key m [header-line mouse-1] #'carriage-ui-goto-outline)
                 m)))
    (propertize (or s "")
                'mouse-face 'mode-line-highlight
                'help-echo "Перейти к заголовку (mouse-1)"
                'local-map omap)))

(defun carriage-ui--hl-show-outline-p (tty avail)
  "Return non-nil when outline should be shown given TTY flag and AVAIL width."
  (and carriage-mode-headerline-show-outline (not tty) (> avail 30)))

(defun carriage-ui--hl-fit (pseg bseg oseg show-outline avail sep)
  "Truncate segments to fit AVAIL width. Returns list (P B O).
Truncation order: outline → buffer → project."
  (let* ((base (concat pseg sep bseg))
         (full (if show-outline (concat base sep oseg) base)))
    (when (> (length full) avail)
      (when show-outline
        (let* ((ol-max (max 10 (- avail (length base) (length sep)))))
          (setq oseg (carriage-ui--truncate-middle oseg ol-max))
          (setq full (concat base sep oseg))))
      (when (> (length full) avail)
        (let* ((buf-max (max 10 (- avail (length pseg) (length sep)))))
          (setq bseg (carriage-ui--truncate-middle bseg buf-max))
          (setq base (concat pseg sep bseg))
          (setq full (if show-outline (concat base sep oseg) base))))
      (when (> (length full) avail)
        (setq pseg (carriage-ui--truncate-middle pseg (max 5 (- avail (length sep) (length bseg)))))
        (setq base (concat pseg sep bseg))
        (setq full (if show-outline (concat base sep oseg) base))))
    (list pseg bseg oseg)))

(defun carriage-ui--header-line-for (win)
  "Wrapper to render header-line for WIN, ensuring width calculations use WIN rather than the selected window."
  (let ((carriage-ui--rendering-window (and (window-live-p win) win)))
    (carriage-ui--header-line)))

(defun carriage-ui--header-line ()
  "Build header-line: [icon] project › [icon] buffer › org-outline-path (no icon for heading).
- Graceful degradation in TTY and narrow windows (hide outline).
- Outline segment is clickable in org-mode to jump to the heading.
- Visuals: all text sections except the last are muted gray; icons keep their colors.
Optimized with caching to reduce allocations on redisplay."
  (let* ((project (carriage-ui--project-name))
         (bufname (buffer-name))
         (win (or carriage-ui--rendering-window (selected-window)))
         (use-icons (carriage-ui--icons-available-p))
         ;; Include icon env in cache key to handle theme/icon changes
         (icon-env (and use-icons (carriage-ui--icon-cache-env-current)))
         (sep (carriage-ui--hl-sep))
         ;; Window width and policy
         (w (or (ignore-errors
                  (and (window-live-p win) (window-total-width win)))
                80))
         (maxw (or (and (boundp 'carriage-mode-headerline-max-width)
                        carriage-mode-headerline-max-width)
                   w))
         (reserve 10)
         (avail (max 0 (- maxw reserve)))
         (tty (not (display-graphic-p)))
         (show-outline (carriage-ui--hl-show-outline-p tty avail))
         (outline (and show-outline (or carriage-ui--last-outline-path-str "")))
         (cache-key (list avail show-outline project bufname outline use-icons icon-env)))
    (if (and (equal cache-key carriage-ui--hl-cache-key)
             (stringp carriage-ui--hl-cache))
        carriage-ui--hl-cache
      (let* ((p-icon (and use-icons (carriage-ui--icon 'project)))
             (f-icon (and use-icons (carriage-ui--icon 'file)))
             (pseg (concat (carriage-ui--left-pad) (carriage-ui--hl-build-seg project p-icon)))
             (bseg (carriage-ui--hl-build-seg bufname f-icon))
             (oseg (or outline "")))
        (cl-destructuring-bind (p1 b1 o1)
            (carriage-ui--hl-fit pseg bseg oseg show-outline avail sep)
          ;; Apply muted face to all but the last section
          (let* ((p2 p1) (b2 b1))
            (if show-outline
                (progn
                  (setq p2 (carriage-ui--hl-mute-tail p2))
                  (setq b2 (carriage-ui--hl-mute-tail b2)))
              (setq p2 (carriage-ui--hl-mute-tail p2)))
            (let* ((base (concat p2 sep b2))
                   (res (if show-outline
                            (concat base sep (carriage-ui--hl-clickable-outline o1))
                          base)))
              (setq carriage-ui--hl-cache res
                    carriage-ui--hl-cache-key cache-key)
              res)))))))

;; Cache and refresh outline segment on cursor move to ensure timely header updates.
(defvar-local carriage-ui--project-name-cached nil
  "Cached project name for header-line; computed once per buffer.")

(defvar-local carriage-ui--hl-cache nil
  "Cached header-line string for the current buffer.")

(defvar-local carriage-ui--hl-cache-key nil
  "Signature (key) of the last rendered header-line for quick short-circuit.")

(defvar-local carriage-ui--last-outline-path-str nil
  "Cached outline path string for the current buffer's header-line refresh.")
(defvar-local carriage-ui--last-outline-level nil
  "Cached outline level at point for fast header-line refresh.")
(defvar-local carriage-ui--last-outline-title nil
  "Cached outline heading title at point for the current buffer's header-line refresh.")

(defvar-local carriage-ui--outline-dirty nil
  "Non-nil when the org outline path string needs recomputation on idle.")

(defvar-local carriage-ui--headerline-last-bol nil
  "Last `line-beginning-position' observed by `carriage-ui--headerline-post-command'.
Used to avoid marking outline dirty on every command (perf).")

(defvar-local carriage-ui--patch-count-cache nil

  "Cached count of #+begin_patch blocks in the current buffer.")
(defvar-local carriage-ui--patch-count-tick nil
  "Buffer tick corresponding to `carriage-ui--patch-count-cache'.")
(defvar-local carriage-ui--patch-count-cache-time 0
  "Timestamp (float seconds) of the last patch count recomputation.")

(defcustom carriage-ui-patch-count-refresh-interval 0.4
  "Minimum seconds between patch-count recomputations in a buffer.

Patch counting requires scanning the buffer for #+begin_patch blocks.
To keep redisplay fast, scanning is done asynchronously and throttled."
  :type 'number
  :group 'carriage-ui)

(defvar-local carriage-ui--patch-refresh-timer nil
  "Timer object for a scheduled patch-count refresh (buffer-local).")

(defvar-local carriage-ui--patch-last-refresh-time 0.0
  "Timestamp (float seconds) of the last completed patch-count refresh in this buffer.")

(defun carriage-ui--patch-refresh-now (&optional buffer)
  "Recompute patch ranges/count for BUFFER (or current buffer) and refresh modeline.
Never call this from redisplay/modeline."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'org-mode)
      ;; This recomputes ranges at most once per tick and also updates count cache.
      (ignore-errors (carriage-ui--get-patch-ranges))
      (setq carriage-ui--patch-last-refresh-time (float-time))
      (setq carriage-ui--patch-count-cache-time carriage-ui--patch-last-refresh-time)
      (when (fboundp 'carriage-ui--invalidate-ml-cache)
        (carriage-ui--invalidate-ml-cache))
      (force-mode-line-update t))
    t))

(defun carriage-ui--patch-schedule-refresh (&optional delay)
  "Schedule a throttled patch-count refresh for the current buffer."
  (when (timerp carriage-ui--patch-refresh-timer)
    (ignore-errors (cancel-timer carriage-ui--patch-refresh-timer)))
  (let* ((buf (current-buffer))
         (now (float-time))
         (interval (max 0.05 (or carriage-ui-patch-count-refresh-interval 0.4)))
         (age (- now (or carriage-ui--patch-last-refresh-time 0.0)))
         (d (cond
             ((numberp delay) delay)
             ((>= age interval) 0.05)
             (t (max 0.05 (- interval age))))))
    (setq carriage-ui--patch-refresh-timer
          (run-at-time
           (max 0.01 (float d)) nil
           (lambda (b)
             (when (buffer-live-p b)
               (with-current-buffer b
                 (setq carriage-ui--patch-refresh-timer nil)
                 (ignore-errors (carriage-ui--patch-refresh-now b)))))
           buf)))
  t)

(defun carriage-ui--patch-after-change (_beg _end _len)
  "After-change hook: coalesce patch-count refresh requests."
  ;; Only meaningful in Org buffers; keep the hook cheap.
  (when (derived-mode-p 'org-mode)
    (carriage-ui--patch-schedule-refresh)))

(defvar-local carriage-ui--patch-ranges nil
  "Cached list of (BEG . END) ranges for #+begin_patch…#+end_patch blocks in the current buffer.")
(defvar-local carriage-ui--patch-ranges-tick nil
  "Buffer tick corresponding to `carriage-ui--patch-ranges'.")

(defvar-local carriage-ui--last-patch-range nil
  "Cached (BEG . END) of the last patch block that contained point for fast rechecks.")

(defun carriage-ui--get-patch-ranges ()
  "Return cached list of patch block ranges as cons cells (BEG . END).
Ranges are recomputed at most once per buffer-chars-modified-tick."
  (if (not (derived-mode-p 'org-mode))
      nil
    (let ((tick (buffer-chars-modified-tick)))
      (if (and carriage-ui--patch-ranges
               carriage-ui--patch-ranges-tick
               (= tick carriage-ui--patch-ranges-tick))
          carriage-ui--patch-ranges
        (let ((ranges '()))
          (save-excursion
            (goto-char (point-min))
            (let ((case-fold-search t))
              (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
                (let ((beg (match-beginning 0)))
                  (if (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                      (let ((end (match-beginning 0)))
                        (push (cons beg end) ranges))
                    (push (cons beg (point-max)) ranges))))))
          (setq ranges (nreverse ranges))
          (setq carriage-ui--patch-ranges ranges
                carriage-ui--patch-ranges-tick tick
                ;; keep count cache in sync for free
                carriage-ui--patch-count-cache (length ranges)
                carriage-ui--patch-count-tick tick)
          ranges)))))

(defcustom carriage-ui-apply-visibility-cache-ttl 0
  "Deprecated: Dry/Apply buttons removed from modeline; cache TTL unused."
  :type '(choice (const :tag "Disabled" 0) number)
  :group 'carriage-ui)

(defvar-local carriage-ui--apply-visibility-cache nil
  "Deprecated: no longer used (Dry/Apply removed).")

(defvar-local carriage-ui--apply-visible-snapshot nil
  "Per-render snapshot of [Dry]/[Apply] visibility, set by carriage-ui--modeline to deduplicate computation.")

;; Branch name cache (to avoid heavy VC/git calls on every modeline render)
(defcustom carriage-ui-branch-cache-ttl 30.0
  "TTL in seconds for cached VCS branch name used in the modeline.
When nil, the cache is considered always valid (until explicitly invalidated)."
  :type '(choice (const :tag "Unlimited (never auto-refresh)" nil) number)
  :group 'carriage-ui)

(defcustom carriage-ui-branch-async-refresh t
  "When non-nil, fetch Git branch name asynchronously off the redisplay path.
If `vc-mode' provides a branch name, it is used immediately. Heavy fallbacks
(vc/git) run on an idle timer and the modeline updates when finished."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-ui-branch-refresh-delay 0.07
  "Idle delay (seconds) before attempting an asynchronous branch refresh."
  :type 'number
  :group 'carriage-ui)

(defvar-local carriage-ui--branch-cache-string nil
  "Cached branch name for current buffer modeline (or nil).")

(defvar-local carriage-ui--branch-cache-time 0
  "Timestamp of the last branch cache refresh (float seconds).")

(defun carriage-ui--branch-name-cached ()
  "Return VCS branch name for the current buffer using a lightweight cache.
Prefers parsing `vc-mode' when available; falls back to git helper rarely.
Respects `carriage-ui-branch-cache-ttl'. When outside a Git repo, returns :no-git."
  (let* ((ttl carriage-ui-branch-cache-ttl)
         (now (float-time))
         (valid (or (null ttl)
                    (< (- now (or carriage-ui--branch-cache-time 0)) (or ttl 0)))))
    (if (and carriage-ui--branch-cache-string valid)
        carriage-ui--branch-cache-string
      ;; Fast path: outside git repo → mark and return :no-git without any processes.
      (when (not (and (fboundp 'carriage-git--repo-present-p)
                      (carriage-git--repo-present-p default-directory)))
        (setq carriage-ui--branch-cache-string :no-git
              carriage-ui--branch-cache-time now)
        (cl-return-from carriage-ui--branch-name-cached :no-git))
      (let* ((br
              (or
               ;; Try to parse from vc-mode string to avoid processes
               (let* ((s (and (boundp 'vc-mode) vc-mode)))
                 (when (stringp s)
                   (cond
                    ((string-match "[: -]\\([^: -]+\\)\\'" s) (match-string 1 s))
                    (t nil))))
               ;; Optionally query VC, but guard to avoid expensive calls
               (when (require 'vc-git nil t)
                 (ignore-errors
                   (when (fboundp 'vc-git--symbolic-branch)
                     (vc-git--symbolic-branch default-directory))))
               ;; Fallback to our git helper (single process, robust to non-repo by returning nil)
               (condition-case _e
                   (carriage-git-current-branch default-directory)
                 (error nil)))))
        (setq carriage-ui--branch-cache-string br
              carriage-ui--branch-cache-time now)
        br))))

(defun carriage-ui--patch-count ()
  "Return the number of #+begin_patch blocks in the current buffer.

Perf invariant:
- MUST be O(1) and redisplay-safe.
- MUST NOT scan the buffer.
The cache is maintained asynchronously by `carriage-ui--patch-refresh-now'
scheduled from `after-change-functions' in `carriage-mode--init-ui'."
  (if (not (derived-mode-p 'org-mode))
      0
    (let ((v carriage-ui--patch-count-cache))
      (if (numberp v) v 0))))

(defun carriage-ui--point-in-patch-block-p ()
  "Return non-nil when point is inside a #+begin_patch … #+end_patch block.
Uses precomputed patch ranges to avoid regex scans on redisplay."
  (when (derived-mode-p 'org-mode)
    (let* ((pt (point))
           (lr carriage-ui--last-patch-range))
      (cond
       ((and (consp lr) (<= (car lr) pt) (< pt (cdr lr))) t)
       (t
        (let* ((ranges (carriage-ui--get-patch-ranges))
               (found nil)
               (lst ranges))
          (while (and lst (not found))
            (let ((r (car lst)))
              (when (and (<= (car r) pt) (< pt (cdr r)))
                (setq carriage-ui--last-patch-range r)
                (setq found t)))
            (setq lst (cdr lst)))
          found))))))

(defun carriage-ui--region-has-patch-p ()
  "Return non-nil when the active region contains a patch block."
  (when (use-region-p)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end))
            (case-fold-search t))
        (goto-char beg)
        (re-search-forward "^[ \t]*#\\+begin_patch\\b" end t)))))

(defun carriage-ui--show-apply-buttons-p ()
  "Deprecated: Dry/Apply buttons removed; always returns nil."
  nil)

(defvar-local carriage-ui--last-iter-cache-id nil
  "Cached last iteration id used to detect presence of last-iteration blocks.")
(defvar-local carriage-ui--last-iter-cache-tick nil
  "Buffer tick corresponding to `carriage-ui--last-iter-cache-id'.")
(defvar-local carriage-ui--last-iter-cache-result nil
  "Cached boolean result for last-iteration presence detection.")

(defun carriage-ui--last-iteration-present-p ()
  "Return non-nil when there are patch blocks associated with the last iteration.

Performance invariant:
- Must be O(1) and safe for redisplay/modeline.
- The source of truth is a buffer-local flag maintained by streaming/accept code."
  (and (boundp 'carriage--last-iteration-has-patches)
       carriage--last-iteration-has-patches))

(defcustom carriage-mode-headerline-show-outline t
  "When non-nil, show org outline segment in header-line. Turning it off reduces overhead in large Org files."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-mode-headerline-idle-interval 0.2
  "Idle interval in seconds before refreshing header-line after cursor/scroll changes."
  :type 'number
  :group 'carriage-ui)

(defvar-local carriage-ui--headerline-idle-timer nil
  "Idle timer used to coalesce frequent header-line refreshes.")

(defun carriage-ui--headerline-queue-refresh ()
  "Schedule a header-line refresh on idle to reduce churn.
Avoid rescheduling when a timer is already pending."
  (unless (timerp carriage-ui--headerline-idle-timer)
    (let* ((buf (current-buffer))
           (interval (or (and (boundp 'carriage-mode-headerline-idle-interval)
                              carriage-mode-headerline-idle-interval)
                         0.12)))
      (setq carriage-ui--headerline-idle-timer
            (run-with-idle-timer interval nil
                                 (lambda ()
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (carriage-ui--headerline-idle-refresh-run))))))))
  t)

(defun carriage-ui--headerline-post-command ()
  "Post-command hook: keep header-line responsive without doing Org work per keystroke.

Performance invariant:
- Must be O(1) for every command (including self-insert).
- Must NOT call `org-back-to-heading', `org-get-heading' etc.

We mark outline dirty only when point moved to another line, then schedule an idle
refresh; the heavy outline path computation happens in
`carriage-ui--headerline-idle-refresh-run'."
  (when (and carriage-mode-headerline-show-outline
             (derived-mode-p 'org-mode)
             (get-buffer-window (current-buffer) t))
    (let ((bol (line-beginning-position)))
      (unless (eq bol carriage-ui--headerline-last-bol)
        (setq carriage-ui--headerline-last-bol bol)
        (setq carriage-ui--outline-dirty t)
        (carriage-ui--headerline-queue-refresh)))))
(defun carriage-ui--headerline-window-scroll (_win _start)
  "Refresh header-line on window scroll for instant visual updates."
  (carriage-ui--headerline-queue-refresh))

(defun carriage-ui--ml-button (label fn help)
  "Return a clickable LABEL that invokes FN, preserving LABEL's text properties.
Memoized to avoid per-redisplay keymap/props allocation.

Important: the cache key includes label's text properties to ensure visual updates
(e.g. toggle icons changing color) are not suppressed by memoization."
  (let* ((kprops (and (stringp label) (text-properties-at 0 label)))
         (key (list label help fn kprops))
         (cache (or carriage-ui--button-cache
                    (setq carriage-ui--button-cache (make-hash-table :test 'equal))))
         (hit (and cache (gethash key cache))))
    (if (stringp hit)
        hit
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-1] fn)
        (let* ((s (copy-sequence (or label "")))
               (len (length s)))
          (when (> len 0)
            (add-text-properties
             0 len
             (list 'mouse-face 'mode-line-highlight
                   'help-echo help
                   'local-map map)
             s))
          (puthash key s cache)
          s)))))

(defun carriage-ui--toggle-icon (key onp)
  "Return colored icon for toggle KEY based on ONP, with caching."
  (when (carriage-ui--icons-available-p)
    (carriage-ui--maybe-refresh-icon-cache-env)
    (let* ((cache (or carriage-ui--icon-cache
                      (setq carriage-ui--icon-cache (make-hash-table :test 'equal))))
           (ckey (list 'toggle key (if onp t nil)))
           (hit (gethash ckey cache)))
      (if (stringp hit)
          hit
        (let* ((face (if onp
                         (pcase key
                           ('auto    'carriage-ui-accent-blue-face)
                           ('diffs   'carriage-ui-accent-orange-face)
                           ('confirm 'carriage-ui-accent-purple-face)
                           ('icons   'carriage-ui-accent-cyan-face)
                           ('ctx     'carriage-ui-accent-blue-face)
                           ('files   'carriage-ui-accent-purple-face)
                           (_        'carriage-ui-accent-blue-face))
                       'carriage-ui-muted-face))
               (fg (carriage-ui--accent-hex face))
               (fplist (list :inherit nil :foreground fg))
               (res
                (pcase key
                  ('auto
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "autorenew"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('diffs
                   (cond
                    ((fboundp 'all-the-icons-octicon)
                     (all-the-icons-octicon "diff"
                                            :height carriage-mode-icon-height
                                            :v-adjust carriage-mode-icon-v-adjust
                                            :face fplist))
                    ((fboundp 'all-the-icons-material)
                     (all-the-icons-material "difference"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist))
                    (t nil)))
                  ('confirm
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "done_all"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('icons
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "image"
                                             :height carriage-mode-icon-height
                                             :v-adjust carriage-mode-icon-v-adjust
                                             :face fplist)))
                  ('ctx
                   (when (fboundp 'all-the-icons-material)
                     (ignore-errors
                       (all-the-icons-material "toc"
                                               :height carriage-mode-icon-height
                                               :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                               :face fplist))))
                  ('files
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-material "featured_play_list"
                                             :height carriage-mode-icon-height
                                             :v-adjust (- carriage-mode-icon-v-adjust 0.14)
                                             :face fplist)))
                  ('patched
                   (cond
                    ((fboundp 'all-the-icons-material)
                     (all-the-icons-material "code"
                                             :height carriage-mode-icon-height
                                             :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                             :face fplist))
                    ((fboundp 'all-the-icons-octicon)
                     (all-the-icons-octicon "checklist"
                                            :height carriage-mode-icon-height
                                            :v-adjust carriage-mode-icon-v-adjust
                                            :face fplist))
                    (t nil)))
                  ('visible
                   (cond
                    ((fboundp 'all-the-icons-material)
                     (all-the-icons-material "visibility"
                                             :height carriage-mode-icon-height
                                             :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                             :face fplist))
                    ((fboundp 'all-the-icons-octicon)
                     (all-the-icons-octicon "eye"
                                            :height carriage-mode-icon-height
                                            :v-adjust carriage-mode-icon-v-adjust
                                            :face fplist))
                    (t nil)))
                  (_ nil))))
          (when (stringp res)
            (puthash ckey res cache))
          res)))))

(defun carriage-ui--toggle (label var-sym fn help &optional icon-key)
  "Build a toggle button with LABEL; highlight when VAR-SYM is non-nil.
When ICON-KEY is non-nil and icons are available, show a colored icon that
reflects toggle state (muted when off, bright when on)."
  (let* ((on (and (boundp var-sym) (symbol-value var-sym)))
         (lbl (if (and icon-key (carriage-ui--icons-available-p))
                  (carriage-ui--toggle-icon icon-key on)
                (if on (propertize label 'face 'mode-line-emphasis) label))))
    (carriage-ui--ml-button lbl fn help)))

(defun carriage-ui--maybe-in-report-buffer ()
  "Return non-nil if current buffer is a Carriage report buffer."
  (or (derived-mode-p 'carriage-report-mode)
      (string= (buffer-name) "*carriage-report*")))

(defun carriage-ui--diff-button ()
  "Open Diff for report item if available; otherwise switch to report (no error in batch)."
  (interactive)
  (condition-case _
      (if (carriage-ui--maybe-in-report-buffer)
          (call-interactively #'carriage-report-show-diff-at-point)
        (let* ((buf (get-buffer "*carriage-report*")))
          (when buf
            (pop-to-buffer buf)
            (message "Select a row, then press RET or [Diff]"))))
    (error
     (message "Нет доступного отчёта для Diff"))))

(defun carriage-ui--ediff-button ()
  "Open Ediff for report item if available; otherwise switch to report (no error in batch)."
  (interactive)
  (condition-case _
      (if (carriage-ui--maybe-in-report-buffer)
          (call-interactively #'carriage-report-ediff-at-point)
        (let* ((buf (get-buffer "*carriage-report*")))
          (when buf
            (pop-to-buffer buf)
            (message "Select a row, then press e or [Ediff]"))))
    (error
     (message "Нет доступного отчёта для Ediff"))))

;; -- Optional: settings "gear" button to open the menu directly.
(defun carriage-ui--settings-btn ()
  "Return a clickable gear settings button for the modeline."
  (let* ((use-icons (carriage-ui--icons-available-p))
         (ic (and use-icons (fboundp 'all-the-icons-octicon)
                  (all-the-icons-octicon "gear" :height carriage-mode-icon-height :v-adjust carriage-mode-icon-v-adjust
                                         :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
         (label (or ic "Menu"))
         (btn (carriage-ui--ml-button label #'carriage-keys-open-menu "Открыть меню Carriage (C-c e)")))
    btn))
(defalias 'settings-btn 'carriage-ui--settings-btn)

(defvar carriage--icon-intent-ask "A"
  "Modeline marker for Ask intent.")
(defvar carriage--icon-intent-code "C"
  "Modeline marker for Code intent.")
(defvar carriage--icon-intent-hybrid "H"
  "Modeline marker for Hybrid intent.")

(defun carriage-ui--segment-intent-suite ()
  "Return a short modeline segment for Intent/Suite."
  (let* ((intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (suite  (and (boundp 'carriage-mode-suite) carriage-mode-suite))
         (i (pcase intent
              ('Ask    carriage--icon-intent-ask)
              ('Code   carriage--icon-intent-code)
              ('Hybrid carriage--icon-intent-hybrid)
              (_       carriage--icon-intent-ask)))
         (s (if (symbolp suite) (symbol-name suite) (or suite ""))))
    (format "[%s|%s]" i (if (string-empty-p s) "-" s))))

(defun carriage-ui--flash-last-iteration-patches (&optional buffer)
  "Flash all begin_patch…end_patch blocks of the last streamed region in BUFFER (or current).
If a streamed region is not available, flash all patch blocks in the buffer.
Uses pulse.el when available, otherwise temporary overlays."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((dur (or (and (boundp 'carriage-mode-flash-duration)
                         carriage-mode-flash-duration)
                    1.0))
           (use-pulse (require 'pulse nil t))
           (r (and (fboundp 'carriage-stream-region) (carriage-stream-region)))
           (beg (and (consp r) (car r)))
           (end (and (consp r) (cdr r))))
      (save-excursion
        (save-restriction
          (when (and beg end) (narrow-to-region beg end))
          (goto-char (point-min))
          (let ((ranges '()))
            (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
              (let ((pbeg (match-beginning 0)))
                (if (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                    (let ((pend (line-end-position)))
                      (push (cons pbeg pend) ranges))
                  (push (cons pbeg (point-max)) ranges))))
            (dolist (rg ranges)
              (let ((rb (car rg)) (re (cdr rg)))
                (if use-pulse
                    (pulse-momentary-highlight-region rb re 'carriage-patch-valid-face)
                  (let ((ov (make-overlay rb re)))
                    (overlay-put ov 'face 'carriage-patch-valid-face)
                    (run-at-time dur nil
                                 (lambda (o) (when (overlayp o) (delete-overlay o)))
                                 ov)))))))))))
;; -------------------------------------------------------------------
;; Refactor: split carriage-ui--modeline into smaller helpers

(defun carriage-ui--ml-cache-key ()
  "Compute cache key for the modeline string based on current UI environment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (blocks (if (and (listp carriage-ui-modeline-blocks)
                          carriage-ui-modeline-blocks)
                     carriage-ui-modeline-blocks
                   carriage-ui--modeline-default-blocks))
         (state  (and (boundp 'carriage--ui-state) carriage--ui-state))
         (ctx-ver (and (memq 'context blocks) (or carriage-ui--ctx-badge-version 0)))
         (apply-ver (and (memq 'apply-status blocks) (or carriage-ui--apply-badge-version 0)))
         ;; Avoid scanning buffers during redisplay: patch count must come from cache,
         ;; maintained asynchronously by `carriage-ui--patch-refresh-now'.
         (patch-count (and (memq 'patch blocks)
                           (not (memq state '(sending streaming dispatch waiting reasoning)))
                           (numberp carriage-ui--patch-count-cache)
                           carriage-ui--patch-count-cache))
         (has-last (and (memq 'all blocks)
                        (carriage-ui--last-iteration-present-p)))
         (spin   (and carriage-ui-enable-spinner
                      (memq state '(sending streaming dispatch waiting reasoning))
                      (carriage-ui--spinner-char)))
         ;; Do not force git/vc calls in key; rely on cache timestamp only
         (branch-t (and (memq 'branch blocks) carriage-ui--branch-cache-time))
         (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
    (list uicons
          state spin
          ctx-ver apply-ver
          patch-count has-last abortp blocks
          (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
          (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
          (and (boundp 'carriage-mode-model)   carriage-mode-model)
          (and (boundp 'carriage-mode-backend) carriage-mode-backend)
          (and (boundp 'carriage-mode-provider) carriage-mode-provider)
          (and (boundp 'carriage-apply-engine) carriage-apply-engine)
          (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
          branch-t
          (and (boundp 'carriage-mode-include-gptel-context)
               carriage-mode-include-gptel-context)
          (and (boundp 'carriage-mode-include-doc-context)
               carriage-mode-include-doc-context)
          (and (boundp 'carriage-mode-include-visible-context)
               carriage-mode-include-visible-context)
          (and (boundp 'carriage-mode-include-patched-files)
               carriage-mode-include-patched-files)
          (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope))))

(defun carriage-ui--ml-seg-intent ()
  "Build Intent segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (label
          (if uicons
              (pcase intent
                ('Ask   (or (carriage-ui--icon 'ask) "Ask"))
                ('Code  (or (carriage-ui--icon 'patch) "Code"))
                (_      (or (carriage-ui--icon 'hybrid) "Hybrid")))
            (format "%s" (pcase intent ('Ask "Ask") ('Code "Code") (_ "Hybrid"))))))
    (carriage-ui--ml-button label #'carriage-ui-toggle-intent "Toggle Ask/Code/Hybrid intent")))

(defun carriage-ui--ml-seg-suite ()
  "Build Suite segment.

GUI policy:
- When icons are available and suite icons are enabled, render ONLY the icon in the modeline.
- Current suite value is shown in tooltip (help-echo).
TTY / icons disabled: keep text label fallback."
  (let* ((uicons (carriage-ui--icons-available-p))
         (suite  (and (boundp 'carriage-mode-suite) carriage-mode-suite))
         (suite-str (cond ((symbolp suite) (symbol-name suite))
                          ((stringp suite) suite) (t "udiff")))
         ;; GUI policy: when icons are available, render ONLY the icon in the modeline.
         ;; The current suite value is shown in tooltip.
         (icon (and uicons (carriage-ui--icon 'suite)))
         (_ nil)
         (label
          (if icon
              ;; GUI: icon-only, value goes to tooltip.
              icon
            (let ((name (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                            (carriage-i18n :suite) "Suite")))
              (format "%s: %s" name suite-str))))
         (help0 (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                    (carriage-i18n :suite-tooltip)
                  "Select Suite (sre|udiff)"))
         (help (format "%s\nТекущее значение: %s" help0 suite-str)))
    (carriage-ui--ml-button label #'carriage-select-suite help)))

(defun carriage-ui--ml-seg-model ()
  "Build Model segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (model-val (and (boundp 'carriage-mode-model) carriage-mode-model))
         (raw-model (cond
                     ((stringp model-val) model-val)
                     ((symbolp model-val) (symbol-name model-val))
                     ((null model-val) "")
                     (t (format "%s" model-val))))
         (resolved (carriage-llm-resolve-model carriage-mode-backend carriage-mode-provider raw-model))
         (display-source (if (stringp resolved) resolved raw-model))
         (display-name (carriage-llm-display-name (or display-source "")))
         (bm-text (format "%s" (if (and (stringp display-name)
                                        (not (string-empty-p display-name)))
                                   display-name
                                 "-")))
         (ic (and uicons (carriage-ui--icon 'model)))
         (full-id
          (let* ((candidate (and (stringp resolved) resolved))
                 (has-colon (and candidate (string-match-p ":" candidate))))
            (cond
             (has-colon candidate)
             (t (or (carriage-llm-make-full-id carriage-mode-backend carriage-mode-provider display-source)
                    display-source
                    raw-model
                    "-")))))
         ;; Make only the text part clickable to ensure mouse highlight and clicks work reliably.
         (btn (carriage-ui--ml-button bm-text
                                      #'carriage-select-model
                                      (format "Модель: %s (клик — выбрать)"
                                              (or full-id "-")))))
    (if ic
        (concat ic (carriage-ui--icon-gap) btn)
      btn)))

(defun carriage-ui--ml-seg-engine ()
  "Build Engine segment.

GUI policy:
- When icons are available and engine icons are enabled, render ONLY the icon in the modeline.
- Current engine value is shown in tooltip (help-echo).
TTY / icons disabled: keep text label fallback."
  (let* ((uicons (carriage-ui--icons-available-p))
         (engine-str
          (let ((e (and (boundp 'carriage-apply-engine) carriage-apply-engine)))
            (cond
             ((eq e 'git)
              (let ((pol (and (boundp 'carriage-git-branch-policy)
                              carriage-git-branch-policy)))
                (format "git:%s" (if (symbolp pol) (symbol-name pol) ""))))
             ((symbolp e) (symbol-name e))
             ((stringp e) e)
             (t "git"))))
         ;; GUI policy: when icons are available, render ONLY the icon in the modeline.
         ;; The current engine value is shown in tooltip.
         (icon (and uicons (carriage-ui--icon 'engine)))
         (_ (require 'carriage-i18n nil t))
         (eng (and (boundp 'carriage-apply-engine) carriage-apply-engine))
         (policy (and (eq eng 'git)
                      (boundp 'carriage-git-branch-policy)
                      (symbol-name carriage-git-branch-policy)))
         (help0 (cond
                 ((and (eq eng 'git)
                       (featurep 'carriage-i18n) (fboundp 'carriage-i18n)
                       (stringp policy))
                  (carriage-i18n :engine-tooltip-branch engine-str policy))
                 ((and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                  (carriage-i18n :engine-tooltip))
                 (t "Select apply engine")))
         (help (format "%s\nТекущее значение: %s" help0 engine-str))
         (label
          (if icon
              ;; GUI: icon-only, value goes to tooltip.
              icon
            (let ((name (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                            (carriage-i18n :engine-label)
                          "Engine")))
              (format "%s: %s" name engine-str)))))
    (carriage-ui--ml-button label #'carriage-select-apply-engine help)))

(defun carriage-ui--ml-seg-state ()
  "Build State segment with spinner and face, including help-echo tooltip when available.
This segment represents *request/transport* state."
  (let* ((st (let ((s (and (boundp 'carriage--ui-state) carriage--ui-state)))
               (if (symbolp s) s 'idle)))
         (label (carriage-ui--state-label st))
         (txt (format "%s%s"
                      label
                      (if (and carriage-ui-enable-spinner
                               (memq st '(sending streaming dispatch waiting reasoning)))
                          (concat " " (carriage-ui--spinner-char))
                        "")))
         (face (pcase st
                 ('idle 'carriage-ui-state-idle-face)
                 ((or 'sending 'streaming 'dispatch 'waiting 'reasoning) 'carriage-ui-state-sending-face)
                 ('done 'carriage-ui-state-success-face)
                 ('error 'carriage-ui-state-error-face)
                 (_ nil)))
         (help (and (boundp 'carriage--ui-state-tooltip) carriage--ui-state-tooltip)))
    (cond
     ((and face help) (propertize txt 'face face 'help-echo help))
     (face            (propertize txt 'face face))
     (help            (propertize txt 'help-echo help))
     (t               txt))))

(defun carriage-ui--apply--diag->line (m)
  "Format a single diagnostic message plist M for tooltip."
  (let* ((sev (or (plist-get m :severity) 'info))
         (code (or (plist-get m :code) 'UNKNOWN))
         (file (or (plist-get m :file) (plist-get m :path)))
         (details (string-trim (or (plist-get m :details) ""))))
    (concat " - [" (format "%s" sev) "] " (format "%s" code)
            (if (and (stringp details) (not (string-empty-p details)))
                (concat " — " details)
              "")
            (if (and (stringp file) (not (string-empty-p file)))
                (concat " (file " file ")")
              ""))))

(defun carriage-ui--apply--tooltip-from-report (report)
  "Build tooltip for apply-status badge from REPORT."
  (let* ((phase (plist-get report :phase))
         (sum (or (plist-get report :summary) '()))
         (ok (or (plist-get sum :ok) 0))
         (fail (or (plist-get sum :fail) 0))
         (skipped (or (plist-get sum :skipped) 0))
         (msgs (or (plist-get report :messages) '()))
         (head (format "%s: ok=%d fail=%d skipped=%d"
                       (pcase phase ('dry-run "Dry-run") ('apply "Apply") (_ "Apply"))
                       ok fail skipped))
         (tail (when (and (listp msgs) msgs)
                 (let* ((limit 20)
                        (shown (cl-subseq msgs 0 (min (length msgs) limit)))
                        (more (- (length msgs) (length shown))))
                   (mapconcat #'identity
                              (append (list "Diagnostics:")
                                      (mapcar #'carriage-ui--apply--diag->line shown)
                                      (when (> more 0) (list (format "… (+%d more)" more))))
                              "\n")))))
    (string-join (delq nil (list head tail "Click to open report")) "\n")))

(defun carriage-ui--apply--label-and-face ()
  "Return (LABEL . FACE) for apply-status badge based on `carriage--ui-apply-state`."
  (let* ((st (or carriage--ui-apply-state 'none)))
    (pcase st
      ;; IMPORTANT: when there was no apply/dry-run yet, do not show the badge at all.
      ('none       (cons nil nil))
      ('running    (cons "Apply…"     'carriage-ui-state-active-face))
      ('dry-ok     (cons "Dry OK"     'carriage-ui-state-success-face))
      ('dry-fail   (cons "Dry ERR"    'carriage-ui-state-error-face))
      ('apply-ok   (cons "OK"   'carriage-ui-state-success-face))
      ('apply-fail (cons "ERR"  'carriage-ui-state-error-face))
      ('aborted    (cons "ABORT" 'carriage-ui-state-error-face))
      (_           (cons (format "Apply %s" (symbol-name st)) nil)))))

(defun carriage-ui--apply-open-last-report (&optional _event)
  "Open last apply/dry-run report stored for the apply-status badge."
  (interactive "e")
  (if (and (listp carriage--ui-apply-last-report)
           (fboundp 'carriage-report-open))
      (carriage-report-open carriage--ui-apply-last-report)
    (message "Carriage: no apply report available")))

(defun carriage-ui--ml-seg-apply-status ()
  "Build Apply/Dry-run status segment (separate from request state)."
  (let* ((lf (carriage-ui--apply--label-and-face))
         (lbl (car lf)))
    ;; When there was no dry-run/apply yet, hide the badge entirely.
    (when (and (stringp lbl) (not (string-empty-p lbl)))
      (let* ((face (cdr lf))
             (tip (or carriage--ui-apply-tooltip
                      (and (listp carriage--ui-apply-last-report)
                           (carriage-ui--apply--tooltip-from-report carriage--ui-apply-last-report))
                      "Last apply/dry-run status (click to open report)"))
             (btn (carriage-ui--ml-button lbl #'carriage-ui--apply-open-last-report tip)))
        (if face
            (propertize btn 'face face)
          btn)))))

(defun carriage-ui-apply-set-state (state &optional tooltip)
  "Set apply-status badge STATE and optional TOOLTIP (string)."
  (setq carriage--ui-apply-state (or state 'none))
  (when (and (stringp tooltip) (not (string-empty-p tooltip)))
    (setq carriage--ui-apply-tooltip tooltip))
  (setq carriage-ui--apply-badge-version (1+ (or carriage-ui--apply-badge-version 0)))
  (carriage-ui--invalidate-ml-cache)
  (force-mode-line-update))

(defun carriage-ui-apply-reset ()
  "Reset/hide apply-status badge and forget the last apply report."
  (interactive)
  (setq carriage--ui-apply-state 'none)
  (setq carriage--ui-apply-tooltip nil)
  (setq carriage--ui-apply-last-report nil)
  (setq carriage-ui--apply-badge-version (1+ (or carriage-ui--apply-badge-version 0)))
  (carriage-ui--invalidate-ml-cache)
  (force-mode-line-update))

(defun carriage-ui-note-apply-report (report)
  "Update apply-status badge from apply/dry-run REPORT."
  (when (listp report)
    (setq carriage--ui-apply-last-report report)
    (let* ((phase (plist-get report :phase))
           (sum (or (plist-get report :summary) '()))
           (fail (or (plist-get sum :fail) 0))
           (st (pcase phase
                 ('dry-run (if (> fail 0) 'dry-fail 'dry-ok))
                 ('apply   (if (> fail 0) 'apply-fail 'apply-ok))
                 (_        (if (> fail 0) 'apply-fail 'apply-ok)))))
      (carriage-ui-apply-set-state st (carriage-ui--apply--tooltip-from-report report)))))

(defun carriage-ui--ml-seg-context ()
  "Build Context badge segment (click to refresh now).

Robustness:
- Preferred return from `carriage-ui--context-badge' is a cons (LABEL . TOOLTIP).
- Some older/experimental code paths may return a propertized LABEL string.
- Some third-party/legacy integrations may return a plist (:label/:tooltip).
Never signal from redisplay/modeline.

This helper normalizes label strings by stripping surrounding square brackets
so modeline segments do not show brackets like \"[Ctx:...]\"."
  (let ((ctx (carriage-ui--context-badge)))
    (cl-labels ((strip-brackets (s)
                  (if (and (stringp s)
                           (string-match-p "\\`\\[.*\\]\\'" s))
                      (replace-regexp-in-string "\\`\\[\\(.*\\)\\]\\'" "\\1" s)
                    s)))
      (cond
       ;; Preferred: (LABEL . TOOLTIP)
       ((consp ctx)
        (let ((lbl (strip-brackets (car ctx)))
              (hint (cdr ctx)))
          (when (stringp lbl)
            (carriage-ui--ml-button lbl
                                    #'carriage-ui-refresh-context-badge
                                    (or hint "Обновить контекст (mouse-1)")))))

       ;; Plist form: (:label "..." :tooltip "...")
       ((and (listp ctx) (plist-member ctx :label))
        (let ((lbl (strip-brackets (plist-get ctx :label)))
              (hint (or (plist-get ctx :tooltip) (plist-get ctx :help) "Обновить контекст (mouse-1)")))
          (when (stringp lbl)
            (carriage-ui--ml-button lbl #'carriage-ui-refresh-context-badge hint))))

       ;; Raw string (possibly propertized with help-echo)
       ((stringp ctx)
        (let* ((lbl (strip-brackets ctx))
               (hint (or (get-text-property 0 'help-echo ctx) "Обновить контекст (mouse-1)")))
          (carriage-ui--ml-button lbl #'carriage-ui-refresh-context-badge hint)))

       (t nil)))))

(defun carriage-ui--ml-seg-branch ()
  "Build Branch segment."
  (let* ((uicons (carriage-ui--icons-available-p))
         (br (carriage-ui--branch-name-cached))
         (txt (cond
               ((eq br :no-git)
                (propertize "no-git" 'face 'carriage-ui-muted-face 'help-echo "Нет Git-репозитория"))
               ((and (stringp br) (not (string-empty-p br)))
                br)
               (t nil))))
    (when txt
      (if (and uicons (fboundp 'all-the-icons-octicon))
          (let ((ic (all-the-icons-octicon "git-branch"
                                           :height carriage-mode-icon-height
                                           :v-adjust carriage-mode-icon-v-adjust
                                           :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
            (concat ic (carriage-ui--icon-gap) txt))
        txt))))

(defun carriage-ui--ml-seg-patch ()
  "Build Patch counter segment.

Performance:
- Must not scan the buffer from redisplay/modeline.
- Uses `carriage-ui--patch-count-cache' which is maintained asynchronously."
  (let ((patch-count (or carriage-ui--patch-count-cache 0)))
    (when (and (numberp patch-count) (> patch-count 0))
      (propertize (format "P:%d" patch-count)
                  'help-echo "Количество #+begin_patch блоков в буфере"))))

(defun carriage-ui--ml-seg-dry ()
  "Dry-run button removed from modeline."
  nil)

(defun carriage-ui--ml-seg-apply ()
  "Apply button removed from modeline."
  nil)

(defun carriage-ui--ml-seg-all ()
  "Build Apply-all button."
  (when (carriage-ui--last-iteration-present-p)
    (let* ((uicons (carriage-ui--icons-available-p))
           (label (or (and uicons (carriage-ui--icon 'all)) "All")))
      (carriage-ui--ml-button label #'carriage-apply-last-iteration "Apply last iteration"))))

(defun carriage-ui--ml-seg-diff ()
  "Build Diff button."
  (let* ((uicons (carriage-ui--icons-available-p))
         (label (or (and uicons (carriage-ui--icon 'diff)) "Diff")))
    (carriage-ui--ml-button label #'carriage-ui--diff-button "Открыть Diff для элемента отчёта")))

(defun carriage-ui--ml-seg-ediff ()
  "Build Ediff button."
  (let* ((uicons (carriage-ui--icons-available-p))
         (label (or (and uicons (carriage-ui--icon 'ediff)) "Ediff")))
    (carriage-ui--ml-button label #'carriage-ui--ediff-button "Открыть Ediff для элемента отчёта")))

(defun carriage-ui--ml-seg-abort ()
  "Build Abort button (visible only when an abort handler is registered)."
  (when (and (boundp 'carriage--abort-handler) carriage--abort-handler)
    (let* ((uicons (carriage-ui--icons-available-p))
           (label (or (and uicons (carriage-ui--icon 'abort)) "Abort")))
      (carriage-ui--ml-button label #'carriage-abort-current "Abort current request"))))

(defun carriage-ui--ml-seg-report ()
  "Build Report button."
  (let* ((uicons (carriage-ui--icons-available-p))
         (label (or (and uicons (carriage-ui--icon 'report)) "Report")))
    (carriage-ui--ml-button label #'carriage-report-open "Open report buffer")))

(defun carriage-ui--ml-seg-save ()
  "Build Save-settings button."
  (let* ((uicons (carriage-ui--icons-available-p))
         (label (or (and uicons (carriage-ui--icon 'save)) "Save")))
    (carriage-ui--ml-button label #'carriage-save-settings "Сохранить настройки Carriage сейчас")))

(defun carriage-ui--ml-seg-toggle-ctx ()
  "Build GPT context toggle."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :ctx-tooltip)
                 "Toggle including gptel-context (buffers/files)")))
    (carriage-ui--toggle "Ctx" 'carriage-mode-include-gptel-context
                         #'carriage-toggle-include-gptel-context
                         help 'ctx)))

(defun carriage-ui--ml-seg-toggle-files ()
  "Build Files context toggle."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :files-tooltip)
                 "Toggle including files from #+begin_context")))
    (carriage-ui--toggle "Files" 'carriage-mode-include-doc-context
                         #'carriage-toggle-include-doc-context
                         help 'files)))

(defun carriage-ui--ml-seg-toggle-patched ()
  "Build toggle for including files referenced by #+patch_done markers."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :patched-tooltip)
                 "Toggle including files from applied begin_patch blocks (:applied t)")))
    (carriage-ui--toggle "Patched" 'carriage-mode-include-patched-files
                         #'carriage-toggle-include-patched-files
                         help 'patched)))

(defun carriage-ui--ml-seg-toggle-visible ()
  "Build Visible-buffers context toggle."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :visible-tooltip)
                 "Toggle including visible frame buffers")))
    (carriage-ui--toggle "Vis" 'carriage-mode-include-visible-context
                         #'carriage-toggle-include-visible-context
                         help 'visible)))

(defun carriage-ui--ml-seg-doc-scope-all ()
  "Build button to select 'all doc-context scope."
  (let* ((enabled (if (boundp 'carriage-mode-include-doc-context)
                      carriage-mode-include-doc-context
                    t)))
    (when enabled
      (let* ((on (eq (and (boundp 'carriage-doc-context-scope)
                          carriage-doc-context-scope)
                     'all))
             (_ (require 'carriage-i18n nil t))
             (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                       (carriage-i18n :doc-scope-all-tip)
                     "Use all begin_context blocks"))
             (uicons (carriage-ui--icons-available-p))
             ;; Active scope: bright icon. Inactive: muted icon. No extra markers.
             (ic (and uicons (carriage-ui--icon (if on 'scope-all 'scope-all-off))))
             (label
              (cond
               (ic ic)
               (t (let ((txt "AllCtx"))
                    (if on (propertize txt 'face 'mode-line-emphasis) txt))))))
        (carriage-ui--ml-button label #'carriage-ui-select-doc-context-all help)))))

(defun carriage-ui--ml-seg-doc-scope-last ()
  "Build button to select 'last doc-context scope."
  (let* ((enabled (if (boundp 'carriage-mode-include-doc-context)
                      carriage-mode-include-doc-context
                    t)))
    (when enabled
      (let* ((on (eq (and (boundp 'carriage-doc-context-scope)
                          carriage-doc-context-scope)
                     'last))
             (_ (require 'carriage-i18n nil t))
             (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                       (carriage-i18n :doc-scope-last-tip)
                     "Use the last begin_context block in the buffer"))
             (uicons (carriage-ui--icons-available-p))
             ;; Active scope: bright icon. Inactive: muted icon. No extra markers.
             (ic (and uicons (carriage-ui--icon (if on 'scope-last 'scope-last-off))))
             (label
              (cond
               (ic ic)
               (t (let ((txt "LastCtx"))
                    (if on (propertize txt 'face 'mode-line-emphasis) txt))))))
        (carriage-ui--ml-button label #'carriage-ui-select-doc-context-last help)))))


(defun carriage-ui--ml-seg-settings ()
  "Build Settings button."
  (carriage-ui--settings-btn))

;; Wrapper commands to ensure required features are loaded and UI refreshes properly.
(defun carriage-ui-toggle-intent ()
  "Wrapper to toggle intent and refresh UI immediately."
  (interactive)
  (when (fboundp 'carriage-toggle-intent)
    (call-interactively 'carriage-toggle-intent))
  (carriage-ui--invalidate-and-refresh))

(defun carriage-ui-select-doc-context-all ()
  "Wrapper to select doc-context scope 'all' and refresh UI/badge."
  (interactive)
  (require 'carriage-context nil t)
  (when (fboundp 'carriage-select-doc-context-all)
    (call-interactively 'carriage-select-doc-context-all))
  (carriage-ui--ctx-invalidate-and-refresh))

(defun carriage-ui-select-doc-context-last ()
  "Wrapper to select doc-context scope 'last' and refresh UI/badge."
  (interactive)
  (require 'carriage-context nil t)
  (when (fboundp 'carriage-select-doc-context-last)
    (call-interactively 'carriage-select-doc-context-last))
  (carriage-ui--ctx-invalidate-and-refresh))

(defun carriage-ui--ml-render-block (blk)
  "Dispatch builder for a single modeline block BLK symbol."
  (pcase blk
    ('suite         (carriage-ui--ml-seg-suite))
    ('engine        (carriage-ui--ml-seg-engine))
    ('branch        (carriage-ui--ml-seg-branch))
    ('model         (carriage-ui--ml-seg-model))
    ('intent        (carriage-ui--ml-seg-intent))
    ('state         (carriage-ui--ml-seg-state))
    ('apply-status  (carriage-ui--ml-seg-apply-status))
    ('context       (carriage-ui--ml-seg-context))
    ('patch         (carriage-ui--ml-seg-patch))
    ;; 'dry removed
    ;; 'apply removed
    ('all           (carriage-ui--ml-seg-all))
    ('toggle-patched (carriage-ui--ml-seg-toggle-patched))
    ('toggle-visible (carriage-ui--ml-seg-toggle-visible))
    ('abort         (carriage-ui--ml-seg-abort))
    ('report        (carriage-ui--ml-seg-report))
    ('toggle-ctx    (carriage-ui--ml-seg-toggle-ctx))
    ('toggle-files  (carriage-ui--ml-seg-toggle-files))
    ('doc-scope-all (carriage-ui--ml-seg-doc-scope-all))
    ('doc-scope-last (carriage-ui--ml-seg-doc-scope-last))
    ('save          (carriage-ui--ml-seg-save))
    ('settings      (carriage-ui--ml-seg-settings))
    (_ nil)))

(defun carriage-ui--modeline ()
  "Build Carriage modeline segment using `carriage-ui-modeline-blocks' (refactored)."
  (let ((pre nil))
    (if (and (stringp pre) (not carriage-ui--ml-stale-p))
        pre
      (let* ((blocks (if (and (listp carriage-ui-modeline-blocks)
                              carriage-ui-modeline-blocks)
                         carriage-ui-modeline-blocks
                       carriage-ui--modeline-default-blocks)))
        ;; Compute once per render; Dry/Apply removed, snapshot unused.
        (setq carriage-ui--apply-visible-snapshot nil)
        (let* ((key (carriage-ui--ml-cache-key)))
          (if (and (equal key carriage-ui--ml-cache-key)
                   (stringp carriage-ui--ml-cache))
              carriage-ui--ml-cache
            (let* ((segments (cl-loop for blk in blocks
                                      for seg = (carriage-ui--ml-render-block blk)
                                      if (stringp seg)
                                      collect seg))
                   (res (if segments (mapconcat #'identity segments " ") "")))
              (setq carriage-ui--ml-cache-key key
                    carriage-ui--ml-cache     res
                    carriage--mode-modeline-string nil
                    carriage-ui--ml-stale-p nil)
              res)))))))

;; -------------------------------------------------------------------
;; State tooltips: buffer-local meta, helpers and public note-* API

(defcustom carriage-mode-state-tooltip-verbosity 'brief
  "Verbosity for state tooltip: 'brief or 'detailed."
  :type '(choice (const brief) (const detailed))
  :group 'carriage-ui)

(defcustom carriage-mode-state-tooltip-max-chars 1000
  "Maximum total length of state tooltip (characters)."
  :type 'integer
  :group 'carriage-ui)

(defcustom carriage-mode-state-tooltip-reasoning-tail 300
  "Maximum number of characters from the end of reasoning to include in tooltip."
  :type 'integer
  :group 'carriage-ui)

(defcustom carriage-mode-state-tooltip-show-backtrace nil
  "When non-nil, include backtrace in error tooltip (discouraged by default)."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-ui-tooltip-update-interval 0.12
  "Minimum seconds between forced modeline refreshes caused by tooltip updates.

Streaming may update tooltip text very frequently. This setting throttles
`force-mode-line-update' calls triggered only by tooltip changes."
  :type 'number
  :group 'carriage-ui)

(defvar-local carriage-ui--tooltip-last-update 0.0
  "Last time (float seconds) we forced mode-line update due to tooltip changes.")

(defvar-local carriage--ui-state-tooltip nil
  "Cached tooltip string for current UI state (help-echo for [STATE]).")

(defvar-local carriage--ui-state-meta nil
  "Buffer-local plist with lightweight state metadata used to render tooltips.
Keys (optional): :source :time-start :time-last :model :provider
:tokens-out :tokens-in :chunks :reasoning-tail (:error (:code :message :data))
:apply-summary (:ok :skip :fail :total) :phase.")

(defun carriage-ui--trim-right (s n)
  "Return S trimmed to at most N chars from the right."
  (if (or (null n) (<= (length s) n)) s (substring s 0 n)))

(defun carriage-ui--tail (s n)
  "Return last N characters of S."
  (if (or (null n) (<= (length s) n)) s (substring s (- (length s) n))))

(defun carriage-ui--fmt-seconds (t0 t1)
  "Format duration between T0 and T1 seconds as string."
  (format "%.1f" (max 0.0 (- (or t1 (float-time)) (or t0 (float-time))))))

(defun carriage-ui--set-tooltip (s)
  "Set state tooltip to S without invalidating the whole modeline.
This avoids heavy redisplay churn during streaming; we only tick the line (throttled)."
  (setq carriage--ui-state-tooltip
        (when (and (stringp s) (> (length s) 0))
          (let* ((mx (or carriage-mode-state-tooltip-max-chars 1000)))
            (carriage-ui--trim-right s mx))))
  ;; Do not invalidate the whole modeline cache on tooltip-only changes.
  ;; A simple mode-line tick is enough; segments will pick up the new help-echo.
  ;; Throttle the tick to avoid per-chunk mode-line work during streaming.
  (let* ((now (float-time))
         (min (or (and (boundp 'carriage-ui-tooltip-update-interval)
                       carriage-ui-tooltip-update-interval)
                  0.12))
         (last (or (and (boundp 'carriage-ui--tooltip-last-update)
                        carriage-ui--tooltip-last-update)
                   0.0)))
    (when (or (not (numberp min))
              (<= min 0)
              (>= (- now last) min))
      (setq carriage-ui--tooltip-last-update now)
      (force-mode-line-update))))

(defun carriage-ui--i18n (key &rest args)
  "Format i18n KEY with ARGS when available; fallback to format."
  (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
      (apply #'carriage-i18n key args)
    (apply #'format (cons (symbol-name key) args))))

(defun carriage-ui--render-state-tooltip (&optional state meta)
  "Render tooltip string for STATE using META (both optional; defaults to current)."
  (let* ((st (or state carriage--ui-state 'idle))
         (m  (or meta carriage--ui-state-meta))
         (phase (plist-get m :phase))
         (src (plist-get m :source))
         (t0  (plist-get m :time-start))
         (t1  (plist-get m :time-last))
         (dur (carriage-ui--fmt-seconds t0 t1))
         (mdl (plist-get m :model))
         (prov (plist-get m :provider))
         (chunks (or (plist-get m :chunks) 0))
         (err (plist-get m :error))
         (tail (carriage-ui--tail (or (plist-get m :reasoning-tail) "") (or carriage-mode-state-tooltip-reasoning-tail 300)))
         (sum  (plist-get m :apply-summary)))
    (pcase st
      ('error
       (let ((code (or (plist-get err :code) "-"))
             (msg  (or (plist-get err :message) "-"))
             (srcs (or src "-")))
         (carriage-ui--i18n :state-tt-error (format "%s" code) (format "%s" msg) (format "%s" srcs))))
      ((or 'sending 'streaming 'dispatch 'waiting)
       (carriage-ui--i18n :state-tt-streaming
                          (or mdl "-")
                          (symbol-name st)
                          dur
                          (or chunks 0)))
      ('reasoning
       (let ((head (carriage-ui--i18n :state-tt-streaming
                                      (or mdl "-")
                                      (symbol-name st)
                                      dur
                                      (or chunks 0))))
         (if (and tail (> (length (string-trim tail)) 0))
             (concat head "\n" (carriage-ui--i18n :state-tt-reasoning tail))
           head)))
      ('done
       ;; Completion of the *request/transport* lifecycle.
       ;; Apply/dry-run results MUST be shown in the separate apply-status badge
       ;; (see `carriage-ui-note-apply-report'), not in the request state tooltip.
       (format "Done: model=%s duration=%ss chunks=%s"
               (or mdl "-") dur (or chunks 0)))
      (_ nil))))

;;; Public note-* API (called by transports/engines/mode)

(defun carriage-ui-note-error (plist)
  "Update state meta and tooltip for an error described by PLIST.
Expected keys: :code, :message, optional :source and :data."
  (let* ((m (or carriage--ui-state-meta '()))
         (err (list :code (plist-get plist :code)
                    :message (plist-get plist :message)
                    :data (plist-get plist :data))))
    (setq carriage--ui-state-meta
          (plist-put m :error err))
    (carriage-ui--set-tooltip
     (carriage-ui--render-state-tooltip 'error carriage--ui-state-meta))))

(defun carriage-ui-note-stream-progress (plist)
  "Update progress-related meta from PLIST and refresh tooltip for sending/streaming phases.
Accepted keys: :model :provider :time-start :inc-chunk (t to increment) :time-last."
  (let* ((m (or carriage--ui-state-meta '()))
         (m (if (plist-get plist :model)     (plist-put m :model (plist-get plist :model)) m))
         (m (if (plist-get plist :provider)  (plist-put m :provider (plist-get plist :provider)) m))
         (m (if (plist-get plist :time-start) (plist-put m :time-start (plist-get plist :time-start)) m))
         (m (if (plist-get plist :time-last) (plist-put m :time-last (plist-get plist :time-last)) m))
         (chunks (or (plist-get m :chunks) 0))
         (m (if (plist-get plist :inc-chunk) (plist-put m :chunks (1+ chunks)) m)))
    (setq carriage--ui-state-meta m)
    (when (memq (or carriage--ui-state 'idle) '(sending streaming waiting dispatch reasoning))
      (carriage-ui--set-tooltip
       (carriage-ui--render-state-tooltip carriage--ui-state carriage--ui-state-meta)))))

(defun carriage-ui-note-reasoning-chunk (text)
  "Update reasoning tail in meta and tooltip with TEXT."
  (let* ((m (or carriage--ui-state-meta '()))
         (old (or (plist-get m :reasoning-tail) ""))
         (tail (concat old (or text ""))))
    (setq carriage--ui-state-meta (plist-put m :reasoning-tail tail))
    (when (eq carriage--ui-state 'reasoning)
      (carriage-ui--set-tooltip
       (carriage-ui--render-state-tooltip 'reasoning carriage--ui-state-meta)))))

(defun carriage-ui-note-apply-summary (plist)
  "Update apply/dry-run status badge from a lightweight summary PLIST.

PLIST keys: :phase ('apply|'dry-run), :ok :skip|:skipped :fail :total.

Important:
- This function MUST NOT write into request/transport state metadata.
- The only UI surface updated here is the separate `apply-status' badge."
  (let* ((sum (list :ok (or (plist-get plist :ok) 0)
                    :skipped (or (plist-get plist :skipped) (plist-get plist :skip) 0)
                    :fail (or (plist-get plist :fail) 0)
                    :total (or (plist-get plist :total) 0)))
         (phase (plist-get plist :phase)))
    ;; Update apply-status badge best-effort (no messages in this legacy API).
    (ignore-errors
      (let* ((report (list :phase phase
                           :summary (list :ok (plist-get sum :ok)
                                          :fail (plist-get sum :fail)
                                          :skipped (plist-get sum :skipped))
                           :messages nil
                           :items nil)))
        (when (fboundp 'carriage-ui-note-apply-report)
          (carriage-ui-note-apply-report report))))))

;;;###autoload
(defun carriage-show-state-details ()
  "Show the current state tooltip in the echo area as a fallback for TTY."
  (interactive)
  (let ((help (or carriage--ui-state-tooltip
                  (carriage-ui--render-state-tooltip))))
    (if (and (stringp help) (> (length (string-trim help)) 0))
        (message "%s" help)
      (message "Нет подробностей статуса"))))

;; -------------------------------------------------------------------
;; Assist integration: optional ranking and context-delta apply (schema-locked)

(defgroup carriage-ui-assist nil
  "Assist (LLM) integration for Carriage UI: ranking and context-delta."
  :group 'carriage-ui
  :prefix "carriage-ui-")

(defcustom carriage-ui-enable-assist-ranking t
  "When non-nil, allow Assist to rank the fixed action palette in menus.
Ranking is advisory only; it never adds new actions or writes into buffers."
  :type 'boolean
  :group 'carriage-ui-assist)

(defun carriage-ui--assist-build-ctx ()
  "Build a lightweight context snapshot for Assist ranking/delta."
  (list
   :buffer (buffer-name)
   :mode (symbol-name major-mode)
   :has-plan (save-excursion
               (goto-char (point-min))
               (re-search-forward "^\\*+\\s-+Plan\\b" nil t))
   :has-tests (save-excursion
                (goto-char (point-min))
                (re-search-forward "^\\*+\\s-+\\(Tests\\|Tests / Checks\\)\\b" nil t))
   :ctx-profile (cond
                 ((and (boundp 'carriage-doc-context-scope)
                       (eq carriage-doc-context-scope 'last)) "last")
                 ((boundp 'carriage-doc-context-scope) (format "%s" carriage-doc-context-scope))
                 (t "all"))))

(defun carriage-ui-rank-actions-with-assist (actions)
  "Return ACTIONS reordered by Assist ranking when available.
ACTIONS is a list of keyspec plists (with :id). Ranking is best-effort and
never adds/removes actions. On errors, returns ACTIONS unchanged."
  (require 'cl-lib)
  (let ((out actions))
    (when (and (require 'carriage-transport-gptel nil t)
               (fboundp 'carriage-assist-suggest))
      (let* ((ctx (carriage-ui--assist-build-ctx))
             (sug (condition-case _ (carriage-assist-suggest ctx) (error nil))))
        (when (and (listp sug)
                   (fboundp 'carriage-assist--suggest-valid-p)
                   (carriage-assist--suggest-valid-p sug))
          (let ((wmap (make-hash-table :test 'eq)))
            (dolist (it sug)
              (puthash (plist-get it :id) (or (plist-get it :weight) 0) wmap))
            (setq out
                  (cl-stable-sort (copy-sequence actions)
                                  (lambda (a b)
                                    (> (gethash (plist-get a :id) wmap 0)
                                       (gethash (plist-get b :id) wmap 0)))))))))
    out))

(defun carriage-ui--context-block-range ()
  "Return cons (BODY-BEG . BODY-END) for the last #+begin_context…#+end_context block, or nil."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search t)
              (last-body-beg nil)
              (last-body-end nil))
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t)
            (let ((beg (line-end-position)))
              (if (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
                  (progn
                    (setq last-body-beg (1+ beg))
                    (setq last-body-end (line-beginning-position)))
                (setq last-body-beg (1+ beg))
                (setq last-body-end (point-max)))))
          (when (and last-body-beg last-body-end (> last-body-end last-body-beg))
            (cons last-body-beg last-body-end)))))))

(defun carriage-ui--context-read-lines ()
  "Read non-empty, non-comment lines from the last begin_context block. Return list of strings."
  (let ((rg (carriage-ui--context-block-range)))
    (if (not rg) '()
      (save-excursion
        (save-restriction
          (narrow-to-region (car rg) (cdr rg))
          (goto-char (point-min))
          (let ((acc '()))
            (while (not (eobp))
              (let* ((ln (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                     (s (string-trim ln)))
                (unless (or (string-empty-p s)
                            (string-prefix-p "#" s)
                            (string-prefix-p ";" s))
                  (push s acc)))
              (forward-line 1))
            (nreverse (delete-dups acc))))))))

(defun carriage-ui--ensure-context-block ()
  "Ensure there is a begin_context block in the current buffer.
Return cons (BODY-BEG . BODY-END) of its body."
  (let ((rg (carriage-ui--context-block-range)))
    (when rg (cl-return-from carriage-ui--ensure-context-block rg))
    ;; Insert a fresh block near the top: after #+ lines
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (looking-at-p "^[ \t]*#\\+") (forward-line 1))
        (unless (bolp) (insert "\n"))
        (let ((start (point)))
          (insert "#+begin_context\n#+end_context\n")
          (cons (+ start (length "#+begin_context\n"))
                (+ start (length "#+begin_context\n"))))))))

(defun carriage-ui--context-normalize-path (s root)
  "Normalize S against ROOT; return repo-relative path or nil."
  (cond
   ;; Quick remote check
   ((and (stringp s) (file-remote-p s)) nil)
   ;; Use context normalizer when available
   ((and (featurep 'carriage-context) (fboundp 'carriage-context--normalize-path))
    (let* ((res (carriage-context--normalize-path s root)))
      (when (and (consp res) (car res))
        (let ((rel (cadr res)))
          ;; Inside root → rel is repo-relative; Outside → absolute truename (reject)
          (when (and (stringp rel)
                     (not (file-name-absolute-p rel))
                     (not (string-empty-p rel)))
            rel)))))
   ;; Fallback: accept only repo-relative strings
   ((and (stringp s)
         (not (file-name-absolute-p s))
         (not (file-remote-p s))
         (not (string-empty-p s)))
    s)
   (t nil)))

(defun carriage-ui--context-prepare-delta (delta root)
  "Prepare cleaned delta lists for begin_context block. Return plist :adds :rem :dropped."
  (let* ((adds-raw (cl-remove-if #'string-empty-p (copy-sequence (or (plist-get delta :add) '()))))
         (rem-raw  (cl-remove-if #'string-empty-p (copy-sequence (or (plist-get delta :remove) '()))))
         (adds (delq nil (mapcar (lambda (s) (carriage-ui--context-normalize-path s root)) adds-raw)))
         (rem  (delq nil (mapcar (lambda (s) (carriage-ui--context-normalize-path s root)) rem-raw)))
         (dropped (- (+ (length adds-raw) (length rem-raw))
                     (+ (length adds)    (length rem)))))
    (list :adds adds :rem rem :dropped dropped)))

(defun carriage-ui--context-write-lines (rg lines)
  "Rewrite begin_context body denoted by RG with LINES list."
  (when (consp rg)
    (save-excursion
      (save-restriction
        (narrow-to-region (car rg) (cdr rg))
        (delete-region (point-min) (point-max))
        (when lines
          (insert (mapconcat #'identity lines "\n") "\n")))))
  t)

(defun carriage-ui--apply-context-delta (delta)
  "Apply DELTA {:add list :remove list} to the last (or ensured) begin_context block.
Paths are normalized and validated:
- Only project-root-relative, local file paths are accepted.
- TRAMP/remote, absolute, or out-of-root paths are ignored with a warning.
All edits occur within a single undo group."
  (require 'carriage-context nil t)
  (let* ((rg   (or (carriage-ui--context-block-range)
                   (carriage-ui--ensure-context-block)))
         (root (or (and (fboundp 'carriage-project-root) (carriage-project-root))
                   default-directory))
         (prep (carriage-ui--context-prepare-delta delta root))
         (adds (plist-get prep :adds))
         (rem  (plist-get prep :rem))
         (dropped (plist-get prep :dropped)))
    (atomic-change-group
      (let* ((cur (carriage-ui--context-read-lines))
             (cur2 (if rem (cl-remove-if (lambda (s) (member s rem)) cur) cur))
             ;; Add new unique lines (append at end)
             (final (append cur2 (cl-remove-if (lambda (s) (member s cur2)) adds))))
        (carriage-ui--context-write-lines rg final)))
    (when (> (or dropped 0) 0)
      (message "Context-delta: skipped %d invalid path(s) (remote/absolute/out-of-root)" dropped))
    t))

;;;###autoload
(defun carriage-ui-context-delta-assist ()
  "Ask Assist to propose context delta and apply after confirmation (schema-locked, no auto-writes).

- Gathers current begin_context lines.
- Requests {:add [] :remove [] :why} from Assist.
- Validates schema; shows preview; applies only on user confirmation.
- Invalid responses never modify the buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Context delta: not an Org buffer"))
  (unless (require 'carriage-transport-gptel nil t)
    (user-error "Assist transport not available"))
  (let* ((cur (carriage-ui--context-read-lines))
         (ctx (list :begin-context cur))
         (delta (condition-case e
                    (carriage-assist-context-delta ctx)
                  (error
                   (message "Assist error: %s" (error-message-string e))
                   nil))))
    (if (not (and (listp delta)
                  (fboundp 'carriage-assist--ctx-delta-valid-p)
                  (carriage-assist--ctx-delta-valid-p delta)))
        (progn
          (message "Assist Context-Delta: invalid or unavailable; no changes")
          nil)
      (let* ((adds (or (plist-get delta :add) '()))
             (rem  (or (plist-get delta :remove) '()))
             (why  (or (plist-get delta :why) "")))
        (if (y-or-n-p
             (concat
              (format "Assist Context-Delta — apply?\nWhy: %s\n" why)
              (format "Add:\n%s\n"
                      (if adds (mapconcat (lambda (s) (concat " + " s)) adds "\n") " (none)"))
              (format "Remove:\n%s\n"
                      (if rem (mapconcat (lambda (s) (concat " - " s)) rem "\n") " (none)"))))
            (progn
              (carriage-ui--apply-context-delta delta)
              (message "Context delta applied")
              t)
          (message "Context delta cancelled")
          nil)))))

(defun carriage-ui--headerline-idle-refresh-run ()
  "Idle worker to refresh outline path string (if dirty) and update the header-line.
Avoids heavy org computations on redisplay path."
  (unwind-protect
      (progn
        (when (and carriage-mode-headerline-show-outline
                   (derived-mode-p 'org-mode))
          (let* ((win (or (get-buffer-window (current-buffer) t)
                          (selected-window)))
                 (w (ignore-errors (and (window-live-p win) (window-total-width win))))
                 (wide (or (null w) (>= w 40))))
            (when (and wide
                       (or carriage-ui--outline-dirty
                           (null carriage-ui--last-outline-path-str)))
              (setq carriage-ui--last-outline-path-str
                    (or (carriage-ui--org-outline-path) ""))
              (setq carriage-ui--outline-dirty nil))))
        (when (get-buffer-window (current-buffer) t)
          (force-mode-line-update)))
    (setq carriage-ui--headerline-idle-timer nil)))

;; -- Event-driven invalidation for UI toggles/selectors (avoid idle churn)
(defvar carriage-ui--advices-installed nil
  "Non-nil when Carriage UI advices for modeline/context invalidation are installed.")

(defun carriage-ui--invalidate-and-refresh (&rest _)
  "Invalidate modeline/icon caches and refresh mode-line now."
  (carriage-ui--invalidate-icon-cache)
  (carriage-ui--invalidate-ml-cache)
  (force-mode-line-update))

(defun carriage-ui--ctx-invalidate-and-refresh (&rest _)
  "Invalidate context badge/modeline and refresh UI now."
  (carriage-ui--ctx-invalidate)
  (force-mode-line-update))

(defun carriage-ui--install-advices ()
  "Install advices to keep modeline in sync on user actions without constant recompute."
  (unless carriage-ui--advices-installed
    (setq carriage-ui--advices-installed t)
    ;; Intent (icon/text must change immediately)
    (when (fboundp 'carriage-toggle-intent)
      (advice-add 'carriage-toggle-intent :after #'carriage-ui--invalidate-and-refresh))
    ;; Suite / Engine / Model selections (labels/icons)
    (when (fboundp 'carriage-select-suite)
      (advice-add 'carriage-select-suite :after #'carriage-ui--invalidate-and-refresh))
    (when (fboundp 'carriage-select-apply-engine)
      (advice-add 'carriage-select-apply-engine :after #'carriage-ui--invalidate-and-refresh))
    (when (fboundp 'carriage-select-model)
      (advice-add 'carriage-select-model :after #'carriage-ui--invalidate-and-refresh))
    ;; Context toggles/scope/profile — bump badge and refresh
    (when (fboundp 'carriage-toggle-include-gptel-context)
      (advice-add 'carriage-toggle-include-gptel-context :after #'carriage-ui--ctx-invalidate-and-refresh))
    (when (fboundp 'carriage-toggle-include-doc-context)
      (advice-add 'carriage-toggle-include-doc-context :after #'carriage-ui--ctx-invalidate-and-refresh))
    (when (fboundp 'carriage-toggle-include-visible-context)
      (advice-add 'carriage-toggle-include-visible-context :after #'carriage-ui--ctx-invalidate-and-refresh))
    (when (fboundp 'carriage-toggle-include-patched-files)
      (advice-add 'carriage-toggle-include-patched-files :after #'carriage-ui--ctx-invalidate-and-refresh))
    (when (fboundp 'carriage-select-doc-context-all)
      (advice-add 'carriage-select-doc-context-all :after #'carriage-ui--ctx-invalidate-and-refresh))
    (when (fboundp 'carriage-select-doc-context-last)
      (advice-add 'carriage-select-doc-context-last :after #'carriage-ui--ctx-invalidate-and-refresh))
    (when (fboundp 'carriage-context-profile-set)
      (advice-add 'carriage-context-profile-set :after #'carriage-ui--ctx-invalidate-and-refresh))
    (when (fboundp 'carriage-toggle-context-profile)
      (advice-add 'carriage-toggle-context-profile :after #'carriage-ui--ctx-invalidate-and-refresh))))

;; Install advices now (safe via fboundp) and also after potential late loads.
(carriage-ui--install-advices)
(with-eval-after-load 'carriage-mode
  (ignore-errors (carriage-ui--install-advices)))


(when nil

  ;; Header-line refresh throttling and selection-aware gating
  ;; - Update header-line only for the selected window (opt-in, default t).
  ;; - Coalesce bursts via a per-window debounce timer.
  ;; - Avoid scheduling repeated timers while one is pending.
  ;;
  ;; This reduces redisplay pressure when Carriage window is visible but point is
  ;; in a different window, and prevents perceived "keyboard stalls" due to heavy
  ;; header-line recomputation on every scroll/command in other windows.

  (defgroup carriage-ui-performance nil
    "Performance-related settings for Carriage UI."
    :group 'carriage)

  (defcustom carriage-ui-headerline-only-selected-window t
    "When non-nil, update Carriage header-line only for the selected window."
    :type 'boolean :group 'carriage-ui-performance)

  (defcustom carriage-ui-headerline-debounce-ms 120
    "Minimum delay in milliseconds between header-line refreshes per window."
    :type 'integer :group 'carriage-ui-performance)

  (defun carriage-ui--hl--selected-window-p (win)
    "Return non-nil if WIN is the currently selected window."
    (and (window-live-p win) (eq win (selected-window))))

  (defun carriage-ui--hl--window-from-args (args)
    "Try to extract a window from ARGS; fallback to `selected-window'."
    (let ((w (car args)))
      (if (window-live-p w) w (selected-window))))

  (defun carriage-ui--hl--debounce (win fn args)
    "Debounce FN ARGS for WIN using per-window timer."
    (let ((existing (and (window-live-p win)
                         (window-parameter win 'carriage-ui--hl-timer))))
      (unless (timerp existing)
        (let* ((delay (/ (max 0 (or carriage-ui-headerline-debounce-ms 0)) 1000.0))
               (timerobj
                (run-at-time
                 delay nil
                 (lambda ()
                   (when (window-live-p win)
                     (set-window-parameter win 'carriage-ui--hl-timer nil)
                     (when (or (not carriage-ui-headerline-only-selected-window)
                               (carriage-ui--hl--selected-window-p win))
                       (apply fn args)))))))
          (when (window-live-p win)
            (set-window-parameter win 'carriage-ui--hl-timer timerobj))))))

  ;; Gate and coalesce header-line refresh requests.
  (ignore-errors
    (advice-add
     'carriage-ui--headerline-queue-refresh :around
     (lambda (orig-fn &rest args)
       (let ((win (carriage-ui--hl--window-from-args args)))
         (if (and carriage-ui-headerline-only-selected-window
                  (not (carriage-ui--hl--selected-window-p win)))
             ;; Skip refresh for non-selected windows entirely.
             nil
           ;; Coalesce bursts via debounce.
           (carriage-ui--hl--debounce win orig-fn args))))))

  ;; Avoid doing work on window-scroll for non-selected windows.
  (ignore-errors
    (advice-add
     'carriage-ui--headerline-window-scroll :around
     (lambda (orig-fn &rest args)
       (let ((win (carriage-ui--hl--window-from-args args)))
         (when (or (not carriage-ui-headerline-only-selected-window)
                   (carriage-ui--hl--selected-window-p win))
           (apply orig-fn args))))))

  ;; Header-line refresh throttling and selection gating
  ;; Goal:
  ;; - Update header line only when this buffer is shown in the selected window.
  ;; - Debounce updates to avoid redisplay storms.
  ;; - Still allow frequent enough updates for spinner animation while streaming.
  ;;
  ;; This layer wraps carriage-ui--headerline-queue-refresh with a small scheduler.
  ;; It coalesces rapid calls and skips refresh when the buffer is not in the
  ;; selected window, preventing heavy work while cursor is elsewhere.

  (defgroup carriage-ui-headerline nil
    "Throttling and gating for Carriage header-line refresh."
    :group 'carriage)

  (defcustom carriage-ui-headerline-debounce-ms 80
    "Minimum debounce for header-line refresh in milliseconds.
Keep small enough for spinner animation (60–120 ms)."
    :type 'integer :group 'carriage-ui-headerline)

  (defcustom carriage-ui-headerline-streaming-debounce-ms 60
    "Debounce for header-line during streaming/reasoning (ms)."
    :type 'integer :group 'carriage-ui-headerline)

  (defvar-local carriage-ui--hl-refresh-timer nil
    "Coalescing timer for header-line refresh in this buffer.")

  (defun carriage-ui--headerline--buffer-in-selected-window-p ()
    "Return non-nil if current buffer is shown in the selected window."
    (let ((w (get-buffer-window (current-buffer) t)))
      (and (window-live-p w)
           (eq w (selected-window)))))

  (defun carriage-ui--headerline--streaming-p ()
    "Best-effort check: return non-nil when UI is in streaming/reasoning/sending phase.
We avoid hard deps; rely on public note fns to set state elsewhere.
Fallback to nil when state is unavailable."
    (condition-case _e
        (let* ((sym (and (boundp 'carriage-ui--state) 'carriage-ui--state))
               (st  (and sym (symbol-value sym)))
               (ph  (and (listp st) (plist-get st :phase))))
          (memq ph '(sending streaming reasoning)))
      (error nil)))

  (defun carriage-ui--headerline--schedule (thunk)
    "Schedule THUNK with debounce; coalesce rapid calls for this buffer."
    (let* ((delay-ms (if (carriage-ui--headerline--streaming-p)
                         carriage-ui-headerline-streaming-debounce-ms
                       carriage-ui-headerline-debounce-ms))
           (delay (max 0.01 (/ (float (or delay-ms 80)) 1000.0))))
      (unless (timerp carriage-ui--hl-refresh-timer)
        (setq carriage-ui--hl-refresh-timer
              (run-at-time
               delay nil
               (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (setq carriage-ui--hl-refresh-timer nil)
                     ;; Refresh only if this buffer is visible in the selected window
                     (when (carriage-ui--headerline--buffer-in-selected-window-p)
                       (condition-case err
                           (funcall thunk)
                         (error
                          (ignore-errors
                            (when (fboundp 'carriage-log)
                              (carriage-log "headerline refresh error: %s"
                                            (error-message-string err))))))))))
               (current-buffer))))))

  (defun carriage-ui--headerline--advice-queue (orig-fn &rest args)
    "Around advice for `carriage-ui--headerline-queue-refresh'.
- Skip when buffer is not in selected window (no need to update header-line).
- Debounce calls to avoid redisplay storms.
- While streaming, use a slightly faster debounce for spinner."
    (if (not (carriage-ui--headerline--buffer-in-selected-window-p))
        ;; Not the selected window — skip refresh entirely.
        nil
      ;; Selected window — coalesce via timer.
      (carriage-ui--headerline--schedule (lambda () (apply orig-fn args)))))

  ;; Install advice if the function is present
  (when (fboundp 'carriage-ui--headerline-queue-refresh)
    (advice-add 'carriage-ui--headerline-queue-refresh :around
                #'carriage-ui--headerline--advice-queue))

  ;; Header-line refresh throttling and selection gating (no unnecessary updates
  ;; when this buffer is not in the selected window; coalesce bursts).
  (defgroup carriage-ui-headerline nil
    "Throttle and gating options for Carriage header-line updates."
    :group 'carriage)

  (defcustom carriage-ui-headerline-debounce-ms 80
    "Minimum milliseconds between header-line refreshes in the selected window.
Small values (e.g., 60–120 ms) allow spinner animation while avoiding heavy redisplay."
    :type 'integer :group 'carriage-ui-headerline)

  (defvar-local carriage-ui--headerline-last-run 0.0
    "Timestamp (float-time) of the last executed header-line refresh in this buffer.")

  (defvar-local carriage-ui--headerline-timer nil
    "Pending timer object to coalesce header-line refresh requests in this buffer.")

  (defun carriage-ui--headerline--selected-buffer-p ()
    "Return non-nil when current buffer is the buffer of the selected window."
    (let ((sel (selected-window)))
      (and (window-live-p sel)
           (eq (current-buffer) (window-buffer sel)))))

  (defun carriage-ui--headerline--coalesced-call (orig args)
    "Coalesce header-line refresh calls to avoid over-refreshing.
Execute ORIG with ARGS either immediately (if outside the debounce window) or schedule
a single delayed run to honor `carriage-ui-headerline-debounce-ms'."
    (let* ((now (float-time))
           (last (or carriage-ui--headerline-last-run 0.0))
           (deb (max 0.0 (/ (float (or carriage-ui-headerline-debounce-ms 80)) 1000.0)))
           (elapsed (- now last)))
      (if (>= elapsed deb)
          (progn
            (setq carriage-ui--headerline-last-run now)
            (apply orig args))
        ;; Schedule one delayed run; cancel any previous pending timer.
        (when (timerp carriage-ui--headerline-timer)
          (ignore-errors (cancel-timer carriage-ui--headerline-timer)))
        (let* ((delay (max 0.0 (- deb elapsed)))
               (buf (current-buffer)))
          (setq carriage-ui--headerline-timer
                (run-at-time
                 delay nil
                 (lambda ()
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (setq carriage-ui--headerline-timer nil)
                       (setq carriage-ui--headerline-last-run (float-time))
                       (apply orig args))))))))
      ;; Return nil (advice wrapper does not need to return anything specific)
      nil))

  ;; Around advice for queue refresh: run only when this buffer is in the selected window
  ;; and coalesce rapid calls to keep redisplay fast and keyboard responsive.
  (when (fboundp 'carriage-ui--headerline-queue-refresh)
    (advice-add
     'carriage-ui--headerline-queue-refresh :around
     (lambda (orig &rest args)
       (when (carriage-ui--headerline--selected-buffer-p)
         (carriage-ui--headerline--coalesced-call orig args)))))

  ;; Optional gating for scroll-driven refreshers as well (only in selected window).
  (when (fboundp 'carriage-ui--headerline-window-scroll)
    (advice-add
     'carriage-ui--headerline-window-scroll :around
     (lambda (orig &rest args)
       (when (carriage-ui--headerline--selected-buffer-p)
         (carriage-ui--headerline--coalesced-call orig args)))))

  ;; Integrated robust spinner/header-line safeguards (formerly carriage-preloader-hotfix)

  (defun carriage-ui--hl--debounce-safe (win fn args)
    "Safe debounce implementation for header-line refresh, avoiding void vars."
    (let ((existing (and (window-live-p win)
                         (window-parameter win 'carriage-ui--hl-timer))))
      (unless (timerp existing)
        (let* ((ms (or (and (boundp 'carriage-ui-headerline-debounce-ms)
                            carriage-ui-headerline-debounce-ms)
                       80))
               (delay (max 0.0 (/ (float ms) 1000.0)))
               (timerobj
                (run-at-time
                 delay nil
                 (lambda ()
                   (when (window-live-p win)
                     (set-window-parameter win 'carriage-ui--hl-timer nil)
                     (when (or (not (boundp 'carriage-ui-headerline-only-selected-window))
                               (not carriage-ui-headerline-only-selected-window)
                               (eq win (selected-window)))
                       (apply fn args)))))))
          (when (window-live-p win)
            (set-window-parameter win 'carriage-ui--hl-timer timerobj))))))

  (when (fboundp 'carriage-ui--hl--debounce)
    (unless (advice-member-p #'carriage-ui--hl--debounce-safe 'carriage-ui--hl--debounce)
      (advice-add 'carriage-ui--hl--debounce :around
                  (lambda (orig win fn args)
                    (ignore orig)
                    (carriage-ui--hl--debounce-safe win fn args)))))

  (when (fboundp 'carriage-ui--headerline-idle-refresh-run)
    (unless (advice-member-p #'carriage-ui--headerline-idle-safe 'carriage-ui--headerline-idle-refresh-run)
      (defun carriage-ui--headerline-idle-safe (orig &rest args)
        (condition-case _e
            (apply orig args)
          (error
           (ignore-errors (message "carriage headerline idle error (guarded)")))))
      (advice-add 'carriage-ui--headerline-idle-refresh-run :around
                  #'carriage-ui--headerline-idle-safe)))

  ;; Sane debounce wrapper: guards against stale/broken implementations that may signal void variables (e.g., 'tm).
  (when (fboundp 'carriage-ui--hl--debounce)
    (unless (advice-member-p 'carriage-ui--hl--debounce@sane 'carriage-ui--hl--debounce)
      (defun carriage-ui--hl--debounce@sane (orig win fn args)
        (condition-case _
            (funcall orig win fn args)
          (error
           (let ((existing (and (window-live-p win)
                                (window-parameter win 'carriage-ui--hl-timer))))
             (unless (timerp existing)
               (let* ((delay (/ (max 0 (or carriage-ui-headerline-debounce-ms 0)) 1000.0))
                      (timerobj
                       (run-at-time
                        delay nil
                        (lambda ()
                          (when (window-live-p win)
                            (set-window-parameter win 'carriage-ui--hl-timer nil)
                            (when (or (not carriage-ui-headerline-only-selected-window)
                                      (carriage-ui--hl--selected-window-p win))
                              (apply fn args))))))))
               (when (window-live-p win)
                 (set-window-parameter win 'carriage-ui--hl-timer timerobj)))))))
      (advice-add 'carriage-ui--hl--debounce :around #'carriage-ui--hl--debounce@sane)))

  ;; Hard guard: swallow headerline refresh errors (e.g., stray 'tm) so they don't
  ;; break timers/overlays (preloader spinner) or spam the minibuffer.
  (unless (advice-member-p 'carriage-ui--headerline-idle-safe 'carriage-ui--headerline-idle-refresh-run)
    (defun carriage-ui--headerline-idle-safe (orig &rest args)
      (condition-case e
          (apply orig args)
        (error
         (ignore-errors
           (message "headerline refresh error: %s" (error-message-string e))))))

    (advice-add 'carriage-ui--headerline-idle-refresh-run :around
                #'carriage-ui--headerline-idle-safe))

  ;; Debounce gate: robust wrapper that never references unknown symbols (e.g., 'tm)
  ;; and falls back to a per-window timer if the primary gate errors.
  (unless (advice-member-p 'carriage-ui--hl--debounce@sane 'carriage-ui--hl--debounce)
    (defun carriage-ui--hl--debounce@sane (orig win fn args)
      (condition-case _
          (funcall orig win fn args)
        (error
         (let ((existing (and (window-live-p win)
                              (window-parameter win 'carriage-ui--hl-timer))))
           (unless (timerp existing)
             (let* ((delay (/ (max 0 (or carriage-ui-headerline-debounce-ms 0)) 1000.0))
                    (timerobj
                     (run-at-time
                      delay nil
                      (lambda ()
                        (when (window-live-p win)
                          (set-window-parameter win 'carriage-ui--hl-timer nil)
                          (when (or (not (boundp 'carriage-ui-headerline-only-selected-window))
                                    (not carriage-ui-headerline-only-selected-window)
                                    (eq win (selected-window)))
                            (apply fn args))))))))
             (when (window-live-p win)
               (set-window-parameter win 'carriage-ui--hl-timer timerobj)))))))
    (advice-add 'carriage-ui--hl--debounce :around #'carriage-ui--hl--debounce@sane))



  )

;; Integrated guards and cursor policy for inline ID and streaming cursor behavior.

(defvar-local carriage--inline-id-inserted nil
  "Non-nil when the inline CARRIAGE_ITERATION_ID has already been inserted in this buffer for the current request.")

(defvar-local carriage--fingerprint-inserted nil
  "Non-nil when the per-send CARRIAGE_FINGERPRINT line has already been inserted for the current request.")

(defun carriage--inline-id-reset (&rest _)
  "Reset per-request inline-id/fingerprint flags."
  (setq carriage--inline-id-inserted nil)
  (setq carriage--fingerprint-inserted nil))

(defun carriage--fingerprint--collect ()
  "Collect a small, response-shaping fingerprint plist (best-effort).
This must be safe and never signal. Only include keys that affect:
- what the model does (intent/suite/model),
- and what context it sees (ctx toggles/scope/profile/budgets/injection)."
  (condition-case _e
      (let* ((raw
              (cond
               ;; Prefer doc-state collector when available (already returns :CAR_* keys)
               ((and (require 'carriage-doc-state nil t)
                     (fboundp 'carriage-doc-state--collect-current))
                (ignore-errors (carriage-doc-state--collect-current)))
               (t
                ;; Fallback: minimal set from common vars
                (let ((pl '()))
                  (when (boundp 'carriage-mode-intent)  (setq pl (plist-put pl :CAR_INTENT carriage-mode-intent)))
                  (when (boundp 'carriage-mode-suite)   (setq pl (plist-put pl :CAR_SUITE carriage-mode-suite)))
                  (when (boundp 'carriage-mode-model)   (setq pl (plist-put pl :CAR_MODEL carriage-mode-model)))
                  (when (boundp 'carriage-mode-backend) (setq pl (plist-put pl :CAR_BACKEND carriage-mode-backend)))
                  (when (boundp 'carriage-mode-provider) (setq pl (plist-put pl :CAR_PROVIDER carriage-mode-provider)))
                  (when (boundp 'carriage-mode-include-doc-context) (setq pl (plist-put pl :CAR_CTX_DOC carriage-mode-include-doc-context)))
                  (when (boundp 'carriage-mode-include-gptel-context) (setq pl (plist-put pl :CAR_CTX_GPTEL carriage-mode-include-gptel-context)))
                  (when (boundp 'carriage-mode-include-visible-context) (setq pl (plist-put pl :CAR_CTX_VISIBLE carriage-mode-include-visible-context)))
                  (when (boundp 'carriage-mode-include-patched-files) (setq pl (plist-put pl :CAR_CTX_PATCHED carriage-mode-include-patched-files)))
                  (when (boundp 'carriage-doc-context-scope) (setq pl (plist-put pl :CAR_DOC_CTX_SCOPE carriage-doc-context-scope)))
                  (when (boundp 'carriage-context-profile) (setq pl (plist-put pl :CAR_CTX_PROFILE carriage-context-profile)))
                  (when (boundp 'carriage-mode-context-injection) (setq pl (plist-put pl :CAR_CTX_INJECTION carriage-mode-context-injection)))
                  (when (boundp 'carriage-mode-context-max-files) (setq pl (plist-put pl :CAR_CTX_MAX_FILES carriage-mode-context-max-files)))
                  (when (boundp 'carriage-mode-context-max-total-bytes) (setq pl (plist-put pl :CAR_CTX_MAX_BYTES carriage-mode-context-max-total-bytes)))
                  pl))))
             ;; Whitelist fingerprint keys: response-shaping + context-shaping (INCLUDING budgets).
             ;; Avoid UI-only toggles.
             (imp (if (listp raw)
                      (let ((pl raw)
                            (out '()))
                        (dolist (k '(:CAR_INTENT :CAR_SUITE :CAR_MODEL :CAR_BACKEND :CAR_PROVIDER
                                                 :CAR_CTX_DOC :CAR_CTX_GPTEL :CAR_CTX_VISIBLE :CAR_CTX_PATCHED
                                                 :CAR_DOC_CTX_SCOPE :CAR_CTX_PROFILE :CAR_CTX_INJECTION
                                                 :CAR_CTX_MAX_FILES :CAR_CTX_MAX_BYTES))
                          (when (plist-member pl k)
                            (setq out (plist-put out k (plist-get pl k)))))
                        out)
                    raw))
             (iter (and (boundp 'carriage--last-iteration-id) carriage--last-iteration-id))
             (ts (format-time-string "%Y-%m-%d %H:%M:%S")))
        (setq imp (or imp '()))
        ;; Add lightweight metadata without polluting :CAR_* namespace.
        (plist-put (plist-put imp :ITER iter) :TS ts))
    (error (list :TS (format-time-string "%Y-%m-%d %H:%M:%S")))))

(defun carriage--fingerprint--line ()
  "Return the canonical fingerprint line text."
  (format "#+CARRIAGE_FINGERPRINT: %s\n"
          (prin1-to-string (or (carriage--fingerprint--collect) '()))))

(defun carriage--fingerprint--upsert-after-pos (pos)
  "Upsert fingerprint line immediately after POS's line (best-effort, never signal)."
  (ignore-errors
    (when (and (numberp pos)
               (>= pos (point-min))
               (<= pos (point-max))
               (derived-mode-p 'org-mode))
      (save-excursion
        (goto-char pos)
        (beginning-of-line)
        (forward-line 1)
        (let ((inhibit-read-only t)
              (case-fold-search t))
          ;; If a fingerprint line is already right here, replace it (idempotent).
          (when (looking-at "^[ \t]*#\\+CARRIAGE_FINGERPRINT\\b.*$")
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position)))))
          (insert (carriage--fingerprint--line)))))))

;; Reset the flag whenever a new stream is initialized.
(ignore-errors
  (advice-add 'carriage-stream-reset :after #'carriage--inline-id-reset))

(defun carriage--inline-id-around (orig-fun &rest args)
  "Guard duplicate inline-id insertions. If already inserted, skip.
Fingerprint insertion is handled by send commands (carriage-mode) to avoid duplicates."
  (if carriage--inline-id-inserted
      ;; Return a sensible position for downstream code: prefer stream-origin,
      ;; otherwise current point.
      (or (and (boundp 'carriage--stream-origin-marker)
               (markerp carriage--stream-origin-marker)
               (marker-position carriage--stream-origin-marker))
          (point))
    (let ((res (apply orig-fun args)))
      (setq carriage--inline-id-inserted t)
      res)))

(ignore-errors
  ;; Ensure any late call to insert the marker becomes a no-op when already inserted.
  (advice-add 'carriage-insert-inline-iteration-marker-now :around #'carriage--inline-id-around))

;; Keep user cursor free during streaming unless explicitly enabled.
(defvar carriage-mode-preloader-follow-point
  (and (boundp 'carriage-mode-preloader-follow-point)
       carriage-mode-preloader-follow-point)
  "When non-nil, move point to the streaming tail on each chunk. Defaults to nil.")

(defcustom carriage-stream-redisplay-interval 0.03
  "Minimum seconds between forced redisplay calls during streaming chunk insertion.

Goal: keep stream output visibly updating even without user input and even when the
window is not selected, while staying cheap (throttled)."
  :type 'number
  :group 'carriage-ui)

(defvar-local carriage--stream-last-redisplay 0.0
  "Last (float-time) when we forced redisplay due to a streaming chunk in this buffer.")

(defun carriage--stream-maybe-redisplay (&optional buffer)
  "Force a throttled redisplay when BUFFER is visible in any window.
This avoids the \"prints only when I move the cursor\" effect during streaming."
  (let* ((buf (or buffer (current-buffer)))
         (win (and (buffer-live-p buf) (get-buffer-window buf t)))
         (now (float-time)))
    (when (and (window-live-p win)
               (>= (- now (or (buffer-local-value 'carriage--stream-last-redisplay buf) 0.0))
                   (max 0.0 (or carriage-stream-redisplay-interval 0.03))))
      (with-current-buffer buf
        (setq carriage--stream-last-redisplay now))
      ;; Use forced redisplay so updates are visible without user input.
      (redisplay t))))

(defun carriage--stream-chunk-save-point (orig-fun &rest args)
  "Call ORIG-FUN without moving point unless follow-point is enabled.
Also ensure streaming output becomes visible without requiring user input (throttled)."
  (prog1
      (if carriage-mode-preloader-follow-point
          (apply orig-fun args)
        (save-excursion
          (apply orig-fun args)))
    (ignore-errors (carriage--stream-maybe-redisplay (current-buffer)))))

(ignore-errors
  (advice-add 'carriage-insert-stream-chunk :around #'carriage--stream-chunk-save-point))

;;; Integrated guards: single inline-id insertion and free cursor during streaming

;; -------------------------------------------------------------------
;; Performance overrides (must be the last definitions in the file).
;;
;; These functions are allowed to be invoked indirectly from redisplay/modeline
;; and therefore MUST be O(1) and MUST NOT read files / scan buffers.
;;
;; NOTE: Everything below is legacy/iterative "final override" debris kept in-tree
;; during refactors. It is now disabled to avoid accidental redefinition storms.
(when nil

  (defun carriage-ui--last-iteration-present-p ()
    "Return non-nil when there are patch blocks associated with the last iteration.

Performance invariant:
- Must be O(1) and safe for redisplay/modeline.
- The source of truth is a buffer-local flag maintained by streaming/accept code."
    (and (boundp 'carriage--last-iteration-has-patches)
         carriage--last-iteration-has-patches))

  (defun carriage-ui--compute-context-badge (inc-doc inc-gpt inc-vis inc-patched)
    "Compute context badge (LABEL . HINT) for toggles using `carriage-context-count' (fast).

Important (perf):
- MUST NOT call `carriage-context-collect' (reads file contents).
- MUST NOT scan the buffer.
The UI only needs counts + per-item omission reason, which `carriage-context-count'
provides without reading file contents."
    (let* ((off (not (or inc-doc inc-gpt inc-vis inc-patched))))
      (if off
          (cons "[Ctx:-]" "Контекст выключен (doc=off, gptel=off, vis=off, patched=off)")
        (let* ((res (and (fboundp 'carriage-context-count)
                         (ignore-errors
                           (carriage-context-count (current-buffer) (point)))))
               (cnt  (or (and (listp res) (plist-get res :count)) 0))
               (items (or (and (listp res) (plist-get res :items)) '()))
               (warns (or (and (listp res) (plist-get res :warnings)) '()))
               (limit (or (and (boundp 'carriage-ui-context-tooltip-max-items)
                               carriage-ui-context-tooltip-max-items)
                          50))
               (n (length items))
               (shown (if (and (integerp limit) (> limit 0))
                          (cl-subseq items 0 (min n limit))
                        items))
               (more (max 0 (- n (length shown))))
               (head-line (format "Контекст: файлов=%d — источники: doc=%s, gptel=%s, vis=%s, patched=%s, scope=%s, profile=%s"
                                  cnt
                                  (if inc-doc "on" "off")
                                  (if inc-gpt "on" "off")
                                  (if inc-vis "on" "off")
                                  (if inc-patched "on" "off")
                                  (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                                    (if (eq sc 'last) "last" "all"))
                                  (let ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)))
                                    (if (eq pr 'p3) "P3" "P1"))))
               (warn-lines (when warns
                             (cons "Предупреждения:"
                                   (mapcar (lambda (w) (concat " - " w)) warns))))
               (item-lines (when shown
                             (cons "Элементы:"
                                   (append (mapcar #'carriage-ui--context-item->line shown)
                                           (when (> more 0)
                                             (list (format "… (+%d more)" more))))))))
          (cons (format "[Ctx:%d]" cnt)
                (mapconcat #'identity
                           (delq nil (list head-line
                                           ;; Profile/limits line with explicit warning for P3
                                           (let* ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile))
                                                  (mf (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files))
                                                  (mb (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes)))
                                             (format "Профиль: %s — лимиты: файлы=%s байты=%s%s"
                                                     (if (eq pr 'p3) "P3-debug" "P1-core")
                                                     (or mf "-") (or mb "-")
                                                     (if (eq pr 'p3) " (внимание: расширенный бюджет — выше стоимость/шум)" "")))
                                           (and warn-lines (mapconcat #'identity warn-lines "\n"))
                                           (and item-lines (mapconcat #'identity item-lines "\n"))))
                           "\n"))))))

  ;; -------------------------------------------------------------------
  ;; Context badge (Ctx:N) — robust cached implementation (performance critical).
  ;;
  ;; Goals:
  ;; - NEVER compute context (or scan buffers/files) on redisplay/modeline path.
  ;; - Keep UI "fresh enough" by recomputing on idle with debounce.
  ;; - Fix Ctx:0 flicker by keeping last known good badge and using a placeholder
  ;;   until the first compute finishes.
  ;;
  ;; This section intentionally overrides earlier implementations in this file.
  ;; -------------------------------------------------------------------

  (defvar-local carriage-ui--ctx-badge-cache nil
    "Cached context badge cons cell (LABEL . TOOLTIP) for the current buffer.")

  (defvar-local carriage-ui--ctx-badge-cache-sig nil
    "Signature (plist) for `carriage-ui--ctx-badge-cache' validity.")

  (defvar-local carriage-ui--ctx-badge-refresh-timer nil
    "Idle timer used to debounce context badge recomputation.")

  (defvar-local carriage-ui--ctx-badge-pending nil
    "Non-nil when an idle recomputation of the context badge is scheduled.")

  (defcustom carriage-ui-context-badge-refresh-interval 1.0
    "Minimum seconds between context badge recomputations for a buffer.

This controls how often [Ctx:N] may update while you move point/scroll.
Lower values increase UI freshness but may add overhead; 1.0 is a safe default."
    :type 'number
    :group 'carriage-ui)

  (defvar-local carriage-ui--ctx-badge-last-time 0.0
    "Timestamp (float-time) of the last successful context badge computation for this buffer.")

  (defun carriage-ui--ctx--toggles-snapshot ()
    "Return a plist snapshot of context-related toggles/options affecting the badge."
    (list
     :inc-doc (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context)
     :inc-gpt (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context)
     :inc-vis (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context)
     :inc-patched (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files)
     :scope (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
     :profile (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)
     :ctx-max-files (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files)
     :ctx-max-bytes (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes)))

  (defun carriage-ui--ctx--badge-signature ()
    "Return signature for context badge cache validity in the current buffer."
    (append
     (list :algo 2
           :mode major-mode)
     (carriage-ui--ctx--toggles-snapshot)))

  (defun carriage-ui--ctx--project-root ()
    "Return project root for context normalization."
    (or (and (fboundp 'carriage-project-root) (ignore-errors (carriage-project-root)))
        default-directory))

  (defun carriage-ui--ctx--normalize-doc-line (s root)
    "Return a doc-context candidate line S for UI counting (best-effort).

Important (perf/UX):
- This is used ONLY for the UI badge/tooltip (Ctx:N), not for request building.
- For UI we must be robust: accept absolute local paths even when they are outside ROOT,
  otherwise an Org doc outside the project would always show Ctx:0.
- Remote/TRAMP paths are still ignored in v1.
Real normalization/validation happens later in `carriage-context-collect'."
    (ignore root)
    (let* ((x (string-trim (or s ""))))
      (cond
       ((or (string-empty-p x)
            (string-prefix-p "#" x)
            (string-prefix-p ";" x))
        nil)
       ;; Keep v1 invariant: do not accept remote paths.
       ((file-remote-p x) nil)
       ;; Accept absolute and relative as-is for UI purposes.
       ((file-name-absolute-p x) x)
       (t
        ;; For relative paths, keep a minimal traversal guard.
        (when (and (not (string-match-p "\\`\\.\\./" x))
                   (not (string-match-p "/\\.\\./" x)))
          x)))))

  (defun carriage-ui--ctx--doc-lines-from-buffer (&optional scope)
    "Return list of normalized doc-context repo-relative paths from begin_context blocks.
SCOPE is 'all or 'last (defaults to current `carriage-doc-context-scope' semantics)."
    (let* ((root (carriage-ui--ctx--project-root))
           (sc (or scope (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope) 'all))
           (case-fold-search t)
           (blocks '()))
      (when (derived-mode-p 'org-mode)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t)
              (let ((b (save-excursion (forward-line 1) (point)))
                    (e nil))
                (if (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
                    (setq e (line-beginning-position))
                  (setq e (point-max)))
                (push (cons b e) blocks)))))
        (setq blocks (nreverse blocks))
        (when (eq sc 'last)
          (setq blocks (and blocks (list (car (last blocks))))))
        (let ((acc '()))
          (dolist (rg blocks)
            (when (and (consp rg) (< (car rg) (cdr rg)))
              (save-excursion
                (save-restriction
                  (narrow-to-region (car rg) (cdr rg))
                  (goto-char (point-min))
                  (while (not (eobp))
                    (let* ((ln (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                           (rel (carriage-ui--ctx--normalize-doc-line ln root)))
                      (when (and (stringp rel) (not (string-empty-p rel)))
                        (push rel acc)))
                    (forward-line 1)))))))
        (nreverse (delete-dups (delq nil acc))))))

  (defun carriage-ui--ctx--format-tooltip (count items &optional warnings)
    "Build tooltip string for context badge."
    (let* ((head (format "Контекст: файлов=%d" (or count 0)))
           (warn (when (and (listp warnings) warnings)
                   (concat "Предупреждения:\n"
                           (mapconcat (lambda (w) (concat " - " w)) warnings "\n"))))
           (body (when (and (listp items) items)
                   (let* ((limit (or (and (boundp 'carriage-ui-context-tooltip-max-items)
                                          carriage-ui-context-tooltip-max-items)
                                     50))
                          (shown (cl-subseq items 0 (min (length items) (max 0 limit))))
                          (more (max 0 (- (length items) (length shown)))))
                     (concat "Элементы:\n"
                             (mapconcat (lambda (p) (concat " - " p)) shown "\n")
                             (when (> more 0) (format "\n… (+%d more)" more)))))))
      (mapconcat #'identity (delq nil (list head warn body)) "\n")))

  (defun carriage-ui--ctx--compute-badge ()
    "Compute context badge (LABEL . TOOLTIP) for current buffer (may be moderately expensive).

Correctness goal:
- Must match what would be sent from *current point* (scope=last is point-relative).
- Uses `carriage-context-count' (fast; no file reads) and therefore counts all enabled
  sources (doc/gptel/visible/patched) under current limits.

Perf goal:
- MUST NOT run on redisplay; only from our debounced timer."
    (let* ((tog (carriage-ui--ctx--toggles-snapshot))
           (inc-doc (plist-get tog :inc-doc))
           (inc-gpt (plist-get tog :inc-gpt))
           (inc-vis (plist-get tog :inc-vis))
           (inc-pat (plist-get tog :inc-patched))
           (off (not (or inc-doc inc-gpt inc-vis inc-pat))))
      (if off
          (cons "[Ctx:-]" "Контекст выключен (doc=off, gptel=off, vis=off, patched=off)")
        (require 'carriage-context nil t)
        (let* ((res (and (fboundp 'carriage-context-count)
                         (ignore-errors (carriage-context-count (current-buffer) (point)))))
               (cnt   (or (and (listp res) (plist-get res :count)) 0))
               (items (or (and (listp res) (plist-get res :items)) '()))
               (warns (or (and (listp res) (plist-get res :warnings)) '()))
               (stats (or (and (listp res) (plist-get res :stats)) '()))
               (limit (or (and (boundp 'carriage-ui-context-tooltip-max-items)
                               carriage-ui-context-tooltip-max-items)
                          50))
               (n (length items))
               (shown (if (and (integerp limit) (> limit 0))
                          (cl-subseq items 0 (min n limit))
                        items))
               (more (max 0 (- n (length shown))))
               (head
                (format "Контекст (fast, ≤%.1fs): файлов=%d — doc=%s gptel=%s vis=%s patched=%s scope=%s проф=%s — лимиты: files=%s bytes=%s — бюджет: included=%s skipped=%s bytes=%s"
                        (or (and (boundp 'carriage-ui-context-badge-refresh-interval)
                                 carriage-ui-context-badge-refresh-interval)
                            1.0)
                        cnt
                        (if inc-doc "on" "off")
                        (if inc-gpt "on" "off")
                        (if inc-vis "on" "off")
                        (if inc-pat "on" "off")
                        (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                          (if (eq sc 'last) "last" "all"))
                        (let ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)))
                          (if (eq pr 'p3) "P3" "P1"))
                        (or (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files) "-")
                        (or (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes) "-")
                        (or (plist-get stats :included) 0)
                        (or (plist-get stats :skipped) 0)
                        (or (plist-get stats :total-bytes) 0)))
               (warn-lines (when warns
                             (cons "Предупреждения:"
                                   (mapcar (lambda (w) (concat " - " w)) warns))))
               (item-lines
                (cons "Элементы:"
                      (append (mapcar #'carriage-ui--context-item->line shown)
                              (when (> more 0)
                                (list (format "… (+%d more)" more))))))
               (tip (mapconcat #'identity
                               (delq nil
                                     (list head
                                           (and warn-lines (mapconcat #'identity warn-lines "\n"))
                                           (and item-lines (mapconcat #'identity item-lines "\n"))))
                               "\n")))
          (cons (format "[Ctx:%d]" cnt) tip)))))

  (defun carriage-ui--ctx--apply-badge (badge sig)
    "Install computed BADGE with SIG into cache and refresh UI.
Normalizes BADGE to a cons cell (LABEL . TOOLTIP) to keep modeline code robust."
    (let* ((b (cond
               ((consp badge) badge)
               ((stringp badge)
                (cons badge
                      (or (get-text-property 0 'help-echo badge)
                          "Контекст: подробности недоступны")))
               (t (cons "[Ctx:?]" "Контекст: вычисление… (обновится автоматически)")))))
      (setq carriage-ui--ctx-badge-cache b)
      (setq carriage-ui--ctx-badge-cache-sig sig)
      (setq carriage-ui--ctx-badge-last-time (float-time))
      ;; bump version so modeline cache key changes
      (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
      ;; IMPORTANT: the modeline string itself is cached; invalidate it so changes
      ;; become visible immediately (e.g., after switching to LastCtx/AllCtx).
      (when (fboundp 'carriage-ui--invalidate-ml-cache)
        (carriage-ui--invalidate-ml-cache))
      (force-mode-line-update t)))

  (defun carriage-ui--ctx--schedule-refresh (&optional delay)
    "Debounced scheduling of context badge recomputation for the current buffer.

Important UX/perf note:
- Using only `run-with-idle-timer' may starve updates during continuous scrolling,
  leaving the modeline stuck at placeholder \"[Ctx:?]\".
- We still debounce/coalesce, but schedule via `run-at-time' so the refresh happens
  even when Emacs rarely becomes idle."
    (when (timerp carriage-ui--ctx-badge-refresh-timer)
      (ignore-errors (cancel-timer carriage-ui--ctx-badge-refresh-timer)))
    (let* ((buf (current-buffer))
           (d (or delay 0.15)))
      (setq carriage-ui--ctx-badge-pending t)
      (setq carriage-ui--ctx-badge-refresh-timer
            (run-at-time
             (max 0.01 (float d)) nil
             (lambda (b)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (setq carriage-ui--ctx-badge-refresh-timer nil)
                   (setq carriage-ui--ctx-badge-pending nil)
                   (let* ((sig (carriage-ui--ctx--badge-signature))
                          (badge (ignore-errors (carriage-ui--ctx--compute-badge))))
                     ;; Always apply: `carriage-ui--ctx--apply-badge' normalizes
                     ;; strings/invalid values into a (LABEL . TOOLTIP) cons cell.
                     (carriage-ui--ctx--apply-badge badge sig)))))
             buf)))
    t)

  (defun carriage-ui--ctx-invalidate (&rest _)
    "Invalidate cached context badge and schedule recomputation.

This is called from toggle/scope commands (e.g. LastCtx/AllCtx).  It must:
- make the modeline reflect the change immediately (drop caches),
- schedule a recompute soon (not idle-only),
- keep the previous value visible until the recompute finishes."
    (setq carriage-ui--ctx-badge-cache-sig nil)
    ;; Force modeline cache key change right away so redisplay doesn't keep a stale
    ;; precomputed string while we wait for the 1Hz refresh.
    (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
    (when (fboundp 'carriage-ui--invalidate-ml-cache)
      (carriage-ui--invalidate-ml-cache))
    ;; Make next refresh happen ASAP (debounced/coalesced inside scheduler).
    (setq carriage-ui--ctx-badge-last-time 0.0)
    (carriage-ui--ctx--schedule-refresh 0.05)
    (force-mode-line-update t)
    t)

  (defun carriage-ui--context-badge ()
    "Return cached (LABEL . TOOLTIP) for context badge and schedule recompute when needed.

Rules (perf/UX):
- Redisplay-safe: no scanning, no IO.
- Refresh is throttled to at most once per `carriage-ui-context-badge-refresh-interval'
  seconds (default 1.0), but still reflects point-dependent scope ('last) because
  we refresh by time even without buffer edits."
    (let* ((sig (carriage-ui--ctx--badge-signature))
           (now (float-time))
           (interval (max 0.1 (or carriage-ui-context-badge-refresh-interval 1.0)))
           (last (or carriage-ui--ctx-badge-last-time 0.0))
           (age (- now last))
           (sig-stale (not (equal sig carriage-ui--ctx-badge-cache-sig)))
           (have (or (consp carriage-ui--ctx-badge-cache)
                     (stringp carriage-ui--ctx-badge-cache))))
      ;; Schedule refresh when:
      ;; - no cache yet (first fill), OR
      ;; - signature changed (toggles/scope/profile/budgets), OR
      ;; - time-based refresh due (reflect current point for scope=last).
      (when (and (not carriage-ui--ctx-badge-pending)
                 (or (not have) sig-stale (>= age interval)))
        ;; If we already have a value and we're under interval, avoid churn.
        (when (or (not have) (>= age interval) sig-stale)
          (carriage-ui--ctx--schedule-refresh (if have 0.05 0.01))))
      ;; Always return (LABEL . TOOLTIP).
      (let ((val carriage-ui--ctx-badge-cache))
        (cond
         ((consp val) val)
         ((stringp val)
          (cons val
                (or (get-text-property 0 'help-echo val)
                    "Контекст: подробности недоступны")))
         (t (cons "[Ctx:?]" "Контекст: вычисление… (обновится автоматически)"))))))

  ;; -------------------------------------------------------------------
  ;; Performance override: modeline cache key must NEVER trigger buffer scans.
  ;;
  ;; In particular, patch counting must not call `carriage-ui--patch-count' (which may
  ;; rescan the buffer when tick changes). Modeline should only read the latest
  ;; cached patch count maintained asynchronously by `carriage-ui--patch-refresh-now'.
  ;;
  (defun carriage-ui--ml-cache-key ()
    "Compute cache key for the modeline string based on current UI environment.

Perf invariants:
- MUST NOT scan buffers or read files.
- MUST NOT call `carriage-ui--patch-count' / `carriage-ui--get-patch-ranges'."
    (let* ((uicons (carriage-ui--icons-available-p))
           (blocks (if (and (listp carriage-ui-modeline-blocks)
                            carriage-ui-modeline-blocks)
                       carriage-ui-modeline-blocks
                     carriage-ui--modeline-default-blocks))
           (state  (and (boundp 'carriage--ui-state) carriage--ui-state))
           (ctx-ver (and (memq 'context blocks) (or carriage-ui--ctx-badge-version 0)))
           ;; Patch count is display-only; never compute here. Read cache only.
           (patch-count (and (memq 'patch blocks)
                             (not (memq state '(sending streaming dispatch waiting reasoning)))
                             (numberp carriage-ui--patch-count-cache)
                             carriage-ui--patch-count-cache))
           (has-last (and (memq 'all blocks)
                          (carriage-ui--last-iteration-present-p)))
           (spin   (and carriage-ui-enable-spinner
                        (memq state '(sending streaming dispatch waiting reasoning))
                        (carriage-ui--spinner-char)))
           ;; Do not force git/vc calls in key; rely on cache timestamp only
           (branch-t (and (memq 'branch blocks) carriage-ui--branch-cache-time))
           (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
      (list uicons
            state spin
            ctx-ver
            patch-count has-last abortp blocks
            (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
            (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
            (and (boundp 'carriage-mode-model)   carriage-mode-model)
            (and (boundp 'carriage-mode-backend) carriage-mode-backend)
            (and (boundp 'carriage-mode-provider) carriage-mode-provider)
            (and (boundp 'carriage-apply-engine) carriage-apply-engine)
            (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
            branch-t
            (and (boundp 'carriage-mode-include-gptel-context)
                 carriage-mode-include-gptel-context)
            (and (boundp 'carriage-mode-include-doc-context)
                 carriage-mode-include-doc-context)
            (and (boundp 'carriage-mode-include-visible-context)
                 carriage-mode-include-visible-context)
            (and (boundp 'carriage-mode-include-patched-files)
                 carriage-mode-include-patched-files)
            (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope))))

  ;; -------------------------------------------------------------------
  ;; Compatibility: some older call sites may still call `carriage-ui--compute-context-badge'.
  ;; Keep it O(1) and redisplay-safe by delegating to the cached badge getter.
  (defun carriage-ui--compute-context-badge (_inc-doc _inc-gpt _inc-vis _inc-patched)
    "Return cached context badge (LABEL . TOOLTIP), scheduling refresh if stale.
Redisplay/modeline safe: does not scan buffers and does not read files."
    (carriage-ui--context-badge))

  ;; -------------------------------------------------------------------
  ;; FINAL OVERRIDES (performance + correctness)
  ;;
  ;; Goals (hard requirements):
  ;; - Ctx badge MUST reflect the *next request* context selection (doc/gptel/visible/patched + limits),
  ;;   but MUST recompute at most once per `carriage-ui-context-badge-refresh-interval' seconds.
  ;; - Redisplay/modeline path MUST be O(1): NO buffer scans, NO file IO, NO heavy collectors.
  ;; - Single stable contract for Ctx cache: always (LABEL . TOOLTIP) cons.
  ;; - Patch counter segment MUST be O(1) and MUST NOT trigger patch-range scans in redisplay.
  ;;
  ;; Note: This section intentionally overrides earlier experimental/legacy definitions
  ;; in this file by re-defining functions at the end.

  (defcustom carriage-ui-context-badge-refresh-interval 1.0
    "Minimum seconds between context badge recomputations for a buffer (throttle).
Smaller values make Ctx more \"live\" while moving point, but increase overhead."
    :type 'number
    :group 'carriage-ui)

  (defvar-local carriage-ui--ctx-badge-cache nil
    "Cached context badge cons cell (LABEL . TOOLTIP) for the current buffer.")

  (defvar-local carriage-ui--ctx-badge-cache-sig nil
    "Signature (plist) for `carriage-ui--ctx-badge-cache' validity.")

  (defvar-local carriage-ui--ctx-badge-refresh-timer nil
    "Debounce timer used to schedule context badge recomputation.")

  (defvar-local carriage-ui--ctx-badge-pending nil
    "Non-nil when a context badge recomputation is scheduled and not yet executed.")

  (defvar-local carriage-ui--ctx-badge-last-time 0.0
    "Timestamp (float-time) of the last context badge computation for this buffer.")

  (defvar-local carriage-ui--ctx-badge-version 0
    "Monotonic version bump for ctx badge; used to invalidate modeline cache.")

  (defun carriage-ui--ctx--normalize-badge (badge)
    "Normalize BADGE to a cons cell (LABEL . TOOLTIP). Never signal."
    (condition-case _e
        (cond
         ((consp badge)
          (cons (if (stringp (car badge)) (car badge) (format "%s" (car badge)))
                (if (stringp (cdr badge)) (cdr badge) (and (cdr badge) (format "%s" (cdr badge))))))
         ((and (listp badge) (plist-member badge :label))
          (let ((lbl (plist-get badge :label))
                (tip (or (plist-get badge :tooltip) (plist-get badge :help))))
            (cons (if (stringp lbl) lbl (format "%s" lbl))
                  (if (stringp tip) tip (and tip (format "%s" tip))))))
         ((stringp badge)
          (cons badge
                (or (get-text-property 0 'help-echo badge)
                    "Контекст: подробности недоступны")))
         (t
          (cons "[Ctx:?]" "Контекст: вычисление…")))
      (error (cons "[Ctx:!]" "Контекст: ошибка нормализации"))))

  (defun carriage-ui--ctx--toggles-snapshot ()
    "Snapshot of toggles/options that affect context selection (for cache signature)."
    (list
     :inc-doc (if (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context t)
     :inc-gpt (if (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context t)
     :inc-vis (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context)
     :inc-patched (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files)
     :scope (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
     :profile (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)
     :ctx-max-files (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files)
     :ctx-max-bytes (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes)))

  (defun carriage-ui--ctx--badge-signature ()
    "Return signature for ctx badge cache validity (does NOT include point).
Point-dependence is handled by the time throttle (<= interval)."
    (append (list :algo 3 :mode major-mode) (carriage-ui--ctx--toggles-snapshot)))

  (defun carriage-ui--ctx--compute-badge ()
    "Compute context badge (LABEL . TOOLTIP) for current buffer/point (no file reads).
Uses `carriage-context-count' which is UI-safe (budgets via file-attributes/buffer-size)."
    (require 'carriage-context nil t)
    (let* ((tog (carriage-ui--ctx--toggles-snapshot))
           (inc-doc (plist-get tog :inc-doc))
           (inc-gpt (plist-get tog :inc-gpt))
           (inc-vis (plist-get tog :inc-vis))
           (inc-pat (plist-get tog :inc-patched))
           (off (not (or inc-doc inc-gpt inc-vis inc-pat))))
      (if off
          (cons "[Ctx:-]" "Контекст выключен (doc=off, gptel=off, vis=off, patched=off)")
        (let* ((res (and (fboundp 'carriage-context-count)
                         (ignore-errors (carriage-context-count (current-buffer) (point)))))
               (cnt  (or (and (listp res) (plist-get res :count)) 0))
               (items (or (and (listp res) (plist-get res :items)) '()))
               (warns (or (and (listp res) (plist-get res :warnings)) '()))
               (stats (or (and (listp res) (plist-get res :stats)) '()))
               (limit (or (and (boundp 'carriage-ui-context-tooltip-max-items)
                               carriage-ui-context-tooltip-max-items)
                          50))
               (n (length items))
               (shown (if (and (integerp limit) (> limit 0))
                          (cl-subseq items 0 (min n limit))
                        items))
               (more (max 0 (- n (length shown))))
               (head (format "Контекст: файлов=%d — doc=%s gptel=%s vis=%s patched=%s scope=%s profile=%s — лимиты: files=%s bytes=%s — бюджет: included=%s skipped=%s bytes=%s"
                             cnt
                             (if inc-doc "on" "off")
                             (if inc-gpt "on" "off")
                             (if inc-vis "on" "off")
                             (if inc-pat "on" "off")
                             (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                               (if (eq sc 'last) "last" "all"))
                             (let ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)))
                               (if (eq pr 'p3) "P3" "P1"))
                             (or (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files) "-")
                             (or (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes) "-")
                             (or (plist-get stats :included) 0)
                             (or (plist-get stats :skipped) 0)
                             (or (plist-get stats :total-bytes) 0)))
               (warn-lines (when warns
                             (cons "Предупреждения:"
                                   (mapcar (lambda (w) (concat " - " w)) warns))))
               (item-lines (cons "Элементы:"
                                 (append (mapcar #'carriage-ui--context-item->line shown)
                                         (when (> more 0) (list (format "… (+%d more)" more))))))
               (tip (mapconcat #'identity
                               (delq nil (list head
                                               (and warn-lines (mapconcat #'identity warn-lines "\n"))
                                               (and item-lines (mapconcat #'identity item-lines "\n"))))
                               "\n")))
          (cons (format "[Ctx:%d]" cnt) tip)))))

  (defun carriage-ui--ctx--apply-badge (badge sig)
    "Install computed BADGE with signature SIG into cache and refresh modeline."
    (let ((b (carriage-ui--ctx--normalize-badge badge)))
      (setq carriage-ui--ctx-badge-cache b
            carriage-ui--ctx-badge-cache-sig sig
            carriage-ui--ctx-badge-last-time (float-time))
      ;; bump version so modeline cache key changes (and cached modeline string is rebuilt)
      (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
      (when (fboundp 'carriage-ui--invalidate-ml-cache)
        (carriage-ui--invalidate-ml-cache))
      (force-mode-line-update)))

  (defun carriage-ui--ctx--schedule-refresh (&optional delay)
    "Schedule ctx recompute via `run-at-time` (not idle-only). Coalesces requests."
    (when (timerp carriage-ui--ctx-badge-refresh-timer)
      (ignore-errors (cancel-timer carriage-ui--ctx-badge-refresh-timer)))
    (let* ((buf (current-buffer))
           (d (max 0.01 (float (or delay 0.05)))))
      (setq carriage-ui--ctx-badge-pending t)
      (setq carriage-ui--ctx-badge-refresh-timer
            (run-at-time
             d nil
             (lambda (b)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (unwind-protect
                       (let* ((sig (carriage-ui--ctx--badge-signature))
                              (badge
                               (condition-case e
                                   (carriage-ui--ctx--compute-badge)
                                 (error
                                  (cons "[Ctx:!]"
                                        (format "Ctx compute error: %s" (error-message-string e)))))))
                         (carriage-ui--ctx--apply-badge badge sig))
                     ;; always clear pending/timer
                     (setq carriage-ui--ctx-badge-refresh-timer nil
                           carriage-ui--ctx-badge-pending nil)))))
             buf)))
    t)

  (defun carriage-ui--ctx-invalidate (&rest _)
    "Invalidate ctx cache and schedule recompute ASAP (but still throttled)."
    (setq carriage-ui--ctx-badge-cache-sig nil
          carriage-ui--ctx-badge-last-time 0.0)
    (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
    (when (fboundp 'carriage-ui--invalidate-ml-cache)
      (carriage-ui--invalidate-ml-cache))
    (carriage-ui--ctx--schedule-refresh 0.02)
    (force-mode-line-update)
    t)

  (defun carriage-ui--context-badge ()
    "Return cached (LABEL . TOOLTIP) and schedule recompute at most once per interval.
Redisplay-safe: never scans buffers and never reads files."
    (let* ((sig (carriage-ui--ctx--badge-signature))
           (now (float-time))
           (interval (max 0.1 (or carriage-ui-context-badge-refresh-interval 1.0)))
           (last (or carriage-ui--ctx-badge-last-time 0.0))
           (age (- now last))
           (sig-stale (not (equal sig carriage-ui--ctx-badge-cache-sig)))
           (have (consp carriage-ui--ctx-badge-cache)))
      ;; Ensure type stability even if something old wrote a string.
      (when (and (not have) (stringp carriage-ui--ctx-badge-cache))
        (setq carriage-ui--ctx-badge-cache
              (carriage-ui--ctx--normalize-badge carriage-ui--ctx-badge-cache))
        (setq have (consp carriage-ui--ctx-badge-cache)))
      ;; Schedule recompute when needed, but never more often than interval.
      (when (and (not carriage-ui--ctx-badge-pending)
                 (or (not have) sig-stale (>= age interval)))
        (carriage-ui--ctx--schedule-refresh (if have 0.05 0.01)))
      (if (consp carriage-ui--ctx-badge-cache)
          carriage-ui--ctx-badge-cache
        (cons "[Ctx:?]" "Контекст: вычисление… (обновится автоматически)"))))

  (defun carriage-ui--ml-seg-context ()
    "Build Context badge segment (click to refresh now). Must not signal."
    (let* ((ctx (condition-case _e
                    (carriage-ui--context-badge)
                  (error (cons "Ctx:!" "Контекст: ошибка (см. *Messages*)"))))
           (ctx2 (carriage-ui--ctx--normalize-badge ctx))
           (lbl (car ctx2))
           (hint (cdr ctx2)))
      (when (stringp lbl)
        (carriage-ui--ml-button lbl #'carriage-ui--ctx-invalidate
                                (or hint "Обновить контекст (mouse-1)")))))

  ;; -------------------------------------------------------------------
  ;; Patch count: must be O(1) in redisplay/modeline. Never scan here.

  (defun carriage-ui--patch-count ()
    "Return cached number of #+begin_patch blocks (O(1), redisplay-safe)."
    (if (not (derived-mode-p 'org-mode))
        0
      (let ((v carriage-ui--patch-count-cache))
        (if (numberp v) v 0))))

  (defun carriage-ui--ml-seg-patch ()
    "Build Patch counter segment (O(1), reads only from cache)."
    (let ((n (and (numberp carriage-ui--patch-count-cache) carriage-ui--patch-count-cache)))
      (when (and (numberp n) (> n 0))
        (propertize (format "[P:%d]" n)
                    'help-echo "Количество #+begin_patch блоков в буфере"))))

  (defun carriage-ui--ml-cache-key ()
    "Compute cache key for the modeline string based on current UI environment.

Perf invariants:
- MUST NOT scan buffers or read files.
- MUST NOT call `carriage-ui--get-patch-ranges'."
    (let* ((uicons (carriage-ui--icons-available-p))
           (blocks (if (and (listp carriage-ui-modeline-blocks)
                            carriage-ui-modeline-blocks)
                       carriage-ui-modeline-blocks
                     carriage-ui--modeline-default-blocks))
           (state  (and (boundp 'carriage--ui-state) carriage--ui-state))
           (ctx-ver (and (memq 'context blocks) (or carriage-ui--ctx-badge-version 0)))
           ;; Patch count: read cache only (never compute).
           (patch-count (and (memq 'patch blocks)
                             (not (memq state '(sending streaming dispatch waiting reasoning)))
                             (numberp carriage-ui--patch-count-cache)
                             carriage-ui--patch-count-cache))
           (has-last (and (memq 'all blocks)
                          (carriage-ui--last-iteration-present-p)))
           (spin   (and carriage-ui-enable-spinner
                        (memq state '(sending streaming dispatch waiting reasoning))
                        (carriage-ui--spinner-char)))
           (branch-t (and (memq 'branch blocks) carriage-ui--branch-cache-time))
           (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
      (list uicons
            state spin
            ctx-ver
            patch-count has-last abortp blocks
            (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
            (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
            (and (boundp 'carriage-mode-model)   carriage-mode-model)
            (and (boundp 'carriage-mode-backend) carriage-mode-backend)
            (and (boundp 'carriage-mode-provider) carriage-mode-provider)
            (and (boundp 'carriage-apply-engine) carriage-apply-engine)
            (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
            branch-t
            (and (boundp 'carriage-mode-include-gptel-context)
                 carriage-mode-include-gptel-context)
            (and (boundp 'carriage-mode-include-doc-context)
                 carriage-mode-include-doc-context)
            (and (boundp 'carriage-mode-include-visible-context)
                 carriage-mode-include-visible-context)
            (and (boundp 'carriage-mode-include-patched-files)
                 carriage-mode-include-patched-files)
            (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
            ;; include refresh interval so changing it invalidates cached modeline string
            (and (boundp 'carriage-ui-context-badge-refresh-interval)
                 carriage-ui-context-badge-refresh-interval))))

  ;; -------------------------------------------------------------------
  ;; FINAL PERF-SAFE OVERRIDES (single source of truth, redisplay O(1))
  ;;
  ;; Goals:
  ;; - Ctx badge reflects what would be sent in the *next* request (per toggles/scope/limits),
  ;;   updates at most once per `carriage-ui-context-badge-refresh-interval' seconds, and
  ;;   NEVER blocks redisplay.
  ;; - Patch counter MUST NOT scan the buffer from redisplay/modeline. Scans are allowed
  ;;   only from existing after-change debounced refresh (`carriage-ui--patch-refresh-now').
  ;; - Remove "type drift": always use (LABEL . TOOLTIP) cons for cached badges.
  ;; -------------------------------------------------------------------

  (defun carriage-ui--ctx--normalize-badge (badge)
    "Normalize BADGE to a cons (LABEL . TOOLTIP). Never signal."
    (condition-case _e
        (cond
         ((consp badge)
          (cons (if (stringp (car badge)) (car badge) (format "%s" (car badge)))
                (if (stringp (cdr badge)) (cdr badge) (format "%s" (cdr badge)))))
         ((and (listp badge) (plist-member badge :label))
          (let ((lbl (plist-get badge :label))
                (tip (or (plist-get badge :tooltip) (plist-get badge :help) "")))
            (cons (if (stringp lbl) lbl (format "%s" lbl))
                  (if (stringp tip) tip (format "%s" tip)))))
         ((stringp badge)
          (cons badge (or (get-text-property 0 'help-echo badge) "")))
         (t (cons "[Ctx:?]" "Контекст: вычисление…")))
      (error (cons "[Ctx:!]" "Контекст: ошибка нормализации"))))

  (defun carriage-ui--ctx--apply-badge (badge sig)
    "Install computed BADGE with signature SIG into cache.
Avoid churn: update/bump only when value actually changes."
    (let* ((new (carriage-ui--ctx--normalize-badge badge))
           (old (and (boundp 'carriage-ui--ctx-badge-cache) carriage-ui--ctx-badge-cache))
           (changed (not (equal new old))))
      (setq carriage-ui--ctx-badge-cache new
            carriage-ui--ctx-badge-cache-sig sig
            carriage-ui--ctx-badge-last-time (float-time))
      (when changed
        (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
        (when (fboundp 'carriage-ui--invalidate-ml-cache)
          (carriage-ui--invalidate-ml-cache))
        ;; Local refresh is enough; avoid global t.
        (force-mode-line-update))
      new))

  (defun carriage-ui--ctx--schedule-refresh (&optional delay)
    "Schedule ctx recompute via `run-at-time` (not idle-only). Coalesces requests.
Never signals; on error caches [Ctx:!] with tooltip."
    (when (timerp carriage-ui--ctx-badge-refresh-timer)
      (ignore-errors (cancel-timer carriage-ui--ctx-badge-refresh-timer)))
    (let* ((buf (current-buffer))
           (d (max 0.01 (float (or delay 0.05)))))
      (setq carriage-ui--ctx-badge-pending t)
      (setq carriage-ui--ctx-badge-refresh-timer
            (run-at-time
             d nil
             (lambda (b)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (unwind-protect
                       (let* ((sig (carriage-ui--ctx--badge-signature))
                              (badge
                               (condition-case e
                                   ;; Single source of truth: "next request" context count
                                   ;; (fast, no file reads) from carriage-context.el
                                   (progn
                                     (require 'carriage-context nil t)
                                     (carriage-ui--ctx--compute-badge))
                                 (error
                                  (cons "[Ctx:!]"
                                        (format "Ctx compute error: %s" (error-message-string e)))))))
                         (carriage-ui--ctx--apply-badge badge sig))
                     (setq carriage-ui--ctx-badge-refresh-timer nil
                           carriage-ui--ctx-badge-pending nil)))))
             buf)))
    t)

  (defun carriage-ui--ctx-invalidate (&rest _)
    "Invalidate ctx cache and schedule recompute ASAP (still throttled by getter)."
    (setq carriage-ui--ctx-badge-cache-sig nil
          carriage-ui--ctx-badge-last-time 0.0)
    (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
    (when (fboundp 'carriage-ui--invalidate-ml-cache)
      (carriage-ui--invalidate-ml-cache))
    (carriage-ui--ctx--schedule-refresh 0.02)
    (force-mode-line-update)
    t)

  (defun carriage-ui--context-badge ()
    "Return cached (LABEL . TOOLTIP) and schedule recompute at most once per interval.
Redisplay-safe: never scans buffers and never reads file contents."
    (let* ((sig (carriage-ui--ctx--badge-signature))
           (now (float-time))
           (interval (max 0.1 (or carriage-ui-context-badge-refresh-interval 1.0)))
           (last (or carriage-ui--ctx-badge-last-time 0.0))
           (age (- now last))
           (sig-stale (not (equal sig carriage-ui--ctx-badge-cache-sig)))
           (have (consp carriage-ui--ctx-badge-cache)))
      ;; Normalize any legacy string cache to cons once.
      (when (and (not have) (stringp carriage-ui--ctx-badge-cache))
        (setq carriage-ui--ctx-badge-cache
              (carriage-ui--ctx--normalize-badge carriage-ui--ctx-badge-cache))
        (setq have (consp carriage-ui--ctx-badge-cache)))
      ;; Schedule recompute when needed, but never more often than interval.
      (when (and (not carriage-ui--ctx-badge-pending)
                 (or (not have) sig-stale (>= age interval)))
        (carriage-ui--ctx--schedule-refresh (if have 0.05 0.01)))
      (if (consp carriage-ui--ctx-badge-cache)
          carriage-ui--ctx-badge-cache
        (cons "[Ctx:?]" "Контекст: вычисление… (обновится автоматически)"))))

  (defun carriage-ui-refresh-context-badge ()
    "Force refresh of the [Ctx:N] badge (async, nonblocking) and update modeline."
    (interactive)
    (carriage-ui--ctx-invalidate))

  (defun carriage-ui--compute-context-badge (_inc-doc _inc-gpt _inc-vis _inc-patched)
    "Compatibility shim: always return cached (LABEL . TOOLTIP) and schedule refresh if stale.
Never compute synchronously from redisplay/modeline."
    (carriage-ui--context-badge))

  (defun carriage-ui--ml-seg-context ()
    "Build Context badge segment (click to refresh). Robust to legacy formats; never signals."
    (let* ((ctx (condition-case _e
                    (carriage-ui--context-badge)
                  (error (cons "[Ctx:!]" "Контекст: ошибка (см. *Messages*)"))))
           (ctx2 (carriage-ui--ctx--normalize-badge ctx))
           (lbl (car ctx2))
           (hint (cdr ctx2)))
      (when (stringp lbl)
        (carriage-ui--ml-button lbl #'carriage-ui--ctx-invalidate
                                (or hint "Обновить контекст (mouse-1)")))))

  ;; -------------------------------------------------------------------
  ;; Patch count: must be O(1) in redisplay/modeline. Never scan here.
  ;; If cache is missing/stale, schedule a debounced refresh and return 0.

  (defun carriage-ui--patch-count ()
    "Return strictly O(1) value of last known patch count for modeline use."
    (or (and (derived-mode-p 'org-mode)
             (numberp carriage-ui--patch-count-cache)
             carriage-ui--patch-count-cache)
        0))

  (defun carriage-ui--ml-seg-patch ()
    "Build Patch counter segment (O(1), never scans buffer on redisplay)."
    (let ((n (carriage-ui--patch-count)))
      (when (and (numberp n) (> n 0))
        (propertize (format "[P:%d]" n)
                    'help-echo "Количество #+begin_patch блоков в буфере"))))

  (defun carriage-ui--ml-cache-key ()
    "Compute modeline cache key (strictly O(1), patch-count only from cache)."
    (let* ((uicons (carriage-ui--icons-available-p))
           (blocks (if (and (listp carriage-ui-modeline-blocks)
                            carriage-ui-modeline-blocks)
                       carriage-ui-modeline-blocks
                     carriage-ui--modeline-default-blocks))
           (state  (and (boundp 'carriage--ui-state) carriage--ui-state))
           (ctx-ver (and (memq 'context blocks) (or carriage-ui--ctx-badge-version 0)))
           ;; !! Patch count: use strictly cached value, never triggers scan.
           (patch-count (and (memq 'patch blocks)
                             (not (memq state '(sending streaming dispatch waiting reasoning)))
                             (numberp carriage-ui--patch-count-cache)
                             carriage-ui--patch-count-cache))
           (has-last (and (memq 'all blocks)
                          (carriage-ui--last-iteration-present-p)))
           (spin   (and carriage-ui-enable-spinner
                        (memq state '(sending streaming dispatch waiting reasoning))
                        (carriage-ui--spinner-char)))
           (branch-t (and (memq 'branch blocks) carriage-ui--branch-cache-time))
           (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
      (list uicons
            state spin
            ctx-ver
            patch-count has-last abortp blocks
            (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
            (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
            (and (boundp 'carriage-mode-model)   carriage-mode-model)
            (and (boundp 'carriage-mode-backend) carriage-mode-backend)
            (and (boundp 'carriage-mode-provider) carriage-mode-provider)
            (and (boundp 'carriage-apply-engine) carriage-apply-engine)
            (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
            branch-t
            (and (boundp 'carriage-mode-include-gptel-context)
                 carriage-mode-include-gptel-context)
            (and (boundp 'carriage-mode-include-doc-context)
                 carriage-mode-include-doc-context)
            (and (boundp 'carriage-mode-include-visible-context)
                 carriage-mode-include-visible-context)
            (and (boundp 'carriage-mode-include-patched-files)
                 carriage-mode-include-patched-files)
            (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
            (and (boundp 'carriage-ui-context-badge-refresh-interval)
                 carriage-ui-context-badge-refresh-interval))))
  ;; -------------------------------------------------------------------
  ;; FINAL OVERRIDES (SINGLE SOURCE OF TRUTH)
  ;;
  ;; Goals:
  ;; - Ctx in modeline reflects the *next request* context (doc/gptel/visible/patched + limits),
  ;;   and updates no more often than `carriage-ui-context-badge-refresh-interval'.
  ;; - All redisplay/modeline code is O(1): no buffer scans, no IO, no expensive Org ops.
  ;; - Patch counter is O(1) in modeline (reads only the cache maintained by debounced workers).

  (defcustom carriage-ui-context-badge-refresh-interval 1.0
    "Minimum seconds between context badge recomputations for a buffer."
    :type 'number
    :group 'carriage-ui)

  (defvar-local carriage-ui--ctx-badge-cache (cons "[Ctx:?]" "Контекст: вычисление…"))
  (defvar-local carriage-ui--ctx-badge-cache-sig nil)
  (defvar-local carriage-ui--ctx-badge-last-time 0.0)
  (defvar-local carriage-ui--ctx-badge-refresh-timer nil)
  (defvar-local carriage-ui--ctx-badge-pending nil)
  (defvar-local carriage-ui--ctx-badge-version 0)

  (defun carriage-ui--ctx--badge-signature ()
    "Signature for ctx badge cache validity (does not include point).
Point-dependence is handled by the time throttle."
    (list
     :algo 4
     :mode major-mode
     :inc-doc (if (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context t)
     :inc-gpt (if (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context t)
     :inc-vis (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context)
     :inc-patched (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files)
     :scope (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
     :profile (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)
     :ctx-max-files (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files)
     :ctx-max-bytes (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes)))

  (defun carriage-ui--ctx--normalize-badge (badge)
    "Normalize BADGE into (LABEL . TOOLTIP) cons cell; never signal."
    (condition-case _e
        (cond
         ((consp badge)
          (cons (if (stringp (car badge)) (car badge) (format "%s" (car badge)))
                (if (stringp (cdr badge)) (cdr badge) (format "%s" (cdr badge)))))
         ((and (listp badge) (plist-member badge :label))
          (let ((lbl (plist-get badge :label))
                (tip (or (plist-get badge :tooltip) (plist-get badge :help) "")))
            (cons (if (stringp lbl) lbl (format "%s" lbl))
                  (if (stringp tip) tip (format "%s" tip)))))
         ((stringp badge)
          (cons badge (or (get-text-property 0 'help-echo badge) "")))
         (t
          (cons "[Ctx:?]" "Контекст: вычисление…")))
      (error (cons "[Ctx:!]" "Контекст: ошибка нормализации"))))

  (defun carriage-ui--ctx--compute-badge ()
    "Compute context badge via `carriage-context-count` (fast, no file contents).
Runs only from timer/worker (never on redisplay)."
    (let* ((inc-doc (if (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context t))
           (inc-gpt (if (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context t))
           (inc-vis (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context))
           (inc-pat (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files))
           (off (not (or inc-doc inc-gpt inc-vis inc-pat))))
      (if off
          (cons "[Ctx:-]" "Контекст выключен (doc=off, gptel=off, vis=off, patched=off)")
        (require 'carriage-context nil t)
        (let* ((res (and (fboundp 'carriage-context-count)
                         (ignore-errors (carriage-context-count (current-buffer) (point)))))
               (cnt  (or (and (listp res) (plist-get res :count)) 0))
               (items (or (and (listp res) (plist-get res :items)) '()))
               (warns (or (and (listp res) (plist-get res :warnings)) '()))
               (stats (or (and (listp res) (plist-get res :stats)) '()))
               (limit (or (and (boundp 'carriage-ui-context-tooltip-max-items)
                               carriage-ui-context-tooltip-max-items)
                          50))
               (n (length items))
               (shown (if (and (integerp limit) (> limit 0))
                          (cl-subseq items 0 (min n limit))
                        items))
               (more (max 0 (- n (length shown))))
               (head (format "Контекст: файлов=%d — doc=%s gptel=%s vis=%s patched=%s scope=%s profile=%s — лимиты: files=%s bytes=%s — бюджет: included=%s skipped=%s bytes=%s"
                             cnt
                             (if inc-doc "on" "off")
                             (if inc-gpt "on" "off")
                             (if inc-vis "on" "off")
                             (if inc-pat "on" "off")
                             (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                               (if (eq sc 'last) "last" "all"))
                             (let ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)))
                               (if (eq pr 'p3) "P3" "P1"))
                             (or (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files) "-")
                             (or (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes) "-")
                             (or (plist-get stats :included) 0)
                             (or (plist-get stats :skipped) 0)
                             (or (plist-get stats :total-bytes) 0)))
               (warn-lines (when warns
                             (cons "Предупреждения:"
                                   (mapcar (lambda (w) (concat " - " w)) warns))))
               (item-lines (cons "Элементы:"
                                 (append (mapcar #'carriage-ui--context-item->line shown)
                                         (when (> more 0) (list (format "… (+%d more)" more))))))
               (tip (mapconcat #'identity
                               (delq nil (list head
                                               (and warn-lines (mapconcat #'identity warn-lines "\n"))
                                               (and item-lines (mapconcat #'identity item-lines "\n"))))
                               "\n")))
          (cons (format "[Ctx:%d]" cnt) tip)))))

  (defun carriage-ui--ctx--apply-badge (badge sig)
    "Install computed BADGE with signature SIG into cache.
Only bumps versions and forces modeline update when the value actually changed."
    (let* ((new (carriage-ui--ctx--normalize-badge badge))
           (old carriage-ui--ctx-badge-cache)
           (changed (not (equal new old))))
      (setq carriage-ui--ctx-badge-cache new
            carriage-ui--ctx-badge-cache-sig sig
            carriage-ui--ctx-badge-last-time (float-time))
      (when changed
        (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
        (when (fboundp 'carriage-ui--invalidate-ml-cache)
          (carriage-ui--invalidate-ml-cache))
        (force-mode-line-update))
      new))

  (defun carriage-ui--ctx--schedule-refresh (&optional delay)
    "Schedule ctx recomputation with coalescing; uses run-at-time (not idle-only)."
    (when (timerp carriage-ui--ctx-badge-refresh-timer)
      (ignore-errors (cancel-timer carriage-ui--ctx-badge-refresh-timer)))
    (let* ((buf (current-buffer))
           (d (max 0.01 (float (or delay 0.05)))))
      (setq carriage-ui--ctx-badge-pending t)
      (setq carriage-ui--ctx-badge-refresh-timer
            (run-at-time
             d nil
             (lambda (b)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (unwind-protect
                       (let* ((sig (carriage-ui--ctx--badge-signature))
                              (badge (condition-case e
                                         (carriage-ui--ctx--compute-badge)
                                       (error
                                        (cons "[Ctx:!]" (format "Ctx compute error: %s"
                                                                (error-message-string e)))))))
                         (carriage-ui--ctx--apply-badge badge sig))
                     (setq carriage-ui--ctx-badge-refresh-timer nil
                           carriage-ui--ctx-badge-pending nil)))))
             buf)))
    t)

  (defun carriage-ui--ctx-invalidate (&rest _)
    "Invalidate ctx cache and schedule recompute ASAP (still throttled by the getter)."
    (setq carriage-ui--ctx-badge-cache-sig nil
          carriage-ui--ctx-badge-last-time 0.0)
    (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
    (when (fboundp 'carriage-ui--invalidate-ml-cache)
      (carriage-ui--invalidate-ml-cache))
    (carriage-ui--ctx--schedule-refresh 0.02)
    (force-mode-line-update)
    t)

  (defun carriage-ui--context-badge ()
    "Return cached (LABEL . TOOLTIP) and schedule recompute at most once per interval.
Redisplay-safe: never scans buffers and never reads file contents."
    (let* ((sig (carriage-ui--ctx--badge-signature))
           (now (float-time))
           (interval (max 0.1 (or carriage-ui-context-badge-refresh-interval 1.0)))
           (last (or carriage-ui--ctx-badge-last-time 0.0))
           (age (- now last))
           (sig-stale (not (equal sig carriage-ui--ctx-badge-cache-sig)))
           (have (consp carriage-ui--ctx-badge-cache)))
      ;; Normalize any legacy string cache once.
      (when (and (not have) (stringp carriage-ui--ctx-badge-cache))
        (setq carriage-ui--ctx-badge-cache (carriage-ui--ctx--normalize-badge carriage-ui--ctx-badge-cache))
        (setq have (consp carriage-ui--ctx-badge-cache)))
      ;; Schedule recompute when needed, but never more often than interval.
      (when (and (not carriage-ui--ctx-badge-pending)
                 (or (not have) sig-stale (>= age interval)))
        (carriage-ui--ctx--schedule-refresh (if have 0.05 0.01)))
      (if (consp carriage-ui--ctx-badge-cache)
          carriage-ui--ctx-badge-cache
        (cons "[Ctx:?]" "Контекст: вычисление… (обновится автоматически)"))))

  (defun carriage-ui-refresh-context-badge ()
    "Force refresh of the [Ctx:N] badge (async, nonblocking) and update modeline."
    (interactive)
    (carriage-ui--ctx-invalidate))

  (defun carriage-ui--compute-context-badge (_inc-doc _inc-gpt _inc-vis _inc-patched)
    "Compatibility shim: return cached (LABEL . TOOLTIP) and schedule refresh if stale."
    (carriage-ui--context-badge))

  (defun carriage-ui--ml-seg-context ()
    "Build Context badge segment (click to refresh). Never signals."
    (let* ((ctx (condition-case _e
                    (carriage-ui--context-badge)
                  (error (cons "[Ctx:!]" "Контекст: ошибка (см. *Messages*)"))))
           (ctx2 (carriage-ui--ctx--normalize-badge ctx))
           (lbl (car ctx2))
           (hint (cdr ctx2)))
      (when (stringp lbl)
        (carriage-ui--ml-button lbl #'carriage-ui--ctx-invalidate
                                (or hint "Обновить контекст (mouse-1)")))))

  ;; --- Patch-count: strict O(1) on redisplay/modeline.
  (defun carriage-ui--patch-count ()
    "Return strictly O(1) cached number of #+begin_patch blocks."
    (or (and (derived-mode-p 'org-mode)
             (numberp carriage-ui--patch-count-cache)
             carriage-ui--patch-count-cache)
        0))

  (defun carriage-ui--ml-seg-patch ()
    "Build Patch counter segment (O(1), never scans buffer on redisplay)."
    (let ((n (carriage-ui--patch-count)))
      (when (and (numberp n) (> n 0))
        (propertize (format "[P:%d]" n)
                    'help-echo "Количество #+begin_patch блоков в буфере"))))

  (defun carriage-ui--ml-cache-key ()
    "Compute modeline cache key (strictly O(1), no scans / IO)."
    (let* ((uicons (carriage-ui--icons-available-p))
           (blocks (if (and (listp carriage-ui-modeline-blocks)
                            carriage-ui-modeline-blocks)
                       carriage-ui-modeline-blocks
                     carriage-ui--modeline-default-blocks))
           (state  (and (boundp 'carriage--ui-state) carriage--ui-state))
           (ctx-ver (and (memq 'context blocks) (or carriage-ui--ctx-badge-version 0)))
           (patch-count (and (memq 'patch blocks)
                             (not (memq state '(sending streaming dispatch waiting reasoning)))
                             (numberp carriage-ui--patch-count-cache)
                             carriage-ui--patch-count-cache))
           (has-last (and (memq 'all blocks)
                          (carriage-ui--last-iteration-present-p)))
           (spin   (and carriage-ui-enable-spinner
                        (memq state '(sending streaming dispatch waiting reasoning))
                        (carriage-ui--spinner-char)))
           (branch-t (and (memq 'branch blocks) carriage-ui--branch-cache-time))
           (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
      (list uicons
            state spin
            ctx-ver
            patch-count has-last abortp blocks
            (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
            (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
            (and (boundp 'carriage-mode-model)   carriage-mode-model)
            (and (boundp 'carriage-mode-backend) carriage-mode-backend)
            (and (boundp 'carriage-mode-provider) carriage-mode-provider)
            (and (boundp 'carriage-apply-engine) carriage-apply-engine)
            (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
            branch-t
            (and (boundp 'carriage-mode-include-gptel-context)
                 carriage-mode-include-gptel-context)
            (and (boundp 'carriage-mode-include-doc-context)
                 carriage-mode-include-doc-context)
            (and (boundp 'carriage-mode-include-visible-context)
                 carriage-mode-include-visible-context)
            (and (boundp 'carriage-mode-include-patched-files)
                 carriage-mode-include-patched-files)
            (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
            (and (boundp 'carriage-ui-context-badge-refresh-interval)
                 carriage-ui-context-badge-refresh-interval))))

;;; -------------------------------------------------------------------
;;; Final non-blocking overrides (single source of truth)
;;; - Modeline/header-line must be O(1): no scans, no IO, no require.
;;; - Ctx refresh: run-at-time (not idle), throttle ≤ carriage-ui-context-badge-refresh-interval.
;;; - Patch-count in modeline: strictly from cache (no regex scans in redisplay).

  ;; -------------------------
  ;; O(1) patch-count in redisplay

  (defun carriage-ui--patch-count ()
    "Return strictly O(1) cached number of #+begin_patch blocks for modeline use."
    (or (and (derived-mode-p 'org-mode)
             (numberp carriage-ui--patch-count-cache)
             carriage-ui--patch-count-cache)
        0))

  (defun carriage-ui--ml-seg-patch ()
    "Build Patch counter segment from cached value only (no scans)."
    (let ((n (carriage-ui--patch-count)))
      (when (and (numberp n) (> n 0))
        (propertize (format "[P:%d]" n)
                    'help-echo "Количество #+begin_patch блоков (кэш, без пересканов)"))))

  (defun carriage-ui--ml-cache-key ()
    "Compute modeline cache key (strict O(1), no scans/IO in redisplay)."
    (let* ((uicons (carriage-ui--icons-available-p))
           (blocks (if (and (listp carriage-ui-modeline-blocks)
                            carriage-ui-modeline-blocks)
                       carriage-ui-modeline-blocks
                     carriage-ui--modeline-default-blocks))
           (state  (and (boundp 'carriage--ui-state) carriage--ui-state))
           ;; Ctx: use version stamp only; worker bumps version on real change.
           (ctx-ver (and (memq 'context blocks) (or carriage-ui--ctx-badge-version 0)))
           ;; Patch count: use cached number only (never compute from redisplay).
           (patch-count (and (memq 'patch blocks)
                             (not (memq state '(sending streaming dispatch waiting reasoning)))
                             (numberp carriage-ui--patch-count-cache)
                             carriage-ui--patch-count-cache))
           (has-last (and (memq 'all blocks)
                          (carriage-ui--last-iteration-present-p)))
           (spin (and carriage-ui-enable-spinner
                      (memq state '(sending streaming dispatch waiting reasoning))
                      (carriage-ui--spinner-char)))
           (branch-t (and (memq 'branch blocks) carriage-ui--branch-cache-time))
           (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
      (list uicons
            state spin
            ctx-ver
            patch-count has-last abortp blocks
            (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
            (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
            (and (boundp 'carriage-mode-model)   carriage-mode-model)
            (and (boundp 'carriage-mode-backend) carriage-mode-backend)
            (and (boundp 'carriage-mode-provider) carriage-mode-provider)
            (and (boundp 'carriage-apply-engine) carriage-apply-engine)
            (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
            branch-t
            (and (boundp 'carriage-mode-include-gptel-context)
                 carriage-mode-include-gptel-context)
            (and (boundp 'carriage-mode-include-doc-context)
                 carriage-mode-include-doc-context)
            (and (boundp 'carriage-mode-include-visible-context)
                 carriage-mode-include-visible-context)
            (and (boundp 'carriage-mode-include-patched-files)
                 carriage-mode-include-patched-files)
            (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
            (and (boundp 'carriage-ui-context-badge-refresh-interval)
                 carriage-ui-context-badge-refresh-interval))))

  ;; -------------------------
  ;; Non-blocking Ctx badge (single worker; ≤ 1Hz; run-at-time, not idle)

  ;; Guard local vars (redeclare safely).
  (defvar-local carriage-ui--ctx-badge-cache (cons "[Ctx:?]" "Контекст: вычисляется…"))
  (defvar-local carriage-ui--ctx-badge-cache-sig nil)
  (defvar-local carriage-ui--ctx-badge-last-time 0.0)
  (defvar-local carriage-ui--ctx-badge-refresh-timer nil)
  (defvar-local carriage-ui--ctx-badge-pending nil)
  (defvar-local carriage-ui--ctx-badge-version 0)

  ;; Helper: normalize any legacy/string/plist forms into (LABEL . TOOLTIP)
  (defun carriage-ui--ctx--normalize-badge (badge)
    (condition-case _
        (cond
         ((consp badge)
          (cons (if (stringp (car badge)) (car badge) (format "%s" (car badge)))
                (if (stringp (cdr badge)) (cdr badge) (format "%s" (cdr badge)))))
         ((and (listp badge) (plist-member badge :label))
          (let ((lbl (plist-get badge :label))
                (tip (or (plist-get badge :tooltip) (plist-get badge :help) "")))
            (cons (if (stringp lbl) lbl (format "%s" lbl))
                  (if (stringp tip) tip (format "%s" tip)))))
         ((stringp badge)
          (cons badge (or (get-text-property 0 'help-echo badge) "")))
         (t
          (cons "[Ctx:?]" "Контекст: вычисляется…")))
      (error (cons "[Ctx:!]" "Контекст: ошибка нормализации"))))

  (defun carriage-ui--ctx--apply-badge (badge sig)
    "Install computed BADGE (cons) with SIG into cache; bump version only on real change."
    (let* ((new (carriage-ui--ctx--normalize-badge badge))
           (old carriage-ui--ctx-badge-cache)
           (changed (not (equal new old))))
      (setq carriage-ui--ctx-badge-cache new
            carriage-ui--ctx-badge-cache-sig sig
            carriage-ui--ctx-badge-last-time (float-time))
      (when changed
        (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
        (when (fboundp 'carriage-ui--invalidate-ml-cache)
          (carriage-ui--invalidate-ml-cache))
        (force-mode-line-update))
      new))

  (defun carriage-ui--ctx--badge-signature ()
    "Return signature for ctx badge cache validity (no point/tick; only toggles/scope/limits)."
    (list
     :algo 'final
     :mode major-mode
     :inc-doc (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context)
     :inc-gpt (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context)
     :inc-vis (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context)
     :inc-patched (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files)
     :scope (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
     :profile (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)
     :ctx-max-files (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files)
     :ctx-max-bytes (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes)))

  (defun carriage-ui--ctx--compute-badge ()
    "Compute (LABEL . TOOLTIP) via carriage-context-count (fast; no file contents).
Runs only in worker timer (never from redisplay)."
    (require 'carriage-context nil t)
    (let* ((res (and (fboundp 'carriage-context-count)
                     (ignore-errors (carriage-context-count (current-buffer) (point)))))
           (cnt (or (and (listp res) (plist-get res :count)) 0))
           (items (or (and (listp res) (plist-get res :items)) '()))
           (warns (or (and (listp res) (plist-get res :warnings)) '()))
           (limit (or (and (boundp 'carriage-ui-context-tooltip-max-items)
                           carriage-ui-context-tooltip-max-items)
                      50))
           (n (length items))
           (shown (if (and (integerp limit) (> limit 0))
                      (cl-subseq items 0 (min n limit))
                    items))
           (more (max 0 (- n (length shown))))
           (head (format "Контекст: файлов=%d — doc=%s gptel=%s vis=%s patched=%s scope=%s profile=%s"
                         cnt
                         (if (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context) "on" "off")
                         (if (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context) "on" "off")
                         (if (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context) "on" "off")
                         (if (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files) "on" "off")
                         (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                           (if (eq sc 'last) "last" "all"))
                         (let ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)))
                           (if (eq pr 'p3) "P3" "P1"))))
           (warn-lines (when (and warns (> (length warns) 0))
                         (cons "Предупреждения:"
                               (mapcar (lambda (w) (concat " - " w)) warns))))
           (item-lines
            (cons "Элементы:"
                  (append (mapcar (lambda (it)
                                    (let ((p (plist-get it :path))
                                          (src (plist-get it :source)))
                                      (format " - [%s] %s" (or src "?") (or p "-"))))
                                  shown)
                          (when (> more 0) (list (format "… (+%d more)" more)))))))
      (cons (format "[Ctx:%d]" cnt)
            (mapconcat #'identity
                       (delq nil (list head
                                       (and warn-lines (mapconcat #'identity warn-lines "\n"))
                                       (and item-lines (mapconcat #'identity item-lines "\n"))))
                       "\n"))))

  (defun carriage-ui--ctx--schedule-refresh (&optional delay)
    "Schedule ctx recomputation (run-at-time, not idle). Coalesces requests."
    (when (timerp carriage-ui--ctx-badge-refresh-timer)
      (ignore-errors (cancel-timer carriage-ui--ctx-badge-refresh-timer)))
    (let* ((buf (current-buffer))
           (d (max 0.01 (float (or delay 0.05)))))
      (setq carriage-ui--ctx-badge-pending t)
      (setq carriage-ui--ctx-badge-refresh-timer
            (run-at-time
             d nil
             (lambda (b)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (unwind-protect
                       (let* ((sig (carriage-ui--ctx--badge-signature))
                              (badge (condition-case e
                                         (carriage-ui--ctx--compute-badge)
                                       (error (cons "[Ctx:!]" (format "Ctx compute error: %s" (error-message-string e)))))))
                         (carriage-ui--ctx--apply-badge badge sig))
                     (setq carriage-ui--ctx-badge-refresh-timer nil
                           carriage-ui--ctx-badge-pending nil)))))))))

  (defun carriage-ui--ctx-invalidate (&rest _)
    "Invalidate ctx cache and schedule recompute ASAP (still throttled by getter)."
    (setq carriage-ui--ctx-badge-cache-sig nil
          carriage-ui--ctx-badge-last-time 0.0)
    (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
    (when (fboundp 'carriage-ui--invalidate-ml-cache)
      (carriage-ui--invalidate-ml-cache))
    (carriage-ui--ctx--schedule-refresh 0.02)
    (force-mode-line-update)
    t)

  (defun carriage-ui--context-badge ()
    "Return cached (LABEL . TOOLTIP) and schedule recompute ≤ 1 Hz when stale.
Redisplay-safe: never scans buffers and never reads file contents."
    (let* ((sig (carriage-ui--ctx--badge-signature))
           (now (float-time))
           (interval (max 0.1 (or carriage-ui-context-badge-refresh-interval 1.0)))
           (last (or carriage-ui--ctx-badge-last-time 0.0))
           (age (- now last))
           (sig-stale (not (equal sig carriage-ui--ctx-badge-cache-sig)))
           (have (consp carriage-ui--ctx-badge-cache)))
      ;; Normalize any legacy string cache once.
      (when (and (not have) (stringp carriage-ui--ctx-badge-cache))
        (setq carriage-ui--ctx-badge-cache
              (carriage-ui--ctx--normalize-badge carriage-ui--ctx-badge-cache))
        (setq have (consp carriage-ui--ctx-badge-cache)))
      ;; Schedule recompute when needed, but never more often than interval.
      (when (and (not carriage-ui--ctx-badge-pending)
                 (or (not have) sig-stale (>= age interval)))
        (carriage-ui--ctx--schedule-refresh (if have 0.05 0.01)))
      (if (consp carriage-ui--ctx-badge-cache)
          carriage-ui--ctx-badge-cache
        (cons "[Ctx:?]" "Контекст: вычисляется… (обновится автоматически)"))))

  ;; Robust modeline segment: always operate on cons, never signals.
  (defun carriage-ui--ml-seg-context ()
    "Build Context badge segment (click to refresh) from cached value only."
    (let* ((ctx (condition-case _
                    (carriage-ui--context-badge)
                  (error (cons "[Ctx:!]" "Контекст: ошибка (см. *Messages*)"))))
           (ctx2 (carriage-ui--ctx--normalize-badge ctx))
           (lbl (car ctx2))
           (hint (cdr ctx2)))
      (when (stringp lbl)
        (carriage-ui--ml-button lbl #'carriage-ui--ctx-invalidate
                                (or hint "Обновить контекст (mouse-1)")))))

  ;; -------------------------------------------------------------------
  ;; FINAL NON-BLOCKING OVERRIDES (single source of truth, O(1) in redisplay)
  ;;
  ;; - Modeline reads cached values only (no scans/IO/require).
  ;; - Context badge is computed by a single throttled worker (≤ carriage-ui-context-badge-refresh-interval).
  ;; - Patch-count in modeline reads strictly from cache (no regex scans in redisplay).
  ;;
  ;; This block deliberately redefines a few functions at the very end of the file to
  ;; ensure no earlier experimental/legacy definitions are active.

  (defcustom carriage-ui-context-badge-refresh-interval 1.0
    "Minimum seconds between context badge recomputations for the current buffer (throttle)."
    :type 'number
    :group 'carriage-ui)

  (defvar-local carriage-ui--ctx-badge-cache nil)          ;; (LABEL . TOOLTIP)
  (defvar-local carriage-ui--ctx-badge-last-time 0.0)      ;; float-time of last successful compute
  (defvar-local carriage-ui--ctx-badge-refresh-timer nil)  ;; pending timer object
  (defvar-local carriage-ui--ctx-badge-pending nil)        ;; flag (non-nil when scheduled/running)
  (defvar-local carriage-ui--ctx-badge-version 0)          ;; bump on value change (for ml cache key)

  (defun carriage-ui--ctx--normalize-badge (badge)
    "Normalize BADGE to a cons (LABEL . TOOLTIP), robust to legacy forms."
    (cond
     ((and (consp badge) (stringp (car badge)))
      (cons (car badge) (and (stringp (cdr badge)) (cdr badge))))
     ((stringp badge)
      (cons badge (or (get-text-property 0 'help-echo badge) "")))
     ((and (listp badge) (plist-member badge :label))
      (let ((lbl (plist-get badge :label))
            (tip (or (plist-get badge :tooltip) (plist-get badge :help) "")))
        (cons (if (stringp lbl) lbl (format "%s" lbl))
              (if (stringp tip) tip (format "%s" tip)))))
     (t (cons "[Ctx:?]" "Контекст: вычисляется…"))))

  (defun carriage-ui--ctx--badge-signature ()
    "Compute a signature of context-affecting toggles/options (no point/tick).
Used to decide if recomputation is needed."
    (list
     :mode major-mode
     :doc (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context)
     :gpt (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context)
     :vis (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context)
     :patched (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files)
     :scope (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
     :profile (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)
     :max-files (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files)
     :max-bytes (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes)))

  (defun carriage-ui--ctx--compute-badge ()
    "Compute context badge as (LABEL . TOOLTIP) using carriage-context-count (no file IO).
Returns a normalized cons cell; never signals (errors are reported as Ctx:!)."
    (condition-case e
        (progn
          (require 'carriage-context nil t)
          (let* ((res (and (fboundp 'carriage-context-count)
                           (carriage-context-count (current-buffer) (point))))
                 (cnt (or (and (listp res) (plist-get res :count)) 0))
                 (items (or (and (listp res) (plist-get res :items)) '()))
                 (warns (or (and (listp res) (plist-get res :warnings)) '()))
                 (limit (or (and (boundp 'carriage-ui-context-tooltip-max-items)
                                 carriage-ui-context-tooltip-max-items)
                            50))
                 (shown (if (and (integerp limit) (> limit 0))
                            (cl-subseq items 0 (min (length items) limit))
                          items))
                 (more (max 0 (- (length items) (length shown))))
                 (head (format "Контекст: файлов=%d — doc=%s gptel=%s vis=%s patched=%s scope=%s profile=%s"
                               cnt
                               (if (and (boundp 'carriage-mode-include-doc-context)
                                        carriage-mode-include-doc-context) "on" "off")
                               (if (and (boundp 'carriage-mode-include-gptel-context)
                                        carriage-mode-include-gptel-context) "on" "off")
                               (if (and (boundp 'carriage-mode-include-visible-context)
                                        carriage-mode-include-visible-context) "on" "off")
                               (if (and (boundp 'carriage-mode-include-patched-files)
                                        carriage-mode-include-patched-files) "on" "off")
                               (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                                 (if (eq sc 'last) "last" "all"))
                               (let ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)))
                                 (if (eq pr 'p3) "P3" "P1"))))
                 (warn-lines (when (and warns (> (length warns) 0))
                               (cons "Предупреждения:"
                                     (mapcar (lambda (w) (concat " - " w)) warns))))
                 (item-lines (cons "Элементы:"
                                   (append (mapcar (lambda (it)
                                                     (let ((p (plist-get it :path))
                                                           (src (plist-get it :source)))
                                                       (format " - [%s] %s" (or src "?") (or p "-"))))
                                                   shown)
                                           (when (> more 0)
                                             (list (format "… (+%d more)" more))))))
                 (tip (mapconcat #'identity
                                 (delq nil (list head
                                                 (and warn-lines (mapconcat #'identity warn-lines "\n"))
                                                 (and item-lines (mapconcat #'identity item-lines "\n"))))
                                 "\n")))
            (cons (format "[Ctx:%d]" cnt) tip)))
      (error (cons "[Ctx:!]" (format "Ctx compute error: %s" (error-message-string e))))))

  (defun carriage-ui--ctx--apply-badge (badge sig)
    "Install BADGE (cons) with SIG into cache; bump modeline only on real change."
    (let* ((new (carriage-ui--ctx--normalize-badge badge))
           (old carriage-ui--ctx-badge-cache)
           (changed (not (equal new old))))
      (setq carriage-ui--ctx-badge-cache new
            carriage-ui--ctx-badge-last-time (float-time))
      (when changed
        (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
        (when (fboundp 'carriage-ui--invalidate-ml-cache)
          (carriage-ui--invalidate-ml-cache))
        (force-mode-line-update))
      new))

  (defun carriage-ui--ctx--schedule-refresh (&optional delay)
    "Schedule context recompute via run-at-time (not idle-only). Coalesces requests."
    (unless carriage-ui--ctx-badge-pending
      (setq carriage-ui--ctx-badge-pending t)
      (let* ((buf (current-buffer))
             (d (max 0.01 (float (or delay 0.05)))))
        (when (timerp carriage-ui--ctx-badge-refresh-timer)
          (ignore-errors (cancel-timer carriage-ui--ctx-badge-refresh-timer)))
        (setq carriage-ui--ctx-badge-refresh-timer
              (run-at-time
               d nil
               (lambda (b)
                 (when (buffer-live-p b)
                   (with-current-buffer b
                     (unwind-protect
                         (let* ((sig (carriage-ui--ctx--badge-signature))
                                (badge (carriage-ui--ctx--compute-badge)))
                           (carriage-ui--ctx--apply-badge badge sig))
                       (setq carriage-ui--ctx-badge-pending nil
                             carriage-ui--ctx-badge-refresh-timer nil)))))
               buf)))))

  (defun carriage-ui--ctx-invalidate (&rest _)
    "Invalidate context badge cache and schedule recomputation ASAP."
    (setq carriage-ui--ctx-badge-last-time 0.0)
    (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
    (when (fboundp 'carriage-ui--invalidate-ml-cache)
      (carriage-ui--invalidate-ml-cache))
    (carriage-ui--ctx--schedule-refresh 0.02)
    (force-mode-line-update)
    t)

  (defun carriage-ui--context-badge ()
    "Return cached (LABEL . TOOLTIP) and schedule recompute at most once per interval.

Important: if there is no cached value yet, schedule recompute regardless of AGE,
otherwise the modeline may get stuck at [Ctx:?] when `carriage-ui--ctx-badge-last-time'
was updated without actually computing the badge."
    (let* ((now (float-time))
           (interval (max 0.1 (or carriage-ui-context-badge-refresh-interval 1.0)))
           (age (- now (or carriage-ui--ctx-badge-last-time 0.0)))
           (cache carriage-ui--ctx-badge-cache)
           (have (consp cache))
           (lbl (and have (car cache)))
           ;; Watchdog: если pending=true, но таймер "пропал" ИЛИ мы слишком долго
           ;; остаёмся в placeholder, то сбрасываем pending и пересоздаём таймер.
           ;; Это лечит ситуацию "Ctx:? вычисляется" навсегда.
           (stuck (and carriage-ui--ctx-badge-pending
                       (or (not (timerp carriage-ui--ctx-badge-refresh-timer))
                           (>= age (max 0.3 (* 2 interval))))
                       (or (not have)
                           (and (stringp lbl) (string= lbl "[Ctx:?]"))))))
      (when stuck
        (when (timerp carriage-ui--ctx-badge-refresh-timer)
          (ignore-errors (cancel-timer carriage-ui--ctx-badge-refresh-timer)))
        (setq carriage-ui--ctx-badge-refresh-timer nil
              carriage-ui--ctx-badge-pending nil))

      ;; Normalize legacy string cache once
      (when (stringp carriage-ui--ctx-badge-cache)
        (setq carriage-ui--ctx-badge-cache
              (carriage-ui--ctx--normalize-badge carriage-ui--ctx-badge-cache)))

      (let ((have2 (consp carriage-ui--ctx-badge-cache)))
        ;; Schedule recompute when:
        ;; - no cached badge yet (first fill), OR
        ;; - the throttle interval elapsed (keep scope=last reasonably fresh).
        (when (and (not carriage-ui--ctx-badge-pending)
                   (or (not have2) (>= age interval)))
          (carriage-ui--ctx--schedule-refresh 0.01))

        (if have2
            carriage-ui--ctx-badge-cache
          (cons "[Ctx:?]" "Контекст: вычисляется… (обновится автоматически)")))))

  (defun carriage-ui--ml-seg-context ()
    "Build Context badge segment (click to refresh).
Uses cached value only; never computes in redisplay."
    (let* ((ctx (condition-case _ (carriage-ui--context-badge)
                  (error (cons "[Ctx:!]" "Контекст: ошибка (см. *Messages*)"))))
           (ctx2 (carriage-ui--ctx--normalize-badge ctx))
           (lbl (car ctx2))
           (tip (cdr ctx2)))
      (when (stringp lbl)
        (carriage-ui--ml-button lbl #'carriage-ui--ctx-invalidate
                                (or tip "Обновить контекст (mouse-1)")))))

  ;; -------------------------
  ;; Patch-count: strict O(1) in redisplay/modeline (cache only)

  (defvar-local carriage-ui--patch-count-cache 0
    "Cached number of #+begin_patch blocks (maintained by background refreshers).")

  (defun carriage-ui--patch-count ()
    "Return strictly O(1) cached number of #+begin_patch blocks."
    (if (numberp carriage-ui--patch-count-cache)
        carriage-ui--patch-count-cache
      0))

  (defun carriage-ui--ml-seg-patch ()
    "Build Patch counter segment from cached value only."
    (let ((n (carriage-ui--patch-count)))
      (when (and (numberp n) (> n 0))
        (propertize (format "[P:%d]" n)
                    'help-echo "Количество #+begin_patch блоков (кэш, без пересканов)"))))

  (defun carriage-ui--ml-cache-key ()
    "Compute modeline cache key (strict O(1), no scans/IO in redisplay)."
    (let* ((uicons (carriage-ui--icons-available-p))
           (blocks (if (and (listp carriage-ui-modeline-blocks)
                            carriage-ui-modeline-blocks)
                       carriage-ui-modeline-blocks
                     carriage-ui--modeline-default-blocks))
           (state  (and (boundp 'carriage--ui-state) carriage--ui-state))
           ;; Ctx: use version stamp (worker bumps on real change)
           (ctx-ver (and (memq 'context blocks) (or carriage-ui--ctx-badge-version 0)))
           ;; Patch count: read cached number only (never compute from redisplay).
           (patch-count (and (memq 'patch blocks)
                             (not (memq state '(sending streaming dispatch waiting reasoning)))
                             (numberp carriage-ui--patch-count-cache)
                             carriage-ui--patch-count-cache))
           (has-last (and (memq 'all blocks)
                          (and (boundp 'carriage--last-iteration-has-patches)
                               carriage--last-iteration-has-patches)))
           (spin (and carriage-ui-enable-spinner
                      (memq state '(sending streaming dispatch waiting reasoning))
                      (carriage-ui--spinner-char)))
           (branch-t (and (memq 'branch blocks) (boundp 'carriage-ui--branch-cache-time) carriage-ui--branch-cache-time))
           (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
      (list uicons
            state spin
            ctx-ver
            patch-count has-last abortp blocks
            (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
            (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
            (and (boundp 'carriage-mode-model)   carriage-mode-model)
            (and (boundp 'carriage-mode-backend) carriage-mode-backend)
            (and (boundp 'carriage-mode-provider) carriage-mode-provider)
            (and (boundp 'carriage-apply-engine) carriage-apply-engine)
            (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
            branch-t
            (and (boundp 'carriage-mode-include-gptel-context)
                 carriage-mode-include-gptel-context)
            (and (boundp 'carriage-mode-include-doc-context)
                 carriage-mode-include-doc-context)
            (and (boundp 'carriage-mode-include-visible-context)
                 carriage-mode-include-visible-context)
            (and (boundp 'carriage-mode-include-patched-files)
                 carriage-mode-include-patched-files)
            (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
            (and (boundp 'carriage-ui-context-badge-refresh-interval)
                 carriage-ui-context-badge-refresh-interval))))

  ;; Final non-blocking overrides (single source of truth, O(1) in redisplay)
  ;; - Patch-count in modeline: strictly from cache (no regex scans, no buffer walks).
  ;; - Cache-key for modeline must NOT call any function that can scan buffers or load features.
  ;; - Keep context badge throttling out of redisplay; read only its version stamp here.

  (defun carriage-ui--patch-count ()
    "Return strictly O(1) cached number of #+begin_patch blocks for modeline use."
    (or (and (numberp carriage-ui--patch-count-cache)
             carriage-ui--patch-count-cache)
        0))

  (defun carriage-ui--ml-seg-patch ()
    "Build Patch counter segment (O(1), never scans buffer on redisplay)."
    (let ((n (carriage-ui--patch-count)))
      (when (and (numberp n) (> n 0))
        (propertize (format "[P:%d]" n)
                    'help-echo "Количество #+begin_patch блоков (кэш, без пересканов)"))))

  (defun carriage-ui--ml-cache-key ()
    "Compute modeline cache key (strict O(1), no scans/IO in redisplay).
Includes only cheap, cached values. Never calls functions that may scan buffers."
    (let* ((uicons (and (fboundp 'carriage-ui--icons-available-p)
                        (carriage-ui--icons-available-p)))
           (blocks (if (and (boundp 'carriage-ui-modeline-blocks)
                            (listp carriage-ui-modeline-blocks)
                            carriage-ui-modeline-blocks)
                       carriage-ui-modeline-blocks
                     (and (boundp 'carriage-ui--modeline-default-blocks)
                          carriage-ui--modeline-default-blocks)))
           (state  (and (boundp 'carriage--ui-state) carriage--ui-state))
           ;; Version stamp of context badge; worker bumps it when value changes.
           (ctx-ver (and (memq 'context blocks)
                         (boundp 'carriage-ui--ctx-badge-version)
                         carriage-ui--ctx-badge-version))
           ;; Patch-count strictly from cache (never scan here).
           (patch-count (and (memq 'patch blocks)
                             (not (memq state '(sending streaming dispatch waiting reasoning)))
                             (numberp (and (boundp 'carriage-ui--patch-count-cache)
                                           carriage-ui--patch-count-cache))
                             carriage-ui--patch-count-cache))
           ;; Spinner char is already cheap (pure UI); keep it as is.
           (spin   (and (boundp 'carriage-ui-enable-spinner) carriage-ui-enable-spinner
                        (memq state '(sending streaming dispatch waiting reasoning))
                        (fboundp 'carriage-ui--spinner-char)
                        (carriage-ui--spinner-char)))
           ;; Abort handler presence affects modeline (O(1)).
           (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
      (list uicons
            state spin
            ctx-ver
            patch-count abortp blocks
            ;; Cheap identity fields (do not force loads/scans)
            (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
            (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
            (and (boundp 'carriage-mode-model)   carriage-mode-model)
            (and (boundp 'carriage-mode-backend) carriage-mode-backend)
            (and (boundp 'carriage-mode-provider) carriage-mode-provider)
            (and (boundp 'carriage-apply-engine) carriage-apply-engine)
            (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
            (and (boundp 'carriage-mode-include-gptel-context)
                 carriage-mode-include-gptel-context)
            (and (boundp 'carriage-mode-include-doc-context)
                 carriage-mode-include-doc-context)
            (and (boundp 'carriage-mode-include-visible-context)
                 carriage-mode-include-visible-context)
            (and (boundp 'carriage-mode-include-patched-files)
                 carriage-mode-include-patched-files)
            (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
            (and (boundp 'carriage-ui-context-badge-refresh-interval)
                 carriage-ui-context-badge-refresh-interval))))

  (defun carriage-ui--compute-context-badge (inc-doc inc-gpt inc-vis inc-patched)
    "Compute context badge (LABEL . TOOLTIP) synchronously (test/tooling helper).

Important:
- This function is used by ERT tests and other non-redisplay call sites.
- It MUST NOT read file contents; it relies on `carriage-context-count' (fast path).
- Modeline rendering MUST NOT call this directly; modeline uses cached badge logic."
    (let* ((off (not (or inc-doc inc-gpt inc-vis inc-patched))))
      (if off
          (cons "[Ctx:-]" "Контекст выключен (doc=off, gptel=off, vis=off, patched=off)")
        (require 'carriage-context nil t)
        (let* ((res (and (fboundp 'carriage-context-count)
                         (ignore-errors (carriage-context-count (current-buffer) (point)))))
               (cnt (or (and (listp res) (plist-get res :count)) 0))
               (profile (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile))
               (prof (if (eq profile 'p3) "P3" "P1"))
               (mf (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files))
               (mb (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes))
               (scope (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                        (if (eq sc 'last) "last" "all")))
               (warn (if (eq profile 'p3)
                         " (внимание: расширенный бюджет — выше стоимость/шум)"
                       "")))
          (cons (format "[Ctx:%d]" cnt)
                (mapconcat
                 #'identity
                 (delq nil
                       (list
                        (format "Контекст: файлов=%d — источники: doc=%s, gptel=%s, vis=%s, patched=%s, scope=%s"
                                cnt
                                (if inc-doc "on" "off")
                                (if inc-gpt "on" "off")
                                (if inc-vis "on" "off")
                                (if inc-patched "on" "off")
                                scope)
                        (format "Профиль: %s%s" prof warn)
                        (format "Лимиты: файлы=%s байты=%s"
                                (or mf "-") (or mb "-"))))
                 "\n"))))))

  )

(when nil
  ;; -------------------------------------------------------------------
  ;; Final modeline performance overrides (single source of truth)
  ;;
  ;; Requirements:
  ;; - Redisplay/modeline path MUST be O(1): no buffer scans, no file reads.
  ;; - Context badge computation (carriage-context-count) runs only in a timer.
  ;; - Patch counter in modeline is read strictly from `carriage-ui--patch-count-cache'.

  (defvar-local carriage-ui--ctx-badge-cache (cons "[Ctx:?]" "Контекст: вычисление…"))
  (defvar-local carriage-ui--ctx-badge-cache-sig nil)
  (defvar-local carriage-ui--ctx-badge-last-time 0.0)
  (defvar-local carriage-ui--ctx-badge-refresh-timer nil)
  (defvar-local carriage-ui--ctx-badge-pending nil)
  (defvar-local carriage-ui--ctx-badge-version 0)

  (defun carriage-ui--ctx-badge--signature ()
    "Return a cheap signature of context-affecting toggles/options (no point/tick)."
    (list
     :mode major-mode
     :doc (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context)
     :gpt (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context)
     :vis (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context)
     :patched (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files)
     :scope (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
     :profile (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)
     :max-files (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files)
     :max-bytes (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes)))

  (defun carriage-ui--ctx-badge--normalize (badge)
    "Normalize BADGE to (LABEL . TOOLTIP) cons cell. Never signal."
    (condition-case _e
        (cond
         ((and (consp badge) (stringp (car badge)))
          (cons (car badge) (if (stringp (cdr badge)) (cdr badge) "")))
         ((stringp badge)
          (cons badge (or (get-text-property 0 'help-echo badge) "")))
         ((and (listp badge) (plist-member badge :label))
          (let ((lbl (plist-get badge :label))
                (tip (or (plist-get badge :tooltip) (plist-get badge :help) "")))
            (cons (if (stringp lbl) lbl (format "%s" lbl))
                  (if (stringp tip) tip (format "%s" tip)))))
         (t
          (cons "[Ctx:?]" "Контекст: вычисление…")))
      (error (cons "[Ctx:!]" "Контекст: ошибка нормализации"))))

  (defun carriage-ui--ctx-badge--compute ()
    "Compute context badge using `carriage-context-count' (fast; no file contents).
This MUST NOT run from redisplay/modeline."
    (let* ((inc-doc (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context))
           (inc-gpt (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context))
           (inc-vis (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context))
           (inc-pat (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files))
           (off (not (or inc-doc inc-gpt inc-vis inc-pat))))
      (if off
          (cons "[Ctx:-]" "Контекст выключен (doc=off, gptel=off, vis=off, patched=off)")
        (require 'carriage-context nil t)
        (let* ((res (and (fboundp 'carriage-context-count)
                         (ignore-errors (carriage-context-count (current-buffer) (point)))))
               (cnt (or (and (listp res) (plist-get res :count)) 0))
               (items (or (and (listp res) (plist-get res :items)) '()))
               (warns (or (and (listp res) (plist-get res :warnings)) '()))
               (limit (or (and (boundp 'carriage-ui-context-tooltip-max-items)
                               carriage-ui-context-tooltip-max-items)
                          50))
               (shown (if (and (integerp limit) (> limit 0))
                          (cl-subseq items 0 (min (length items) limit))
                        items))
               (more (max 0 (- (length items) (length shown))))
               (head (format "Контекст: файлов=%d — doc=%s gptel=%s vis=%s patched=%s scope=%s profile=%s"
                             cnt
                             (if inc-doc "on" "off")
                             (if inc-gpt "on" "off")
                             (if inc-vis "on" "off")
                             (if inc-pat "on" "off")
                             (let ((sc (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)))
                               (if (eq sc 'last) "last" "all"))
                             (let ((pr (and (boundp 'carriage-doc-context-profile) carriage-doc-context-profile)))
                               (if (eq pr 'p3) "P3" "P1"))))
               (warn-lines (when warns
                             (cons "Предупреждения:"
                                   (mapcar (lambda (w) (concat " - " w)) warns))))
               (item-lines
                (cons "Элементы:"
                      (append
                       (mapcar #'carriage-ui--context-item->line shown)
                       (when (> more 0) (list (format "… (+%d more)" more))))))
               (tip (mapconcat #'identity
                               (delq nil
                                     (list head
                                           (and warn-lines (mapconcat #'identity warn-lines "\n"))
                                           (and item-lines (mapconcat #'identity item-lines "\n"))))
                               "\n")))
          (cons (format "[Ctx:%d]" cnt) tip)))))

  (defun carriage-ui--ctx-badge--apply (badge sig)
    "Install computed BADGE with SIG into cache; bump version only on real change."
    (let* ((new (carriage-ui--ctx-badge--normalize badge))
           (old carriage-ui--ctx-badge-cache)
           (changed (not (equal new old))))
      (setq carriage-ui--ctx-badge-cache new
            carriage-ui--ctx-badge-cache-sig sig
            carriage-ui--ctx-badge-last-time (float-time))
      (when changed
        (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
        (when (fboundp 'carriage-ui--invalidate-ml-cache)
          (carriage-ui--invalidate-ml-cache))
        (force-mode-line-update))
      new))

  (defun carriage-ui--ctx-badge--schedule-refresh (&optional delay)
    "Schedule debounced context badge recomputation (run-at-time, not idle-only)."
    (when (timerp carriage-ui--ctx-badge-refresh-timer)
      (ignore-errors (cancel-timer carriage-ui--ctx-badge-refresh-timer)))
    (let* ((buf (current-buffer))
           (d (max 0.01 (float (or delay 0.05)))))
      (setq carriage-ui--ctx-badge-pending t)
      (setq carriage-ui--ctx-badge-refresh-timer
            (run-at-time
             d nil
             (lambda (b)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (unwind-protect
                       (let* ((sig (carriage-ui--ctx-badge--signature))
                              (badge (condition-case e
                                         (carriage-ui--ctx-badge--compute)
                                       (error
                                        (cons "[Ctx:!]"
                                              (format "Ctx compute error: %s"
                                                      (error-message-string e)))))))
                         (carriage-ui--ctx-badge--apply badge sig))
                     (setq carriage-ui--ctx-badge-refresh-timer nil
                           carriage-ui--ctx-badge-pending nil)))))
             buf)))
    t)

  (defun carriage-ui--ctx-invalidate (&rest _)
    "Invalidate context badge cache and schedule recomputation ASAP."
    (setq carriage-ui--ctx-badge-cache-sig nil
          carriage-ui--ctx-badge-last-time 0.0)
    (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
    (when (fboundp 'carriage-ui--invalidate-ml-cache)
      (carriage-ui--invalidate-ml-cache))
    (carriage-ui--ctx-badge--schedule-refresh 0.02)
    (force-mode-line-update)
    t)

  (defun carriage-ui--context-badge ()
    "Return cached (LABEL . TOOLTIP) and schedule recompute at most once per interval.
Redisplay-safe: never scans buffers and never reads file contents."
    (let* ((sig (carriage-ui--ctx-badge--signature))
           (now (float-time))
           (interval (max 0.1 (or (and (boundp 'carriage-ui-context-badge-refresh-interval)
                                       carriage-ui-context-badge-refresh-interval)
                                  1.0)))
           (last (or carriage-ui--ctx-badge-last-time 0.0))
           (age (- now last))
           (sig-stale (not (equal sig carriage-ui--ctx-badge-cache-sig)))
           (have (consp carriage-ui--ctx-badge-cache)))
      (when (and (not carriage-ui--ctx-badge-pending)
                 (or (not have) sig-stale (>= age interval)))
        (carriage-ui--ctx-badge--schedule-refresh (if have 0.05 0.01)))
      (if (consp carriage-ui--ctx-badge-cache)
          carriage-ui--ctx-badge-cache
        (cons "[Ctx:?]" "Контекст: вычисление… (обновится автоматически)"))))

  (defun carriage-ui-refresh-context-badge ()
    "Force refresh of the [Ctx:N] badge asynchronously (nonblocking)."
    (interactive)
    (carriage-ui--ctx-invalidate))

  (defun carriage-ui--ml-seg-context ()
    "Build Context badge segment (click to refresh) using cached value only."
    (let* ((ctx (condition-case _e
                    (carriage-ui--context-badge)
                  (error (cons "[Ctx:!]" "Контекст: ошибка (см. *Messages*)"))))
           (ctx2 (carriage-ui--ctx-badge--normalize ctx))
           (lbl (car ctx2))
           (tip (cdr ctx2)))
      (when (stringp lbl)
        (carriage-ui--ml-button lbl #'carriage-ui--ctx-invalidate
                                (or tip "Обновить контекст (mouse-1)")))))

  (defun carriage-ui--patch-count ()
    "Return strictly O(1) cached number of #+begin_patch blocks."
    (or (and (boundp 'carriage-ui--patch-count-cache)
             (numberp carriage-ui--patch-count-cache)
             carriage-ui--patch-count-cache)
        0))

  (defun carriage-ui--ml-seg-patch ()
    "Build Patch counter segment from cached value only (no scans)."
    (let ((n (carriage-ui--patch-count)))
      (when (and (numberp n) (> n 0))
        (propertize (format "[P:%d]" n)
                    'help-echo "Количество #+begin_patch блоков (кэш, без пересканов)"))))

  (defun carriage-ui--ml-cache-key ()
    "Compute modeline cache key (strict O(1), no scans/IO in redisplay)."
    (let* ((uicons (and (fboundp 'carriage-ui--icons-available-p)
                        (carriage-ui--icons-available-p)))
           (blocks (if (and (boundp 'carriage-ui-modeline-blocks)
                            (listp carriage-ui-modeline-blocks)
                            carriage-ui-modeline-blocks)
                       carriage-ui-modeline-blocks
                     carriage-ui--modeline-default-blocks))
           (state (and (boundp 'carriage--ui-state) carriage--ui-state))
           (spin (and (boundp 'carriage-ui-enable-spinner) carriage-ui-enable-spinner
                      (memq state '(sending streaming dispatch waiting reasoning))
                      (fboundp 'carriage-ui--spinner-char)
                      (carriage-ui--spinner-char)))
           (ctx-ver (and (memq 'context blocks) (or carriage-ui--ctx-badge-version 0)))
           (patch-count (and (memq 'patch blocks)
                             (not (memq state '(sending streaming dispatch waiting reasoning)))
                             (carriage-ui--patch-count)))
           (abortp (and (boundp 'carriage--abort-handler) carriage--abort-handler)))
      (list uicons state spin
            ctx-ver patch-count abortp blocks
            (and (boundp 'carriage-mode-intent)  carriage-mode-intent)
            (and (boundp 'carriage-mode-suite)   carriage-mode-suite)
            (and (boundp 'carriage-mode-model)   carriage-mode-model)
            (and (boundp 'carriage-mode-backend) carriage-mode-backend)
            (and (boundp 'carriage-mode-provider) carriage-mode-provider)
            (and (boundp 'carriage-apply-engine) carriage-apply-engine)
            (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)
            (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context)
            (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context)
            (and (boundp 'carriage-mode-include-visible-context) carriage-mode-include-visible-context)
            (and (boundp 'carriage-mode-include-patched-files) carriage-mode-include-patched-files)
            (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
            (and (boundp 'carriage-ui-context-badge-refresh-interval) carriage-ui-context-badge-refresh-interval))))

  (defun carriage-ui--compute-context-badge (inc-doc inc-gpt inc-vis inc-patched)
    "Synchronous helper (tests/tooling): compute (LABEL . TOOLTIP) via carriage-context-count.
Must not read file contents; uses the fast counter."
    (let* ((off (not (or inc-doc inc-gpt inc-vis inc-patched))))
      (if off
          (cons "[Ctx:-]" "Контекст выключен (doc=off, gptel=off, vis=off, patched=off)")
        (require 'carriage-context nil t)
        (let* ((res (and (fboundp 'carriage-context-count)
                         (ignore-errors (carriage-context-count (current-buffer) (point)))))
               (cnt (or (and (listp res) (plist-get res :count)) 0)))
          (cons (format "[Ctx:%d]" cnt)
                (format "Контекст: файлов=%d — источники: doc=%s, gptel=%s, vis=%s, patched=%s"
                        cnt
                        (if inc-doc "on" "off")
                        (if inc-gpt "on" "off")
                        (if inc-vis "on" "off")
                        (if incpatched "on" "off")))))))

  )

(defvar carriage-ui--ctx-timer-purge-last-time 0.0
  "Timestamp (float seconds) of the last stale ctx-timer purge attempt.")

(defun carriage-ui--purge-stale-ctx-timers ()
  "Best-effort: cancel stale context refresh timers created by older Carriage versions.

Motivation:
- During upgrades, an already scheduled timer may still point to an older
  byte-compiled closure with a required argument list like \"#[(b) ...]\".
- Such timers may be invoked with 0 args (ARGS=nil) → wrong-number-of-arguments.

This function is intentionally conservative and must never signal."
  (ignore-errors (require 'timer))
  (let* ((now (float-time))
         ;; Throttle by time to avoid repeated scans when called defensively.
         (last (or (and (boundp 'carriage-ui--ctx-timer-purge-last-time)
                        carriage-ui--ctx-timer-purge-last-time)
                   0.0))
         (_ (setq carriage-ui--ctx-timer-purge-last-time now))
         (candidates (ignore-errors (append (copy-sequence timer-list)
                                            (copy-sequence timer-idle-list))))
         (killed 0))
    (dolist (tm candidates)
      (condition-case _e
          (when (timerp tm)
            (let* (;; Robustly extract function/args across Emacs versions.
                   (fn (cond
                        ((fboundp 'timer--function) (ignore-errors (timer--function tm)))
                        ((and (vectorp tm) (> (length tm) 5)) (aref tm 5))
                        (t nil)))
                   (args (cond
                          ((fboundp 'timer--args) (ignore-errors (timer--args tm)))
                          ((and (vectorp tm) (> (length tm) 6)) (aref tm 6))
                          (t nil)))
                   (s (and fn (prin1-to-string fn)))
                   ;; If args are missing, Emacs will call the timer with 0 args.
                   ;; Old byte-compiled closures expecting 1+ required args will explode.
                   (no-args (or (null args) (equal args '()))))
              (when (and (stringp s)
                         ;; Only touch byte-compiled closures (riskier ones on upgrades).
                         (string-match-p "#\\[" s)
                         ;; Only touch timers clearly related to ctx badge internals.
                         ;; Be slightly broader: some old closures don't contain our exact helper symbols,
                         ;; but do contain compute-context-badge / ctx-cache / ctx-schedule-refresh.
                         (or (string-match-p "carriage-ui--compute-context-badge" s)
                             (string-match-p "carriage-ui--context-badge" s)
                             (string-match-p "carriage-ui--ctx-schedule-refresh" s)
                             (string-match-p "carriage-ui--ctx-refresh-timer" s)
                             (string-match-p "carriage-ui--ctx-cache" s))
                         no-args
                         ;; Known broken pattern: closure has a required arg list (b) (or similar).
                         ;; If it has required args and the timer has no args saved, it will be invoked with 0 args.
                         (or (string-match-p "#\\[(b)" s)
                             (string-match-p "#\\[([^)]*\\<b\\>[^)]*)" s)
                             (string-match-p "save-current-buffer" s)))
                (ignore-errors (cancel-timer tm))
                (setq killed (1+ killed)))))
        (error nil)))
    killed))

;; Purge stale ctx timers from older Carriage versions once at load time, so a legacy
;; timer cannot fire (and error) before the first new refresh is scheduled.
(ignore-errors (carriage-ui--purge-stale-ctx-timers))

(defvar carriage-ui--gptel-context-version 0
  "Monotonic global counter incremented when gptel-context changes.

This is used as an O(1) invalidation signal for [Ctx:N] caches, without polling
and without scanning gptel-context from the modeline/redisplay path.")

(defun carriage-ui--note-gptel-context-change (&rest _ignored)
  "Note that gptel-context changed and refresh Ctx in visible Carriage buffers.

Policy:
- Always bump global `carriage-ui--gptel-context-version' so caches become stale.
- Additionally invalidate+schedule refresh only for *visible* carriage-mode buffers
  that have gptel-context enabled."
  (setq carriage-ui--gptel-context-version (1+ (or carriage-ui--gptel-context-version 0)))
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (bound-and-true-p carriage-mode)
                   (boundp 'carriage-mode-include-gptel-context)
                   carriage-mode-include-gptel-context
                   (get-buffer-window buf t)
                   (fboundp 'carriage-ui--ctx-invalidate))
          (ignore-errors (carriage-ui--ctx-invalidate)))))))

(with-eval-after-load 'gptel-context
  (dolist (fn '(gptel-context-add gptel-context-remove gptel-context-clear))
    (when (and (fboundp fn)
               (not (advice-member-p #'carriage-ui--note-gptel-context-change fn)))
      (advice-add fn :after #'carriage-ui--note-gptel-context-change))))

(defun carriage-ui--ctx-schedule-refresh (toggles tick delay)
  "Schedule a near-future context badge recomputation with TOGGLES and TICK after DELAY.

Coalescing semantics (perf critical):
- Repeated calls update pending snapshot (`carriage-ui--ctx-pending-toggles/tick`).
- At most one timer is pending per buffer.

Rate limiting:
- Applies a hard throttle on top of debounce: actual recomputation happens at most
  once per `carriage-ui-context-min-refresh-interval' seconds (default 1.0).

Robustness:
- Timer callback accepts `&rest` to tolerate older timer invocation conventions.
- Computation runs only for visible buffers (avoid background churn).
- Modeline cache is invalidated ONLY when the computed badge actually changes."
  ;; Always keep the latest request (cheap; avoids rescheduling churn).
  (setq carriage-ui--ctx-pending-toggles toggles)
  (setq carriage-ui--ctx-pending-tick tick)
  (let* ((buf (current-buffer))
         (now (float-time))
         (base-delay (max 0.0 (or delay 0.05)))
         (min-interval (max 0.0 (or (and (boundp 'carriage-ui-context-min-refresh-interval)
                                         carriage-ui-context-min-refresh-interval)
                                    1.0)))
         (last-time (or (and (listp carriage-ui--ctx-cache)
                             (numberp (plist-get carriage-ui--ctx-cache :time))
                             (plist-get carriage-ui--ctx-cache :time))
                        0.0))
         (earliest (+ last-time min-interval))
         (desired (max (+ now base-delay) earliest))
         (delay2 (max 0.01 (- desired now))))
    (cond
     ;; If a timer already exists, push it out if needed (do NOT recreate on every keystroke).
     ((timerp carriage-ui--ctx-refresh-timer)
      (ignore-errors
        (when (fboundp 'timer-set-time)
          (timer-set-time carriage-ui--ctx-refresh-timer (seconds-to-time desired)))))

     ;; Otherwise create a new one.
     (t
      (setq carriage-ui--ctx-refresh-timer
            (run-at-time
             delay2 nil
             (lambda (&rest _ignored)
               (ignore _ignored)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   ;; Always clear the timer first to avoid a stuck pending state.
                   (setq carriage-ui--ctx-refresh-timer nil)
                   ;; Skip background work for hidden buffers; keep last badge until visible again.
                   (when (get-buffer-window buf t)
                     (condition-case _e
                         (let* ((tog (or carriage-ui--ctx-pending-toggles
                                         (carriage-ui--context-toggle-states)))
                                (tk  (or carriage-ui--ctx-pending-tick
                                         (buffer-chars-modified-tick)))
                                (doc (plist-get tog :doc))
                                (gpt (plist-get tog :gpt))
                                (vis (plist-get tog :vis))
                                (pt  (plist-get tog :patched))
                                (val (carriage-ui--compute-context-badge doc gpt vis pt))
                                (t2 (float-time))
                                (old (and (listp carriage-ui--ctx-cache)
                                          (plist-get carriage-ui--ctx-cache :value)))
                                (changed (not (equal val old))))
                           ;; Always update cache time/tick, but invalidate UI only on real change.
                           (setq carriage-ui--ctx-cache
                                 (carriage-ui--ctx-build-cache tog tk t2 val))
                           (setq carriage-ui--ctx-placeholder-count 0
                                 carriage-ui--ctx-last-placeholder-time 0)
                           (when changed
                             (setq carriage-ui--ctx-badge-version (1+ (or carriage-ui--ctx-badge-version 0)))
                             (carriage-ui--invalidate-ml-cache)))
                       (error nil))
                     (force-mode-line-update t))))))))))
  t)

(defvar carriage--perf--ctx-after-change-guard-installed nil)

(unless carriage--perf--ctx-after-change-guard-installed
  (setq carriage--perf--ctx-after-change-guard-installed t)
  (when (fboundp 'carriage-ui--ctx-after-change)
    (advice-add
     'carriage-ui--ctx-after-change
     :around
     (lambda (orig &rest args)
       ;; If no context sources are enabled, avoid any scheduling on every edit.
       (if (or (bound-and-true-p carriage-mode-include-doc-context)
               (bound-and-true-p carriage-mode-include-gptel-context)
               (bound-and-true-p carriage-mode-include-visible-context)
               (bound-and-true-p carriage-mode-include-patched-files))
           (apply orig args)
         nil)))))

(provide 'carriage-ui)
;;; carriage-ui.el ends here
