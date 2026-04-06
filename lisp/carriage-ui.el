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
(require 'carriage-ui-faces)
(require 'carriage-ui-spinner)
(require 'carriage-ui-modeline)
(require 'carriage-ui-header)
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

(defcustom carriage-mode-icon-height 0.65
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

;; Faces moved to carriage-ui-faces.el

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

(defun carriage-ui--money-symbol ()
  "Return currency symbol string for UI rendering (best-effort)."
  (cond
   ((and (boundp 'carriage-pricing-currency-symbol)
         (stringp carriage-pricing-currency-symbol))
    carriage-pricing-currency-symbol)
   (t "₽")))

(defun carriage-ui--format-money-suffix (amount-u &optional currency-symbol)
  "Format AMOUNT-U (µ₽ integer) as \"12.34₽\" with half-up kopeck rounding.
When AMOUNT-U is nil/non-integer, return \"—\"."
  (if (not (integerp amount-u))
      "—"
    (let* ((sym (or (and (stringp currency-symbol) currency-symbol)
                    (carriage-ui--money-symbol)))
           (kopecks (/ (+ amount-u 5000) 10000)) ;; 1 коп. = 10_000 µ₽, half-up
           (rub (/ kopecks 100))
           (kop (% kopecks 100)))
      (format "%d.%02d%s" rub kop (or sym "")))))

(defun carriage-ui-doc-cost-refresh (&optional _event)
  "Interactive wrapper: refresh doc-cost now for current buffer."
  (interactive "e")
  (ignore-errors (carriage-ui-doc-cost-refresh-now (current-buffer))))

(defun carriage-ui-doc-cost-refresh-now (&optional buffer)
  "Recompute and cache document cost for BUFFER (or current buffer).
May scan buffer text; must not be called from redisplay."
  (with-current-buffer (or buffer (current-buffer))
    (let ((sum 0)
          (known 0)
          (unknown 0)
          (case-fold-search t)
          (results-found 0))
      (save-excursion
        ;; Primary source: CARRIAGE_RESULT lines
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+CARRIAGE_RESULT:[ \t]*\\(.*\\)$" nil t)
          (let* ((s (match-string 1))
                 (obj (condition-case _e
                          (car (read-from-string s))
                        (error nil)))
                 (v (and (listp obj) (plist-get obj :CAR_COST_TOTAL_U))))
            (setq results-found (1+ results-found))
            (cond
             ((integerp v)
              (setq sum (+ sum v))
              (setq known (1+ known)))
             ((and (listp obj) (plist-member obj :CAR_COST_TOTAL_U))
              ;; Explicit nil (unknown cost)
              (setq unknown (1+ unknown)))
             (t nil)))))
      ;; Fallback logic:
      ;; - If there are NO result lines at all → use fingerprint lines.
      ;; - If there ARE result lines but NONE has a known integer cost → use fingerprint lines.
      (when (or (<= results-found 0) (<= known 0))
        (setq sum 0 known 0 unknown 0)
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
                (setq unknown (1+ unknown)))
               (t nil))))))
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

;; Spinner moved to carriage-ui-spinner.el

;;; State management

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
    toggle-typed
    structure
    model
    state
    apply-status
    abort
    all
    context
    toggle-visible
    toggle-plain
    toggle-ctx
    toggle-map
    toggle-patched
    toggle-files
    doc-scope-all
    doc-scope-last
    req-cost
    doc-cost
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
- `toggle-map' — Project Map context toggle (begin_map).
- `doc-scope-all' — select doc-context scope 'all.
- `doc-scope-last' — select doc-context scope 'last.
- `req-cost' — last request cost/usage (fallback: tokens → bytes).
- `doc-cost' — document cost (sum of known per-request costs from result/fingerprint lines).
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
                         (const :tag "Request cost/usage (last request)" req-cost)
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
                         (const :tag "Project map toggle" toggle-map)
                         (const :tag "Visible-buffers toggle" toggle-visible)
                         (const :tag "Plain-text toggle" toggle-plain)
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

(defun carriage-ui--project-root-fast (&optional dir)
  "Return a best-effort project root for DIR using cheap heuristics.

Used by header-line rendering and MUST NOT call `project-current' or Projectile.

Returns an absolute directory name with trailing slash, or nil."
  (condition-case _e
      (let* ((d (file-name-as-directory (expand-file-name (or dir default-directory)))))
        (cond
         ;; Don't try to resolve project roots for remote dirs from UI code.
         ((file-remote-p d) nil)
         ;; Fast git root check (no external processes).
         (t
          (let ((root (locate-dominating-file d ".git")))
            (when (stringp root)
              (file-name-as-directory (expand-file-name root)))))))
    (error nil)))

(defun carriage-ui--project-name ()
  "Return cached project name (directory name of project root) or \"-\".

Important (perf):
- Must be cheap for header-line render path.
- MUST NOT call `project-current' / Projectile from here."
  (or carriage-ui--project-name-cached
      (let* ((root (or (carriage-ui--project-root-fast default-directory)
                       default-directory))
             (dir  (file-name-nondirectory (directory-file-name (or root default-directory))))
             (name (or (and (stringp dir) (not (string-empty-p dir)) dir) "-")))
        (setq carriage-ui--project-name-cached name)
        name)))


(defun carriage-ui-goto-outline (&optional _event)
  "Go to the nearest Org heading above point (best-effort) when clicking outline in header-line.
Must never open debugger, even when `debug-on-error' is non-nil."
  (interactive "e")
  (when (derived-mode-p 'org-mode)
    (let ((inhibit-debugger t))
      (condition-case _e
          (progn
            (require 'org nil t)
            (when (fboundp 'org-back-to-heading)
              (org-back-to-heading t)
              (recenter 1)))
        (error nil)))))

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
                 ('ctx-limit (when (fboundp 'all-the-icons-material)
                               (all-the-icons-material "data_usage"
                                                       :height carriage-mode-icon-height
                                                       :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                       :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
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
                 ('map (cond
                        ((fboundp 'all-the-icons-material)
                         (all-the-icons-material "map"
                                                 :height carriage-mode-icon-height
                                                 :v-adjust carriage-mode-icon-v-adjust
                                                 :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                        ((fboundp 'all-the-icons-faicon)
                         (all-the-icons-faicon "map"
                                               :height carriage-mode-icon-height
                                               :v-adjust carriage-mode-icon-v-adjust
                                               :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                        ((fboundp 'all-the-icons-octicon)
                         (all-the-icons-octicon "tree"
                                                :height carriage-mode-icon-height
                                                :v-adjust carriage-mode-icon-v-adjust
                                                :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                        (t nil)))
                 ('plain (cond
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "subject"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "file-text"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          (t nil)))
                 ('typed (cond
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "view_agenda"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "list-unordered"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                          (t nil)))
                 ;; Additional icons for doc-state badges (scope/profile/injection)
                 ;; Keep generic 'scope for other UI bits, and add explicit icons for modeline All/Last scope.
                 ('scope-all (when (fboundp 'all-the-icons-material)
                               (all-the-icons-material "layers"
                                                       :height carriage-mode-icon-height
                                                       :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                       :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('scope-all-off (when (fboundp 'all-the-icons-material)
                                   (all-the-icons-material "layers"
                                                           :height carriage-mode-icon-height
                                                           :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                           :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-muted-face)))))
                 ('scope-last (when (fboundp 'all-the-icons-material)
                                ;; NOTE: avoid glyphs that embed dots/digits (e.g. filter_1/looks_one).
                                ;; Use a clean arrow for "last/nearest" scope marker.
                                (all-the-icons-material "filter_1"
                                                        :height carriage-mode-icon-height
                                                        :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                        :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('scope-last-off (when (fboundp 'all-the-icons-material)
                                    (all-the-icons-material "filter_1"
                                                            :height carriage-mode-icon-height
                                                            :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                            :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-muted-face)))))
                 ('scope  (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "layers"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('profile (when (fboundp 'all-the-icons-material)
                             (all-the-icons-material "line_weight"
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

(defcustom carriage-mode-headerline-refresh-on-scroll nil
  "When non-nil, refresh Carriage header-line on `window-scroll-functions'.

Scrolling can generate very frequent events; enabling this may noticeably increase
CPU usage. Usually Carriage header-line depends on point (not window start), so
the default is nil."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-mode-headerline-idle-interval 0.2
  "Idle interval in seconds before refreshing header-line after cursor/scroll changes."
  :type 'number
  :group 'carriage-ui)

(defvar-local carriage-ui--headerline-idle-timer nil
  "Idle timer used to coalesce frequent header-line refreshes.")

(defun carriage-ui--headerline-idle-refresh-run-for-buffer (buf)
  "Timer entry point to refresh Carriage header-line in BUF (best-effort)."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (ignore-errors (carriage-ui--headerline-idle-refresh-run)))))

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
                                 #'carriage-ui--headerline-idle-refresh-run-for-buffer
                                 buf))))
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
                  ('map
                   (when (fboundp 'all-the-icons-material)
                     (all-the-icons-faicon "map"
                                           :height carriage-mode-icon-height
                                           :v-adjust carriage-mode-icon-v-adjust
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
                  ('plain
                   (cond
                    ((fboundp 'all-the-icons-material)
                     (all-the-icons-material "subject"
                                             :height carriage-mode-icon-height
                                             :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                             :face fplist))
                    ((fboundp 'all-the-icons-octicon)
                     (all-the-icons-octicon "file-text"
                                            :height carriage-mode-icon-height
                                            :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                            :face fplist))
                    (t nil)))
                  ('typed
                   (cond
                    ((fboundp 'all-the-icons-material)
                     (all-the-icons-material "view_agenda"
                                             :height carriage-mode-icon-height
                                             :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                             :face fplist))
                    ((fboundp 'all-the-icons-octicon)
                     (all-the-icons-octicon "list-unordered"
                                            :height carriage-mode-icon-height
                                            :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                            :face fplist))
                    (t nil)))
                  ('structure
                   (cond
                    ((fboundp 'all-the-icons-material)
                     (all-the-icons-material "device_hub"
                                             :height carriage-mode-icon-height
                                             :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                             :face fplist))
                    ((fboundp 'all-the-icons-faicon)
                     (all-the-icons-faicon "tree"
                                           :height carriage-mode-icon-height
                                           :v-adjust carriage-mode-icon-v-adjust
                                           :face fplist))
                    ((fboundp 'all-the-icons-octicon)
                     (all-the-icons-octicon "repo"
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
         (_ (require 'carriage-i18n nil t))
         (tip (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                  (carriage-i18n :menu-open-tooltip)
                "Open Carriage menu (C-c e SPC)"))
         (btn (carriage-ui--ml-button label #'carriage-menu-open tip)))
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
         (doc-cost-ver (and (memq 'doc-cost blocks) (or carriage-ui--doc-cost-version 0)))
         ;; State help-echo changes must invalidate the cached modeline string, otherwise
         ;; tooltips can become stale (force-mode-line-update alone is not enough).
         (state-tt-ver (and (memq 'state blocks)
                            (or (and (boundp 'carriage-ui--state-tooltip-version)
                                     carriage-ui--state-tooltip-version)
                                0)))
         ;; HTTP/error details (captured by transport) must also invalidate cache
         ;; so the [State] segment updates label/tooltip.
         (http-code (and (memq 'state blocks)
                         (boundp 'carriage--last-http-status)
                         carriage--last-http-status))
         (http-text (and (memq 'state blocks)
                         (boundp 'carriage--last-http-status-text)
                         carriage--last-http-status-text))
         (be-err (and (memq 'state blocks)
                      (boundp 'carriage--last-backend-error)
                      carriage--last-backend-error))
         (err-class (and (memq 'state blocks)
                         (boundp 'carriage--last-error-class)
                         carriage--last-error-class))
         (err-detail (and (memq 'state blocks)
                          (boundp 'carriage--last-error-detail)
                          carriage--last-error-detail))
         (ctx-limited (and (memq 'state blocks)
                           (boundp 'carriage--last-context-limited)
                           carriage--last-context-limited))
         (ctx-omitted (and (memq 'state blocks)
                           (boundp 'carriage--last-context-omitted)
                           carriage--last-context-omitted))
         ;; Last request cost/usage snapshot for req-cost block.
         (req-cost-key
          (and (memq 'req-cost blocks)
               (list
                (and (boundp 'carriage--last-cost)
                     (listp carriage--last-cost)
                     (plist-get carriage--last-cost :cost-total-u))
                (and (boundp 'carriage--last-usage)
                     (listp carriage--last-usage)
                     (plist-get carriage--last-usage :tokens-in))
                (and (boundp 'carriage--last-usage)
                     (listp carriage--last-usage)
                     (plist-get carriage--last-usage :tokens-out))
                (and (boundp 'carriage--last-usage)
                     (listp carriage--last-usage)
                     (plist-get carriage--last-usage :bytes-in))
                (and (boundp 'carriage--last-usage)
                     (listp carriage--last-usage)
                     (plist-get carriage--last-usage :bytes-out))
                (and (boundp 'carriage--last-model-id)
                     carriage--last-model-id))))
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
          state spin state-tt-ver http-code http-text be-err err-class err-detail ctx-limited ctx-omitted
          ctx-ver apply-ver doc-cost-ver req-cost-key
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
         (resolved (ignore-errors (carriage-llm-resolve-model carriage-mode-backend carriage-mode-provider raw-model)))
         (display-source (if (and (stringp resolved) (not (string-empty-p resolved)))
                             resolved
                           raw-model))
         (display-name (or (and (stringp display-source)
                                (ignore-errors (carriage-llm-display-name display-source)))
                           "-"))
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
             (t (or (and (fboundp 'carriage-llm-make-full-id)
                         (carriage-llm-make-full-id carriage-mode-backend carriage-mode-provider (or display-source raw-model)))
                    (and (stringp display-source) display-source)
                    (and (stringp raw-model) raw-model)
                    "-")))))
         ;; Make only the text part clickable to ensure mouse highlight and clicks work reliably.
         (btn (carriage-ui--ml-button bm-text
                                      #'carriage-select-model
                                      (format "Модель: %s (клик — выбрать)"
                                              (or full-id "-")))))
    (if (and ic (stringp btn))
        (concat ic (carriage-ui--icon-gap) btn)
      (or btn "-"))))

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
             ((null e) "git")
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
         (policy-sym (and (eq eng 'git)
                          (boundp 'carriage-git-branch-policy)
                          carriage-git-branch-policy))
         (policy-str (if (symbolp policy-sym) (symbol-name policy-sym) ""))
         (help0 (cond
                 ((and (eq eng 'git)
                       (featurep 'carriage-i18n) (fboundp 'carriage-i18n)
                       (stringp policy-str))
                  (carriage-i18n :engine-tooltip-branch engine-str policy-str))
                 ((and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                  (carriage-i18n :engine-tooltip))
                 (t "Select apply engine")))
         (help (format "%s\nТекущее значение: %s" (or help0 "") engine-str))
         (label
          (if icon
              ;; GUI: icon-only, value goes to tooltip.
              icon
            (let ((name (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                            (carriage-i18n :engine-label)
                          "Engine")))
              (format "%s: %s" (or name "Engine") engine-str)))))
    (carriage-ui--ml-button (or label "Engine") #'carriage-select-apply-engine (or help ""))))

(defun carriage-ui--ml-seg-state ()
  "Build State segment with spinner and face, including help-echo tooltip when available.
This segment represents *request/transport* state."
  (let* ((st (let ((s (and (boundp 'carriage--ui-state) carriage--ui-state)))
               (if (symbolp s) s 'idle)))
         (http-code (and (boundp 'carriage--last-http-status) carriage--last-http-status))
         (http-text (and (boundp 'carriage--last-http-status-text) carriage--last-http-status-text))
         (be-err (and (boundp 'carriage--last-backend-error) carriage--last-backend-error))
         (mid (and (boundp 'carriage--last-model-id) carriage--last-model-id))
         (ctx-limited (and (boundp 'carriage--last-context-limited) carriage--last-context-limited))
         (ctx-omitted (and (boundp 'carriage--last-context-omitted) carriage--last-context-omitted))
         (ctx-ic (when ctx-limited
                   (or (and (carriage-ui--icons-available-p)
                            (carriage-ui--icon 'ctx-limit))
                       "⚠")))
         (err-detail
          (cond
           ((and (stringp http-code) (not (string-empty-p http-code))) http-code)
           ((and (boundp 'carriage--last-error-detail)
                 (stringp carriage--last-error-detail)
                 (not (string-empty-p carriage--last-error-detail)))
            carriage--last-error-detail)
           ((and (boundp 'carriage--last-error-class)
                 (symbolp carriage--last-error-class))
            (pcase carriage--last-error-class
              ('LLM_E_TIMEOUT "timeout")
              ('LLM_E_ABORT "abort")
              ('LLM_E_NETWORK "network")
              ('LLM_E_PROVIDER "provider")
              ('LLM_E_INTERNAL "internal")
              (_ "error")))
           (t nil)))
         (label
          (cond
           ((and (eq st 'error) (stringp err-detail) (not (string-empty-p err-detail)))
            (format "Error: %s" err-detail))
           (t
            (carriage-ui--state-label st))))
         (txt (concat
               label
               (if (and carriage-ui-enable-spinner
                        (memq st '(sending streaming dispatch waiting reasoning)))
                   (concat " " (carriage-ui--spinner-char))
                 "")
               (if (stringp ctx-ic) (concat " " ctx-ic) "")))
         (face (pcase st
                 ('idle 'carriage-ui-state-idle-face)
                 ((or 'sending 'streaming 'dispatch 'waiting 'reasoning) 'carriage-ui-state-sending-face)
                 ('done 'carriage-ui-state-success-face)
                 ('error 'carriage-ui-state-error-face)
                 (_ nil)))
         (help0 (and (boundp 'carriage--ui-state-tooltip) carriage--ui-state-tooltip))
         (help-http
          (when (or http-code http-text be-err mid err-detail ctx-limited)
            (string-join
             (delq nil
                   (list
                    (and ctx-limited
                         (format "Context: limited (omitted=%s)"
                                 (if (integerp ctx-omitted) ctx-omitted (or ctx-omitted "-"))))
                    (and (stringp err-detail) (not (string-empty-p err-detail))
                         (format "Error class: %s" err-detail))
                    (and http-code
                         (format "HTTP: %s%s"
                                 http-code
                                 (if (and (stringp http-text) (not (string-empty-p http-text)))
                                     (format " — %s" http-text)
                                   "")))
                    (and (stringp be-err) (not (string-empty-p be-err))
                         (format "Backend: %s" be-err))
                    (and (stringp mid) (not (string-empty-p mid))
                         (format "Model: %s" mid))))
             "\n")))
         (help
          (string-join (delq nil (list help0 help-http)) "\n")))
    (cond
     ((and face (stringp help) (not (string-empty-p help)))
      (propertize txt 'face face 'help-echo help))
     (face
      (propertize txt 'face face))
     ((and (stringp help) (not (string-empty-p help)))
      (propertize txt 'help-echo help))
     (t txt))))

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

(defun carriage-ui--context-badge ()
  "Return a cons (LABEL . TOOLTIP) for the context badge.
Stub implementation — replace with actual logic if needed."
  (cons "Ctx" "Context"))

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
  "Build toggle for including files referenced by patch blocks in the current document."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :patched-tooltip)
                 "Include current contents of files referenced by #+begin_patch headers (subject to limits). Patch bodies are never sent to the LLM.")))
    (carriage-ui--toggle "Patched" 'carriage-mode-include-patched-files
                         #'carriage-toggle-include-patched-files
                         help 'patched)))

(defun carriage-ui--ml-seg-toggle-map ()
  "Build Project Map context toggle (begin_map)."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :map-tooltip)
                 "Toggle including Project Map (gitignore-aware file tree) as begin_map in request context.")))
    (carriage-ui--toggle "Map" 'carriage-mode-include-project-map
                         #'carriage-toggle-include-project-map
                         help 'map)))

(defun carriage-ui--ml-seg-toggle-visible ()
  "Build Visible-buffers context toggle."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :visible-tooltip)
                 "Toggle including visible frame buffers")))
    (carriage-ui--toggle "Vis" 'carriage-mode-include-visible-context
                         #'carriage-toggle-include-visible-context
                         help 'visible)))

(defun carriage-ui--ml-seg-toggle-plain ()
  "Build Plain-text (outside typed blocks) context toggle."
  (let* ((_ (require 'carriage-i18n nil t))
         (help (if (and (featurep 'carriage-i18n) (fboundp 'carriage-i18n))
                   (carriage-i18n :plain-tooltip)
                 "Toggle including plain text (outside typed blocks)")))
    (carriage-ui--toggle "Plain" 'carriage-mode-include-plain-text-context
                         #'carriage-toggle-include-plain-text-context
                         help 'plain)))

(defun carriage-ui--ml-seg-toggle-typed ()
  "Build toggle for including \"Typed Blocks (v1)\" guidance in Ask/Hybrid prompts."
  (let* ((intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (_ (require 'carriage-i18n nil t))
         (help "Toggle guidance asking the model to wrap key information into typed blocks (Ask/Hybrid)"))
    ;; In Code intent typedblocks guidance is irrelevant (patch-only output); render as disabled.
    (if (eq intent 'Code)
        (let* ((lbl (if (carriage-ui--icons-available-p)
                        (carriage-ui--toggle-icon 'typed nil)
                      (propertize "Typed" 'face 'carriage-ui-muted-face)))
               (s (copy-sequence lbl)))
          (add-text-properties
           0 (length s)
           (list 'help-echo "Typed Blocks guidance disabled in Intent=Code (patch-only). Switch to Ask/Hybrid to use it.")
           s)
          s)
      (carriage-ui--toggle "Typed" 'carriage-mode-typedblocks-structure-hint
                           #'carriage-toggle-typedblocks-structure-hint
                           help 'typed))))

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

(defun carriage-ui--ml-seg-doc-cost ()
  "Build document total cost segment from cached doc-cost snapshot (O(1)).

Label format:
- \"[123.45₽]\" (sum of known costs)
If cache is empty/uninitialized, schedule an async refresh and show a placeholder."
  (let* ((snap (carriage-ui-doc-cost-get))
         (total-u (and (listp snap) (plist-get snap :known-total-u)))
         (known (and (listp snap) (plist-get snap :known-count)))
         (unknown (and (listp snap) (plist-get snap :unknown-count)))
         (ts (and (listp snap) (plist-get snap :ts)))
         ;; Trigger initial compute (async) when cache hasn't been filled yet.
         (_ (when (and (not ts)
                       (fboundp 'carriage-ui-doc-cost-schedule-refresh)
                       (not (timerp carriage-ui--doc-cost-refresh-timer)))
              (ignore-errors (carriage-ui-doc-cost-schedule-refresh 0.05))))
         ;; Avoid misleading "0.00₽" when we have no known costs yet.
         (money
          (cond
           ((not ts) "—")
           ((<= (or known 0) 0) "—")
           (t (carriage-ui--format-money-suffix total-u))))
         (lbl (format "[%s]" money))
         (tip (format "Document cost (known only)\nknown=%s unknown=%s\nClick to refresh"
                      (or known 0) (or unknown 0))))
    (carriage-ui--ml-button lbl #'carriage-ui-doc-cost-refresh tip)))

(defun carriage-ui--req-cost--label ()
  "Return short label for last request cost; fallback to tokens/bytes when unknown."
  (let* ((cost (and (boundp 'carriage--last-cost) carriage--last-cost))
         (usage (and (boundp 'carriage--last-usage) carriage--last-usage))
         (known (and (listp cost)
                     (or (plist-get cost :known)
                         (plist-get cost :cost-known))))
         (total-u (and (listp cost) (plist-get cost :cost-total-u)))
         (unit0 (and (listp usage)
                     (or (plist-get usage :usage-unit)
                         (plist-get usage :unit)
                         (plist-get usage :CAR_USAGE_UNIT))))
         (unit (cond
                ((eq unit0 'bytes) 'bytes)
                ((eq unit0 'tokens) 'tokens)
                ((stringp unit0) (intern (downcase unit0)))
                (t nil)))
         (tin (and (listp usage) (plist-get usage :tokens-in)))
         (tout (and (listp usage) (plist-get usage :tokens-out)))
         (bin (and (listp usage) (plist-get usage :bytes-in)))
         (bout (and (listp usage) (plist-get usage :bytes-out))))
    (cond
     ((and known (integerp total-u) (> total-u 0))
      (format "Req:%s" (carriage-ui--format-money-suffix total-u)))

     ;; IMPORTANT: when usage is explicitly marked as bytes, do not present it as tokens
     ;; even if (:tokens-in/:tokens-out) are filled with byte counts by transport.
     ((eq unit 'bytes)
      (cond
       ((or (integerp bin) (integerp bout))
        (format "bytes:%s/%s"
                (if (integerp bin) bin "—")
                (if (integerp bout) bout "—")))
       ;; Fallback (if bytes are missing but tokens are present for some reason)
       ((or (integerp tin) (integerp tout))
        (format "tok:%s/%s"
                (if (integerp tin) tin "—")
                (if (integerp tout) tout "—")))
       (t "Req:—")))

     ;; Default policy: tokens first, then bytes.
     ((or (integerp tin) (integerp tout))
      (format "tok:%s/%s"
              (if (integerp tin) tin "—")
              (if (integerp tout) tout "—")))
     ((or (integerp bin) (integerp bout))
      (format "bytes:%s/%s"
              (if (integerp bin) bin "—")
              (if (integerp bout) bout "—")))
     (t "Req:—"))))

(defun carriage-ui--req-cost--tooltip ()
  "Build tooltip for last request cost/usage segment (best-effort)."
  (let* ((cost (and (boundp 'carriage--last-cost) carriage--last-cost))
         (usage (and (boundp 'carriage--last-usage) carriage--last-usage))
         (mid (and (boundp 'carriage--last-model-id) carriage--last-model-id))
         (known (and (listp cost)
                     (or (plist-get cost :known)
                         (plist-get cost :cost-known))))
         (total-u (and (listp cost) (plist-get cost :cost-total-u)))
         (tin (and (listp usage) (plist-get usage :tokens-in)))
         (tout (and (listp usage) (plist-get usage :tokens-out)))
         (bin (and (listp usage) (plist-get usage :bytes-in)))
         (bout (and (listp usage) (plist-get usage :bytes-out))))
    (string-join
     (delq nil
           (list
            "Last request cost/usage"
            (and (stringp mid) (not (string-empty-p mid)) (format "Model: %s" mid))
            (format "Cost known: %s" (if known "yes" "no"))
            (format "Cost total: %s" (if (integerp total-u) (carriage-ui--format-money-suffix total-u) "—"))
            (format "Tokens: in=%s out=%s"
                    (if (integerp tin) tin "—")
                    (if (integerp tout) tout "—"))
            (format "Bytes: in=%s out=%s"
                    (if (integerp bin) bin "—")
                    (if (integerp bout) bout "—"))
            (when (not known)
              "Note: token usage may be unavailable in streaming mode; fallback uses tokens/bytes when possible.")))
     "\n")))

(defun carriage-ui--ml-seg-req-cost ()
  "Build last request cost/usage segment (money → tokens → bytes → em dash)."
  (let* ((lbl (carriage-ui--req-cost--label))
         (tip (carriage-ui--req-cost--tooltip)))
    (propertize lbl
                'help-echo (and (stringp tip) (not (string-empty-p tip)) tip)
                'mouse-face 'mode-line-highlight)))

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
    ('req-cost      (carriage-ui--ml-seg-req-cost))
    ('doc-cost      (carriage-ui--ml-seg-doc-cost))
    ('context       (carriage-ui--ml-seg-context))
    ('patch         (carriage-ui--ml-seg-patch))
    ;; 'dry removed
    ;; 'apply removed
    ('all           (carriage-ui--ml-seg-all))
    ('toggle-patched (carriage-ui--ml-seg-toggle-patched))
    ('toggle-map     (carriage-ui--ml-seg-toggle-map))
    ('toggle-visible (carriage-ui--ml-seg-toggle-visible))
    ('toggle-plain   (carriage-ui--ml-seg-toggle-plain))
    ('toggle-typed   (carriage-ui--ml-seg-toggle-typed))
    ('structure     (carriage-ui--ml-seg-structure))
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
      (let* ((blocks0 (if (and (listp carriage-ui-modeline-blocks)
                               carriage-ui-modeline-blocks)
                          carriage-ui-modeline-blocks
                        carriage-ui--modeline-default-blocks))
             (blocks (carriage-ui--modeline-blocks-normalize blocks0)))
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

(defvar-local carriage-ui--state-tooltip-version 0
  "Monotonic version for state tooltip changes.
Used to invalidate the cached modeline string so help-echo updates are visible.")

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
  "Set state tooltip to S without forcing full modeline churn."
  (setq carriage--ui-state-tooltip
        (when (and (stringp s) (> (length s) 0))
          (let ((mx (or carriage-mode-state-tooltip-max-chars 1000)))
            (carriage-ui--trim-right s mx))))
  ;; Tooltip changes alone should not continuously rebuild the whole modeline.
  ;; Only bump tooltip version at a throttled rate; avoid immediate redisplay forcing here.
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
      (setq carriage-ui--state-tooltip-version
            (1+ (or carriage-ui--state-tooltip-version 0))))))

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
      (condition-case _e
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
                  (let ((inhibit-debugger t))
                    (setq carriage-ui--last-outline-path-str
                          (condition-case nil
                              (or (carriage-ui--org-outline-path) "")
                            (error ""))))
                  (setq carriage-ui--outline-dirty nil))))
            (when (get-buffer-window (current-buffer) t)
              (force-mode-line-update)))
        (error
         ;; Fail-closed: never let headerline idle timer surface Org parser errors.
         (setq carriage-ui--last-outline-path-str "")
         (setq carriage-ui--outline-dirty nil)
         (when (get-buffer-window (current-buffer) t)
           (force-mode-line-update))))
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

;; Install advices after carriage-mode loads (safe via fboundp).
(with-eval-after-load 'carriage-mode
  (ignore-errors (carriage-ui--install-advices)))




;; Integrated guards and cursor policy for inline ID and streaming cursor behavior.

(defvar-local carriage--inline-id-inserted nil
  "Non-nil when the inline per-request marker has already been inserted in this buffer for the current request.")

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


;; -----------------------------------------------------------------------------
;; Modeline toggle: \"Соблюдать структуру\" (Org hierarchy / outline compliance)
;;
;; IMPORTANT:
;; - This toggle is independent from TypedBlocks guidance.
;; - Prompt integration is done outside UI (see carriage-mode / typedblocks layers).

(defun carriage-ui--modeline-blocks-normalize (blocks)
  "Normalize modeline BLOCKS order:
- Ensure 'structure appears immediately after 'toggle-typed when 'toggle-typed exists.
- Otherwise, insert 'structure immediately before 'model when 'model exists.
- Otherwise, append 'structure to the end.
Idempotent."
  (let* ((bs (delete-dups (copy-sequence (or blocks '()))))
         (bs (delq 'structure bs)))
    (cond
     ((memq 'toggle-typed bs)
      (let ((out '()))
        (dolist (b bs (nreverse out))
          (push b out)
          (when (eq b 'toggle-typed)
            (push 'structure out)))))
     ((memq 'model bs)
      (let ((out '()))
        (dolist (b bs (nreverse out))
          (when (eq b 'model)
            (push 'structure out))
          (push b out))))
     (t
      (append bs (list 'structure))))))

(defun carriage-ui--structure-toggle-icon (&optional enabled)
  "Return a \"tree\" icon string for the structure toggle.
ENABLED controls color: ON → cyan-ish, OFF → grey.
Must not break nerd-icons/all-the-icons glyph properties."
  (let* ((ico
          (cond
           ;; Prefer nerd-icons (Nerd Font)
           ((and (require 'nerd-icons nil t)
                 (fboundp 'nerd-icons-mdicon))
            (or (ignore-errors (nerd-icons-mdicon "nf-md-file_tree_outline"))
                (ignore-errors (nerd-icons-mdicon "nf-md-file_tree"))
                (ignore-errors (nerd-icons-mdicon "nf-md-file_tree_outline"))
                "T"))
           ;; Fallback to all-the-icons
           ((and (require 'all-the-icons nil t)
                 (fboundp 'all-the-icons-material))
            (or (ignore-errors (all-the-icons-material "line_weight" :height 0.95 :v-adjust 0.0))
                (ignore-errors (and (fboundp 'all-the-icons-faicon)
                                    (all-the-icons-faicon "sitemap" :height 0.95 :v-adjust 0.0)))
                "T"))
           ;; Plain fallback
           (t "T")))
         ;; Preserve glyph text-properties (font family/face) by copying.
         (s (if (stringp ico) (copy-sequence ico) (copy-sequence (format "%s" ico))))
         ;; Choose a color face and *append* it to keep icon font face intact.
         (on-face (cond ((facep 'nerd-icons-cyan) 'nerd-icons-cyan)
                        ((facep 'all-the-icons-blue) 'all-the-icons-blue)
                        (t 'link)))
         (off-face (cond ((facep 'nerd-icons-silver) 'nerd-icons-silver)
                         ((facep 'all-the-icons-silver) 'all-the-icons-silver)
                         (t 'shadow))))
    (add-face-text-property 0 (length s) (if enabled on-face off-face) 'append s)
    ;; Extra safety: ensure disabled is grey even if the icon already has strong foreground.
    (unless enabled
      (add-face-text-property 0 (length s) 'shadow 'append s))
    s))

(defun carriage-ui--ml-seg-structure ()
  "Build modeline segment for Org structure compliance toggle."
  (let* ((intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (_ (require 'carriage-i18n nil t))
         (help
          (string-join
           '("Соблюдать структуру (Org)"
             ""
             "Когда включено, Carriage добавляет строгие правила формата ответа:"
             "- Ответ должен быть валидным Org (без Markdown fences ```)"
             "- Первая непустая строка — заголовок Org нужного уровня (L+1 или уровень 1)"
             "- Заголовок кратко описывает суть итерации из контекста"
             ""
             "mouse-1: переключить")
           "\n")))
    ;; Policy: in Intent=Code this toggle is effectively disabled (Code requires patch-only output).
    ;; Render as a disabled (non-clickable) muted icon/label, similar to Typed toggle.
    (if (eq intent 'Code)
        (let* ((lbl (if (carriage-ui--icons-available-p)
                        (carriage-ui--toggle-icon 'structure nil)
                      (propertize "Struct" 'face 'carriage-ui-muted-face)))
               (s (copy-sequence lbl)))
          (add-text-properties
           0 (length s)
           (list 'help-echo
                 "Соблюдать структуру отключено в Intent=Code (patch-only). Переключитесь на Ask/Hybrid, чтобы использовать этот режим.")
           s)
          s)
      ;; Render like other toggles (Typed/Map/Plain): icon-only in GUI, text in TTY.
      ;; ON  -> accent-blue (same as other context toggles)
      ;; OFF -> muted grey, without breaking icon glyph properties.
      (carriage-ui--toggle
       "Struct"
       'carriage-mode-org-structure-hint
       #'carriage-toggle-org-structure-hint
       help
       'structure))))


(provide 'carriage-ui)
;;; carriage-ui.el ends here
