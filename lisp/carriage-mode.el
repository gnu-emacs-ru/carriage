;;; carriage-mode.el --- Main minor mode and entry points  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: tools, convenience
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/carriage-mode-v2.org
;;   spec/ui-v2.org
;;   spec/llm-transport-v2.org
;;   spec/context-integration-v2.org
;;   spec/keyspec-v2.org
;;
;;; Commentary:
;; Minor mode, public commands, and integration glue for Carriage.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-parser)
(require 'carriage-apply)
(require 'carriage-report)
(require 'carriage-llm-registry)
(require 'carriage-ui)
(require 'carriage-suite)
(require 'carriage-pricing nil t)
(require 'carriage-sre-core)
(require 'carriage-doc-state nil t)
(require 'carriage-doc-state-perf nil t)
(require 'carriage-reasoning-fold nil t)

;; -----------------------------------------------------------------------------
;; Reasoning fold UX:
;; - When reasoning blocks are folded via overlays, reveal the original text when point enters.
;; - Restore the fold when point leaves the block.
;; Best-effort: never signal; supports unknown overlay internals by using heuristics.
(defvar-local carriage--reasoning-fold--revealed-ovs nil
  "Internal list of reasoning fold overlays currently revealed due to point being inside.")

(defun carriage--reasoning-fold--overlay-p (ov)
  "Return non-nil when OV looks like a reasoning-fold overlay.

Important: keep this predicate narrow to avoid touching unrelated overlays
(e.g., doc-state/fingerprint folds)."
  (and (overlayp ov)
       (overlay-buffer ov)
       (or (overlay-get ov 'carriage-reasoning-fold)
           (overlay-get ov 'carriage-reasoning)
           ;; carriage-reasoning-fold.el marks its overlays by category.
           (eq (overlay-get ov 'category) 'carriage-reasoning-fold))
       ;; Never touch doc-state/fingerprint fold overlays.
       (not (eq (overlay-get ov 'category) 'carriage-doc-state-fold))
       (not (overlay-get ov 'carriage-fold-summary))
       (not (overlay-get ov 'carriage-fold-tooltip))))

(defun carriage--reasoning-fold--reveal (ov)
  "Reveal OV by removing hiding properties, saving them for restoration."
  (when (overlayp ov)
    (unless (overlay-get ov 'carriage--saved-invisible)
      (overlay-put ov 'carriage--saved-invisible (overlay-get ov 'invisible)))
    (unless (overlay-get ov 'carriage--saved-display)
      (overlay-put ov 'carriage--saved-display (overlay-get ov 'display)))
    (unless (overlay-get ov 'carriage--saved-before-string)
      (overlay-put ov 'carriage--saved-before-string (overlay-get ov 'before-string)))
    ;; Reveal (show original text)
    ;; Also remove cursor-intangible/intangible so point can actually enter the block.
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'display nil)
    (overlay-put ov 'before-string nil)
    (overlay-put ov 'intangible nil)
    (overlay-put ov 'cursor-intangible nil)))

(defun carriage--reasoning-fold--sanitize-display (disp)
  "Return DISP with noisy hints removed (e.g., \"(TAB: toggle …)\").

Important:
- Do not remove generic words like \"toggle\" globally (it can be part of legit text).
- If sanitization would produce an empty placeholder, return a minimal \"…\" so the
  overlay remains visible and clickable."
  (if (not (stringp disp))
      disp
    (let* ((s disp))
      ;; Remove parenthesized hint chunks like: "(TAB: toggle ...)" (case-insensitive for TAB).
      (setq s (replace-regexp-in-string
               "(\\s-*\\(TAB\\|Tab\\):\\s-*toggle\\([^)]*\\))" "" s t))
      ;; Remove non-parenthesized tail hints like: "TAB: toggle ..." (keep left side).
      (setq s (replace-regexp-in-string
               "\\s-*\\(TAB\\|Tab\\):\\s-*toggle\\b.*\\'" "" s t))
      ;; Collapse whitespace.
      (setq s (replace-regexp-in-string "[ \t][ \t]+" " " s t t))
      (setq s (string-trim s))
      (if (string-empty-p s) "…" s))))

(defun carriage--reasoning-fold--restore (ov)
  "Restore OV hiding properties if previously saved."
  (when (overlayp ov)
    (when (overlay-get ov 'carriage--saved-invisible)
      (overlay-put ov 'invisible (overlay-get ov 'carriage--saved-invisible)))
    (when (overlay-get ov 'carriage--saved-display)
      (let ((d (overlay-get ov 'carriage--saved-display)))
        (overlay-put ov 'display (carriage--reasoning-fold--sanitize-display d))))
    (when (overlay-get ov 'carriage--saved-before-string)
      (let ((b (overlay-get ov 'carriage--saved-before-string)))
        (overlay-put ov 'before-string (carriage--reasoning-fold--sanitize-display b))))
    ;; Ensure point can enter folded reasoning placeholder (avoid "cursor stuck").
    (overlay-put ov 'intangible nil)
    (overlay-put ov 'cursor-intangible nil)))

(defun carriage--reasoning-fold--sanitize-all (&optional buffer)
  "Best-effort sanitize all reasoning-fold overlays in BUFFER (or current buffer).

- Remove noisy \"TAB: toggle …\" hints from overlay display/before-string.
- Disable cursor-intangible/intangible so point can enter the folded placeholder."
  (with-current-buffer (or buffer (current-buffer))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (carriage--reasoning-fold--overlay-p ov)
        (ignore-errors
          (let ((d (overlay-get ov 'display)))
            (when d
              (overlay-put ov 'display (carriage--reasoning-fold--sanitize-display d))))
          (let ((b (overlay-get ov 'before-string)))
            (when b
              (overlay-put ov 'before-string (carriage--reasoning-fold--sanitize-display b))))
          (overlay-put ov 'intangible nil)
          (overlay-put ov 'cursor-intangible nil))))
    t))

(defun carriage--reasoning-fold--post-command ()
  "Post-command hook to reveal folded reasoning block at point and refold on leave."
  (condition-case _e
      (let* ((pt (point))
             ;; Also inspect overlays adjacent to point to avoid the \"stuck at boundary\"
             ;; behavior when approaching folded overlays from below.
             (lo (max (point-min) (1- pt)))
             (hi (min (point-max) (1+ pt)))
             (near (cl-remove-if-not #'carriage--reasoning-fold--overlay-p
                                     (overlays-in lo hi)))
             (cur  (cl-remove-if-not #'carriage--reasoning-fold--overlay-p
                                     (overlays-at pt)))
             (prev (or carriage--reasoning-fold--revealed-ovs '())))
        ;; Ensure the cursor can ENTER folded placeholders from either direction.
        ;; If overlays remain cursor-intangible, point never enters → our reveal hook never runs.
        (dolist (ov near)
          (ignore-errors
            (overlay-put ov 'intangible nil)
            (overlay-put ov 'cursor-intangible nil)
            ;; Also clean up noisy hints in-place (TAB: toggle ...)
            (let ((d (overlay-get ov 'display))
                  (b (overlay-get ov 'before-string)))
              (when d (overlay-put ov 'display (carriage--reasoning-fold--sanitize-display d)))
              (when b (overlay-put ov 'before-string (carriage--reasoning-fold--sanitize-display b))))))

        ;; Restore overlays that are no longer under point
        (dolist (ov prev)
          (unless (memq ov cur)
            (ignore-errors (carriage--reasoning-fold--restore ov))))
        ;; Reveal current overlays under point
        (dolist (ov cur)
          (unless (memq ov prev)
            (ignore-errors (carriage--reasoning-fold--reveal ov))))
        (setq carriage--reasoning-fold--revealed-ovs cur))
    (error nil)))

;; Autoload stub ensures calling carriage-global-mode works even if file isn't loaded yet.
(require 'carriage-global-mode)

;; Safety shim: guarantee normalizer exists even if carriage-web isn't loaded yet (timers may fire early).
(unless (fboundp 'carriage-web--payload-normalize)
  (defun carriage-web--payload-normalize (payload)
    (condition-case _e
        payload
      (error payload))))

;; Ensure 'transports' subdirectory is on load-path when loading carriage-mode directly
(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (transports-dir (and this-dir (expand-file-name "transports" this-dir))))
  (when (and transports-dir (file-directory-p transports-dir))
    (add-to-list 'load-path transports-dir)))
;; Defer transport to avoid circular require; also provide autoloads for adapters
(autoload 'carriage-transport-begin "carriage-transport" "Transport: begin (install abort handler and set UI state)." t)
(autoload 'carriage-transport-streaming "carriage-transport" "Transport: switch UI to streaming." t)
(autoload 'carriage-transport-complete "carriage-transport" "Transport: finalize (clear abort and set UI state)." t)
(autoload 'carriage-transport-dispatch "carriage-transport" "Transport: dispatch request to adapter with lazy loading." t)
(declare-function carriage-select-apply-engine "carriage-apply-engine" (&optional engine))

(defun carriage--ensure-transport ()
  "Load carriage-transport when its functions are not yet defined (no autoloads)."
  (unless (fboundp 'carriage-transport-begin)
    (ignore-errors (require 'carriage-transport))))

;; -------------------------------------------------------------------
;; Streaming: remove legacy advice that can block/slow streaming insertion.
;;
;; Some older builds advised `carriage-insert-stream-chunk' (e.g. with
;; `carriage--stream-chunk-save-point'). When such advice is loaded late
;; (after `carriage-mode' is enabled), it can re-introduce the symptom:
;; "stream prints only after keypress".
;;
;; Policy:
;; - Best-effort: never signal.
;; - Remove the legacy advice immediately when detected.
;; - Keep a short-lived `after-load-functions' cleaner so late loads cannot
;;   re-add the advice. Uninstall the cleaner once the advice is gone.
(defvar carriage--stream-legacy-advice-cleaner-installed nil
  "Non-nil when legacy stream advice cleaner hook was installed.")

(defun carriage--stream-remove-legacy-advice (&optional _file)
  "Best-effort remove legacy advice that can block streaming insertion."
  (ignore-errors
    (when (and (fboundp 'carriage--stream-chunk-save-point)
               (advice-member-p #'carriage--stream-chunk-save-point 'carriage-insert-stream-chunk))
      (advice-remove 'carriage-insert-stream-chunk #'carriage--stream-chunk-save-point)))
  ;; If the bad advice is gone, we can uninstall ourselves to avoid per-load overhead.
  (ignore-errors
    (when (and (fboundp 'carriage--stream-chunk-save-point)
               (not (advice-member-p #'carriage--stream-chunk-save-point 'carriage-insert-stream-chunk)))
      (remove-hook 'after-load-functions #'carriage--stream-remove-legacy-advice)))
  t)

(defun carriage--stream-install-legacy-advice-cleaner ()
  "Install `after-load-functions' cleaner for legacy stream advice (idempotent)."
  (unless carriage--stream-legacy-advice-cleaner-installed
    (setq carriage--stream-legacy-advice-cleaner-installed t)
    (add-hook 'after-load-functions #'carriage--stream-remove-legacy-advice)))

(defcustom carriage-mode-default-intent 'Ask
  "Default Intent for Carriage: 'Ask | 'Code | 'Hybrid."
  :type '(choice (const Ask) (const Code) (const Hybrid))
  :group 'carriage)

(defcustom carriage-mode-default-suite 'aibo
  "Default Suite: one of 'sre, 'aibo or 'udiff."
  :type '(choice (const sre) (const aibo) (const udiff))
  :group 'carriage)

(defcustom carriage-mode-default-model "gptel-default"
  "Default LLM model name for Carriage."
  :type 'string
  :group 'carriage)

(defcustom carriage-mode-default-backend 'gptel
  "Default LLM transport backend for Carriage (e.g., 'gptel)."
  :type '(choice symbol string)
  :group 'carriage)

(defcustom carriage-mode-state-file ".context/carriage/carriage-state.el"
  "Project-relative path for per-project Carriage state persistence file."
  :type 'string
  :group 'carriage)


(defcustom carriage-mode-report-open-policy 'on-error
  "Report auto-open policy:
- 'on-error — open only when there are failures (default),
- 'always   — always open after dry-run/apply,
- 'never    — never open automatically."
  :type '(choice (const on-error) (const always) (const never))
  :group 'carriage)
(make-variable-buffer-local 'carriage-mode-report-open-policy)

(defun carriage--report-open-maybe (report)
  "Open report according to =carriage-mode-report-open-policy'."
  (let ((pol (and (boundp 'carriage-mode-report-open-policy)
                  carriage-mode-report-open-policy)))
    (pcase pol
      ('always (carriage-report-open report))
      ('never  nil)
      (_
       (let* ((sum   (plist-get report :summary))
              (fails (or (plist-get sum :fail) 0))
              (msgs  (plist-get report :messages))
              (has-err (cl-some (lambda (m) (eq (plist-get m :severity) 'error)) msgs)))
         (when (or (> fails 0) has-err)
           (carriage-report-open report)))))))

(defcustom carriage-mode-show-diffs t
  "Require showing diffs before apply."
  :type 'boolean :group 'carriage)
(make-variable-buffer-local 'carriage-mode-show-diffs)

(defcustom carriage-mode-auto-open-log nil
  "When non-nil, open *carriage-log* automatically on mode enable and when sending."
  :type 'boolean :group 'carriage)
(make-variable-buffer-local 'carriage-mode-auto-open-log)

(defcustom carriage-mode-auto-open-traffic nil
  "When non-nil, open *carriage-traffic* automatically when sending."
  :type 'boolean :group 'carriage)
(make-variable-buffer-local 'carriage-mode-auto-open-traffic)

(defcustom carriage-mode-confirm-apply-all nil
  "Ask for confirmation before applying all blocks (C-c e A)."
  :type 'boolean :group 'carriage)
(make-variable-buffer-local 'carriage-mode-confirm-apply-all)

(defcustom carriage-mode-confirm-apply nil
  "Ask for confirmation before applying a single block or a region."
  :type 'boolean :group 'carriage)
(make-variable-buffer-local 'carriage-mode-confirm-apply)




(defcustom carriage-mode-use-icons t
  "Use all-the-icons in mode-line if available."
  :type 'boolean :group 'carriage)
(make-variable-buffer-local 'carriage-mode-use-icons)

;; UI v1.3 — Suite/Engine iconized labels
(defcustom carriage-mode-use-suite-icon t
  "When non-nil, show Suite label as an icon (with [value]) in mode-line."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-use-engine-icon t
  "When non-nil, show Engine label as an icon (with [value]) in mode-line."
  :type 'boolean :group 'carriage)

;; UI v1.3 — Flash and audio notifications
(defcustom carriage-mode-flash-patches t
  "When non-nil, flash last-iteration patch blocks on successful request completion."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-flash-duration 1.0
  "Flash duration (seconds) for highlighting generated patches."
  :type 'number :group 'carriage)

(defcustom carriage-mode-audio-notify nil
  "When non-nil, play a sound on successful request completion."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-audio-sound 'beep
  "Sound to use for notifications: 'beep or a path to a sound file."
  :type '(choice (const :tag "Beep" beep) (string :tag "Sound file path"))
  :group 'carriage)

(defun carriage--audio-notify-success ()
  "Play an audio notification according to Customize settings."
  (when (and (boundp 'carriage-mode-audio-notify) carriage-mode-audio-notify)
    (condition-case _e
        (let ((snd (and (boundp 'carriage-mode-audio-sound) carriage-mode-audio-sound)))
          (cond
           ((and (stringp snd) (file-exists-p snd))
            (play-sound-file snd))
           ((eq snd 'beep)
            (beep))
           (t
            (ignore-errors (beep)))))
      (error nil))))

(defcustom carriage-mode-include-reasoning 'block
  "Policy for including reasoning during streaming:
- 'block — print reasoning inside #+begin_reasoning/#+end_reasoning
- 'ignore — do not insert reasoning into the source buffer (still logged)."
  :type '(choice (const block) (const ignore))
  :group 'carriage)

(defcustom carriage-mode-hide-reasoning-blocks t
  "When non-nil, fold all #+begin_reasoning…#+end_reasoning blocks in Org buffers.
Folding is UI-only (overlays via `carriage-reasoning-fold') and never modifies text.
Also folds newly streamed reasoning as early as possible and keeps it folded when completed."
  :type 'boolean
  :group 'carriage)
(make-variable-buffer-local 'carriage-mode-hide-reasoning-blocks)

;; v1.1 — Context toggles and limits
(defcustom carriage-mode-include-gptel-context nil
  "When non-nil, include gptel-context (buffers/files) into the request context."
  :type 'boolean :group 'carriage)
(make-variable-buffer-local 'carriage-mode-include-gptel-context)

(defcustom carriage-mode-include-doc-context t
  "When non-nil, include file contents from document #+begin_context blocks.
Which block(s) are used depends on `carriage-doc-context-scope' (all vs last)."
  :type 'boolean :group 'carriage)
(make-variable-buffer-local 'carriage-mode-include-doc-context)

(defcustom carriage-mode-include-visible-context nil
  "When non-nil, include visible buffers (current frame) into the request context."
  :type 'boolean :group 'carriage)
(make-variable-buffer-local 'carriage-mode-include-visible-context)

(defcustom carriage-mode-context-injection 'system
  "Where to inject collected context: 'system (default) or 'user."
  :type '(choice (const system) (const user))
  :group 'carriage)

(defcustom carriage-mode-context-max-files 100
  "Max number of files to include from context sources."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-context-max-total-bytes 1048576
  "Max total bytes of file contents included from context sources."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-wip-branch "carriage/WIP"
  "Default Git WIP branch name used for applying changes."
  :type 'string :group 'carriage)

(defcustom carriage-mode-sre-preview-max 3
  "Maximum number of SRE preview chunks (mini-diffs) to show per pair in dry-run.
For :occur all, at most this many previews are included; the rest are summarized
as a “(+N more)” tail."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-sre-preview-context-lines 1
  "Number of context lines to include above and below each SRE mini-diff preview.
0 means no context (only -old/+new lines)."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-max-batch-pairs 200
  "Maximum number of pairs allowed in an :op 'sre' block."
  :type 'integer :group 'carriage)

(defcustom carriage-mode-sre-noop-on-zero-matches nil
  "When non-nil, treat :occur first with 0 matches as NOOP: report 'skip with a warning.
If nil (default v1 behavior), such cases are considered a failure in dry-run."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-show-header-line t
  "When non-nil, install a buffer-local header-line segment for Carriage."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-headerline-max-width nil
  "Maximum width of header-line in columns. When nil, use window width."
  :type '(choice (const :tag "Auto" nil) integer)
  :group 'carriage)

(defcustom carriage-mode-show-mode-line-ui t
  "When non-nil, add a buffer-local modeline segment for Carriage."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-spinner-interval 0.7
  "Spinner update interval in seconds for sending/streaming states."
  :type 'number :group 'carriage)

(defcustom carriage-mode-allow-patch-binary nil
  "Allow binary patches in :op \"patch\" blocks.
Note: v1 forbids binary patches; this option remains nil in v1 and is reserved for future versions."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-require-patch-description t
  "When non-nil, prompt fragments (sre/aibo/patch) instruct the model to include
:description \"Короткое описание\" in each #+begin_patch header. Parsers ignore
unknown keys, so this does not affect apply pipeline semantics."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-mode-hide-applied-patches t
  "When non-nil, automatically fold applied #+begin_patch blocks (:applied t) in Org buffers.
Folding is UI-only (uses overlays via carriage-patch-fold); it does not modify buffer text.
Applied patch contents are still excluded from outgoing LLM payloads independently of this setting.
When a patch is applied and annotated, hiding takes effect immediately (overlays are refreshed)."
  :type 'boolean
  :group 'carriage)

(defcustom carriage-commit-default-message "carriage: apply changes"
  "Default commit message used by Commit commands.
May be a string or a function of zero args returning string."
  :type '(choice string function) :group 'carriage)


(defcustom carriage-enable-legacy-bindings nil
  "When non-nil, enable legacy bindings (C-c M-RET / C-c RET) in carriage-mode buffers.
These are provided for compatibility and may be removed in a future release."
  :type 'boolean :group 'carriage)

(defvar-local carriage-mode-intent carriage-mode-default-intent
  "Current Carriage Intent for this buffer: 'Ask | 'Code | 'Hybrid.")

(defvar-local carriage-mode-suite carriage-mode-default-suite
  "Current Carriage Suite for this buffer.")

(defvar-local carriage-mode-model carriage-mode-default-model
  "Current Carriage model string for this buffer.")

(defvar-local carriage-mode-backend carriage-mode-default-backend
  "Current Carriage backend identifier (symbol or string) for this buffer.")

(defvar-local carriage-mode-provider nil
  "Current LLM provider slug for the backend (e.g., \"ai-tunnel\" for gptel), or nil.")

(defvar-local carriage--mode-prev-header-line-format nil
  "Saved previous value of =header-line-format' to restore on mode disable.")

(defvar-local carriage--mode-modeline-construct nil
  "The exact modeline construct object inserted by Carriage for later removal.")

(defvar-local carriage--mode-prev-mode-line-format nil
  "Saved previous value of `mode-line-format' to restore on mode disable.")

(defvar-local carriage--mode-prev-mode-line-format-was-local nil
  "Non-nil when `mode-line-format' was buffer-local before enabling `carriage-mode'.")

(defvar-local carriage--mode-prev-mode-line-format-saved nil
  "Non-nil when previous `mode-line-format' snapshot was captured for this buffer.")

(defvar-local carriage--abort-handler nil
  "Buffer-local abort handler function for the current Carriage activity, or nil.

The handler should be a zero-argument function that cancels the ongoing request or apply.
Set by transports/pipelines when starting an async activity; cleared on completion or when disabling carriage-mode.")

(defvar-local carriage--mode-emulation-map nil
  "Per-buffer emulation keymap used to provide Carriage prefix bindings without
populating =carriage-mode-map' with a real prefix (satisfies tests that expect
(bare) C-c e to be unbound in the mode map when transient=nil).")

(defvar-local carriage--emulation-map-alist nil
  "Buffer-local alist for emulation-mode-map-alists, mapping =carriage-mode' to
a per-buffer emulation keymap. This lets us provide prefix sequences in buffers
without turning =carriage-mode-map' into a real prefix.")

(defvar-local carriage--prev-local-function-key-map nil
  "Previous value of local-function-key-map before Carriage tweaks (if any).")

(defvar-local carriage--translation-map nil
  "Local translation keymap we may install when transient=t (reserved; may be nil).")

(defvar carriage-mode-map (make-sparse-keymap)
  "Keymap for carriage-mode.
Do not define bindings here; all key bindings are applied via keyspec and mode setup.")

;; Ensure menu command autoload is available even if keyspec isn't loaded yet.
;; This allows binding C-c e to open the menu immediately in carriage-mode buffers.
(autoload 'carriage-keys-open-menu "carriage-keyspec" "Open Carriage action menu from keyspec." t)
(autoload 'carriage-context-plan-compile-at-point "carriage-context-plan"
  "Compile begin_context_plan at point into begin_context." t)

;; Capability probe for current engine (parity with dispatcher)
(defun carriage--engine-supports-op-p (op)
  "Return non-nil when the active apply engine supports OP.
Consults engine capabilities; safe when registry is not yet loaded."
  (condition-case _e
      (let* ((eng (and (fboundp 'carriage-apply-engine)
                       (carriage-apply-engine)))
             (rec (and (fboundp 'carriage-apply-engine--get)
                       (carriage-apply-engine--get eng)))
             (capf (and (listp rec) (plist-get rec :capabilities)))
             (caps (and (functionp capf) (ignore-errors (funcall capf op))))
             (ops  (and (listp caps) (plist-get caps :ops))))
        (and ops (memq op ops)))
    (error nil)))

;;; Internal helpers split out of define-minor-mode

(defun carriage-mode--init-state ()
  "Preflight checks and buffer-local state initialization for Carriage."
  (unless (derived-mode-p 'org-mode)
    (if (bound-and-true-p noninteractive)
        (carriage-log "carriage-mode enabled outside org-mode (batch); limited UI")
      (progn
        (setq carriage-mode nil)
        (user-error "carriage-mode работает только в org-mode"))))
  (let ((root (carriage-project-root)))
    (unless root
      (carriage-log "carriage-mode: Git repository not detected; continuing with limited features")))
  (setq carriage-mode-intent carriage-mode-default-intent)
  (setq carriage-mode-suite  carriage-mode-default-suite)
  (setq carriage-mode-model carriage-mode-default-model)
  (setq carriage-mode-backend carriage-mode-default-backend)
  (setq carriage-mode-provider nil)
  ;; Ensure default backend:model is present in registry for completion.
  (when (require 'carriage-llm-registry nil t)
    (let* ((bsym (if (symbolp carriage-mode-backend)
                     carriage-mode-backend
                   (intern (format "%s" carriage-mode-backend))))
           (backs (carriage-llm-available-backends)))
      (unless (member (symbol-name bsym) backs)
        (carriage-llm-register-backend bsym :models (list carriage-mode-model))))))

(defun carriage-mode--modeline--as-list (fmt)
  "Normalize mode-line format FMT to a list."
  (cond
   ((null fmt) '())
   ((listp fmt) (copy-sequence fmt))
   (t (list fmt))))

(defun carriage-mode--modeline--insert-visible (ml construct)
  "Return ML with CONSTRUCT inserted at a likely-visible position.

Insertion order (first available wins):
- before `mode-line-end-spaces'
- before `mode-line-modes'
- before `mode-line-position'
- before `mode-line-misc-info'
Fallback:
- after `mode-line-front-space' when present
- otherwise at the beginning."
  (let* ((anchors '(mode-line-end-spaces
                    mode-line-modes
                    mode-line-position
                    mode-line-misc-info))
         (pos (cl-loop for a in anchors
                       for p = (cl-position a ml)
                       when (numberp p) return p)))
    (cond
     ((numberp pos)
      (append (cl-subseq ml 0 pos)
              (list construct)
              (nthcdr pos ml)))
     (t
      (let ((front (cl-position 'mode-line-front-space ml)))
        (if (numberp front)
            (let ((i (1+ front)))
              (append (cl-subseq ml 0 i)
                      (list construct)
                      (nthcdr i ml)))
          (cons construct ml)))))))

(defun carriage-mode--modeline-ensure-once (&optional buffer)
  "Ensure Carriage modeline fully replaces BUFFER's `mode-line-format'.

This is intentionally a one-shot, lightweight helper (no periodic watchdogs).

Policy:
- In carriage-mode buffers, Carriage modeline must be the ONLY mode-line.
- We still save/restore the previous `mode-line-format' on enable/disable."
  (with-current-buffer (or buffer (current-buffer))
    (when (and (bound-and-true-p carriage-mode)
               (boundp 'carriage-mode-show-mode-line-ui)
               carriage-mode-show-mode-line-ui)
      (unless carriage--mode-modeline-construct
        (setq carriage--mode-modeline-construct '(:eval (carriage-ui--modeline))))
      ;; Replace mode-line completely (do not append/insert into existing global mode-line).
      (setq-local mode-line-format (list carriage--mode-modeline-construct))
      (force-mode-line-update t))))

(defun carriage-mode--install-headerline ()
  "Install Carriage header-line (buffer-local)."
  (when carriage-mode-show-header-line
    (setq carriage--mode-prev-header-line-format header-line-format)
    (setq-local header-line-format '(:eval (carriage-ui--header-line-for (selected-window))))
    (add-hook 'post-command-hook #'carriage-ui--headerline-post-command nil t)
    (add-hook 'window-scroll-functions #'carriage-ui--headerline-window-scroll nil t)))

(defun carriage-mode--install-modeline ()
  "Replace `mode-line-format' with Carriage modeline (buffer-local) and save snapshot.

Policy (requested UX):
- In carriage-mode buffers, Carriage modeline must *fully replace* the existing mode-line.
- The previous `mode-line-format' is saved and restored on mode disable."
  (unless carriage--mode-prev-mode-line-format-saved
    (setq carriage--mode-prev-mode-line-format mode-line-format
          carriage--mode-prev-mode-line-format-was-local (local-variable-p 'mode-line-format)
          carriage--mode-prev-mode-line-format-saved t))
  (setq carriage--mode-modeline-construct '(:eval (carriage-ui--modeline)))
  ;; Replace mode-line completely (do not append/insert into existing global mode-line).
  (setq-local mode-line-format (list carriage--mode-modeline-construct))
  (force-mode-line-update))

(defun carriage-mode--ctx-window-change ()
  "Window configuration change hook to refresh [Ctx:N] when external context may change.

External means:
- visible-context (depends on window layout)
- gptel-context   (can change without touching the Org buffer)

Debounced refresh is handled by `carriage-ui--ctx-schedule-refresh`."
  (when (and (bound-and-true-p carriage-mode)
             (or (and (boundp 'carriage-mode-include-visible-context)
                      carriage-mode-include-visible-context)
                 (and (boundp 'carriage-mode-include-gptel-context)
                      carriage-mode-include-gptel-context))
             (fboundp 'carriage-ui--ctx-invalidate))
    ;; Invalidate cached badge (preserves last value), and schedule a refresh soon.
    (ignore-errors (carriage-ui--ctx-invalidate))))

(defun carriage-mode--install-counters-hooks ()
  "Install modeline counters (patch/context) maintenance hooks."
  ;; Patch-count is expensive to compute by scanning; keep it off the redisplay path.
  ;; Maintain the cache asynchronously on edits, but only when patch counter is enabled.
  (when (and (derived-mode-p 'org-mode)
             (boundp 'carriage-ui-modeline-blocks)
             (memq 'patch carriage-ui-modeline-blocks)
             (fboundp 'carriage-ui--patch-after-change))
    (add-hook 'after-change-functions #'carriage-ui--patch-after-change nil t)
    (when (fboundp 'carriage-ui--patch-schedule-refresh)
      (ignore-errors (carriage-ui--patch-schedule-refresh 0.05))))
  ;; Keep [Ctx:N] badge up-to-date on document edits (debounced, non-blocking).
  ;; IMPORTANT: no context computation on redisplay; only schedule a refresh timer.
  (when (and (boundp 'carriage-ui-modeline-blocks)
             (memq 'context carriage-ui-modeline-blocks)
             (fboundp 'carriage-ui--ctx-after-change))
    (add-hook 'after-change-functions #'carriage-ui--ctx-after-change nil t)
    ;; Save/revert are strong signals that doc paths / file sizes may have changed.
    (when (fboundp 'carriage-ui--after-save-refresh)
      (add-hook 'after-save-hook #'carriage-ui--after-save-refresh nil t)
      (add-hook 'after-revert-hook #'carriage-ui--after-save-refresh nil t))
    ;; Visible-context depends on window layout; refresh on window config changes.
    (add-hook 'window-configuration-change-hook #'carriage-mode--ctx-window-change nil t))
  ;; Document cost badge: compute asynchronously once on mode enable so existing
  ;; fingerprints are reflected in the modeline without waiting for a new request.
  (when (and (boundp 'carriage-ui-modeline-blocks)
             (memq 'doc-cost carriage-ui-modeline-blocks)
             (fboundp 'carriage-ui-doc-cost-schedule-refresh))
    (ignore-errors (carriage-ui-doc-cost-schedule-refresh 0.05))))

(defun carriage--point-in-org-block-p (begin-re end-re)
  "Return non-nil when point is inside an Org block defined by BEGIN-RE/END-RE.
BEGIN-RE and END-RE are regexps matching marker lines (BOL-anchored recommended)."
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search t)
            (pt (point))
            beg end)
        (when (re-search-backward begin-re nil t)
          (setq beg (match-beginning 0))
          (goto-char beg)
          (when (re-search-forward end-re nil t)
            (setq end (match-end 0))
            (and (numberp beg) (numberp end)
                 (>= pt beg) (<= pt end))))))))

(defun carriage-ctrl-c-ctrl-c ()
  "Context-sensitive handler for C-c C-c in carriage-mode buffers.

Routing:
- Inside begin_context_plan → compile it to begin_context.
- Inside begin_patch        → apply at point or region.
- Otherwise                 → delegate to Org's `org-ctrl-c-ctrl-c`."
  (interactive)
  (cond
   ;; 1) Context plan compilation
   ((carriage--point-in-org-block-p
     "^[ \t]*#\\+begin_context_plan\\b"
     "^[ \t]*#\\+end_context_plan\\b")
    (if (fboundp 'carriage-context-plan-compile-at-point)
        (call-interactively #'carriage-context-plan-compile-at-point)
      (user-error "carriage-context-plan not available")))
   ;; 2) Patch apply (legacy convenience; keeps prior semantics)
   ((carriage--point-in-org-block-p
     "^[ \t]*#\\+begin_patch\\b"
     "^[ \t]*#\\+end_patch\\b")
    (if (fboundp 'carriage-apply-at-point-or-region)
        (call-interactively #'carriage-apply-at-point-or-region)
      (user-error "carriage apply commands are not available")))
   ;; 3) Delegate to Org
   (t
    (require 'org)
    (call-interactively #'org-ctrl-c-ctrl-c))))

(defun carriage-mode--setup-keys-and-panels ()
  "Apply keyspec keymaps, define bindings, and open optional panels."
  (when (require 'carriage-keyspec nil t)
    (carriage-keys-apply-known-keymaps)
    (let ((prefixes (carriage-keys-prefixes)))
      (dolist (pref prefixes)
        (define-key carriage-mode-map (kbd pref) #'carriage-keys-open-menu)))
    (setq carriage--mode-emulation-map nil
          carriage--emulation-map-alist nil)
    ;; Legacy bindings:
    ;; - C-c C-c → context router (plan→compile, patch→apply, else Org)
    ;; - C-c !   → apply last iteration (override org-time-stamp in carriage-mode buffers)
    (define-key carriage-mode-map (kbd "C-c !") #'carriage-apply-last-iteration)
    ;; Always bind in carriage-mode buffers (REQ-ctxplan-013).
    (define-key carriage-mode-map (kbd "C-c C-c") #'carriage-ctrl-c-ctrl-c)
    (when (and carriage-mode-auto-open-log (fboundp 'carriage-show-log))
      (ignore-errors (carriage-show-log)))
    (when (and carriage-mode-auto-open-traffic (fboundp 'carriage-show-traffic))
      (ignore-errors (carriage-show-traffic)))
    (carriage-log "carriage-mode enabled in %s" (buffer-name))))

(defun carriage-mode--fold-ui-enable ()
  "Enable UI folds for applied patches and reasoning blocks in this buffer."
  ;; Folding UI must not depend on keyspec being present/loaded.
  ;; Fold applied patch blocks if enabled
  (when (and (boundp 'carriage-mode-hide-applied-patches)
             carriage-mode-hide-applied-patches
             (require 'carriage-patch-fold nil t))
    (ignore-errors (carriage-patch-fold-enable)))
  ;; Fold all reasoning blocks on mode enable (and keep newly streamed ones folded).
  (when (and (boundp 'carriage-mode-hide-reasoning-blocks)
             carriage-mode-hide-reasoning-blocks
             (require 'carriage-reasoning-fold nil t))
    (ignore-errors (carriage-reasoning-fold-enable))
    (ignore-errors (carriage-reasoning-fold-hide-all (current-buffer)))
    (ignore-errors (carriage--reasoning-fold--sanitize-all (current-buffer)))
    ;; Reveal original reasoning text when point enters a folded block.
    (add-hook 'post-command-hook #'carriage--reasoning-fold--post-command nil t)))

(defun carriage-mode--init-ui ()
  "Install header/modeline and key bindings; open optional panels."
  ;; UI (buffer-local, no global effects); respect batch/noninteractive
  (unless (bound-and-true-p noninteractive)
    (carriage-mode--install-headerline)
    (when carriage-mode-show-mode-line-ui
      (carriage-mode--install-modeline))
    (carriage-mode--install-counters-hooks)
    (carriage-mode--setup-keys-and-panels)
    (carriage-mode--fold-ui-enable)))

(defun carriage-mode--enable ()
  "Enable Carriage mode in the current buffer (internal)."
  (carriage-mode--init-state)
  (carriage-mode--init-ui)

  ;; Defensive: remove any legacy/third-party advice that can block or delay
  ;; `carriage-insert-stream-chunk' (symptom: stream appears only after keypress).
  ;; This must be best-effort and never signal.
  (ignore-errors
    (when (and (fboundp 'carriage--stream-chunk-save-point)
               (advice-member-p #'carriage--stream-chunk-save-point 'carriage-insert-stream-chunk))
      (advice-remove 'carriage-insert-stream-chunk #'carriage--stream-chunk-save-point)))

  ;; One-shot deferred ensure: survive late `setq-local mode-line-format' rewrites during file open.
  ;; No watchdogs, no post-command hooks, no variable watchers.
  (when (fboundp 'carriage-mode--modeline-ensure-once)
    (let ((buf (current-buffer)))
      (run-at-time
       0 nil
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (bound-and-true-p carriage-mode)
               (ignore-errors (carriage-mode--modeline-ensure-once buf)))))))))

  ;; Warm up common modules on idle to avoid cold-start delays in first send.
  (run-at-time 0.2 nil
               (lambda ()
                 (ignore-errors (require 'carriage-intent-registry nil t))
                 (ignore-errors (require 'carriage-op-sre nil t))
                 (ignore-errors (require 'carriage-op-aibo nil t))
                 (ignore-errors (require 'carriage-op-patch nil t))
                 (ignore-errors (require 'carriage-op-file nil t))
                 ;; Context badge is handled by carriage-ui + carriage-context-count (fast, no file reads).
                 ))
  ;; Restore state from document, persist snapshot, then fold the block; install save hook.
  (when (require 'carriage-doc-state nil t)
    (ignore-errors (carriage-doc-state-restore))
    (when (and (boundp 'carriage-doc-state-sync-on-change)
               carriage-doc-state-sync-on-change)
      (ignore-errors (carriage-doc-state-write-current)))
    ;; Enable modern display-based fold UI and refresh immediately (no legacy invisibility).
    (ignore-errors (carriage-doc-state-summary-enable))
    (ignore-errors (carriage-doc-state-summary-refresh))
    (ignore-errors
      (when (and (boundp 'carriage-doc-state-save-on-save)
                 carriage-doc-state-save-on-save)
        (carriage-doc-state-install-save-hook)))))

(defun carriage-mode--disable ()
  "Disable Carriage mode in the current buffer (internal)."
  ;; Optionally reflect mode off in the document (soft; no autosave).
  (when (require 'carriage-doc-state nil t)
    (ignore-errors
      (let* ((pl (carriage-doc-state-read))
             (pl2 (plist-put (or pl '()) :CAR_MODE nil)))
        (carriage-doc-state-write pl2))))
  ;; Disable: restore header-line and remove modeline segment (buffer-local)
  (unless (bound-and-true-p noninteractive)
    (when (local-variable-p 'header-line-format)
      (setq-local header-line-format carriage--mode-prev-header-line-format))
    (setq carriage--mode-prev-header-line-format nil)
    (remove-hook 'post-command-hook #'carriage-ui--headerline-post-command t)
    (remove-hook 'window-scroll-functions #'carriage-ui--headerline-window-scroll t)
    (when (and (boundp 'carriage-ui--headerline-idle-timer)
               (timerp carriage-ui--headerline-idle-timer))
      (cancel-timer carriage-ui--headerline-idle-timer))
    (setq carriage-ui--headerline-idle-timer nil)
    ;; Remove any legacy fallback we might have inserted earlier.
    (when (local-variable-p 'global-mode-string)
      (setq-local global-mode-string
                  (delq carriage--mode-modeline-construct global-mode-string)))

    ;; Restore mode-line-format exactly to what it was before enabling Carriage.
    (when carriage--mode-prev-mode-line-format-saved
      (if carriage--mode-prev-mode-line-format-was-local
          (setq-local mode-line-format carriage--mode-prev-mode-line-format)
        (kill-local-variable 'mode-line-format))
      (setq carriage--mode-prev-mode-line-format nil
            carriage--mode-prev-mode-line-format-was-local nil
            carriage--mode-prev-mode-line-format-saved nil))

    (setq carriage--mode-modeline-construct nil)
    ;; Clear abort handler and stop spinner if running
    (setq carriage--abort-handler nil)
    (when (fboundp 'carriage-ui--spinner-stop)
      (carriage-ui--spinner-stop t))
    (when (fboundp 'carriage--preloader-stop)
      (carriage--preloader-stop))
    ;; Remove per-buffer emulation maps we installed for this buffer
    (when (local-variable-p 'emulation-mode-map-alists)
      (let ((lst emulation-mode-map-alists))
        (setq-local emulation-mode-map-alists
                    (delq carriage--emulation-map-alist lst))))
    (setq carriage--emulation-map-alist nil)
    (setq carriage--mode-emulation-map nil)
    (force-mode-line-update t))
  (when (featurep 'carriage-patch-fold)
    (ignore-errors (carriage-patch-fold-disable)))
  ;; Remove reveal hook and restore any temporarily revealed overlays.
  (remove-hook 'post-command-hook #'carriage--reasoning-fold--post-command t)
  (when (listp carriage--reasoning-fold--revealed-ovs)
    (dolist (ov carriage--reasoning-fold--revealed-ovs)
      (ignore-errors (carriage--reasoning-fold--restore ov))))
  (setq carriage--reasoning-fold--revealed-ovs nil)

  (when (featurep 'carriage-reasoning-fold)
    (ignore-errors (carriage-reasoning-fold-disable)))
  (carriage-log "carriage-mode disabled in %s" (buffer-name)))

;;;###autoload
(define-minor-mode carriage-mode
  "Toggle Carriage minor mode for working with patch blocks in org buffers."
  :lighter (:eval (concat " Carriage" (carriage-ui-state-lighter)))
  :keymap carriage-mode-map
  :group 'carriage
  (if carriage-mode
      (carriage-mode--enable)
    (carriage-mode--disable)))

;; Streaming insertion state and helpers

;; Group all streaming edits into a single undo step using change groups.
(defvar-local carriage--undo-change-group nil
  "Handle of the active change group for streaming, or nil.")

(defun carriage--undo-group-start ()
  "Start a change group for streaming if supported and not already active."
  (when (and (fboundp 'prepare-change-group)
             (fboundp 'activate-change-group)
             (null carriage--undo-change-group))
    (let ((cg (prepare-change-group)))
      (setq carriage--undo-change-group cg)
      (activate-change-group cg)
      cg)))

(defun carriage--undo-group-accept ()
  "Accept the active change group for streaming and insert a boundary."
  (when carriage--undo-change-group
    (when (fboundp 'accept-change-group)
      (accept-change-group carriage--undo-change-group))
    (setq carriage--undo-change-group nil)
    (when (fboundp 'undo-boundary)
      (ignore-errors (undo-boundary)))
    t))

(defun carriage--undo-group-cancel ()
  "Cancel the active change group for streaming."
  (when carriage--undo-change-group
    (when (fboundp 'cancel-change-group)
      (cancel-change-group carriage--undo-change-group))
    (setq carriage--undo-change-group nil)
    t))

(defun carriage--undo-group-on-abort ()
  "Finalize change group on abort according to policy."
  (pcase (and (boundp 'carriage-mode-stream-undo-on-abort)
              carriage-mode-stream-undo-on-abort)
    ('drop (carriage--undo-group-cancel))
    (_     (carriage--undo-group-accept))))

(defcustom carriage-mode-stream-undo-on-abort 'keep
  "Policy for undo change group on abort: 'keep (accept accumulated changes) or 'drop (cancel them)."
  :type '(choice (const keep) (const drop))
  :group 'carriage)

(defcustom carriage-mode-reasoning-log-verbose nil
  "When non-nil, emit verbose reasoning begin/end logs to *carriage-log*."
  :type 'boolean :group 'carriage)

;; Preloader (buffer-local spinner at insertion point before first stream chunk)

(defcustom carriage-mode-preloader-enabled t
  "When non-nil, show a lightweight preloader spinner at the insertion point before streaming starts."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-preloader-interval 0.2
  "Update interval, in seconds, for the buffer preloader spinner."
  :type 'number :group 'carriage)

(defcustom carriage-mode-preloader-face 'mode-line-emphasis
  "Face used to render the in-buffer preloader spinner.
Choose a visible face for your theme; 'shadow can be too dim."
  :type 'face :group 'carriage)

(defcustom carriage-mode-preloader-window-local nil
  "When non-nil, show the spinner only in the window where streaming started."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-preloader-follow-point nil
  "When non-nil, keep point after the spinner during streaming by moving it to the stream tail on each chunk."
  :type 'boolean :group 'carriage)

(defconst carriage--preloader-frames-unicode
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Spinner frames for the buffer preloader (Unicode).")

(defconst carriage--preloader-frames-ascii
  ["." ".." "..."]
  "Spinner frames for the buffer preloader (ASCII fallback).")

(defvar-local carriage--preloader-overlay nil
  "Overlay used to render the buffer preloader spinner.")

(defvar-local carriage--preloader-timer nil
  "Timer updating the buffer preloader spinner.")

(defvar-local carriage--preloader-index 0
  "Current frame index for the buffer preloader spinner.")

(defun carriage--preloader-frames ()
  "Return vector of frames appropriate for current display."
  (if (display-graphic-p)
      carriage--preloader-frames-unicode
    carriage--preloader-frames-ascii))

(defun carriage--preloader--render (pos)
  "Render preloader at POS, updating overlay text."
  ;; Ensure overlay variable is bound and overlay exists for this buffer.
  (unless (and (boundp 'carriage--preloader-overlay)
               (overlayp carriage--preloader-overlay))
    (setq carriage--preloader-overlay (make-overlay pos pos)))
  (let* ((frames (carriage--preloader-frames))
         (n (length frames))
         (i (mod (or carriage--preloader-index 0) (max 1 n)))
         (frame (aref frames i)))
    (when (overlayp carriage--preloader-overlay)
      (overlay-put carriage--preloader-overlay 'after-string nil)
      (overlay-put carriage--preloader-overlay 'before-string
                   (propertize (concat frame "\n") 'face carriage-mode-preloader-face))
      ;; Refresh a visible window showing this buffer; avoid repainting all windows.
      (let ((w (get-buffer-window (current-buffer) t)))
        (when (window-live-p w)
          (force-window-update w))))
    (setq carriage--preloader-index (1+ i))))

(defun carriage--preloader-start ()
  "Start buffer preloader at the origin of the current stream region."
  (when carriage-mode-preloader-enabled
    ;; Hard reset any stale state so spinner always (re)appears.
    (when (timerp carriage--preloader-timer)
      (cancel-timer carriage--preloader-timer))
    (setq carriage--preloader-timer nil)
    (when (overlayp carriage--preloader-overlay)
      (delete-overlay carriage--preloader-overlay))
    (setq carriage--preloader-overlay nil)
    (let* ((pos (cond
                 ((and (markerp carriage--stream-origin-marker)
                       (buffer-live-p (marker-buffer carriage--stream-origin-marker)))
                  (marker-position carriage--stream-origin-marker))
                 (t (point))))
           (interval (or carriage-mode-preloader-interval 0.2))
           (buf (current-buffer))
           (timer nil))
      (setq carriage--preloader-index 0)
      ;; Render first frame immediately so spinner is visible right away.
      (carriage--preloader--render pos)
      ;; Ensure high priority so spinner is not obscured by other overlays.
      (when (overlayp carriage--preloader-overlay)
        (overlay-put carriage--preloader-overlay 'priority 1001)
        (when (and (boundp 'carriage-mode-preloader-window-local)
                   carriage-mode-preloader-window-local)
          (overlay-put carriage--preloader-overlay 'window (selected-window))))

      (setq timer
            (run-at-time
             0 interval
             (lambda ()
               (if (not (buffer-live-p buf))
                   (when (timerp timer)
                     (cancel-timer timer)
                     (setq timer nil))
                 (with-current-buffer buf
                   (let* ((ov carriage--preloader-overlay)
                          (ob (and (overlayp ov) (overlay-buffer ov))))
                     (when (and (overlayp ov)
                                (buffer-live-p ob)
                                (get-buffer-window ob t))
                       (carriage--preloader--render
                        (or (overlay-start ov) pos)))))))))
      (setq carriage--preloader-timer timer))))

(defun carriage--preloader-stop ()
  "Stop and remove buffer preloader spinner if active."
  (when (timerp carriage--preloader-timer)
    (cancel-timer carriage--preloader-timer))
  (setq carriage--preloader-timer nil)
  (when (overlayp carriage--preloader-overlay)
    (delete-overlay carriage--preloader-overlay))
  (setq carriage--preloader-overlay nil)
  (setq carriage--preloader-index 0))

(defvar-local carriage--stream-beg-marker nil
  "Buffer-local begin marker of the current streaming region.")
(defvar-local carriage--stream-end-marker nil
  "Buffer-local end marker of the current streaming region.")
(defvar-local carriage--stream-origin-marker nil
  "Buffer-local origin marker set at request time; first chunk will use it.")

;; Streaming performance: coalesce small chunks into a single insert using a per-buffer flush timer.

(defcustom carriage-stream-flush-interval 0.03
  "Interval (seconds) to coalesce incoming stream chunks before inserting into the buffer.

Smaller values make streaming look more real-time but increase buffer edit frequency.
Larger values reduce overhead and make streaming smoother under load."
  :type 'number
  :group 'carriage)

(defcustom carriage-stream-flush-bytes-threshold 8192
  "Flush pending stream queue early when accumulated bytes reach this threshold.
Set to nil to disable early flush by size."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'carriage)

(defcustom carriage-stream-redisplay-interval 0.05
  "Minimum seconds between forced redisplay calls triggered by streaming flush.

Goal: make streamed text visible even without user input (no keypress), including
when the buffer is visible but not selected."
  :type 'number
  :group 'carriage)

(defvar-local carriage--stream-pending-events nil
  "Pending streamed events as a reversed list of (TYPE . STRING).

TYPE is either 'text or 'reasoning.
The queue is flushed by a timer to reduce buffer modification overhead.")

(defvar-local carriage--stream-pending-bytes 0
  "Approximate total bytes of enqueued streamed chunks waiting to be flushed.")

(defvar-local carriage--stream-flush-timer nil
  "Timer object for scheduled streaming flush in this buffer.")

(defvar-local carriage--stream-last-redisplay 0.0
  "Last time (float seconds) when we forced redisplay due to a streaming flush in this buffer.")

(defun carriage--stream--normalize-type (type)
  "Normalize TYPE to either 'text or 'reasoning."
  (pcase type
    ((or 'reasoning :reasoning) 'reasoning)
    (_ 'text)))

(defun carriage--stream--coalesce-events (events)
  "Coalesce EVENTS (a list of (TYPE . STRING)) into a minimal list.

Adjacent events with the same TYPE are merged to reduce buffer operations."
  (let ((acc nil)
        (cur-type nil)
        (cur-parts nil))
    (dolist (ev events)
      (let ((ty (car ev))
            (s  (cdr ev)))
        (if (eq ty cur-type)
            (push s cur-parts)
          (when cur-type
            (push (cons cur-type (apply #'concat (nreverse cur-parts))) acc))
          (setq cur-type ty)
          (setq cur-parts (list s)))))
    (when cur-type
      (push (cons cur-type (apply #'concat (nreverse cur-parts))) acc))
    (nreverse acc)))

(defun carriage--stream--buffer-visible-p (&optional buffer)
  "Return non-nil when BUFFER (or current buffer) is visible in any window."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (get-buffer-window-list buf t t))))

(defun carriage--stream--maybe-redisplay (&optional buffer)
  "Force a throttled redisplay when BUFFER is visible in any window."
  (let* ((buf (or buffer (current-buffer)))
         (wins (carriage--stream--buffer-visible-p buf))
         (now (float-time))
         (min (max 0.0 (or carriage-stream-redisplay-interval 0.05)))
         (last (or (and (boundp 'carriage--stream-last-redisplay)
                        (buffer-local-value 'carriage--stream-last-redisplay buf))
                   0.0)))
    (when (and wins
               (not (bound-and-true-p noninteractive))
               (>= (- now last) min))
      (with-current-buffer buf
        (setq carriage--stream-last-redisplay now))
      (redisplay t))))

(defun carriage--stream--flush-now (&optional buffer)
  "Flush any pending streamed events into BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (when (timerp carriage--stream-flush-timer)
      (cancel-timer carriage--stream-flush-timer))
    (setq carriage--stream-flush-timer nil)
    (let* ((pending (nreverse (or carriage--stream-pending-events '()))))
      (setq carriage--stream-pending-events nil)
      (setq carriage--stream-pending-bytes 0)
      (when pending
        (carriage--undo-group-start)
        (let ((events (carriage--stream--coalesce-events pending)))
          (dolist (ev events)
            (pcase (car ev)
              ('reasoning
               (when (eq carriage-mode-include-reasoning 'block)
                 (carriage-begin-reasoning)
                 (let ((inhibit-read-only t)
                       (s (or (cdr ev) "")))
                   (save-excursion
                     (let* ((tail (or carriage--reasoning-tail-marker carriage--stream-end-marker))
                            (tailpos (marker-position tail)))
                       (goto-char tailpos)
                       (insert s)
                       (let ((newpos (point)))
                         ;; Advance tail; if end==tail, advance end as well
                         (when (markerp carriage--reasoning-tail-marker)
                           (set-marker carriage--reasoning-tail-marker newpos (current-buffer)))
                         (when (and (markerp carriage--stream-end-marker)
                                    (= (marker-position carriage--stream-end-marker) tailpos))
                           (set-marker carriage--stream-end-marker newpos (current-buffer)))))))))
              (_
               (carriage--stream-insert-at-end (or (cdr ev) ""))))))
        ;; Move preloader overlay to stream tail (single update per flush)
        (when (and (boundp 'carriage--preloader-overlay)
                   (overlayp carriage--preloader-overlay)
                   (markerp carriage--stream-end-marker))
          (let ((endpos (marker-position carriage--stream-end-marker)))
            (move-overlay carriage--preloader-overlay endpos endpos)
            (when (and (boundp 'carriage-mode-preloader-follow-point)
                       carriage-mode-preloader-follow-point)
              (goto-char endpos))))
        ;; Ensure visible output even without user input (throttled)
        (carriage--stream--maybe-redisplay (current-buffer))
        t))))

(defun carriage-stream-flush-now (&optional buffer)
  "Public: flush pending streamed chunks into BUFFER (or current buffer).
Best-effort; never signals."
  (interactive)
  (ignore-errors (carriage--stream--flush-now buffer)))

(defun carriage--stream--schedule-flush (&optional delay)
  "Schedule a one-shot flush of pending stream events after DELAY seconds."
  (unless (timerp carriage--stream-flush-timer)
    (let* ((buf (current-buffer))
           (d (max 0.0 (float (or delay (or carriage-stream-flush-interval 0.03))))))
      (setq carriage--stream-flush-timer
            (run-at-time
             d nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq carriage--stream-flush-timer nil)
                   (ignore-errors (carriage--stream--flush-now buf))))))))))

(defun carriage--stream-enqueue (type s)
  "Enqueue streamed chunk S of TYPE ('text or 'reasoning) and schedule flush."
  (let* ((ty (carriage--stream--normalize-type type))
         (str (or s "")))
    (push (cons ty str) carriage--stream-pending-events)
    (setq carriage--stream-pending-bytes
          (+ (or carriage--stream-pending-bytes 0) (string-bytes str)))
    ;; Early flush by size threshold (schedule ASAP; do not flush inline by default).
    (when (and carriage-stream-flush-bytes-threshold
               (numberp carriage-stream-flush-bytes-threshold)
               (>= carriage--stream-pending-bytes carriage-stream-flush-bytes-threshold))
      (when (timerp carriage--stream-flush-timer)
        (cancel-timer carriage--stream-flush-timer)
        (setq carriage--stream-flush-timer nil))
      (carriage--stream--schedule-flush 0.0))
    ;; Ensure we have at least one flush scheduled.
    (carriage--stream--schedule-flush)))

(defvar-local carriage--reasoning-open nil
  "Non-nil when a #+begin_reasoning block is currently open for streaming.")
(defvar-local carriage--reasoning-tail-marker nil
  "Marker pointing to the end of reasoning content (where #+end_reasoning should be inserted).")
(defvar-local carriage--reasoning-beg-marker nil
  "Marker pointing to the beginning line of the current #+begin_reasoning block.")

(defvar-local carriage--iteration-inline-marker-inserted nil
  "Non-nil when an inline iteration marker has been inserted for the current stream.")

(defun carriage-insert-inline-iteration-marker-now ()
  "Insert inline iteration marker immediately at the current position.

Uses stream origin when available.
Insertion never splits a line: marker is inserted at the beginning of the target line.
Also adjusts stream origin to the line immediately after the marker so the preloader
and streamed content start strictly below it.

Policy: after insertion, move point to the new stream origin (preloader line).

Returns non-nil when inserted."
  (interactive)
  (when (and (not carriage--iteration-inline-marker-inserted)
             (boundp 'carriage--last-iteration-id)
             (stringp carriage--last-iteration-id)
             (> (length (string-trim carriage--last-iteration-id)) 0))
    (let* ((pos (cond
                 ((and (markerp carriage--stream-origin-marker)
                       (buffer-live-p (marker-buffer carriage--stream-origin-marker)))
                  (marker-position carriage--stream-origin-marker))
                 ((and (markerp carriage--stream-beg-marker)
                       (buffer-live-p (marker-buffer carriage--stream-beg-marker)))
                  (marker-position carriage--stream-beg-marker))
                 (t (point))))
           (endpos (ignore-errors
                     (carriage-iteration--write-inline-marker pos carriage--last-iteration-id))))
      (when (numberp endpos)
        ;; Stream (and preloader) must start strictly after the marker.
        (setq carriage--stream-origin-marker (copy-marker endpos t))
        ;; Put cursor where the preloader will render (policy requirement).
        (goto-char endpos))
      (setq carriage--iteration-inline-marker-inserted t)
      (and (numberp endpos) t))))

(defun carriage-stream-reset (&optional origin-marker)
  "Reset streaming state for current buffer and set ORIGIN-MARKER if provided.
Does not modify buffer text; only clears markers/state so the next chunk opens a region."
  (setq carriage--stream-beg-marker nil)
  (setq carriage--stream-end-marker nil)
  ;; Streaming perf: clear pending coalesced events and cancel flush timer.
  (setq carriage--stream-pending-events nil)
  (when (timerp carriage--stream-flush-timer)
    (cancel-timer carriage--stream-flush-timer))
  (setq carriage--stream-flush-timer nil)
  (setq carriage--stream-last-redisplay 0.0)
  ;; Reset Apply badge when starting a new request: Apply should reappear only after the next apply attempt.
  ;; Best-effort: avoid errors when UI is not loaded.
  (when (fboundp 'carriage-ui-apply-reset)
    (ignore-errors (carriage-ui-apply-reset)))
  ;; Always pin origin to the cursor position at the moment of reset.
  ;; Never reuse an old marker object: stale origin makes preloader “drift” upward.
  (setq carriage--stream-origin-marker
        (cond
         ((and (markerp origin-marker)
               (buffer-live-p (marker-buffer origin-marker)))
          (copy-marker (marker-position origin-marker) t))
         (t
          (copy-marker (point) t))))
  (setq carriage--reasoning-open nil)
  (setq carriage--reasoning-tail-marker nil)
  (setq carriage--reasoning-beg-marker nil)
  (setq carriage--iteration-inline-marker-inserted nil)
  (setq carriage--fingerprint-inline-inserted nil)
  (setq carriage--separator-inserted nil)
  ;; O(1) modeline support: reset last-iteration detection state.
  (setq carriage--last-iteration-has-patches nil)
  (setq carriage--stream-detect-tail "")
  (carriage--undo-group-start)
  t)

(defun carriage-stream-region ()
  "Return (BEG . END) of current streaming region in this buffer, or nil."
  (when (and (markerp carriage--stream-beg-marker)
             (markerp carriage--stream-end-marker)
             (eq (marker-buffer carriage--stream-beg-marker) (current-buffer))
             (eq (marker-buffer carriage--stream-end-marker) (current-buffer)))
    (cons (marker-position carriage--stream-beg-marker)
          (marker-position carriage--stream-end-marker))))

(defun carriage--ensure-stream-region ()
  "Ensure streaming region exists. Use origin marker if set; otherwise current point.
Avoid inserting an extra newline when we are already parked just below the
CARRIAGE_ID (or its optional separator)."
  (unless (and (markerp carriage--stream-beg-marker)
               (markerp carriage--stream-end-marker))
    (let* ((pos (cond
                 ((and (markerp carriage--stream-origin-marker)
                       (buffer-live-p (marker-buffer carriage--stream-origin-marker)))
                  (marker-position carriage--stream-origin-marker))
                 (t (point)))))
      (setq carriage--stream-beg-marker (copy-marker pos t))
      (setq carriage--stream-end-marker (copy-marker pos t)))))

(defun carriage-begin-reasoning ()
  "Open a #+begin_reasoning block at the end of streaming region if not open.
Records begin and tail markers so main text can be inserted after reasoning
without auto-closing; the end marker is inserted later at tail."
  (when (eq carriage-mode-include-reasoning 'block)
    (carriage--ensure-stream-region)
    (unless carriage--reasoning-open
      (let ((inhibit-read-only t)
            (pos (marker-position carriage--stream-end-marker)))
        (save-excursion
          (goto-char pos)
          (insert "#+begin_reasoning\n")
          ;; After inserting, the begin line starts at POS. Record markers:
          (setq carriage--reasoning-beg-marker (copy-marker pos))
          ;; Tail starts where we are now (immediately after begin line)
          (setq carriage--reasoning-tail-marker (copy-marker (point) t))
          ;; Stream end also advances to current point
          (set-marker carriage--stream-end-marker (point) (current-buffer)))
        (setq carriage--reasoning-open t)
        (when carriage-mode-reasoning-log-verbose
          (carriage-log "reasoning: begin inserted beg=%s tail=%s end=%s"
                        (and (markerp carriage--reasoning-beg-marker)
                             (marker-position carriage--reasoning-beg-marker))
                        (and (markerp carriage--reasoning-tail-marker)
                             (marker-position carriage--reasoning-tail-marker))
                        (and (markerp carriage--stream-end-marker)
                             (marker-position carriage--stream-end-marker))))
        ;; Fold as early as possible so users see a compact placeholder by default.
        ;; Best-effort: do not signal, and do not force-hide when point is inside.
        (ignore-errors
          (when (and (boundp 'carriage-mode-hide-reasoning-blocks)
                     carriage-mode-hide-reasoning-blocks
                     (require 'carriage-reasoning-fold nil t))
            (carriage-reasoning-fold-refresh-now (current-buffer))
            (carriage-reasoning-fold-hide-all (current-buffer))
            (carriage--reasoning-fold--sanitize-all (current-buffer))))))))

(defun carriage--reasoning-tail-pos ()
  "Return tail position for inserting #+end_reasoning, or nil."
  (or (and (markerp carriage--reasoning-tail-marker)
           (marker-position carriage--reasoning-tail-marker))
      (and (markerp carriage--stream-end-marker)
           (marker-position carriage--stream-end-marker))))

(defun carriage--reasoning-prev-line-at (pos)
  "Return the previous line text at POS (without properties), or an empty string."
  (if (numberp pos)
      (save-excursion
        (goto-char pos)
        (forward-line -1)
        (buffer-substring-no-properties
         (line-beginning-position) (line-end-position)))
    ""))

(defun carriage--reasoning-find-begin ()
  "Find matching #+begin_reasoning above point and return its position, or nil."
  (save-excursion
    (when (re-search-backward "^[ \t]*#\\+begin_reasoning\\b" nil t)
      (match-beginning 0))))

(defun carriage--reasoning-end-present-p (beg cur-end)
  "Return non-nil when #+end_reasoning exists between BEG and CUR-END."
  (when (and (numberp beg) (numberp cur-end))
    (save-excursion
      (goto-char (1+ beg))
      (re-search-forward "^[ \t]*#\\+end_reasoning\\b" cur-end t))))

(defun carriage--reasoning-insert-end-at (tailpos)
  "Insert #+end_reasoning at TAILPOS, adjust markers, and return end position."
  (when (numberp tailpos)
    (goto-char tailpos)
    (unless (bolp) (insert "\n"))
    (insert "#+end_reasoning\n")
    (let ((end-pos (point)))
      ;; If stream-end was equal to tail, advance it; otherwise leave it.
      (when (and (markerp carriage--stream-end-marker)
                 (= (marker-position carriage--stream-end-marker) tailpos))
        (set-marker carriage--stream-end-marker end-pos (current-buffer)))
      ;; Advance tail marker to the new end as well.
      (when (markerp carriage--reasoning-tail-marker)
        (set-marker carriage--reasoning-tail-marker end-pos (current-buffer)))
      end-pos)))

(defun carriage--reasoning-log-end (skipped end-pos beg-pos prev-line)
  "Log concise reasoning end diagnostics when verbose logging is enabled."
  (when carriage-mode-reasoning-log-verbose
    (carriage-log "reasoning: end %s (end-pos=%s beg-pos=%s prev='%s')"
                  (pcase skipped
                    ('none "inserted")
                    ('prev-line "skipped-duplicate")
                    ('already-present "skipped-already-present")
                    (_ skipped))
                  (or end-pos -1) (or beg-pos -1)
                  (condition-case _
                      (substring prev-line 0 (min 80 (length prev-line)))
                    (error prev-line)))))

(defun carriage-end-reasoning ()
  "Close the #+begin_reasoning block if it is open. Do not fold here; folding is done on finalize."
  (when carriage-mode-reasoning-log-verbose
    (carriage-log "reasoning: end-request open=%s" carriage--reasoning-open))
  (when carriage--reasoning-open
    (let ((inhibit-read-only t)
          (end-pos nil) (beg-pos nil)
          (skipped 'none)
          (prev-line "")
          (tailpos (carriage--reasoning-tail-pos)))
      (save-excursion
        ;; Context for diagnostics
        (when (numberp tailpos) (goto-char tailpos))
        (setq prev-line (carriage--reasoning-prev-line-at tailpos))
        ;; Detect begin and existing end marker between beg..end
        (setq beg-pos (carriage--reasoning-find-begin))
        (let ((cur (and (markerp carriage--stream-end-marker)
                        (marker-position carriage--stream-end-marker))))
          (when (carriage--reasoning-end-present-p beg-pos cur)
            (setq skipped 'already-present)))
        ;; Quick guard: previous line already an end marker
        (when (and (eq skipped 'none)
                   (string-match-p "^[ \t]*#\\+end_reasoning\\b" prev-line))
          (setq skipped 'prev-line))
        ;; Insert end marker at tail when needed
        (when (and (eq skipped 'none) (numberp tailpos))
          (setq end-pos (carriage--reasoning-insert-end-at tailpos))))
      (carriage--reasoning-log-end skipped end-pos beg-pos prev-line))
    ;; Fold immediately when reasoning is completed (as soon as end marker is received).
    ;; Respect point: do not force-hide the block containing point.
    (ignore-errors
      (when (and (boundp 'carriage-mode-hide-reasoning-blocks)
                 carriage-mode-hide-reasoning-blocks
                 (require 'carriage-reasoning-fold nil t))
        (carriage-reasoning-fold-refresh-now (current-buffer))
        (carriage-reasoning-fold-hide-all (current-buffer))
        (carriage--reasoning-fold--sanitize-all (current-buffer))))
    (setq carriage--reasoning-open nil)
    t))

(defun carriage--stream-insert-at-end (s)
  "Insert string S at the end of the current streaming region."
  (carriage--ensure-stream-region)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position carriage--stream-end-marker))
      (insert s)
      (set-marker carriage--stream-end-marker (point) (current-buffer)))))

;;;###autoload
(defun carriage-insert-stream-chunk (string &optional type)
  "Insert STRING chunk into the current buffer streaming region.
TYPE is either 'text (default) or 'reasoning.
- 'text: append to the region as-is (even if reasoning is open).
- 'reasoning: when carriage-mode-include-reasoning='block, ensure a #+begin_reasoning
  and append to the reasoning tail marker so that main text remains outside the block.

Implementation note (perf/UX):
- This function is intentionally hot-path and MUST NOT modify the buffer.
- It enqueues chunks and schedules a coalesced flush timer that performs a small
  number of inserts + a throttled `redisplay` so streaming remains visible even
  without user input (and in non-selected windows)."
  (let ((s (or string "")))
    ;; O(1) modeline support: detect begin_patch even when token is split across chunks.
    (when (and (not carriage--last-iteration-has-patches)
               (stringp s))
      (let* ((probe (concat (or carriage--stream-detect-tail "") s)))
        (when (string-match-p "#\\+begin_patch\\b" probe)
          (setq carriage--last-iteration-has-patches t))
        (setq carriage--stream-detect-tail
              (if (> (length probe) 80)
                  (substring probe (- (length probe) 80))
                probe))))
    ;; Enqueue and schedule a coalesced flush (buffer edits happen only in flush).
    (carriage--stream-enqueue type s))
  (point))

;;;###autoload
(defun carriage-stream-finalize (&optional errorp mark-last-iteration)
  "Finalize the current streaming session.
- Ensure any open reasoning block is closed.
- Fold the reasoning block after the full answer is printed.
- When MARK-LAST-ITERATION and not ERRORP: insert inline marker (if configured) and mark the region as last iteration.
- Trigger UI effects (flash/audio) on success."
  ;; Ensure any pending coalesced stream chunks are inserted before finalizing.
  (when (fboundp 'carriage-stream-flush-now)
    (ignore-errors (carriage-stream-flush-now (current-buffer))))
  ;; Close reasoning if still open (end marker inserted at tail)
  (ignore-errors (carriage-end-reasoning))
  ;; Ensure end marker and any last buffered chunks are visible.
  (when (fboundp 'carriage-stream-flush-now)
    (ignore-errors (carriage-stream-flush-now (current-buffer))))
  ;; Stop preloader if still running.
  (when (fboundp 'carriage--preloader-stop)
    (carriage--preloader-stop))
  ;; Fold reasoning now (after the whole response), using recorded begin marker if available.
  ;; If overlay-based reasoning folding is enabled, it is responsible for folding and we avoid
  ;; mixing org-fold invisibility with our overlays.
  (when (and (not errorp)
             (markerp carriage--reasoning-beg-marker)
             (not (and (featurep 'carriage-reasoning-fold)
                       (boundp 'carriage-reasoning-fold--enabled)
                       (bound-and-true-p carriage-reasoning-fold--enabled))))
    (condition-case _e
        (progn
          (require 'org)
          (let ((beg (marker-position carriage--reasoning-beg-marker)))
            (cond
             ((fboundp 'org-fold-hide-drawer-or-block)
              (save-excursion
                (goto-char beg)
                (org-fold-hide-drawer-or-block t)))
             ((fboundp 'org-hide-block-toggle)
              (save-excursion
                (goto-char beg)
                (org-hide-block-toggle t)))
             ((featurep 'org-fold)
              (save-excursion
                (goto-char beg)
                ;; Best effort: fold the block body region
                (let ((body-beg (progn (forward-line 1) (point)))
                      (body-end (save-excursion
                                  (when (re-search-forward "^[ \t]*#\\+end_reasoning\\b" nil t)
                                    (forward-line -1))
                                  (line-end-position))))
                  (org-fold-region body-beg body-end t))))
             (t nil))))
      (error nil)))

  ;; Effects on success
  (when (not errorp)
    (when (and (boundp 'carriage-mode-flash-patches) carriage-mode-flash-patches
               (fboundp 'carriage-ui--flash-last-iteration-patches))
      (carriage-ui--flash-last-iteration-patches (current-buffer)))
    (when (fboundp 'carriage--audio-notify-success)
      (carriage--audio-notify-success)))
  ;; Nudge redisplay for any windows showing this buffer so final state is visible immediately.
  (dolist (w (get-buffer-window-list (current-buffer) t t))
    (force-window-update w))
  t)


;;; Prompt construction helpers

;; v1.1 — Полный идентификатор модели для tooltip в модлайне.
(defun carriage-llm-full-id (&optional backend provider model)
  "Return normalized full LLM id backend[:provider]:model for current buffer or given args.
Deduplicates segments if MODEL already contains provider/backend."
  (let* ((be (or backend (and (boundp 'carriage-mode-backend) carriage-mode-backend)))
         (pr (or provider (and (boundp 'carriage-mode-provider) carriage-mode-provider)))
         (mo (or model   (and (boundp 'carriage-mode-model)   carriage-mode-model))))
    (let ((resolved (ignore-errors (carriage-llm-resolve-model be pr mo))))
      (when (stringp resolved)
        (setq mo resolved)))
    (let* ((be-str (cond
                    ((symbolp be) (symbol-name be))
                    ((stringp be) be)
                    ((null be) "")
                    (t (format "%s" be))))
           (pr-str (cond
                    ((symbolp pr) (symbol-name pr))
                    ((stringp pr) pr)
                    ((null pr) "")
                    (t (format "%s" pr))))
           (mo-str (cond
                    ((symbolp mo) (symbol-name mo))
                    ((stringp mo) mo)
                    ((null mo) "")
                    (t (format "%s" mo)))))
      (let* ((parts (and (stringp mo-str) (not (string-empty-p mo-str))
                         (split-string mo-str ":" t)))
             (n (length parts)))
        (cond
         ;; No model → return backend (or empty)
         ((or (null parts) (zerop n))
          (or be-str ""))
         ;; MODEL already like "backend:...": return as-is
         ((and (not (string-empty-p be-str))
               (string-prefix-p (concat be-str ":") mo-str))
          mo-str)
         ;; MODEL has two parts "provider:model" → prefix backend
         ((= n 2)
          (if (and (not (string-empty-p be-str)))
              (concat be-str ":" mo-str)
            mo-str))
         ;; MODEL has ≥3 parts → assume fully-qualified and return as-is
         ((>= n 3)
          mo-str)
         ;; MODEL is a bare name → compose "backend[:provider]:model" with dedup when pr==backend
         (t
          (if (string-empty-p be-str)
              mo-str
            (concat be-str
                    (if (and (not (string-empty-p pr-str))
                             (not (string= pr-str be-str)))
                        (concat ":" pr-str)
                      "")
                    ":" mo-str))))))))

;; -------------------------------------------------------------------
;; Per-send fingerprint (inline, near iteration marker)

(defcustom carriage-mode-insert-fingerprint t
  "When non-nil, Send commands insert an inline fingerprint line at the stream origin:

  #+CARRIAGE_FINGERPRINT: (<plist>)

The fingerprint captures only response-shaping and context-shaping parameters
(intent/suite/model/context toggles/scope/profile/budgets/injection), and MUST
never be included in outgoing LLM prompts (transports must filter it)."
  :type 'boolean
  :group 'carriage)

(defvar-local carriage--fingerprint-inline-inserted nil
  "Non-nil when an inline CARRIAGE_FINGERPRINT line has been inserted for the current stream.")

(defvar-local carriage--fingerprint-line-marker nil
  "Marker pointing to the beginning of the current request's inline fingerprint line, or nil.

This marker is used for two-phase fingerprint upsert (usage+cost) on request completion.")

(defvar-local carriage--separator-inserted nil
  "Non-nil when a visual separator '-----' was already inserted for the current stream.")

;; O(1) modeline support: last-iteration presence (avoid regexp scans in redisplay).
(defvar-local carriage--last-iteration-has-patches nil
  "Non-nil when the current \"last iteration\" region contains at least one begin_patch block.

This flag is maintained by streaming insertion and by accept/insert helpers.
UI must treat it as best-effort (user may manually edit/delete blocks).")

(defvar-local carriage--stream-detect-tail ""
  "Small tail buffer used to detect begin_patch tokens split across stream chunks.")

(defun carriage--fingerprint-plist ()
  "Return a sanitized fingerprint plist for the current buffer.
Important: only response/context shaping fields (no UI prefs, no secrets)."
  (list
   ;; Lightweight timestamp for diagnostics
   :CAR_TS (float-time)

   ;; Response shape
   :CAR_INTENT (and (boundp 'carriage-mode-intent) carriage-mode-intent)
   :CAR_SUITE  (and (boundp 'carriage-mode-suite) carriage-mode-suite)

   ;; LLM identity
   :CAR_BACKEND  (and (boundp 'carriage-mode-backend) carriage-mode-backend)
   :CAR_PROVIDER (and (boundp 'carriage-mode-provider) carriage-mode-provider)
   :CAR_MODEL    (and (boundp 'carriage-mode-model) carriage-mode-model)

   ;; Context sources
   :CAR_CTX_DOC     (and (boundp 'carriage-mode-include-doc-context)
                         carriage-mode-include-doc-context)
   :CAR_CTX_GPTEL   (and (boundp 'carriage-mode-include-gptel-context)
                         carriage-mode-include-gptel-context)
   :CAR_CTX_VISIBLE (and (boundp 'carriage-mode-include-visible-context)
                         carriage-mode-include-visible-context)
   :CAR_CTX_PATCHED (and (boundp 'carriage-mode-include-patched-files)
                         carriage-mode-include-patched-files)

   ;; Context shaping
   :CAR_DOC_CTX_SCOPE (or (and (boundp 'carriage-doc-context-scope) carriage-doc-context-scope)
                          (and (boundp 'carriage-mode-doc-context-scope) carriage-mode-doc-context-scope))
   :CAR_CTX_PROFILE   (or (and (boundp 'carriage-context-profile) carriage-context-profile)
                          (and (boundp 'carriage-mode-context-profile) carriage-mode-context-profile))
   :CAR_CTX_MAX_FILES (and (boundp 'carriage-mode-context-max-files)
                           carriage-mode-context-max-files)
   :CAR_CTX_MAX_BYTES (and (boundp 'carriage-mode-context-max-total-bytes)
                           carriage-mode-context-max-total-bytes)
   :CAR_CTX_INJECTION (and (boundp 'carriage-mode-context-injection)
                           carriage-mode-context-injection)))

(defun carriage-insert-inline-fingerprint-now ()
  "Insert/update inline #+CARRIAGE_FINGERPRINT: ... line at current stream origin.

Called by Send commands at stream origin. Advances `carriage--stream-origin-marker'
so preloader/streaming starts strictly below the fingerprint line."
  (interactive)
  (when (and (boundp 'carriage-mode-insert-fingerprint)
             carriage-mode-insert-fingerprint
             (not carriage--fingerprint-inline-inserted))
    (let* ((pos (cond
                 ((and (markerp carriage--stream-origin-marker)
                       (buffer-live-p (marker-buffer carriage--stream-origin-marker)))
                  (marker-position carriage--stream-origin-marker))
                 ((and (markerp carriage--stream-beg-marker)
                       (buffer-live-p (marker-buffer carriage--stream-beg-marker)))
                  (marker-position carriage--stream-beg-marker))
                 (t (point))))
           (fp (carriage--fingerprint-plist))
           (line (format "#+CARRIAGE_FINGERPRINT: %s\n" (prin1-to-string fp)))
           (inhibit-read-only t)
           (newpos nil))
      (save-excursion
        (goto-char (max (point-min) (min pos (point-max))))
        (beginning-of-line)
        ;; Idempotent: if there is already a fingerprint line at origin, replace it.
        (let ((case-fold-search t))
          (when (looking-at "^[ \t]*#\\+CARRIAGE_FINGERPRINT\\b.*$")
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position))))
            (goto-char (line-beginning-position))))
        (let ((beg (point)))
          (insert line)
          ;; Marker for two-phase fingerprint upsert (usage+cost) on completion.
          (setq carriage--fingerprint-line-marker (copy-marker beg t))
          (setq newpos (point))))
      (when (numberp newpos)
        ;; Stream (and preloader) must start strictly after the fingerprint.
        (setq carriage--stream-origin-marker (copy-marker newpos t))
        ;; Policy: keep point at the new stream origin.
        (goto-char newpos)
        ;; Ensure doc-state fold overlays are refreshed so the fingerprint line is immediately folded
        ;; (shows at least Intent+Model) without relying on debounced after-change timers.
        (ignore-errors
          (when (require 'carriage-doc-state nil t)
            (when (fboundp 'carriage-doc-state-summary-enable)
              (carriage-doc-state-summary-enable))
            (when (fboundp 'carriage-doc-state-summary-refresh)
              (carriage-doc-state-summary-refresh (current-buffer)))))
        ;; Defensive de-dup: if any stray separators were added below the fingerprint line
        ;; (e.g., by legacy advice or double calls), remove a few consecutive ones under it.
        (save-excursion
          (end-of-line)
          (forward-line 1)                 ;; go to the line after the inserted fingerprint
          (let ((k 0))
            (while (and (< k 3)
                        (looking-at "^[ \t]*-----[ \t]*$")
                        (< (line-end-position) (point-max)))
              (delete-region (line-beginning-position)
                             (min (point-max) (1+ (line-end-position))))
              ;; do not advance point: after deletion the next candidate is at the same line-beg
              (setq k (1+ k))))))))
  (setq carriage--fingerprint-inline-inserted t)
  t)

(defun carriage-fingerprint--find-line-marker ()
  "Return a marker pointing at a fingerprint line suitable for upsert, or nil.
Prefers `carriage--fingerprint-line-marker' when live; otherwise finds the LAST
#+CARRIAGE_FINGERPRINT line in the buffer (to match the most recent request)."
  (cond
   ((and (markerp carriage--fingerprint-line-marker)
         (buffer-live-p (marker-buffer carriage--fingerprint-line-marker)))
    carriage--fingerprint-line-marker)
   (t
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
            (last nil))
        (while (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT:" nil t)
          (setq last (line-beginning-position)))
        (when (numberp last)
          (copy-marker last t)))))))

(defun carriage-fingerprint--read-plist-at (marker)
  "Read fingerprint plist at MARKER (beginning of line). Return plist or nil."
  (when (and (markerp marker) (buffer-live-p (marker-buffer marker)))
    (save-excursion
      (goto-char (marker-position marker))
      (beginning-of-line)
      (let ((case-fold-search t))
        (when (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT:[ \t]*\\(.*\\)$"
                                 (line-end-position) t)
          (let* ((s (match-string 1))
                 (obj (condition-case _e
                          (car (read-from-string s))
                        (error nil))))
            (when (listp obj) obj)))))))

(defun carriage-fingerprint--write-plist-at (marker plist)
  "Replace fingerprint line at MARKER with PLIST (printed). Return t on success."
  (when (and (markerp marker) (buffer-live-p (marker-buffer marker)))
    (save-excursion
      (goto-char (marker-position marker))
      (beginning-of-line)
      (let ((inhibit-read-only t)
            (case-fold-search t))
        (when (looking-at "^[ \t]*#\\+CARRIAGE_FINGERPRINT\\b.*$")
          (delete-region (line-beginning-position)
                         (min (point-max) (1+ (line-end-position)))))
        (let ((beg (point)))
          (insert (format "#+CARRIAGE_FINGERPRINT: %s\n" (prin1-to-string (or plist '()))))
          (set-marker marker beg (current-buffer))
          t)))))

(defun carriage-fingerprint-note-usage-and-cost (usage &optional backend provider model)
  "Upsert current request fingerprint line with USAGE and computed cost.

USAGE is a plist with keys:
- :tokens-in :tokens-out :audio-in :audio-out

When pricing is available, also adds:
- :CAR_COST_KNOWN boolean
- :CAR_COST_IN_U :CAR_COST_OUT_U :CAR_COST_AUDIO_IN_U :CAR_COST_AUDIO_OUT_U :CAR_COST_TOTAL_U

This function is best-effort and must never signal."
  (condition-case _e
      (let* ((marker (carriage-fingerprint--find-line-marker))
             (old (or (carriage-fingerprint--read-plist-at marker) '()))
             (tin (plist-get usage :tokens-in))
             (tout (plist-get usage :tokens-out))
             (ain (plist-get usage :audio-in))
             (aout (plist-get usage :audio-out))
             (be (or backend (and (boundp 'carriage-mode-backend) carriage-mode-backend)))
             (pr (or provider (and (boundp 'carriage-mode-provider) carriage-mode-provider)))
             (mo (or model (and (boundp 'carriage-mode-model) carriage-mode-model)))
             (canonical (and (fboundp 'carriage-llm-full-id)
                             (carriage-llm-full-id be pr mo)))
             (root (or (ignore-errors (carriage-project-root)) default-directory))
             (tab (and (require 'carriage-pricing nil t)
                       (fboundp 'carriage-pricing-load-table)
                       (carriage-pricing-load-table root)))
             (cost (and (hash-table-p tab)
                        (stringp canonical)
                        (fboundp 'carriage-pricing-compute)
                        (carriage-pricing-compute canonical usage tab)))
             ;; Determine known ONLY if a concrete integer total cost (in µ₽) is present.
             (known (and (listp cost) (integerp (plist-get cost :cost-total-u))))
             (pl old))
        ;; Always write computed pricing to log (best-effort), even if fingerprint line is absent.
        (when (fboundp 'carriage-log)
          (ignore-errors
            (let* ((total-u (and (listp cost) (plist-get cost :cost-total-u)))
                   (money (cond
                           ((fboundp 'carriage-pricing-format-money)
                            (carriage-pricing-format-money total-u))
                           ((integerp total-u) (format "%d" total-u))
                           (t "—"))))
              (carriage-log "pricing: model=%s known=%s tokens-in=%s tokens-out=%s audio-in=%s audio-out=%s total-u=%s total=%s"
                            (or canonical (or model "") "")
                            (if known "t" "nil")
                            (if (integerp tin) tin "—")
                            (if (integerp tout) tout "—")
                            (if (integerp ain) ain "—")
                            (if (integerp aout) aout "—")
                            (if (integerp total-u) total-u "—")
                            money))))
        (unless (markerp marker)
          (cl-return-from carriage-fingerprint-note-usage-and-cost nil))
        ;; Usage keys (always upsert, even if nil)
        (setq pl (plist-put pl :CAR_TOKENS_IN tin))
        (setq pl (plist-put pl :CAR_TOKENS_OUT tout))
        (setq pl (plist-put pl :CAR_AUDIO_IN ain))
        (setq pl (plist-put pl :CAR_AUDIO_OUT aout))
        ;; Cost keys (best-effort)
        (when (listp cost)
          (setq pl (plist-put pl :CAR_COST_KNOWN (if known t nil)))
          (setq pl (plist-put pl :CAR_COST_IN_U (plist-get cost :cost-in-u)))
          (setq pl (plist-put pl :CAR_COST_OUT_U (plist-get cost :cost-out-u)))
          (setq pl (plist-put pl :CAR_COST_AUDIO_IN_U (plist-get cost :cost-audio-in-u)))
          (setq pl (plist-put pl :CAR_COST_AUDIO_OUT_U (plist-get cost :cost-audio-out-u)))
          (setq pl (plist-put pl :CAR_COST_TOTAL_U (plist-get cost :cost-total-u))))
        (carriage-fingerprint--write-plist-at marker pl)
        ;; Ensure fingerprint fold overlay sees the updated cost immediately (no debounce wait).
        (when (and (require 'carriage-doc-state nil t)
                   (fboundp 'carriage-doc-state-summary-refresh))
          (ignore-errors (carriage-doc-state-summary-refresh (current-buffer))))
        ;; Nudge doc-cost refresh if UI exposes it (best-effort; not required for correctness).
        (when (fboundp 'carriage-ui-doc-cost-schedule-refresh)
          (ignore-errors (carriage-ui-doc-cost-schedule-refresh 0.05)))
        t)
    (error nil)))

(defun carriage--payload-from-source (source buffer)
  "Return raw payload text for SOURCE from BUFFER.

SOURCE is either:
- 'buffer  — entire buffer text
- 'subtree — current Org subtree when in org-mode; otherwise entire buffer."
  (with-current-buffer buffer
    (let ((mode (buffer-local-value 'major-mode buffer)))
      (pcase source
        ('subtree
         (if (eq mode 'org-mode)
             (save-excursion
               (require 'org)
               (ignore-errors (org-back-to-heading t))
               (let ((beg (save-excursion (org-back-to-heading t) (point)))
                     (end (save-excursion (org-end-of-subtree t t) (point))))
                 (buffer-substring-no-properties beg end)))
           (buffer-substring-no-properties (point-min) (point-max))))
        (_ (buffer-substring-no-properties (point-min) (point-max)))))))

(defun carriage--payload-strip-doc-state-and-markers (text)
  "Remove doc-state and per-send marker lines from TEXT.
This must never leak into the LLM payload."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let ((case-fold-search t))
      ;; Doc-state (canonical header) must never reach the LLM
      (while (re-search-forward
              "^[ \t]*#\\+PROPERTY:[ \t]+\\(CARRIAGE_STATE\\)\\b.*$"
              nil t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position)))))
      ;; Per-send markers (must never reach the LLM)
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*#\\+\\(CARRIAGE_FINGERPRINT\\)\\b.*$"
              nil t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))
        (goto-char (line-beginning-position))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun carriage--patch--read-header-plist (sexp-str)
  "Parse SEXP-STR from a #+begin_patch header into a plist, or nil.
Never signals."
  (when (stringp sexp-str)
    (condition-case _e
        (let ((obj (car (read-from-string sexp-str))))
          (and (listp obj) obj))
      (error nil))))

(defun carriage--patch--applied-p (plist)
  "Return non-nil when PLIST marks the patch as applied."
  (and (listp plist) (plist-get plist :applied)))

(defun carriage--patch--applied-desc (plist)
  "Return normalized description for an applied patch PLIST."
  (let* ((desc (or (and (listp plist)
                        (or (plist-get plist :description)
                            (plist-get plist :result)))
                   "Applied"))
         (s (string-trim (format "%s" desc))))
    (if (string-empty-p s) "Applied" s)))

(defun carriage--patch--summary-line (desc)
  "Build the one-line summary placeholder for applied patch body."
  (format ";; applied: %s (content omitted)\n" desc))

(defun carriage--patch--body-begin-pos (beg-line-end)
  "Compute body start position given BEG-LINE-END (end of #+begin_patch line)."
  (min (point-max) (1+ beg-line-end)))

(defun carriage--patch--find-end-pos (from)
  "Find #+end_patch from FROM and return match end position, or nil."
  (save-excursion
    (goto-char from)
    (re-search-forward "^[ \t]*#\\+end_patch\\b.*$" nil t)))

(defun carriage--patch--end-line-begin-pos (end-pos)
  "Return line-beginning-position for END-POS."
  (save-excursion
    (goto-char end-pos)
    (line-beginning-position)))

(defun carriage--patch--replace-body-with-summary (body-beg end-line-beg summary)
  "Replace patch body between BODY-BEG and END-LINE-BEG with SUMMARY."
  (delete-region body-beg end-line-beg)
  (goto-char body-beg)
  (insert summary))

(defun carriage--patch--truncate-to-end-and-close (body-beg summary)
  "Truncate buffer from BODY-BEG to end; insert SUMMARY and a synthetic #+end_patch."
  (delete-region body-beg (point-max))
  (goto-char body-beg)
  (insert summary "#+end_patch\n")
  (goto-char (point-max)))

(defun carriage--patch--goto-next-line-safe ()
  "Advance point to the next line safely and return point."
  (goto-char (min (point-max) (1+ (line-end-position))))
  (point))

(defun carriage--patch--goto-after-end-line ()
  "Move point to just after the next #+end_patch line; fallback to next-line."
  (if (re-search-forward "^[ \t]*#\\+end_patch\\b.*$" nil t)
      (progn
        (goto-char (min (point-max) (1+ (line-end-position))))
        (point))
    (carriage--patch--goto-next-line-safe)))

(defun carriage--payload-summarize-applied-patches (text &optional keep-applied-bodies)
  "Replace bodies of applied #+begin_patch blocks in TEXT with a short summary.

When KEEP-APPLIED-BODIES is non-nil, return TEXT unchanged.

Policy (when summarizing):
- Keep '#+begin_patch (...)' and '#+end_patch' lines.
- Replace the body with one comment line:
  ;; applied: <desc> (content omitted)

This lets LLM see history without sending potentially large patch bodies."
  (if keep-applied-bodies
      (or text "")
    (with-temp-buffer
      (insert (or text ""))
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" nil t)
          (let* ((beg-line-end (line-end-position))
                 (sexp-str (match-string 1))
                 (plist (carriage--patch--read-header-plist sexp-str)))
            (if (not (carriage--patch--applied-p plist))
                (carriage--patch--goto-next-line-safe)
              (let* ((desc (carriage--patch--applied-desc plist))
                     (summary (carriage--patch--summary-line desc))
                     (body-beg (carriage--patch--body-begin-pos beg-line-end))
                     (end-pos (carriage--patch--find-end-pos body-beg)))
                (if end-pos
                    (let ((end-line-beg (carriage--patch--end-line-begin-pos end-pos)))
                      (carriage--patch--replace-body-with-summary body-beg end-line-beg summary)
                      ;; Continue scanning after the end_patch line (positions may have shifted).
                      (carriage--patch--goto-after-end-line))
                  (carriage--patch--truncate-to-end-and-close body-beg summary)))))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun carriage--sanitize-payload-for-llm (text)
  "Return TEXT sanitized for LLM payload.
Strips doc-state/markers and summarizes applied patch bodies unless Patch/Patched source is enabled."
  (let ((keep-applied (and (boundp 'carriage-mode-include-patched-files)
                           carriage-mode-include-patched-files)))
    (carriage--payload-summarize-applied-patches
     (carriage--payload-strip-doc-state-and-markers text)
     keep-applied)))

(defun carriage--context-target ()
  "Return current buffer's context injection target ('system or 'user)."
  (if (boundp 'carriage-mode-context-injection)
      carriage-mode-context-injection
    'system))

(defun carriage--context-sources-enabled-p ()
  "Return non-nil when any context source is enabled in current buffer."
  (let* ((inc-doc (if (boundp 'carriage-mode-include-doc-context)
                      carriage-mode-include-doc-context
                    t))
         (inc-gpt (and (boundp 'carriage-mode-include-gptel-context)
                       carriage-mode-include-gptel-context))
         (inc-vis (and (boundp 'carriage-mode-include-visible-context)
                       carriage-mode-include-visible-context))
         (inc-pat (and (boundp 'carriage-mode-include-patched-files)
                       carriage-mode-include-patched-files)))
    (or inc-doc inc-gpt inc-vis inc-pat)))

(defun carriage--context-collect-and-format (buffer target)
  "Return formatted external context text for BUFFER injected at TARGET, or nil.
Best-effort: never signal."
  (condition-case _e
      (when (and (require 'carriage-context nil t)
                 (carriage--context-sources-enabled-p))
        (let* ((root (or (carriage-project-root) default-directory))
               (col (carriage-context-collect buffer root)))
          (when col
            ;; Traffic: log context summary and individual elements (paths only)
            (let* ((files (plist-get col :files))
                   (stats (plist-get col :stats))
                   (inc   (and stats (plist-get stats :included)))
                   (sk    (and stats (plist-get stats :skipped)))
                   (bytes (and stats (plist-get stats :total-bytes))))
              (carriage-traffic-log 'out "context: target=%s included=%s skipped=%s total-bytes=%s"
                                    target (or inc 0) (or sk 0) (or bytes 0))
              (dolist (f files)
                (let ((rel (plist-get f :rel))
                      (included (plist-get f :content))
                      (reason (plist-get f :reason)))
                  (carriage-traffic-log 'out " - %s (%s)"
                                        rel
                                        (if (stringp included)
                                            "included"
                                          (format "omitted%s"
                                                  (if reason (format ": %s" reason) "")))))))
            (carriage-context-format col :where target))))
    (error nil)))

(defun carriage--build-context (source buffer)
  "Return context plist for prompt builder with at least :payload.
SOURCE is 'buffer or 'subtree. BUFFER is the source buffer.
May include :context-text and :context-target per v1.1."
  (with-current-buffer buffer
    (let* ((payload-raw (carriage--payload-from-source source buffer))
           (payload (carriage--sanitize-payload-for-llm payload-raw))
           (target (carriage--context-target))
           (ctx-text (carriage--context-collect-and-format buffer target))
           (res (list :payload payload)))
      (when (and (stringp ctx-text) (not (string-empty-p ctx-text)))
        (setq res (append res (list :context-text ctx-text :context-target target))))
      res)))

;;; Commands (stubs/minimal implementations)

;;;###autoload
(defun carriage-send-buffer ()
  "Send entire buffer to LLM according to current Intent/Suite."
  (interactive)
  ;; Early, immediate feedback before any heavy preparation:
  (carriage-ui-set-state 'sending)
  ;; Reset apply-status when starting a new request (requirement: Apply must reappear only after next apply attempt).
  (when (fboundp 'carriage-ui-apply-reset)
    (ignore-errors (carriage-ui-apply-reset)))
  ;; Give redisplay a chance right away
  (sit-for 0)
  ;; Defer heavy preparation to the next tick so UI updates (spinner/state) are visible instantly.
  (let ((srcbuf (current-buffer))
        (prefix current-prefix-arg)
        (origin-marker
         (copy-marker
          (progn
            ;; Ensure Send always starts on a fresh line *below* the current one.
            ;; If point is in the middle/end of a non-empty line, insert a newline and
            ;; move point to the new line so the separator+fingerprint never appear above
            ;; the user's current line.
            (when (or (not (bolp))
                      (save-excursion
                        (beginning-of-line)
                        (re-search-forward "[^ \t]" (line-end-position) t)))
              (end-of-line)
              (insert "\n"))
            (point))
          t)))
    ;; Prepare stream immediately at cursor: reset → begin-iteration → insert marker+separator → preloader.
    (carriage-stream-reset origin-marker)
    (when (fboundp 'carriage-begin-iteration)
      (ignore-errors (carriage-begin-iteration)))
    ;; Insert separator first, then per-send fingerprint at stream origin (single canonical line).
    (when (fboundp 'carriage-insert-send-separator)
      (ignore-errors (carriage-insert-send-separator)))
    (when (fboundp 'carriage-insert-inline-fingerprint-now)
      (ignore-errors (carriage-insert-inline-fingerprint-now)))
    (when (fboundp 'carriage--preloader-start)
      (ignore-errors (carriage--preloader-start)))
    (run-at-time
     0 nil
     (lambda ()
       (let ((current-prefix-arg prefix))
         (with-current-buffer srcbuf
           (let* ((backend carriage-mode-backend)
                  (model   carriage-mode-model)
                  (intent  carriage-mode-intent)
                  (suite   carriage-mode-suite)
                  (ctx nil)
                  (built nil) (sys nil) (pr nil))
             ;; Build context after immediate UI feedback
             (setq ctx (carriage--build-context 'buffer srcbuf))
             (setq built (carriage-build-prompt intent suite ctx)
                   sys   (plist-get built :system)
                   pr    (plist-get built :prompt))
             (carriage-ui-set-state 'dispatch)
             (carriage-log "send-buffer: intent=%s suite=%s backend=%s model=%s"
                           intent suite backend model)
             (when (and carriage-mode-auto-open-log (not (bound-and-true-p noninteractive)))
               (ignore-errors (carriage-show-log)))
             (when (and carriage-mode-auto-open-traffic (not (bound-and-true-p noninteractive)))
               (ignore-errors (carriage-show-traffic)))

             (carriage--ensure-transport)
             (let* ((unreg (carriage-transport-begin)))
               (carriage-traffic-log 'out "request begin: source=buffer backend=%s model=%s"
                                     backend model)
               (condition-case err
                   (progn
                     ;; Dispatch via transport (placeholder will log error if no adapter).
                     (carriage-transport-dispatch :source 'buffer
                                                  :backend backend
                                                  :model model
                                                  :prompt pr
                                                  :system sys
                                                  :buffer srcbuf
                                                  :mode (symbol-name (buffer-local-value 'major-mode srcbuf))
                                                  :insert-marker (or (and (markerp carriage--stream-origin-marker)
                                                                          (buffer-live-p (marker-buffer carriage--stream-origin-marker))
                                                                          carriage--stream-origin-marker)
                                                                     origin-marker))
                     t)
                 (quit
                  (carriage-log "send-buffer: keyboard-quit; aborting")
                  (ignore-errors (carriage-abort-current))
                  (ignore-errors (carriage-transport-complete t))
                  (carriage-ui-set-state 'idle)
                  nil)
                 (error
                  (carriage-log "send-buffer error: %s" (error-message-string err))
                  (carriage-transport-complete t)))))))))))

;;;###autoload
(defun carriage-send-subtree ()
  "Send current org subtree to LLM according to current Intent/Suite."
  (interactive)
  ;; Early, immediate feedback before any heavy preparation:
  (carriage-ui-set-state 'sending)
  ;; Reset apply-status when starting a new request (requirement: Apply must reappear only after next apply attempt).
  (when (fboundp 'carriage-ui-apply-reset)
    (ignore-errors (carriage-ui-apply-reset)))
  ;; Give redisplay a chance right away
  (sit-for 0)
  (let* ((backend carriage-mode-backend)
         (model   carriage-mode-model)
         (intent  carriage-mode-intent)
         (suite   carriage-mode-suite)
         (srcbuf  (current-buffer))
         (origin-marker
          (copy-marker
           (progn
             ;; Ensure Send always starts on a fresh line *below* the current one.
             (when (or (not (bolp))
                       (save-excursion
                         (beginning-of-line)
                         (re-search-forward "[^ \t]" (line-end-position) t)))
               (end-of-line)
               (insert "\n"))
             (point))
           t))
         (ctx nil)
         (built nil) (sys nil) (pr nil))
    ;; Prepare stream origin and preloader; insert only the fingerprint line (no inline iteration-id line).
    (carriage-stream-reset origin-marker)
    (when (fboundp 'carriage-begin-iteration)
      (ignore-errors (carriage-begin-iteration)))
    (when (fboundp 'carriage-insert-inline-fingerprint-now)
      (ignore-errors (carriage-insert-inline-fingerprint-now)))
    (when (fboundp 'carriage--preloader-start)
      (ignore-errors (carriage--preloader-start)))
    ;; Keep spinner/active feedback during context build, then build context
    (carriage-ui-set-state 'sending)
    (setq ctx (carriage--build-context 'subtree srcbuf))
    (setq built (carriage-build-prompt intent suite ctx)
          sys   (plist-get built :system)
          pr    (plist-get built :prompt))
    (carriage-ui-set-state 'dispatch)
    (carriage-log "send-subtree: intent=%s suite=%s backend=%s model=%s"
                  intent suite backend model)
    ;; Best-effort derive a small payload boundary for logs
    (when (derived-mode-p 'org-mode)
      (carriage-log "send-subtree: org-mode detected; using subtree-at-point as payload"))
    (when (and carriage-mode-auto-open-log (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-log)))
    (when (and carriage-mode-auto-open-traffic (not (bound-and-true-p noninteractive)))
      (ignore-errors (carriage-show-traffic)))

    (carriage--ensure-transport)
    (let* ((unreg (carriage-transport-begin)))
      (carriage-traffic-log 'out "request begin: source=subtree backend=%s model=%s"
                            backend model)
      (condition-case err
          (progn
            (carriage-transport-dispatch :source 'subtree
                                         :backend backend
                                         :model model
                                         :prompt pr
                                         :system sys
                                         :buffer srcbuf
                                         :mode (symbol-name (buffer-local-value 'major-mode srcbuf))
                                         ;; Keep consistent with `carriage-send-buffer': when we inserted inline marker
                                         ;; and fingerprint, `carriage--stream-origin-marker' was advanced to start
                                         ;; strictly below them. Use it when available.
                                         :insert-marker (or (and (markerp carriage--stream-origin-marker)
                                                                 (buffer-live-p (marker-buffer carriage--stream-origin-marker))
                                                                 carriage--stream-origin-marker)
                                                            origin-marker))
            t)
        (error
         (carriage-log "send-subtree error: %s" (error-message-string err))
         (carriage-transport-complete t))))))

;;;###autoload
(defun carriage-dry-run-at-point ()
  "Run dry-run for the patch block at point and open report."
  (interactive)
  (when (fboundp 'carriage-ui-apply-set-state)
    (carriage-ui-apply-set-state 'running "Dry-run…"))
  (let* ((root (or (carriage-project-root) default-directory))
         (plan-item
          (condition-case e
              (carriage-parse-block-at-point root)
            (error
             ;; Parse error is a local (apply/dry-run) concern, not a request concern.
             (when (fboundp 'carriage-ui-apply-set-state)
               (carriage-ui-apply-set-state
                'dry-fail
                (format "Ошибка парсинга: %s" (error-message-string e))))
             (user-error "Carriage parse error: %s" (error-message-string e))))))
    ;; Early Suite↔Engine guard for patch
    (let ((op (or (and (listp plan-item) (plist-get plan-item :op))
                  (alist-get :op plan-item))))
      (when (and (eq op 'patch)
                 (not (carriage--engine-supports-op-p 'patch)))
        (when (fboundp 'carriage-ui-apply-set-state)
          (carriage-ui-apply-set-state 'dry-fail "Patch unsupported by current engine; switch to git"))
        (user-error "Patch unsupported by %s engine; switch to git" (carriage-apply-engine)))
      ;; If git engine selected but no repository, refuse early with a friendly hint
      (when (and (eq op 'patch)
                 (eq (carriage-apply-engine) 'git)
                 (null (carriage-project-root)))
        (when (fboundp 'carriage-ui-apply-set-state)
          (carriage-ui-apply-set-state 'dry-fail "Нет Git-репозитория (dry-run/apply невозможен для udiff без Git)"))
        (user-error "Нет Git-репозитория; выберите движок emacs (ограниченный) или инициализируйте Git")))
    (let ((report (carriage-dry-run-plan (list plan-item) root)))
      (when (not noninteractive)
        (carriage--report-open-maybe report))
      ;; Apply badge is updated from the report (carriage-ui-note-apply-report).
      report)))

(defun carriage--parse-plan-item-or-error (root)
  "Parse block at point under ROOT or signal user-error with UI state update.

Important UI rule:
- Parse/validation errors are local (apply/dry-run) concerns and must not mutate
  request/transport state."
  (condition-case e
      (carriage-parse-block-at-point root)
    (error
     (when (fboundp 'carriage-ui-apply-set-state)
       (carriage-ui-apply-set-state
        'apply-fail
        (format "Ошибка парсинга: %s" (error-message-string e))))
     (user-error "Carriage parse error: %s" (error-message-string e)))))

(defun carriage--guard-patch-engine-for-item (plan-item)
  "Ensure that applying PLAN-ITEM is allowed for current engine; user-error otherwise."
  (let ((op (or (and (listp plan-item) (plist-get plan-item :op))
                (alist-get :op plan-item))))
    (when (and (eq op 'patch)
               (not (carriage--engine-supports-op-p 'patch)))
      (when (fboundp 'carriage-ui-apply-set-state)
        (carriage-ui-apply-set-state 'apply-fail "Patch unsupported by current engine; switch to git"))
      (user-error "Patch unsupported by %s engine; switch to git" (carriage-apply-engine)))))

(defun carriage--dry-run-single-item (plan-item root)
  "Run dry-run for a single PLAN-ITEM under ROOT, setting UI state appropriately."
  (when (fboundp 'carriage-ui-apply-set-state)
    (carriage-ui-apply-set-state 'running "Dry-run…"))
  (carriage-dry-run-plan (list plan-item) root))

(defun carriage--announce-apply-success (report)
  "Show Messages summary for successful apply REPORT."
  (when (and report (not (bound-and-true-p noninteractive)))
    (let* ((items (or (plist-get report :items) '()))
           (oks (cl-remove-if-not (lambda (it) (eq (plist-get it :status) 'ok)) items))
           (created 0) (deleted 0) (renamed 0) (modified 0)
           (files '()))
      (dolist (it oks)
        (let ((op (plist-get it :op)))
          (pcase op
            ('create (setq created (1+ created)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
            ('delete (setq deleted (1+ deleted)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
            ('rename (setq renamed (1+ renamed)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
            ((or 'patch 'sre 'aibo 'replace) (setq modified (1+ modified)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
            (_ (push (or (plist-get it :file) (plist-get it :path) "-") files)))))
      (let* ((total (length oks))
             (files-str (mapconcat #'identity (nreverse (delete-dups (delq nil files))) ", ")))
        (when (> total 0)
          (message "Carriage: applied OK (%d items) — created:%d modified:%d deleted:%d renamed:%d — %s"
                   total created modified deleted renamed files-str))))))
(defun carriage--apply-async-enabled-p ()
  "Return non-nil when async apply is enabled for the current session."
  (and (boundp 'carriage-apply-async)
       carriage-apply-async
       (not noninteractive)))

(defun carriage--apply-report-success-p (report)
  "Return non-nil when REPORT indicates success (fail==0)."
  (let* ((sum (and (listp report) (plist-get report :summary)))
         (fails (or (and sum (plist-get sum :fail)) 0)))
    (and (numberp fails) (zerop fails))))

(defun carriage--apply-handle-finished-report (report)
  "Common post-processing for finished apply REPORT (open report, announce success)."
  (when (and report (not noninteractive))
    (carriage--report-open-maybe report)
    (when (carriage--apply-report-success-p report)
      (carriage--announce-apply-success report)))
  ;; Apply badge is updated from the apply report (carriage-ui-note-apply-report).
  report)

(defun carriage--apply-single-item-sync (plan-item root)
  "Apply a single PLAN-ITEM under ROOT synchronously and post-process the report."
  (carriage--apply-handle-finished-report
   (carriage-apply-plan (list plan-item) root)))

(defun carriage--apply-single-item-async (plan-item root)
  "Apply a single PLAN-ITEM under ROOT asynchronously and post-process the report."
  (carriage-log "apply-at-point: async apply scheduled")
  (carriage-apply-plan-async
   (list plan-item) root
   (lambda (rep)
     (carriage--apply-handle-finished-report rep))))

(defun carriage--apply-single-item-dispatch (plan-item root)
  "Apply single PLAN-ITEM under ROOT, async when configured; update UI/report."
  (let* ((op (or (and (listp plan-item) (plist-get plan-item :op)) (alist-get :op plan-item)))
         (path (or (alist-get :path plan-item) (alist-get :file plan-item)))
         (eng (carriage-apply-engine))
         (pol (and (eq eng 'git) (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)))
    (carriage-log "apply-dispatch: op=%s target=%s engine=%s policy=%s" op (or path "-") eng pol)
    (if (carriage--apply-async-enabled-p)
        (carriage--apply-single-item-async plan-item root)
      (carriage--apply-single-item-sync plan-item root))))

;;;###autoload
(defun carriage-apply-at-point ()
  "Dry-run → confirm → apply for the patch block at point (async by default)."
  (interactive)
  (let* ((root (or (carriage-project-root) default-directory))
         (plan-item (carriage--parse-plan-item-or-error root)))
    (carriage--guard-patch-engine-for-item plan-item)
    ;; Guard: forbid applying on WIP/ephemeral branches unless explicitly allowed
    (let* ((cur-br (ignore-errors (and (fboundp 'carriage-git-current-branch)
                                       (carriage-git-current-branch root))))
           (epref (or (and (boundp 'carriage-git-ephemeral-prefix)
                           carriage-git-ephemeral-prefix)
                      "carriage/tmp"))
           (wip (string= cur-br "carriage/WIP"))
           (eph (and (stringp cur-br) (string-prefix-p epref cur-br))))
      (when (and (or wip eph)
                 (not (and (boundp 'carriage-allow-apply-on-wip) carriage-allow-apply-on-wip)))
        (when (fboundp 'carriage-ui-apply-set-state)
          (carriage-ui-apply-set-state
           'apply-fail
           (format "Запрещено применять на ветке %s (WIP/ephemeral)" (or cur-br ""))))
        (user-error "Нельзя применять патчи на ветке %s (не слита с основной)" (or cur-br ""))))
    (let ((dry (carriage--dry-run-single-item plan-item root)))
      (when (not noninteractive)
        (carriage--report-open-maybe dry))
      (let* ((sum (plist-get dry :summary))
             (fails (or (plist-get sum :fail) 0)))
        (if (> fails 0)
            (progn
              ;; Apply badge is already updated from the dry-run report.
              (user-error "Dry-run failed; see report for details"))
          (when (or (not carriage-mode-confirm-apply)
                    (y-or-n-p "Apply this block? "))
            (when (fboundp 'carriage-ui-apply-set-state)
              (carriage-ui-apply-set-state 'running "Apply…"))
            (carriage--apply-single-item-dispatch plan-item root)))))))

;;;###autoload
(defun carriage-apply-at-point-or-region ()
  "Dry-run → confirm → apply the block at point; when region is active, apply all patch blocks in region as a group."
  (interactive)
  (if (use-region-p)
      (let* ((root (or (carriage-project-root) default-directory))
             (beg (region-beginning))
             (end (region-end))
             (plan (carriage-parse-blocks-in-region beg end root)))
        (when (or (null plan) (zerop (length plan)))
          (when (fboundp 'carriage-ui-apply-set-state)
            (carriage-ui-apply-set-state 'apply-fail "Нет patch-блоков в регионе"))
          (user-error "Нет patch-блоков в регионе"))
        ;; Early guard: 'patch requires git engine
        (let ((has-patch (cl-some (lambda (it)
                                    (eq (or (alist-get :op it)
                                            (and (listp it) (plist-get it :op)))
                                        'patch))
                                  plan)))
          (when (and has-patch (not (carriage--engine-supports-op-p 'patch)))
            (when (fboundp 'carriage-ui-apply-set-state)
              (carriage-ui-apply-set-state 'dry-fail "Patch unsupported by current engine; switch to git"))
            (user-error "Patch unsupported by %s engine; switch to git" (carriage-apply-engine))))
        ;; Guard: forbid applying on WIP/ephemeral branches unless explicitly allowed
        (let* ((cur-br (ignore-errors (and (fboundp 'carriage-git-current-branch)
                                           (carriage-git-current-branch root))))
               (epref (or (and (boundp 'carriage-git-ephemeral-prefix)
                               carriage-git-ephemeral-prefix)
                          "carriage/tmp"))
               (wip (string= cur-br "carriage/WIP"))
               (eph (and (stringp cur-br) (string-prefix-p epref cur-br))))
          (when (and (or wip eph)
                     (not (and (boundp 'carriage-allow-apply-on-wip) carriage-allow-apply-on-wip)))
            (when (fboundp 'carriage-ui-apply-set-state)
              (carriage-ui-apply-set-state
               'apply-fail
               (format "Запрещено применять на ветке %s (WIP/ephemeral)" (or cur-br ""))))
            (user-error "Нельзя применять патчи на ветке %s (не слита с основной)" (or cur-br ""))))
        ;; Dry-run group (local apply/dry-run badge only)
        (when (fboundp 'carriage-ui-apply-set-state)
          (carriage-ui-apply-set-state 'running "Dry-run…"))
        (let* ((dry (carriage-dry-run-plan plan root)))
          (when (not noninteractive)
            (carriage--report-open-maybe dry))
          (let* ((sum (plist-get dry :summary))
                 (fails (or (plist-get sum :fail) 0)))
            (if (> fails 0)
                (progn
                  ;; Apply badge is already updated from the dry-run report.
                  (user-error "Dry-run провалился для части блоков; смотрите отчёт"))
              (when (or (not carriage-mode-confirm-apply)
                        (y-or-n-p "Применить группу блоков? "))
                (when (fboundp 'carriage-ui-apply-set-state)
                  (carriage-ui-apply-set-state 'running "Apply…"))
                ;; Force sync for grouped apply
                (let ((carriage-apply-async nil))
                  (let ((ap (carriage-apply-plan plan root)))
                    (when (not noninteractive)
                      (carriage--report-open-maybe ap))
                    (when (and (not noninteractive)
                               (let* ((sum (plist-get ap :summary)))
                                 (and sum (zerop (or (plist-get sum :fail) 0)))))
                      (carriage--announce-apply-success ap))
                    ;; Apply badge is updated from the apply report (carriage-ui-note-apply-report).
                    ap)))))))
    (call-interactively #'carriage-apply-at-point)))

;;;###autoload
(defun carriage-apply-last-iteration ()
  "Dry-run → подтверждение → применение всех блоков «последней итерации».
Последняя итерация определяется как все #+begin_patch блоки, расположенные
строго ниже последней строки `#+CARRIAGE_FINGERPRINT:` в текущем буфере."
  (interactive)
  (let* ((root (or (carriage-project-root) default-directory))
         (fp-pos (save-excursion
                   (let ((last nil)
                         (case-fold-search t))
                     (goto-char (point-min))
                     (while (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT:" nil t)
                       (setq last (line-beginning-position)))
                     last)))
         (plan (when (numberp fp-pos)
                 (carriage-parse-blocks-in-region fp-pos (point-max) root))))
    (when (or (null fp-pos) (null plan) (zerop (length plan)))
      (when (fboundp 'carriage-ui-apply-set-state)
        (carriage-ui-apply-set-state 'apply-fail "Нет последнего отпечатка (CARRIAGE_FINGERPRINT) или нет патчей ниже него"))
      (user-error "Нет последнего отпечатка (CARRIAGE_FINGERPRINT) или нет патчей ниже него"))
    (when (and carriage-mode-confirm-apply-all
               (not (y-or-n-p (format "Применить все блоки (%d)? " (length plan)))))
      (when (fboundp 'carriage-ui-apply-set-state)
        (carriage-ui-apply-set-state 'aborted "Отменено пользователем"))
      (user-error "Отменено"))
    ;; Guard engine support for patch
    (let ((has-patch (seq-some
                      (lambda (it)
                        (eq (or (alist-get :op it)
                                (and (listp it) (plist-get it :op)))
                            'patch))
                      plan)))
      (when (and has-patch (not (carriage--engine-supports-op-p 'patch)))
        (when (fboundp 'carriage-ui-apply-set-state)
          (carriage-ui-apply-set-state 'dry-fail "Patch unsupported by current engine; switch to git"))
        (user-error "Patch unsupported by %s engine; switch to git" (carriage-apply-engine))))
    ;; Guard WIP/ephemeral branches
    (let* ((cur-br (ignore-errors (and (fboundp 'carriage-git-current-branch)
                                       (carriage-git-current-branch root))))
           (epref (or (and (boundp 'carriage-git-ephemeral-prefix)
                           carriage-git-ephemeral-prefix)
                      "carriage/tmp"))
           (wip (string= cur-br "carriage/WIP"))
           (eph (and (stringp cur-br) (string-prefix-p epref cur-br))))
      (when (and (or wip eph)
                 (not (and (boundp 'carriage-allow-apply-on-wip) carriage-allow-apply-on-wip)))
        (when (fboundp 'carriage-ui-apply-set-state)
          (carriage-ui-apply-set-state
           'apply-fail
           (format "Запрещено применять на ветке %s (WIP/ephemeral)" (or cur-br ""))))
        (user-error "Нельзя применять патчи на ветке %s (не слита с основной)" (or cur-br ""))))
    ;; Dry-run → apply (local apply/dry-run badge only)
    (when (fboundp 'carriage-ui-apply-set-state)
      (carriage-ui-apply-set-state 'running "Dry-run…"))
    (let* ((dry (carriage-dry-run-plan plan root)))
      (when (not noninteractive)
        (carriage--report-open-maybe dry))
      (let* ((sum (plist-get dry :summary))
             (fails (or (plist-get sum :fail) 0)))
        (if (> fails 0)
            (progn
              ;; Apply badge is already updated from the dry-run report.
              (user-error "Dry-run провалился для части блоков; смотрите отчёт"))
          (when (or (not carriage-mode-confirm-apply-all)
                    (y-or-n-p "Применить группу блоков? "))
            (when (fboundp 'carriage-ui-apply-set-state)
              (carriage-ui-apply-set-state 'running "Apply…"))
            (let ((carriage-apply-async nil))
              (if (and (boundp 'carriage-apply-async) carriage-apply-async (not noninteractive))
                  (progn
                    (carriage-log "apply-all: async apply scheduled (%d items)" (length plan))
                    (carriage-apply-plan-async
                     plan root
                     (lambda (rep)
                       (when (not noninteractive)
                         (carriage--report-open-maybe rep))
                       ;; Apply badge is updated from the apply report.
                       rep)))
                (let ((ap (carriage-apply-plan plan root)))
                  (when (not noninteractive)
                    (carriage--report-open-maybe ap))
                  (when (and (not noninteractive)
                             (let* ((sum2 (plist-get ap :summary)))
                               (and sum2 (zerop (or (plist-get sum2 :fail) 0)))))
                    (carriage--announce-apply-success ap))
                  ;; Apply badge is updated from the apply report.
                  ap)))))))))


;;;###autoload
(defun carriage-wip-checkout ()
  "Create or switch to the WIP branch in the current repository."
  (interactive)
  (let ((root (carriage-project-root)))
    (unless root
      (user-error "Git repository not detected"))
    (carriage-git-checkout-wip root)
    (message "Carriage: switched to WIP branch in %s" root)))

;;;###autoload
(defun carriage-wip-reset-soft (&optional rev)
  "Soft reset last commit (default REV is HEAD~1) in WIP."
  (interactive)
  (let ((root (carriage-project-root)))
    (unless root
      (user-error "Git repository not detected"))
    (carriage-git-reset-soft root (or rev "HEAD~1"))
    (message "Carriage: soft reset to %s" (or rev "HEAD~1"))))

(defun carriage--commit--default-message ()
  "Return default commit message from `carriage-commit-default-message'."
  (let ((v (and (boundp 'carriage-commit-default-message) carriage-commit-default-message)))
    (cond
     ((functionp v) (ignore-errors (funcall v)))
     ((stringp v) v)
     (t "carriage: apply changes"))))

;;;###autoload
(defun carriage-commit-changes (&optional message)
  "Commit all changes according to staging policy as a single commit.
- If staging policy is 'none, stage everything (git add -A) before commit.
- If staging policy is 'index, commit current index as-is."
  (interactive)
  (let* ((root (carriage-project-root))
         (msg (or message (read-string "Commit message: " (carriage--commit--default-message)))))
    (unless root
      (user-error "Git repository not detected"))
    (when (not (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index))
      (ignore-errors (carriage-git-add-all root)))
    (let* ((res (carriage-git-commit root msg))
           (exit (plist-get res :exit))
           (stderr (string-trim (or (plist-get res :stderr) ""))))
      (if (and exit (zerop exit))
          (message "Carriage: committed changes")
        (user-error "Commit failed: %s" (if (string-empty-p stderr) (format "%S" res) stderr))))))

;;;###autoload
(defun carriage-commit-last-iteration (&optional message)
  "Commit only files changed by the last iteration as a single commit.
Stages as needed depending on staging policy; with 'none, runs git add -A then restricts commit to those paths."
  (interactive)
  (let* ((root (carriage-project-root)))
    (unless root
      (user-error "Git repository not detected"))
    (let* ((plan (carriage-collect-last-iteration-blocks root))
           (files
            (cl-loop for it in plan
                     for op = (alist-get :op it)
                     append
                     (pcase op
                       ((or 'sre 'create 'delete)
                        (let ((f (alist-get :file it))) (if f (list f) '())))
                       ('patch
                        (let ((p (alist-get :path it))) (if p (list p) '())))
                       ('rename
                        (delq nil (list (alist-get :from it) (alist-get :to it))))
                       (_ '()))))
           (msg (or message (read-string "Commit message: " (carriage--commit--default-message)))))
      (when (null files)
        (user-error "No files in last iteration"))
      ;; Stage as necessary
      (when (not (eq (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy) 'index))
        (ignore-errors (carriage-git-add-all root)))
      ;; Commit restricted to files
      (let* ((res (apply #'carriage-git-commit root msg files))
             (exit (plist-get res :exit))
             (stderr (string-trim (or (plist-get res :stderr) ""))))
        (if (and exit (zerop exit))
            (message "Carriage: committed last iteration (%d file(s))" (length files))
          (user-error "Commit failed: %s" (if (string-empty-p stderr) (format "%S" res) stderr)))))))

;;;###autoload
(defun carriage-toggle-intent ()
  "Cycle Intent: Ask → Code → Hybrid → Ask."
  (interactive)
  (setq carriage-mode-intent
        (pcase carriage-mode-intent
          ('Ask 'Code)
          ('Code 'Hybrid)
          (_ 'Ask)))
  (message "Carriage intent: %s" carriage-mode-intent)
  (force-mode-line-update t))

;;;###autoload
(defun carriage-select-suite (&optional suite)
  "Select Suite (sre|aibo|udiff)."
  (interactive)
  (let* ((choices
          (condition-case _e
              (let ((ids (and (fboundp 'carriage-suite-ids) (carriage-suite-ids))))
                (if (and ids (listp ids))
                    (mapcar (lambda (s) (if (symbolp s) (symbol-name s) (format "%s" s))) ids)
                  '("sre" "aibo" "udiff")))
            (error '("sre" "aibo" "udiff"))))
         (default (if (symbolp carriage-mode-suite)
                      (symbol-name carriage-mode-suite)
                    (or carriage-mode-suite "aibo")))
         (sel (or suite (completing-read "Suite: " choices nil t default))))
    (setq carriage-mode-suite (intern sel))
    (message "Carriage suite: %s" carriage-mode-suite)
    (force-mode-line-update t)))

;;;###autoload
(defun carriage-select-model (&optional model)
  "Select LLM MODEL for Carriage, optionally as \"backend:model\".

When registry has entries, offer completion over:
- current backend's models, and
- combined \"backend:model\" candidates.

If the choice is \"backend:model\" (or \"backend:provider:model\"), backend and model are updated.
Falls back to plain string prompt when registry is empty."
  (interactive)
  ;; Ensure at least the current backend:model is registered for completion.
  (when (require 'carriage-llm-registry nil t)
    (let* ((bsym (if (symbolp carriage-mode-backend)
                     carriage-mode-backend
                   (intern (format "%s" carriage-mode-backend))))
           (backs (carriage-llm-available-backends)))
      (unless (member (symbol-name bsym) backs)
        (carriage-llm-register-backend bsym :models (list carriage-mode-model)))))
  (let* ((bcur (if (symbolp carriage-mode-backend)
                   (symbol-name carriage-mode-backend)
                 (or carriage-mode-backend "")))
         (models (carriage-llm-available-models (and (stringp bcur) (intern bcur))))
         (pairs  (carriage-llm-candidates))
         ;; Offer both combined and plain model names; keep gptel pairs first.
         (collection (delete-dups (append (or pairs '()) (or models '()))))
         (def-full (and collection
                        (carriage-llm-default-candidate bcur carriage-mode-model pairs carriage-mode-provider)))
         (def-full-sane (if (and (stringp def-full)
                                 (fboundp 'carriage-llm--dedupe-leading-backend))
                            (carriage-llm--dedupe-leading-backend def-full)
                          def-full))
         (prompt (if collection
                     (format "Model (or backend:model) [%s]: " bcur)
                   "Model: "))
         (choice (or model
                     (if collection
                         (let* ((initial (if (and (stringp carriage-mode-model)
                                                  (string= carriage-mode-model "gptel-default"))
                                             ""
                                           def-full-sane))
                                (def (unless (and (stringp carriage-mode-model)
                                                  (string= carriage-mode-model "gptel-default"))
                                       def-full-sane)))
                           (completing-read prompt collection nil t initial nil def))
                       (let ((initial (if (and (stringp carriage-mode-model)
                                               (string= carriage-mode-model "gptel-default"))
                                          ""
                                        def-full-sane)))
                         (read-string prompt initial))))))
    ;; Apply selection:
    ;; When choice contains ':', treat first segment as backend and last segment as model;
    ;; otherwise treat it as plain model (keep backend unchanged).
    (let* ((parts (when (stringp choice) (split-string choice ":" t)))
           (backend  (and parts (car parts)))
           (provider (and (>= (length parts) 3) (nth 1 parts)))
           (model-str (if (and parts (>= (length parts) 2))
                          (car (last parts))
                        choice)))
      (when (and backend (not (string-empty-p backend)))
        (setq carriage-mode-backend (intern backend)))
      (setq carriage-mode-provider (and (stringp provider) (not (string-empty-p provider)) provider))
      (setq carriage-mode-model model-str)
      ;; Ensure registry has backend->model mapping for future completion.
      (when (require 'carriage-llm-registry nil t)
        (let* ((bsym (if (symbolp carriage-mode-backend)
                         carriage-mode-backend
                       (intern (format "%s" carriage-mode-backend))))
               (existing (or (carriage-llm-available-models bsym) '())))
          (carriage-llm-register-backend bsym :models (delete-dups (cons carriage-mode-model existing)))))
      (message "Carriage model: %s (backend %s%s)"
               carriage-mode-model
               (if (symbolp carriage-mode-backend)
                   (symbol-name carriage-mode-backend)
                 carriage-mode-backend)
               (if carriage-mode-provider
                   (format " provider %s" carriage-mode-provider)
                 ""))
      (force-mode-line-update t)
      (cons carriage-mode-backend carriage-mode-model))))

;;;###autoload
(defun carriage-select-backend (&optional backend)
  "Select LLM transport BACKEND for Carriage.

When registry is available, completion is offered over registered backends.
Otherwise falls back to a free-form prompt. Stores backend as a symbol."
  (interactive)
  ;; Ensure registry exists; preseed with current backend:model if empty.
  (when (require 'carriage-llm-registry nil t)
    (let ((backs (carriage-llm-available-backends)))
      (when (null backs)
        (let* ((bsym (if (symbolp carriage-mode-backend)
                         carriage-mode-backend
                       (intern (format "%s" carriage-mode-backend)))))
          (carriage-llm-register-backend bsym :models (list carriage-mode-model))))))
  (let* ((backs (carriage-llm-available-backends))
         (choice (or backend
                     (if backs
                         (completing-read "Backend: " backs nil t
                                          (if (symbolp carriage-mode-backend)
                                              (symbol-name carriage-mode-backend)
                                            (or carriage-mode-backend "")))
                       (read-string "Backend (symbol or string): ")))))
    (setq carriage-mode-backend
          (cond
           ((symbolp choice) choice)
           ((stringp choice) (intern choice))
           (t (carriage-llm--norm-backend choice))))
    ;; Make sure selected backend is present in registry with current model for backend:model completion.
    (when (require 'carriage-llm-registry nil t)
      (let* ((bsym (if (symbolp carriage-mode-backend)
                       carriage-mode-backend
                     (intern (format "%s" carriage-mode-backend))))
             (existing (or (carriage-llm-available-models bsym) '())))
        (carriage-llm-register-backend bsym :models (delete-dups (cons carriage-mode-model existing)))))
    (message "Carriage backend set to: %s"
             (if (symbolp carriage-mode-backend)
                 (symbol-name carriage-mode-backend)
               carriage-mode-backend))))

;;;###autoload
(defun carriage-save-settings ()
  "Manually save Carriage state from this buffer into the document."
  (interactive)
  (require 'carriage-doc-state nil t)
  (let ((ok (ignore-errors (carriage-doc-state-write-current))))
    (if ok
        (message "Carriage: настройки сохранены")
      (message "Carriage: не удалось сохранить настройки"))))

;;; Navigation placeholders

(defun carriage-next-patch-block ()
  "Jump to next patch block (placeholder)."
  (interactive)
  (message "carriage-next-patch-block: not implemented yet"))

(defun carriage-prev-patch-block ()
  "Jump to previous patch block (placeholder)."
  (interactive)
  (message "carriage-prev-patch-block: not implemented yet"))

(defun carriage--extract-patch-blocks (text)
  "Extract all #+begin_patch ... #+end_patch blocks from TEXT.
Return a single string with blocks concatenated by blank lines."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let* ((chunks '()))
      (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
        (let* ((beg (match-beginning 0)))
          (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
            (goto-char (point-max)))
          (let* ((end (line-end-position)))
            (push (buffer-substring-no-properties beg end) chunks))
          (forward-line 1)))
      (mapconcat #'identity (nreverse chunks) "\n\n"))))

(defun carriage--sanitize-llm-response (raw)
  "Return only sanitized #+begin_patch blocks from RAW.

Sanitization rules:
- Keep only begin_patch blocks; drop any text outside blocks.
- For :op create: use the raw body between begin/end (no delimiter markers, no rewriting)."
  (with-temp-buffer
    (insert (or raw ""))
    (goto-char (point-min))
    (let ((acc '()))
      (while (re-search-forward "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" nil t)
        (let* ((hdr-str (match-string 1))
               (hdr     (car (read-from-string hdr-str)))
               (op      (plist-get hdr :op))
               (opstr   (format "%s" op))
               (body-beg (progn (forward-line 1) (point)))
               (body-end (progn
                           (unless (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
                             (goto-char (point-max)))
                           (line-beginning-position)))
               (body0 (buffer-substring-no-properties body-beg body-end)))
          (when (string-prefix-p ":" (or opstr ""))
            (setq opstr (substring opstr 1)))
          ;; Normalize header for create: drop legacy :delim if present.
          (let* ((hdr1 (if (string= opstr "create")
                           (let ((pl hdr) (res nil))
                             (while pl
                               (let ((k (car pl)) (v (cadr pl)))
                                 (unless (eq k :delim)
                                   (setq res (append res (list k v)))))
                               (setq pl (cddr pl)))
                             res)
                         hdr)))
            ;; For :op create, strip legacy DELIM markers and an accidental trailing "#+end"
            ;; that models sometimes emit inside the body.
            (let* ((sanitized-body
                    (if (string= opstr "create")
                        (let* ((lines (split-string body0 "\n" nil))
                               (rx-head "^[ \t]*<<[0-9a-f]\\{6\\}[ \t]*$")
                               (rx-tail "^[ \t]*:[0-9a-f]\\{6\\}[ \t]*$")
                               (rx-stray-end "^[ \t]*#\\+end[ \t]*$")
                               ;; drop leading "<<hex"
                               (lines1 (if (and lines (string-match-p rx-head (car lines)))
                                           (cdr lines)
                                         lines))
                               ;; drop one trailing ":hex"
                               (lines2 (let ((lst lines1))
                                         (when (and lst (string-match-p rx-tail (or (car (last lst)) "")))
                                           (setq lst (butlast lst 1)))
                                         lst))
                               ;; drop one trailing stray "#+end"
                               (lines3 (let ((lst lines2))
                                         (when (and lst (string-match-p rx-stray-end (or (car (last lst)) "")))
                                           (setq lst (butlast lst 1)))
                                         lst)))
                          (mapconcat #'identity lines3 "\n"))
                      body0))
                   (hdr-print (prin1-to-string hdr1))
                   (block (concat "#+begin_patch " hdr-print "\n" sanitized-body "\n#+end_patch\n")))
              (push block acc))))
        (forward-line 1))
      (mapconcat #'identity (nreverse acc) "\n"))))

(defun carriage--accept-insertion-target (insert-marker)
  "Return (cons BUFFER . POS) for insertion target based on INSERT-MARKER."
  (if (and (markerp insert-marker)
           (buffer-live-p (marker-buffer insert-marker)))
      (cons (marker-buffer insert-marker) (marker-position insert-marker))
    (cons (current-buffer) nil)))

(defun carriage--insert-blocks-and-mark (buf pos blocks)
  "Insert BLOCKS into BUF at POS (or point if nil), mark last iteration.
Return cons (BEG . END) of inserted region."
  (with-current-buffer buf
    (save-excursion
      (when pos (goto-char pos))
      (let* ((ins-beg (point)))
        (unless (bolp) (insert "\n"))
        (insert blocks "\n")
        (let* ((ins-end (point)))
          (carriage-log "accept: inserted region %d..%d (%d chars)"
                        ins-beg ins-end (- ins-end ins-beg))
          ;; O(1) modeline support: this acceptance inserted patch blocks.
          (when (and (stringp blocks)
                     (string-match-p "#\\+begin_patch\\b" blocks))
            (setq-local carriage--last-iteration-has-patches t))
          (cons ins-beg ins-end))))))

(defun carriage--dry-run-last-iteration (root)
  "Collect last-iteration blocks for ROOT, run dry-run, open report if configured.
Return the dry-run report plist.

UI rule: this is a local dry-run/apply concern and must not affect request state."
  (carriage-log "accept: collecting last-iteration blocks…")
  (let* ((plan (carriage-collect-last-iteration-blocks root)))
    (carriage-log "accept: plan-size=%d" (length plan))
    (when (fboundp 'carriage-ui-apply-set-state)
      (carriage-ui-apply-set-state 'running "Dry-run…"))
    (let ((rep (carriage-dry-run-plan plan root)))
      (when (and carriage-mode-auto-open-report (not noninteractive))
        (carriage-report-open rep))
      ;; Apply badge is updated from the dry-run report.
      rep)))

;;;###autoload
(defun carriage-accept-llm-response (&optional input insert-marker)
  "Accept an LLM response INPUT, keep only begin_patch blocks, insert and dry-run.

- Sanitizes begin_patch blocks (enforces :delim only for create).
- Inserts into buffer at INSERT-MARKER or point.
- Marks the region as the last iteration and runs dry-run with report."
  (interactive
   (list (read-string "Paste LLM response (only begin_patch blocks will be kept):\n")))
  (let* ((raw (or input ""))
         (sanitized (carriage--sanitize-llm-response raw))
         (blocks (carriage--extract-patch-blocks sanitized)))
    (when (or (null blocks) (string-empty-p (string-trim blocks)))
      (user-error "No begin_patch blocks found in input"))
    (let* ((target (carriage--accept-insertion-target insert-marker))
           (buf (car target))
           (pos (cdr target)))
      (with-current-buffer buf
        (let* ((root (or (carriage-project-root) default-directory)))
          (carriage-traffic-log 'in "Accepted LLM response (%d chars)" (length raw))
          (carriage-log "accept: extracted blocks bytes=%d" (string-bytes blocks))
          (ignore (carriage--insert-blocks-and-mark buf pos blocks))
          (carriage--dry-run-last-iteration root))))))

;;; Toggles and helpers (UI accessibility)

;;;###autoload
(defun carriage-abort-current ()
  "Abort current Carriage request/apply if one is active.

In v1 this calls a buffer-local handler registered by the transport/pipeline.
If no handler is present, stops UI spinner and reports no active request."
  (interactive)
  (let ((handler carriage--abort-handler))
    (cond
     ((functionp handler)
      ;; Clear handler first to avoid reentrancy; then attempt abort.
      (setq carriage--abort-handler nil)
      (condition-case err
          (funcall handler)
        (error
         (message "Abort handler error: %s" (error-message-string err))))
      (when (fboundp 'carriage-ui--spinner-stop)
        (carriage-ui--spinner-stop t))
      (when (fboundp 'carriage--preloader-stop)
        (carriage--preloader-stop))
      (when (fboundp 'carriage-ui-set-state)
        (carriage-ui-set-state 'idle))
      (message "Carriage: abort requested"))
     (t
      (when (fboundp 'carriage-ui--spinner-stop)
        (carriage-ui--spinner-stop t))
      (when (fboundp 'carriage--preloader-stop)
        (carriage--preloader-stop))
      (when (fboundp 'carriage-ui-set-state)
        (carriage-ui-set-state 'idle))
      (message "Нет активного запроса")))))

(defun carriage-register-abort-handler (fn)
  "Register buffer-local abort handler FN and return an unregister lambda.
FN must be a zero-argument function that cancels the ongoing activity."
  (setq carriage--abort-handler fn)
  (force-mode-line-update t)
  (lambda ()
    (when (eq carriage--abort-handler fn)
      (setq carriage--abort-handler nil)
      (force-mode-line-update t))))

(defun carriage-clear-abort-handler ()
  "Clear buffer-local abort handler if any."
  (setq carriage--abort-handler nil)
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-auto-open-report ()
  "Toggle auto-opening report after dry-run."
  (interactive)
  (setq-local carriage-mode-auto-open-report (not carriage-mode-auto-open-report))
  (message "Auto-open report: %s" (if carriage-mode-auto-open-report "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-show-diffs ()
  "Toggle requirement to show diffs before apply."
  (interactive)
  (setq-local carriage-mode-show-diffs (not carriage-mode-show-diffs))
  (message "Show diffs before apply: %s" (if carriage-mode-show-diffs "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-confirm-apply-all ()
  "Toggle confirmation before applying all blocks (C-c e A)."
  (interactive)
  (setq-local carriage-mode-confirm-apply-all (not carriage-mode-confirm-apply-all))
  (message "Confirm apply-all: %s" (if carriage-mode-confirm-apply-all "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-use-icons ()
  "Toggle using icons in the UI (requires all-the-icons)."
  (interactive)
  (setq-local carriage-mode-use-icons (not carriage-mode-use-icons))
  (message "Use icons: %s" (if carriage-mode-use-icons "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-include-gptel-context ()
  "Toggle including gptel-context (buffers/files) into the request context."
  (interactive)
  (setq-local carriage-mode-include-gptel-context (not carriage-mode-include-gptel-context))
  (when (fboundp 'carriage-ui--reset-context-cache)
    (carriage-ui--reset-context-cache))
  (message "Include gptel-context: %s" (if carriage-mode-include-gptel-context "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-include-doc-context ()
  "Toggle including file contents from the nearest #+begin_context block in the document."
  (interactive)
  (setq-local carriage-mode-include-doc-context (not carriage-mode-include-doc-context))
  (when (fboundp 'carriage-ui--reset-context-cache)
    (carriage-ui--reset-context-cache))
  (message "Include #+begin_context files: %s" (if carriage-mode-include-doc-context "on" "off"))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-include-visible-context ()
  "Toggle including visible buffers (current frame) into the request context."
  (interactive)
  (setq-local carriage-mode-include-visible-context
              (not (and (boundp 'carriage-mode-include-visible-context)
                        carriage-mode-include-visible-context)))
  (when (fboundp 'carriage-ui--reset-context-cache)
    (carriage-ui--reset-context-cache))
  (message "Include visible buffers: %s"
           (if (and (boundp 'carriage-mode-include-visible-context)
                    carriage-mode-include-visible-context)
               "on" "off"))
  (force-mode-line-update t))

;; -------------------------------------------------------------------
;; Project-scoped ephemeral buffer (open-buffer) and exit prompt

(defvar carriage--project-buffers (make-hash-table :test 'equal)
  "Map of project-root (string) → live buffer for Carriage ephemeral project buffers.")

(defvar carriage--ephemeral-exit-hook-installed nil
  "Guard to install kill-emacs query hook only once per session.")

(defvar-local carriage--ephemeral-project-buffer nil
  "Non-nil in buffers created by =carriage-open-buffer' (ephemeral, not visiting a file by default).")

(defun carriage--project-name-from-root (root)
  "Return project name (basename of ROOT directory)."
  (file-name-nondirectory (directory-file-name (or root default-directory))))

(defun carriage--ensure-ephemeral-exit-hook ()
  "Install a kill-emacs query hook to offer saving ephemeral project buffers."
  (unless carriage--ephemeral-exit-hook-installed
    (setq carriage--ephemeral-exit-hook-installed t)
    (add-hook
     'kill-emacs-query-functions
     (lambda ()
       (let* ((bufs (cl-loop for k being the hash-keys of carriage--project-buffers
                             for b = (gethash k carriage--project-buffers)
                             when (and (buffer-live-p b)
                                       (with-current-buffer b
                                         (and carriage--ephemeral-project-buffer
                                              (buffer-modified-p))))
                             collect b))
              (need (and bufs (> (length bufs) 0))))
         (if (not need)
             t
           (when (y-or-n-p "Carriage: save ephemeral project buffers to files before exit? ")
             (dolist (b bufs)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (when (and carriage--ephemeral-project-buffer
                              (buffer-modified-p))
                     ;; Offer a filename interactively
                     (call-interactively #'write-file))))))
           t))))))

;;;###autoload
(defun carriage-open-buffer ()
  "Open or switch to the Carriage ephemeral buffer for the current project.
Creates an org-mode buffer with carriage-mode enabled and default-directory bound to the project root."
  (interactive)
  (let* ((root (or (carriage-project-root) default-directory))
         (pname (carriage--project-name-from-root root))
         (bname (format "*carriage:%s*" (or (and pname (not (string-empty-p pname)) pname) "-")))
         (existing (gethash root carriage--project-buffers))
         (buf (if (and (buffer-live-p existing)) existing (get-buffer-create bname))))
    (puthash root buf carriage--project-buffers)
    (carriage--ensure-ephemeral-exit-hook)
    (unless (get-buffer-window buf t)
      (pop-to-buffer buf))
    (with-current-buffer buf
      (setq default-directory (file-name-as-directory (expand-file-name root)))
      (setq carriage--ephemeral-project-buffer t)
      (unless (derived-mode-p 'org-mode)
        (ignore-errors (org-mode)))
      ;; Ensure carriage-mode is enabled
      (unless (bound-and-true-p carriage-mode)
        (carriage-mode 1)))
    buf))

;;; File chat buffers (one per source file)

(defvar carriage--file-chat-buffers (make-hash-table :test 'equal)
  "Map of absolute truenames → live buffers for Carriage file-chat buffers.")

(defun carriage--file-chat--ensure-context-block (abs-path)
  "Ensure current buffer contains a single begin_context block with ABS-PATH.
If no begin_context is present, insert a minimal header and block at point-max."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (unless (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (let ((title (format "File chat: %s" (file-name-nondirectory abs-path))))
          (insert (format "#+title: %s\n\n" title)))
        (insert "#+begin_context\n")
        ;; One absolute path per spec; leading space matches common formatting.
        (insert (format " %s\n" abs-path))
        (insert "#+end_context\n")))))

;;;###autoload
(defun carriage-open-file-chat ()
  "Open or switch to a unique Carriage chat buffer for the current file.
- Intent is set to Ask.
- Only document begin_context is enabled (GPTel/visible context disabled).
- A begin_context block with the absolute file path is ensured."
  (interactive)
  (let* ((file buffer-file-name))
    (unless (and (stringp file) (file-exists-p file))
      (user-error "Текущий буфер не посещает локальный файл"))
    (when (file-remote-p file)
      (user-error "Remote/TRAMP файлы не поддерживаются для File chat"))
    (let* ((abs (file-truename file))
           (root (or (carriage-project-root) default-directory))
           (buf (gethash abs carriage--file-chat-buffers))
           (name (format "*carriage-file:%s*" (file-name-nondirectory abs))))
      (if (and (bufferp buf) (buffer-live-p buf))
          (pop-to-buffer buf)
        (setq buf (get-buffer-create name))
        (puthash abs buf carriage--file-chat-buffers)
        (pop-to-buffer buf)
        (with-current-buffer buf
          (setq default-directory (file-name-as-directory (expand-file-name root)))
          (unless (derived-mode-p 'org-mode)
            (ignore-errors (org-mode)))
          (ignore-errors (carriage-mode 1))
          ;; Configure intent and context toggles (buffer-local)
          (setq-local carriage-mode-intent 'Ask)
          (setq-local carriage-mode-include-doc-context t)
          (setq-local carriage-mode-include-gptel-context nil)
          (setq-local carriage-mode-include-visible-context nil)
          ;; Ensure begin_context contains the absolute path
          (carriage--file-chat--ensure-context-block abs)
          (goto-char (point-min)))))
    (current-buffer)))

;; Freeze Carriage UI during transient menus to reduce redisplay churn.
(when (fboundp 'transient--recursive-edit)
  (defvar-local carriage--ui-pre-transient-state nil
    "Saved UI state before entering transient for this buffer.")

  (advice-add
   'transient--recursive-edit :around
   (lambda (orig &rest args)
     (let ((srcbuf (current-buffer))
           (saved-state (and (boundp 'carriage--ui-state) carriage--ui-state)))
       (when (bound-and-true-p carriage-mode)
         (setq carriage--ui-pre-transient-state saved-state)
         (when (fboundp 'carriage-ui--spinner-stop)
           (ignore-errors (carriage-ui--spinner-stop t)))
         (when (fboundp 'carriage--preloader-stop)
           (ignore-errors (carriage--preloader-stop))))
       (unwind-protect
           (apply orig args)
         (when (buffer-live-p srcbuf)
           (with-current-buffer srcbuf
             (when (bound-and-true-p carriage-mode)
               (let ((st carriage--ui-pre-transient-state))
                 (setq carriage--ui-pre-transient-state nil)
                 (when st (ignore-errors (carriage-ui-set-state st))))))))))))

;; Batch-friendly success announcement on report open
(with-eval-after-load 'carriage-report
  (defun carriage--announce--after-report-open (report &rest _ignore)
    (condition-case _e
        (let* ((sum (plist-get report :summary))
               (fails (or (plist-get sum :fail) 0))
               (items (or (plist-get report :items) '()))
               (oks (cl-remove-if-not (lambda (it) (eq (plist-get it :status) 'ok)) items)))
          (when (and (numberp fails) (zerop fails) (> (length oks) 0))
            (let ((created 0) (deleted 0) (renamed 0) (modified 0)
                  (files '()))
              (dolist (it oks)
                (let ((op (plist-get it :op)))
                  (pcase op
                    ('create (setq created (1+ created)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
                    ('delete (setq deleted (1+ deleted)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
                    ('rename (setq renamed (1+ renamed)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
                    ((or 'patch 'sre 'aibo) (setq modified (1+ modified)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
                    (_ (push (or (plist-get it :file) (plist-get it :path) "-") files)))))
              (let* ((total (length oks))
                     (files-str (mapconcat #'identity (nreverse (delete-dups (delq nil files))) ", ")))
                (message "Carriage: applied OK (%d items) — created:%d modified:%d deleted:%d renamed:%d — %s"
                         total created modified deleted renamed files-str)))))
      (error nil)))
  (advice-add 'carriage-report-open :after #'carriage--announce--after-report-open))

;; Persist document state after key parameter changes (advice hooks).
(with-eval-after-load 'carriage-doc-state
  (defvar carriage--doc-state--advice-targets
    '(;; Core state
      carriage-toggle-intent
      carriage-select-suite
      carriage-select-model
      carriage-select-backend
      carriage-select-apply-engine

      ;; Context sources (GPTel/Doc/Visible/etc.)
      carriage-toggle-include-gptel-context
      carriage-toggle-include-doc-context
      carriage-toggle-include-visible-context
      carriage-toggle-include-patched-files

      ;; Doc-context scope selectors (AllCtx/LastCtx/etc.)
      carriage-select-doc-context-all
      carriage-select-doc-context-last
      carriage-toggle-doc-context-scope
      carriage-select-doc-context-scope
      ;; Extra/legacy entry-points (advised only when they exist)
      carriage-select-doc-context
      carriage-toggle-doc-context-all
      carriage-toggle-doc-context-last
      ;; Back-compat typo/old name (safe: advised only if fboundp)
      carriage_toggle-doc-context-scope

      ;; Context profile / presets
      carriage-toggle-context-profile
      carriage-context-profile-set
      carriage-context-profile-select

      ;; UI/behaviour toggles that must persist too
      carriage-toggle-auto-open-report
      carriage-toggle-show-diffs
      carriage-toggle-confirm-apply-all
      carriage-toggle-confirm-apply
      carriage-toggle-use-icons
      carriage-toggle-flash-patches
      carriage-toggle-audio-notify)
    "Commands that should trigger doc-state persistence when invoked.

This list must include *all* interactive commands that change persisted state,
including doc-context scope (LastCtx/AllCtx) and context source toggles.
Persistence is best-effort; invalid/unreadable CARRIAGE_STATE must never break commands.")

  (defvar carriage--doc-state--advised nil
    "List of commands that already have doc-state persistence advice installed.")

  (defun carriage--doc-state-write-safe (&rest _)
    "Persist document state if doc-state sync is enabled; never signal.
If CARRIAGE_STATE is invalid/unreadable, this must not break anything."
    (when (and (boundp 'carriage-doc-state-sync-on-change)
               carriage-doc-state-sync-on-change)
      (ignore-errors (carriage-doc-state-write-current))))

  (defun carriage--doc-state--ensure-advice (&optional _file)
    "Ensure all known doc-state advice targets are advised.
Designed for use from `after-load-functions' so commands loaded later
(still) start persisting state."
    (dolist (fn carriage--doc-state--advice-targets)
      (when (and (fboundp fn) (not (memq fn carriage--doc-state--advised)))
        (ignore-errors (advice-add fn :after #'carriage--doc-state-write-safe))
        (push fn carriage--doc-state--advised))))

  ;; Install now for already-loaded commands, and keep installing as other modules load.
  (carriage--doc-state--ensure-advice)
  (add-hook 'after-load-functions #'carriage--doc-state--ensure-advice))

;; Robust preloader, stream region, and point behavior harmonization

(defgroup carriage-mode-preloader nil
  "Preloader/spinner behavior for Carriage."
  :group 'convenience)

;; Keep cursor free during streaming; spinner still moves to the tail.
(defvar carriage--perf--reasoning-after-change-guard-installed nil)

(defun carriage--perf--after-change-snippet (beg end &optional max-chars)
  "Return small text snippet around BEG..END for cheap after-change guards."
  (let* ((max-chars (or max-chars 800))
         (lo (save-excursion (goto-char beg) (line-beginning-position)))
         (hi (save-excursion (goto-char end) (line-end-position)))
         (hi2 (min hi (+ lo max-chars))))
    (buffer-substring-no-properties lo hi2)))

(defun carriage--perf--snippet-matches-p (beg end re)
  (string-match-p re (carriage--perf--after-change-snippet beg end)))

(unless carriage--perf--reasoning-after-change-guard-installed
  (setq carriage--perf--reasoning-after-change-guard-installed t)
  (when (fboundp 'carriage-reasoning-fold--after-change)
    (advice-add
     'carriage-reasoning-fold--after-change
     :around
     (lambda (orig beg end len)
       ;; Hot path: most edits are not inside reasoning blocks; bail out early.
       (if (carriage--perf--snippet-matches-p
            beg end
            "^[ \t]*#\\+\\(begin\\|end\\)_reasoning\\b\\|^[ \t]*#\\+begin_assistant\\b\\|^[ \t]*#\\+end_assistant\\b")
           (funcall orig beg end len)
         nil)))))

(provide 'carriage-mode)

(defgroup carriage-separator nil
  "Settings related to insertion of visual separator lines on Send."
  :group 'carriage)

(defcustom carriage-send-insert-separator t
  "When non-nil, insert a visual separator line \"-----\" after inserting fingerprint on Send."
  :type 'boolean
  :group 'carriage-separator)

(defun carriage-insert-send-separator ()
  "Insert a separator line \"-----\" above the reply insertion point when enabled.
Idempotent:
- Only runs when `carriage-send-insert-separator' is non-nil.
- Skips when separator was already inserted in this stream.
- Skips when adjacent lines already contain a single \"-----\"."
  (when carriage-send-insert-separator
    (unless carriage--separator-inserted
      (save-excursion
        (let ((pos (and (fboundp 'carriage--reply-insertion-point)
                        (carriage--reply-insertion-point))))
          (when pos (goto-char pos)))
        ;; Insert separator ABOVE current line; avoid duplicates with previous/this line.
        (let* ((here-bol (line-beginning-position))
               (cur-line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
               (prev-line (save-excursion
                            (forward-line -1)
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position)))))
          (goto-char here-bol)
          (unless (or (string-match-p "^-----\\s-*$" prev-line)
                      (string-match-p "^-----\\s-*$" cur-line))
            (insert "-----\n")
            (setq carriage--separator-inserted t)))))))

(defun carriage--after-insert-fingerprint-advice (&rest _)
  "Advice: run after fingerprint insertion to add visual separator."
  (ignore-errors (carriage-insert-send-separator)))

;; Wire the advice when function is present (defined earlier in this file).




;;; carriage-mode.el ends here
