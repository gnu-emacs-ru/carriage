;;; carriage-keyspec.el --- Keyspec v3 (bindings-first, menu-agnostic)  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.3
;; Keywords: keyspec, keymaps, menu
;;
;; Specifications:
;;   spec/keyspec-v2.org
;;   spec/context-menu-keys-v2.org
;;   spec/i18n-v2.org
;;
;;; Commentary:
;;
;; Bindings-first keyspec:
;; - All user-visible bindings are declared here and compiled into real Emacs keymaps.
;; - Primary prefix (default: "C-c e") is ALWAYS a prefix keymap (`carriage-prefix-map`).
;; - Menu UI is optional, invoked from inside the prefix-map:
;;   - C-c e SPC → `carriage-menu-open`
;;   - C-c e ?   → `carriage-menu-help`
;; - Menu provider is pluggable (transient/hydra/completing-read/disabled) and MUST NOT
;;   affect bindings.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Optional deps
(require 'carriage-i18n nil t)

;; ---------------------------------------------------------------------------
;; Autoloads for commands referenced by bindings/actions.
;; Keep keyspec usable even when defining modules are not loaded yet.
(autoload 'carriage-send-buffer "carriage-mode" nil t)
(autoload 'carriage-send-subtree "carriage-mode" nil t)
(autoload 'carriage-dry-run-at-point "carriage-mode" nil t)
(autoload 'carriage-apply-at-point-or-region "carriage-mode" nil t)
(autoload 'carriage-apply-last-iteration "carriage-mode" nil t)
(autoload 'carriage-ctrl-c-ctrl-c "carriage-mode" nil t)
(autoload 'carriage-abort-current "carriage-mode" nil t)

(autoload 'carriage-report-open "carriage-report" nil t)
(autoload 'carriage-report-show-diff-at-point "carriage-report" nil t)
(autoload 'carriage-report-ediff-at-point "carriage-report" nil t)
(autoload 'carriage-report-apply-at-point "carriage-report" nil t)

(autoload 'carriage-select-model "carriage-mode" nil t)
(autoload 'carriage-select-suite "carriage-mode" nil t)
(autoload 'carriage-toggle-intent "carriage-mode" nil t)
(autoload 'carriage-select-apply-engine "carriage-apply-engine" nil t)

(autoload 'carriage-toggle-include-gptel-context "carriage-mode" nil t)
(autoload 'carriage-toggle-include-doc-context "carriage-mode" nil t)
(autoload 'carriage-toggle-include-patched-files "carriage-mode" nil t)
(autoload 'carriage-toggle-include-project-map "carriage-mode" nil t)
(autoload 'carriage-toggle-include-visible-context "carriage-mode" nil t)

(autoload 'carriage-select-doc-context-all "carriage-context" nil t)
(autoload 'carriage-select-doc-context-last "carriage-context" nil t)
(autoload 'carriage-toggle-doc-context-scope "carriage-context" nil t)
(autoload 'carriage-toggle-context-profile "carriage-context" nil t)

(autoload 'carriage-wip-checkout "carriage-mode" nil t)
(autoload 'carriage-wip-reset-soft "carriage-mode" nil t)
(autoload 'carriage-commit-changes "carriage-mode" nil t)
(autoload 'carriage-commit-last-iteration "carriage-mode" nil t)

(autoload 'carriage-show-log "carriage-logging" nil t)
(autoload 'carriage-show-traffic "carriage-logging" nil t)

(autoload 'carriage-open-buffer "carriage-mode" nil t)
(autoload 'carriage-open-file-chat "carriage-mode" nil t)

(autoload 'carriage-insert-plan-section "carriage-task" nil t)
(autoload 'carriage-insert-step-section "carriage-task" nil t)
(autoload 'carriage-insert-test-section "carriage-task" nil t)
(autoload 'carriage-insert-retro-section "carriage-task" nil t)
(autoload 'carriage-insert-transient "carriage-task" nil t)

(autoload 'carriage-ui-context-delta-assist "carriage-ui" nil t)

;; Swarm (Supervisor) — optional
(autoload 'carriage-swarm-agent-start "carriage-swarm-supervisor" nil t)
(autoload 'carriage-swarm-agent-stop "carriage-swarm-supervisor" nil t)
(autoload 'carriage-swarm-gc-stale "carriage-swarm-supervisor" nil t)
(autoload 'carriage-swarm-hub-start "carriage-swarm-supervisor" nil t)
(autoload 'carriage-swarm-hub-stop "carriage-swarm-supervisor" nil t)
(autoload 'carriage-swarm-open-dashboard "carriage-swarm-supervisor" nil t)

;; ---------------------------------------------------------------------------
;; Declarations (byte-compile hygiene)
(declare-function carriage-i18n "carriage-i18n" (key &rest args))

(declare-function carriage-send-buffer "carriage-mode" ())
(declare-function carriage-send-subtree "carriage-mode" ())
(declare-function carriage-dry-run-at-point "carriage-mode" ())
(declare-function carriage-apply-at-point-or-region "carriage-mode" ())
(declare-function carriage-apply-last-iteration "carriage-mode" ())
(declare-function carriage-ctrl-c-ctrl-c "carriage-mode" ())
(declare-function carriage-abort-current "carriage-mode" ())
(declare-function carriage-report-open "carriage-report" (&optional report))
(declare-function carriage-report-show-diff-at-point "carriage-report" ())
(declare-function carriage-report-ediff-at-point "carriage-report" ())
(declare-function carriage-report-apply-at-point "carriage-report" ())
(declare-function carriage-select-model "carriage-mode" (&optional model))
(declare-function carriage-select-suite "carriage-mode" (&optional suite))
(declare-function carriage-toggle-intent "carriage-mode" ())
(declare-function carriage-select-apply-engine "carriage-apply-engine" (&optional engine))

(declare-function carriage-toggle-include-gptel-context "carriage-mode" ())
(declare-function carriage-toggle-include-doc-context "carriage-mode" ())
(declare-function carriage-toggle-include-patched-files "carriage-mode" ())
(declare-function carriage-toggle-include-project-map "carriage-mode" ())
(declare-function carriage-toggle-include-visible-context "carriage-mode" ())
(declare-function carriage-select-doc-context-all "carriage-context" ())
(declare-function carriage-select-doc-context-last "carriage-context" ())
(declare-function carriage-toggle-doc-context-scope "carriage-context" ())
(declare-function carriage-toggle-context-profile "carriage-context" ())

(declare-function carriage-wip-checkout "carriage-mode" ())
(declare-function carriage-wip-reset-soft "carriage-mode" (&optional rev))
(declare-function carriage-commit-changes "carriage-mode" (&optional message))
(declare-function carriage-commit-last-iteration "carriage-mode" (&optional message))

(declare-function carriage-show-log "carriage-logging" ())
(declare-function carriage-show-traffic "carriage-logging" ())
(declare-function carriage-open-buffer "carriage-mode" ())
(declare-function carriage-open-file-chat "carriage-mode" ())

(declare-function carriage-insert-plan-section "carriage-task" ())
(declare-function carriage-insert-step-section "carriage-task" ())
(declare-function carriage-insert-test-section "carriage-task" ())
(declare-function carriage-insert-retro-section "carriage-task" ())
(declare-function carriage-insert-transient "carriage-task" ())
(declare-function carriage-ui-context-delta-assist "carriage-ui" ())

(declare-function carriage-swarm-agent-start "carriage-swarm-supervisor" ())
(declare-function carriage-swarm-agent-stop "carriage-swarm-supervisor" ())
(declare-function carriage-swarm-gc-stale "carriage-swarm-supervisor" ())
(declare-function carriage-swarm-hub-start "carriage-swarm-supervisor" ())
(declare-function carriage-swarm-hub-stop "carriage-swarm-supervisor" ())
(declare-function carriage-swarm-open-dashboard "carriage-swarm-supervisor" ())

;; ---------------------------------------------------------------------------

(defgroup carriage-keyspec nil
  "Keyspec v3 (bindings-first) for Carriage."
  :group 'applications
  :prefix "carriage-keys-")

(defcustom carriage-keys-prefix "C-c e "
  "Primary Carriage prefix. Trailing whitespace is ignored."
  :type 'string
  :group 'carriage-keyspec)

(defcustom carriage-keys-prefix-alias nil
  "Optional additional prefix key sequence(s), e.g. \"C-c C-e \".

May be nil, a string, or a list of strings. Trailing whitespace is ignored."
  :type '(choice (const nil) string (repeat string))
  :group 'carriage-keyspec)

(defcustom carriage-menu-provider 'auto
  "Menu provider for `carriage-menu-open'.

- auto            Prefer transient when available, else completing-read.
- transient       Use transient when available, else completing-read.
- completing-read Always use completing-read (no transient deps).
- hydra           Use hydra when available, else completing-read.
- nil             Disable menu UI (bindings still work); `carriage-menu-open' shows a message."
  :type '(choice (const auto) (const transient) (const completing-read) (const hydra) (const nil))
  :group 'carriage-keyspec)

(defvar carriage-prefix-map (make-sparse-keymap)
  "Carriage prefix keymap. Installed under `carriage-keys-prefix' in relevant maps.")

(defvar carriage-keys--installed nil
  "Internal registry of installed overrides in `global-map': list of (MAP KEYSTR . OLD).

Used only to restore `global-map' bindings on disable of `carriage-global-mode'.
Bindings installed into Carriage-owned mode maps are not restored (they are owned).")

(defun carriage-keys-prefixes ()
  "Return list of primary and alias prefixes as kbd-ready strings."
  (let* ((raw (cons carriage-keys-prefix
                    (cond
                     ((null carriage-keys-prefix-alias) nil)
                     ((listp carriage-keys-prefix-alias) carriage-keys-prefix-alias)
                     (t (list carriage-keys-prefix-alias)))))
         (clean
          (delete-dups
           (delq nil
                 (mapcar (lambda (p)
                           (when (stringp p)
                             (let ((s (string-trim-right p "[ \t\n\r]+")))
                               (unless (string-empty-p s) s))))
                         raw)))))
    clean))

(defun carriage-keys--i18n (key &optional fallback)
  "Return i18n(KEY) if available, otherwise FALLBACK (string) or symbol-name."
  (cond
   ((and (fboundp 'carriage-i18n) (symbolp key))
    (let ((s (ignore-errors (carriage-i18n key))))
      (if (and (stringp s) (not (string-empty-p (string-trim s))))
          s
        (or fallback (symbol-name key)))))
   ((stringp fallback) fallback)
   ((symbolp key) (symbol-name key))
   (t (format "%s" key))))

;; ---------------------------------------------------------------------------
;; Action catalog (menu/help provider input)

(defconst carriage-keys--actions
  '(
    ;; Menu / Help (provider-agnostic)
    (:id menu-open :cmd carriage-menu-open :section tools :desc-key :menu-open :label "Menu")
    (:id menu-help :cmd carriage-menu-help :section tools :desc-key :menu-help :label "Help")

    ;; Core send/apply
    (:id send-buffer  :cmd carriage-send-buffer  :section act :desc-key :send-buffer)
    (:id send-subtree :cmd carriage-send-subtree :section act :desc-key :send-subtree)
    (:id dry-run      :cmd carriage-dry-run-at-point :section act :desc-key :dry-run)
    (:id apply        :cmd carriage-apply-at-point-or-region :section act :desc-key :apply)
    (:id apply-all    :cmd carriage-apply-last-iteration :section act :desc-key :apply-all)
    (:id abort        :cmd carriage-abort-current :section act :desc-key :abort)
    (:id report       :cmd carriage-report-open  :section logs :desc-key :report)

    ;; Model / Suite / Intent / Engine
    (:id model-select :cmd carriage-select-model :section tools :desc-key :model-select)
    (:id select-suite :cmd carriage-select-suite :section tools :desc-key :select-suite)
    (:id toggle-intent :cmd carriage-toggle-intent :section tools :desc-key :toggle-intent)
    (:id engine       :cmd carriage-select-apply-engine :section tools :desc-key :engine)

    ;; Context controls (mnemonics frozen by spec/context-menu-keys-v2.org)
    (:id toggle-gptel   :cmd carriage-toggle-include-gptel-context :section context :desc-key :toggle-ctx)
    (:id toggle-doc     :cmd carriage-toggle-include-doc-context   :section context :desc-key :toggle-doc)
    (:id toggle-patched :cmd carriage-toggle-include-patched-files :section context :desc-key :toggle-patched)
    (:id toggle-map     :cmd carriage-toggle-include-project-map   :section context :desc-key :toggle-map)
    (:id toggle-visible :cmd carriage-toggle-include-visible-context :section context :desc-key :visible-tooltip)
    (:id doc-scope-all  :cmd carriage-select-doc-context-all       :section context :desc-key :doc-scope-all)
    (:id doc-scope-last :cmd carriage-select-doc-context-last      :section context :desc-key :doc-scope-last)
    (:id doc-scope-cycle :cmd carriage-toggle-doc-context-scope    :section context :desc-key :doc-scope-cycle :label "Cycle Scope")
    (:id toggle-profile :cmd carriage-toggle-context-profile       :section context :desc-key :toggle-profile :label "Toggle P1/P3")

    ;; Git/WIP
    (:id wip          :cmd carriage-wip-checkout      :section session :desc-key :wip)
    (:id reset        :cmd carriage-wip-reset-soft    :section session :desc-key :reset)
    (:id commit-all   :cmd carriage-commit-changes    :section session :desc-key :commit-all)
    (:id commit-last  :cmd carriage-commit-last-iteration :section session :desc-key :commit-last)

    ;; Tools / buffers
    (:id open-buffer  :cmd carriage-open-buffer       :section session :desc-key :open-buffer)
    (:id file-chat    :cmd carriage-open-file-chat    :section tools   :desc-key :file-chat)

    ;; Logs
    (:id show-log     :cmd carriage-show-log          :section logs :desc-key :show-log)
    (:id show-traffic :cmd carriage-show-traffic      :section logs :desc-key :show-traffic)

    ;; Insert / Assist
    (:id insert-plan  :cmd carriage-insert-plan-section  :section act :desc-key :insert-plan)
    (:id insert-step  :cmd carriage-insert-step-section  :section act :desc-key :insert-step)
    (:id insert-test  :cmd carriage-insert-test-section  :section act :desc-key :insert-test)
    (:id insert-retro :cmd carriage-insert-retro-section :section act :desc-key :insert-retro)
    (:id assist-context-delta :cmd carriage-ui-context-delta-assist :section act :desc-key :assist-context-delta)
    (:id insert-menu  :cmd carriage-insert-transient     :section act :desc-key :insert-assist-menu)

    ;; Swarm supervisor (optional)
    (:id swarm-agent-start :cmd carriage-swarm-agent-start :section session :label "Swarm: start agent")
    (:id swarm-agent-stop  :cmd carriage-swarm-agent-stop  :section session :label "Swarm: stop agent…")
    (:id swarm-gc-stale    :cmd carriage-swarm-gc-stale    :section session :label "Swarm: cleanup stale")
    (:id swarm-hub-start   :cmd carriage-swarm-hub-start   :section session :label "Swarm: start hub")
    (:id swarm-hub-stop    :cmd carriage-swarm-hub-stop    :section session :label "Swarm: stop hub")
    (:id swarm-dashboard   :cmd carriage-swarm-open-dashboard :section session :label "Swarm: open dashboard")
    )
  "Action catalog used by menu/help providers.")

(defconst carriage-keys--prefix-bindings
  '(
    ;; Menu / help
    (:key "SPC" :cmd carriage-menu-open)
    (:key "?"   :cmd carriage-menu-help)

    ;; Send/apply
    (:key "RET"   :cmd carriage-send-buffer)
    (:key "M-RET" :cmd carriage-send-subtree)
    (:key "d"     :cmd carriage-dry-run-at-point)
    (:key "a"     :cmd carriage-apply-at-point-or-region)
    (:key "A"     :cmd carriage-apply-last-iteration)
    (:key "k"     :cmd carriage-abort-current)
    (:key "r"     :cmd carriage-report-open)

    ;; Model / suite / intent / engine
    (:key "m" :cmd carriage-select-model)
    (:key "S" :cmd carriage-select-suite)
    (:key "i" :cmd carriage-toggle-intent)
    (:key "E" :cmd carriage-select-apply-engine)

    ;; Context (two-stroke, stable mnemonics)
    (:key "t g" :cmd carriage-toggle-include-gptel-context)
    (:key "t f" :cmd carriage-toggle-include-doc-context)
    (:key "t p" :cmd carriage-toggle-include-patched-files)
    (:key "t m" :cmd carriage-toggle-include-project-map)
    (:key "t v" :cmd carriage-toggle-include-visible-context)
    (:key "t a" :cmd carriage-select-doc-context-all)
    (:key "t l" :cmd carriage-select-doc-context-last)
    (:key "t s" :cmd carriage-toggle-doc-context-scope)
    (:key "t P" :cmd carriage-toggle-context-profile)

    ;; Git/WIP
    (:key "w"  :cmd carriage-wip-checkout)
    (:key "R"  :cmd carriage-wip-reset-soft)
    (:key "cc" :cmd carriage-commit-changes)
    (:key "cl" :cmd carriage-commit-last-iteration)

    ;; Logs / buffers / tools
    (:key "L" :cmd carriage-show-log)
    (:key "T" :cmd carriage-show-traffic)
    (:key "e" :cmd carriage-open-buffer)
    (:key "f" :cmd carriage-open-file-chat)

    ;; Insert/Assist
    (:key "x p" :cmd carriage-insert-plan-section)
    (:key "x s" :cmd carriage-insert-step-section)
    (:key "x t" :cmd carriage-insert-test-section)
    (:key "x r" :cmd carriage-insert-retro-section)
    (:key "x c" :cmd carriage-ui-context-delta-assist)
    (:key "x x" :cmd carriage-insert-transient)

    ;; Swarm
    (:key "z a" :cmd carriage-swarm-agent-start)
    (:key "z k" :cmd carriage-swarm-agent-stop)
    (:key "z g" :cmd carriage-swarm-gc-stale)
    (:key "z h" :cmd carriage-swarm-hub-start)
    (:key "z H" :cmd carriage-swarm-hub-stop)
    (:key "z o" :cmd carriage-swarm-open-dashboard)
    )
  "Bindings inside `carriage-prefix-map' (suffixes relative to prefix).")

(defconst carriage-keys--direct-bindings
  '(
    ;; carriage-mode local convenience bindings (must not depend on menu provider)
    (:target carriage-mode-map :key "C-c C-c" :cmd carriage-ctrl-c-ctrl-c)
    (:target carriage-mode-map :key "C-c !"   :cmd carriage-apply-last-iteration)

    ;; Report-mode convenience (installed when report map exists)
    (:target carriage-report-mode-map :key "RET" :cmd carriage-report-show-diff-at-point)
    (:target carriage-report-mode-map :key "d"   :cmd carriage-report-show-diff-at-point)
    (:target carriage-report-mode-map :key "e"   :cmd carriage-report-ediff-at-point)
    (:target carriage-report-mode-map :key "a"   :cmd carriage-report-apply-at-point)
    )
  "Direct bindings installed into concrete keymaps (outside prefix-map).")

(defun carriage-keys-actions ()
  "Return keyspec action catalog."
  carriage-keys--actions)

(defun carriage-keys-action-label (action)
  "Return non-empty label for ACTION plist (id/cmd/desc-key/label fallbacks)."
  (let* ((id (plist-get action :id))
         (cmd (plist-get action :cmd))
         (dk  (plist-get action :desc-key))
         (raw (or (and (symbolp dk) (carriage-keys--i18n dk nil))
                  (plist-get action :label)
                  (and (symbolp cmd) (symbol-name cmd))
                  (and (symbolp id) (symbol-name id))
                  (format "%s" id)))
         (lbl (if (and (stringp raw) (not (string-empty-p (string-trim raw))))
                  raw
                (if (symbolp id) (symbol-name id) (format "%s" id)))))
    ;; Do not append bracket hints; tests assert this.
    (if (and (stringp lbl) (string-empty-p (string-trim lbl)))
        "?"
      lbl)))

(defun carriage-keys--ensure-prefix-map ()
  "Ensure `carriage-prefix-map' is a keymap."
  (unless (keymapp carriage-prefix-map)
    (setq carriage-prefix-map (make-sparse-keymap)))
  carriage-prefix-map)

(defun carriage-keys--reset-prefix-map ()
  "Clear all bindings in `carriage-prefix-map' (preserving the keymap object)."
  (carriage-keys--ensure-prefix-map)
  (setcdr carriage-prefix-map nil)
  carriage-prefix-map)

(defun carriage-keys-build-prefix-map ()
  "Rebuild `carriage-prefix-map' from `carriage-keys--prefix-bindings'."
  (carriage-keys--reset-prefix-map)
  (dolist (pl carriage-keys--prefix-bindings)
    (let ((k (plist-get pl :key))
          (cmd (plist-get pl :cmd)))
      (when (and (stringp k) (symbolp cmd))
        (define-key carriage-prefix-map (kbd (string-trim k)) cmd))))
  carriage-prefix-map)

(defun carriage-keys--map-value (sym)
  "Return keymap value of SYM, or nil."
  (when (and (symbolp sym) (boundp sym))
    (let ((v (symbol-value sym)))
      (when (keymapp v) v))))

(defun carriage-keys--define-key-owned (map keystr cmd)
  "Define KEYSTR in MAP to CMD. Return t when applied."
  (when (and (keymapp map) (stringp keystr) (symbolp cmd))
    (define-key map (kbd keystr) cmd)
    t))

(defun carriage-keys-install-known-keymaps ()
  "Install Carriage bindings into known Carriage-owned keymaps (idempotent).

Installs:
- `carriage-prefix-map' under configured prefixes in `carriage-mode-map' and
  `carriage-report-mode-map' (when those maps exist).
- direct bindings listed in `carriage-keys--direct-bindings'."
  (carriage-keys-build-prefix-map)
  (let ((prefixes (carriage-keys-prefixes)))
    ;; Prefix binding in owned mode maps
    (dolist (msym '(carriage-mode-map carriage-report-mode-map))
      (let ((m (carriage-keys--map-value msym)))
        (when m
          (dolist (pref prefixes)
            (define-key m (kbd pref) carriage-prefix-map))))))
  ;; Direct bindings in owned maps
  (dolist (pl carriage-keys--direct-bindings)
    (let* ((msym (plist-get pl :target))
           (m (carriage-keys--map-value msym))
           (k (plist-get pl :key))
           (cmd (plist-get pl :cmd)))
      (when m
        (carriage-keys--define-key-owned m k cmd))))
  t)

(defun carriage-keys-global-enable ()
  "Enable Carriage prefix in `global-map', saving overwritten bindings for restoration."
  (carriage-keys-build-prefix-map)
  (let ((prefixes (carriage-keys-prefixes)))
    (dolist (pref prefixes)
      (let* ((keystr pref)
             (seq (kbd keystr))
             (old (lookup-key global-map seq)))
        (push (cons global-map (cons keystr old)) carriage-keys--installed)
        (define-key global-map seq carriage-prefix-map))))
  t)

(defun carriage-keys-global-disable ()
  "Disable Carriage prefix in `global-map', restoring overwritten bindings (best-effort)."
  (let ((rest carriage-keys--installed))
    (setq carriage-keys--installed nil)
    (dolist (rec rest)
      (let ((map (car rec))
            (keystr (cadr rec))
            (old (cddr rec)))
        (when (and (eq map global-map) (stringp keystr))
          (define-key global-map (kbd keystr) old)))))
  t)

;; ---------------------------------------------------------------------------
;; Menu providers

(defun carriage-keys--menu-actions-for-current-buffer ()
  "Return list of action plists suitable for menu selection in current buffer."
  (cl-remove-if-not
   (lambda (a)
     (let ((cmd (plist-get a :cmd)))
       (and (symbolp cmd) (commandp cmd))))
   (carriage-keys-actions)))

(defun carriage--menu-open-completing-read ()
  "Fallback menu provider: completing-read over known actions."
  (let* ((acts (carriage-keys--menu-actions-for-current-buffer))
         (pairs (mapcar (lambda (a)
                          (cons (carriage-keys-action-label a) (plist-get a :cmd)))
                        acts))
         (choice (completing-read "Carriage: " (mapcar #'car pairs) nil t)))
    (let ((cmd (cdr (assoc choice pairs))))
      (when (commandp cmd)
        (call-interactively cmd)))))

;; ---------------------------------------------------------------------------
;; Transient provider (generated from keyspec data at runtime, no hardcoded layouts)

(defvar carriage--keyspec-transient-defined nil
  "Non-nil when transient menus were defined for this session.")

(defun carriage--keyspec-transient--section-title (sec)
  "Return UI title string for SEC (best-effort, i18n-aware)."
  (pcase sec
    ('act     (if (fboundp 'carriage-i18n) (carriage-i18n :act-title) "Actions"))
    ('tools   (if (fboundp 'carriage-i18n) (carriage-i18n :tools-title) "Tools"))
    ('session (if (fboundp 'carriage-i18n) (carriage-i18n :session-title) "Session/Git"))
    ('logs    (if (fboundp 'carriage-i18n) (carriage-i18n :logs-title) "Logs/Reports"))
    ('navigate (if (fboundp 'carriage-i18n) (carriage-i18n :navigate-title) "Navigate"))
    ('context (if (fboundp 'carriage-i18n) (carriage-i18n :context-title) "Context"))
    (_        "Other")))

(defun carriage--keyspec-transient--group (title items)
  "Return a transient group vector with TITLE and ITEMS."
  (apply #'vector (cons title items)))

(defun carriage--keyspec-transient--make-runner (cmd)
  "Return an interactive command that closes transient and runs CMD asynchronously."
  (lambda ()
    (interactive)
    (ignore-errors
      (when (featurep 'transient)
        (if (fboundp 'transient-quit-all)
            (transient-quit-all)
          (transient-quit-one))))
    (let ((prefix current-prefix-arg))
      (run-at-time
       0 nil
       (lambda ()
         (let ((current-prefix-arg prefix))
           (when (commandp cmd)
             (call-interactively cmd))))))))

(defun carriage--keyspec-transient--index-actions (acts)
  "Return hash table mapping command symbols to action plists from ACTS."
  (let ((cmd->act (make-hash-table :test 'eq)))
    (dolist (a acts)
      (let ((cmd (plist-get a :cmd)))
        (when (symbolp cmd)
          (puthash cmd a cmd->act))))
    cmd->act))

(defun carriage--keyspec-transient--derive-items (cmd->act)
  "Derive transient heads from keyspec bindings using CMD->ACT index.

Return a cons cell (SEC->ITEMS . CTX-ITEMS) where:
- SEC->ITEMS is a hash table: section symbol -> list of transient items
- CTX-ITEMS is a list of transient items for Context menu."
  (let ((sec->items (make-hash-table :test 'eq))
        (ctx-items '()))
    ;; Derive transient heads from the *real* prefix bindings.
    (dolist (pl carriage-keys--prefix-bindings)
      (let* ((k (plist-get pl :key))
             (cmd (plist-get pl :cmd)))
        ;; Skip menu/help: transient is opened by `carriage-menu-open` already.
        (unless (member k '("SPC" "?"))
          (let* ((a (and (symbolp cmd) (gethash cmd cmd->act)))
                 (sec (or (and a (plist-get a :section)) 'tools))
                 (lbl (if a
                          (carriage-keys-action-label a)
                        (if (symbolp cmd) (symbol-name cmd) (format "%s" cmd))))
                 (runner (carriage--keyspec-transient--make-runner cmd)))
            (cond
             ;; Context actions: "t g" etc → appear in a dedicated Context transient under key "t".
             ((and (stringp k) (string-prefix-p "t " k))
              (let ((k2 (string-trim (substring k 2))))
                (when (and (stringp k2) (not (string-empty-p k2)))
                  (push (list k2 lbl runner) ctx-items))))
             ;; Regular actions: grouped by :section.
             (t
              (let ((lst (gethash sec sec->items)))
                (puthash sec (append lst (list (list k lbl runner))) sec->items))))))))
    (cons sec->items (nreverse ctx-items))))

(defun carriage--keyspec-transient--build-groups (sec->items)
  "Build transient groups from SEC->ITEMS hash in a stable order."
  (let ((groups '()))
    (dolist (sec '(act tools session logs navigate))
      (let ((items (gethash sec sec->items)))
        (when (and (listp items) items)
          (push (carriage--keyspec-transient--group
                 (carriage--keyspec-transient--section-title sec)
                 items)
                groups))))
    (nreverse groups)))

(defun carriage--keyspec-transient--define-menus (main-title ctx-title groups ctx-items)
  "Define transient menus from MAIN-TITLE/CTX-TITLE/GROUPS/CTX-ITEMS."
  (let* ((ctx-group
          (when (and (listp ctx-items) ctx-items)
            (carriage--keyspec-transient--group
             ctx-title
             (append ctx-items
                     (list (list "q" "Quit" #'transient-quit-one))))))
         ;; Main menu always contains an entry to open Context transient when present.
         (main-groups
          (append
           groups
           (when ctx-group
             (list
              (carriage--keyspec-transient--group
               ctx-title
               (list (list "t" (format "%s…" ctx-title) #'carriage--transient-context)))))
           (list (carriage--keyspec-transient--group "" (list (list "q" "Quit" #'transient-quit-one)))))))
    ;; Define transient prefixes dynamically (byte-compile safe).
    (eval
     `(progn
        (transient-define-prefix carriage--transient-context ()
          ,(format "%s: %s" main-title ctx-title)
          ,ctx-group)
        (transient-define-prefix carriage--transient-menu ()
          ,main-title
          ,@main-groups)))))

(defun carriage--keyspec--ensure-transient-defined ()
  "Define transient menus lazily (runtime) when transient is available.

Important:
- menus are GENERATED from keyspec (actions + prefix-bindings)
- bindings remain real keymaps compiled from keyspec."
  (unless carriage--keyspec-transient-defined
    (when (require 'transient nil t)
      (ignore-errors (require 'carriage-i18n nil t))
      (let* ((acts (carriage-keys-actions))
             (cmd->act (carriage--keyspec-transient--index-actions acts))
             (derived (carriage--keyspec-transient--derive-items cmd->act))
             (sec->items (car derived))
             (ctx-items (cdr derived))
             (groups (carriage--keyspec-transient--build-groups sec->items))
             (ctx-title (carriage--keyspec-transient--section-title 'context))
             (main-title (if (fboundp 'carriage-i18n) (carriage-i18n :carriage-menu) "Carriage")))
        (carriage--keyspec-transient--define-menus main-title ctx-title groups ctx-items)
        (setq carriage--keyspec-transient-defined t)))))

(defun carriage--menu-open-transient ()
  "Menu provider: transient (when available), otherwise fallback to completing-read."
  (interactive)
  (if (require 'transient nil t)
      (progn
        (carriage--keyspec--ensure-transient-defined)
        (if (fboundp 'carriage--transient-menu)
            (call-interactively #'carriage--transient-menu)
          (carriage--menu-open-completing-read)))
    (carriage--menu-open-completing-read)))

(defun carriage-menu-open ()
  "Open Carriage menu according to `carriage-menu-provider'.

Bindings (by keyspec, inside prefix-map):
- C-c e SPC → this command.

Provider policy:
- nil: menu disabled (bindings still work)
- completing-read: always use completing-read
- transient/hydra: best-effort, fallback to completing-read when unavailable."
  (interactive)
  (pcase carriage-menu-provider
    ((or 'nil 'disabled)
     (message "%s" (carriage-keys--i18n :menu-open-tooltip "Menu disabled (provider=nil)")))
    ('completing-read
     (carriage--menu-open-completing-read))
    ('hydra
     (if (require 'hydra nil t)
         ;; Hydra provider can be added later; for now fallback (bindings are still real).
         (carriage--menu-open-completing-read)
       (carriage--menu-open-completing-read)))
    ((or 'auto 'transient)
     ;; Best-effort transient; fallback always works.
     (if (require 'transient nil t)
         (carriage--menu-open-transient)
       (carriage--menu-open-completing-read)))
    (_
     (carriage--menu-open-completing-read))))

(defun carriage-menu-help ()
  "Show Carriage key cheatsheet for the current configuration.

Bindings (by keyspec, inside prefix-map):
- C-c e ? → this command."
  (interactive)
  (let* ((buf (get-buffer-create "*carriage-keys*"))
         (prefixes (carriage-keys-prefixes)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (read-only-mode -1)
        (erase-buffer)
        (insert "Carriage keys (bindings-first)\n\n")
        (insert (format "Prefix(es): %s\n\n" (mapconcat #'identity prefixes ", ")))
        (insert "Inside prefix map (suffix → command):\n")
        (dolist (pl carriage-keys--prefix-bindings)
          (let ((k (plist-get pl :key))
                (cmd (plist-get pl :cmd)))
            (when (and (stringp k) (symbolp cmd))
              (insert (format "  %-8s → %s\n" k cmd)))))
        (insert "\nDirect bindings:\n")
        (dolist (pl carriage-keys--direct-bindings)
          (let ((tgt (plist-get pl :target))
                (k (plist-get pl :key))
                (cmd (plist-get pl :cmd)))
            (when (and (symbolp tgt) (stringp k) (symbolp cmd))
              (insert (format "  [%s] %-8s → %s\n" tgt k cmd)))))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)))

;; Late-load support: ensure bindings are installed once optional maps appear.
(defun carriage-keys--after-load-install (&optional file)
  "After-load hook: re-install keyspec bindings when Carriage maps become available.
FILE is the loaded file name as passed by `after-load-functions'."
  (when (and (stringp file)
             (string-match-p "carriage-\\(mode\\|report\\)\\.elc?\\'" file))
    (ignore-errors (carriage-keys-install-known-keymaps)))
  t)

(add-hook 'after-load-functions #'carriage-keys--after-load-install)

(provide 'carriage-keyspec)
;;; carriage-keyspec.el ends here
