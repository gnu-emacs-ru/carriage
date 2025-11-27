;;; carriage-keyspec.el --- Keyspec and keymap generator  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: keyspec, menu
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/keyspec-v2.org
;;   spec/ui-v2.org
;;   spec/i18n-v2.org
;;   spec/document-branching-and-templates-v1.org
;;
;;; Commentary:
;; Centralized keyspec for Carriage commands and transient/menu generation.
;; Context column uses two-stroke keys ("t x") and reserves 't' when present.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(autoload 'carriage-create-task-doc "carriage-task" "Create task document from the current heading." t)
(autoload 'carriage-branching-transient "carriage-task" "Open Branching UI: choose template and inheritance." t)
(autoload 'carriage-ui-context-delta-assist "carriage-ui" "Suggest and apply context delta (with confirmation)." t)

(defgroup carriage-keyspec nil
  "Centralized key binding model for Carriage."
  :group 'applications
  :prefix "carriage-keys-")

(defcustom carriage-keys-profile 'classic
  "Active keyspec profile (classic|vimish|custom)."
  :type '(choice (const classic) (const vimish) (const custom))
  :group 'carriage-keyspec)

(defcustom carriage-keys-prefix "C-c e "
  "Prefix for all Carriage mode keybindings."
  :type 'string
  :group 'carriage-keyspec)

(defcustom carriage-keys-prefix-alias nil
  "Optional additional prefix key sequence (e.g., \"C-c C-e \") as an alias for `carriage-keys-prefix'.
Used for menu binding etc., not for all suffix bindings."
  :type '(choice (const nil) string (repeat string))
  :group 'carriage-keyspec)

(defun carriage-keys-prefixes ()
  "Return list of primary and alias prefixes without trailing whitespace."
  (let* ((raw (cons carriage-keys-prefix
                    (cond
                     ((null carriage-keys-prefix-alias) nil)
                     ((listp carriage-keys-prefix-alias) carriage-keys-prefix-alias)
                     (t (list carriage-keys-prefix-alias)))))
         (clean (cl-remove-if-not
                 #'identity
                 (mapcar (lambda (p)
                           (when (stringp p)
                             (let ((trim (string-trim-right p "[ \t\n\r]+")))
                               (unless (string-empty-p trim) trim))))
                         raw))))
    (delete-dups clean)))

(defvar carriage-keys--profile-overlays
  '((classic . nil)
    (vimish  . nil)
    (custom  . nil))
  "Optional profile overlays to add/remove keys for actions.
Each value is a plist with :add and/or :remove lists of (:id ID :keys (..)).")

(defvar carriage-keys--spec
  '(
    ;; Tools/model/context
    (:id model-select :cmd carriage-select-model :keys ("m") :contexts (carriage) :section tools :desc-key :model-select)
    (:id toggle-ctx   :cmd carriage-toggle-include-gptel-context :keys ("t c") :menu-key "t g" :contexts (carriage) :section context :desc-key :toggle-ctx)
    (:id toggle-doc   :cmd carriage-toggle-include-doc-context   :keys ("t f") :menu-key "t f" :contexts (carriage) :section context :desc-key :toggle-doc)
    (:id toggle-patched :cmd carriage-toggle-include-patched-files :keys ("t p") :menu-key "t p" :contexts (carriage) :section context :desc-key :toggle-patched)
    (:id toggle-visible :cmd carriage-toggle-include-visible-context :keys ("t v") :menu-key "t v" :contexts (carriage) :section context :desc-key :visible-tooltip)
    (:id doc-scope-all  :cmd carriage-select-doc-context-all       :keys ("t a") :menu-key "t a" :contexts (carriage) :section context :desc-key :doc-scope-all)
    (:id doc-scope-last :cmd carriage-select-doc-context-last      :keys ("t l") :menu-key "t l" :contexts (carriage) :section context :desc-key :doc-scope-last)
    (:id doc-scope-cycle :cmd carriage-toggle-doc-context-scope    :keys ("t s") :menu-key "t s" :contexts (carriage) :section context :desc-key :doc-scope-cycle :label "Cycle Doc Scope")
    (:id toggle-profile :cmd carriage-toggle-context-profile        :keys ("t P") :menu-key "t P" :contexts (carriage) :section context :desc-key :toggle-profile :label "Toggle P1/P3")
    ;; Suite/Intent (tools)
    (:id select-suite :cmd carriage-select-suite                 :keys ("S")   :contexts (carriage) :section tools :desc-key :select-suite)
    (:id toggle-intent :cmd carriage-toggle-intent               :keys ("i")   :contexts (carriage) :section tools :desc-key :toggle-intent)
    ;; Actions
    (:id dry-run      :cmd carriage-dry-run-at-point        :keys ("d")     :contexts (carriage) :section act :desc-key :dry-run)
    (:id apply        :cmd carriage-apply-at-point-or-region :keys ("a")     :contexts (carriage) :section act :desc-key :apply)
    (:id apply-all    :cmd carriage-apply-last-iteration    :keys ("A")     :contexts (carriage) :section act :desc-key :apply-all)
    (:id abort        :cmd carriage-abort-current           :keys ("k")     :contexts (carriage) :section act :desc-key :abort)
    (:id send-buffer  :cmd carriage-send-buffer             :keys ("RET")   :menu-key "RET" :contexts (carriage) :section act :desc-key :send-buffer)
    (:id send-subtree :cmd carriage-send-subtree            :keys ("M-RET") :menu-key "M-RET" :contexts (carriage) :section act :desc-key :send-subtree)
    (:id report       :cmd carriage-report-open             :keys ("r")     :contexts (carriage) :section tools :desc-key :report)
    (:id clean        :cmd carriage-clear-patch-blocks     :keys ("D")     :contexts (carriage) :section act :desc-key :clean)
    ;; Report context actions (available in report buffers under the configured prefix)
    (:id report-diff  :cmd carriage-report-show-diff-at-point :keys ("d")   :contexts (report)   :section act   :desc-key :report-diff)
    (:id report-ediff :cmd carriage-report-ediff-at-point     :keys ("e")   :contexts (report)   :section act   :desc-key :report-ediff)
    (:id report-apply :cmd carriage-report-apply-at-point     :keys ("a")   :contexts (report)   :section act   :desc-key :report-apply)
    ;; Git/WIP
    (:id wip          :cmd carriage-wip-checkout            :keys ("w")  :contexts (carriage) :section session :desc-key :wip)
    (:id reset        :cmd carriage-wip-reset-soft          :keys ("R")  :contexts (carriage) :section session :desc-key :reset)
    (:id commit-all   :cmd carriage-commit-changes          :keys ("C")  :contexts (carriage) :section session :desc-key :commit-all)
    (:id commit-last  :cmd carriage-commit-last-iteration   :keys ("L")     :contexts (carriage) :section session :desc-key :commit-last)
    ;; Global
    (:id show-log     :cmd carriage-show-log                :keys ("L")     :contexts (carriage report global) :section logs :desc-key :show-log)
    (:id show-traffic :cmd carriage-show-traffic            :keys ("T")     :contexts (carriage report global) :section logs :desc-key :show-traffic)
    (:id aux-quit     :cmd quit-window                      :keys ("q")     :contexts (report log traffic)     :section navigate :desc-key :quit)
    (:id open-buffer  :cmd carriage-open-buffer             :keys ("e")     :contexts (global)                 :section session :desc-key :open-buffer)
    (:id task-new     :cmd carriage-create-task-doc         :keys ("n")     :contexts (carriage org global) :section tools :desc-key :task-new :label "Create task doc")
    (:id branch-doc   :cmd carriage-branching-transient     :keys ("N")     :contexts (carriage org global) :section tools :desc-key :branch-doc :label "Branch from template")
    (:id file-chat    :cmd carriage-open-file-chat          :keys ("f")     :contexts (carriage org global) :section tools :desc-key :file-chat :label "File chat")
    ;; Palette insert actions (minimal v1)
    (:id insert-plan  :cmd carriage-insert-plan-section     :keys ("x p")   :contexts (carriage org) :section act :desc-key :insert-plan)
    (:id insert-step  :cmd carriage-insert-step-section     :keys ("x s")   :contexts (carriage org) :section act :desc-key :insert-step)
    (:id insert-test  :cmd carriage-insert-test-section     :keys ("x t")   :contexts (carriage org) :section act :desc-key :insert-test)
    (:id insert-retro :cmd carriage-insert-retro-section    :keys ("x r")   :contexts (carriage org) :section act :desc-key :insert-retro)
    (:id assist-context-delta :cmd carriage-ui-context-delta-assist :keys ("x c") :contexts (carriage org) :section act :desc-key :assist-context-delta)
    (:id insert-menu  :cmd carriage-insert-transient        :keys ("x x")   :contexts (carriage org) :section act :desc-key :insert-assist-menu)
    ;; Engine
    (:id engine       :cmd carriage-select-apply-engine     :keys ("E")  :contexts (carriage) :section tools :desc-key :engine))
  "Keyspec: list of action plists with :id :cmd :keys :contexts :section :desc-key.
All keys are relative to carriage-keys-prefix (default \"C-c e \").")

;;;###autoload
(defun carriage-keys-register-actions (actions)
  "Register or override keyspec actions.

ACTIONS is a list of plists. Each plist should contain at least:
  :id SYMBOL     Unique action identifier.
  :cmd SYMBOL    Interactive command symbol.

Optional keys:
  :keys (LIST OF STR)   Keys relative to `carriage-keys-prefix' (e.g., (\"n\") not full prefix).
  :contexts (LIST)      Contexts like (carriage report global org).
  :section SYMBOL       Grouping for menu (navigate act session tools logs).
  :desc-key SYMBOL      i18n key for labels/tooltips.

If an :id already exists, its entry is replaced. Otherwise, a new entry is appended."
  (dolist (pl actions)
    (let ((id (plist-get pl :id)))
      (when id
        (setq carriage-keys--spec
              (nconc
               (cl-remove-if (lambda (el) (eq (plist-get el :id) id))
                             carriage-keys--spec)
               (list pl))))))
  t)

(defun carriage-keys--ensure-kbd (key)
  "Return a kbd string for KEY under `carriage-keys-prefix'.
KEY may be a single token (\"m\") or a space-separated sequence (\"t c\")."
  (let* ((prefix (or carriage-keys-prefix "C-c e "))
         (ks (string-trim key)))
    (kbd (concat prefix ks))))

(defun carriage-keys--actions-for-context (context)
  "Return actions from keyspec applicable to CONTEXT."
  (cl-remove-if-not
   (lambda (pl)
     (let ((cs (plist-get pl :contexts)))
       (or (null cs) (memq context cs))))
   carriage-keys--spec))

(defun carriage-keys--actions-for-contexts (contexts)
  "Return merged action list for CONTEXTS with left-to-right priority.
Earlier contexts in CONTEXTS take precedence over later ones by :id."
  (let ((seen (make-hash-table :test 'eq))
        (acc '()))
    (dolist (ctx contexts)
      (dolist (pl (carriage-keys--actions-for-context ctx))
        (let ((id (plist-get pl :id)))
          (unless (gethash id seen)
            (puthash id t seen)
            (push pl acc)))))
    (nreverse acc)))

(defun carriage-keys--current-contexts ()
  "Detect active contexts in current buffer with priority order.
- In carriage buffers: (carriage [report|log|traffic?] org global)
- In report/log/traffic buffers: (that-context org global)
- Else: include `org' when in Org buffers; add `global' only if `carriage-global-mode' is on."
  (let* ((in-carriage (and (boundp 'carriage-mode) carriage-mode))
         (is-report  (derived-mode-p 'carriage-report-mode))
         (is-log     (string= (buffer-name) "*carriage-log*"))
         (is-traffic (string= (buffer-name) "*carriage-traffic*"))
         (is-org     (derived-mode-p 'org-mode))
         (ctxs '()))
    (when in-carriage (push 'carriage ctxs))
    (when is-report   (push 'report ctxs))
    (when is-log      (push 'log ctxs))
    (when is-traffic  (push 'traffic ctxs))
    (when is-org      (push 'org ctxs))
    ;; Global is available always inside carriage-mode; outside only if carriage-global-mode is enabled.
    (when (or in-carriage (bound-and-true-p carriage-global-mode))
      (setq ctxs (append ctxs (list 'global))))
    (or ctxs (when (bound-and-true-p carriage-global-mode) '(global)))))

(defun carriage-keys--apply-action (map action)
  "Apply ACTION binding(s) to MAP according to keyspec + profile overlays."
  (let* ((id   (plist-get action :id))
         (cmd  (plist-get action :cmd))
         (keys (copy-sequence (or (plist-get action :keys) '()))))
    (when (symbolp cmd)
      ;; Apply profile overlays: remove/add
      (let* ((ov (alist-get carriage-keys-profile carriage-keys--profile-overlays))
             (rm (plist-get ov :remove))
             (ad (plist-get ov :add))
             (rm-keys (cl-loop for el in rm
                               when (eq (plist-get el :id) id)
                               append (or (plist-get el :keys) '())))
             (ad-keys (cl-loop for el in ad
                               when (eq (plist-get el :id) id)
                               append (or (plist-get el :keys) '()))))
        (dolist (rk rm-keys)
          (setq keys (delete rk keys)))
        (dolist (ak ad-keys)
          (push ak keys)))
      ;; Bind all effective keys
      (dolist (k (delete-dups (delq nil keys)))
        (let* ((seq (carriage-keys--ensure-kbd k))
               (ok t)
               (i 0))
          ;; Skip binding if any prefix of seq is already bound to a non-prefix command in MAP.
          (while (and ok (< i (1- (length seq))))
            (let* ((sub (cl-subseq seq 0 (1+ i)))
                   (binding (lookup-key map sub)))
              (when (and binding (not (keymapp binding)))
                (setq ok nil)))
            (setq i (1+ i)))
          (when ok
            (define-key map seq cmd)))))))

(defun carriage-keys-apply-to (map context)
  "Apply keyspec to MAP for CONTEXT."
  (dolist (act (carriage-keys--actions-for-context context))
    (carriage-keys--apply-action map act))
  map)

(defun carriage-keys-apply-prefix-suffixes (map context)
  "Apply keyspec of CONTEXT to prefix MAP by binding suffix keys relative to MAP.

This is intended for true prefix maps already assigned to a leading prefix
derived from =carriage-keys-prefix'. The keys from keyspec are bound WITHOUT the =carriage-keys-prefix'
added. For example:
- \"t c\" in keyspec becomes (kbd \"t c\") inside MAP,
- \"RET\" becomes (kbd \"RET\") inside MAP.

Profile overlays (:add/:remove) are respected similar to =carriage-keys--apply-action'."
  (dolist (act (carriage-keys--actions-for-context context))
    (let* ((id   (plist-get act :id))
           (cmd  (plist-get act :cmd))
           (keys (copy-sequence (or (plist-get act :keys) '()))))
      (when (and (symbolp cmd) keys)
        (let* ((ov (alist-get carriage-keys-profile carriage-keys--profile-overlays))
               (rm (plist-get ov :remove))
               (ad (plist-get ov :add))
               (rm-keys (cl-loop for el in rm
                                 when (eq (plist-get el :id) id)
                                 append (or (plist-get el :keys) '())))
               (ad-keys (cl-loop for el in ad
                                 when (eq (plist-get el :id) id)
                                 append (or (plist-get el :keys) '()))))
          (dolist (rk rm-keys)
            (setq keys (delete rk keys)))
          (dolist (ak ad-keys)
            (push ak keys)))
        (dolist (k (delete-dups (delq nil keys)))
          (define-key map (kbd (string-trim k)) cmd)))))
  map)

(defun carriage-keys-apply-multi (map contexts)
  "Apply keyspec for CONTEXTS to MAP in order; later contexts override earlier.
Example: (global carriage) → локальные биндинги перекрывают глобальные."
  (dolist (ctx contexts)
    (carriage-keys-apply-to map ctx))
  map)

(defun carriage-keys-apply-known-keymaps ()
  "Apply keyspec to known Carriage keymaps ensuring all prefixes open the menu.
Installs bindings for the primary prefix and any aliases in Carriage and auxiliary maps."
  (let ((prefixes (carriage-keys-prefixes)))
    (when (and (boundp 'carriage-mode-map) (keymapp carriage-mode-map))
      (dolist (pref prefixes)
        (define-key carriage-mode-map (kbd pref) #'carriage-keys-open-menu)))
    (dolist (mp '(carriage-report-mode-map carriage-aux-mode-map org-mode-map))
      (let ((map (when (boundp mp) (symbol-value mp))))
        (when (keymapp map)
          (dolist (pref prefixes)
            (define-key map (kbd pref) #'carriage-keys-open-menu))))))
  ;; Legacy alias: C-c ! applies last iteration (UI v1 legacy)
  (ignore-errors
    (global-set-key (kbd "C-c !") #'carriage-apply-last-iteration))
  t)

(defun carriage-keys-first-key (id)
  "Return the first effective key (kbd string) for action ID in current profile."
  (let* ((act (cl-find id carriage-keys--spec :key (lambda (pl) (plist-get pl :id))))
         (keys (and act (plist-get act :keys))))
    (when (and act keys)
      (carriage-keys--ensure-kbd (car keys)))))

(defun carriage-keys-lint-collisions ()
  "Return a list of (key . (ID1 ID2 ...)) for collisions inside keyspec."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (act carriage-keys--spec)
      (let* ((id (plist-get act :id))
             (keys (plist-get act :keys)))
        (dolist (k keys)
          (let* ((kbd (key-description (carriage-keys--ensure-kbd k)))
                 (cur (gethash kbd table '())))
            (puthash kbd (cons id cur) table)))))
    (cl-loop for k being the hash-keys of table
             for v = (gethash k table)
             when (> (length v) 1)
             collect (cons k (nreverse v)))))


(defun carriage-keys-lint-menu ()
  "Lint transient-menu invariants.

Returns a plist:
  :duplicate-menu-keys ((KEY . (ID1 ID2 ...)) ...)
  :has-single-t-when-t-prefix BOOL
  :empty-label-ids (ID ...)

Menu-keys are derived similarly to `carriage-keys-open-menu':
- explicit :menu-key wins;
- \"tc\" form becomes \"t c\";
- keys with spaces are kept as-is;
- otherwise last token of full binding is used.

Labels are computed as: i18n(:desc-key) → :label → symbol-name(:cmd) → symbol-name(:id)."
  (let* ((dups (make-hash-table :test 'equal))
         (seen (make-hash-table :test 'equal))
         (empty-label-ids '())
         (has-t-prefix nil)
         (has-single-t nil)
         (_ (require 'carriage-i18n nil t)))
    (dolist (pl carriage-keys--spec)
      (let* ((id (plist-get pl :id))
             (cmd (plist-get pl :cmd))
             (k   (car (plist-get pl :keys)))
             (mkey (plist-get pl :menu-key))
             (desc (and (stringp k)
                        (ignore-errors
                          (key-description (carriage-keys--ensure-kbd k)))))
             (lasttok (and (stringp desc)
                           (car (last (split-string desc " " t)))))
             (menu-key
              (let ((mk (and (stringp mkey) (string-trim mkey))))
                (if (and mk (> (length mk) 0))
                    mk
                  (cond
                   ((and (stringp k)
                         (string-match-p "\\`t[[:alnum:]]\\'" k))
                    (format "t %s" (substring k 1 2)))
                   ((and (stringp k) (string-match-p " " k))
                    (string-trim k))
                   (t lasttok)))))
             (desc-key (plist-get pl :desc-key))
             (raw-label (or (and (fboundp 'carriage-i18n) (carriage-i18n desc-key))
                            (plist-get pl :label)
                            (and (symbolp cmd) (symbol-name cmd))
                            (and (symbolp id) (symbol-name id))
                            (format "%s" id)))
             (lbl (if (and (stringp raw-label)
                           (string-match-p "\\`[ \t]*\\'" raw-label))
                      (symbol-name id)
                    raw-label)))
        (when (and (stringp menu-key) (string-prefix-p "t " menu-key))
          (setq has-t-prefix t))
        (when (and (stringp menu-key) (string= menu-key "t"))
          (setq has-single-t t))
        (when (or (null lbl)
                  (and (stringp lbl)
                       (string-match-p "\\`[ \t]*\\'" lbl)))
          (push id empty-label-ids))
        (when (stringp menu-key)
          (let ((cur (gethash menu-key seen)))
            (if cur
                (puthash menu-key (cons id cur) dups)
              (puthash menu-key (list id) seen))))))
    (let ((dup-list
           (cl-loop for k being the hash-keys of dups
                    for v = (gethash k dups)
                    when (> (length v) 1)
                    collect (cons k (nreverse v)))))
      (list :duplicate-menu-keys dup-list
            :has-single-t-when-t-prefix (and has-t-prefix has-single-t)
            :empty-label-ids (nreverse (delete-dups empty-label-ids))))))

;;;###autoload
(defun carriage-keys-open-menu ()
  "Open Carriage action menu from keyspec.
If transient is available, show multi-column grouped menu with i18n headers.
Fallback: completing-read (group prefix in labels)."
  (interactive)
  (let* ((ctxs (carriage-keys--current-contexts))
         (all-acts (carriage-keys--actions-for-contexts ctxs))
         ;; exclude :menu itself
         (acts (cl-remove-if (lambda (pl) (eq (plist-get pl :id) 'menu)) all-acts))
         ;; Optional Assist-based ranking of fixed actions (no new actions are added)
         (_rank (ignore-errors
                  (when (and (require 'carriage-ui nil t)
                             (boundp 'carriage-ui-enable-assist-ranking)
                             carriage-ui-enable-assist-ranking
                             (fboundp 'carriage-ui-rank-actions-with-assist))
                    (setq acts (carriage-ui-rank-actions-with-assist acts)))))
         (sections '(navigate act context session tools logs)))
    (if (and (require 'transient nil t) (fboundp 'transient-define-prefix))
        (let* ((_ (require 'carriage-i18n nil t))
               ;; Build data per section: each element -> (menu-key label cmd id section)
               (per-sec
                (cl-loop for sec in sections
                         collect
                         (cl-loop for pl in acts
                                  for s = (plist-get pl :section)
                                  when (eq s sec)
                                  collect
                                  (let* ((cmd (plist-get pl :cmd))
                                         (id  (plist-get pl :id))
                                         (k   (car (plist-get pl :keys)))
                                         (mkey (plist-get pl :menu-key))
                                         ;; Compute menu key:
                                         ;; 1) Respect explicit :menu-key when provided (e.g., "t g").
                                         ;; 2) For two-stroke like "tc"/"tf"/..., show "t c"/"t f"/...
                                         ;; 3) If K already contains a space, keep as-is.
                                         ;; 4) Else fall back to the last token of full binding.
                                         (desc (and (stringp k)
                                                    (ignore-errors
                                                      (key-description (carriage-keys--ensure-kbd k)))))
                                         (lasttok (and (stringp desc)
                                                       (car (last (split-string desc " " t)))))
                                         (menu-key
                                          (let ((mk (and (stringp mkey) (string-trim mkey))))
                                            (if (and mk (> (length mk) 0))
                                                mk
                                              (cond
                                               ((and (stringp k)
                                                     (string-match-p "\\`t[[:alnum:]]\\'" k))
                                                (format "t %s" (substring k 1 2)))
                                               ((and (stringp k) (string-match-p " " k))
                                                (string-trim k))
                                               (t lasttok)))))
                                         (desc-key (plist-get pl :desc-key))
                                         (fallback-label (plist-get pl :label))
                                         (raw-label (or (and (fboundp 'carriage-i18n) (carriage-i18n desc-key))
                                                        fallback-label
                                                        (and (symbolp cmd) (symbol-name cmd))
                                                        (format "%s" id)))
                                         (lbl (let ((s (if (and (stringp raw-label)
                                                                (not (string-match-p "\\`[ \t]*\\'" raw-label)))
                                                           raw-label
                                                         (if (symbolp id) (symbol-name id) (format "%s" id)))))
                                                (if (or (null s)
                                                        (and (stringp s)
                                                             (string-match-p "\\`[ \t]*\\'" s)))
                                                    (if (symbolp id) (symbol-name id) (format "%s" id))
                                                  s))))
                                    (list menu-key lbl cmd id sec)))))
               ;; Flatten to resolve unique keys globally; uniqueness by full sequence ("t c" vs "c")
               (flat (apply #'append per-sec))
               (used (make-hash-table :test 'equal))
               ;; Reserve plain "t" if any multi-stroke key starts with "t ".
               ;; This prevents assigning single-key 't' to an item which would shadow
               ;; sequences like "t c"/"t f"/… inside the transient keymap.
               (_ (when (cl-some (lambda (it)
                                   (let ((b (nth 0 it)))
                                     (and (stringp b)
                                          (string-prefix-p "t " b))))
                                 flat)
                    (puthash "t" t used)))
               (unique
                (cl-loop for it in flat
                         for base = (nth 0 it)          ;; may be "t c" or "c" or "RET"
                         for lbl  = (nth 1 it)
                         for cmd  = (nth 2 it)
                         for id   = (nth 3 it)
                         for sec  = (nth 4 it)
                         for idc  = (substring (symbol-name id) 0 1)
                         ;; Fallback candidates:
                         ;; 1) full sequence ("t c"), 2) last token ("c"),
                         ;; 3) UPPER(last token), 4..) digits.
                         ;; Special keys (RET, M-RET, TAB, SPC, DEL, BACKSPACE) are preserved as-is.
                         for lastonly = (and (stringp base)
                                             (car (last (split-string base " " t))))
                         for special-keys = '("RET" "M-RET" "TAB" "SPC" "DEL" "BACKSPACE")
                         for cand = (if (and (stringp base) (member base special-keys))
                                        (list base)
                                      (delq nil (list base lastonly (and lastonly (upcase lastonly))
                                                      idc "1" "2" "3" "4" "5" "6" "7" "8" "9")))
                         for final = (if (and (stringp base) (member base special-keys))
                                         base
                                       (cl-loop for c in cand
                                                when (and (stringp c) (not (gethash c used)))
                                                return c))
                         do (puthash (or final base "x") t used)
                         collect (list (or final base "x") lbl cmd id sec)))
               ;; Build transient spec: a vector per section (column) with i18n title
               (build-col
                (lambda (sec)
                  (let* ((title (pcase sec
                                  ('navigate (if (fboundp 'carriage-i18n) (carriage-i18n :navigate-title) "Navigate"))
                                  ('act      (if (fboundp 'carriage-i18n) (carriage-i18n :act-title) "Actions"))
                                  ('context  (if (fboundp 'carriage-i18n) (carriage-i18n :context-title) "Context"))
                                  ('session  (if (fboundp 'carriage-i18n) (carriage-i18n :session-title) "Session/Git"))
                                  ('tools    (if (fboundp 'carriage-i18n) (carriage-i18n :tools-title) "Tools"))
                                  ('logs     (if (fboundp 'carriage-i18n) (carriage-i18n :logs-title) "Logs"))
                                  (_ "Carriage")))
                         ;; Add a small help-echo for the Context column to explain two-stroke keys.
                         (title* (if (eq sec 'context)
                                     (propertize title 'help-echo (if (fboundp 'carriage-i18n)
                                                                      (carriage-i18n :context-help)
                                                                    "Press t, then letter (g,f,p,v,a,l,s,P)"))
                                   title))
                         (items (cl-loop for it in unique
                                         for ukey = (nth 0 it)
                                         for lbl  = (nth 1 it)
                                         for cmd  = (nth 2 it)
                                         for s    = (nth 4 it)
                                         when (and (eq s sec) (commandp cmd))
                                         collect `(,ukey ,lbl ,cmd))))
                    (vconcat (list title*) items))))
               (cols (cl-loop for sec in sections
                              for col = (funcall build-col sec)
                              when (> (length col) 1)
                              collect col))
               (menu-title (if (and (require 'carriage-i18n nil t)
                                    (fboundp 'carriage-i18n))
                               (carriage-i18n :carriage-menu)
                             "Carriage Menu")))
          ;; Redefine transient prefix dynamically
          (when (fboundp 'carriage-keys--menu)
            (fset 'carriage-keys--menu nil))
          ;; Build multi-column layout: top-level vector with title and nested column vectors.
          (let ((layout (apply #'vector (cons menu-title cols))))
            (eval
             `(transient-define-prefix carriage-keys--menu ()
                ,layout)))
          (call-interactively #'carriage-keys--menu))
      ;; Fallback: completing-read with section prefix in label
      (let* ((_ (require 'carriage-i18n nil t))
             (pairs
              (mapcar
               (lambda (pl)
                 (let* ((id  (plist-get pl :id))
                        (cmd (plist-get pl :cmd))
                        (sec (plist-get pl :section))
                        (desc-key (plist-get pl :desc-key))
                        (sec-name (pcase sec
                                    ('navigate (if (fboundp 'carriage-i18n) (carriage-i18n :navigate-title) "Navigate"))
                                    ('act      (if (fboundp 'carriage-i18n) (carriage-i18n :act-title) "Actions"))
                                    ('session  (if (fboundp 'carriage-i18n) (carriage-i18n :session-title) "Session/Git"))
                                    ('tools    (if (fboundp 'carriage-i18n) (carriage-i18n :tools-title) "Tools"))
                                    ('logs     (if (fboundp 'carriage-i18n) (carriage-i18n :logs-title) "Logs"))
                                    (_ "Other")))
                        (lbl (or (and (fboundp 'carriage-i18n) (carriage-i18n desc-key))
                                 (and (symbolp cmd) (symbol-name cmd))
                                 (format "%s" id)))
                        (label (format "[%s] %s" sec-name lbl)))
                   (cons label cmd)))
               acts))
             (choice (completing-read "Carriage action: " (mapcar #'car pairs) nil t)))
        (let ((cmd (cdr (assoc choice pairs))))
          (when (commandp cmd)
            (call-interactively cmd)))))))

;;;###autoload
(defun carriage-keys-which-key-register ()
  "Register which-key replacements for Carriage prefix keys (i18n-aware)."
  (interactive)
  (when (require 'which-key nil t)
    (let* ((_ (require 'carriage-i18n nil t))
           (prefixes (carriage-keys-prefixes))
           (base (car prefixes))
           (aliases (cdr prefixes))
           (menu   (if (fboundp 'carriage-i18n) (carriage-i18n :carriage-menu) "Carriage Menu"))
           (toggles (if (fboundp 'carriage-i18n) (carriage-i18n :carriage-toggles) "Carriage Toggles"))
           (task (if (fboundp 'carriage-i18n)
                     (or (carriage-i18n :task-new) "Create task doc")
                   "Create task doc"))
           (filechat (if (fboundp 'carriage-i18n)
                         (or (carriage-i18n :file-chat) "File chat")
                       "File chat"))
           (insert (if (fboundp 'carriage-i18n)
                       (or (carriage-i18n :insert-assist) "Insert/Assist")
                     "Insert/Assist"))
           (insert-menu (if (fboundp 'carriage-i18n)
                            (or (carriage-i18n :insert-assist-menu) "Insert/Assist Menu")
                          "Insert/Assist Menu")))
      (when base
        (which-key-add-key-based-replacements base menu)
        (which-key-add-key-based-replacements (concat base " t") toggles)
        ;; Two-stroke toggles inside the transient/menu
        ;; Mnemonic alias inside transient/group: t g — gptel-context
        (which-key-add-key-based-replacements (concat base " t g")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :ctx-tooltip) "Toggle gptel-context")
            "Toggle gptel-context"))
        (which-key-add-key-based-replacements (concat base " t f")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :files-tooltip) "Toggle document files")
            "Toggle document files"))
        (which-key-add-key-based-replacements (concat base " t p")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :patched-tooltip) "Applied patch files")
            "Applied patch files"))
        (which-key-add-key-based-replacements (concat base " t v")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :visible-tooltip) "Toggle visible buffers")
            "Toggle visible buffers"))
        (which-key-add-key-based-replacements (concat base " t a")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :doc-scope-all-tip) "Use all begin_context blocks")
            "Use all begin_context blocks"))
        (which-key-add-key-based-replacements (concat base " t l")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :doc-scope-last-tip) "Use last/nearest begin_context block")
            "Use last/nearest begin_context block"))
        (which-key-add-key-based-replacements (concat base " t s")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :doc-scope-cycle-tip) "Cycle doc scope")
            "Cycle doc scope"))
        (which-key-add-key-based-replacements (concat base " t P")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :toggle-profile) "Toggle P1/P3")
            "Toggle P1/P3"))
        (which-key-add-key-based-replacements (concat base " n") task)
        (which-key-add-key-based-replacements (concat base " f") filechat)
        ;; Insert/Assist group and menu
        (which-key-add-key-based-replacements (concat base " x") insert)
        (which-key-add-key-based-replacements (concat base " x x") insert-menu))
      (dolist (alias aliases)
        (which-key-add-key-based-replacements alias menu)
        (which-key-add-key-based-replacements (concat alias " t") toggles)
        ;; Two-stroke toggles inside the transient/menu for alias prefixes
        ;; Mnemonic alias inside transient/group: t g — gptel-context
        (which-key-add-key-based-replacements (concat alias " t g")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :ctx-tooltip) "Toggle gptel-context")
            "Toggle gptel-context"))
        (which-key-add-key-based-replacements (concat alias " t f")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :files-tooltip) "Toggle document files")
            "Toggle document files"))
        (which-key-add-key-based-replacements (concat alias " t p")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :patched-tooltip) "Applied patch files")
            "Applied patch files"))
        (which-key-add-key-based-replacements (concat alias " t v")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :visible-tooltip) "Toggle visible buffers")
            "Toggle visible buffers"))
        (which-key-add-key-based-replacements (concat alias " t a")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :doc-scope-all-tip) "Use all begin_context blocks")
            "Use all begin_context blocks"))
        (which-key-add-key-based-replacements (concat alias " t l")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :doc-scope-last-tip) "Use last/nearest begin_context block")
            "Use last/nearest begin_context block"))
        (which-key-add-key-based-replacements (concat alias " t s")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :doc-scope-cycle-tip) "Cycle doc scope")
            "Cycle doc scope"))
        (which-key-add-key-based-replacements (concat alias " t P")
          (if (fboundp 'carriage-i18n)
              (or (carriage-i18n :toggle-profile) "Toggle P1/P3")
            "Toggle P1/P3"))
        ;; Additional common entries under alias prefixes
        (which-key-add-key-based-replacements (concat alias " n") task)
        (which-key-add-key-based-replacements (concat alias " f") filechat)
        ;; Insert/Assist group and menu for alias
        (which-key-add-key-based-replacements (concat alias " x") insert)
        (which-key-add-key-based-replacements (concat alias " x x") insert-menu))
      t)))

(defun carriage-keys-which-key-unregister ()
  "Remove which-key replacements for Carriage prefix keys, if present."
  (interactive)
  (when (require 'which-key nil t)
    (let* ((prefixes (carriage-keys-prefixes))
           (base (car prefixes))
           (aliases (cdr prefixes)))
      (when (and base (fboundp 'which-key-remove-key-based-replacements))
        (ignore-errors (which-key-remove-key-based-replacements base))
        ;; Base toggle group and individual two-stroke entries
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t")))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t g")))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t f")))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t p")))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t v")))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t a")))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t l")))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t s")))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " t P")))
        ;; Other common entries
        (ignore-errors (which-key-remove-key-based-replacements (concat base " n")))
        (ignore-errors (which-key-remove-key-based-replacements (concat base " f"))))
      (dolist (alias aliases)
        (when (fboundp 'which-key-remove-key-based-replacements)
          (ignore-errors (which-key-remove-key-based-replacements alias))
          ;; Toggle group and entries for each alias
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " t")))
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " t g")))
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " t f")))
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " t p")))
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " t v")))
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " t a")))
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " t l")))
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " t s")))
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " t P")))
          ;; Other common entries under alias
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " n")))
          (ignore-errors (which-key-remove-key-based-replacements (concat alias " f")))))
      t)))

(provide 'carriage-keyspec)
;;; carriage-keyspec.el ends here
