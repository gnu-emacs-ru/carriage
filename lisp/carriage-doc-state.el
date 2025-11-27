;;; carriage-doc-state.el --- Persist/restore document state in Org -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: org, persistence, convenience
;;
;; Specifications:
;;   spec/doc-state-v2.org
;;   spec/ui-v2.org
;;   spec/project-overview-v2.org
;;
;;; Commentary:
;; Store and restore Carriage parameters in the Org document itself.
;; A single top-level heading "* Carriage State" with a property drawer
;; is used as the canonical storage. Fallback reader supports #+PROPERTY
;; lines "CARRIAGE_* ...", but writing always updates the drawer.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org nil t)
(require 'carriage-block-fold nil t)
;; Share kinds setting with the generic block-fold module
(defvaralias 'carriage-doc-state-fold-kinds 'carriage-block-fold-kinds)

(declare-function carriage-project-root "carriage-utils" ())
(declare-function carriage-llm-full-id "carriage-mode" (&optional backend provider model))
(declare-function carriage-llm-resolve-model "carriage-mode" (backend provider model))
(declare-function carriage-mode "carriage-mode" (&optional arg))
(declare-function carriage-log "carriage-logging" (fmt &rest args))

(defgroup carriage-doc-state nil
  "Persist and restore Carriage state in Org documents."
  :group 'applications
  :prefix "carriage-doc-state-")

(defcustom carriage-doc-state-save-on-save nil
  "When non-nil, write current Carriage state to the document before each save (buffer-local)."
  :type 'boolean
  :group 'carriage-doc-state)
(make-variable-buffer-local 'carriage-doc-state-save-on-save)

(defcustom carriage-doc-state-sync-on-change nil
  "When non-nil, update document properties on each state-changing command (buffer-local)."
  :type 'boolean
  :group 'carriage-doc-state)
(make-variable-buffer-local 'carriage-doc-state-sync-on-change)

(defcustom carriage-doc-state-heading "Carriage State"
  "Title of the top-level heading used to store Carriage properties."
  :type 'string
  :group 'carriage-doc-state)

(defun carriage-doc-state--bool->str (v)
  "Normalize boolean V to \"t\" or \"nil\" string."
  (if v "t" "nil"))

(defun carriage-doc-state--str->bool (s)
  "Normalize string S to boolean t/nil. Accepts t/true/1 and nil/false/0."
  (let ((x (downcase (format "%s" s))))
    (cond
     ((member x '("t" "true" "1" "yes" "on")) t)
     ((member x '("nil" "false" "0" "no" "off" "")) nil)
     (t nil))))

(defun carriage-doc-state--goto-or-create-heading ()
  "Move point to the Carriage State heading; create it if needed.
Returns point at the heading line."
  (unless (derived-mode-p 'org-mode)
    (user-error "carriage-doc-state: buffer is not in org-mode"))
  (save-excursion
    (goto-char (point-min))
    (let ((rx (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote carriage-doc-state-heading))))
      (if (re-search-forward rx nil t)
          (match-beginning 0)
        ;; Create heading at top (after file-wide #+ options, if any)
        (goto-char (point-min))
        ;; Skip file keywords (#+...)
        (while (looking-at-p "^[ \t]*#\\+")
          (forward-line 1))
        (let ((pos (point)))
          (insert (format "* %s\n:PROPERTIES:\n:END:\n" carriage-doc-state-heading))
          pos)))))

(defun carriage-doc-state--find-heading ()
  "Return position of Carriage State heading, or nil if absent."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((rx (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote carriage-doc-state-heading))))
        (when (re-search-forward rx nil t)
          (match-beginning 0))))))

(defun carriage-doc-state--within-heading (pos)
  "Narrow to heading at POS; return non-nil if successful."
  (when (and (numberp pos) (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char pos)
      (ignore-errors (org-back-to-heading t))
      (let ((beg (point))
            (end (save-excursion (org-end-of-subtree t t) (point))))
        (save-restriction
          (narrow-to-region beg end)
          t)))))

(defun carriage-doc-state--read-drawer ()
  "Read properties from the Carriage State drawer into an alist of (KEY . VAL)."
  (let ((pos (carriage-doc-state--find-heading)))
    (when pos
      (with-temp-buffer
        (let (alist)
          (save-excursion
            (with-current-buffer (current-buffer)
              ;; Use original buffer to extract properties via org-entry-get
              (with-current-buffer (window-buffer (selected-window))
                (save-excursion
                  (goto-char pos)
                  (ignore-errors (org-back-to-heading t))
                  (let* ((props (ignore-errors (org-entry-properties nil 'standard))))
                    (dolist (cell props)
                      ;; Keep :KEY as presented by org, e.g., "CAR_MODE"
                      (let ((k (car cell))
                            (v (cdr cell)))
                        (when (and (stringp k)
                                   (string-prefix-p "CAR_" k))
                          (push (cons k v) alist)))))))))
          alist)))))

(defun carriage-doc-state--read-properties-lines ()
  "Fallback: read file-level #+PROPERTY: CARRIAGE_* lines into an alist."
  (save-excursion
    (goto-char (point-min))
    (let ((alist '()))
      (while (re-search-forward "^[ \t]*\\(?:#\\+\\)?PROPERTY:[ \t]+\\(CARRIAGE_[A-Z0-9_]+\\)[ \t]+\\(.+\\)$" nil t)
        (let ((k (match-string 1))
              (v (string-trim (match-string 2))))
          (push (cons k v) alist)))
      alist)))

(defun carriage-doc-state--alist->plist (alist)
  "Turn ALIST of (KEY . VAL) into a plist with :KEY VAL (keys normalized to upper-case)."
  (let ((pl '()))
    (dolist (cell alist)
      (let* ((k (car cell))
             (v (cdr cell))
             (key (format ":%s" (upcase (string-remove-prefix ":" (format "%s" k))))))
        (setq pl (append pl (list (intern key) v)))))
    pl))

(defun carriage-doc-state-read (&optional buffer)
  "Read Carriage State from BUFFER (or current) with priority:
1) #+begin_carriage block,
2) file-level #+PROPERTY: CARRIAGE_*,
3) legacy \"Carriage State\" property drawer.
Returns a plist with :CAR_* keys."
  (carriage-doc-state--read-preferring-file buffer))

(defun carriage-doc-state--set-prop (key val)
  "Set org property KEY to VAL in the Carriage State heading (create if missing)."
  (let ((pos (carriage-doc-state--goto-or-create-heading)))
    (save-excursion
      (goto-char pos)
      (ignore-errors (org-back-to-heading t))
      (let ((inhibit-read-only t))
        (org-set-property key (format "%s" val))))))

(defun carriage-doc-state--collect-current ()
  "Collect current buffer Carriage parameters as an alist of (KEY . VAL).

Preserves provenance keys written by branching (e.g., CAR_TEMPLATE_ID/VER,
CAR_CONTEXT_PROFILE, CAR_INHERITED) if they are already present in the
document's #+begin_carriage block. This prevents auto-save from dropping
those fields."
  (let* ((intent (and (boundp 'carriage-mode-intent) carriage-mode-intent))
         (suite  (and (boundp 'carriage-mode-suite) carriage-mode-suite))
         (backend (and (boundp 'carriage-mode-backend) carriage-mode-backend))
         (provider (and (boundp 'carriage-mode-provider) carriage-mode-provider))
         (model  (and (boundp 'carriage-mode-model) carriage-mode-model))
         (full-id (ignore-errors (carriage-llm-full-id backend provider model)))
         (engine (and (fboundp 'carriage-apply-engine) (carriage-apply-engine)))
         (branch (and (eq engine 'git)
                      (boundp 'carriage-git-branch-policy)
                      carriage-git-branch-policy))
         (ctx-g (and (boundp 'carriage-mode-include-gptel-context) carriage-mode-include-gptel-context))
         (ctx-d (and (boundp 'carriage-mode-include-doc-context) carriage-mode-include-doc-context))
         (rpt   (and (boundp 'carriage-mode-report-open-policy) carriage-mode-report-open-policy))
         (stage (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy))
         (icons (and (boundp 'carriage-mode-use-icons) carriage-mode-use-icons))
         (flash (and (boundp 'carriage-mode-flash-patches) carriage-mode-flash-patches))
         (audio (and (boundp 'carriage-mode-audio-notify) carriage-mode-audio-notify))
         ;; Base alist collected from current in-memory state
         (base (cl-remove-if-not
                #'identity
                (list
                 (cons "CAR_MODE" (carriage-doc-state--bool->str
                                   (and (boundp 'carriage-mode) carriage-mode)))
                 (and intent (cons "CAR_INTENT" (format "%s" intent)))
                 (and suite  (cons "CAR_SUITE"  (format "%s" suite)))
                 (and full-id (cons "CAR_MODEL_ID" full-id))
                 (and engine (cons "CAR_ENGINE" (format "%s" engine)))
                 (and branch (cons "CAR_BRANCH_POLICY" (format "%s" branch)))
                 (cons "CAR_CTX_GPTEL" (carriage-doc-state--bool->str ctx-g))
                 (cons "CAR_CTX_DOC"   (carriage-doc-state--bool->str ctx-d))
                 (cons "CAR_CTX_VISIBLE"
                       (carriage-doc-state--bool->str
                        (and (boundp 'carriage-mode-include-visible-context)
                             carriage-mode-include-visible-context)))
                 (and rpt   (cons "CAR_REPORT_POLICY" (format "%s" rpt)))
                 (and stage (cons "CAR_STAGE_POLICY" (format "%s" stage)))
                 (cons "CAR_ICONS"        (carriage-doc-state--bool->str icons))
                 (cons "CAR_FLASH"        (carriage-doc-state--bool->str flash))
                 (cons "CAR_AUDIO_NOTIFY" (carriage-doc-state--bool->str audio)))))
         ;; Provenance extras already present in the buffer (from branching)
         (existing-pl (ignore-errors (carriage-doc-state-read (current-buffer)))))
    (let ((alist base))
      ;; Merge important keys from the existing document state to avoid losing them
      ;; when current in-memory vars are not bound (e.g., carriage-mode not loaded in tests).
      (when (listp existing-pl)
        (let ((pl existing-pl)
              (keep '("CAR_MODE" "CAR_INTENT" "CAR_SUITE" "CAR_MODEL_ID"
                      "CAR_ENGINE" "CAR_BRANCH_POLICY"
                      "CAR_CTX_GPTEL" "CAR_CTX_DOC" "CAR_CTX_VISIBLE"
                      "CAR_REPORT_POLICY" "CAR_STAGE_POLICY"
                      "CAR_ICONS" "CAR_FLASH" "CAR_AUDIO_NOTIFY"
                      "CAR_CONTEXT_PROFILE" "CAR_INHERITED")))
          (while pl
            (let* ((k (car pl))
                   (v (cadr pl)))
              (when (and (keywordp k))
                (let* ((ks (symbol-name k))                  ; e.g., ":CAR_TEMPLATE_ID"
                       (plain (upcase (string-remove-prefix ":" ks))))
                  (when (or (string-prefix-p "CAR_TEMPLATE_" plain)
                            (member plain keep))
                    (unless (assoc plain alist)
                      (push (cons plain (format "%s" v)) alist))))))
            (setq pl (cddr pl)))))
      alist)))

(defun carriage-doc-state-write (data &optional buffer)
  "Write DATA into the canonical Carriage storage (begin_carriage) of BUFFER (or current).
DATA may be:
- a plist (:CAR_* …) or
- an alist ((\"CAR_*\" . VAL) …).
Returns t on success."
  (carriage-doc-state--write-to-file-properties data buffer))

(defun carriage-doc-state-write-current (&optional buffer)
  "Write current buffer Carriage parameters into the document and fold the block."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((alist (carriage-doc-state--collect-current))
           (res   (carriage-doc-state-write alist (current-buffer)))
           (rg1   (carriage-doc-state--carriage-block-range)))
      (carriage-log "doc-state: write-current keys=%d res=%s block-after=%s"
                    (length alist) (if res "t" "nil") (if rg1 "present" "absent")))
    (ignore-errors (carriage-doc-state-hide (current-buffer)))))

(defun carriage-doc-state-restore (&optional buffer)
  "Restore Carriage parameters from the document for BUFFER (or current).
Gracefully ignores missing keys."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((pl (carriage-doc-state-read (current-buffer))))
      ;; Mode (enable/disable carriage-mode) — avoid redundant toggles
      (let ((m (plist-get pl :CAR_MODE)))
        (when m
          (ignore-errors
            (let ((want (carriage-doc-state--str->bool m))
                  (cur  (and (boundp 'carriage-mode) carriage-mode)))
              (when (not (eq cur want))
                (carriage-mode (if want 1 0)))))))
      ;; Intent
      (let ((v (plist-get pl :CAR_INTENT)))
        (when v (setq carriage-mode-intent (intern (format "%s" v)))))
      ;; Suite
      (let ((v (plist-get pl :CAR_SUITE)))
        (when v (setq carriage-mode-suite (intern (format "%s" v)))))
      ;; Model/backend/provider
      (let ((id (plist-get pl :CAR_MODEL_ID)))
        (when (and (stringp id) (> (length id) 0))
          (let* ((parts (split-string id ":" t))
                 (n (length parts))
                 (backend (and (>= n 1) (intern (nth 0 parts))))
                 (provider (and (>= n 3) (nth 1 parts)))
                 (model    (cond ((>= n 3) (nth 2 parts))
                                 ((= n 2) (nth 1 parts))
                                 ((= n 1) (nth 0 parts))
                                 (t id))))
            (when backend (setq carriage-mode-backend backend))
            (setq carriage-mode-provider (and provider (not (string-empty-p provider)) provider))
            (when model (setq carriage-mode-model model)))))
      ;; Engine + policy
      (let ((eng (plist-get pl :CAR_ENGINE)))
        (when eng
          (setq carriage-apply-engine (intern (format "%s" eng)))))
      (let ((pol (plist-get pl :CAR_BRANCH_POLICY)))
        (when (and pol (boundp 'carriage-git-branch-policy))
          (setq carriage-git-branch-policy (intern (format "%s" pol)))))
      ;; Context toggles
      (let ((g (plist-get pl :CAR_CTX_GPTEL)))
        (when g (setq-local carriage-mode-include-gptel-context (carriage-doc-state--str->bool g))))
      (let ((d (plist-get pl :CAR_CTX_DOC)))
        (when d (setq-local carriage-mode-include-doc-context (carriage-doc-state--str->bool d))))
      ;; Report policy
      (let ((rp (plist-get pl :CAR_REPORT_POLICY)))
        (when rp (setq-local carriage-mode-report-open-policy (intern (format "%s" rp)))))
      ;; Stage policy
      (let ((st (plist-get pl :CAR_STAGE_POLICY)))
        (when st (setq carriage-apply-stage-policy (intern (format "%s" st)))))
      ;; UI toggles
      (let ((ic (plist-get pl :CAR_ICONS)))
        (when ic (setq-local carriage-mode-use-icons (carriage-doc-state--str->bool ic))))
      (let ((fl (plist-get pl :CAR_FLASH)))
        (when fl (setq-local carriage-mode-flash-patches (carriage-doc-state--str->bool fl))))
      (let ((au (plist-get pl :CAR_AUDIO_NOTIFY)))
        (when au (setq-local carriage-mode-audio-notify (carriage-doc-state--str->bool au))))
      (force-mode-line-update t)
      t)))

(defun carriage-doc-state-hide (&optional buffer)
  "Fold property drawer of the Carriage State heading and the begin_carriage block in BUFFER (or current)."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'org-mode)
      ;; Keep property drawer behavior minimal (no org-fold toggles for our block).
      (let ((pos (carriage-doc-state--find-heading)))
        (when pos
          (ignore-errors
            (save-excursion (goto-char pos) (outline-hide-subtree)))))
      ;; Ensure our overlay fold for the Carriage block.
      (ignore-errors (carriage-doc-state--ensure-carriage-block-hidden (current-buffer))))))

(defun carriage-doc-state-auto-enable ()
  "Find-file hook: auto-enable carriage-mode when CAR_MODE=t or a begin_carriage block is present.
Requires an Org buffer and a recognizable project root."
  (when (derived-mode-p 'org-mode)
    (condition-case _e
        (let* ((pl (carriage-doc-state-read (current-buffer)))
               (m  (plist-get pl :CAR_MODE))
               (on (carriage-doc-state--str->bool (or m "")))
               (has-block (ignore-errors (carriage-doc-state--carriage-block-range))))
          (when (and (fboundp 'carriage-project-root)
                     (carriage-project-root)
                     (or on has-block))
            (require 'carriage-mode)
            (carriage-mode 1)))
      (error nil))))

(defun carriage-doc-state--maybe-auto-enable ()
  "Auto-enable carriage-mode on visit when carriage-global-mode is active.
Calls `carriage-doc-state-auto-enable' only if global mode is on."
  (when (bound-and-true-p carriage-global-mode)
    (ignore-errors (carriage-doc-state-auto-enable))))

;; -------------------------------------------------------------------
;; v1.1+: Prefer file-level #+PROPERTY: CARRIAGE_* storage
;; - Override read/write to use #+PROPERTY lines as canonical storage.
;; - Hide those lines by default (using an invisibility spec).
;; - Keep drawer-based heading readable for backwards compatibility.

(defun carriage-doc-state--prop-key->file (key)
  "Map drawer key string like \"CAR_MODE\" to file-level key \"CARRIAGE_MODE\"."
  (let* ((s (format "%s" key)))
    (cond
     ((string-match-p "\\`CARRIAGE_" s) s)
     ((string-match-p "\\`CAR_" s)
      (concat "CARRIAGE_" (substring s 4)))
     (t (concat "CARRIAGE_" s)))))

(defun carriage-doc-state--file-key->drawer (key)
  "Map file-level key string like \"CARRIAGE_MODE\" to drawer key \"CAR_MODE\"."
  (let ((s (format "%s" key)))
    (if (string-prefix-p "CARRIAGE_" s)
        (concat "CAR_" (substring s 9))
      s)))

(defun carriage-doc-state--set-file-property (key val)
  "Upsert a single #+PROPERTY line for KEY (string) to VAL (string)."
  (save-excursion
    (let* ((inhibit-read-only t)
           (k (upcase (format "%s" key)))
           (line (format "#+PROPERTY: %s %s" k (or val "")))
           (rx (format "^[ \t]*#\\+PROPERTY:[ \t]+%s\\b" (regexp-quote k))))
      (goto-char (point-min))
      (if (re-search-forward rx nil t)
          (progn
            (goto-char (match-beginning 0))
            (delete-region (line-beginning-position) (line-end-position))
            (insert line))
        ;; Insert after existing #+ lines (file keywords)
        (goto-char (point-min))
        (while (looking-at-p "^[ \t]*#\\+")
          (forward-line 1))
        (let ((pos (point)))
          (unless (bolp) (insert "\n"))
          (goto-char pos)
          (insert line "\n"))))
    t))

(defun carriage-doc-state--write-to-file-properties (data &optional buffer)
  "Override writer: write DATA to #+PROPERTY: CARRIAGE_* lines in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((alist
            (cond
             ((and (listp data) (keywordp (car data)))
              (let ((acc '()) (pl data))
                (while pl
                  (let ((k (car pl)) (v (cadr pl)))
                    (setq acc (append acc
                                      (list (cons (upcase (string-remove-prefix ":" (format "%s" k))) v)))))
                  (setq pl (cddr pl)))
                acc))
             ((and (listp data) (consp (car data))) data)
             (t (user-error "Unsupported DATA format for carriage-doc-state-write")))))
      (dolist (cell alist)
        (let* ((k (car cell))
               (v (cdr cell))
               (file-k (carriage-doc-state--prop-key->file k)))
          (carriage-doc-state--set-file-property file-k v)))
      t)))

(defun carriage-doc-state--read-preferring-file (&optional buffer)
  "Read state from BUFFER preferring file-level #+PROPERTY: CARRIAGE_* lines.
Falls back to drawer-based heading when file-level properties are absent."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((props (carriage-doc-state--read-properties-lines)))
      (if (and props (> (length props) 0))
          (let* ((mapped (mapcar (lambda (cell)
                                   (cons (carriage-doc-state--file-key->drawer (car cell))
                                         (cdr cell)))
                                 props)))
            (carriage-doc-state--alist->plist mapped))
        ;; Fallback to drawer
        (carriage-doc-state--alist->plist (carriage-doc-state--read-drawer))))))

(defun carriage-doc-state--on-before-save ()
  "Before-save: apply state from begin_carriage (if present), then persist normalized block and fold.
No-op unless `carriage-doc-state-save-on-save' is non-nil in this buffer."
  (when (and (derived-mode-p 'org-mode)
             (boundp 'carriage-doc-state-save-on-save)
             carriage-doc-state-save-on-save)
    (carriage-log "doc-state: before-save (buf=%s modified=%s)"
                  (buffer-name) (buffer-modified-p))
    (condition-case e
        (progn
          ;; Apply user-edited state first (if valid), then normalize/write and fold.
          (ignore-errors (carriage-doc-state-restore (current-buffer)))
          (ignore-errors (carriage-doc-state-write-current (current-buffer)))
          (let ((present (and (carriage-doc-state--carriage-block-range) t)))
            (unless present
              ;; Fallback: write block directly if alias chain failed
              (let* ((alist (carriage-doc-state--collect-current))
                     (ok (carriage-doc-state--write-carriage-block alist)))
                (carriage-log "doc-state: before-save fallback write-carriage-block=%s"
                              (if ok "t" "nil")))))
          (ignore-errors (carriage-doc-state--fold-carriage-block-now (current-buffer)))
          (carriage-log "doc-state: before-save done (block=%s)"
                        (if (carriage-doc-state--carriage-block-range) "present" "absent"))
          t)
      (error
       (carriage-log "doc-state: before-save error: %s" (error-message-string e))
       nil))))

(defun carriage-doc-state-install-save-hook ()
  "Install buffer-local hooks to persist Carriage state on save (idempotent).

Adds both:
- before-save-hook (runs for modified buffers)
- write-contents-functions (runs even on 'clean' saves).

Only installed when opt-in is enabled in this buffer and not already present."
  (when (and (derived-mode-p 'org-mode)
             (boundp 'carriage-doc-state-save-on-save)
             carriage-doc-state-save-on-save
             (not carriage-doc-state--save-hooks-installed))
    (add-hook 'before-save-hook #'carriage-doc-state--on-before-save nil t)
    (add-hook 'write-contents-functions #'carriage-doc-state--on-write-contents nil t)
    (setq carriage-doc-state--save-hooks-installed t)
    (carriage-log "doc-state: installing save hooks in %s" (buffer-name))))

(defun carriage-doc-state--on-write-contents ()
  "Persist begin_carriage on 'clean' saves; always return nil to continue normal save.
No-op unless `carriage-doc-state-save-on-save' is non-nil in this buffer."
  (when (and (derived-mode-p 'org-mode)
             (boundp 'carriage-doc-state-save-on-save)
             carriage-doc-state-save-on-save)
    (carriage-log "doc-state: write-contents fired (buf=%s modified=%s)"
                  (buffer-name) (buffer-modified-p))
    (condition-case e
        (progn
          (ignore-errors (carriage-doc-state-restore (current-buffer)))
          (ignore-errors (carriage-doc-state-write-current (current-buffer)))
          (let ((present (and (carriage-doc-state--carriage-block-range) t)))
            (unless present
              ;; Fallback: write block directly if alias chain failed
              (let* ((alist (carriage-doc-state--collect-current))
                     (ok (carriage-doc-state--write-carriage-block alist)))
                (carriage-log "doc-state: write-contents fallback write-carriage-block=%s"
                              (if ok "t" "nil")))))
          (ignore-errors (carriage-doc-state--fold-carriage-block-now (current-buffer)))
          (carriage-log "doc-state: write-contents done (block=%s)"
                        (if (carriage-doc-state--carriage-block-range) "present" "absent")))
      (error
       (carriage-log "doc-state: write-contents error: %s" (error-message-string e)))))
  ;; Return nil to let Emacs proceed with normal saving
  nil)

(defun carriage-doc-state-remove-save-hook ()
  "Remove buffer-local save hooks for persisting document state."
  (remove-hook 'before-save-hook #'carriage-doc-state--on-before-save t)
  (remove-hook 'write-contents-functions #'carriage-doc-state--on-write-contents t)
  (setq carriage-doc-state--save-hooks-installed nil))

(defvar-local carriage-doc-state--props-overlay nil
  "Overlay covering folded file-level Carriage property lines.")
(defvar-local carriage-doc-state--save-hooks-installed nil
  "Non-nil when doc-state save hooks are installed in this buffer.")

(defun carriage-doc-state--props-range ()
  "Return (BEG END COUNT) for contiguous file-level CARRIAGE_* property lines, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((rx "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_[A-Z0-9_]+\\b.*$")
          (beg nil) (end nil) (count 0))
      (while (re-search-forward rx nil t)
        (unless beg (setq beg (line-beginning-position)))
        (setq end (line-end-position))
        (setq count (1+ count)))
      (when beg (list beg end count)))))

(defun carriage-doc-state--hide-file-properties ()
  "Fold file-level #+PROPERTY: CARRIAGE_* lines into a single overlay.
The overlay is marked invisible via `carriage-doc-state' and shows a one-line
placeholder that can be toggled with `carriage-doc-state-toggle-visibility'."
  (when (derived-mode-p 'org-mode)
    (let ((range (carriage-doc-state--props-range)))
      (when range
        (let ((b (nth 0 range)) (e (nth 1 range)) (n (nth 2 range)))
          (when (overlayp carriage-doc-state--props-overlay)
            (delete-overlay carriage-doc-state--props-overlay))
          (let ((ov (make-overlay b e)))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'invisible 'carriage-doc-state)
            (add-to-invisibility-spec 'carriage-doc-state)
            (overlay-put ov 'before-string
                         (propertize
                          (format "… Carriage State properties (%d) — M-x carriage-doc-state-toggle-visibility …\n" n)
                          'face 'shadow))
            (setq carriage-doc-state--props-overlay ov)
            t))))))

;; -------------------------------------------------------------------
;; begin_carriage block folding via overlay (org-fold independent)

(defvar-local carriage-doc-state--carriage-block-overlay nil
  "Overlay that hides the #+begin_carriage … #+end_carriage block.")
;; Generic registry of folded overlays per block kind (e.g., 'carriage, later 'context, …).
(defvar-local carriage-doc-state--block-overlays nil
  "Alist of (KIND . OVERLAY) for folded begin_<kind> blocks in this buffer.")

(defun carriage-doc-state--carriage-block-overlay-range ()
  "Return (BEG . END) for overlay covering the entire begin_carriage block lines.
Returns nil when no block is present."
  (let ((range (carriage-doc-state--carriage-block-range)))
    (when range
      (let* ((body-beg (car range))
             (body-end (cdr range))
             (beg (save-excursion
                    (goto-char body-beg)
                    (forward-line -1)
                    (line-beginning-position)))
             (end (save-excursion
                    (goto-char body-end)
                    (end-of-line)
                    (point))))
        (cons beg end)))))

(defun carriage-doc-state--ensure-carriage-block-hidden (&optional buffer)
  "Idempotently hide #+begin_carriage block in BUFFER (or current) using a reusable module."
  (carriage-block-fold-ensure-overlay 'carriage (or buffer (current-buffer))))

(defun carriage-doc-state--reveal-carriage-block (&optional buffer)
  "Reveal #+begin_carriage block in BUFFER (or current) without removing its overlay."
  (carriage-block-fold-reveal 'carriage (or buffer (current-buffer)))
  t)

;; Hook into existing API:
;; - Override read/write to use file-level properties.
;; - After hide, also hide file-level property lines.

(defun carriage-doc-state--show-file-properties ()
  "Remove invisibility from #+PROPERTY: CARRIAGE_* lines in the current buffer."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let ((rx "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_[A-Z0-9_]+\\b.*$"))
        (goto-char (point-min))
        (while (re-search-forward rx nil t)
          (let ((b (line-beginning-position))
                (e (line-end-position)))
            (remove-text-properties b e '(invisible nil)))))
      (when (and (listp buffer-invisibility-spec)
                 (member 'carriage-doc-state buffer-invisibility-spec))
        (remove-from-invisibility-spec 'carriage-doc-state))
      t)))

;;;###autoload
(defun carriage-doc-state-toggle-visibility ()
  "Toggle visibility of document-level Carriage state.

- When hidden, reveal:
  • file-level lines \"#+PROPERTY: CARRIAGE_*\"
  • the #+begin_carriage block (if present)
- When visible, hide both using a buffer-local invisibility spec.

Best-effort: this command does not touch the legacy \"Carriage State\" property drawer."
  (interactive)
  (let* ((hidden (and (listp buffer-invisibility-spec)
                      (member 'carriage-doc-state buffer-invisibility-spec))))
    (if hidden
        (progn
          (carriage-doc-state--show-file-properties)
          (ignore-errors (carriage-doc-state--reveal-carriage-block (current-buffer)))
          (when (and (listp buffer-invisibility-spec)
                     (member 'carriage-doc-state buffer-invisibility-spec))
            (remove-from-invisibility-spec 'carriage-doc-state))
          (message "Carriage: показаны свойства и блок begin_carriage"))
      (progn
        (carriage-doc-state--hide-file-properties)
        (ignore-errors (carriage-doc-state--ensure-carriage-block-hidden (current-buffer)))
        (message "Carriage: скрыты свойства и блок begin_carriage"))))
  t)

(provide 'carriage-doc-state)
;; -------------------------------------------------------------------
;; v1.2 — Prefer a dedicated, foldable block at the top of the document:
;;   #+begin_carriage
;;   CARRIAGE_MODE t
;;   CARRIAGE_INTENT Code
;;   ...
;;   #+end_carriage
;;
;; Behavior:
;; - Read priority: begin_carriage block → file-level #+PROPERTY: CARRIAGE_* → drawer heading.
;; - Write: upsert/replace begin_carriage block body with current keys.
;; - Hide: fold the begin_carriage block on demand.

(defun carriage-doc-state--carriage-block-range ()
  "Return cons (BODY-BEG . BODY-END) for #+begin_carriage…#+end_carriage block, or nil.
BODY-BEG points to the first char after the begin line newline.
BODY-END points to the beginning of the #+end_carriage line."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search t)
              (beg nil) (end nil))
          (when (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t)
            (setq beg (line-end-position))
            (when (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)
              (setq end (line-beginning-position))))
          (when (and beg end (> end beg))
            (cons (1+ beg) end)))))))

(defun carriage-doc-state--parse-carriage-block (beg end)
  "Parse key/value lines between BEG and END of a begin_carriage block.
Accepts lines in the forms:
  KEY: VALUE
  KEY VALUE
Ignores empty lines and lines starting with ; or #.
Returns an alist of (KEY . VAL) where KEY is the drawer-style key (e.g., \"CAR_MODE\")."
  (let ((alist '()))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((ln (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 (s (string-trim ln)))
            (unless (or (string-empty-p s)
                        (string-prefix-p ";" s)
                        (string-prefix-p "#" s))
              ;; Accept "KEY: VALUE" or "KEY VALUE" (first token is key, rest is value)
              (let* ((parts (split-string s "[: \t]" t))
                     (kraw (car parts))
                     (rest (string-trim (substring s (length kraw))))
                     (val (string-trim (replace-regexp-in-string "\\`[: \t]+" "" rest))))
                (when (and (stringp kraw) (not (string-empty-p kraw)))
                  ;; Map file-level CARRIAGE_* → drawer CAR_*
                  (let ((k (carriage-doc-state--file-key->drawer kraw)))
                    (push (cons k val) alist))))))
          (forward-line 1))))
    (nreverse alist)))

(defun carriage-doc-state--write-carriage-block (alist)
  "Upsert the begin_carriage block with ALIST of (KEY . VAL) pairs.
Keys may be drawer-style (\"CAR_*\") or file-style (\"CARRIAGE_*\"); they are
normalized to file-style CARRIAGE_* inside the block."
  (let* ((norm
          (mapcar (lambda (cell)
                    (let* ((k (car cell))
                           (v (cdr cell))
                           (fk (carriage-doc-state--prop-key->file k)))
                      (cons fk (format "%s" v))))
                  alist))
         ;; Keep a stable order by sorting keys
         (sorted (sort norm (lambda (a b) (string< (car a) (car b)))))
         (body (mapconcat (lambda (cell)
                            (format "%s %s" (car cell) (cdr cell)))
                          sorted "\n"))
         (block (concat "#+begin_carriage\n" body "\n#+end_carriage\n")))
    (save-excursion
      (save-restriction
        (widen)
        (let ((range (carriage-doc-state--carriage-block-range)))
          (cond
           (range
            (let ((beg (car range))
                  (end (cdr range))
                  (inhibit-read-only t))
              ;; Replace body only
              (delete-region beg end)
              (goto-char beg)
              (insert body "\n")
              t))
           (t
            ;; Insert near the very top: after initial #+ lines, before content
            (goto-char (point-min))
            (while (looking-at-p "^[ \t]*#\\+")
              (forward-line 1))
            (let ((inhibit-read-only t))
              (unless (bolp) (insert "\n"))
              (insert block))
            t)))))))

(defun carriage-doc-state--hide-carriage-block ()
  "Hide the #+begin_carriage block using an overlay (idempotent)."
  (carriage-doc-state--ensure-carriage-block-hidden (current-buffer)))

;; Override readers/writers to prioritize the carriage block
;;
(defun carriage-doc-state--read-preferring-file (&optional buffer)
  "Read state with priority:
1) #+begin_carriage block,
2) file-level #+PROPERTY: CARRIAGE_*,
3) drawer heading \"Carriage State\"."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((range (carriage-doc-state--carriage-block-range)))
      (if range
          (let* ((alist (carriage-doc-state--parse-carriage-block (car range) (cdr range))))
            (carriage-doc-state--alist->plist alist))
        ;; Fallbacks: file-level properties → drawer
        (let* ((props (carriage-doc-state--read-properties-lines)))
          (if (and props (> (length props) 0))
              (let* ((mapped (mapcar (lambda (cell)
                                       (cons (carriage-doc-state--file-key->drawer (car cell))
                                             (cdr cell)))
                                     props)))
                (carriage-doc-state--alist->plist mapped))
            (carriage-doc-state--alist->plist (carriage-doc-state--read-drawer))))))))

(defun carriage-doc-state--write-to-file-properties (data &optional buffer)
  "Write DATA to the #+begin_carriage block (upsert/replace).
DATA may be a plist (:CAR_* …) or an alist of (\"CAR_*\" . VAL)."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((alist
            (cond
             ;; plist → alist
             ((and (listp data) (keywordp (car data)))
              (let ((acc '()) (pl data))
                (while pl
                  (let ((k (car pl)) (v (cadr pl)))
                    (setq acc (append acc (list (cons (upcase (string-remove-prefix ":" (format "%s" k))) v)))))
                  (setq pl (cddr pl)))
                acc))
             ;; alist
             ((and (listp data) (consp (car data))) data)
             (t (user-error "Unsupported DATA format for carriage-doc-state-write")))))
      (carriage-log "doc-state: write-carriage-block keys=%d buf=%s"
                    (length alist) (buffer-name))
      (let ((ok (carriage-doc-state--write-carriage-block alist)))
        (carriage-log "doc-state: write-carriage-block result=%s block=%s"
                      (if ok "t" "nil")
                      (if (carriage-doc-state--carriage-block-range) "present" "absent"))
        ok))))

;; Also fold the new block on hide
(ignore-errors
  (advice-add 'carriage-doc-state-hide :after (lambda (&rest _)
                                                (ignore-errors (carriage-doc-state--hide-carriage-block)))))

;; Auto-fold the #+begin_carriage block on visit and after save (non-intrusive).
(defun carriage-doc-state--fold-carriage-block-now (&optional buffer)
  "Ensure begin_<kind> blocks are folded in BUFFER (or current) using overlays.
This delegates to the reusable carriage-block-fold module."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        ;; Immediate ensure for all enabled kinds
        (dolist (cell carriage-doc-state-fold-kinds)
          (when (cdr cell)
            (ignore-errors (carriage-block-fold-ensure-overlay (car cell)))))
        ;; Reinforce after other hooks run: re-ensure, (re)install watchers, and refresh overlays
        (run-at-time
         0 nil
         (lambda (b)
           (when (buffer-live-p b)
             (with-current-buffer b
               (when (derived-mode-p 'org-mode)
                 (dolist (cell carriage-doc-state-fold-kinds)
                   (when (cdr cell)
                     (ignore-errors (carriage-block-fold-ensure-overlay (car cell)))))
                 (ignore-errors (carriage-block-fold-install-cursor-watch b))
                 (ignore-errors (carriage-block-fold-install-change-watch b))
                 (ignore-errors (carriage-block-fold-schedule-overlay-refresh 0.05 b))))))
         buf)
        t))))


(defun carriage-doc-state--fold-on-visit ()
  "Fold the #+begin_carriage block when visiting an Org document and on saves."
  (let ((buf (current-buffer)))
    (run-at-time
     0 nil
     (lambda ()
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'org-mode)
             (carriage-doc-state--fold-carriage-block-now)
             (add-hook 'after-save-hook #'carriage-doc-state--fold-carriage-block-now nil t))))))))

;; Install visit hooks: fold at visit, after major mode activation, and on revert; ensure after-save handled per-buffer.
(defun carriage-doc-state--maybe-install-save-hook ()
  "Install doc-state before-save hook when carriage-mode is active in this buffer."
  (when (and (derived-mode-p 'org-mode)
             (boundp 'carriage-mode) carriage-mode)
    (ignore-errors (carriage-doc-state-install-save-hook))))

(add-hook 'find-file-hook #'carriage-doc-state--fold-on-visit)
(add-hook 'find-file-hook #'carriage-doc-state--maybe-auto-enable)
(add-hook 'after-change-major-mode-hook #'carriage-doc-state--fold-on-visit)
(add-hook 'org-mode-hook #'carriage-doc-state--fold-carriage-block-now)
(add-hook 'after-revert-hook #'carriage-doc-state--fold-carriage-block-now)

;; -------------------------------------------------------------------
;; Overlay-based folding for begin_<kind> blocks (generic, enabled for :carriage)
;; NOTE: This supersedes org-fold for these blocks; do not use org-fold toggles here.
;; The blocks are hidden via overlays and will not auto-unfold due to isearch/fragile logic.
;; They are revealed automatically when the cursor enters the block, and hidden again on leave.
;;
;; Behavior:
;; - Hidden by an overlay with a muted one-line placeholder (before-string).
;; - When point enters the overlay range, the block is revealed (invisible=nil).
;; - When point leaves, the overlay hides the block again and restores the placeholder.
;; - No org-fold is used; overlays are idempotently ensured and rechecked after saves.

(defface carriage-doc-state-block-summary-face
  '((t :inherit shadow :slant italic :height 0.9))
  "Face for the single-line placeholder of folded begin_<kind> blocks.")


(defun carriage-doc-state--block-range-of (kind)
  "Return (BEG . END) inclusive line bounds of the begin_<KIND>…end_<KIND> block, or nil."
  (let* ((name (symbol-name kind)))
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let* ((case-fold-search t)
                 (rb (and (re-search-forward (format "^[ \t]*#\\+begin_%s\\b" (regexp-quote name)) nil t)
                          (line-beginning-position)))
                 (re (and rb (progn
                               (when (re-search-forward (format "^[ \t]*#\\+end_%s\\b" (regexp-quote name)) nil t)
                                 (line-end-position))))))
            (when (and rb re (> re rb)) (cons rb re))))))))

(defun carriage-doc-state--block-summary-string (kind beg end)
  "Build a one-line placeholder for folded block KIND covering BEG..END."
  (let* ((arrow (if (display-graphic-p) "▸" ">"))
         (nm (format "begin_%s" (symbol-name kind)))
         (lines (max 1 (count-lines beg end)))
         (txt (format "%s %s (%d lines) — mouse-1: toggle" arrow nm lines))
         (s (propertize txt 'face 'carriage-doc-state-block-summary-face)))
    ;; Make the placeholder clickable to reveal/hide this KIND.
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1]
                  (lambda () (interactive)
                    (let ((ov (alist-get kind carriage-doc-state--block-overlays)))
                      (if (and (overlayp ov) (overlay-get ov 'invisible))
                          (carriage-doc-state--reveal-block kind)
                        (carriage-doc-state--hide-block kind)))))
      (add-text-properties 0 (length s)
                           (list 'local-map map
                                 'mouse-face 'mode-line-highlight
                                 'help-echo (format "Toggle %s block visibility" nm))
                           s))
    s))

(defun carriage-doc-state--ensure-block-overlay (kind)
  "Idempotently ensure an overlay hiding begin_<KIND>…end_<KIND> with a summary line.
Returns the overlay or nil if block is absent."
  (let* ((rg (carriage-doc-state--block-range-of kind)))
    (when rg
      (let* ((beg (car rg)) (end (cdr rg))
             (ov (alist-get kind carriage-doc-state--block-overlays)))
        (cond
         ;; Reuse overlay when it already covers the correct range.
         ((and (overlayp ov)
               (= (overlay-start ov) beg)
               (= (overlay-end   ov) end))
          (unless (overlay-get ov 'carriage-block-revealed)
            (overlay-put ov 'before-string (carriage-doc-state--block-summary-string kind beg end))
            (overlay-put ov 'invisible 'carriage-doc-state))
          (unless (member 'carriage-doc-state buffer-invisibility-spec)
            (add-to-invisibility-spec 'carriage-doc-state))
          ov)
         (t
          (when (overlayp ov) (delete-overlay ov))
          (let ((new (make-overlay beg end)))
            (overlay-put new 'evaporate t)
            (overlay-put new 'category 'carriage-block-fold)
            (overlay-put new 'carriage-block-kind kind)
            (overlay-put new 'carriage-block-revealed nil)
            (overlay-put new 'before-string (carriage-doc-state--block-summary-string kind beg end))
            (overlay-put new 'invisible 'carriage-doc-state)
            (add-to-invisibility-spec 'carriage-doc-state)
            (setf (alist-get kind carriage-doc-state--block-overlays) new)
            ;; Back-compat single var for 'carriage kind.
            (when (eq kind 'carriage)
              (setq carriage-doc-state--carriage-block-overlay new))
            new)))))))

(defun carriage-doc-state--reveal-block (kind)
  "Reveal begin_<KIND> block if folded; keeps overlay for later rehiding."
  (let ((ov (or (alist-get kind carriage-doc-state--block-overlays) nil)))
    (when (overlayp ov)
      (overlay-put ov 'before-string nil)
      (overlay-put ov 'invisible nil)
      (overlay-put ov 'carriage-block-revealed t))
    ov))

(defun carriage-doc-state--hide-block (kind)
  "Hide begin_<KIND> block with a placeholder; creates overlay if missing."
  (let* ((ov (or (alist-get kind carriage-doc-state--block-overlays)
                 (carriage-doc-state--ensure-block-overlay kind))))
    (when (overlayp ov)
      (let ((beg (overlay-start ov))
            (end (overlay-end ov)))
        (overlay-put ov 'before-string (carriage-doc-state--block-summary-string kind beg end))
        (overlay-put ov 'invisible 'carriage-doc-state)
        (overlay-put ov 'carriage-block-revealed nil)
        (unless (member 'carriage-doc-state buffer-invisibility-spec)
          (add-to-invisibility-spec 'carriage-doc-state))))
    ov))

(defun carriage-doc-state--cursor-ensure-visibility ()
  "Reveal folded blocks when point enters them; hide back when point leaves."
  (when (and (derived-mode-p 'org-mode)
             (listp carriage-doc-state-fold-kinds))
    (dolist (cell carriage-doc-state-fold-kinds)
      (let* ((kind (car cell))
             (enabled (cdr cell)))
        (when enabled
          (let ((rg (carriage-doc-state--block-range-of kind)))
            (when rg
              (let* ((pos (point))
                     (beg (car rg)) (end (cdr rg)))
                (if (and (>= pos beg) (<= pos end))
                    (carriage-doc-state--reveal-block kind)
                  (carriage-doc-state--hide-block kind))))))))))

(defun carriage-doc-state--install-cursor-watch ()
  "Install buffer-local watcher to auto-reveal folded blocks on cursor enter."
  (add-hook 'post-command-hook #'carriage-doc-state--cursor-ensure-visibility nil t))

;; -----------------------------------------------------------------------------
;; Overlay refresh on edits (keep folded overlays accurate when user edits)
;;
(defvar-local carriage-doc-state--overlay-refresh-timer nil
  "Idle timer used to coalesce overlay refresh after buffer edits.")

(defun carriage-doc-state--refresh-overlays (&optional kinds)
  "Rescan and (re)ensure overlays for enabled KINDS (or all from defcustom).
Deletes stale overlays when corresponding blocks disappear."
  (when (derived-mode-p 'org-mode)
    (let* ((kinds (or kinds (mapcar #'car carriage-doc-state-fold-kinds))))
      (dolist (k kinds)
        (let* ((enabled (alist-get k carriage-doc-state-fold-kinds))
               (rg (and enabled (carriage-doc-state--block-range-of k)))
               (ov (alist-get k carriage-doc-state--block-overlays)))
          (cond
           ;; Ensure overlay for present block
           ((and enabled rg)
            (ignore-errors (carriage-doc-state--ensure-block-overlay k)))
           ;; Remove stale overlay if block missing or kind disabled
           (ov
            (when (overlayp ov) (delete-overlay ov))
            (setf (alist-get k carriage-doc-state--block-overlays) nil))))))
    t))

(defun carriage-doc-state--schedule-overlay-refresh (&optional delay)
  "Schedule a near-future overlays refresh (debounced).
Optional DELAY in seconds; defaults to 0.1."
  (when (timerp carriage-doc-state--overlay-refresh-timer)
    (cancel-timer carriage-doc-state--overlay-refresh-timer))
  (let* ((d (or delay 0.1))
         (buf (current-buffer)))
    (setq carriage-doc-state--overlay-refresh-timer
          (run-at-time d nil
                       (lambda (b)
                         (when (buffer-live-p b)
                           (with-current-buffer b
                             (ignore-errors (carriage-doc-state--refresh-overlays))
                             (setq carriage-doc-state--overlay-refresh-timer nil))))
                       buf))
    buf)
  t)

(defun carriage-doc-state--after-change (_beg _end _len)
  "After-change hook to keep block overlays in sync with buffer edits."
  (when (derived-mode-p 'org-mode)
    (carriage-doc-state--schedule-overlay-refresh 0.1)))

(defun carriage-doc-state--install-change-watch ()
  "Install buffer-local after-change watcher to refresh folded overlays."
  (add-hook 'after-change-functions #'carriage-doc-state--after-change nil t))

(defun carriage-doc-state--remove-change-watch ()
  "Remove buffer-local after-change watcher."
  (remove-hook 'after-change-functions #'carriage-doc-state--after-change t))

;; Install/uninstall before-save hook when carriage-mode toggles in this buffer.
(defun carriage-doc-state--on-carriage-mode (&rest _args)
  "Attach or detach before-save hook when `carriage-mode' toggles."
  (when (and (derived-mode-p 'org-mode)
             (boundp 'carriage-mode))
    (if carriage-mode
        (ignore-errors (carriage-doc-state-install-save-hook))
      (ignore-errors (carriage-doc-state-remove-save-hook)))))

(with-eval-after-load 'carriage-mode
  (ignore-errors
    (advice-add 'carriage-mode :after #'carriage-doc-state--on-carriage-mode)))

;; Canonicalize public API to v1.2 block-first reader/writer (remove legacy paths)
(defalias 'carriage-doc-state-read  'carriage-doc-state--read-preferring-file)
(defalias 'carriage-doc-state-write 'carriage-doc-state--write-to-file-properties)

;;; carriage-doc-state.el ends here
