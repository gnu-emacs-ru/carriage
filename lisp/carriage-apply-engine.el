;;; carriage-apply-engine.el --- Apply engine registry  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: engines
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/apply-engines-v2.org
;;   spec/apply-pipeline-v2.org
;;   spec/tool-contracts-v2.org
;;
;;; Commentary:
;; Engine registry and dispatcher for dry-run/apply steps.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)

(defgroup carriage-engines nil
  "Apply engines for Carriage (registry and selection)."
  :group 'carriage)

(defcustom carriage-apply-engine 'emacs
  "Active apply engine symbol. Default is 'emacs."
  :type '(choice symbol)
  :group 'carriage-engines)

(defcustom carriage-apply-engine-extra-args nil
  "Extra arguments for apply engines (per-kind list).
For 'git engine, this is a plist/alist with optional keys:
  :apply (list of strings)  ; appended to git apply invocation
  :check (list of strings)  ; appended to git apply --check
Example: '(:apply (\"--reject\" \"--whitespace=nowarn\") :check (\"--verbose\"))."
  :type '(repeat sexp)
  :group 'carriage-engines)

(defcustom carriage-apply-timeout-seconds nil
  "Per-step timeout in seconds for apply engines.
When non-nil, takes precedence over engine-specific timeouts."
  :type '(choice (const :tag "Engine default" nil) integer)
  :group 'carriage-engines)

(defvar carriage-apply-engine--registry nil
  "Alist registry of apply engines.
Shape: ((SYM . (:name STRING :dry-run FN :apply FN :capabilities FN?)) ...).")

(defun carriage-apply-engine--norm (sym)
  "Normalize engine SYM to a symbol."
  (cond
   ((symbolp sym) sym)
   ((stringp sym) (intern sym))
   (t (intern (format "%s" sym)))))

(defun carriage-register-apply-engine (sym name &rest kvs)
  "Register apply engine SYM with human NAME and callbacks in KVS.

KVS keys:
  :dry-run (op item repo on-done on-fail)
  :apply   (op item repo on-done on-fail)
  :capabilities (optional FN taking OP, returns plist)

Returns an unregister lambda."
  (let* ((s (carriage-apply-engine--norm sym))
         (entry (list :name (or name (symbol-name s))
                      :dry-run (plist-get kvs :dry-run)
                      :apply   (plist-get kvs :apply)
                      :capabilities (plist-get kvs :capabilities)))
         (cell (assoc s carriage-apply-engine--registry)))
    (if cell
        (setcdr cell entry)
      (push (cons s entry) carriage-apply-engine--registry))
    (carriage-log "Engine registered: %s (%s)" s (plist-get entry :name))
    (lambda ()
      (setq carriage-apply-engine--registry
            (assq-delete-all s carriage-apply-engine--registry))
      (carriage-log "Engine unregistered: %s" s))))

(defun carriage-apply-engine ()
  "Return currently selected engine symbol (fallback to first registered)."
  (let* ((cur (carriage-apply-engine--norm carriage-apply-engine)))
    (cond
     ((assoc cur carriage-apply-engine--registry) cur)
     ((carriage-apply-engine--first) (carriage-apply-engine--first))
     (t cur))))

(defun carriage-apply-engine--first ()
  "Return first registered engine symbol or nil."
  (caar carriage-apply-engine--registry))

(defun carriage-apply-engine--get (sym)
  "Return registry entry plist for engine SYM."
  (cdr (assoc (carriage-apply-engine--norm sym) carriage-apply-engine--registry)))

(defun carriage-available-apply-engines ()
  "Return list of available engines as strings for completion.
For 'git engine also provide policy-specific combos:
  git:in-place — <name> (in-place)
  git:wip      — <name> (wip)
  git:ephemeral— <name> (ephemeral)"
  (let* (;; Hide bare git entry; git policies are listed via combos below.
         (registry (cl-remove-if (lambda (cell) (eq (car cell) 'git))
                                 (reverse carriage-apply-engine--registry)))
         (base (mapcar (lambda (cell)
                         (let* ((s (car cell))
                                (pl (cdr cell))
                                (nm (or (plist-get pl :name) (symbol-name s))))
                           (format "%s%s" (symbol-name s)
                                   (if (and nm (not (string= nm (symbol-name s))))
                                       (format " — %s" nm) ""))))
                       registry))
         (git-cell (assoc 'git carriage-apply-engine--registry))
         (git-name (and git-cell (or (plist-get (cdr git-cell) :name) "git")))
         (combos (when git-cell
                   (list (format "git:in-place — %s (in-place)" git-name)
                         (format "git:wip — %s (wip)" git-name)
                         (format "git:ephemeral — %s (ephemeral)" git-name)))))
    (append combos base)))

(defun carriage-apply-engine--parse-choice (choice)
  "Extract engine symbol from CHOICE string produced by carriage-available-apply-engines."
  (when (and (stringp choice)
             (string-match "\\=\\([^  —]+\\)" choice))
    (intern (match-string 1 choice))))

;;;###autoload
(defun carriage-select-apply-engine (&optional engine)
  "Interactively select active apply ENGINE from the registry.
Supports git policy combos: git:in-place|git:wip|git:ephemeral."
  (interactive)
  (let* ((choices (carriage-available-apply-engines))
         (cur-sym (carriage-apply-engine))
         ;; Default prompt shows git:<policy> when engine is 'git to match combos.
         (current (if (and (eq cur-sym 'git) (boundp 'carriage-git-branch-policy))
                      (format "git:%s" carriage-git-branch-policy)
                    (symbol-name cur-sym)))
         (sel-s (cond
                 ((and engine (symbolp engine)) (symbol-name engine))
                 ((and engine (stringp engine)) engine)
                 ((and choices (completing-read
                                (format "Apply engine [%s]: " current)
                                choices nil t nil nil current)))
                 (t current))))
    ;; Handle git:POLICY combos
    (cond
     ((and (stringp sel-s)
           (string-match "\\`git:\\([^  —]+\\)" sel-s))
      (let* ((pol (intern (match-string 1 sel-s))))
        (setq carriage-apply-engine 'git)
        (when (boundp 'carriage-git-branch-policy)
          (setq carriage-git-branch-policy pol))
        (carriage-log "Apply engine selected: git (policy=%s)" pol)
        (message "Apply engine: git (policy=%s)" pol)
        (force-mode-line-update t)
        'git))
     (t
      (let* ((sym (or (and sel-s (carriage-apply-engine--parse-choice sel-s))
                      (carriage-apply-engine--norm sel-s))))
        (setq carriage-apply-engine sym)
        (carriage-log "Apply engine selected: %s" sym)
        (message "Apply engine: %s" sym)
        (force-mode-line-update t)
        sym)))))

(defun carriage-apply-engine-dispatch (kind op plan-item repo-root on-done on-fail)
  "Dispatch KIND ('dry-run|'apply) for OP via active engine with callbacks.
Callbacks are invoked on the main thread via run-at-time 0. Returns the engine token
(if the engine callback returns one), or nil otherwise."
  (let* ((eng (carriage-apply-engine))
         (rec (carriage-apply-engine--get eng))
         (cb  (pcase kind
                ((or 'dry-run :dry-run) (plist-get rec :dry-run))
                ((or 'apply   :apply)   (plist-get rec :apply))
                (_ nil))))
    ;; Suite↔Engine guard (spec v1.1):
    ;; - По умолчанию patch требует git-движок.
    ;; - Но если активный движок явно заявляет поддержку patch (capabilities), разрешаем.
    (let* ((capf (plist-get rec :capabilities))
           (caps (and (functionp capf) (ignore-errors (funcall capf op))))
           (ops  (and (listp caps) (plist-get caps :ops)))
           (supported (and ops (memq op ops))))
      (if (and (eq op 'patch) (not (eq eng 'git)) (not supported))
          (progn
            (carriage-log "Engine dispatch: patch unsupported by %s" eng)
            (if (eq eng 'emacs)
                ;; Friendly skip row for emacs engine
                (let ((row (list :engine 'emacs :op 'patch :status 'skip
                                 :details "patch unsupported by emacs engine")))
                  (when (functionp on-done)
                    (run-at-time 0 nil (lambda () (funcall on-done row)))))
              ;; Other non-git engines → hard dispatch error
              (let ((err (list :code 'MODE_E_DISPATCH
                               :engine eng :op 'patch
                               :details "patch unsupported by selected engine")))
                (when (functionp on-fail)
                  (run-at-time 0 nil (lambda () (funcall on-fail err))))))
            nil)
        (if (functionp cb)
            (condition-case e
                (let ((ret
                       (funcall cb op plan-item repo-root
                                (lambda (result)
                                  (run-at-time 0 nil
                                               (lambda ()
                                                 (when (functionp on-done)
                                                   (funcall on-done result)))))
                                (lambda (err)
                                  (run-at-time 0 nil
                                               (lambda ()
                                                 (when (functionp on-fail)
                                                   (funcall on-fail err))))))))
                  ret)
              (error
               (carriage-log "Engine dispatch error (%s): %s" eng (error-message-string e))
               (when (functionp on-fail)
                 (run-at-time 0 nil (lambda () (funcall on-fail e))))
               nil))
          (progn
            (carriage-log "No %s callback for engine %s (op=%s)" kind eng op)
            (when (functionp on-fail)
              (run-at-time 0 nil
                           (lambda ()
                             (funcall on-fail (list :error 'no-callback :engine eng :kind kind)))))
            nil))))))

(provide 'carriage-apply-engine)
;;; carriage-apply-engine.el ends here
