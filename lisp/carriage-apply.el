;;; carriage-apply.el --- Dry-run and apply implementations  -*- lexical-binding: t; -*-
;;
;; (file body unchanged below)
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5") (seq "1.0"))
;; Version: 0.1
;; Keywords: tools, vcs, async
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/apply-pipeline-v2.org
;;   spec/apply-engines-v2.org
;;   spec/git-integration-v2.org
;;   spec/async-workflow-v2.org
;;   spec/data-structures-v2.org
;;   spec/observability-v2.org
;;   spec/tool-contracts-v2.org
;;
;;; Commentary:
;; Core apply pipeline: dry-run and apply implementations, sync and async FSM,
;; preflight and epilog handling for branch policies, and engine dispatch.
;;
;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-format-registry)
;; Ensure 'engines' subdirectory is on load-path when loading carriage-mode directly
;; with :load-path pointing to the lisp/ directory, so that (require 'carriage-apply-engine)
;; and engine modules resolve without extra user configuration.
(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (engines-dir (and this-dir (expand-file-name "engines" this-dir))))
  (when (and engines-dir (file-directory-p engines-dir))
    (add-to-list 'load-path engines-dir)))
(require 'carriage-apply-engine)
;; Also load the 'emacs engine to expose it in the registry (patch unsupported in v1).
(ignore-errors (require 'carriage-engine-emacs))
;; Load default Git apply engine so it registers itself in the engine registry.
(require 'carriage-engine-git)

;; Register abort handler provided by async apply pipeline (declared in carriage-mode).
(declare-function carriage-register-abort-handler "carriage-mode" (fn))


(defcustom carriage-allow-apply-on-wip nil
  "When non-nil, allow applying patches while on WIP/ephemeral branches.
By default (nil) Carriage will refuse to apply on branches named like
=carriage-mode-wip-branch' or starting with =carriage-git-ephemeral-prefix'."
  :type 'boolean :group 'carriage)

(defcustom carriage-apply-stage-policy 'none
  "Policy for staging changes during apply:
- 'none  — modify working tree only (default), do not stage.
- 'index — stage changes into index (e.g., git apply --index, git add)."
  :type '(choice (const none) (const index))
  :group 'carriage)

(defcustom carriage-apply-async t
  "When non-nil, run apply-plan asynchronously in a Lisp thread to avoid UI blocking.
If threads are unavailable or in batch mode, falls back to synchronous execution."
  :type 'boolean :group 'carriage)

(defcustom carriage-apply-timeout-seconds 5
  "Max seconds to wait in synchronous patch apply/dry-run loops before yielding back.
Interactive sessions SHOULD prefer the async path; this setting caps residual sync waits."
  :type 'integer :group 'carriage)

(defcustom carriage-apply-applied-block-policy 'annotate
  "Policy for post-apply handling of successfully applied #+begin_patch blocks:
- 'annotate — do not remove the block; annotate its header with :applied t and metadata.
- 'none     — do nothing (no document edits).

Note: Regardless of policy, pipeline reports remain unchanged (side-effect-only)."
  :type '(choice (const annotate) (const none))
  :group 'carriage)

(defcustom carriage-apply-strip-body-on-annotate nil
  "When non-nil and policy='annotate, clear the body of applied #+begin_patch blocks,
leaving an empty block with an annotated header."
  :type 'boolean
  :group 'carriage)

(defun carriage--report-ok (op &rest kv)
  "Build ok report alist with OP and extra KV plist."
  (append (list :op op :status 'ok) kv))

(defun carriage--report-fail (op &rest kv)
  "Build fail report alist with OP and extra KV plist."
  (append (list :op op :status 'fail) kv))

(defun carriage--plan-get (item key)
  "Get KEY from ITEM supporting both plist and alist representations."
  (if (plist-member item key) (plist-get item key) (alist-get key item)))

(defvar carriage-mode-sre-preview-max 3
  "Default maximum number of SRE preview chunks when Customize is not loaded.")

;;; Plan-level pipeline

(defun carriage--post-apply-handle-applied-blocks (report)
  "Dispatch post-apply block handling per `carriage-apply-applied-block-policy'."
  (pcase carriage-apply-applied-block-policy
    ('annotate (ignore-errors (carriage--annotate-applied-blocks-in-report report)))
    (_ nil)))

(defun carriage--annotate-applied-blocks-in-report (report)
  "Best-effort: annotate source begin_patch blocks from REPORT with :applied t and metadata.
When `carriage-apply-strip-body-on-annotate' is non-nil, clear the body between begin/end.
Side-effect-only; never alters REPORT (REQ-apply-010)."
  (let ((items (plist-get report :items))
        (ts (format-time-string "%Y-%m-%d %H:%M")))
    (dolist (row items)
      (when (eq (plist-get row :status) 'ok)
        (let* ((plan (plist-get row :_plan))
               (buf (and plan (carriage--plan-get plan :_buffer)))
               (mb  (and plan (carriage--plan-get plan :_beg-marker)))
               (me  (and plan (carriage--plan-get plan :_end-marker))))
          (when (and (bufferp buf) (markerp mb) (markerp me)
                     (eq (marker-buffer mb) buf)
                     (eq (marker-buffer me) buf))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (save-excursion
                  (goto-char (marker-position mb))
                  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                              (line-end-position))))
                    (when (string-match "\\`[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*\\'" line)
                      (let* ((sexp-str (match-string 1 line))
                             (plist    (condition-case _e
                                           (car (read-from-string sexp-str))
                                         (error nil)))
                             (plist    (if (listp plist) plist '()))
                             (plist    (plist-put plist :applied t))
                             (plist    (plist-put plist :applied_at ts))
                             (plist    (let ((eng (plist-get row :engine)))
                                         (if eng (plist-put plist :engine eng) plist)))
                             (plist    (let ((res (plist-get row :details)))
                                         (if (and (stringp res) (not (string-empty-p res)))
                                             (plist-put plist :result res) plist)))
                             (plist    (let ((m (plist-get row :matches)))
                                         (if (numberp m) (plist-put plist :matches m) plist)))
                             (plist    (let ((b (plist-get row :changed-bytes)))
                                         (if (numberp b) (plist-put plist :bytes b) plist)))
                             (new-hdr  (concat "#+begin_patch " (prin1-to-string plist))))
                        ;; Rewrite header line
                        (delete-region (line-beginning-position) (line-end-position))
                        (insert new-hdr)
                        ;; Optionally clear body between begin/end (keep markers stable)
                        (when carriage-apply-strip-body-on-annotate
                          (let ((beg (save-excursion (forward-line 1) (point)))
                                (end (save-excursion
                                       (goto-char (marker-position me))
                                       (line-beginning-position))))
                            (when (< beg end)
                              (delete-region beg end))))
                        ;; Immediately enable+refresh patch-fold overlays (hide applied content)
                        (when (and (boundp 'carriage-mode-hide-applied-patches)
                                   carriage-mode-hide-applied-patches
                                   (require 'carriage-patch-fold nil t))
                          (ignore-errors (carriage-patch-fold-enable (current-buffer)))
                          (ignore-errors (carriage-patch-fold-refresh)))))))
                ;; If point is inside the applied block, move it to the next line after it.
                (let ((pt (point)))
                  (when (and (numberp pt)
                             (>= pt (marker-position mb))
                             (<= pt (marker-position me)))
                    (goto-char (marker-position me))
                    (forward-line 1)
                    (beginning-of-line))))))))))
  t)


(defun carriage--op-rank (op)
  "Return rank for OP to sort plan: delete→rename→create→patch→sre."
  (pcase op
    ('delete 1)
    ('rename 2)
    ('create 3)
    ('patch  4)
    (_       5)))

(defun carriage--plan-sort (plan)
  "Return PLAN items sorted by operation type according to v1 order."
  (seq-sort (lambda (a b)
              (< (carriage--op-rank (carriage--plan-get a :op))
                 (carriage--op-rank (carriage--plan-get b :op))))
            plan))

(defun carriage--dry-run-dispatch (item repo-root)
  "Dispatch dry-run for a single ITEM with REPO-ROOT.
For :op 'patch → use apply engine (:dry-run) with a short sync wait loop to gather pid/elapsed;
for other ops → delegate to format registry."
  (let* ((op (carriage--plan-get item :op)))
    (if (eq op 'patch)
        (let* ((done nil)
               (result nil)
               (t0 (float-time))
               ;; Patch operations must use the 'git engine regardless of current selection (v1 spec).
               (token (let ((carriage-apply-engine 'git))
                        (carriage-apply-engine-dispatch
                         :dry-run 'patch item repo-root
                         (lambda (res) (setq result res done t))
                         (lambda (res) (setq result res done t)))))
               (proc (and (listp token) (plist-get token :process)))
               (deadline (+ (float-time)
                            (or (and (boundp 'carriage-apply-timeout-seconds)
                                     carriage-apply-timeout-seconds)
                                15))))
          (while (and (not done) (< (float-time) deadline))
            (if (and proc (process-live-p proc))
                (accept-process-output proc 0.05)
              (accept-process-output nil 0.05))
            ;; Yield briefly to the command loop to keep UI responsive.
            (sit-for 0))
          (let* ((row (carriage--engine-row 'patch result t0
                                            "git apply --check ok"
                                            "git apply --check failed"
                                            :path))
                 (exit (plist-get result :exit))
                 (stderr (string-trim (or (plist-get result :stderr) "")))
                 (stdout (string-trim (or (plist-get result :stdout) "")))
                 (itm-msgs (when (or (not (numberp exit)) (not (zerop exit)))
                             (list (list :code 'PATCH_E_GIT_CHECK
                                         :severity 'error
                                         :file (or (alist-get :path item) "-")
                                         :details (or (and (not (string-empty-p stderr)) stderr)
                                                      (and (not (string-empty-p stdout)) stdout)
                                                      "git apply --check failed"))))))
            (if itm-msgs (append row (list :_messages itm-msgs)) row)))
      (let* ((rec (carriage-format-get op "1"))
             (fn  (and rec (plist-get rec :dry-run))))
        (if (functionp fn)
            (funcall fn item repo-root)
          (carriage--report-fail (or op 'unknown) :details "Unknown op"))))))

(defun carriage--apply-dispatch (item repo-root)
  "Dispatch apply for a single ITEM with REPO-ROOT.
For :op 'patch → use apply engine (:apply) with a short sync wait loop to gather pid/elapsed;
for other ops → delegate to format registry."
  (let* ((op (carriage--plan-get item :op)))
    (if (eq op 'patch)
        (let* ((done nil)
               (result nil)
               (t0 (float-time))
               ;; Patch operations must use the 'git engine regardless of current selection (v1 spec).
               (token (let ((carriage-apply-engine 'git))
                        (carriage-apply-engine-dispatch
                         :apply 'patch item repo-root
                         (lambda (res) (setq result res done t))
                         (lambda (res) (setq result res done t)))))
               (proc (and (listp token) (plist-get token :process)))
               (deadline (+ (float-time)
                            (or (and (boundp 'carriage-apply-timeout-seconds)
                                     carriage-apply-timeout-seconds)
                                15))))
          (while (and (not done) (< (float-time) deadline))
            (if (and proc (process-live-p proc))
                (accept-process-output proc 0.05)
              (accept-process-output nil 0.05))
            ;; Yield to command loop to avoid UI input stalls while waiting.
            (sit-for 0))
          (carriage--engine-row 'patch result t0
                                "Applied"
                                "git apply failed"
                                :path))
      (let* ((rec (carriage-format-get op "1"))
             (fn  (and rec (plist-get rec :apply))))
        (if (functionp fn)
            (funcall fn item repo-root)
          (carriage--report-fail (or op 'unknown) :details "Unknown op"))))))


(defun carriage--dry-run-item-result (item repo-root virt)
  "Return dry-run ROW for ITEM using virtual FS VIRT when needed.
Handles SRE/AIBO simulation on virtual content; otherwise dispatches normally."
  (let* ((op (carriage--plan-get item :op))
         (file (carriage--plan-get item :file)))
    (cond
     ;; If SRE/AIBO targets a file that will be created in this plan, simulate on that content.
     ((and (memq op '(sre aibo))
           file
           (not (let* ((abs (ignore-errors (carriage-normalize-path repo-root file))))
                  (and abs (file-exists-p abs))))
           (assoc-string file virt t))
      (let ((text (cdr (assoc-string file virt t))))
        (cond
         ((and (eq op 'sre) (fboundp 'carriage-sre-dry-run-on-text))
          (carriage-sre-dry-run-on-text item text))
         ((and (eq op 'aibo)
               (require 'carriage-op-aibo nil t)
               (fboundp 'carriage--aibo->sre-item)
               (fboundp 'carriage-sre-dry-run-on-text))
          (let ((sre-item (carriage--aibo->sre-item item)))
            (carriage-sre-dry-run-on-text sre-item text)))
         (t
          (carriage--report-fail op :file file :details "SRE/AIBO simulation not available")))))
     (t
      (carriage--dry-run-dispatch item repo-root)))))

(defun carriage--virt-apply-create (virt item)
  "Return updated VIRT if ITEM is a create op; otherwise, return VIRT unchanged."
  (let ((op (carriage--plan-get item :op))
        (file (carriage--plan-get item :file)))
    (if (and (eq op 'create) file)
        (let ((content (or (carriage--plan-get item :content) "")))
          (cons (cons file content) (assq-delete-all file virt)))
      virt)))

(defun carriage--dry-run-build-report (plan ok fail skip items msgs)
  "Build final dry-run REPORT plist from components."
  (let* ((eng (carriage-apply-engine))
         (bp  (and (eq eng 'git)
                   (boundp 'carriage-git-branch-policy)
                   carriage-git-branch-policy)))
    (list :phase 'dry-run
          :plan plan
          :engine eng
          :branch-policy bp
          :summary (list :ok ok :fail fail :skipped skip)
          :items (nreverse items)
          :messages (nreverse msgs))))

(defun carriage-dry-run-plan (plan repo-root)
  "Dry-run PLAN (list of plan items) under REPO-ROOT.
Return report alist:
  (:plan PLAN
   :summary (:ok N :fail M :skipped K)
   :items ...
   :messages LIST) where :messages aggregates per-item diagnostics."
  (let* ((sorted (carriage--plan-sort plan))
         (items '())
         (ok 0) (fail 0) (skip 0)
         (virt '())  ; virtual created files: (\"path\" . content)
         (msgs '()))
    (dolist (it sorted)
      (let* ((res0 (carriage--dry-run-item-result it repo-root virt))
             ;; Stash original plan item and repo root into report item for UI actions (e.g., Ediff).
             (res (append res0 (list :_plan it :_root repo-root)))
             (status (plist-get res :status)))
        (push res items)
        ;; Aggregate per-item diagnostics into top-level :messages if present.
        (let ((im (plist-get res :_messages)))
          (when im
            (dolist (d im)
              (push d msgs))))
        (pcase status
          ('ok   (setq ok (1+ ok)))
          ('fail (setq fail (1+ fail)))
          (_     (setq skip (1+ skip)))))
      ;; Update virtual FS for subsequent SRE checks
      (setq virt (carriage--virt-apply-create virt it)))
    (let ((report (carriage--dry-run-build-report plan ok fail skip items msgs)))
      (ignore-errors
        (when (fboundp 'carriage-ui-note-apply-summary)
          (carriage-ui-note-apply-summary
           (list :phase 'dry-run :ok ok :skip skip :fail fail :total (+ ok fail skip)))))
      report)))

(defun carriage-apply-plan (plan repo-root)
  "Apply PLAN (list of plan items) under REPO-ROOT sequentially.
Stops on first failure. Returns report alist as in carriage-dry-run-plan."
  (let* ((sorted (carriage--plan-sort plan))
         (items '())
         (ok 0) (fail 0) (skip 0)
         (stop nil)
         (msgs '()))
    (dolist (it sorted)
      (unless stop
        (let* ((res0 (carriage--apply-dispatch it repo-root))
               ;; Store original plan item and root on the row (parity with dry-run report)
               (res (append res0 (list :_plan it :_root repo-root)))
               (status (plist-get res :status))
               (im (plist-get res :_messages)))
          ;; Aggregate per-item diagnostics into top-level :messages
          (when im
            (dolist (d im)
              (push d msgs)))
          (push res items)
          (pcase status
            ('ok   (setq ok (1+ ok)))
            ('fail (setq fail (1+ fail))
                   (setq stop t))
            (_     (setq skip (1+ skip)))))))
    (let* ((eng (carriage-apply-engine))
           (bp  (and (eq eng 'git)
                     (boundp 'carriage-git-branch-policy)
                     carriage-git-branch-policy))
           (report (list :phase 'apply
                         :plan plan
                         :engine eng
                         :branch-policy bp
                         :branch-name (and (eq eng 'git) (carriage-git-current-branch repo-root))
                         :summary (list :ok ok :fail fail :skipped skip)
                         :items (nreverse items)
                         :messages (nreverse msgs))))
      ;; Update UI state tooltip summary (sync path)
      (ignore-errors
        (when (fboundp 'carriage-ui-note-apply-summary)
          (carriage-ui-note-apply-summary
           (let* ((sum (plist-get report :summary)))
             (list :phase 'apply
                   :ok (or (plist-get sum :ok) 0)
                   :skip (or (plist-get sum :skipped) 0)
                   :fail (or (plist-get sum :fail) 0)
                   :total (+ (or (plist-get sum :ok) 0)
                             (or (plist-get sum :skipped) 0)
                             (or (plist-get sum :fail) 0)))))))
      ;; Announce concise success summary (only when no failures and in interactive session)
      (let* ((sum (plist-get report :summary))
             (fail (and (listp sum) (plist-get sum :fail))))
        (when (and (not (bound-and-true-p noninteractive))
                   (numberp fail) (= fail 0))
          (let* ((its (or (plist-get report :items) '()))
                 (oks (cl-remove-if-not (lambda (it) (eq (plist-get it :status) 'ok)) its))
                 (created 0) (deleted 0) (renamed 0) (modified 0)
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
              (when (> total 0)
                (message "Carriage: applied OK (%d items) — created:%d modified:%d deleted:%d renamed:%d — %s"
                         total created modified deleted renamed files-str)))))
        ;; Annotate successfully applied patch blocks (policy='annotate) or skip (policy='none) (sync path; also on partial success)
        (ignore-errors (carriage--post-apply-handle-applied-blocks report))
        report))))

(defun carriage--make-apply-state (queue repo-root)
  "Create initial async apply STATE plist."
  (list :queue queue
        :root repo-root
        :ok 0 :fail 0 :skipped 0
        :items '()
        :messages '()
        :current nil
        :aborted nil
        :fs-timer nil))

(defun carriage--apply-summary (state)
  "Build summary plist from STATE."
  (list :ok (plist-get state :ok)
        :fail (plist-get state :fail)
        :skipped (plist-get state :skipped)))

(defun carriage--apply-build-report (plan state)
  "Build final apply REPORT plist from PLAN and STATE."
  (let* ((eng (carriage-apply-engine))
         (bp  (and (eq eng 'git)
                   (boundp 'carriage-git-branch-policy)
                   carriage-git-branch-policy))
         (items (nreverse (plist-get state :items)))
         (msgs  (nreverse (plist-get state :messages)))
         (sum   (carriage--apply-summary state))
         (synthetic
          (when (and (null items) (null msgs))
            (list (list :code 'APPLY_EMPTY_REPORT
                        :severity 'warn
                        :details (format "Apply produced no rows; plan-size=%d ok=%d fail=%d skipped=%d"
                                         (length plan)
                                         (plist-get sum :ok)
                                         (plist-get sum :fail)
                                         (plist-get sum :skipped))))))
         (report (list :phase 'apply
                       :plan plan
                       :engine eng
                       :branch-policy bp
                       :branch-name (plist-get state :branch-name)
                       :summary sum
                       :items items
                       :messages (or msgs synthetic))))
    report))

(defun carriage--apply-log-summary (report)
  "Log a concise summary about REPORT to the general log."
  (carriage-log "apply-finish: items=%d ok=%d fail=%d skipped=%d phase=apply"
                (length (or (plist-get report :items) '()))
                (or (plist-get (plist-get report :summary) :ok) 0)
                (or (plist-get (plist-get report :summary) :fail) 0)
                (or (plist-get (plist-get report :summary) :skipped) 0)))

(defun carriage--apply-announce-success (report)
  "Announce concise success summary (interactive only, when there are no failures)."
  (let* ((sum (plist-get report :summary))
         (fail (and (listp sum) (plist-get sum :fail))))
    (when (and (not (bound-and-true-p noninteractive))
               (numberp fail) (= fail 0))
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
              ((or 'patch 'sre 'aibo) (setq modified (1+ modified)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
              (_ (push (or (plist-get it :file) (plist-get it :path) "-") files)))))
        (let* ((total (length oks))
               (files-str (mapconcat #'identity (nreverse (delete-dups (delq nil files))) ", ")))
          (when (> total 0)
            (message "Carriage: applied OK (%d items) — created:%d modified:%d deleted:%d renamed:%d — %s"
                     total created modified deleted renamed files-str)))))))



(defun carriage--apply-run-callback (callback report)
  "Invoke CALLBACK with REPORT on the main thread, if CALLBACK is callable."
  (when (functionp callback)
    (run-at-time 0 nil (lambda () (funcall callback report)))))

(defun carriage--apply-finish (plan state callback)
  "Finish async apply: build REPORT from PLAN and STATE, invoke CALLBACK if any."
  (let* ((report (carriage--apply-build-report plan state)))
    (ignore-errors
      (when (fboundp 'carriage-ui-note-apply-summary)
        (let* ((sum (plist-get report :summary)))
          (carriage-ui-note-apply-summary
           (list :phase 'apply
                 :ok (or (plist-get sum :ok) 0)
                 :skip (or (plist-get sum :skipped) 0)
                 :fail (or (plist-get sum :fail) 0)
                 :total (+ (or (plist-get sum :ok) 0)
                           (or (plist-get sum :skipped) 0)
                           (or (plist-get sum :fail) 0)))))))
    (carriage--apply-log-summary report)
    (carriage--apply-announce-success report)
    ;; Annotate successfully applied patch blocks (policy='annotate) or skip (policy='none)
    (ignore-errors (carriage--post-apply-handle-applied-blocks report))
    (carriage--apply-run-callback callback report)
    report))

(defun carriage--apply-acc-row (state row)
  "Accumulate ROW in STATE, annotating with plan item and repo root for later actions."
  (let ((aug (append row (list :_plan (plist-get state :pending-item)
                               :_root (plist-get state :root)))))
    (plist-put state :items (cons aug (plist-get state :items)))))

(defun carriage--apply-acc-msg (state msg)
  "Accumulate diagnostic MSG in STATE."
  (plist-put state :messages (cons msg (plist-get state :messages))))

(defun carriage--apply-bump (state status)
  "Bump counters in STATE per STATUS."
  (pcase status
    ('ok   (plist-put state :ok (1+ (plist-get state :ok))))
    ('fail (plist-put state :fail (1+ (plist-get state :fail))))
    (_     (plist-put state :skipped (1+ (plist-get state :skipped))))))

(defun carriage--apply-update-abort (state token)
  "Update TOKEN :abort-fn to cancel current async operation based on STATE.
Registers the handler with the mode when available."
  (let* ((cur (plist-get state :current)))
    (setf (plist-get token :abort-fn)
          (lambda ()
            (plist-put state :aborted t)
            ;; Cancel pending FS timer if any
            (let ((tm (plist-get state :fs-timer)))
              (when (timerp tm)
                (ignore-errors (cancel-timer tm))
                (plist-put state :fs-timer nil)))
            ;; Engine-specific abort (e.g., git)
            (cond
             ((and (plist-get cur :engine)
                   (eq (plist-get cur :engine) 'git)
                   (fboundp 'carriage-engine-git-abort))
              (ignore-errors (carriage-engine-git-abort cur))
              t)
             (t t)))))
  (when (fboundp 'carriage-register-abort-handler)
    (carriage-register-abort-handler (plist-get token :abort-fn))))

(defun carriage--engine-row (op res t0 ok-details fail-details &optional path-key)
  "Normalize engine RES into a report row for OP started at T0."
  (let* ((exit (plist-get res :exit))
         (pid  (plist-get res :pid))
         (pth  (or (plist-get res (or path-key :path))
                   (alist-get :file res) "-"))
         (stderr (string-trim (or (plist-get res :stderr) "")))
         (elapsed (truncate (* 1000 (max 0.0 (- (float-time) t0))))))
    (if (and (numberp exit) (zerop exit))
        (list :op op :status 'ok
              (if (eq op 'patch) :path :file) pth
              :details ok-details
              :pid pid :elapsed-ms elapsed :engine 'git)
      (list :op op :status 'fail
            (if (eq op 'patch) :path :file) pth
            :details (if (string-empty-p stderr) fail-details stderr)
            :pid pid :elapsed-ms elapsed :engine 'git))))

(defun carriage--apply-done-patch (state t0 res plan repo-root callback token)
  "Handle completion of a patch step."
  (let ((row (carriage--engine-row 'patch res t0 "Applied" "git apply failed" :path)))
    (carriage--apply-acc-row state row)
    (carriage--apply-bump state (plist-get row :status))
    (if (eq (plist-get row :status) 'ok)
        (carriage--apply-next state plan repo-root callback token)
      (carriage--apply-finish plan state callback))))

(defun carriage--apply-run-engine (state kind op item repo-root on-ok on-fail token)
  "Dispatch KIND/OP ITEM via engine and update STATE/TOKEN.

For :op 'patch always force 'git engine (parity with sync path)."
  (let* ((t0 (float-time))
         ;; Force 'git for patch operations in async path.
         (carriage-apply-engine (if (eq op 'patch) 'git carriage-apply-engine))
         (eng-token (carriage-apply-engine-dispatch
                     kind op item repo-root
                     (lambda (res) (funcall on-ok t0 res))
                     (lambda (res) (funcall on-fail t0 res)))))
    (plist-put state :current eng-token)
    (carriage--apply-update-abort state token)))

(defun carriage--apply-fs-async (state thunk token plan)
  "Run THUNK asynchronously for filesystem ops; on error, mark STATE aborted and finish."
  (carriage-log "fs-async: schedule filesystem operation")
  (let ((tm (run-at-time
             0 nil
             (lambda ()
               (carriage-log "fs-async: run filesystem operation")
               (condition-case e
                   (funcall thunk)
                 (error
                  (carriage--apply-acc-msg
                   state (list :code 'MODE_E_DISPATCH :severity 'error
                               :details (error-message-string e)))
                  (carriage-log "fs-async: error: %s" (error-message-string e))
                  (plist-put state :aborted t)))))))
    (plist-put state :fs-timer tm)
    (carriage--apply-update-abort state token)))

(defun carriage--apply-run-item (state item repo-root plan callback token)
  "Run one ITEM according to its :op, updating STATE and continuing or finishing."
  (let* ((op (carriage--plan-get item :op))
         (target (or (carriage--plan-get item :path) (carriage--plan-get item :file))))
    (plist-put state :pending-item item)
    (carriage-log "apply-run-item: op=%s target=%s engine=%s stage=%s"
                  op (or target "-")
                  (carriage-apply-engine)
                  (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy))
    (pcase op
      ('patch
       ;; Best-effort: ensure parent directory exists for created/renamed paths in udiff
       (let ((p (carriage--plan-get item :path)))
         (when p
           (condition-case _e
               (make-directory (file-name-directory (expand-file-name p repo-root)) t)
             (error nil))))
       (carriage--apply-run-engine
        state :apply 'patch item repo-root
        (lambda (t0 res) (carriage--apply-done-patch state t0 res plan repo-root callback token))
        (lambda (t0 res) (carriage--apply-done-patch state t0 res plan repo-root callback token))
        token))
      ('delete
       (if (and (eq carriage-apply-stage-policy 'index)
                (eq (carriage-apply-engine) 'git))
           (carriage--apply-run-engine
            state :apply 'delete item repo-root
            (lambda (t0 res)
              (let ((row (carriage--engine-row 'delete res t0 "Deleted (staged)" "git rm failed" :path)))
                (carriage--apply-acc-row state row)
                (carriage--apply-bump state (plist-get row :status))
                (if (eq (plist-get row :status) 'ok)
                    (carriage--apply-next state plan repo-root callback token)
                  (carriage--apply-finish plan state callback))))
            (lambda (t0 res)
              (let ((row (carriage--engine-row 'delete res t0 "Deleted (staged)" "git rm failed" :path)))
                (carriage--apply-acc-row state row)
                (carriage--apply-bump state (plist-get row :status))
                (carriage--apply-finish plan state callback)))
            token)
         (carriage--apply-fs-async
          state
          (lambda ()
            (let ((row (carriage-apply-delete item repo-root)))
              (carriage--apply-acc-row state row)
              (carriage--apply-bump state (plist-get row :status))
              (carriage--apply-next state plan repo-root callback token)))
          token plan)))
      ('rename
       (if (and (eq carriage-apply-stage-policy 'index)
                (eq (carriage-apply-engine) 'git))
           (carriage--apply-run-engine
            state :apply 'rename item repo-root
            (lambda (t0 res)
              (let ((row (carriage--engine-row 'rename res t0 "Renamed (staged)" "git mv failed" :path)))
                (carriage--apply-acc-row state row)
                (carriage--apply-bump state (plist-get row :status))
                (if (eq (plist-get row :status) 'ok)
                    (carriage--apply-next state plan repo-root callback token)
                  (carriage--apply-finish plan state callback))))
            (lambda (t0 res)
              (let ((row (carriage--engine-row 'rename res t0 "Renamed (staged)" "git mv failed" :path)))
                (carriage--apply-acc-row state row)
                (carriage--apply-bump state (plist-get row :status))
                (carriage--apply-finish plan state callback)))
            token)
         (carriage--apply-fs-async
          state
          (lambda ()
            (let ((row (carriage-apply-rename item repo-root)))
              (carriage--apply-acc-row state row)
              (carriage--apply-bump state (plist-get row :status))
              (carriage--apply-next state plan repo-root callback token)))
          token plan)))
      ('create
       (carriage--apply-fs-async
        state
        (lambda ()
          (let ((row0 (carriage-apply-create item repo-root)))
            (if (and (eq carriage-apply-stage-policy 'index)
                     (eq (carriage-apply-engine) 'git))
                (let* ((file (carriage--plan-get item :file)))
                  (carriage--apply-run-engine
                   state :apply 'create (list (cons :file file)) repo-root
                   (lambda (t0 res)
                     (let ((r (carriage--engine-row 'create res t0 "Created (staged)" "git add failed" :path)))
                       (carriage--apply-acc-row state (plist-put (copy-sequence r) :file file))
                       (carriage--apply-bump state (plist-get r :status))
                       (if (eq (plist-get r :status) 'ok)
                           (carriage--apply-next state plan repo-root callback token)
                         (carriage--apply-finish plan state callback))))
                   (lambda (t0 res)
                     (let ((r (carriage--engine-row 'create res t0 "Created (staged)" "git add failed" :path)))
                       (carriage--apply-acc-row state (plist-put (copy-sequence r) :file file))
                       (carriage--apply-bump state (plist-get r :status))
                       (carriage--apply-finish plan state callback)))
                   token))
              (carriage--apply-acc-row state row0)
              (carriage--apply-bump state (plist-get row0 :status))
              (carriage--apply-next state plan repo-root callback token))))
        token plan))
      ('sre
       (carriage--apply-fs-async
        state
        (lambda ()
          (let ((row (carriage-apply-sre item repo-root)))
            (carriage--apply-acc-row state row)
            (carriage--apply-bump state (plist-get row :status))
            (carriage--apply-next state plan repo-root callback token)))
        token plan))
      ('aibo
       (carriage--apply-fs-async
        state
        (lambda ()
          (let ((row (carriage-apply-aibo item repo-root)))
            (carriage--apply-acc-row state row)
            (carriage--apply-bump state (plist-get row :status))
            (carriage--apply-next state plan repo-root callback token)))
        token plan))
      (_
       (carriage--apply-fs-async
        state
        (lambda ()
          (let ((row (carriage--report-fail (or op 'unknown) :details "Unknown op")))
            (carriage--apply-acc-row state row)
            (carriage--apply-bump state 'fail)
            (carriage--apply-finish plan state callback)))
        token plan)))))

(defun carriage--apply-next (state plan repo-root callback token)
  "Advance to the next item or finish."
  (if (plist-get state :aborted)
      (carriage--apply-finish plan state callback)
    (let ((q (plist-get state :queue)))
      (if (null q)
          (carriage--apply-complete state plan repo-root callback token)
        (let ((item (car q)))
          (plist-put state :queue (cdr q))
          (carriage--apply-run-item state item repo-root plan callback token))))))

(defun carriage--preflight--record-orig-branch (state repo-root)
  "Best-effort record of current branch into STATE for epilog."
  (ignore-errors
    (plist-put state :orig-branch (carriage-git-current-branch repo-root))))

(defun carriage--preflight--wip-async (state plan repo-root callback token)
  "Checkout WIP branch and proceed to next step; record :branch-name or report error."
  (carriage-git-checkout-wip-async
   repo-root nil
   (lambda (_ok)
     (plist-put state :branch-name (or (and (boundp 'carriage-mode-wip-branch)
                                            carriage-mode-wip-branch)
                                       "carriage/WIP"))
     (carriage--apply-next state plan repo-root callback token))
   (lambda (err2)
     (let* ((stderr (string-trim (or (plist-get err2 :stderr) "")))
            (stdout (string-trim (or (plist-get err2 :stdout) "")))
            (msg (list :code 'GIT_E_APPLY :severity 'error
                       :details (or (and (not (string-empty-p stderr)) stderr)
                                    (and (not (string-empty-p stdout)) stdout)
                                    "WIP checkout failed"))))
       (carriage--apply-acc-msg state msg)
       (carriage--apply-bump state 'fail)
       (carriage--apply-finish plan state callback)))))

(defun carriage--preflight--ephemeral-async (state plan repo-root callback token)
  "Create an ephemeral branch and proceed; on error report and finish."
  (carriage-git-create-ephemeral-branch-async
   repo-root
   (lambda (r-ephem)
     (let ((br (or (plist-get r-ephem :branch) "")))
       (plist-put state :branch-name br))
     (carriage--apply-next state plan repo-root callback token))
   (lambda (err2)
     (let* ((stderr (string-trim (or (plist-get err2 :stderr) "")))
            (stdout (string-trim (or (plist-get err2 :stdout) "")))
            (msg (list :code 'GIT_E_APPLY :severity 'error
                       :details (or (and (not (string-empty-p stderr)) stderr)
                                    (and (not (string-empty-p stdout)) stdout)
                                    "Ephemeral branch create failed"))))
       (carriage--apply-acc-msg state msg)
       (carriage--apply-bump state 'fail)
       (carriage--apply-finish plan state callback)))))

(defun carriage--preflight--ensure-repo-async (state plan repo-root callback token policy)
  "Ensure REPO exists, record orig branch, then branch per POLICY."
  (carriage-git-ensure-repo-async
   repo-root
   (lambda (_r1)
     (carriage--preflight--record-orig-branch state repo-root)
     (pcase policy
       ('in-place
        (carriage--apply-next state plan repo-root callback token))
       ('wip
        (carriage--preflight--wip-async state plan repo-root callback token))
       ('ephemeral
        (carriage--preflight--ephemeral-async state plan repo-root callback token))
       (_
        (carriage--preflight--wip-async state plan repo-root callback token))))
   (lambda (err1)
     (let* ((stderr (string-trim (or (plist-get err1 :stderr) "")))
            (stdout (string-trim (or (plist-get err1 :stdout) "")))
            (msg (list :code 'GIT_E_APPLY :severity 'error
                       :details (or (and (not (string-empty-p stderr)) stderr)
                                    (and (not (string-empty-p stdout)) stdout)
                                    "Git repo not detected"))))
       (carriage--apply-acc-msg state msg)
       (carriage--apply-bump state 'fail)
       (carriage--apply-finish plan state callback)))))

(defun carriage--apply-preflight-branch-async (state plan repo-root callback token)
  "Async preflight per =carriage-git-branch-policy'.
- in-place: no switching.
- wip: ensure/checkout carriage/WIP.
- ephemeral: create and checkout a unique ephemeral branch."
  (let* ((eng (carriage-apply-engine))
         (policy (and (eq eng 'git)
                      (boundp 'carriage-git-branch-policy)
                      carriage-git-branch-policy)))
    (plist-put state :branch-policy policy)
    (if (not (eq eng 'git))
        (carriage--apply-next state plan repo-root callback token)
      (carriage--preflight--ensure-repo-async state plan repo-root callback token policy))))

(defun carriage--epilog--delete-ephemeral-if-needed-async (state plan repo-root callback cur ok)
  "Delete ephemeral CUR when policy=ephemeral and auto-delete is enabled.
Deletion happens only when there are no applied items (OK=0) and branch appears empty.
If :orig-branch is known, verify emptiness by comparing diffs with ORIG. Respect
carriage-git-ephemeral-keep-on-fail: if any step failed and KEEP-ON-FAIL is t, do not delete."
  (let* ((auto-del (and (boundp 'carriage-git-auto-delete-empty-branch)
                        carriage-git-auto-delete-empty-branch))
         (keep-on-fail (and (boundp 'carriage-git-ephemeral-keep-on-fail)
                            carriage-git-ephemeral-keep-on-fail))
         (fail (plist-get state :fail))
         (orig (plist-get state :orig-branch)))
    (cond
     ;; Do not delete when failures occurred and policy says to keep the branch
     ((and keep-on-fail (numberp fail) (> fail 0))
      (carriage--apply-finish plan state callback))
     ;; Consider auto-delete only when OK==0 and branch name is sane
     ((and auto-del
           (numberp ok) (= ok 0)
           (stringp cur) (not (string-empty-p cur)))
      (if (and (stringp orig) (not (string-empty-p orig)) (not (string= orig cur))
               (fboundp 'carriage-git-branches-diff-empty-async))
          ;; Verify that ephemeral branch is effectively empty w.r.t. ORIG
          (carriage-git-branches-diff-empty-async
           repo-root orig cur
           (lambda (r)
             (if (plist-get r :empty)
                 (carriage-git-delete-branch-async
                  repo-root cur
                  (lambda (_okdel)
                    (carriage--apply-acc-msg
                     state (list :code 'GIT_INFO :severity 'info
                                 :details (format "Ephemeral branch %s deleted" (or cur ""))))
                    (carriage--apply-finish plan state callback))
                  (lambda (err)
                    (let* ((stderr (string-trim (or (plist-get err :stderr) "")))
                           (stdout (string-trim (or (plist-get err :stdout) ""))))
                      (carriage--apply-acc-msg
                       state (list :code 'GIT_E_APPLY :severity 'warn
                                   :details (or (and (not (string-empty-p stderr)) stderr)
                                                (and (not (string-empty-p stdout)) stdout)
                                                "Ephemeral branch delete failed")))
                      (carriage--apply-finish plan state callback))))
               ;; Not empty — keep branch
               (carriage--apply-finish plan state callback)))
           ;; If diff check failed, attempt best-effort deletion when ok==0 (fallback).
           (lambda (_err)
             (carriage-git-delete-branch-async
              repo-root cur
              (lambda (_okdel)
                (carriage--apply-acc-msg
                 state (list :code 'GIT_INFO :severity 'info
                             :details (format "Ephemeral branch %s deleted" (or cur ""))))
                (carriage--apply-finish plan state callback))
              (lambda (err2)
                (let* ((stderr (string-trim (or (plist-get err2 :stderr) "")))
                       (stdout (string-trim (or (plist-get err2 :stdout) ""))))
                  (carriage--apply-acc-msg
                   state (list :code 'GIT_E_APPLY :severity 'warn
                               :details (or (and (not (string-empty-p stderr)) stderr)
                                            (and (not (string-empty-p stdout)) stdout)
                                            "Ephemeral branch delete failed")))
                  (carriage--apply-finish plan state callback))))))
        ;; No ORIG available — fallback to simple ok==0 heuristic
        (carriage-git-delete-branch-async
         repo-root cur
         (lambda (_okdel)
           (carriage--apply-acc-msg
            state (list :code 'GIT_INFO :severity 'info
                        :details (format "Ephemeral branch %s deleted" (or cur ""))))
           (carriage--apply-finish plan state callback))
         (lambda (err)
           (let* ((stderr (string-trim (or (plist-get err :stderr) "")))
                  (stdout (string-trim (or (plist-get err :stdout) ""))))
             (carriage--apply-acc-msg
              state (list :code 'GIT_E_APPLY :severity 'warn
                          :details (or (and (not (string-empty-p stderr)) stderr)
                                       (and (not (string-empty-p stdout)) stdout)
                                       "Ephemeral branch delete failed")))
             (carriage--apply-finish plan state callback))))))
     (t
      (carriage--apply-finish plan state callback)))))

(defun carriage--epilog--after-switch (state plan repo-root callback)
  "After switching back (if needed), possibly delete ephemeral branch and finish."
  (let* ((policy (plist-get state :branch-policy))
         (ok (plist-get state :ok))
         (cur (plist-get state :branch-name)))
    (if (eq policy 'ephemeral)
        (carriage--epilog--delete-ephemeral-if-needed-async state plan repo-root callback cur ok)
      (carriage--apply-finish plan state callback))))

(defun carriage--epilog--maybe-switch-back (state plan repo-root callback)
  "Switch back to original branch when recorded and different; otherwise finish."
  (let* ((orig (plist-get state :orig-branch))
         (cur  (plist-get state :branch-name)))
    (if (and (stringp orig) (not (string-empty-p orig)) (not (string= orig cur)))
        (carriage-git-switch-branch-async
         repo-root orig
         (lambda (_oksw)
           (carriage--apply-acc-msg
            state (list :code 'GIT_INFO :severity 'info
                        :details (format "Switched back to %s" (or orig ""))))
           (carriage--epilog--after-switch state plan repo-root callback))
         (lambda (err)
           (let* ((stderr (string-trim (or (plist-get err :stderr) "")))
                  (stdout (string-trim (or (plist-get err :stdout) ""))))
             (carriage--apply-acc-msg
              state (list :code 'GIT_E_APPLY :severity 'warn
                          :details (or (and (not (string-empty-p stderr)) stderr)
                                       (and (not (string-empty-p stdout)) stdout)
                                       "Switch back failed")))
             (carriage--epilog--after-switch state plan repo-root callback))))
      (carriage--epilog--after-switch state plan repo-root callback))))

(defun carriage--apply-epilog-branch-async (state plan repo-root callback token)
  "Epilog for Git branch policy: switch back and optionally delete ephemeral branch.
Runs only when engine='git and policy∈{'wip 'ephemeral} and switch-back is enabled."
  (let* ((eng (carriage-apply-engine))
         (policy (plist-get state :branch-policy))
         (switch-back (and (boundp 'carriage-git-switch-back-on-complete)
                           carriage-git-switch-back-on-complete)))
    (if (or (not (eq eng 'git))
            (not (memq policy '(wip ephemeral)))
            (not switch-back))
        (carriage--apply-finish plan state callback)
      (carriage--epilog--maybe-switch-back state plan repo-root callback))))

(defun carriage--apply-complete (state plan repo-root callback token)
  "Decide whether to run branch epilog or finish immediately."
  (let* ((eng (carriage-apply-engine))
         (policy (plist-get state :branch-policy))
         (need-epilog (and (eq eng 'git)
                           (memq policy '(wip ephemeral)))))
    (if need-epilog
        (carriage--apply-epilog-branch-async state plan repo-root callback token)
      (carriage--apply-finish plan state callback))))

(defun carriage--plan-has-patch-p (queue)
  "Return non-nil when QUEUE contains any :op 'patch."
  (seq-some (lambda (it) (eq (carriage--plan-get it :op) 'patch)) queue))

(defun carriage--guard-apply-on-wip (repo-root)
  "Signal user-error when on WIP/ephemeral branch and applying is not allowed."
  (let ((allow-wip (and (boundp 'carriage-allow-apply-on-wip)
                        carriage-allow-apply-on-wip)))
    (unless allow-wip
      (condition-case _e
          (let* ((default-directory (file-name-as-directory (expand-file-name repo-root)))
                 (br (string-trim (or (with-temp-buffer
                                        (if (zerop (call-process "git" nil t nil "rev-parse" "--abbrev-ref" "HEAD"))
                                            (buffer-string) ""))
                                      "")))
                 (wip (or (and (boundp 'carriage-mode-wip-branch) carriage-mode-wip-branch)
                          "carriage/WIP"))
                 (epfx (or (and (boundp 'carriage-git-ephemeral-prefix) carriage-git-ephemeral-prefix)
                           "carriage/tmp")))
            (when (and (not (string-empty-p br))
                       (or (string= br wip)
                           (string-prefix-p epfx br)))
              (user-error "Вы на ветке %s (WIP/ephemeral). Применение отменено." br)))
        (error nil)))))

(defun carriage--async-initialize (plan repo-root)
  "Prepare initial async state, token and patch presence flag for PLAN."
  (let* ((queue (carriage--plan-sort plan))
         (state (carriage--make-apply-state queue repo-root))
         (token (list :abort-fn nil))
         (patch-present (carriage--plan-has-patch-p queue)))
    (carriage-log "async-init: plan-size=%d ops=%s"
                  (length queue)
                  (mapconcat (lambda (it) (format "%s" (carriage--plan-get it :op))) queue ", "))
    (list :queue queue :state state :token token :patch-present patch-present)))


(defun carriage--async-run (plan repo-root state token patch-present callback)
  "Start async run: force engine when needed, handle preflight, or proceed to first item."
  (let ((carriage-apply-engine (if patch-present 'git carriage-apply-engine)))
    (let* ((eng (carriage-apply-engine))
           (policy (and (eq eng 'git)
                        (boundp 'carriage-git-branch-policy)
                        carriage-git-branch-policy)))
      (carriage-log "async-run: engine=%s policy=%s patch-present=%s queue=%d"
                    eng policy (if patch-present "yes" "no") (length (plist-get state :queue)))
      (if (and (eq eng 'git) (not (eq policy 'in-place)))
          (carriage--apply-preflight-branch-async state plan repo-root callback token)
        (progn
          (when (eq eng 'git)
            (plist-put state :branch-policy policy))
          (carriage--apply-next state plan repo-root callback token))))))

(defun carriage-apply-plan-async (plan repo-root &optional callback)
  "Apply PLAN under REPO-ROOT asynchronously (event-driven FSM).
Returns a TOKEN plist with :abort-fn that cancels the current step (engine kill) or pending timers.
CALLBACK, when non-nil, is invoked with the final REPORT on the main thread."
  (carriage--guard-apply-on-wip repo-root)
  (cl-destructuring-bind (&key state token patch-present &allow-other-keys)
      (carriage--async-initialize plan repo-root)
    ;; Register abort handler early; will be updated per step
    (carriage--apply-update-abort state token)
    ;; Schedule start without passing extra ARGS to the lambda
    (run-at-time 0 nil
                 (lambda ()
                   (carriage--async-run plan repo-root state token patch-present callback)))
    token))

(provide 'carriage-apply)
;;; carriage-apply.el ends here
