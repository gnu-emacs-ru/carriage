;;; carriage-report.el --- Apply/Dry-run report buffer and actions  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: report, ui
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/ui-v2.org
;;   spec/apply-pipeline-v2.org
;;
;;; Commentary:
;; Render and actions for apply/dry-run reports.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'button)
(require 'carriage-utils)
(require 'carriage-apply)
(require 'carriage-logging)
(require 'carriage-ui)
(require 'carriage-traffic-batch nil t)
;; ediff is loaded lazily in interactive branches to avoid batch-mode side effects.
;; Byte-compile hygiene: declare external function used conditionally.
(declare-function carriage-sre-simulate-apply "carriage-op-sre" (plan-item repo-root))

(defconst carriage--report-buffer-name "*carriage-report*"
  "Name of the Carriage report buffer.")

(defun carriage-report-buffer ()
  "Return the report buffer, creating it if necessary."
  (get-buffer-create carriage--report-buffer-name))


(defun carriage--report-insert-line (cols &optional face)
  "Insert an Org-table row from COLS list, applying FACE to the whole row."
  (let* ((cells
          (mapcar (lambda (c)
                    (let* ((s (if (stringp c) c (format "%s" c))))
                      (setq s (replace-regexp-in-string "[\n\r]+" " " s))
                      (string-trim s)))
                  cols))
         (line (concat "| " (mapconcat #'identity cells " | ") " |"))
         (beg (point)))
    (insert line "\n")
    (when face
      (add-text-properties beg (point) (list 'face face)))))

(defun carriage--report-summary-line (report)
  "Build a summary header line for REPORT."
  (let* ((summary (plist-get report :summary))
         (ok      (plist-get summary :ok))
         (fail    (plist-get summary :fail))
         (skipped (plist-get summary :skipped))
         (engine  (plist-get report :engine))
         (policy  (plist-get report :branch-policy))
         (branch  (plist-get report :branch-name))
         (engine-part (if engine (format " (engine=%s%s%s)"
                                         engine
                                         (if policy (format ", policy=%s" policy) "")
                                         (if branch (format ", branch=%s" branch) ""))
                        "")))
    (format "Carriage report%s  ok:%s  fail:%s  skipped:%s\n\n"
            engine-part
            (or ok 0) (or fail 0) (or skipped 0))))

(defun carriage--report-row-face (status)
  "Return face symbol for STATUS."
  (pcase status
    ('ok    'carriage-report-ok-face)
    ('fail  'carriage-report-err-face)
    ((or 'warn 'skip) 'carriage-report-warn-face)
    (_      nil)))

(defun carriage--report-insert-header ()
  "Insert Org-table header and hline."
  (carriage--report-insert-line '("#" "op" "path" "status" "matches" "details" "preview" "actions"))
  (insert "|---+----+------+--------+---------+---------+---------+---------|\n"))

(defun carriage--report-attach-row (row-beg row-end it has-preview)
  "Attach item IT to row [ROW-BEG, ROW-END) and create buttons. HAS-PREVIEW gates [Diff]."
  ;; Attach item payload to the whole row for RET activation.
  (add-text-properties row-beg row-end (list 'carriage-report-item it))
  ;; [Diff]
  (when has-preview
    (save-excursion
      (goto-char row-beg)
      (when (search-forward "[Diff]" row-end t)
        (make-text-button (- (point) 6) (point)
                          'help-echo "Show full diff preview"
                          'follow-link t
                          'action (lambda (_btn) (carriage-report-show-diff-at-point))))))
  ;; [Ediff]
  (save-excursion
    (goto-char row-beg)
    (when (search-forward "[Ediff]" row-end t)
      (make-text-button (- (point) 7) (point)
                        'help-echo "Open Ediff for this item"
                        'follow-link t
                        'action (lambda (_btn) (carriage-report-ediff-at-point)))))
  ;; [Apply]
  (save-excursion
    (goto-char row-beg)
    (when (search-forward "[Apply]" row-end t)
      (make-text-button (- (point) 7) (point)
                        'help-echo "Apply this item"
                        'follow-link t
                        'action (lambda (_btn) (carriage-report-apply-at-point))))))



(defun carriage-report-render (report)
  "Render REPORT alist into the report buffer.
REPORT shape:
  (:plan PLAN
   :summary (:ok N :fail M :skipped K)
   :items ((:op OP :file PATH :status STATUS :details STR :diff PREVIEW :_plan PLAN-ITEM) ...))"
  (let* ((buf (carriage-report-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (read-only-mode -1)
        (erase-buffer)
        (insert (carriage--report-summary-line report))
        ;; Render top-level messages if present
        (let* ((msgs (plist-get report :messages))
               (fpr  (plist-get report :fingerprint))
               (msgs2 (if (and (stringp fpr) (> (length fpr) 0))
                          (append (list (list :severity 'info :code 'FINGERPRINT :details fpr)) msgs)
                        msgs)))
          (when (and msgs2 (listp msgs2))
            (insert "messages:\n")
            (dolist (m msgs2)
              (let* ((sev (or (plist-get m :severity) 'info))
                     (code (or (plist-get m :code) 'UNKNOWN))
                     (file (or (plist-get m :file) (plist-get m :path)))
                     (details (or (plist-get m :details) "")))
                (insert (format "- [%s] %s — %s%s\n"
                                sev code details
                                (if file (format " (file %s)" file) "")))))
            (insert "\n")))
        (carriage--report-insert-header)
        ;; Items
        (let* ((items (or (plist-get report :items) '()))
               (i 0))
          (dolist (it items)
            (setq i (1+ i))
            (let* ((op          (plist-get it :op))
                   (file        (or (plist-get it :file) (plist-get it :path)))
                   (status      (plist-get it :status))
                   (matches     (plist-get it :matches))
                   (details     (or (plist-get it :details) ""))
                   ;; Append changed-bytes to details when present
                   (_dummy-cb (let ((cb (plist-get it :changed-bytes)))
                                (when (numberp cb)
                                  (setq details (format "%s (Δbytes=%d)" details cb)))))
                   (preview-raw  (or (plist-get it :diff) ""))
                   (has-preview  (and (stringp preview-raw) (> (length preview-raw) 0)))
                   (preview-flat (if has-preview
                                     (string-trim (replace-regexp-in-string "[\n\r]+" " " preview-raw))
                                   ""))
                   (preview-short (if has-preview
                                      (truncate-string-to-width preview-flat 60 nil nil t)
                                    ""))
                   (matches-str (cond
                                 ((numberp matches) (number-to-string matches))
                                 ((stringp matches) matches)
                                 ((null matches) "")
                                 (t (format "%s" matches))))
                   (action (concat (if has-preview "[Diff]" "") " [Ediff] [Apply]"))
                   (row-face (carriage--report-row-face status))
                   (row-beg (point)))
              ;; Insert row
              (carriage--report-insert-line
               (list i op file status matches-str details preview-short action)
               row-face)
              ;; Attach properties and buttons to the just inserted row
              (let ((row-end (save-excursion
                               (forward-line -1)
                               (line-end-position))))
                (add-text-properties row-beg row-end (list 'carriage-report-item it))
                (carriage--report-attach-row row-beg row-end it has-preview)))))
        ;; Finalize buffer
        (goto-char (point-min))
        (carriage-report-mode)
        (read-only-mode 1)))
    buf))

;;;###autoload
(defun carriage-report-open (&optional report)
  "Open the report buffer and optionally RENDER REPORT alist.
Keeps focus and major-mode of the current buffer intact.

The report buffer is shown in a top side window (above the main window)."
  (interactive)
  (when report
    ;; Do not overwrite an existing useful report with an empty one.
    (let* ((items (plist-get report :items))
           (msgs  (plist-get report :messages)))
      (if (and (null items) (null msgs))
          (carriage-log "report-open: skip rendering empty report items=%d msgs=%d"
                        (length (or items '())) (length (or msgs '())))
        (carriage-report-render report)))
    ;; Announce success in Messages when all items succeeded (ok>0, fail==0).
    (let* ((summary (plist-get report :summary))
           (ok      (and summary (plist-get summary :ok)))
           (fail    (and summary (plist-get summary :fail))))
      (when (and (numberp ok) (> ok 0)
                 (numberp fail) (zerop fail))
        (let* ((items (or (plist-get report :items) '()))
               (oks (cl-remove-if-not (lambda (it) (eq (plist-get it :status) 'ok)) items))
               (created 0) (deleted 0) (renamed 0) (modified 0)
               (files '()))
          (dolist (it oks)
            (let* ((op-raw (plist-get it :op))
                   (op (if (and (consp op-raw) (eq (car op-raw) 'quote)) (cadr op-raw) op-raw)))
              (pcase op
                ('create (setq created (1+ created)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
                ('delete (setq deleted (1+ deleted)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
                ('rename (setq renamed (1+ renamed)) (push (or (plist-get it :file) (plist-get it :path) "-") files))
                ((or 'patch 'sre 'aibo 'replace)
                 (setq modified (1+ modified))
                 (push (or (plist-get it :file) (plist-get it :path) "-") files))
                (_ (push (or (plist-get it :file) (plist-get it :path) "-") files)))))
          (let* ((total (length oks))
                 (files-str (mapconcat #'identity (nreverse (delete-dups (delq nil files))) ", ")))
            (when (> total 0)
              (message "Carriage: applied OK (%d items) — created:%d modified:%d deleted:%d renamed:%d — %s"
                       total created modified deleted renamed files-str)))))))
  (save-selected-window
    ;; Show report in a top side window so it is easy to reach via window navigation.
    (carriage--display-aux-buffer (carriage-report-buffer) 'top 0.33 t)))


(defun carriage-report--item-at-point ()
  "Return report item plist stored at point, or nil."
  (or (get-text-property (point) 'carriage-report-item)
      (get-text-property (line-beginning-position) 'carriage-report-item)))

(defun carriage-report-show-diff-at-point ()
  "Open a buffer with full :diff preview for the report item at point.
If no preview is available, signal a user-visible message."
  (interactive)
  (let* ((it (carriage-report--item-at-point))
         (diff (and it (plist-get it :diff)))
         (path (or (plist-get it :file) (plist-get it :path) "")))
    (if (and (stringp diff) (> (length diff) 0))
        (let* ((buf (get-buffer-create (format "*carriage-diff: %s*" path))))
          (with-current-buffer buf
            (read-only-mode -1)
            (erase-buffer)
            (insert diff)
            (goto-char (point-min))
            (view-mode 1))
          (carriage--display-aux-buffer buf))
      (user-error "No diff preview available at point"))))

(defun carriage--report-ediff-at-point-impl ()
  "Implementation for =carriage-report-ediff-at-point'. See that command for behavior."
  (let* ((it (carriage-report--item-at-point)))
    (unless it
      (user-error "No report item at point"))
    (let* ((plan (plist-get it :_plan))
           (op   (or (plist-get it :op) (and plan (alist-get :op plan))))
           (root (or (plist-get it :_root) (carriage-project-root) default-directory)))
      (pcase op
        ('sre
         (let* ((file (or (plist-get it :file) (and plan (alist-get :file plan))))
                (abs  (and file (ignore-errors (carriage-normalize-path root file)))))
           (unless (and abs (file-exists-p abs))
             (user-error "File not found: %s" (or file "<nil>")))
           (if (bound-and-true-p noninteractive)
               (message "Prepared SRE Ediff (noninteractive)")
             (let* ((before (carriage-read-file-string abs))
                    (sim    (if (and plan (fboundp 'carriage-sre-simulate-apply))
                                (carriage-sre-simulate-apply plan root)
                              (list :after before :count 0)))
                    (after  (or (plist-get sim :after) before))
                    (bufA   (get-buffer-create (format "*carriage-ediff A: %s/" file)))
                    (bufB   (get-buffer-create (format "*carriage-ediff B: %s/" file))))
               (with-current-buffer bufA (read-only-mode -1) (erase-buffer) (insert before) (set-buffer-modified-p nil) (read-only-mode 1))
               (with-current-buffer bufB (read-only-mode -1) (erase-buffer) (insert after)  (set-buffer-modified-p nil) (read-only-mode 1))
               (let ((default-directory (or (and root (file-name-as-directory (expand-file-name root))) "/")))
                 (require 'ediff nil t)
                 (ediff-buffers bufA bufB))))))
        ('patch
         (let* ((diff (or (plist-get it :diff) (and plan (alist-get :diff plan))))
                (path (or (plist-get it :path) (and plan (alist-get :path plan))))
                (abs  (and path (ignore-errors (carriage-normalize-path root path)))))
           (if (bound-and-true-p noninteractive)
               (progn
                 (if diff
                     (message "Prepared patch for Ediff (noninteractive)")
                   (message "No diff available for patch item (noninteractive)")))
             (let* ((patch-file (and diff (make-temp-file "carriage-ediff-" nil ".diff"))))
               (unwind-protect
                   (progn
                     (when patch-file
                       (with-temp-file patch-file (insert diff)))
                     (unless diff
                       (user-error "Cannot run Ediff for patch: missing :diff"))
                     (let ((default-directory (or (and root (file-name-as-directory (expand-file-name root))) "/")))
                       (require 'ediff nil t)
                       (if (and abs (file-exists-p abs))
                           (ediff-patch-file patch-file abs)
                         (let* ((ext (file-name-extension (or path "") ""))
                                (base (make-temp-file "carriage-ediff-base-" nil (if (and ext (not (string-empty-p ext))) (concat "." ext)))))
                           ;; Create empty base file to emulate /dev/null for create patches.
                           (with-temp-file base)
                           (message "Carriage: base file missing, launching Ediff against empty file")
                           (ediff-patch-file patch-file base)))))
                 (when patch-file (ignore-errors (delete-file patch-file))))))))
        (_
         (if (bound-and-true-p noninteractive)
             (message "Ediff not supported for op: %S (noninteractive)" op)
           (user-error "Ediff not supported for op: %S" op)))))))

(defun carriage-report-ediff-at-point ()
  "Open Ediff for the report item at point.
For SRE: build in-memory \"after\" and run ediff-buffers.
For patch: run ediff-patch-file with the unified diff and target file.
In noninteractive (batch) mode, prepare data and never signal an error."
  (interactive)
  (if (bound-and-true-p noninteractive)
      (condition-case err
          (carriage--report-ediff-at-point-impl)
        (error
         (message "Ediff suppressed in batch: %s" (error-message-string err))
         nil))
    (carriage--report-ediff-at-point-impl)))

;;; NEW: apply-at-point

;;;###autoload
(defun carriage-report-apply-at-point ()
  "Apply the report item at point using its stored plan and root (async by default).
In batch mode runs non-interactively and refreshes report."
  (interactive)
  (let* ((it (carriage-report--item-at-point)))
    (unless it
      (user-error "No report item at point"))
    (let* ((plan-item (plist-get it :_plan))
           (root      (or (plist-get it :_root) (carriage-project-root) default-directory)))
      (unless (and plan-item root)
        (user-error "No plan/root stored on this row"))
      (let* ((op (or (and (listp plan-item) (plist-get plan-item :op)) (alist-get :op plan-item)))
             (target (or (alist-get :path plan-item) (alist-get :file plan-item))))
        (carriage-log "report-apply: op=%s target=%s root=%s" op (or target "-") root))
      ;; Apply/dry-run is a local pipeline concern: reflect it via apply-status badge,
      ;; do not mutate request/transport state.
      (when (fboundp 'carriage-ui-apply-set-state)
        (carriage-ui-apply-set-state 'running "Apply…"))
      (if (and (boundp 'carriage-apply-async) carriage-apply-async (not noninteractive))
          (progn
            (carriage-log "report-apply: async apply scheduled for %s" (plist-get it :path))
            (carriage-apply-plan-async
             (list plan-item) root
             (lambda (rep)
               (carriage-report-open rep)
               (when (and (not noninteractive)
                          (let* ((sum (plist-get rep :summary)))
                            (and sum (zerop (or (plist-get sum :fail) 0)))))
                 (let* ((items (or (plist-get rep :items) '()))
                        (oks (cl-remove-if-not (lambda (it) (eq (plist-get it :status) 'ok)) items))
                        (created 0) (deleted 0) (renamed 0) (modified 0)
                        (files '()))
                   (dolist (it oks)
                     (let* ((op-raw (plist-get it :op))
                            (op (if (and (consp op-raw) (eq (car op-raw) 'quote)) (cadr op-raw) op-raw)))
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
               ;; apply-status badge is updated from the apply report (carriage-ui-note-apply-report).
               rep))))
      (let* ((rep (carriage-apply-plan (list plan-item) root)))
        (carriage-report-open rep)
        ;; apply-status badge is updated from the apply report.
        rep))))



(unless (boundp 'carriage-report-mode-map)
  (defvar carriage-report-mode-map
    (let* ((map (make-sparse-keymap)))
      (set-keymap-parent map special-mode-map)
      map)
    "Keymap for Carriage report buffers.

All report bindings are provided by `carriage-keyspec' (bindings-first):
- report-local: RET/d/e/a (see keyspec)
- prefix-map: C-c e → `carriage-prefix-map' (see keyspec/global-mode)."))

(defun carriage-report-mode (&optional _arg)
  "Major mode for Carriage report buffers."
  (interactive)
  (kill-all-local-variables)
  (use-local-map carriage-report-mode-map)
  (setq major-mode 'carriage-report-mode)
  (setq mode-name "Carriage-Report")
  (setq-local header-line-format "Carriage: Report")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (run-mode-hooks 'carriage-report-mode-hook)
  ;; Ensure keyspec bindings and prefix-map are installed.
  (when (require 'carriage-keyspec nil t)
    (ignore-errors (carriage-keys-install-known-keymaps))))

;; Report buffer mode is now set inside carriage-report-render; no advice needed.

(provide 'carriage-report)
;;; carriage-report.el ends here
