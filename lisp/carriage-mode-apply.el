;;; carriage-mode-apply.el --- Apply commands for Carriage mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Apply patch commands for Carriage mode.
;; Extracted from carriage-mode.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-project-root "carriage-utils" ())
(declare-function carriage-apply-engine "carriage-apply-engine" ())
(declare-function carriage-apply-plan "carriage-apply" (plan root))
(declare-function carriage-apply-plan-async "carriage-apply" (plan root callback))
(declare-function carriage-parse-blocks-in-region "carriage-apply" (beg end root))
(declare-function carriage-git-current-branch "carriage-git" (&optional dir))
(declare-function carriage--guard-patch-engine-for-item "carriage-mode" (plan-item))
(declare-function carriage--parse-plan-item-or-error "carriage-mode" (root))
(declare-function carriage--report-open-maybe "carriage-mode" (report))
(declare-function carriage-ui-apply-set-state "carriage-ui" (state msg))
(declare-function carriage-ui-apply-reset "carriage-ui" ())

(defvar-local carriage--apply-entry-id nil)
(defvar-local carriage--apply-entry-log-count 0)
(defvar-local carriage--apply-response-fingerprint nil)
(defvar-local carriage--apply-claim-keys nil)

(defun carriage--apply-async-enabled-p ()
  "Return non-nil when async apply is enabled for the current session."
  (and (boundp 'carriage-apply-async)
       carriage-apply-async
       (not noninteractive)))

(defun carriage--auto-apply-guard-claim (&optional source response-fingerprint unit)
  "Claim auto-apply for a concrete apply UNIT within the current send entry."
  (let* ((entry (and (boundp 'carriage--current-send-entry-id)
                     carriage--current-send-entry-id))
         (source1 (or source 'unknown))
         (fp (and (stringp response-fingerprint)
                  (not (string-empty-p response-fingerprint))
                  response-fingerprint))
         (key (carriage--auto-apply-unit-key source1 unit fp)))
    (setq carriage--apply-entry-log-count (1+ (or carriage--apply-entry-log-count 0)))
    (if (member key carriage--apply-claim-keys)
        (progn
          (carriage-log "auto-apply: DROP duplicate source=%s entry=%s fp=%s key=%S attempt=%s"
                        source1 (or entry "-") (or fp "-") key carriage--apply-entry-log-count)
          nil)
      (progn
        (push key carriage--apply-claim-keys)
        (carriage-log "auto-apply: claim source=%s entry=%s fp=%s key=%S attempt=%s"
                      source1 (or entry "-") (or fp "-") key carriage--apply-entry-log-count)
        t))))

(defun carriage--apply-report-success-p (report)
  "Return non-nil when REPORT indicates success (fail==0)."
  (let* ((sum (and (listp report) (plist-get report :summary)))
         (fails (or (and sum (plist-get sum :fail)) 0)))
    (and (numberp fails) (zerop fails))))

(defun carriage--apply-handle-finished-report (report)
  "Common post-processing for finished apply REPORT."
  (when (and report (not noninteractive))
    (carriage--report-open-maybe report)
    (when (carriage--apply-report-success-p report)
      (carriage--announce-apply-success report)))
  report)

(defun carriage--apply-single-item-sync (plan-item root)
  "Apply a single PLAN-ITEM under ROOT synchronously."
  (carriage--apply-handle-finished-report
   (carriage-apply-plan (list plan-item) root)))

(defun carriage--apply-single-item-async (plan-item root)
  "Apply a single PLAN-ITEM under ROOT asynchronously."
  (carriage-log "apply-at-point: async apply scheduled")
  (carriage-apply-plan-async
   (list plan-item) root
   (lambda (rep)
     (carriage--apply-handle-finished-report rep))))

(defun carriage--apply-single-item-dispatch (plan-item root)
  "Apply single PLAN-ITEM under ROOT, async when configured."
  (let* ((op (or (and (listp plan-item) (plist-get plan-item :op)) (alist-get :op plan-item)))
         (path (or (alist-get :path plan-item) (alist-get :file plan-item)))
         (eng (carriage-apply-engine))
         (pol (and (eq eng 'git) (boundp 'carriage-git-branch-policy) carriage-git-branch-policy))
         (manual-apply (null (and (boundp 'carriage--current-send-entry-id)
                                 carriage--current-send-entry-id))))
    (carriage-log "apply-dispatch: op=%s target=%s engine=%s policy=%s entry=%s manual=%s"
                  op (or path "-") eng pol
                  (or (and (boundp 'carriage--current-send-entry-id)
                           carriage--current-send-entry-id)
                      "-")
                  (if manual-apply "t" "nil"))
    (if (or manual-apply
            (carriage--auto-apply-guard-claim 'single-item nil plan-item))
        (if (carriage--apply-async-enabled-p)
            (carriage--apply-single-item-async plan-item root)
          (carriage--apply-single-item-sync plan-item root))
      (progn
        (when (fboundp 'carriage-ui-apply-set-state)
          (carriage-ui-apply-set-state 'aborted "Duplicate auto-apply dropped"))
        nil))))

;;;###autoload
(defun carriage-apply-at-point ()
  "Dry-run → confirm → apply for the patch block at point."
  (interactive)
  (let* ((root (or (carriage-project-root) default-directory))
         (plan-item (carriage--parse-plan-item-or-error root)))
    (carriage--guard-patch-engine-for-item plan-item)
    (let* ((cur-br (ignore-errors (and (fboundp 'carriage-git-current-branch)
                                       (carriage-git-current-branch root))))
           (epref (or (and (boundp 'carriage-git-ephemeral-prefix)
                           carriage-git-ephemeral-prefix)
                      "carriage/tmp"))
           (wip (string= cur-br (or (and (boundp 'carriage-mode-wip-branch) carriage-mode-wip-branch)
                                    "carriage/WIP")))
           (eph (and (stringp cur-br) (string-prefix-p epref cur-br))))
      (when (and (or wip eph)
                 (not (and (boundp 'carriage-allow-apply-on-wip) carriage-allow-apply-on-wip)))
        (when (fboundp 'carriage-ui-apply-set-state)
          (carriage-ui-apply-set-state
           'apply-fail
           (format "Запрещено применять на ветке %s (WIP/ephemeral)" (or cur-br ""))))
        (user-error "Нельзя применять патчи на ветке %s" (or cur-br ""))))
    (let ((dry (carriage--dry-run-single-item plan-item root)))
      (when (not noninteractive)
        (carriage--report-open-maybe dry))
      (let* ((sum (plist-get dry :summary))
             (fails (or (plist-get sum :fail) 0)))
        (if (> fails 0)
            (progn
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
        (let ((has-patch (cl-some (lambda (it)
                                    (eq (or (alist-get :op it)
                                            (and (listp it) (plist-get it :op)))
                                        'patch))
                                  plan)))
          (when (and has-patch (not (carriage--engine-supports-op-p 'patch)))
            (when (fboundp 'carriage-ui-apply-set-state)
              (carriage-ui-apply-set-state 'dry-fail "Patch unsupported by current engine"))
            (user-error "Patch unsupported by %s engine" (carriage-apply-engine))))
        (let* ((cur-br (ignore-errors (and (fboundp 'carriage-git-current-branch)
                                           (carriage-git-current-branch root))))
               (epref (or (and (boundp 'carriage-git-ephemeral-prefix)
                               carriage-git-ephemeral-prefix)
                          "carriage/tmp"))
               (wip (string= cur-br (or (and (boundp 'carriage-mode-wip-branch) carriage-mode-wip-branch)
                                        "carriage/WIP")))
               (eph (and (stringp cur-br) (string-prefix-p epref cur-br))))
          (when (and (or wip eph)
                     (not (and (boundp 'carriage-allow-apply-on-wip) carriage-allow-apply-on-wip)))
            (when (fboundp 'carriage-ui-apply-set-state)
              (carriage-ui-apply-set-state
               'apply-fail
               (format "Запрещено применять на ветке %s (WIP/ephemeral)" (or cur-br ""))))
            (user-error "Нельзя применять патчи на ветке %s" (or cur-br ""))))
        (let ((dry (carriage-apply-plan-async
                     plan root
                     (lambda (r)
                       (carriage--apply-handle-finished-report r)))))
          (when (not noninteractive)
            (carriage--report-open-maybe dry)))))
    (carriage-apply-at-point)))

(provide 'carriage-mode-apply)
;;; carriage-mode-apply.el ends here
