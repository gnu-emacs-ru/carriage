;;; carriage-transport-watchdog.el --- Transport-level watchdog and diagnostics  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Transport-level watchdog for stuck requests.
;; Extracted from carriage-transport.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-transport-watchdog nil
  "Watchdog and diagnostics for Carriage transport."
  :group 'carriage)

(defcustom carriage-transport-watchdog-enabled t
  "When non-nil, enable transport-level watchdog for stuck requests."
  :type 'boolean
  :group 'carriage-transport-watchdog)

(defcustom carriage-transport-watchdog-stall-seconds 90.0
  "Seconds of no progress after which the watchdog fails the request."
  :type 'number
  :group 'carriage-transport-watchdog)

(defcustom carriage-transport-watchdog-check-interval 1.0
  "How often (seconds) the watchdog checks for stalling."
  :type 'number
  :group 'carriage-transport-watchdog)

(defcustom carriage-transport-watchdog-debug nil
  "When non-nil, emit extra diagnostics for watchdog/completion paths."
  :type 'boolean
  :group 'carriage-transport-watchdog)

(defcustom carriage-transport-progress-trace nil
  "When non-nil, log every progress call with rid and timestamp."
  :type 'boolean
  :group 'carriage-transport-watchdog)

(defvar-local carriage-transport--watchdog-timer nil)
(defvar-local carriage-transport--watchdog-abort-fn nil)
(defvar-local carriage-transport--watchdog-abort-rid nil)
(defvar-local carriage-transport--watchdog-fired nil)

(defun carriage-transport-note-progress (&optional what buffer)
  "Record progress for request in BUFFER (or current buffer).
Optional WHAT describes the progress kind for diagnostics."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-transport--last-progress-at (float-time))
    (when (and carriage-transport-progress-trace (stringp what))
      (carriage-log "Transport progress: %s rid=%s" what (or carriage-transport--request-id "-")))))

(defun carriage-transport--watchdog-stop (&optional buffer)
  "Stop watchdog timer for BUFFER (or current buffer). Best-effort."
  (with-current-buffer (or buffer (current-buffer))
    (when (timerp carriage-transport--watchdog-timer)
      (ignore-errors (cancel-timer carriage-transport--watchdog-timer)))
    (setq carriage-transport--watchdog-timer nil)
    t))

(defun carriage-transport--watchdog-trigger (rid idle stall &optional buffer)
  "Trigger watchdog failure for RID due to IDLE>=STALL in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (unless carriage-transport--watchdog-fired
      (setq carriage-transport--watchdog-fired t)
      (carriage-log "Transport: watchdog timeout rid=%s idle=%.3fs stall=%.3fs"
                    (or rid "-") (max 0.0 (or idle 0.0)) (max 0.0 (or stall 0.0)))
      (when carriage-transport-watchdog-debug
        (let ((st (and (boundp 'carriage--ui-state) carriage--ui-state))
              (lp (and (boundp 'carriage-transport--last-progress-at)
                       carriage-transport--last-progress-at)))
          (carriage-log "Transport: watchdog diag rid=%s buf=%s state=%s last-progress=%s"
                        (or rid "-") (buffer-name (current-buffer))
                        (or st "-")
                        (if (numberp lp) (format "%.3f" lp) "-"))))
        (when-let* ((bt (carriage-transport--short-backtrace 14)))
          (carriage-log "Transport: watchdog backtrace rid=%s\n%s" (or rid "-") bt)))
      ;; Try abort first, then guarantee completion state change.
      (if (and (functionp carriage-transport--watchdog-abort-fn)
               (stringp rid)
               (stringp carriage-transport--watchdog-abort-rid)
               (string= rid carriage-transport--watchdog-abort-rid))
          (condition-case e
              (funcall carriage-transport--watchdog-abort-fn)
            (error
             (carriage-log "Transport: watchdog abort-fn error rid=%s: %s"
                           (or rid "-") (error-message-string e))))
        (when carriage-transport-watchdog-debug
          (carriage-log "Transport: watchdog abort-fn missing/stale rid=%s abort-rid=%s"
                        (or rid "-")
                        (or carriage-transport--watchdog-abort-rid "-"))))
      (carriage-transport--watchdog-stop (current-buffer))
      (when carriage-transport-watchdog-debug
        (carriage-log "Transport: watchdog -> complete(error) rid=%s current-rid=%s"
                      (or rid "-")
                      (or (carriage-transport-current-request-id (current-buffer)) "-")))
      (ignore-errors (carriage-transport-complete t (current-buffer))))))

(defun carriage-transport--watchdog-start (&optional buffer)
  "Start watchdog timer for BUFFER if enabled."
  (with-current-buffer (or buffer (current-buffer))
    (carriage-transport--watchdog-stop (current-buffer))
    (when (and carriage-transport-watchdog-enabled
               (numberp carriage-transport-watchdog-stall-seconds)
               (> carriage-transport-watchdog-stall-seconds 0)
               (numberp carriage-transport-watchdog-check-interval)
               (> carriage-transport-watchdog-check-interval 0))
      (let* ((buf (current-buffer))
             (stall (float carriage-transport-watchdog-stall-seconds))
             (interval (float carriage-transport-watchdog-check-interval)))
        (setq carriage-transport--watchdog-timer
              (run-at-time
               interval interval
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let* ((rid carriage-transport--request-id)
                            (lp (or carriage-transport--last-progress-at (float-time)))
                            (idle (- (float-time) lp)))
                       (when (or (null rid) (string-empty-p (format "%s" rid)))
                         (carriage-transport--watchdog-stop buf))
                       (when (and (stringp rid) (>= idle stall))
                         (carriage-transport--watchdog-trigger rid idle stall buf))))))))))
    t))

(provide 'carriage-transport-watchdog)
;;; carriage-transport-watchdog.el ends here
