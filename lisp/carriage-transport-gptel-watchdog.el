;;; carriage-transport-gptel-watchdog.el --- Watchdog for GPTel transport  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Watchdog timeout handling for GPTel transport.
;; Extracted from carriage-transport-gptel.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar-local carriage-transport-gptel--wd-timer nil)
(defvar-local carriage-transport-gptel--wd-last-activity 0.0)
(defvar-local carriage-transport-gptel--wd-fired nil)
(defvar-local carriage-transport-gptel--wd-timeout nil
  "Current watchdog timeout (seconds).")
(defvar-local carriage-transport-gptel--wd-extended nil
  "Non-nil once watchdog timeout has been extended after first stream content arrives.")

(defun carriage-transport-gptel--wd-stop ()
  "Stop watchdog timer."
  (when (timerp carriage-transport-gptel--wd-timer)
    (cancel-timer carriage-transport-gptel--wd-timer))
  (setq carriage-transport-gptel--wd-timer nil))

(defun carriage-transport-gptel--wd-touch ()
  "Record activity timestamp."
  (setq carriage-transport-gptel--wd-last-activity (float-time)))

(defun carriage-transport-gptel--wd-extended-timeout ()
  "Return extended (longer) timeout value suitable for streaming.
Returns nil if extended timeout is not configured."
  (let* ((base-timeout (or (and (boundp 'carriage-transport-startup-timeout)
                                carriage-transport-startup-timeout)
                          60.0))
         (multiplier (or (and (boundp 'carriage-transport-silence-timeout-multiplier)
                              carriage-transport-silence-timeout-multiplier)
                         2.0)))
    (when (numberp multiplier)
      (* base-timeout multiplier))))

(defun carriage-transport-gptel--wd-extend ()
  "Extend watchdog timeout after first stream content arrives (idempotent)."
  (unless carriage-transport-gptel--wd-extended
    (let ((new-timeout (carriage-transport-gptel--wd-extended-timeout)))
      (when (numberp new-timeout)
        (setq carriage-transport-gptel--wd-timeout new-timeout)
        (setq carriage-transport-gptel--wd-extended t)
        (carriage-transport-gptel--wd-stop)
        (let* ((origin-buffer (current-buffer))
               (on-timeout (lambda ()
                             (when (buffer-live-p origin-buffer)
                               (with-current-buffer origin-buffer
                                 (setq carriage-transport-gptel--wd-fired t)
                                 (carriage-transport-gptel--finalize
                                  'LLM_E_TIMEOUT "watchdog timeout after extend"))))))
          (carriage-transport-gptel--wd-start origin-buffer new-timeout on-timeout))))))

(defun carriage-transport-gptel--wd-start (origin-buffer id on-timeout)
  "Start watchdog timer for ORIGIN-BUFFER.
ID is used for timer identification.
ON-TIMEOUT is a zero-arg function called when the watchdog fires."
  (carriage-transport-gptel--wd-stop)
  (setq carriage-transport-gptel--wd-fired nil)
  (let* ((timeout (or carriage-transport-gptel--wd-timeout
                       (if (boundp 'carriage-transport-startup-timeout)
                           carriage-transport-startup-timeout
                         60.0)))
         (buf origin-buffer))
    (setq carriage-transport-gptel--wd-timer
          (run-at-time timeout nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (funcall on-timeout))))))
    (carriage-transport-gptel--wd-touch)))

(provide 'carriage-transport-gptel-watchdog)
;;; carriage-transport-gptel-watchdog.el ends here
