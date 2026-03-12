;;; carriage.el --- Entry point and initialization  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: core, entry
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/carriage-mode-v2.org
;;   spec/extensibility-points-v2.org
;;   spec/apply-pipeline-v2.org
;;   spec/apply-engines-v2.org
;;   spec/llm-transport-v2.org
;;
;;; Commentary:
;; Package entry: define-errors, add load-path rules and require core modules.
;;
;;; Code:

(require 'carriage-errors)
(carriage-define-errors)

(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-pricing)
;; Registries and suite builder (ops modules register themselves)
(require 'carriage-format-registry)
(require 'carriage-intent-registry)
(require 'carriage-suite)
(require 'carriage-global-mode)
(require 'carriage-parser)
(require 'carriage-apply)

(require 'carriage-apply-engine)
(require 'carriage-engine-git)
(require 'carriage-engine-emacs)
(require 'carriage-mode)
(require 'carriage-transport)
(require 'carriage-stream-perf)
(require 'carriage-stream-tune)
(require 'carriage-stream-silence)
(require 'carriage-task)
;; Transports are loaded lazily by carriage-transport-dispatch per spec.
;; Do not require adapters by default here.

(require 'carriage-announce)
;; -----------------------------------------------------------------------------
;; Debug tracing: Send/context/transport (to diagnose "Send hangs")
;;
;; Enable:
;;   (setq carriage-send-trace t)
;; or:
;;   M-x carriage-toggle-send-trace
;;
;; Logs go to both `carriage-log' (when available) and *Messages* (fallback).
;;
;; Design:
;; - Advice is installed once, but logging is guarded by `carriage-send-trace'.
;; - Adds a watchdog that fires if transport dispatch did not happen soon.

(require 'cl-lib)

(defgroup carriage-send-trace nil
  "Tracing for Send/context/transport pipeline (debugging hangs)."
  :group 'carriage)

(defcustom carriage-send-trace nil
  "When non-nil, emit detailed trace logs for Send/context/transport."
  :type 'boolean
  :group 'carriage-send-trace)

(defcustom carriage-send-trace-watchdog-seconds 3.0
  "Seconds after Send start to emit a watchdog trace if dispatch did not occur."
  :type 'number
  :group 'carriage-send-trace)

(defvar carriage-send-trace--send-seq 0
  "Monotonic counter for Send trace ids.")

(defvar-local carriage-send-trace--watchdog-timer nil
  "Buffer-local watchdog timer for Send trace.")

(defun carriage-send-trace--log (fmt &rest args)
  "Best-effort trace logger (never signals)."
  (when carriage-send-trace
    (let ((msg (apply #'format fmt args)))
      (condition-case _e
          (when (fboundp 'carriage-log)
            (carriage-log "%s" msg))
        (error nil))
      (condition-case _e
          (message "%s" msg)
        (error nil)))))

(defun carriage-send-trace--watchdog-cancel ()
  "Cancel pending watchdog timer for the current buffer."
  (when (timerp carriage-send-trace--watchdog-timer)
    (ignore-errors (cancel-timer carriage-send-trace--watchdog-timer)))
  (setq carriage-send-trace--watchdog-timer nil))

(defun carriage-send-trace--watchdog-arm (id t0)
  "Arm watchdog for current buffer."
  (carriage-send-trace--watchdog-cancel)
  (let ((buf (current-buffer))
        (delay (max 0.1 (float (or carriage-send-trace-watchdog-seconds 3.0)))))
    (setq carriage-send-trace--watchdog-timer
          (run-at-time
           delay nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (carriage-send-trace--log
                  "TRACE[watchdog] id=%s elapsed=%.2fs ui-state=%S backend=%S provider=%S engine=%S"
                  id (- (float-time) t0)
                  (and (boundp 'carriage--ui-state) carriage--ui-state)
                  (and (boundp 'carriage-mode-backend) carriage-mode-backend)
                  (and (boundp 'carriage-mode-provider) carriage-mode-provider)
                  (and (boundp 'carriage-apply-engine) carriage-apply-engine)))))))))

(defun carriage-send-trace--around-simple (tag)
  "Return an :around advice that logs start/return/error with TAG."
  (lambda (orig &rest args)
    (if (not carriage-send-trace)
        (apply orig args)
      (let* ((id (cl-incf carriage-send-trace--send-seq))
             (t0 (float-time)))
        (carriage-send-trace--log "TRACE[%s] id=%s start args=%d" tag id (length args))
        ;; Watchdog is useful mainly for Send entrypoints.
        (when (string-match-p "\\`send" (downcase (format "%s" tag)))
          (carriage-send-trace--watchdog-arm id t0))
        (condition-case e
            (let* ((ret (apply orig args))
                   (ms (truncate (* 1000 (max 0.0 (- (float-time) t0))))))
              (carriage-send-trace--log "TRACE[%s] id=%s return %dms ret=%S" tag id ms ret)
              ret)
          (error
           (let ((ms (truncate (* 1000 (max 0.0 (- (float-time) t0))))))
             (carriage-send-trace--log "TRACE[%s] id=%s ERROR %dms %s"
                                       tag id ms (error-message-string e)))
           (signal (car e) (cdr e))))))))

(defun carriage-send-trace--around-context-collect-async (orig callback &rest rest)
  "Around-advice for `carriage-context-collect-async' to log callback timing."
  (if (not carriage-send-trace)
      (apply orig callback rest)
    (let* ((t0 (float-time))
           (cb callback))
      (carriage-send-trace--log "TRACE[context-collect-async] start")
      (apply orig
             (lambda (ctx)
               (let* ((ms (truncate (* 1000 (max 0.0 (- (float-time) t0)))))
                      (files (and (listp ctx) (plist-get ctx :files)))
                      (warns (and (listp ctx) (plist-get ctx :warnings)))
                      (st (and (listp ctx) (plist-get ctx :stats))))
                 (carriage-send-trace--log
                  "TRACE[context-collect-async] callback %dms files=%s warnings=%s bytes=%s included=%s skipped=%s"
                  ms
                  (and (listp files) (length files))
                  (and (listp warns) (length warns))
                  (and (listp st) (plist-get st :total-bytes))
                  (and (listp st) (plist-get st :included))
                  (and (listp st) (plist-get st :skipped))))
               (when (functionp cb)
                 (funcall cb ctx)))
             rest))))

(defvar carriage-send-trace--installed nil
  "Non-nil when send tracing advices were installed.")

(defun carriage-send-trace-install ()
  "Install tracing advices (idempotent)."
  (unless carriage-send-trace--installed
    (setq carriage-send-trace--installed t)

    ;; Send entrypoints (best-effort; depends on what is defined in this build).
    (dolist (fn '(carriage-send-buffer
                  carriage-send-region
                  carriage-send
                  carriage-send-at-point
                  carriage-send-last-iteration
                  carriage-send-last-message))
      (when (fboundp fn)
        (advice-add fn :around (carriage-send-trace--around-simple (format "send:%s" fn)))))

    ;; Context
    (when (fboundp 'carriage-context-collect)
      (advice-add 'carriage-context-collect
                  :around (carriage-send-trace--around-simple "context-collect")))
    (when (fboundp 'carriage-context-collect-async)
      (advice-add 'carriage-context-collect-async
                  :around #'carriage-send-trace--around-context-collect-async))
    (when (fboundp 'carriage-context-project-map-build)
      (advice-add 'carriage-context-project-map-build
                  :around (carriage-send-trace--around-simple "project-map-build")))
    (when (fboundp 'carriage-context-project-map-build-async)
      (advice-add 'carriage-context-project-map-build-async
                  :around (carriage-send-trace--around-simple "project-map-build-async")))

    ;; Transport dispatch (watchdog cancel point)
    (when (fboundp 'carriage-transport-dispatch)
      (advice-add
       'carriage-transport-dispatch
       :around
       (lambda (orig &rest args)
         (when carriage-send-trace
           (carriage-send-trace--log
            "TRACE[transport-dispatch] start args=%d backend=%S provider=%S"
            (length args)
            (and (boundp 'carriage-mode-backend) carriage-mode-backend)
            (and (boundp 'carriage-mode-provider) carriage-mode-provider)))
         ;; If we reached dispatch, cancel watchdog for this buffer.
         (carriage-send-trace--watchdog-cancel)
         (apply orig args))))))

(carriage-send-trace-install)

;;;###autoload
(defun carriage-toggle-send-trace ()
  "Toggle `carriage-send-trace' globally."
  (interactive)
  (setq carriage-send-trace (not carriage-send-trace))
  (carriage-send-trace--log "TRACE toggled: carriage-send-trace=%s" carriage-send-trace)
  (message "carriage-send-trace=%s" (if carriage-send-trace "on" "off")))
