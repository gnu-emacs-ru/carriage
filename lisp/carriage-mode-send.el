;;; carriage-mode-send.el --- Send commands and streaming for Carriage mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Send buffer/subtree commands and streaming logic.
;; Extracted from carriage-mode.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-log "carriage-logging" (&rest args))
(declare-function carriage-ui-set-state "carriage-ui" (state &optional tooltip))
(declare-function carriage-stream-reset "carriage-stream" (origin-marker))
(declare-function carriage-clear-abort-handler "carriage-mode" ())

(defvar-local carriage--send-in-flight nil)
(defvar-local carriage--send-dispatch-scheduled nil)
(defvar-local carriage--send-generation 0)
(defvar-local carriage--active-send-generation 0)
(defvar-local carriage--current-send-entry-id nil)
(defvar-local carriage-mode--send-prepare-reentry nil)
(defvar-local carriage-mode--send-prepare-token nil)
(defvar-local carriage--stream-origin-marker nil)

;;; Helper: calculate origin marker for send-buffer
(defun carriage--send-buffer--calc-origin-marker ()
  "Compute and return stream origin marker for carriage-send-buffer."
  (copy-marker
   (save-excursion
     (goto-char (point-min))
     (when (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
       (beginning-of-line)
       (when (looking-at "\n") (delete-char 1))))
   t))

;;; Helper: prepare stream state & preloader for send-buffer
(defun carriage--send-buffer--prepare-stream (origin-marker)
  "Prepare carriage streaming environment at ORIGIN-MARKER for send-buffer."
  (carriage-stream-reset origin-marker)
  (when (fboundp 'carriage-begin-iteration)
    (ignore-errors (carriage-begin-iteration)))
  (when (fboundp 'carriage-insert-inline-fingerprint-now)
    (ignore-errors (carriage-insert-inline-fingerprint-now)))
  (when (fboundp 'carriage--preloader-start)
    (ignore-errors (carriage--preloader-start))))

(defun carriage--send-generation-stale-p (generation srcbuf)
  "Return non-nil when GENERATION is stale for SRCBUF."
  (with-current-buffer srcbuf
    (or (null generation)
        (/= generation carriage--active-send-generation))))

(defun carriage--log-send-entry (entry-sym)
  "Log send ENTRY-SYM as the current send entry."
  (setq carriage--current-send-entry-id (symbol-name entry-sym)))

;;; Helper: deferred dispatch logic for send-buffer
(defun carriage--send-buffer--deferred-dispatch (srcbuf origin-marker prefix generation)
  "Deferred LLM send prep/dispatch for carriage-send-buffer."
  (let ((current-prefix-arg prefix))
    (when (buffer-live-p srcbuf)
      (with-current-buffer srcbuf
        (cond
         ((carriage--send-generation-stale-p generation srcbuf)
          (carriage-log "send-buffer: deferred tick dropped as stale entry=%s gen=%s active-gen=%s"
                        (or carriage--current-send-entry-id "-")
                        generation
                        carriage--active-send-generation)
          (setq carriage--send-in-flight nil)
          (setq carriage--send-dispatch-scheduled nil))
         (carriage--send-in-flight
          (carriage-log "send-buffer: deferred tick dropped (already in-flight)")
          (setq carriage--send-dispatch-scheduled nil))
         (t
          (setq carriage--send-in-flight t)
          (carriage--send-prepare-and-dispatch
           'buffer
           srcbuf
           carriage-mode-backend
           carriage-mode-model
           carriage-mode-intent
           carriage-mode-suite
           origin-marker)))))))

;;;###autoload
(defun carriage-send-buffer ()
  "Send entire buffer to LLM according to current Intent/Suite."
  (interactive)
  (carriage--log-send-entry 'carriage-send-buffer)
  (carriage-log "send-buffer: invoke entry=%s buf=%s reentry=%s in-flight=%s scheduled=%s"
                (or carriage--current-send-entry-id "-")
                (buffer-name (current-buffer))
                (if carriage-mode--send-prepare-reentry "t" "nil")
                (if carriage--send-in-flight "t" "nil")
                (if carriage--send-dispatch-scheduled "t" "nil"))
  (when (bound-and-true-p carriage-mode--send-prepare-reentry)
    (setq carriage-mode--send-prepare-reentry nil))
  (if (or carriage--send-in-flight carriage--send-dispatch-scheduled)
      (progn
        (carriage-log "send-buffer: dropped duplicate invocation buf=%s in-flight=%s scheduled=%s"
                      (buffer-name (current-buffer))
                      (if carriage--send-in-flight "t" "nil")
                      (if carriage--send-dispatch-scheduled "t" "nil"))
        (unless (bound-and-true-p noninteractive)
          (message "Carriage: запрос уже выполняется")))
    (setq carriage--send-generation (1+ (or carriage--send-generation 0)))
    (setq carriage--active-send-generation carriage--send-generation)
    (setq carriage--apply-entry-id nil)
    (setq carriage--apply-entry-log-count 0)
    (setq carriage--apply-response-fingerprint nil)
    (setq carriage--apply-claim-keys nil)
    (setq carriage--send-dispatch-scheduled t)
    (carriage-ui-set-state 'sending)
    (when (fboundp 'carriage-ui-apply-reset)
      (ignore-errors (carriage-ui-apply-reset)))
    (sit-for 0)
    (let ((srcbuf (current-buffer))
          (prefix current-prefix-arg)
          (generation carriage--active-send-generation)
          (origin-marker (carriage--send-buffer--calc-origin-marker)))
      (carriage--send-buffer--prepare-stream origin-marker)
      (run-at-time
       0 nil
       (lambda ()
         (when (buffer-live-p srcbuf)
           (with-current-buffer srcbuf
             (cond
              ((carriage--send-generation-stale-p generation srcbuf)
               (carriage-log "send-buffer: deferred tick dropped before start entry=%s gen=%s active-gen=%s"
                             (or carriage--current-send-entry-id "-")
                             generation
                             carriage--active-send-generation)
               (setq carriage--send-dispatch-scheduled nil))
              (carriage--send-in-flight
               (carriage-log "send-buffer: deferred tick dropped (already in-flight)")
               (setq carriage--send-dispatch-scheduled nil))
              (t
               (setq carriage--send-dispatch-scheduled nil)
               (setq carriage--send-in-flight t)
               (carriage--send-buffer--deferred-dispatch srcbuf origin-marker prefix generation)))))))))))

;;; Helper: calculate origin marker for send-subtree
(defun carriage--send-subtree--calc-origin-marker ()
  "Compute and return stream origin marker for carriage-send-subtree."
  (copy-marker
   (progn
     (when (or (not (bolp))
               (save-excursion
                 (beginning-of-line)
                 (re-search-forward "[^ \t]" (line-end-position) t)))
       (end-of-line)
       (insert "\n"))
     (point))
   t))

;;; Helper: prepare stream state for send-subtree
(defun carriage--send-subtree--prepare-stream (origin-marker)
  "Prepare carriage streaming environment at ORIGIN-MARKER for send-subtree."
  (carriage-stream-reset origin-marker)
  (when (fboundp 'carriage-begin-iteration)
    (ignore-errors (carriage-begin-iteration)))
  (when (fboundp 'carriage-insert-inline-fingerprint-now)
    (ignore-errors (carriage-insert-inline-fingerprint-now)))
  (when (fboundp 'carriage--preloader-start)
    (ignore-errors (carriage--preloader-start))))

;;; Helper: deferred dispatch for send-subtree
(defun carriage--send-subtree--deferred-dispatch (srcbuf origin-marker prefix generation)
  "Deferred LLM send prep/dispatch for carriage-send-subtree."
  (let ((current-prefix-arg prefix))
    (when (buffer-live-p srcbuf)
      (with-current-buffer srcbuf
        (if (carriage--send-generation-stale-p generation srcbuf)
            (progn
              (carriage-log "send-subtree: deferred tick dropped as stale entry=%s gen=%s active-gen=%s"
                            (or carriage--current-send-entry-id "-")
                            generation
                            carriage--active-send-generation)
              (setq carriage--send-in-flight nil)
              (setq carriage--send-dispatch-scheduled nil))
          (progn
            (carriage-log "send-subtree: deferred tick enter entry=%s gen=%s buf=%s in-flight=%s scheduled=%s backend=%s model=%s"
                          (or carriage--current-send-entry-id "-")
                          generation
                          (buffer-name srcbuf)
                          (if carriage--send-in-flight "t" "nil")
                          (if carriage--send-dispatch-scheduled "t" "nil")
                          carriage-mode-backend carriage-mode-model)
            (condition-case err
                (carriage--send-prepare-and-dispatch
                 'subtree
                 srcbuf
                 carriage-mode-backend
                 carriage-mode-model
                 carriage-mode-intent
                 carriage-mode-suite
                 (or (and (markerp carriage--stream-origin-marker)
                          (buffer-live-p (marker-buffer carriage--stream-origin-marker))
                          carriage--stream-origin-marker)
                     origin-marker))
              (quit
               (carriage-log "send-subtree: deferred tick quit; aborting")
               (setq carriage--send-in-flight nil)
               (setq carriage--send-dispatch-scheduled nil)
               (setq carriage-mode--send-prepare-token nil)
               (ignore-errors (carriage-clear-abort-handler))
               (ignore-errors (carriage--preloader-stop))
               (ignore-errors (carriage-ui-set-state 'idle)))
              (error
               (carriage-log "send-subtree: deferred tick ERROR: %s" (error-message-string err))
               (setq carriage--send-in-flight nil)
               (setq carriage--send-dispatch-scheduled nil)
               (setq carriage-mode--send-prepare-token nil)
               (ignore-errors (carriage-clear-abort-handler))
               (ignore-errors (carriage--preloader-stop))
               (ignore-errors (carriage-ui-set-state 'error))
               (unless (bound-and-true-p noninteractive)
                 (message "Carriage: send-subtree crashed: %s" (error-message-string err)))))))))))

;;;###autoload
(defun carriage-send-subtree ()
  "Send current org subtree to LLM according to current Intent/Suite."
  (interactive)
  (carriage--log-send-entry 'carriage-send-subtree)
  (carriage-log "send-subtree: invoke entry=%s buf=%s reentry=%s in-flight=%s scheduled=%s"
                (or carriage--current-send-entry-id "-")
                (buffer-name (current-buffer))
                (if carriage-mode--send-prepare-reentry "t" "nil")
                (if carriage--send-in-flight "t" "nil")
                (if carriage--send-dispatch-scheduled "t" "nil"))
  (when (bound-and-true-p carriage-mode--send-prepare-reentry)
    (setq carriage-mode--send-prepare-reentry nil))
  (if (or carriage--send-in-flight carriage--send-dispatch-scheduled)
      (progn
        (carriage-log "send-subtree: dropped duplicate invocation")
        (unless (bound-and-true-p noninteractive)
          (message "Carriage: запрос уже выполняется")))
    (setq carriage--send-generation (1+ (or carriage--send-generation 0)))
    (setq carriage--active-send-generation carriage--send-generation)
    (setq carriage--send-dispatch-scheduled t)
    (carriage-ui-set-state 'sending)
    (sit-for 0)
    (let ((srcbuf (current-buffer))
          (prefix current-prefix-arg)
          (generation carriage--active-send-generation)
          (origin-marker (carriage--send-subtree--calc-origin-marker)))
      (carriage--send-subtree--prepare-stream origin-marker)
      (run-at-time
       0 nil
       (lambda ()
         (when (buffer-live-p srcbuf)
           (with-current-buffer srcbuf
             (if (carriage--send-generation-stale-p generation srcbuf)
                 (progn
                   (carriage-log "send-subtree: deferred tick dropped as stale")
                   (setq carriage--send-dispatch-scheduled nil))
               (setq carriage--send-dispatch-scheduled nil)
               (setq carriage--send-in-flight t)
               (carriage--send-subtree--deferred-dispatch srcbuf origin-marker prefix generation)))))))))

(provide 'carriage-mode-send)
;;; carriage-mode-send.el ends here
