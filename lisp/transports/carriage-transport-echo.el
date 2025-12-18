;;; carriage-transport-echo.el --- Echo transport (dev)  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: transport, echo
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/llm-transport-v2.org
;;   spec/tool-contracts-v2.org
;;   spec/logging-v2.org
;;   spec/observability-v2.org
;;   spec/golden-fixtures-v2.org
;;
;;; Commentary:
;; Simple echo transport for testing streaming UI and traffic.
;;
;;; Code:

;; A tiny reference adapter that simulates streaming without external deps.
;; - Streams chunks of a derived prompt to *carriage-traffic*.
;; - Drives UI states via carriage-transport-begin/streaming/complete.
;; - Registers an abort handler to cancel the stream timer.
;; - Does NOT call carriage-accept-llm-response (no edits), purely for UX/debug.

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-ui)
(require 'carriage-transport)
(require 'carriage-errors)
;; Avoid hard dependency on carriage-mode to break cycles at load time.
(declare-function carriage-register-abort-handler "carriage-mode" (fn))
(declare-function carriage-insert-stream-chunk "carriage-mode" (string &optional type))
(declare-function carriage-stream-finalize "carriage-mode" (&optional errorp mark-last-iteration))

(defgroup carriage-transport-echo nil
  "Echo transport adapter (development fallback)."
  :group 'carriage)

(defcustom carriage-transport-echo-chunk-ms 120
  "Interval between streamed chunks for echo transport, in ms."
  :type 'integer :group 'carriage-transport-echo)

(defcustom carriage-transport-echo-chunk-size 48
  "Size of each streamed chunk (characters)."
  :type 'integer :group 'carriage-transport-echo)

(defun carriage--echo--prompt (source buffer mode)
  "Build dev prompt string from BUFFER given SOURCE and MODE.
Strips any #+begin_carriage … #+end_carriage blocks and per-send fingerprint lines."
  (with-current-buffer buffer
    (let* ((raw
            (pcase source
              ('subtree
               (if (eq mode 'org-mode)
                   (save-excursion
                     (require 'org)
                     (ignore-errors (org-back-to-heading t))
                     (let ((beg (save-excursion (org-back-to-heading t) (point)))
                           (end (save-excursion (org-end-of-subtree t t) (point))))
                       (buffer-substring-no-properties beg end)))
                 (buffer-substring-no-properties (point-min) (point-max))))
              (_ (buffer-substring-no-properties (point-min) (point-max))))))
      (with-temp-buffer
        (insert (or raw ""))
        (goto-char (point-min))
        (let ((case-fold-search t))
          ;; Legacy/state blocks
          (while (re-search-forward "^[ \t]*#\\+begin_carriage\\b" nil t)
            (let ((beg (match-beginning 0)))
              (if (re-search-forward "^[ \t]*#\\+end_carriage\\b" nil t)
                  (let ((end (line-end-position)))
                    (delete-region beg end)
                    (when (looking-at "\n") (delete-char 1)))
                (delete-region beg (point-max)))))
          ;; Per-send fingerprint line(s)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT\\b.*$" nil t)
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position))))
            (goto-char (line-beginning-position))))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun carriage--echo--chunk-string (s n)
  "Return a list of chunks splitting S into pieces of at most N chars."
  (let* ((len (length s))
         (i 0)
         (acc '()))
    (while (< i len)
      (let* ((j (min len (+ i (max 1 n))))
             (chunk (substring s i j)))
        (push chunk acc)
        (setq i j)))
    (nreverse acc)))

(defun carriage-transport-echo-dispatch (&rest args)
  "Dispatch Carriage request via echo backend when :backend is 'echo.

ARGS is a plist with keys like :backend, :model, :source, :buffer, :mode.

Simulates streaming by emitting chunks to *carriage-traffic* on a timer.
Does not modify the Org buffer and does not call 'carriage-transport-begin'."
  (let* ((backend (plist-get args :backend))
         (model   (plist-get args :model))
         (source  (or (plist-get args :source) 'buffer))
         (buffer  (or (plist-get args :buffer) (current-buffer)))
         (mode    (or (plist-get args :mode)
                      (buffer-local-value 'major-mode buffer))))
    (unless (eq (if (symbolp backend) backend (intern (format "%s" backend))) 'echo)
      (carriage-log "Transport[echo]: backend mismatch (%s), dropping" backend)
      ;; Сигнализируем LLM_E_BACKEND согласно spec/errors-v2.org, затем завершаем транспорт
      (condition-case _
          (signal (carriage-error-symbol 'LLM_E_BACKEND)
                  (list (format "Unknown transport backend: %s" backend)))
        (error nil))
      (carriage-transport-complete t)
      (user-error "No transport adapter for backend: %s" backend))
    (with-current-buffer buffer
      (let* ((raw (carriage--echo--prompt source buffer (intern (format "%s" mode))))
             (trimmed (string-trim raw))
             (payload (if (string-empty-p trimmed)
                          (format "Echo backend active. Set backend to gptel for real LLM. model=%s" model)
                        (format "ECHO: %s" trimmed)))
             (chunks (carriage--echo--chunk-string payload carriage-transport-echo-chunk-size))
             (first t)
             (timer nil)
             ;; Response accumulators for summary
             (carriage--resp-head "") (carriage--resp-tail "")
             (carriage--resp-bytes 0)
             (carriage--resp-head-limit (or (and (boundp 'carriage-traffic-summary-head-bytes)
                                                 carriage-traffic-summary-head-bytes) 4096))
             (carriage--resp-tail-limit (or (and (boundp 'carriage-traffic-summary-tail-bytes)
                                                 carriage-traffic-summary-tail-bytes) 4096)))
        ;; Install abort handler that cancels the streaming timer
        (carriage-register-abort-handler
         (lambda ()
           (when (and timer (timerp timer))
             (cancel-timer timer))
           (setq timer nil)
           (carriage-traffic-log 'in "echo: aborted")
           (with-current-buffer buffer
             (carriage-stream-finalize t nil))
           (carriage-transport-complete t buffer)))
        ;; Structured request
        (carriage-traffic-log-request buffer
                                      :backend 'echo
                                      :model model
                                      :prompt payload
                                      :system nil
                                      :context nil)
        ;; Log request (legacy line)
        (carriage-traffic-log 'out "echo request: source=%s model=%s bytes=%d"
                              source model (length payload))
        ;; Drive streaming via timer
        (let* ((interval (/ (max 1 carriage-transport-echo-chunk-ms) 1000.0))
               (rest chunks))
          (setq timer
                (run-at-time
                 0 interval
                 (lambda ()
                   (condition-case e
                       (progn
                         (if (null rest)
                             (progn
                               (carriage-traffic-log 'in "echo: done")
                               ;; Structured response summary
                               (carriage-traffic-log-response-summary
                                buffer
                                (concat carriage--resp-head
                                        (when (> carriage--resp-bytes
                                                 (+ (string-bytes carriage--resp-head)
                                                    (string-bytes carriage--resp-tail)))
                                          "…")
                                        carriage--resp-tail))
                               (with-current-buffer buffer
                                 (carriage-stream-finalize nil t))
                               (carriage-transport-complete nil)
                               (when (timerp timer)
                                 (cancel-timer timer))
                               (setq timer nil))
                           (progn
                             (when first
                               (setq first nil)
                               (carriage-transport-streaming buffer))
                             (let ((chunk (pop rest)))
                               (with-current-buffer buffer
                                 (carriage-insert-stream-chunk chunk 'text))
                               ;; Accumulate head/tail
                               (setq carriage--resp-bytes (+ carriage--resp-bytes (string-bytes chunk)))
                               (when (< (length carriage--resp-head) carriage--resp-head-limit)
                                 (let* ((need (max 0 (- carriage--resp-head-limit (length carriage--resp-head))))
                                        (take (min need (length chunk))))
                                   (setq carriage--resp-head
                                         (concat carriage--resp-head (substring chunk 0 take)))
                                   (setq chunk (substring chunk take))))
                               (when (> (length chunk) 0)
                                 (let* ((concatd (concat carriage--resp-tail chunk))
                                        (len (length concatd)))
                                   (setq carriage--resp-tail
                                         (if (<= len carriage--resp-tail-limit)
                                             concatd
                                           (substring concatd (- len carriage--resp-tail-limit) len)))))
                               (carriage-traffic-log 'in "%s" chunk)))))
                     (error
                      (carriage-log "Echo stream error: %s" (error-message-string e))
                      (carriage-transport-complete t)
                      (when (timerp timer) (cancel-timer timer))
                      (setq timer nil)))))))))))
(provide 'carriage-transport-echo)
;;; carriage-transport-echo.el ends here
