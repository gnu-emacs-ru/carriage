;;; carriage-doc-state-perf.el --- Gating and debouncing for doc-state fold overlays -*- lexical-binding: t; -*-

;; This module installs lightweight performance guards around doc-state fold refresh:
;; - Run only when buffer is visible and selected.
;; - Coalesce bursts via one pending timer per buffer (debounce).
;; - Throttle heavy refreshes to avoid repeated full rescans.
;; - Cache tooltip strings per arguments to avoid repeated string-join/format storms.
;;
;; It does NOT change semantics of overlays; it defers and batches heavy work.

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-doc-state-perf nil
  "Performance guards for carriage-doc-state fold overlays."
  :group 'carriage)

(defcustom carriage-doc-state-perf-enabled t
  "When non-nil, enable gating/throttling of doc-state fold refresh."
  :type 'boolean :group 'carriage-doc-state-perf)

(defcustom carriage-doc-state-perf-debounce-ms 300
  "Debounce (milliseconds) for coalescing doc-state refresh bursts."
  :type 'integer :group 'carriage-doc-state-perf)

(defcustom carriage-doc-state-perf-min-interval-ms 700
  "Minimum interval (milliseconds) between heavy doc-state refresh runs per buffer."
  :type 'integer :group 'carriage-doc-state-perf)

(defcustom carriage-doc-state-perf-only-selected-window t
  "When non-nil, run refresh only when current buffer is shown in the selected window."
  :type 'boolean :group 'carriage-doc-state-perf)

(defvar-local carriage-doc-state-perf--pending-timer nil)
(defvar-local carriage-doc-state-perf--last-run 0.0)
(defvar-local carriage-doc-state-perf--tooltip-cache nil)

(defun carriage-doc-state-perf--visible-selected-p ()
  "Return non-nil when current buffer is visible (and selected when policy requires)."
  (let* ((w (get-buffer-window (current-buffer) t)))
    (and (window-live-p w)
         (or (not carriage-doc-state-perf-only-selected-window)
             (eq w (selected-window))))))

(defun carriage-doc-state-perf--schedule-refresh (orig args)
  "Schedule a debounced/throttled call to ORIG with ARGS in this buffer."
  (let* ((buf (current-buffer))
         (now (float-time))
         (min-int (max 0.0 (/ (float (or carriage-doc-state-perf-min-interval-ms 700)) 1000.0)))
         (deb (max 0.01 (/ (float (or carriage-doc-state-perf-debounce-ms 300)) 1000.0)))
         (age (- now (or carriage-doc-state-perf--last-run 0.0))))
    (cond
     ;; Throttle: too soon since last heavy run → just ensure one pending timer exists.
     ((and (> min-int 0.0) (< age min-int))
      (unless (timerp carriage-doc-state-perf--pending-timer)
        (setq carriage-doc-state-perf--pending-timer
              (run-at-time
               deb nil
               (lambda (b)
                 (when (buffer-live-p b)
                   (with-current-buffer b
                     (setq carriage-doc-state-perf--pending-timer nil)
                     (when (and carriage-doc-state-perf-enabled
                                (carriage-doc-state-perf--visible-selected-p))
                       (condition-case _
                           (progn
                             (setq carriage-doc-state-perf--last-run (float-time))
                             (apply orig args))
                         (error
                          ;; Never break the UI on errors here; just skip.
                          nil)))))))
              ))
      ;; No throttle: run soon, but still debounce to coalesce bursts.
      (t
       (unless (timerp carriage-doc-state-perf--pending-timer)
         (setq carriage-doc-state-perf--pending-timer
               (run-at-time
                deb nil
                (lambda (b)
                  (when (buffer-live-p b)
                    (with-current-buffer b
                      (setq carriage-doc-state-perf--pending-timer nil)
                      (when (and carriage-doc-state-perf-enabled
                                 (carriage-doc-state-perf--visible-selected-p))
                        (condition-case _
                            (progn
                              (setq carriage-doc-state-perf--last-run (float-time))
                              (apply orig args))
                          (error nil))))))))))))))

(defun carriage-doc-state-perf--advice-refresh (orig &rest args)
  "Around advice for `carriage-doc-state--fold--refresh-overlays' to gate/aggregate work."
  (if (not carriage-doc-state-perf-enabled)
      (apply orig args)
    (if (carriage-doc-state-perf--visible-selected-p)
        (progn
          ;; Never run heavy refresh synchronously; always debounce/throttle.
          (carriage-doc-state-perf--schedule-refresh orig args)
          ;; Skip immediate work.
          nil)
      ;; Not visible/selected → skip entirely.
      nil)))

(defun carriage-doc-state-perf--tooltip-key (args)
  "Return a stable key for ARGS to cache tooltip."
  (condition-case _e
      (prin1-to-string args)
    (error (format "%S" args))))

(defun carriage-doc-state-perf--advice-tooltip (orig &rest args)
  "Around advice for `carriage-doc-state--fold--tooltip' to reuse cached strings."
  (if (not carriage-doc-state-perf-enabled)
      (apply orig args)
    (let* ((key (carriage-doc-state-perf--tooltip-key args)))
      (unless (hash-table-p carriage-doc-state-perf--tooltip-cache)
        (setq carriage-doc-state-perf--tooltip-cache (make-hash-table :test 'equal)))
      (or (gethash key carriage-doc-state-perf--tooltip-cache)
          (let ((val (condition-case _ (apply orig args) (error nil))))
            (when (stringp val)
              (puthash key val carriage-doc-state-perf--tooltip-cache))
            val)))))

(defun carriage-doc-state-perf--install ()
  "Install advices when doc-state is available."
  (when (fboundp 'carriage-doc-state--fold--refresh-overlays)
    (advice-add 'carriage-doc-state--fold--refresh-overlays :around
                #'carriage-doc-state-perf--advice-refresh))
  (when (fboundp 'carriage-doc-state--fold--tooltip)
    (advice-add 'carriage-doc-state--fold--tooltip :around
                #'carriage-doc-state-perf--advice-tooltip)))

(with-eval-after-load 'carriage-doc-state
  (carriage-doc-state-perf--install))

;; If doc-state was loaded before us, try to install immediately too.
(ignore-errors (carriage-doc-state-perf--install))

(provide 'carriage-doc-state-perf)
;;; carriage-doc-state-perf.el ends here
