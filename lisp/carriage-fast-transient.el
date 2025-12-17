;;; carriage-fast-transient.el --- Fast transient exit and early UI feedback  -*- lexical-binding: t; -*-

;; This module forces transient to close immediately on menu actions and
;; schedules heavy commands asynchronously. It also provides early UI feedback
;; (state/preloader) before heavy preparation starts.
;;
;; Loaded from carriage-transport.el (best-effort).

(require 'cl-lib)
(require 'subr-x)

(defun carriage--ft--in-transient-p ()
  "Return non-nil when called from an active transient."
  (and (featurep 'transient)
       (boundp 'transient--stack)
       transient--stack))

(defun carriage--ft--quit-transient-now ()
  "Close transient UI immediately (best-effort)."
  (when (featurep 'transient)
    (ignore-errors (transient-quit-one))))

(defun carriage--ft--early-ui (phase)
  "Set early UI PHASE and start preloader when available."
  (ignore-errors
    (when (fboundp 'carriage-ui-set-state)
      (carriage-ui-set-state phase))
    ;; Give redisplay a chance to show spinner/state
    (sit-for 0)))

(defun carriage--ft--around-defer (orig phase &rest args)
  "Common around-advice: quit transient, show PHASE, and run ORIG."
  (when (carriage--ft--in-transient-p)
    (carriage--ft--quit-transient-now)
    (carriage--ft--early-ui phase))
  (apply orig args))

;; Send commands: early 'sending
(defun carriage--ft-around-send-buffer (orig &rest args)
  (apply #'carriage--ft--around-defer orig 'sending args))

(defun carriage--ft-around-send-subtree (orig &rest args)
  (apply #'carriage--ft--around-defer orig 'sending args))

;; Apply/Dry-run commands: early 'apply / 'dry-run
(defun carriage--ft-around-apply-at-point-or-region (orig &rest args)
  (apply #'carriage--ft--around-defer orig 'apply args))

(defun carriage--ft-around-apply-last-iteration (orig &rest args)
  (apply #'carriage--ft--around-defer orig 'apply args))

(defun carriage--ft-around-dry-run-at-point (orig &rest args)
  (apply #'carriage--ft--around-defer orig 'dry-run args))

;; Install advices when the corresponding functions are available.
(with-eval-after-load 'carriage-mode
  ;; Guard against double-install (e.g., reloads), which otherwise can cause
  ;; send functions to run twice → duplicate inline iteration markers.
  (when (fboundp 'carriage-send-buffer)
    (unless (advice-member-p #'carriage--ft-around-send-buffer 'carriage-send-buffer)
      (advice-add 'carriage-send-buffer :around #'carriage--ft-around-send-buffer)))
  (when (fboundp 'carriage-send-subtree)
    (unless (advice-member-p #'carriage--ft-around-send-subtree 'carriage-send-subtree)
      (advice-add 'carriage-send-subtree :around #'carriage--ft-around-send-subtree)))
  (when (fboundp 'carriage-apply-at-point-or-region)
    (unless (advice-member-p #'carriage--ft-around-apply-at-point-or-region 'carriage-apply-at-point-or-region)
      (advice-add 'carriage-apply-at-point-or-region :around #'carriage--ft-around-apply-at-point-or-region)))
  (when (fboundp 'carriage-apply-last-iteration)
    (unless (advice-member-p #'carriage--ft-around-apply-last-iteration 'carriage-apply-last-iteration)
      (advice-add 'carriage-apply-last-iteration :around #'carriage--ft-around-apply-last-iteration)))
  (when (fboundp 'carriage-dry-run-at-point)
    (unless (advice-member-p #'carriage--ft-around-dry-run-at-point 'carriage-dry-run-at-point)
      (advice-add 'carriage-dry-run-at-point :around #'carriage--ft-around-dry-run-at-point)))

  ;; Idle prewarm to reduce cold-start latency for the first request.
  (run-at-time
   0.5 nil
   (lambda ()
     (ignore-errors (require 'carriage-intent-registry nil t))
     (ignore-errors (require 'carriage-op-sre nil t))
     (ignore-errors (require 'carriage-op-aibo nil t))
     (ignore-errors (require 'carriage-op-patch nil t))
     (ignore-errors (require 'carriage-op-file nil t)))))


(provide 'carriage-fast-transient)
;;; carriage-fast-transient.el ends here
