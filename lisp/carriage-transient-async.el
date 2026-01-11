;;; carriage-transient-async.el --- Transient → async wrappers (instant close) -*- lexical-binding: t; -*-

;; Lightweight advice to ensure transient closes immediately and heavy
;; commands are scheduled on the next tick. Also sets early UI state and
;; starts preloader for instant feedback. No behavior change for non-transient
;; invocations.

(require 'subr-x)

(defvar carriage--transient-async--reentry nil
  "Internal guard to avoid reentry while scheduling async calls from advice.")

(defun carriage--in-transient-p ()
  "Return non-nil when a transient session is active."
  (and (featurep 'transient)
       (boundp 'transient--stack)
       (consp transient--stack)))

(defun carriage--transient-quit ()
  "Close current transient stack immediately (best-effort)."
  (when (featurep 'transient)
    (condition-case _e
        (if (fboundp 'transient-quit-all)
            (transient-quit-all)
          (when (fboundp 'transient-quit-one)
            (transient-quit-one)))
      (error nil))))

(defun carriage--transient-async-around (state)
  "Return an :around advice that closes transient and runs command async.
STATE is an UI state symbol like 'sending, 'apply or 'dry-run."
  (lambda (orig-fn &rest args)
    (if (or carriage--transient-async--reentry
            (not (carriage--in-transient-p)))
        ;; Normal path (outside transient): call immediately.
        (apply orig-fn args)
      (let* ((carriage--transient-async--reentry t)
             (prefix current-prefix-arg)
             ;; Pin buffer to avoid sending/inserting into a different Carriage buffer
             ;; if current-buffer changes between transient closing and timer firing.
             (srcbuf (current-buffer)))
        ;; 1) Close transient now
        (carriage--transient-quit)
        ;; 2) Early feedback: state + preloader (in the originating buffer)
        (when (buffer-live-p srcbuf)
          (with-current-buffer srcbuf
            (when (fboundp 'carriage-ui-set-state)
              (ignore-errors (carriage-ui-set-state state)))
            (when (and (eq state 'sending) (fboundp 'carriage--preloader-start))
              (ignore-errors (carriage--preloader-start)))))
        ;; Give redisplay a chance
        (sit-for 0)
        ;; 3) Schedule the actual call on the next tick (in the originating buffer)
        (run-at-time
         0 nil
         (lambda ()
           (let ((current-prefix-arg prefix))
             (when (buffer-live-p srcbuf)
               (with-current-buffer srcbuf
                 (condition-case e
                     (if (commandp orig-fn)
                         (call-interactively orig-fn)
                       (apply orig-fn args))
                   (error
                    (when (fboundp 'carriage-ui-set-state)
                      (ignore-errors (carriage-ui-set-state 'error)))
                    (when (require 'carriage-logging nil t)
                      (ignore-errors
                        (carriage-log "transient-async: %s" (error-message-string e))))))))))))
      ;; Return promptly to let transient disappear instantly
      nil)))

;; Install advices (best-effort) once carriage-mode is loaded.
(with-eval-after-load 'carriage-mode
  (ignore-errors (advice-add 'carriage-send-buffer :around (carriage--transient-async-around 'sending)))
  (ignore-errors (advice-add 'carriage-send-subtree :around (carriage--transient-async-around 'sending)))
  (ignore-errors (advice-add 'carriage-dry-run-at-point :around (carriage--transient-async-around 'dry-run)))
  (ignore-errors (advice-add 'carriage-apply-at-point-or-region :around (carriage--transient-async-around 'apply)))
  (ignore-errors (advice-add 'carriage-apply-last-iteration :around (carriage--transient-async-around 'apply))))

(provide 'carriage-transient-async)
;;; carriage-transient-async.el ends here
