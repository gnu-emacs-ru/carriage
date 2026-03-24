;;; carriage-transport-test.el --- Transport integration tests (M4) -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(ert-deftest carriage-transport-abort-handler-wires ()
  "Abort handler registers and is invoked by carriage-abort-current; then cleared."
  (with-temp-buffer
    (let ((noninteractive nil)
          (called nil))
      (org-mode)
      (carriage-mode 1)
      (unwind-protect
          (progn
            ;; Begin with abort handler
            (carriage-transport-begin (lambda () (setq called 'yes)))
            ;; Abort should invoke handler and clear it
            (setq called nil)
            (carriage-abort-current)
            (should (eq called 'yes))
            ;; Second abort should not invoke cleared handler
            (setq called 'no)
            (carriage-abort-current)
            (should (eq called 'no)))
        (carriage-mode -1)))))

(ert-deftest carriage-transport-state-transitions ()
  "Begin→sending, streaming→streaming, complete→idle/error; spinner stops on completion."
  (with-temp-buffer
    (let ((noninteractive nil))
      (org-mode)
      (carriage-mode 1)
      (unwind-protect
          (progn
            ;; Begin → sending
            (carriage-transport-begin)
            (should (boundp 'carriage--ui-state))
            (should (eq (symbol-value 'carriage--ui-state) 'sending))
            ;; streaming → streaming
            (carriage-transport-streaming)
            (should (eq (symbol-value 'carriage--ui-state) 'streaming))
            ;; complete (ok) → idle and spinner stopped
            (carriage-transport-complete nil)
            (should (eq (symbol-value 'carriage--ui-state) 'idle))
            (should (or (null (bound-and-true-p carriage--ui-spinner-timer))
                        (not (timerp carriage--ui-spinner-timer))))
            ;; error path
            (carriage-transport-begin)
            (carriage-transport-complete t)
            (should (eq (symbol-value 'carriage--ui-state) 'error)))
        (carriage-mode -1)))))

(ert-deftest carriage-transport-complete-accepts-buffer-arg ()
  (let ((noninteractive nil)
        (origin (generate-new-buffer " *carriage-origin*"))
        (other  (generate-new-buffer " *carriage-other*")))
    (unwind-protect
        (progn
          (with-current-buffer origin
            (org-mode)
            (carriage-mode 1)
            (setq-local carriage--ui-state 'sending)
            (setq-local carriage-transport--request-id "rid-test-origin"))
          (with-current-buffer other
            (org-mode)
            (carriage-mode 1)
            (setq-local carriage--ui-state 'sending)
            (setq-local carriage--active-send-generation 7)
            (setq-local carriage--apply-entry-id "carriage-send-buffer-7")
            (setq-local carriage-transport--request-id "rid-test-other"))
          ;; Complete OTHER while current buffer is ORIGIN.
          (with-current-buffer origin
            (carriage-transport-complete t other))
          (should (eq (with-current-buffer origin carriage--ui-state) 'sending))
          (should (equal (with-current-buffer origin carriage-transport--request-id)
                         "rid-test-origin"))
          (with-current-buffer other
            (should (eq carriage--ui-state 'error))
            (should (null carriage-transport--request-id))
            (should (null carriage--active-send-generation))
            (should (null carriage--apply-entry-id)))))
    (when (buffer-live-p origin) (kill-buffer origin))
    (when (buffer-live-p other)  (kill-buffer other))))

(provide 'carriage-transport-test)
;;; carriage-transport-test.el ends here
