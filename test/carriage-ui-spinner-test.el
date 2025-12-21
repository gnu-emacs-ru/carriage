;;; carriage-ui-spinner-test.el --- Spinner tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)

(ert-deftest carriage-ui-spinner-not-in-batch ()
  "In batch (noninteractive=t), spinner must not start."
  (with-temp-buffer
    (let ((noninteractive t))
      (org-mode)
      (carriage-mode 1)
      (carriage-ui-set-state 'sending)
      (should (or (null (bound-and-true-p carriage--ui-spinner-timer))
                  (not (timerp carriage--ui-spinner-timer))))
      (carriage-mode -1))))

(provide 'carriage-ui-spinner-test)
;;; carriage-ui-spinner-test.el ends here
