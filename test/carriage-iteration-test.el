;;; carriage-iteration-test.el --- ERT for single begin-iteration invariant -*- lexical-binding: t; -*-

(require 'ert)

(defvar carriage-iteration-test--count 0
  "Counter of carriage-begin-iteration invocations during test.")

(defun carriage-iteration-test--advice (&rest _args)
  (setq carriage-iteration-test--count (1+ carriage-iteration-test--count)))


(provide 'carriage-iteration-test)
;;; carriage-iteration-test.el ends here
