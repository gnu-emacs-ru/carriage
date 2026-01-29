;;; typedblocks-tests.el --- ERT tests for Typed Blocks v1  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'carriage-typedblocks)

(defun tb--with-text (text &rest body)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (org-mode)
    (apply #'progn body)))

(ert-deftest typedblocks/extract-order-and-plain ()
  "Extractor should return segments in order and capture plain outside blocks."
  (tb--with-text
   "#+begin_task\nGoal\n#+end_task\n\nPlain here\n\n#+begin_analysis\nA\n#+end_analysis\n"
   (let* ((segs (carriage-typedblocks--extract (current-buffer)))
          (types (mapcar (lambda (s) (plist-get s :type)) segs)))
     (should (equal types '(task plain analysis)))
     (should (string-match-p "Goal" (plist-get (car segs) :text))))))

(ert-deftest typedblocks/payload-defaults-exclude-notes-and-patch ()
  "Default payload excludes notes and patch bodies."
  (tb--with-text
   "#+begin_notes\nn\n#+end_notes\n#+begin_patch\nBODY\n#+end_patch\n#+begin_plan\nP\n#+end_plan\n"
   (let* ((carriage-typedblocks-include-commands t)
          (carriage-mode-include-plain-text-context nil)
          (payload (carriage-typedblocks-build-payload (current-buffer))))
     (should (string-match-p "In plan:\nP" payload))
     (should-not (string-match-p "In notes:" payload))
     (should-not (string-match-p "BODY" payload)))))

(ert-deftest typedblocks/payload-commands-flag ()
  "Commands should be included only when the flag is enabled."
  (tb--with-text
   "#+begin_commands\nrun me\n#+end_commands\n"
   (let ((carriage-typedblocks-include-commands nil))
     (should-not (string-match-p "run me"
                                 (carriage-typedblocks-build-payload (current-buffer)))))
   (let ((carriage-typedblocks-include-commands t))
     (should (string-match-p "In commands:\nrun me"
                             (carriage-typedblocks-build-payload (current-buffer))))))

(ert-deftest typedblocks/payload-plain-flag ()
  "Plain text is included only when the plain flag is enabled."
  (tb--with-text
   "hello\n#+begin_task\ng\n#+end_task\nworld\n"
   (let ((carriage-mode-include-plain-text-context nil))
     (should-not (string-match-p "In plain:" (carriage-typedblocks-build-payload (current-buffer)))))
   (let ((carriage-mode-include-plain-text-context t))
     (should (string-match-p "In plain:\nhello" (carriage-typedblocks-build-payload (current-buffer))))))

(ert-deftest typedblocks/payload-question-answer ()
  "Question/answer blocks are included by default."
  (tb--with-text
   "#+begin_question\nQ?\n#+end_question\n#+begin_answer\nA!\n#+end_answer\n"
   (let ((payload (carriage-typedblocks-build-payload (current-buffer))))
     (should (string-match-p "In question:\nQ\\?" payload))
     (should (string-match-p "In answer:\nA!" payload))))

(provide 'typedblocks-tests)
;;; typedblocks-tests.el ends here
