;;; carriage-transport-gptel-terminal-test.el --- GPTel terminal callback tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'carriage)
(load-file (expand-file-name "../../lisp/carriage-transport-gptel.el"
                             (file-name-directory load-file-name)))

(ert-deftest carriage-transport-gptel-callback-terminal-info-finalizes ()
  (let ((called nil)
        (inserted nil))
    (cl-letf (((symbol-function 'carriage-transport-gptel--callback-finalize-simple)
                (lambda (&rest args)
                  (setq called args)
                  t))
              ((symbol-function 'carriage-transport-gptel--callback-handle-text)
               (lambda (_origin-buffer resp first-stream)
                 (push resp inserted)
                  t)))
      (should
       (equal
         (carriage-transport-gptel--callback-dispatch
          (current-buffer) "id-1" 'gptel "partial" '(:done t) nil)
         '(:finished t :first-stream t :last-resp "partial" :last-info (:done t))))
      (should (equal inserted '("partial")))
      (should called))))

(ert-deftest carriage-transport-gptel-callback-terminal-answer-finalizes ()
  (let ((called nil)
        (inserted nil))
    (cl-letf (((symbol-function 'carriage-transport-gptel--callback-finalize-simple)
                (lambda (&rest args)
                  (setq called args)
                  t))
              ((symbol-function 'carriage-transport-gptel--callback-handle-text)
               (lambda (_origin-buffer resp first-stream)
                 (push resp inserted)
                  t)))
      (should
       (equal
         (carriage-transport-gptel--callback-dispatch
          (current-buffer) "id-2" 'gptel "#+begin_answer\nDone\n#+end_answer" nil nil)
         '(:finished t :first-stream t :last-resp "#+begin_answer\nDone\n#+end_answer" :last-info nil)))
      (should (equal inserted '("#+begin_answer\nDone\n#+end_answer")))
      (should called))))

;;; carriage-transport-gptel-terminal-test.el ends here
