;;; carriage-mode-enable-no-git-test.el --- Tests for opening org outside Git  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Ensure repo lisp/ is on load-path without using git/project discovery.
(let* ((this (file-name-directory (or load-file-name buffer-file-name)))
       (lisp (expand-file-name "../lisp" this)))
  (when (file-directory-p lisp)
    (add-to-list 'load-path lisp)))

(require 'carriage-global-mode)

(ert-deftest carriage-open-org-no-git-does-not-call-git-and-auto-enables ()
  "Opening an Org file outside Git must not call external git and must not hang.
If the document requests :CAR_MODE t, carriage-mode should auto-enable."
  (let* ((tmp (make-temp-file "carriage-open-no-git-" t))
         (file (expand-file-name "note.org" tmp))
         (content (concat
                   "#+title: Note\n"
                   "#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t)\n\n"
                   "* Test\n")))
    (with-temp-file file (insert content))
    (unwind-protect
        (cl-letf (((symbol-function 'carriage--call-git)
                   (lambda (&rest _args)
                     (ert-fail "carriage--call-git must not be called on file open outside repo"))))
          (carriage-global-mode 1)
          ;; Open file (this runs org-mode hooks and doc-state auto-enable path).
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  ;; Carriage should be enabled by doc-state auto-enable.
                  (should (bound-and-true-p carriage-mode)))
              (when (buffer-live-p buf)
                (kill-buffer buf)))))
      (ignore-errors (carriage-global-mode 0)))))

(provide 'carriage-mode-enable-no-git-test)
;;; carriage-mode-enable-no-git-test.el ends here
