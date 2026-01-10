;;; carriage-utils-project-root-no-git-test.el --- Tests for project-root outside Git  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Ensure repo lisp/ is on load-path without using git/project discovery.
(let* ((this (file-name-directory (or load-file-name buffer-file-name)))
       (lisp (expand-file-name "../lisp" this)))
  (when (file-directory-p lisp)
    (add-to-list 'load-path lisp)))

(require 'carriage-utils)

(ert-deftest carriage-utils-project-root-no-git-does-not-call-git ()
  "Outside a Git repo, `carriage-project-root' must NOT call external git."
  (let ((tmp (make-temp-file "carriage-no-git-" t)))
    (cl-letf (((symbol-function 'carriage--call-git)
               (lambda (&rest _args)
                 (ert-fail "carriage--call-git must not be called when no repo is present"))))
      (let ((default-directory (file-name-as-directory tmp)))
        (should (null (carriage-project-root)))))))

(provide 'carriage-utils-project-root-no-git-test)
;;; carriage-utils-project-root-no-git-test.el ends here
