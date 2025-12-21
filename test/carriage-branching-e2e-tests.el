;;; carriage-branching-e2e-tests.el --- E2E branching tests (P1 default, P3+inherit)  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(require 'carriage-task)
(require 'carriage-doc-state)
(require 'carriage-context)

(defun carriage--test--make-temp-dir ()
  "Return a fresh temporary directory path (with trailing slash)."
  (file-name-as-directory (make-temp-file "carriage-e2e-" t)))

(defun carriage--test--write-file (path content)
  "Write CONTENT to PATH, creating directories as needed."
  (make-directory (file-name-directory (expand-file-name path)) t)
  (with-temp-file (expand-file-name path)
    (insert content)))

(defun carriage--test--first-org-created (root)
  "Return path of the first created org doc under ROOT/org matching NN.N-*.org, or nil."
  (let* ((dir (expand-file-name "org" (file-name-as-directory root)))
         (files (and (file-directory-p dir)
                     (directory-files dir t "^[0-9]+\\.[0-9]+-.*\\.org\\'"))))
    (car files)))

(defun carriage--test--buffer-string-of (file)
  "Return contents of FILE as string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'carriage-branching-e2e-tests)
;;; carriage-branching-e2e-tests.el ends here
