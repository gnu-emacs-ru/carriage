;;; carriage-report-apply-test.el --- Report [Apply] button test -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage)
(require 'carriage-report)

(defun carriage-report-apply-test--git (dir &rest args)
  "Run git ARGS in DIR and return exit code."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun carriage-report-apply-test--git-out (dir &rest args)
  "Run git ARGS in DIR and return trimmed stdout."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (apply #'call-process "git" nil t nil args)
      (string-trim (buffer-string)))))

(defun carriage-report-apply-test--write (dir rel content)
  "Write CONTENT to DIR/REL, creating directories as needed."
  (let ((abs (expand-file-name rel dir)))
    (make-directory (file-name-directory abs) t)
    (with-temp-file abs (insert content))
    abs))

(defun carriage-report-apply-test--read (dir rel)
  "Read file DIR/REL and return contents."
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel dir))
    (buffer-string)))

(ert-deftest carriage-report-apply-at-point-sre-applies ()
  "Render report with a single SRE item and apply it via [Apply] at point."
  (let* ((dir (make-temp-file "carriage-rpt-apply-" t))
         ;; Sync path: enable legacy WIP switch for test expectation
         (carriage-apply-require-wip-branch t))
    (unwind-protect
        (progn
          ;; init repo
          (should (zerop (carriage-report-apply-test--git dir "init")))
          (should (zerop (carriage-report-apply-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-report-apply-test--git dir "config" "user.name" "Tester")))
          ;; prepare file and commit
          (carriage-report-apply-test--write dir "x.txt" "hello\n")
          (should (zerop (carriage-report-apply-test--git dir "add" "--" "x.txt")))
          (should (zerop (carriage-report-apply-test--git dir "commit" "-m" "init")))
          ;; plan: SRE hello->world
          (let* ((default-directory (file-name-as-directory dir))
                 (item (list (cons :version "1")
                             (cons :op 'sre)
                             (cons :file "x.txt")
                             (cons :pairs
                                   (list (list (cons :from "hello")
                                               (cons :to   "world")
                                               (cons :opts '(:occur first :match literal)))))))
                 (rep (carriage-dry-run-plan (list item) dir))
                 (buf (carriage-report-render rep)))
            (with-current-buffer buf
              (goto-char (point-min))
              ;; Move to first item row (after header lines)
              (re-search-forward "^|---")
              (forward-line 1)
              ;; Invoke [Apply] via function; should not error in batch
              (let ((errp (condition-case _e
                              (progn
                                (carriage-report-apply-at-point)
                                nil)
                            (error t))))
                (should (not errp))))
            ;; Verify file content changed
            (should (string= (carriage-report-apply-test--read dir "x.txt") "world\n"))
            ;; Verify HEAD is on a named branch (do not assume branch policy here)
            (let ((branch (carriage-report-apply-test--git-out dir "rev-parse" "--abbrev-ref" "HEAD")))
              (should (stringp branch))
              (should (> (length branch) 0))
              (should-not (string= branch "HEAD"))))))
    (ignore-errors (delete-directory dir t))))

(ert-deftest carriage-report-apply-at-point-sre-explicit-manifest-without-text-fails ()
  "Report apply must fail-closed when explicit request-state says has_text=false."
  (let* ((dir (make-temp-file "carriage-rpt-apply-" t)))
    (unwind-protect
        (progn
          (should (zerop (carriage-report-apply-test--git dir "init")))
          (should (zerop (carriage-report-apply-test--git dir "config" "user.email" "tester@example.com")))
          (should (zerop (carriage-report-apply-test--git dir "config" "user.name" "Tester")))
          (carriage-report-apply-test--write dir "x.txt" "hello\n")
          (should (zerop (carriage-report-apply-test--git dir "add" "--" "x.txt")))
          (should (zerop (carriage-report-apply-test--git dir "commit" "-m" "init")))
          (with-temp-buffer
            (insert "#+begin_state_manifest\n")
            (insert "path|exists|has_text\n")
            (insert "x.txt|true|false\n")
            (insert "#+end_state_manifest\n")
            (let* ((item (list (cons :version "1")
                               (cons :op 'sre)
                               (cons :file "x.txt")
                               (cons :pairs
                                     (list (list (cons :from "hello")
                                                 (cons :to   "world")
                                                 (cons :opts '(:occur first :match literal)))))))
                   (rep (carriage-dry-run-plan (list item) dir))
                   (row (car (plist-get rep :items))))
              (should (eq (plist-get row :status) 'fail))
              (should (string-match-p "file text is not present in current request context"
                                      (or (plist-get row :details) "")))))))
      (ignore-errors (delete-directory dir t)))))

(provide 'carriage-report-apply-test)
;;; carriage-report-apply-test.el ends here
