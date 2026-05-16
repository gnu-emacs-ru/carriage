;;; carriage-annotate-changed-bytes-test.el --- Ensure annotate writes :changed-bytes in header -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'carriage)

(ert-deftest carriage-annotate-inserts-changed-bytes-in-header ()
  "When apply succeeds, annotation header must include :changed-bytes in the printed plist."
  (skip-unless (fboundp 'carriage--annotate-applied-blocks-in-report))
  ;; Create temp buffer with a begin_patch block, simulate report with applied row.
  (let* ((tmp (make-temp-file "annotate-" nil ".org" "#+begin_patch (:op patch :file \"x.txt\")\nold\n#+end_patch\n"))
         (buf (find-file-noselect tmp))
         (row (list :op 'patch :status 'ok :file "x.txt" :details "OK" :changed-bytes 12
                    :_plan (list :_buffer buf :_beg-marker (copy-marker (with-current-buffer buf (point-min))) :_end-marker (copy-marker (with-current-buffer buf (point-max)))))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (carriage--annotate-applied-blocks-in-report (list row)))
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "^#\+begin_patch\s-+\((.*)\)" nil t)
            (let* ((sexp (match-string 1))
                   (plist (condition-case _e (car (read-from-string sexp)) (error nil))))
              (should (and (listp plist) (numberp (plist-get plist :changed-bytes))))
              (should (= (plist-get plist :changed-bytes) 12))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (ignore-errors (delete-file tmp))))

(provide 'carriage-annotate-changed-bytes-test)
;;; carriage-annotate-changed-bytes-test.el ends here
