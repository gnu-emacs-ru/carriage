;;; carriage-context-project-map-invalidation-test.el --- Project Map invalidation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage-context)
(require 'carriage-op-file)

(ert-deftest carriage-project-map-invalidated-after-create ()
  "Project Map cache should be invalidated after successful create."
  (let* ((carriage-context--project-map-allow-compute t)
         (root (make-temp-file "carriage-map-create-" t))
         (default-directory root)
         (carriage-context-project-map-cache-ttl 3600))
    (call-process "git" nil nil nil "init" "-q" root)
    (let* ((file "new-file.txt"))
      (should-not (string-match-p
                   (regexp-quote file)
                   (or (plist-get (carriage-context-project-map-build root) :text) "")))
      (should (eq (plist-get (carriage-apply-create
                              (list (cons :file file) (cons :content "hello\n"))
                              root)
                             :status)
                  'ok))
      (should (string-match-p
               (regexp-quote file)
               (or (plist-get (carriage-context-project-map-build root) :text) ""))))))

(ert-deftest carriage-project-map-invalidated-after-rename-and-delete ()
  "Project Map cache should reflect rename/delete immediately after successful file ops."
  (let* ((carriage-context--project-map-allow-compute t)
         (root (make-temp-file "carriage-map-rm-" t))
         (default-directory root)
         (carriage-context-project-map-cache-ttl 3600))
    (call-process "git" nil nil nil "init" "-q" root)
    (with-temp-file (expand-file-name "old.txt" root)
      (insert "data\n"))
    (let ((before (or (plist-get (carriage-context-project-map-build root) :text) "")))
      (should (string-match-p (regexp-quote "old.txt") before)))
    (should (eq (plist-get (carriage-apply-rename
                            (list (cons :from "old.txt") (cons :to "new.txt"))
                            root)
                           :status)
                'ok))
    (let ((after-rename (or (plist-get (carriage-context-project-map-build root) :text) "")))
      (should-not (string-match-p (regexp-quote "old.txt") after-rename))
      (should (string-match-p (regexp-quote "new.txt") after-rename)))
    (should (eq (plist-get (carriage-apply-delete
                            (list (cons :file "new.txt"))
                            root)
                           :status)
                'ok))
    (let ((after-delete (or (plist-get (carriage-context-project-map-build root) :text) "")))
      (should-not (string-match-p (regexp-quote "new.txt") after-delete)))))

(provide 'carriage-context-project-map-invalidation-test)
;;; carriage-context-project-map-invalidation-test.el ends here
