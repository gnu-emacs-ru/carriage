;;; carriage-context-tests.el --- Tests for context profile toggling and basics -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; ERT tests for carriage-context profile toggling:
;; - Toggle P1<->P3 updates buffer-local profile var.
;; - Toggle does not error in a plain Org buffer.
;; - Basic sanity that modeline refresh helpers do not throw (best-effort).

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'carriage-context)

(ert-deftest carriage-context/profile-toggle-updates-state ()
  "carriage-context-profile-set should set buffer-local profile to 'p3 and back to 'p1."
  (with-temp-buffer
    (org-mode)
    ;; Defaults: ensure variable is buffer-local and starts as 'p1.
    (setq-local carriage-doc-context-profile 'p1)
    ;; Switch to P3
    (carriage-context-profile-set 'p3)
    (should (eq carriage-doc-context-profile 'p3))
    ;; Switch back to P1
    (carriage-context-profile-set 'p1)
    (should (eq carriage-doc-context-profile 'p1))))

(ert-deftest carriage-context/profile-toggle-does-not-error ()
  "Toggling profiles should not error; message side-effects are acceptable."
  (with-temp-buffer
    (org-mode)
    (setq-local carriage-doc-context-profile 'p1)
    (should (ignore-errors (carriage-context-profile-set 'p3) t))
    (should (ignore-errors (carriage-context-profile-set 'p1) t))))

(ert-deftest carriage-context/secret-path-warn-skip-omits-content ()
  "Secret-like paths should be included as path-only with a warning when policy=warn-skip."
  (let* ((root (make-temp-file "carriage-secret-" t))
         (default-directory root)
         (f (expand-file-name ".env" root)))
    (unwind-protect
        (progn
          (with-temp-file f (insert "SECRET=1\n"))
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_context\n.env\n#+end_context\n")
            (let ((carriage-context-secret-path-policy 'warn-skip)
                  (carriage-context-secret-path-regexps '("\\(?:\\`\\|/\\)\\.env\\(?:\\..*\\)?\\'")))
              (let* ((ctx (carriage-context-collect (current-buffer) root))
                     (files (plist-get ctx :files))
                     (warns (plist-get ctx :warnings))
                     (entry (cl-find-if (lambda (e) (equal (plist-get e :rel) ".env")) files)))
                (should entry)
                (should (null (plist-get entry :content)))
                (should (eq (plist-get entry :reason) 'secret-path))
                (should (cl-some (lambda (w) (and (stringp w) (string-match-p "secret-path" w))) warns)))))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-context/secret-path-allow-includes-content ()
  "Secret-like paths should include contents when policy=allow."
  (let* ((root (make-temp-file "carriage-secret-" t))
         (default-directory root)
         (f (expand-file-name ".env" root)))
    (unwind-protect
        (progn
          (with-temp-file f (insert "SECRET=1\n"))
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_context\n.env\n#+end_context\n")
            (let ((carriage-context-secret-path-policy 'allow)
                  (carriage-context-secret-path-regexps '("\\(?:\\`\\|/\\)\\.env\\(?:\\..*\\)?\\'")))
              (let* ((ctx (carriage-context-collect (current-buffer) root))
                     (files (plist-get ctx :files))
                     (entry (cl-find-if (lambda (e) (equal (plist-get e :rel) ".env")) files)))
                (should entry)
                (should (stringp (plist-get entry :content)))
                (should (string-match-p "SECRET=1" (plist-get entry :content))))))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-context/doc-paths-dirty-only-when-edit-affects-context-block ()
  "Doc-context cache should not be invalidated by arbitrary edits outside begin_context blocks."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_context\nfoo.txt\n#+end_context\n\nBody.\n")
    ;; Simulate first compute
    (setq-local carriage-context--doc-paths-dirty t)
    (should (equal (carriage-context--doc-paths (current-buffer)) '("foo.txt")))
    (should (eq carriage-context--doc-paths-dirty nil))
    ;; Edit outside block should NOT mark dirty
    (let ((pos (save-excursion (goto-char (point-max)) (point))))
      (carriage-context--doc-paths-mark-dirty pos pos 0)
      (should (eq carriage-context--doc-paths-dirty nil)))
    ;; Edit inside block should mark dirty
    (let ((pos (save-excursion
                 (goto-char (point-min))
                 (search-forward "foo.txt")
                 (point))))
      (carriage-context--doc-paths-mark-dirty pos pos 0)
      (should (eq carriage-context--doc-paths-dirty t)))))

(ert-deftest carriage-context/doc-paths-recompute-after-dirty ()
  "When doc-context cache is dirty, carriage-context--doc-paths should recompute."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_context\nfoo.txt\n#+end_context\n")
    (setq-local carriage-context--doc-paths-dirty t)
    (should (equal (carriage-context--doc-paths (current-buffer)) '("foo.txt")))
    ;; Mutate inside the context block and mark dirty
    (goto-char (point-min))
    (search-forward "foo.txt")
    (insert "\nbar.txt")
    (setq carriage-context--doc-paths-dirty t)
    (should (equal (sort (carriage-context--doc-paths (current-buffer)) #'string<)
                   '("bar.txt" "foo.txt")))
    (should (eq carriage-context--doc-paths-dirty nil))))

(ert-deftest carriage-context/doc-context-marker-must-be-directive-only ()
  "A prose line starting with \"#+begin_context ...\" must NOT open a context block."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_context blocks and full rescans can be expensive on large documents.\n")
    (insert "THIS-LINE-MUST-NOT-BE-TREATED-AS-A-PATH\n")
    (insert "#+end_context\n\n")
    (insert "#+begin_context\nreal.txt\n#+end_context\n")
    (setq-local carriage-doc-context-scope 'all)
    (setq-local carriage-context--doc-paths-dirty t)
    (should (equal (carriage-context--doc-paths (current-buffer)) '("real.txt")))))

(ert-deftest carriage-context/doc-context-unterminated-block-is-ignored-with-warning ()
  "Unterminated begin_context must not consume the rest of the buffer (and should warn)."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_context\nfoo.txt\n")
    (insert "Body line that must never become a \"path\".\n")
    (setq-local carriage-doc-context-scope 'all)
    (setq-local carriage-context--doc-paths-dirty t)
    (should (equal (carriage-context--doc-paths (current-buffer)) '()))
    (let* ((res (carriage-context-count (current-buffer)))
           (warns (plist-get res :warnings)))
      (should (listp warns))
      (should (cl-some (lambda (w)
                         (and (stringp w)
                              (string-match-p "unterminated" w)))
                       warns)))))

(provide 'carriage-context-tests)
;;; carriage-context-tests.el ends here
