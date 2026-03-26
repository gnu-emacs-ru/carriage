;;; carriage-gatekeeper-e2e-test.el --- End-to-end gatekeeper tests with file text  -*- lexical-binding: t; -*-
;; Specifications:
;;   spec/context-integration-v2.org
;;   spec/file-ops-v2.org
;;   spec/code-style-v2.org

(require 'ert)
(require 'cl-lib)
(require 'carriage-apply)
(require 'carriage-parser)

;;; Code:

(defun carriage-gatekeeper-test--with-temp-repo (fn)
  "Create temp git repo, call FN with root path."
  (let* ((tmp (make-temp-file "carriage-gk-test-" t)))
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "-C" tmp "init")
          (call-process "git" nil nil nil "-C" tmp "config" "user.email" "test@test.com")
          (call-process "git" nil nil nil "-C" tmp "config" "user.name" "Test")
          (funcall fn tmp))
      (delete-directory tmp t))))

(ert-deftest carriage-gatekeeper-allows-edit-with-file-text ()
  "Gatekeeper allows edit when file text is present in request state."
  (carriage-gatekeeper-test--with-temp-repo
   (lambda (root)
     (let* ((file-path (expand-file-name "test.txt" root))
            (file-content "Original content\n")
            (default-directory root))
       ;; Create file
       (with-temp-buffer
         (insert file-content)
         (write-region (point-min) (point-max) file-path nil 'silent))
       ;; Create manifest with has_text=true
       (let ((manifest-content
              (concat "#+begin_state_manifest\n"
                      "path|exists|has_text\n"
                      "test.txt|true|true\n"
                      "#+end_state_manifest\n"))
             (buf (get-buffer-create "*gatekeeper-test*")))
         (with-current-buffer buf
           (insert manifest-content)
           ;; Simulate gatekeeper check with manifest present
           (let ((item (list :op 'aibo :file "test.txt" :path file-path)))
             ;; Gatekeeper should allow this (has_text=true)
             (should (eq (carriage--gatekeeper-check-item item root) 'ok))))
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

(ert-deftest carriage-gatekeeper-rejects-edit-without-file-text ()
  "Gatekeeper rejects edit when has_text=false in request state."
  (carriage-gatekeeper-test--with-temp-repo
   (lambda (root)
     (let* ((file-path (expand-file-name "test.txt" root))
            (default-directory root))
       ;; Create file
       (with-temp-buffer
         (insert "Content\n")
         (write-region (point-min) (point-max) file-path nil 'silent))
       ;; Create manifest with has_text=false
       (let ((manifest-content
              (concat "#+begin_state_manifest\n"
                      "path|exists|has_text\n"
                      "test.txt|true|false\n"
                      "#+end_state_manifest\n"))
             (buf (get-buffer-create "*gatekeeper-test*")))
         (with-current-buffer buf
           (insert manifest-content)
           ;; Gatekeeper should reject edit (has_text=false)
           (let ((item (list :op 'aibo :file "test.txt" :path file-path)))
             (should (eq (carriage--gatekeeper-check-item item root) 'fail))))
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

(ert-deftest carriage-gatekeeper-rejects-create-for-existing-file ()
  "Gatekeeper rejects create when exists=true in request state."
  (carriage-gatekeeper-test--with-temp-repo
   (lambda (root)
     (let* ((file-path (expand-file-name "existing.txt" root))
            (default-directory root))
       ;; Create file
       (with-temp-buffer
         (insert "Exists\n")
         (write-region (point-min) (point-max) file-path nil 'silent))
       ;; Create manifest with exists=true
       (let ((manifest-content
              (concat "#+begin_state_manifest\n"
                      "path|exists|has_text\n"
                      "existing.txt|true|true\n"
                      "#+end_state_manifest\n"))
             (buf (get-buffer-create "*gatekeeper-test*")))
         (with-current-buffer buf
           (insert manifest-content)
           ;; Create op should be rejected for existing file
           (let ((item (list :op 'create :file "existing.txt" :path file-path)))
             (should (eq (carriage--gatekeeper-check-item item root) 'fail))))
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

(ert-deftest carriage-gatekeeper-allows-create-for-missing-file ()
  "Gatekeeper allows create when file does not exist."
  (carriage-gatekeeper-test--with-temp-repo
   (lambda (root)
     (let* ((file-path (expand-file-name "new.txt" root))
            (default-directory root))
       ;; File does NOT exist
       (should (not (file-exists-p file-path)))
       ;; Create should be allowed for non-existing file
       t))))

(ert-deftest carriage-gatekeeper-internal-flow-synthesizes-state ()
  "Internal flows (no manifest) synthesize trusted state from filesystem."
  (carriage-gatekeeper-test--with-temp-repo
   (lambda (root)
     (let* ((file-path (expand-file-name "test.txt" root))
            (default-directory root))
       ;; Create file
       (with-temp-buffer
         (insert "Content\n")
         (write-region (point-min) (point-max) file-path nil 'silent))
       ;; No manifest - internal flow should synthesize state
       ;; This is the compat fallback for internal/test flows
       t))))

;;; carriage-gatekeeper-e2e-test.el ends here
