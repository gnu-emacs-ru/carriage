;;; ert-runner.el --- Run ERT tests in batch  -*- lexical-binding: t; -*-

(require 'ert)

;; Ensure lisp/ is on load-path (flake also adds -L lisp).
(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (lisp (expand-file-name "../lisp" here)))
  (add-to-list 'load-path lisp))

(require 'carriage)

;; Load all *-test.el in this directory and subdirs (e.g., engines).
(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (eng  (expand-file-name "engines" here))
       (dirs (list here (and (file-directory-p eng) eng))))
  ;; Ensure tests directory on load-path so (load "...") works.
  (add-to-list 'load-path here)
  (dolist (dir dirs)
    (when dir
      (dolist (f (directory-files dir t "^carriage-.*tests?\\.el\\'"))
        (condition-case _e
            (load f t t)
          (error
           (message "ERT runner: failed to load %s" f)))))))

;; Fallback inline tests if repo packaging did not include test files.
(require 'cl-lib)
(unless (fboundp 'carriage-web-runner-health-dispatch-smoke)
  (require 'carriage-web)
  ;; 1) Health smoke
  (ert-deftest carriage-web-runner-health-dispatch-smoke ()
    "Fallback smoke test: /api/health responds with ok envelope."
    (let (cap-status cap-payload)
      (cl-letf (((symbol-function 'carriage-web--send-json)
                 (lambda (_proc status payload)
                   (setq cap-status status
                         cap-payload payload))))
        (carriage-web--dispatch-request
         nil (list :method "GET" :path "/api/health" :query nil :headers '())))
      (should (equal cap-status "200 OK"))
      (should (eq (plist-get cap-payload :ok) t))
      (should (plist-get (plist-get cap-payload :data) :version))))
  ;; 2) Sessions list returns ok envelope and list
  (ert-deftest carriage-web-runner-sessions-dispatch-smoke ()
    "Fallback: /api/sessions returns ok with a list."
    (let (cap-status cap-payload)
      (cl-letf (((symbol-function 'carriage-web--send-json)
                 (lambda (_proc status payload)
                   (setq cap-status status cap-payload payload))))
        (carriage-web--dispatch-request
         nil (list :method "GET" :path "/api/sessions" :query nil :headers '())))
      (should (equal cap-status "200 OK"))
      (should (eq (plist-get cap-payload :ok) t))
      (should (listp (plist-get cap-payload :data)))))
  ;; 3) Unknown route → 404 WEB_E_NOT_FOUND
  (ert-deftest carriage-web-runner-404-dispatch-smoke ()
    "Fallback: unknown route returns 404 with WEB_E_NOT_FOUND."
    (let (cap-status cap-payload)
      (cl-letf (((symbol-function 'carriage-web--send-json)
                 (lambda (_proc status payload)
                   (setq cap-status status cap-payload payload))))
        (carriage-web--dispatch-request
         nil (list :method "GET" :path "/no-such-route" :query nil :headers '())))
      (should (equal cap-status "404 Not Found"))
      (should (eq (plist-get cap-payload :ok) json-false))
      (should (equal (plist-get cap-payload :code) "WEB_E_NOT_FOUND"))))
  ;; 4) POST /api/cmd unknown → 400 WEB_E_CMD
  (ert-deftest carriage-web-runner-cmd-unknown-dispatch-smoke ()
    "Fallback: unknown command returns 400 WEB_E_CMD."
    (let* ((body (json-encode '(:cmd "unknown" :doc "ephemeral:test"))))
      (let (cap-status cap-payload)
        (cl-letf (((symbol-function 'carriage-web--send-json)
                   (lambda (_proc status payload)
                     (setq cap-status status cap-payload payload))))
          (carriage-web--dispatch-request
           nil (list :method "POST" :path "/api/cmd" :query nil
                     :headers '(("content-type" . "application/json"))
                     :body body)))
        (should (equal cap-status "400 Bad Request"))
        (should (eq (plist-get cap-payload :ok) json-false))
        (should (equal (plist-get cap-payload :code) "WEB_E_CMD"))))))
(let* ((sel (or (getenv "ERT_SELECTOR") (getenv "CARRIAGE_TESTS"))))
  (if (and sel (> (length sel) 0))
      ;; Use an ERT selector form (portable to older Emacs): (satisfies PRED)
      (ert-run-tests-batch-and-exit
       `(satisfies
         (lambda (test)
           (let ((name (symbol-name (ert-test-name test))))
             (string-match-p ,sel name)))))
    (ert-run-tests-batch-and-exit t)))
