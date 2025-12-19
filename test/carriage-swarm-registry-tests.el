;;; carriage-swarm-registry-tests.el --- Tests for swarm registry  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'carriage-swarm-registry)

(ert-deftest carriage-swarm-registry--register-and-read ()
  (let* ((tmp (make-temp-file "carriage-swarm-registry-" t))
         (carriage-swarm-registry-runtime-dir tmp)
         (id "agent-test-1")
         (pid 99999)
         (port 12345)
         (bind "127.0.0.1")
         (token "tok123"))
    (carriage-swarm-registry-agent-register
     :id id :pid pid :port port :bind bind :project "p" :label "l" :version "v1" :token token)
    ;; Registry file exists and does not contain tokens (catalog is discovery only).
    (should (file-exists-p (carriage-swarm-registry-file)))
    (should-not (string-match-p (regexp-quote token)
                                (with-temp-buffer
                                  (insert-file-contents (carriage-swarm-registry-file))
                                  (buffer-string))))
    ;; Runtime dir exists; token is stored only in per-agent token file.
    (should (file-exists-p (carriage-swarm-registry-agent-dir id)))
    (should (string= (carriage-swarm-registry-agent-read-token id) token))
    (should (= (carriage-swarm-registry-agent-read-port id) port))
    (let* ((entries (carriage-swarm-registry-read))
           (e (cl-find-if (lambda (x) (equal (alist-get 'id x) id)) entries)))
      (should e)
      (should (= (alist-get 'pid e) pid))
      (should (= (alist-get 'port e) port))
      (should (string= (alist-get 'bind e) bind))
      ;; Ensure no token-like fields in parsed registry entries.
      (should-not (alist-get 'token e))
      (should-not (alist-get 'auth_token e))
      (should-not (alist-get 'x_auth e)))))

(ert-deftest carriage-swarm-registry--remove ()
  (let* ((tmp (make-temp-file "carriage-swarm-registry-" t))
         (carriage-swarm-registry-runtime-dir tmp)
         (id "agent-test-2"))
    (carriage-swarm-registry-agent-register
     :id id :pid 1 :port 1 :bind "127.0.0.1" :token "t")
    (should (cl-find-if (lambda (x) (equal (alist-get 'id x) id))
                        (carriage-swarm-registry-read)))
    (carriage-swarm-registry-remove id)
    (should-not (cl-find-if (lambda (x) (equal (alist-get 'id x) id))
                            (carriage-swarm-registry-read)))))

(provide 'carriage-swarm-registry-tests)
;;; carriage-swarm-registry-tests.el ends here
