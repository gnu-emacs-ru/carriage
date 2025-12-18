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
    (should (file-exists-p (carriage-swarm-registry-file)))
    (should (file-exists-p (carriage-swarm-registry-agent-dir id)))
    (should (string= (carriage-swarm-registry-agent-read-token id) token))
    (should (= (carriage-swarm-registry-agent-read-port id) port))
    (let* ((entries (carriage-swarm-registry-read))
           (e (cl-find-if (lambda (x) (equal (alist-get 'id x) id)) entries)))
      (should e)
      (should (= (alist-get 'pid e) pid))
      (should (= (alist-get 'port e) port))
      (should (string= (alist-get 'bind e) bind)))))

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
