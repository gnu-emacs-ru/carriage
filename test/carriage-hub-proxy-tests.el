;;; carriage-hub-proxy-tests.el --- Hub unit/smoke tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'carriage-swarm-registry)
(require 'carriage-hub)

(ert-deftest carriage-hub--loads ()
  "Hub module loads and exports entrypoints."
  (should (fboundp 'carriage-hub-start))
  (should (fboundp 'carriage-hub-stop)))

(ert-deftest carriage-hub--agents-payload-from-registry ()
  "Hub agents payload is derived from registry entries (no network)."
  (let* ((tmp (make-temp-file "carriage-hub-registry-" t))
         (carriage-swarm-registry-runtime-dir tmp))
    (carriage-swarm-registry-agent-register
     :id "agent-test-1" :pid 99999 :port 12345 :bind "127.0.0.1"
     :project "p" :label "l" :version "v1" :token "tok123")
    (let* ((rows (carriage-hub--agents-payload))
           (row (cl-find-if (lambda (x) (equal (alist-get 'id x) "agent-test-1")) rows)))
      (should row)
      (should (equal (alist-get 'bind row) "127.0.0.1"))
      (should (= (alist-get 'port row) 12345))
      ;; last_seen may be :null until hub probes; ensure key exists.
      (should (assq 'last_seen row)))))

(provide 'carriage-hub-proxy-tests)
;;; carriage-hub-proxy-tests.el ends here
