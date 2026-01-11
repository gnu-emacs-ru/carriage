;;; carriage-hub-proxy-tests.el --- Hub unit/smoke tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)

(require 'carriage-swarm-registry)
(require 'carriage-hub)
(require 'carriage-web)

(defun carriage-hub-test--split-head-body (s)
  (cond
   ((string-match "\r\n\r\n" s)
    (cons (substring s 0 (match-beginning 0))
          (substring s (match-end 0))))
   ((string-match "\n\n" s)
    (cons (substring s 0 (match-beginning 0))
          (substring s (match-end 0))))
   (t (cons s ""))))

(defun carriage-hub-test--http (port req)
  (let* ((buf "")
         (done nil)
         (deadline (+ (float-time) 2.0))
         (p (make-network-process
             :name "carriage-hub-test-client"
             :buffer nil
             :host "127.0.0.1"
             :service port
             :noquery t
             :family 'ipv4
             :coding 'binary)))
    (set-process-filter
     p
     (lambda (_p chunk)
       (setq buf (concat buf chunk))))
    (set-process-sentinel
     p
     (lambda (_p _e) (setq done t)))
    (process-send-string p req)
    (process-send-eof p)
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output p 0.05))
    (ignore-errors (delete-process p))
    buf))

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

(defun carriage-hub-test--http-open (port req)
  "Send REQ to PORT and keep connection open. Returns (PROC . DATA-STR)."
  (let* ((buf "")
         (p (make-network-process
             :name "carriage-hub-test-client-open"
             :buffer nil
             :host "127.0.0.1"
             :service port
             :noquery t
             :family 'ipv4
             :coding 'binary)))
    (set-process-filter
     p
     (lambda (_p chunk)
       (setq buf (concat buf chunk))))
    (process-send-string p req)
    (cons p (lambda () buf))))

(ert-deftest carriage-hub--proxy-not-found ()
  "Proxying a missing agent id yields WEB_E_NOT_FOUND (no token leak; structured error)."
  (let* ((tmp (make-temp-file "carriage-hub-proxy-notfound-" t))
         (carriage-swarm-registry-runtime-dir tmp)
         (carriage-hub-bind "127.0.0.1")
         (carriage-hub-port 0)
         (carriage-hub--proxy-active 0)
         (srv nil))
    (unwind-protect
        (progn
          (setq srv (carriage-hub-start))
          (let* ((port (process-contact srv :service))
                 (resp (carriage-hub-test--http
                        port
                        (concat
                         "GET /hub/agent/no-such-agent/api/health HTTP/1.1\r\n"
                         (format "Host: 127.0.0.1:%d\r\n" port)
                         "Connection: close\r\n\r\n")))
                 (pair (carriage-hub-test--split-head-body resp))
                 (body (cdr pair)))
            (should (string-match-p "502 Bad Gateway" resp))
            (should (string-match-p "WEB_E_NOT_FOUND" body))
            (should (= carriage-hub--proxy-active 0))))
      (ignore-errors (carriage-hub-stop)))))

(ert-deftest carriage-hub--proxy-cap-overflow ()
  "Hard cap overflow yields WEB_E_CAP deterministically (checked before agent lookup)."
  (let* ((tmp (make-temp-file "carriage-hub-proxy-cap-" t))
         (carriage-swarm-registry-runtime-dir tmp)
         (carriage-hub-bind "127.0.0.1")
         (carriage-hub-port 0)
         (carriage-hub-max-proxy-active 0)
         (carriage-hub--proxy-active 0)
         (srv nil))
    (unwind-protect
        (progn
          (setq srv (carriage-hub-start))
          (let* ((port (process-contact srv :service))
                 (resp (carriage-hub-test--http
                        port
                        (concat
                         "GET /hub/agent/any/api/health HTTP/1.1\r\n"
                         (format "Host: 127.0.0.1:%d\r\n" port)
                         "Connection: close\r\n\r\n")))
                 (pair (carriage-hub-test--split-head-body resp))
                 (body (cdr pair)))
            (should (string-match-p "503 Service Unavailable" resp))
            (should (string-match-p "WEB_E_CAP" body))
            (should (= carriage-hub--proxy-active 0))))
      (ignore-errors (carriage-hub-stop)))))

(ert-deftest carriage-hub--early-reject-oversize-content-length ()
  "Hub rejects oversized requests based on Content-Length before buffering the body."
  (let* ((tmp (make-temp-file "carriage-hub-oversize-" t))
         (carriage-swarm-registry-runtime-dir tmp)
         (carriage-hub-bind "127.0.0.1")
         (carriage-hub-port 0)
         (carriage-hub-max-request-bytes 10)
         (srv nil))
    (unwind-protect
        (progn
          (setq srv (carriage-hub-start))
          (let* ((port (process-contact srv :service))
                 (resp (carriage-hub-test--http
                        port
                        (concat
                         "POST /hub/cleanup HTTP/1.1\r\n"
                         (format "Host: 127.0.0.1:%d\r\n" port)
                         "Content-Type: application/json\r\n"
                         "Content-Length: 999\r\n"
                         "Connection: close\r\n\r\n")))
                 (pair (carriage-hub-test--split-head-body resp))
                 (body (cdr pair)))
            (should (string-match-p "413 Payload Too Large" resp))
            (should (string-match-p "WEB_E_PAYLOAD" body))))
      (ignore-errors (carriage-hub-stop)))))

(ert-deftest carriage-hub--proxy-connection-failure-does-not-leak-slots ()
  "When upstream connection fails, hub must not leak proxy_active slots."
  (let* ((tmp (make-temp-file "carriage-hub-proxy-connfail-" t))
         (carriage-swarm-registry-runtime-dir tmp)
         ;; Choose a likely-unused TCP port by asking the OS for an ephemeral port,
         ;; then immediately releasing it. The test asserts slot cleanup (not status).
         (dummy (make-network-process :name "carriage-hub-dummy-port"
                                      :server t :host "127.0.0.1" :service 0
                                      :noquery t :family 'ipv4 :coding 'binary))
         (unused (process-contact dummy :service))
         (_ (ignore-errors (delete-process dummy)))
         (agent-id "agent-connfail")
         (_reg (carriage-swarm-registry-agent-register
                :id agent-id :pid 99999 :port unused :bind "127.0.0.1"
                :project "p" :label "l" :version "v1" :token "tok"))
         (carriage-hub-bind "127.0.0.1")
         (carriage-hub-port 0)
         (carriage-hub--proxy-active 0)
         (srv nil))
    (unwind-protect
        (progn
          (setq srv (carriage-hub-start))
          (let* ((port (process-contact srv :service))
                 (resp (carriage-hub-test--http
                        port
                        (concat
                         (format "GET /hub/agent/%s/api/health HTTP/1.1\r\n" agent-id)
                         (format "Host: 127.0.0.1:%d\r\n" port)
                         "Connection: close\r\n\r\n"))))
            (should (string-match-p "\\`HTTP/1\\.[01] [0-9][0-9][0-9]" resp))
            ;; Allow async sentinels to run if needed.
            (accept-process-output nil 0.05)
            (should (= carriage-hub--proxy-active 0))))
      (ignore-errors (carriage-hub-stop)))))


(ert-deftest carriage-hub--proxy-sse-downstream-close-releases-slot ()
  "SSE proxy: closing downstream client must close upstream and release proxy_active."
  (let* ((tmp (make-temp-file "carriage-hub-proxy-sse-" t))
         (carriage-swarm-registry-runtime-dir tmp)
         ;; Start an Agent-style web server locally.
         (carriage-web-daemon-p t)
         (carriage-web-bind "127.0.0.1")
         (carriage-web-port 0)
         (carriage-web-auth-token "tok")
         (carriage-web-max-sse-clients 4)
         (agent-srv nil)
         (agent-id "agent-sse")
         (carriage-hub-bind "127.0.0.1")
         (carriage-hub-port 0)
         (carriage-hub--proxy-active 0)
         (hub-srv nil)
         (cli nil)
         (get-buf nil))
    (unwind-protect
        (progn
          (setq agent-srv (carriage-web-start))
          (let* ((aport (process-contact agent-srv :service)))
            (carriage-swarm-registry-agent-register
             :id agent-id :pid 99999 :port aport :bind "127.0.0.1"
             :project "p" :label "l" :version "v1" :token "tok"))
          (setq hub-srv (carriage-hub-start))
          (let* ((hport (process-contact hub-srv :service))
                 (r (carriage-hub-test--http-open
                     hport
                     (concat
                      (format "GET /hub/agent/%s/stream HTTP/1.1\r\n" agent-id)
                      (format "Host: 127.0.0.1:%d\r\n" hport)
                      "Accept: text/event-stream\r\n"
                      "Connection: keep-alive\r\n\r\n"))))
            (setq cli (car r) get-buf (cdr r))
            ;; Wait for headers/hello to arrive through the proxy.
            (let ((deadline (+ (float-time) 1.5)))
              (while (and (< (float-time) deadline)
                          (not (string-match-p "200 OK" (funcall get-buf))))
                (accept-process-output cli 0.05)))
            (should (string-match-p "200 OK" (funcall get-buf)))
            (should (>= carriage-hub--proxy-active 1))
            ;; Close downstream; hub should close upstream and release slot.
            (ignore-errors (delete-process cli))
            (setq cli nil)
            (let ((deadline (+ (float-time) 2.0)))
              (while (and (< (float-time) deadline)
                          (> carriage-hub--proxy-active 0))
                (accept-process-output nil 0.05)))
            (should (= carriage-hub--proxy-active 0))))
      (ignore-errors (when (processp cli) (delete-process cli)))
      (ignore-errors (carriage-hub-stop))
      (ignore-errors (carriage-web-stop)))))

(provide 'carriage-hub-proxy-tests)
;;; carriage-hub-proxy-tests.el ends here
