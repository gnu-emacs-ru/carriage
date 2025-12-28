;;; carriage-agent-server-tests.el --- Minimal agent smoke tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)

(ert-deftest carriage-agent--loads ()
  "Agent module loads and exports entrypoint."
  (require 'carriage-agent)
  (should (fboundp 'carriage-agent-main)))

(defun carriage-agent-test--split-head-body (s)
  (cond
   ((string-match "\r\n\r\n" s)
    (cons (substring s 0 (match-beginning 0))
          (substring s (match-end 0))))
   ((string-match "\n\n" s)
    (cons (substring s 0 (match-beginning 0))
          (substring s (match-end 0))))
   (t (cons s ""))))

(defun carriage-agent-test--http (port req &optional keep-open)
  (let* ((buf "")
         (done nil)
         (deadline (+ (float-time) 2.0))
         (p (make-network-process
             :name "carriage-agent-test-client"
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
    (unless keep-open
      (ignore-errors (delete-process p)))
    (cons p buf)))

(ert-deftest carriage-agent-web--auth-required-for-health ()
  (require 'carriage-web)
  (let* ((carriage-web-daemon-p t)
         (carriage-web-bind "127.0.0.1")
         (carriage-web-port 0)
         (carriage-web-auth-token "tok")
         (srv nil))
    (unwind-protect
        (progn
          (setq srv (carriage-web-start))
          (let* ((port (process-contact srv :service))
                 (resp (cdr (carriage-agent-test--http
                             port
                             "GET /api/health HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n")))
                 (pair (carriage-agent-test--split-head-body resp))
                 (body (cdr pair)))
            (should (string-match-p "401 Unauthorized" resp))
            (should (string-match-p "WEB_E_AUTH" body))))
      (ignore-errors (carriage-web-stop)))))

(ert-deftest carriage-agent-web--stream-does-not-accept-query-token ()
  (require 'carriage-web)
  (let* ((carriage-web-daemon-p t)
         (carriage-web-bind "127.0.0.1")
         (carriage-web-port 0)
         (carriage-web-auth-token "tok")
         (srv nil))
    (unwind-protect
        (progn
          (setq srv (carriage-web-start))
          (let* ((port (process-contact srv :service))
                 (resp (cdr (carriage-agent-test--http
                             port
                             "GET /stream?token=tok HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n")))
                 (pair (carriage-agent-test--split-head-body resp))
                 (body (cdr pair)))
            (should (string-match-p "401 Unauthorized" resp))
            (should (string-match-p "WEB_E_AUTH" body))))
      (ignore-errors (carriage-web-stop)))))

(ert-deftest carriage-agent-web--early-reject-oversize-content-length ()
  "Agent rejects oversized requests based on Content-Length before buffering the body."
  (require 'carriage-web)
  (let* ((carriage-web-daemon-p t)
         (carriage-web-bind "127.0.0.1")
         (carriage-web-port 0)
         (carriage-web-auth-token "tok")
         (carriage-web-max-request-bytes 10)
         (srv nil))
    (unwind-protect
        (progn
          (setq srv (carriage-web-start))
          (let* ((port (process-contact srv :service))
                 (resp (cdr (carriage-agent-test--http
                             port
                             (concat
                              "POST /api/cmd HTTP/1.1\r\n"
                              "Host: 127.0.0.1\r\n"
                              "Content-Type: application/json\r\n"
                              "Content-Length: 999\r\n"
                              "Connection: close\r\n"
                              "X-Auth: tok\r\n"
                              "\r\n"))))
                 (pair (carriage-agent-test--split-head-body resp))
                 (body (cdr pair)))
            (should (string-match-p "413 Payload Too Large" resp))
            (should (string-match-p "WEB_E_PAYLOAD" body))))
      (ignore-errors (carriage-web-stop)))))

(ert-deftest carriage-agent-web--sse-cap-enforced ()
  (require 'carriage-web)
  (let* ((carriage-web-daemon-p t)
         (carriage-web-bind "127.0.0.1")
         (carriage-web-port 0)
         (carriage-web-auth-token "tok")
         (carriage-web-max-sse-clients 1)
         (srv nil)
         (p1 nil))
    (unwind-protect
        (progn
          (setq srv (carriage-web-start))
          (let* ((port (process-contact srv :service))
                 (r1 (carriage-agent-test--http
                      port
                      (concat
                       "GET /stream HTTP/1.1\r\n"
                       "Host: 127.0.0.1\r\n"
                       "Accept: text/event-stream\r\n"
                       "Connection: keep-alive\r\n"
                       "X-Auth: tok\r\n"
                       "\r\n")
                      t)))
            (setq p1 (car r1))
            (should (string-match-p "200 OK" (cdr r1)))
            (let* ((r2 (cdr (carriage-agent-test--http
                             port
                             (concat
                              "GET /stream HTTP/1.1\r\n"
                              "Host: 127.0.0.1\r\n"
                              "Accept: text/event-stream\r\n"
                              "Connection: close\r\n"
                              "X-Auth: tok\r\n"
                              "\r\n"))))
                   (pair (carriage-agent-test--split-head-body r2))
                   (body (cdr pair)))
              (should (string-match-p "503 Service Unavailable" r2))
              (should (string-match-p "WEB_E_CAP" body)))))
      (ignore-errors (when (processp p1) (delete-process p1)))
      (ignore-errors (carriage-web-stop)))))

(provide 'carriage-agent-server-tests)
;;; carriage-agent-server-tests.el ends here
