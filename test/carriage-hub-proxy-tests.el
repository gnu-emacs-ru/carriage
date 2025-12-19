;;; carriage-hub-proxy-tests.el --- Hub unit/smoke tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)

(require 'carriage-swarm-registry)
(require 'carriage-hub)

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

(ert-deftest carriage-hub--proxy-not-found-yields-web-e-not-found ()
  (let* ((tmp (make-temp-file "carriage-hub-registry-" t))
         (carriage-swarm-registry-runtime-dir tmp)
         (carriage-hub-port 0)
         (carriage-hub-max-proxy-active 32))
    (unwind-protect
        (let* ((srv (carriage-hub-start))
               (port (process-contact srv :service))
               (resp (carriage-hub-test--http
                      port
                      "GET /hub/agent/nope/api/health HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"))
               (pair (carriage-hub-test--split-head-body resp))
               (body (cdr pair))
               (obj (condition-case _e
                        (let ((json-object-type 'plist)
                              (json-array-type 'list)
                              (json-false :false))
                          (json-read-from-string body))
                      (error nil))))
          (should (string-match-p "WEB_E_NOT_FOUND" body))
          (should (and (listp obj) (plist-get obj :code)))
          (should (string= (plist-get obj :code) "WEB_E_NOT_FOUND")))
      (ignore-errors (carriage-hub-stop)))))

(ert-deftest carriage-hub--proxy-cap-overflow-yields-web-e-cap ()
  (let* ((tmp (make-temp-file "carriage-hub-registry-" t))
         (carriage-swarm-registry-runtime-dir tmp)
         (carriage-hub-port 0)
         ;; Force immediate overflow regardless of registry state.
         (carriage-hub-max-proxy-active 0))
    (unwind-protect
        (let* ((srv (carriage-hub-start))
               (port (process-contact srv :service))
               (resp (carriage-hub-test--http
                      port
                      "GET /hub/agent/any/api/health HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"))
               (pair (carriage-hub-test--split-head-body resp))
               (body (cdr pair))
               (obj (condition-case _e
                        (let ((json-object-type 'plist)
                              (json-array-type 'list)
                              (json-false :false))
                          (json-read-from-string body))
                      (error nil))))
          (should (string-match-p "WEB_E_CAP" body))
          (should (and (listp obj) (plist-get obj :code)))
          (should (string= (plist-get obj :code) "WEB_E_CAP")))
      (ignore-errors (carriage-hub-stop)))))

(provide 'carriage-hub-proxy-tests)
;;; carriage-hub-proxy-tests.el ends here
