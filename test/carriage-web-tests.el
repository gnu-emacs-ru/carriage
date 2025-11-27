;;; carriage-web-tests.el --- ERT tests for carriage-web HTTP/SSE utils  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Carriage Team <dev@carriage>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (ert "0"))
;; Keywords: tools, ui, http, sse, test
;;
;; Specifications:
;;   spec/spec-on-specs.org
;;   spec/web-dashboard-v1.org
;;   spec/code-style-v2.org
;;   spec/code-style-essentials-v2.org
;;
;;; Commentary:
;; Minimal unit tests covering HTTP parsing helpers and JSON truncation
;; from carriage-web.el. These tests do not open sockets; they validate
;; pure helper behavior and invariants required by the spec.
;;
;;; Code:

(require 'ert)
(require 'carriage-web)

(ert-deftest carriage-web--parse-start-line-basic ()
  "Parse a simple HTTP start line with query."
  (let* ((triple (carriage-web--parse-start-line "GET /api/health?x=1&y=foo HTTP/1.1"))
         (method (nth 0 triple))
         (path   (nth 1 triple))
         (query  (nth 2 triple)))
    (should (equal method "GET"))
    (should (equal path "/api/health"))
    (should (equal (assoc-default "x" query nil #'string=) "1"))
    (should (equal (assoc-default "y" query nil #'string=) "foo"))))

(ert-deftest carriage-web--parse-start-line-no-query ()
  "Parse an HTTP start line without query."
  (let* ((triple (carriage-web--parse-start-line "POST /api/cmd HTTP/1.1"))
         (method (nth 0 triple))
         (path   (nth 1 triple))
         (query  (nth 2 triple)))
    (should (equal method "POST"))
    (should (equal path "/api/cmd"))
    (should (null query))))

(ert-deftest carriage-web--parse-headers-basic ()
  "Parse a small set of headers; keys are downcased."
  (let* ((lines '("Host: localhost:8787"
                  "Content-Type: application/json"
                  "X-Auth: token-123"))
         (hdrs (carriage-web--parse-headers lines)))
    (should (equal (assoc-default "host" hdrs nil #'string=) "localhost:8787"))
    (should (equal (assoc-default "content-type" hdrs nil #'string=) "application/json"))
    (should (equal (assoc-default "x-auth" hdrs nil #'string=) "token-123"))))

(ert-deftest carriage-web--truncate-json-max-bytes ()
  "Ensure JSON encoder applies byte-size truncation limit."
  (let* ((obj `(:ok t :data ,(make-string 4000 ?x)))
         ;; Force small limit to exercise truncation:
         (carriage-web-max-json-bytes 512)
         (json (carriage-web--json obj)))
    (should (stringp json))
    (should (<= (string-bytes json) 512))
    ;; Must still be valid UTF-8 (decode successfully).
    (should (stringp (decode-coding-string json 'utf-8)))))

(ert-deftest carriage-web--buffer-id-stability ()
  "Compute stable ids; ephemeral buffers get a prefixed id."
  (with-temp-buffer
    (rename-buffer "carriage-web-test-ephemeral" t)
    (let ((id (carriage-web--buffer-id (current-buffer))))
      (should (stringp id))
      (should (string-match-p "\\`ephemeral:" id)))))

(ert-deftest carriage-web--parse-start-line-bad ()
  "Return nil for malformed HTTP start lines."
  (should (null (carriage-web--parse-start-line "GARBAGE")))
  (should (null (carriage-web--parse-start-line ""))))

(ert-deftest carriage-web--parse-headers-robust ()
  "Ignore malformed header lines without a colon."
  (let* ((lines '("Host: localhost" "BadHeader Line" "X-Auth: token"))
         (hdrs (carriage-web--parse-headers lines)))
    (should (equal (assoc-default "host" hdrs nil #'string=) "localhost"))
    (should (equal (assoc-default "x-auth" hdrs nil #'string=) "token"))
    ;; Ensure malformed line is ignored
    (should (null (assoc "badheader line" hdrs)))))

(ert-deftest carriage-web--dispatch-health-ok ()
  "Dispatcher returns ok envelope for GET /api/health."
  (let (cap-status cap-payload)
    (cl-letf (((symbol-function 'carriage-web--send-json)
               (lambda (_proc status payload)
                 (setq cap-status status
                       cap-payload payload))))
      (carriage-web--dispatch-request
       nil (list :method "GET" :path "/api/health" :query nil :headers '())))
    (should (equal cap-status "200 OK"))
    (should (eq (plist-get cap-payload :ok) t))
    (let ((data (plist-get cap-payload :data)))
      (should (listp data))
      (should (plist-get data :version))
      (should (plist-get data :engine))
      (should (numberp (plist-get data :ts))))))

(ert-deftest carriage-web--dispatch-404-not-found ()
  "Unknown route returns WEB_E_NOT_FOUND with ok=false."
  (let (cap-status cap-payload)
    (cl-letf (((symbol-function 'carriage-web--send-json)
               (lambda (_proc status payload)
                 (setq cap-status status
                       cap-payload payload))))
      (carriage-web--dispatch-request
       nil (list :method "GET" :path "/nope" :query nil :headers '())))
    (should (equal cap-status "404 Not Found"))
    (should (eq (plist-get cap-payload :ok) json-false))
    (should (equal (plist-get cap-payload :code) "WEB_E_NOT_FOUND"))))

(ert-deftest carriage-web--dispatch-sessions-ok-empty ()
  "GET /api/sessions returns ok envelope and a list (possibly empty)."
  (let (cap-status cap-payload)
    (cl-letf (((symbol-function 'carriage-web--send-json)
               (lambda (_proc status payload)
                 (setq cap-status status
                       cap-payload payload))))
      (carriage-web--dispatch-request
       nil (list :method "GET" :path "/api/sessions" :query nil :headers '())))
    (should (equal cap-status "200 OK"))
    (should (eq (plist-get cap-payload :ok) t))
    (let ((data (plist-get cap-payload :data)))
      (should (listp data)))))

(ert-deftest carriage-web--dispatch-cmd-unknown ()
  "POST /api/cmd with unknown command → 400 WEB_E_CMD and ok=false."
  (let* ((body (json-encode '(:cmd "unknown" :doc "ephemeral:test"))))
    (let (cap-status cap-payload)
      (cl-letf (((symbol-function 'carriage-web--send-json)
                 (lambda (_proc status payload)
                   (setq cap-status status
                         cap-payload payload))))
        (carriage-web--dispatch-request
         nil (list :method "POST" :path "/api/cmd" :query nil
                   :headers '(("content-type" . "application/json"))
                   :body body)))
      (should (equal cap-status "400 Bad Request"))
      (should (eq (plist-get cap-payload :ok) json-false))
      (should (equal (plist-get cap-payload :code) "WEB_E_CMD")))))

(ert-deftest carriage-web--dispatch-auth-required ()
  "With X-Auth token set, endpoints without header must return WEB_E_AUTH; with header → 200."
  (let ((carriage-web-auth-token "t0k"))
    (unwind-protect
        (let (cap-status cap-payload)
          (cl-letf (((symbol-function 'carriage-web--send-json)
                     (lambda (_proc status payload)
                       (setq cap-status status
                             cap-payload payload))))
            ;; No header → 401
            (carriage-web--dispatch-request
             nil (list :method "GET" :path "/api/health" :query nil :headers '())))
          (should (equal cap-status "401 Unauthorized"))
          (should (eq (plist-get cap-payload :ok) json-false))
          (should (equal (plist-get cap-payload :code) "WEB_E_AUTH"))
          ;; With header → 200 OK
          (setq cap-status nil cap-payload nil)
          (cl-letf (((symbol-function 'carriage-web--send-json)
                     (lambda (_proc status payload)
                       (setq cap-status status
                             cap-payload payload))))
            (carriage-web--dispatch-request
             nil (list :method "GET" :path "/api/health" :query nil
                       :headers '(("x-auth" . "t0k")))))
          (should (equal cap-status "200 OK"))
          (should (eq (plist-get cap-payload :ok) t)))
      (setq carriage-web-auth-token nil))))

(ert-deftest carriage-web--report-last-bad-request ()
  "GET /api/report/last without doc parameter returns WEB_E_PAYLOAD."
  (let (cap-status cap-payload)
    (cl-letf (((symbol-function 'carriage-web--send-json)
               (lambda (_proc status payload)
                 (setq cap-status status
                       cap-payload payload))))
      (carriage-web--dispatch-request
       nil (list :method "GET" :path "/api/report/last" :query nil :headers '())))
    (should (equal cap-status "400 Bad Request"))
    (should (eq (plist-get cap-payload :ok) json-false))
    (should (equal (plist-get cap-payload :code) "WEB_E_PAYLOAD"))))

(ert-deftest carriage-web--session-404 ()
  "GET /api/session/<id> returns WEB_E_NOT_FOUND for unknown id."
  (let (cap-status cap-payload)
    (cl-letf (((symbol-function 'carriage-web--send-json)
               (lambda (_proc status payload)
                 (setq cap-status status
                       cap-payload payload))))
      (carriage-web--dispatch-request
       nil (list :method "GET" :path "/api/session/nonexistent" :query nil :headers '())))
    (should (equal cap-status "404 Not Found"))
    (should (eq (plist-get cap-payload :ok) json-false))
    (should (equal (plist-get cap-payload :code) "WEB_E_NOT_FOUND"))))

(ert-deftest carriage-web--dispatch-cmd-abort-unsupported ()
  "POST /api/cmd abort returns WEB_E_CMD when no abort handler is available."
  (let* ((body (json-encode '(:cmd "abort" :doc "ephemeral:test"))))
    (let (cap-status cap-payload)
      (cl-letf (((symbol-function 'carriage-web--send-json)
                 (lambda (_proc status payload)
                   (setq cap-status status
                         cap-payload payload))))
        (carriage-web--dispatch-request
         nil (list :method "POST" :path "/api/cmd" :query nil
                   :headers '(("content-type" . "application/json"))
                   :body body)))
      (should (equal cap-status "400 Bad Request"))
      (should (eq (plist-get cap-payload :ok) json-false))
      (should (equal (plist-get cap-payload :code) "WEB_E_CMD")))))

(ert-deftest carriage-web--report-last-not-found ()
  "GET /api/report/last with unknown doc returns 404 WEB_E_NOT_FOUND."
  (let (cap-status cap-payload)
    (cl-letf (((symbol-function 'carriage-web--send-json)
               (lambda (_proc status payload)
                 (setq cap-status status
                       cap-payload payload))))
      (carriage-web--dispatch-request
       nil (list :method "GET" :path "/api/report/last"
                 :query '(("doc" . "nope")) :headers '())))
    (should (equal cap-status "404 Not Found"))
    (should (eq (plist-get cap-payload :ok) json-false))
    (should (equal (plist-get cap-payload :code) "WEB_E_NOT_FOUND"))))

(ert-deftest carriage-web--dispatch-cmd-toggle-bad-payload ()
  "POST /api/cmd toggle without key/doc returns 400 WEB_E_PAYLOAD."
  (let* ((body (json-encode '(:cmd "toggle")))  ; missing both :key and :doc
         cap-status cap-payload)
    (cl-letf (((symbol-function 'carriage-web--send-json)
               (lambda (_proc status payload)
                 (setq cap-status status
                       cap-payload payload))))
      (carriage-web--dispatch-request
       nil (list :method "POST" :path "/api/cmd" :query nil
                 :headers '(("content-type" . "application/json"))
                 :body body)))
    (should (equal cap-status "400 Bad Request"))
    (should (eq (plist-get cap-payload :ok) json-false))
    (should (equal (plist-get cap-payload :code) "WEB_E_PAYLOAD"))))

(ert-deftest carriage-web--http-health-graceful-close ()
  "Non-SSE response schedules a graceful close and includes Connection: close."
  (require 'cl-lib)
  (let ((outs '())
        (scheduled '()))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc s) (push s outs)))
              ((symbol-function 'run-at-time)
               (lambda (sec _repeat fn &rest args)
                 (push (list sec fn args) scheduled)
                 nil))
              ((symbol-function 'process-send-eof)
               (lambda (&rest _args) (push :eof outs))))
      (let* ((p (make-pipe-process :name "cw-test-health" :noquery t))
             (buf (generate-new-buffer " *cw-health*")))
        (unwind-protect
            (progn
              (set-process-buffer p buf)
              (carriage-web--process-filter
               p "GET /api/health HTTP/1.1\r\nHost: x\r\n\r\n")
              (let ((resp (apply #'concat (nreverse outs))))
                (should (string-match-p "HTTP/1.1 200 OK" resp))
                (should (string-match-p "Connection: close" resp)))
              (should scheduled)
              ;; Verify scheduled delay equals configured close delay
              (should (equal (caar scheduled) carriage-web-close-delay)))
          (when (process-live-p p) (ignore-errors (delete-process p)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest carriage-web--http-sse-unauthorized-graceful-close ()
  "Unauthorized SSE handshake returns 401 JSON and schedules graceful close."
  (require 'cl-lib)
  (let ((outs '())
        (scheduled '())
        (carriage-web-auth-token "t0k"))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc s) (push s outs)))
              ((symbol-function 'run-at-time)
               (lambda (sec _repeat fn &rest args)
                 (push (list sec fn args) scheduled)
                 nil))
              ((symbol-function 'process-send-eof)
               (lambda (&rest _args) (push :eof outs))))
      (let* ((p (make-pipe-process :name "cw-test-sse-unauth" :noquery t))
             (buf (generate-new-buffer " *cw-sse-unauth*")))
        (unwind-protect
            (progn
              (set-process-buffer p buf)
              (carriage-web--process-filter
               p "GET /stream HTTP/1.1\r\nHost: x\r\n\r\n")
              (let ((resp (apply #'concat (nreverse outs))))
                (should (string-match-p "HTTP/1.1 401 Unauthorized" resp))
                (should (string-match-p "application/json" resp))
                (should (string-match-p "\"ok\":false\\|\"ok\":false" resp)))
              (should scheduled)
              (should (equal (caar scheduled) carriage-web-close-delay)))
          (when (process-live-p p) (ignore-errors (delete-process p)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest carriage-web--http-sse-ok-keeps-open ()
  "Authorized SSE handshake responds with event-stream headers; no graceful close scheduled."
  (require 'cl-lib)
  (let ((outs '())
        (scheduled '())
        (carriage-web-auth-token nil))  ;; no token required
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc s) (push s outs)))
              ((symbol-function 'run-at-time)
               (lambda (sec _repeat fn &rest args)
                 (push (list sec fn args) scheduled)
                 nil))
              ((symbol-function 'process-send-eof)
               (lambda (&rest _args) (push :eof outs))))
      (let* ((p (make-pipe-process :name "cw-test-sse-ok" :noquery t))
             (buf (generate-new-buffer " *cw-sse-ok*")))
        (unwind-protect
            (progn
              (set-process-buffer p buf)
              (carriage-web--process-filter
               p "GET /stream HTTP/1.1\r\nHost: x\r\n\r\n")
              (let ((resp (apply #'concat (nreverse outs))))
                (should (string-match-p "HTTP/1.1 200 OK" resp))
                (should (string-match-p "text/event-stream" resp)))
              ;; For SSE we must not schedule a close
              (should (null scheduled)))
          (when (process-live-p p) (ignore-errors (delete-process p)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;; ---------------------------------------------------------------------
;; Server start/stop diagnostics (no real sockets; stubs)

(ert-deftest carriage-web--start-fallback-ephemeral-port ()
  "When binding fixed port fails, fallback to ephemeral service 0 and adopt its port."
  (let* ((carriage-web-bind "127.0.0.1")
         (carriage-web-port 55555)
         (calls 0)
         (fake-proc (make-pipe-process :name "cw-start-fallback" :noquery t))
         (got-args nil)
         (orig-server carriage-web--server-proc)
         (orig-heart (and (boundp 'carriage-web--heartbeat-timer) carriage-web--heartbeat-timer)))
    (unwind-protect
        (cl-letf (((symbol-function 'make-network-process)
                   (lambda (&rest args)
                     (setq got-args args)
                     (setq calls (1+ calls))
                     (if (= calls 1)
                         ;; First attempt simulates bind failure on fixed port
                         (signal 'file-error '("bind" "Address already in use"))
                       ;; Second attempt: ephemeral port (service 0) OK → return fake process
                       fake-proc)))
                  ((symbol-function 'process-contact)
                   (lambda (proc &rest _)
                     ;; Report chosen host/service for the server proc
                     (list :host "127.0.0.1" :service 44321))))
          (setq carriage-web--server-proc nil)
          (when (timerp carriage-web--heartbeat-timer)
            (cancel-timer carriage-web--heartbeat-timer))
          (setq carriage-web--heartbeat-timer nil)
          (should (carriage-web-start))
          (should (process-live-p carriage-web--server-proc))
          (should (= calls 2))                     ;; fallback path taken
          (should (equal (plist-get (apply #'list got-args) :server) t))
          (should (equal carriage-web-bind "127.0.0.1"))
          ;; Port should be adopted from process-contact (fallback)
          (should (equal carriage-web-port 44321)))
      ;; Cleanup
      (ignore-errors (when (process-live-p fake-proc) (delete-process fake-proc)))
      (when (timerp carriage-web--heartbeat-timer)
        (cancel-timer carriage-web--heartbeat-timer))
      (setq carriage-web--heartbeat-timer nil
            carriage-web--server-proc orig-server)
      (when (timerp orig-heart)
        (setq carriage-web--heartbeat-timer orig-heart)))))

(ert-deftest carriage-web--start-already-running-no-rebind ()
  "If server is already running, carriage-web-start should not attempt to rebind."
  (let* ((fake-proc (make-pipe-process :name "cw-already" :noquery t))
         (orig-server carriage-web--server-proc)
         (attempts 0))
    (unwind-protect
        (cl-letf (((symbol-function 'make-network-process)
                   (lambda (&rest _args)
                     (setq attempts (1+ attempts))
                     (make-pipe-process :name "cw-should-not" :noquery t))))
          (setq carriage-web--server-proc fake-proc)
          (should (carriage-web-start))
          ;; Ensure no new make-network-process attempt was made
          (should (= attempts 0)))
      (ignore-errors (when (process-live-p fake-proc) (delete-process fake-proc)))
      (setq carriage-web--server-proc orig-server))))

(ert-deftest carriage-web--stop-kills-server-and-clients ()
  "Stop should disconnect SSE clients and kill server process."
  (let* ((server (make-pipe-process :name "cw-stop-server" :noquery t))
         (cli1 (make-pipe-process :name "cw-stop-cli1" :noquery t))
         (cli2 (make-pipe-process :name "cw-stop-cli2" :noquery t))
         (carriage-web--clients (list (list :proc cli1) (list :proc cli2)))
         (carriage-web--server-proc server)
         (deleted '()))
    (unwind-protect
        (cl-letf (((symbol-function 'delete-process)
                   (lambda (p) (push (process-name p) deleted) (ignore))))
          (should (carriage-web-stop))
          ;; Server and both clients should be scheduled for deletion
          (should (member "cw-stop-server" deleted))
          (should (member "cw-stop-cli1" deleted))
          (should (member "cw-stop-cli2" deleted)))
      (dolist (p (list server cli1 cli2))
        (ignore-errors (when (process-live-p p) (delete-process p))))
      (setq carriage-web--server-proc nil
            carriage-web--clients nil))))

;; ---------------------------------------------------------------------
;; HTTP root and malformed request behaviors

(ert-deftest carriage-web--http-root-serves-html ()
  "GET / should return 200 text/html and schedule graceful close."
  (require 'cl-lib)
  (let ((outs '())
        (scheduled '()))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc s) (push s outs)))
              ((symbol-function 'run-at-time)
               (lambda (sec _repeat fn &rest args)
                 (push (list sec fn args) scheduled)
                 nil))
              ((symbol-function 'process-send-eof)
               (lambda (&rest _args) (push :eof outs))))
      (let* ((p (make-pipe-process :name "cw-test-root" :noquery t))
             (buf (generate-new-buffer " *cw-root*")))
        (unwind-protect
            (progn
              (set-process-buffer p buf)
              (carriage-web--process-filter
               p "GET / HTTP/1.1\r\nHost: x\r\n\r\n")
              (let ((resp (apply #'concat (nreverse outs))))
                (should (string-match-p "\\`HTTP/1.1 200 OK" resp))
                (should (string-match-p "Content-Type: text/html" resp))
                (should (string-match-p "Connection: close" resp))
                ;; Content-Length must match actual body bytes
                (let* ((parts (split-string resp "\r\n\r\n\\|\n\n"))
                       (hdr (car parts))
                       (body (cadr parts))
                       (cl (and (string-match "Content-Length: \\([0-9]+\\)" hdr)
                                (string-to-number (match-string 1 hdr)))))
                  (should (numberp cl))
                  (should (= cl (string-bytes (or body "")))))))
              ;; For non-SSE we should schedule a graceful close
              (should scheduled)
              (should (equal (caar scheduled) carriage-web-close-delay)))
          (when (process-live-p p) (ignore-errors (delete-process p)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest carriage-web--bad-request-immediate-close ()
  "Malformed start-line should yield 400 JSON and immediate close (delete-process).
This reproduces the path that can cause a TCP RST if the peer is too fast."
  (require 'cl-lib)
  (let ((outs '())
        (deleted '())
        (scheduled '()))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc s) (push s outs)))
              ;; In the bad-request path we do not want graceful close scheduling.
              ((symbol-function 'run-at-time)
               (lambda (_sec _repeat _fn &rest _args)
                 (push :scheduled scheduled)
                 nil))
              ((symbol-function 'delete-process)
               (lambda (p) (push (process-name p) deleted)))
              ((symbol-function 'process-send-eof)
               (lambda (&rest _args) (push :eof outs))))
      (let* ((p (make-pipe-process :name "cw-test-bad" :noquery t))
             (buf (generate-new-buffer " *cw-bad*")))
        (unwind-protect
            (progn
              (set-process-buffer p buf)
              ;; Send malformed request (no HTTP start line)
              (carriage-web--process-filter
               p "GARBAGE\r\n\r\n")
              (let ((resp (apply #'concat (nreverse outs))))
                (should (string-match-p "\\`HTTP/1.1 400 Bad Request" resp))
                (should (string-match-p "application/json" resp))
                (should (string-match-p "\"code\":\"WEB_E_PAYLOAD\"" resp)))
              ;; Expect immediate delete (RST risk in real socket)
              (should (member "cw-test-bad" deleted))
              ;; And importantly, no graceful scheduling expected
              (should (null scheduled)))
          (when (process-live-p p) (ignore-errors (delete-process p)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;; carriage-web-tests.el ends here
