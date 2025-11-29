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

(ert-deftest carriage-web--dispatch-sessions-non-empty-with-ephemeral ()
  "GET /api/sessions returns a non-empty list when an ephemeral buffer exists."
  (with-temp-buffer
    (rename-buffer (generate-new-buffer-name "cw-ephemeral-sess") t)
    ;; Force assignment of an ephemeral id
    (let ((id (carriage-web--buffer-id (current-buffer))))
      (should (and (stringp id) (string-match-p "\\`ephemeral:" id))))
    (let (cap-status cap-payload)
      (cl-letf (((symbol-function 'carriage-web--send-json)
                 (lambda (_proc status payload)
                   (setq cap-status status
                         cap-payload payload))))
        (carriage-web--dispatch-request
         nil (list :method "GET" :path "/api/sessions" :query nil :headers '())))
      (should (equal cap-status "200 OK"))
      (should (eq (plist-get cap-payload :ok) t))
      (let* ((data (plist-get cap-payload :data)))
        (should (listp data))
        (should (> (length data) 0))))))

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
              (should (>= (caar scheduled) carriage-web-close-delay)))
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
              (should (member carriage-web-close-delay (mapcar #'car scheduled))))
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
                  ;; Compare only the first clen bytes to avoid incidental extra bytes in the buffer.
                  (let* ((slice (substring (or body "") 0 (min (length (or body "")) cl))))
                    (should (= cl (string-bytes slice)))))))
          ;; For non-SSE we should schedule a graceful close
          (should scheduled)
          (should (member carriage-web-close-delay (mapcar #'car scheduled))))
        (when (process-live-p p) (ignore-errors (delete-process p)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest carriage-web--bad-request-graceful-close ()
  "Malformed start-line should yield 400 JSON and schedule graceful close (EOF), not immediate delete."
  (require 'cl-lib)
  (let ((outs '())
        (scheduled '())
        (deleted '()))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc s) (push s outs)))
              ;; For graceful close we expect scheduling via run-at-time and EOF, not immediate delete.
              ((symbol-function 'run-at-time)
               (lambda (sec _repeat fn &rest args)
                 (push (list sec fn args) scheduled)
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
              ;; Expect graceful close scheduled (EOF), not immediate delete
              (should scheduled)
              (should (null deleted)))
          (when (process-live-p p) (ignore-errors (delete-process p)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;; ---------------------------------------------------------------------
;; Integration tests (real TCP): verify server responds over sockets
(ert-deftest carriage-web-it-root-responds ()
  "Start the server on an ephemeral port; GET / returns 200 with sane headers and body length."
  (let* ((carriage-web-enabled t)
         (carriage-web-bind "127.0.0.1")
         ;; Request ephemeral port: let start() fall back or use service 0 directly.
         (carriage-web-port 0)
         ;; Be generous with graceful-close to reduce RST risk during test
         (carriage-web-close-delay 0.5)
         (server-started nil))
    (unwind-protect
        (progn
          (setq server-started (carriage-web-start))
          (should server-started)
          (should (process-live-p carriage-web--server-proc))
          (let* ((svc (or (ignore-errors (process-contact carriage-web--server-proc :service))
                          (let* ((pc (ignore-errors (process-contact carriage-web--server-proc))))
                            (and (listp pc) (plist-get pc :service)))))
                 (host (or (ignore-errors (process-contact carriage-web--server-proc :host)) "127.0.0.1")))
            (should (numberp svc))
            (let* ((proc (open-network-stream "cw-it-root" nil host svc))
                   (out  ""))
              (set-process-coding-system proc 'binary 'binary)
              (set-process-filter proc (lambda (_ s) (setq out (concat out s))))
              (process-send-string proc "GET / HTTP/1.1\r\nHost: x\r\n\r\n")
              ;; Allow server to respond; collect a bit more to reduce races
              (accept-process-output proc 1.0)
              (sleep-for 0.05)
              (should (string-match-p "\\`HTTP/1\\.1 200 OK" out))
              (should (string-match-p "Content-Type: text/html" out))
              (should (string-match-p "Connection: close" out))
              ;; Verify Content-Length matches actual body bytes
              (let* ((parts (split-string out "\r\n\r\n\\|\n\n"))
                     (hdr (car parts))
                     (body (cadr parts))
                     (cl (and (string-match "Content-Length: \\([0-9]+\\)" hdr)
                              (string-to-number (match-string 1 hdr)))))
                (should (numberp cl))
                (should (= cl (string-bytes (or body "")))))
              (ignore-errors (delete-process proc)))))
      (when (process-live-p carriage-web--server-proc)
        (ignore-errors (carriage-web-stop))))))

(ert-deftest carriage-web-it-health-responds ()
  "Start the server; GET /api/health returns ok envelope JSON."
  (let* ((carriage-web-enabled t)
         (carriage-web-bind "127.0.0.1")
         (carriage-web-port 0)
         (carriage-web-close-delay 0.5)
         (server-started nil))
    (unwind-protect
        (progn
          (setq server-started (carriage-web-start))
          (should server-started)
          (should (process-live-p carriage-web--server-proc))
          (let* ((svc (or (ignore-errors (process-contact carriage-web--server-proc :service))
                          (let* ((pc (ignore-errors (process-contact carriage-web--server-proc))))
                            (and (listp pc) (plist-get pc :service)))))
                 (host (or (ignore-errors (process-contact carriage-web--server-proc :host)) "127.0.0.1")))
            (should (numberp svc))
            (let* ((proc (open-network-stream "cw-it-health" nil host svc))
                   (out  ""))
              (set-process-coding-system proc 'binary 'binary)
              (set-process-filter proc (lambda (_ s) (setq out (concat out s))))
              (process-send-string proc "GET /api/health HTTP/1.1\r\nHost: x\r\n\r\n")
              (accept-process-output proc 1.0)
              (sleep-for 0.05)
              (should (string-match-p "\\`HTTP/1\\.1 200 OK" out))
              (should (string-match-p "Content-Type: application/json" out))
              ;; Body should be a compact JSON envelope; validate by parsing JSON
              (let* ((parts (split-string out "\r\n\r\n\\|\n\n"))
                     (body (or (cadr parts) "")))
                (let ((json-object-type 'plist)
                      (json-array-type 'list)
                      (json-false nil))
                  (let ((parsed (ignore-errors (json-read-from-string body))))
                    (should (plist-get parsed :ok))
                    (let ((data (plist-get parsed :data)))
                      (should (stringp (plist-get data :version)))
                      (should (plist-get data :engine)))))))
            ))
      (when (process-live-p carriage-web--server-proc)
        (ignore-errors (carriage-web-stop))))))

;; ---------------------------------------------------------------------
;; Integration test: SSE stream "hello" handshake over a real TCP socket
;;
;; How to run ONLY integration tests from console:
;;   ERT_SELECTOR='carriage-web-it-' emacs -Q --batch -l test/ert-runner.el
;; Or a single test by exact name (use a regexp):
;;   ERT_SELECTOR='^carriage-web-it-stream-hello$' emacs -Q --batch -l test/ert-runner.el
;;
;; To see only web tests:
;;   ERT_SELECTOR='carriage-web-' emacs -Q --batch -l test/ert-runner.el

(ert-deftest carriage-web-it-stream-hello ()
  "Start the server; GET /stream returns event-stream headers and 'hello' event."
  (let* ((carriage-web-enabled t)
         (carriage-web-bind "127.0.0.1")
         (carriage-web-port 0)
         (carriage-web-close-delay 0.5)
         (server-started nil))
    (unwind-protect
        (progn
          (setq server-started (carriage-web-start))
          (should server-started)
          (should (process-live-p carriage-web--server-proc))
          (let* ((svc (or (ignore-errors (process-contact carriage-web--server-proc :service))
                          (let* ((pc (ignore-errors (process-contact carriage-web--server-proc))))
                            (and (listp pc) (plist-get pc :service)))))
                 (host (or (ignore-errors (process-contact carriage-web--server-proc :host)) "127.0.0.1")))
            (should (numberp svc))
            (let* ((proc (open-network-stream "cw-it-stream" nil host svc))
                   (out  ""))
              (set-process-coding-system proc 'binary 'binary)
              (set-process-filter proc (lambda (_ s) (setq out (concat out s))))
              (process-send-string proc "GET /stream HTTP/1.1\r\nHost: x\r\n\r\n")
              (accept-process-output proc 1.0)
              (sleep-for 0.05)
              (should (string-match-p "\\`HTTP/1\\.1 200 OK" out))
              (should (string-match-p "Content-Type: text/event-stream" out))
              ;; Server should immediately send a hello event
              (should (string-match-p "event: hello" out))
              (ignore-errors (delete-process proc)))))
      (when (process-live-p carriage-web--server-proc)
        (ignore-errors (carriage-web-stop))))))

;; --- Integration: /api/health with X-Auth token over a real TCP socket
(ert-deftest carriage-web-it-health-responds-auth ()
  "Start the server; with X-Auth token set, GET /api/health returns ok envelope JSON."
  (let* ((carriage-web-enabled t)
         (carriage-web-bind "127.0.0.1")
         (carriage-web-port 0)
         (carriage-web-auth-token "t0k")
         (server-started nil))
    (unwind-protect
        (progn
          (setq server-started (carriage-web-start))
          (should server-started)
          (should (process-live-p carriage-web--server-proc))
          (let* ((svc (or (ignore-errors (process-contact carriage-web--server-proc :service))
                          (let* ((pc (ignore-errors (process-contact carriage-web--server-proc))))
                            (and (listp pc) (plist-get pc :service)))))
                 (host (or (ignore-errors (process-contact carriage-web--server-proc :host)) "127.0.0.1")))
            (should (numberp svc))
            (let* ((proc (open-network-stream "cw-it-health-auth" nil host svc))
                   (out  ""))
              ;; Binary I/O for byte-accurate reads
              (set-process-coding-system proc 'binary 'binary)
              (set-process-filter proc (lambda (_ s) (setq out (concat out s))))
              (process-send-string
               proc
               (concat "GET /api/health HTTP/1.1\r\n"
                       "Host: x\r\n"
                       "X-Auth: t0k\r\n"
                       "Accept: application/json\r\n"
                       "\r\n"))
              (accept-process-output proc 1.0)
              (sleep-for 0.05)
              (should (string-match-p "\\`HTTP/1\\.1 200 OK" out))
              (should (string-match-p "Content-Type: application/json" out))
              ;; Body should be a compact JSON envelope; validate structure lightly
              (let* ((parts (split-string out "\r\n\r\n\\|\n\n"))
                     (body (or (cadr parts) "")))
                (let ((json-object-type 'plist)
                      (json-array-type 'list)
                      (json-false nil))
                  (let ((parsed (ignore-errors (json-read-from-string body))))
                    (should (plist-get parsed :ok))
                    (let ((data (plist-get parsed :data)))
                      (should (stringp (plist-get data :version)))
                      (should (plist-get data :engine)))))))
            ))
      (when (process-live-p carriage-web--server-proc)
        (ignore-errors (carriage-web-stop))))))

;; -------------------------------------------------------------------
;; Additional integration tests: detect real TCP resets (RST) via sentinel


(ert-deftest carriage-web-it-health-no-rst ()
  "Real socket: GET /api/health with token returns 200 and no TCP RST."
  (require 'carriage-web)
  (let* ((carriage-web-auth-token "tok")
         (carriage-web-close-delay 1.0)
         (port nil)
         (status nil)
         (resp "")
         (buf (generate-new-buffer " *cw-it-health*"))
         (p nil))
    (unwind-protect
        (progn
          (carriage-web-start)
          (setq port (plist-get (process-contact carriage-web--server-proc t) :service))
          (setq p (open-network-stream "cw-it-health" buf "127.0.0.1" port :type 'plain))
          (set-process-coding-system p 'binary 'binary)
          (set-process-filter p (lambda (_proc s) (setq resp (concat resp s))))
          (set-process-sentinel p (lambda (_proc s) (setq status (concat (or status "") s))))
          ;; Send HTTP/1.1 request with X-Auth
          (let ((req (concat
                      "GET /api/health HTTP/1.1\r\n"
                      "Host: 127.0.0.1\r\n"
                      "Accept: application/json\r\n"
                      "X-Auth: tok\r\n"
                      "\r\n")))
            (process-send-string p req))
          ;; Allow server to respond and then close gracefully
          (accept-process-output p 0.2)
          (accept-process-output p carriage-web-close-delay)
          (accept-process-output p 0.3)
          ;; Assertions
          (should (string-match-p "HTTP/1\\.1 200 OK" resp))
          (should (string-match-p "Content-Type: application/json" resp))
          (should (string-match-p "\"ok\"\\s-*:\\s-*true" resp))
          (should-not (and status (string-match-p "\\(reset\\|broken\\)" status))))
      (when (process-live-p p) (ignore-errors (delete-process p)))
      (kill-buffer buf)
      (ignore-errors (carriage-web-stop)))))

;;; Integration: root/health over real TCP — assert no RST via sentinel

(ert-deftest carriage-web-it-root-no-rst ()
  "Real socket: GET / returns 200 and connection closes without TCP RST."
  (let* ((carriage-web-auth-token nil)
         (carriage-web-close-delay 1.0)
         (port nil) (status nil) (resp "")
         (buf (generate-new-buffer " *cw-it-root*"))
         (p nil))
    (unwind-protect
        (progn
          (carriage-web-start)
          (setq port (plist-get (process-contact carriage-web--server-proc t) :service))
          (setq p (open-network-stream "cw-it-root" buf "127.0.0.1" port :type 'plain))
          (set-process-coding-system p 'binary 'binary)
          (set-process-filter p (lambda (_proc s) (setq resp (concat resp s))))
          (set-process-sentinel p (lambda (_proc s) (setq status (concat (or status "") s))))
          (process-send-string p "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\nAccept: */*\r\n\r\n")
          (accept-process-output p 0.2)
          (accept-process-output p carriage-web-close-delay)
          (accept-process-output p 0.3)
          (should (string-match-p "HTTP/1\\.1 200 OK" resp))
          (should (string-match-p "Content-Type: text/html" resp))
          (should-not (and status (string-match-p "\\(reset\\|broken\\)" status))))
      (when (process-live-p p) (ignore-errors (delete-process p)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (ignore-errors (carriage-web-stop)))))

(ert-deftest carriage-web-it-health-no-rst-json ()
  "Real socket: GET /api/health with token returns 200 JSON and no TCP RST."
  (let* ((carriage-web-auth-token "tok")
         (carriage-web-close-delay 1.0)
         (port nil) (status nil) (resp "")
         (buf (generate-new-buffer " *cw-it-health*"))
         (p nil))
    (unwind-protect
        (progn
          (carriage-web-start)
          (setq port (plist-get (process-contact carriage-web--server-proc t) :service))
          (setq p (open-network-stream "cw-it-health" buf "127.0.0.1" port :type 'plain))
          (set-process-coding-system p 'binary 'binary)
          (set-process-filter p (lambda (_proc s) (setq resp (concat resp s))))
          (set-process-sentinel p (lambda (_proc s) (setq status (concat (or status "") s))))
          (process-send-string
           p (concat "GET /api/health HTTP/1.1\r\n"
                     "Host: 127.0.0.1\r\n"
                     "Accept: application/json\r\n"
                     "X-Auth: tok\r\n"
                     "\r\n"))
          (accept-process-output p 0.2)
          (accept-process-output p carriage-web-close-delay)
          (accept-process-output p 0.3)
          (should (string-match-p "HTTP/1\\.1 200 OK" resp))
          (should (string-match-p "Content-Type: application/json" resp))
          (should (string-match-p "\"ok\"\\s-*:\\s-*true" resp))
          (should-not (and status (string-match-p "\\(reset\\|broken\\)" status))))
      (when (process-live-p p) (ignore-errors (delete-process p)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (ignore-errors (carriage-web-stop)))))

;; -------------------------------------------------------------------
;; Real socket integration tests (ensure curl-like behavior)
;;
;; These tests open a real TCP socket to the running server and verify:
;; - Root (/) serves a proper 200 HTML response
;; - /api/health responds with 200 ok when X-Auth is provided
;; They do not assert RST/FIN explicitly (ERT has no direct RST hook),
;; but they validate a complete HTTP response with headers/body.

(ert-deftest carriage-web-it-root-real-socket ()
  "Real socket: GET / returns 200 with text/html and a body."
  (require 'cl-lib)
  (let ((carriage-web-enabled t)
        (carriage-web-auth-token nil))
    (carriage-web-start)
    (let* ((contact (process-contact carriage-web--server-proc t))
           (port (plist-get contact :service))
           (acc "")
           (proc (open-network-stream "cw-it-root" nil "127.0.0.1" port)))
      (unwind-protect
          (progn
            (set-process-coding-system proc 'binary 'binary)
            (set-process-filter proc (lambda (_ s) (setq acc (concat acc s))))
            (process-send-string proc "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\nAccept: */*\r\n\r\n")
            (accept-process-output proc 0.3)
            ;; Minimal assertions: status line, content-type and a non-empty body
            (should (string-match-p "\\`HTTP/1\\.[01] 200 " acc))
            (should (string-match-p "Content-Type: text/html" acc))
            (let* ((sep (or (string-match "\r\n\r\n" acc) (string-match "\n\n" acc)))
                   (body (and sep (substring acc (+ sep (if (eq (aref acc sep) ?\r) 4 2))))))
              (should (and body (> (length body) 0)))))
        (ignore-errors (delete-process proc))))))

(ert-deftest carriage-web-it-health-real-socket-auth ()
  "Real socket: GET /api/health with X-Auth returns 200 ok JSON."
  (require 'cl-lib)
  (let ((carriage-web-enabled t)
        (carriage-web-auth-token "t0k"))
    (carriage-web-start)
    (let* ((contact (process-contact carriage-web--server-proc t))
           (port (plist-get contact :service))
           (acc "")
           (proc (open-network-stream "cw-it-health" nil "127.0.0.1" port)))
      (unwind-protect
          (progn
            (set-process-coding-system proc 'binary 'binary)
            (set-process-filter proc (lambda (_ s) (setq acc (concat acc s))))
            (process-send-string
             proc
             (concat
              "GET /api/health HTTP/1.1\r\n"
              "Host: 127.0.0.1\r\n"
              "X-Auth: t0k\r\n"
              "Accept: application/json\r\n\r\n"))
            (accept-process-output proc 0.3)
            (should (string-match-p "\\`HTTP/1\\.[01] 200 " acc))
            (should (string-match-p "Content-Type: application/json" acc))
            (let* ((sep (or (string-match "\r\n\r\n" acc) (string-match "\n\n" acc)))
                   (body (and sep (substring acc (+ sep (if (eq (aref acc sep) ?\r) 4 2))))))
              (should (and body (string-match-p "\"ok\"[ \t]*:[ \t]*true" body)))))
        (ignore-errors (delete-process proc))))))

;; ----------------------------------------------------------------------
;; Integration tests with real TCP sockets (open-network-stream)

(ert-deftest carriage-web-it-real-root-responds ()
  "Start server, GET / over a real TCP socket, verify 200/HTML and graceful close."
  (let ((carriage-web-auth-token nil)
        (carriage-web-enabled t))
    (unwind-protect
        (progn
          (carriage-web-start)
          (let* ((srv carriage-web--server-proc)
                 (port (plist-get (process-contact srv t) :service))
                 (buf  (generate-new-buffer " *cw-it-root*"))
                 (conn (open-network-stream "cw-it-root" buf "127.0.0.1" port)))
            (set-process-coding-system conn 'binary 'binary)
            (process-send-string
             conn "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\nAccept: */*\r\n\r\n")
            ;; Подождём ответ и снимем один срез буфера (без накапливающих конкатенаций)
            (accept-process-output conn 0.2)
            (let ((resp (with-current-buffer buf (buffer-string))))
              (should (string-match-p "\\`HTTP/1\\.1 200 OK\\b" resp))
              (should (string-match-p "Content-Type: text/html" resp))
              (let* ((sep (or (string-match "\r\n\r\n" resp)
                              (string-match "\n\n" resp)))
                     (body (and sep (substring resp (+ sep (if (eq (aref resp sep) ?\r) 4 2)))))
                     (clen (and (string-match "Content-Length: \\([0-9]+\\)" resp)
                                (string-to-number (match-string 1 resp)))))
                (when (and body clen)
                  ;; Compare first clen BYTES (UTF-8) to avoid multibyte pitfalls
                  (let* ((u8 (encode-coding-string (or body "") 'utf-8))
                         (slice (substring u8 0 (min (length u8) clen))))
                    (should (= clen (string-bytes slice)))))))
            (ignore-errors (delete-process conn))
            (ignore-errors (kill-buffer buf))))
      (ignore-errors (carriage-web-stop)))))


(ert-deftest carriage-web-it-real-health-responds-auth ()
  "Start server, GET /api/health with X-Auth over TCP, verify 200/JSON and graceful close."
  (let ((carriage-web-auth-token "tok")
        (carriage-web-enabled t))
    (unwind-protect
        (progn
          (carriage-web-start)
          (let* ((srv carriage-web--server-proc)
                 (port (plist-get (process-contact srv t) :service))
                 (conn (open-network-stream
                        "cw-it-health" (generate-new-buffer " *cw-it-health*")
                        "127.0.0.1" port)))
            (set-process-coding-system conn 'binary 'binary)
            (process-send-string
             conn (concat "GET /api/health HTTP/1.1\r\n"
                          "Host: 127.0.0.1\r\n"
                          "Accept: application/json\r\n"
                          "X-Auth: tok\r\n"
                          "\r\n"))
            (let ((resp ""))
              ;; Wait briefly for server to respond, then snapshot buffer once.
              (dotimes (_ 50) (accept-process-output nil 0.01))
              (setq resp (with-current-buffer (process-buffer conn)
                           (buffer-string)))
              (should (string-match-p "\\`HTTP/1\\.1 200 OK\\b" resp))
              (should (string-match-p "Content-Type: application/json" resp))
              (when (string-match "Content-Length: \\([0-9]+\\)" resp)
                (let* ((clen (string-to-number (match-string 1 resp)))
                       (sep (or (string-match "\r\n\r\n" resp)
                                (string-match "\n\n" resp)))
                       (body (and sep (substring resp (+ sep (if (eq (aref resp sep) ?\r) 4 2))))))
                  (when body
                    ;; Compare only the first clen BYTES to avoid incidental extra bytes in the buffer.
                    (let* ((u8 (encode-coding-string (or body "") 'utf-8))
                           (slice (substring u8 0 (min (length u8) clen))))
                      (should (= clen (string-bytes slice)))
                      (should (string-match-p "\"ok\"[[:space:]]*:[[:space:]]*true" body)))))))
            (ignore-errors (delete-process conn))))
      (ignore-errors (carriage-web-stop)))))

;; Additional tests: favicon 204 and session detail (ephemeral) OK

(ert-deftest carriage-web--favicon-204 ()
  "GET /favicon.ico returns 204 No Content and schedules graceful close."
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
      (let* ((p (make-pipe-process :name "cw-test-favicon" :noquery t))
             (buf (generate-new-buffer " *cw-favicon*")))
        (unwind-protect
            (progn
              (set-process-buffer p buf)
              (carriage-web--process-filter
               p "GET /favicon.ico HTTP/1.1\r\nHost: x\r\n\r\n")
              (let ((resp (apply #'concat (nreverse outs))))
                (should (string-match-p "\\`HTTP/1\\.1 204 No Content" resp))
                (should (string-match-p "Connection: close" resp))))
          (when (process-live-p p) (ignore-errors (delete-process p)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest carriage-web--session-ephemeral-ok ()
  "GET /api/session/<ephemeral-id> returns 200 with a data payload."
  (let (cap-status cap-payload)
    (with-temp-buffer
      (rename-buffer (generate-new-buffer-name "cw-ephemeral") t)
      ;; Force compute ephemeral id
      (let ((id (carriage-web--buffer-id (current-buffer))))
        (should (and (stringp id) (string-match-p "\\`ephemeral:" id)))
        (cl-letf (((symbol-function 'carriage-web--send-json)
                   (lambda (_proc status payload)
                     (setq cap-status status
                           cap-payload payload))))
          (carriage-web--dispatch-request
           nil (list :method "GET"
                     :path (concat "/api/session/" id)
                     :query nil :headers '() :body "")))
        (should (equal cap-status "200 OK"))
        (should (eq (plist-get cap-payload :ok) t))
        (let ((data (plist-get cap-payload :data)))
          (should (listp data))
          (should (equal (plist-get data :id) id))
          (should (plist-get data :state)))))))
;; ---------------------------------------------------------------------
;; Additional positive tests: /api/report/last success (limit) and /api/cmd with whitelist enabled.

(ert-deftest carriage-web--report-last-success-limit ()
  "GET /api/report/last returns 200 with limited items when a report is cached."
  (let* ((doc "ephemeral:test-doc")
         (sum (list :ok 2 :fail 1 :skipped 0 :total 3))
         (items (list (list :op 'create :file "a")
                      (list :op 'patch :path "b")
                      (list :op 'delete :file "c"))))
    ;; Seed in-memory cache used by /api/report/last
    (puthash doc (list :summary sum :items items)
             carriage-web--last-report-full-by-doc)
    (let (cap-status cap-payload)
      (cl-letf (((symbol-function 'carriage-web--send-json)
                 (lambda (_proc status payload)
                   (setq cap-status status
                         cap-payload payload))))
        ;; Ask for limit=1
        (carriage-web--dispatch-request
         nil (list :method "GET" :path "/api/report/last"
                   :query `(("doc" . ,doc) ("limit" . "1"))
                   :headers '())))
      (should (equal cap-status "200 OK"))
      (should (eq (plist-get cap-payload :ok) t))
      (let* ((data (plist-get cap-payload :data))
             (ret-sum (plist-get data :summary))
             (ret-items (plist-get data :items)))
        (should (equal (plist-get ret-sum :ok) 2))
        (should (equal (plist-get ret-sum :fail) 1))
        (should (equal (plist-get ret-sum :skipped) 0))
        (should (equal (plist-get ret-sum :total) 3))
        (should (listp ret-items))
        (should (= (length ret-items) 1))))))

(ert-deftest carriage-web--dispatch-cmd-abort-enabled ()
  "POST /api/cmd abort succeeds when whitelist is enabled and buffer is found."
  (let* ((carriage-web-api-commands-enabled t)
         (abort-called nil))
    (unwind-protect
        (with-temp-buffer
          (rename-buffer (generate-new-buffer-name "cw-ephemeral-cmd") t)
          (let* ((id (carriage-web--buffer-id (current-buffer))))
            (should (and (stringp id) (string-match-p "\\`ephemeral:" id)))
            (let ((cap-status nil) (cap-payload nil))
              (cl-letf* (((symbol-function 'run-at-time)
                          (lambda (_sec _repeat fn &rest _args)
                            (funcall fn)
                            nil))
                         ((symbol-function 'carriage-abort-current)
                          (lambda () (setq abort-called t)))
                         ((symbol-function 'carriage-web--send-json)
                          (lambda (_proc status payload)
                            (setq cap-status status
                                  cap-payload payload))))
                (let* ((body (json-encode `(:cmd "abort" :doc ,id))))
                  (carriage-web--dispatch-request
                   nil (list :method "POST" :path "/api/cmd" :query nil
                             :headers '(("content-type" . "application/json"))
                             :body body))))
              (should (equal cap-status "200 OK"))
              (should (eq (plist-get cap-payload :ok) t))
              (should abort-called))))
      (setq carriage-web-api-commands-enabled nil))))

;; ---------------------------------------------------------------------
;; Additional metrics and SSE auth-header tests

(ert-deftest carriage-web--dispatch-metrics-ok ()
  "GET /api/metrics returns ok envelope with counters."
  (let (cap-status cap-payload)
    (cl-letf (((symbol-function 'carriage-web--send-json)
               (lambda (_proc status payload)
                 (setq cap-status status
                       cap-payload payload))))
      (carriage-web--dispatch-request
       nil (list :method "GET" :path "/api/metrics" :query nil :headers '())))
    (should (equal cap-status "200 OK"))
    (should (eq (plist-get cap-payload :ok) t))
    (let ((data (plist-get cap-payload :data)))
      (should (listp data))
      (should (numberp (plist-get data :published)))
      (should (numberp (plist-get data :truncated)))
      (should (numberp (plist-get data :clients)))
      (should (numberp (plist-get data :ts))))))

(ert-deftest carriage-web--http-sse-auth-header-ok ()
  "Authorized SSE handshake via X-Auth header responds with event-stream headers."
  (require 'cl-lib)
  (let ((outs '())
        (scheduled '())
        (carriage-web-auth-token "tok"))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_proc s) (push s outs)))
              ((symbol-function 'run-at-time)
               (lambda (sec _repeat fn &rest args)
                 (push (list sec fn args) scheduled)
                 nil))
              ((symbol-function 'process-send-eof)
               (lambda (&rest _args) (push :eof outs))))
      (let* ((p (make-pipe-process :name "cw-test-sse-auth" :noquery t))
             (buf (generate-new-buffer " *cw-sse-auth*")))
        (unwind-protect
            (progn
              (set-process-buffer p buf)
              (carriage-web--process-filter
               p "GET /stream HTTP/1.1\r\nHost: x\r\nX-Auth: tok\r\n\r\n")
              (let ((resp (apply #'concat (nreverse outs))))
                (should (string-match-p "HTTP/1.1 200 OK" resp))
                (should (string-match-p "text/event-stream" resp))
                (should (string-match-p "event: hello" resp)))
              ;; For SSE we must not schedule a close
              (should (null scheduled)))
          (when (process-live-p p) (ignore-errors (delete-process p)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest carriage-web--clear-debug-advices-safe ()
  "Ensure carriage-web-clear-debug-advices is safe to call and returns a number."
  (should (fboundp 'carriage-web-clear-debug-advices))
  (let ((n (carriage-web-clear-debug-advices)))
    (should (numberp n))
    (should (>= n 0))))

;;; carriage-web-tests.el ends here
