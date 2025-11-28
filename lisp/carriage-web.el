;;; carriage-web.el --- Minimal local web dashboard server (HTTP/SSE)  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Carriage Team
;; URL: https://gnu-emacs.ru/carriage
;; Keywords: tools, http, sse, dashboard

;;; Commentary:
;; Minimal, self-contained HTTP server used by Carriage's local web dashboard.
;;
;; - Serves "/" with static HTML
;; - JSON APIs under /api/*
;; - SSE stream at /stream
;; - Idle SSE clients pruned by heartbeat timer
;; - Designed to be testable with unit and integration (open-network-stream) tests

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-util)
(require 'subr-x)

(defgroup carriage-web nil
  "Local HTTP/SSE server for Carriage dashboard."
  :group 'applications)

(defcustom carriage-web-enabled t
  "When non-nil, server is allowed to run."
  :type 'boolean :group 'carriage-web)

(defcustom carriage-web-bind "127.0.0.1"
  "Bind address for the server."
  :type 'string :group 'carriage-web)

(defcustom carriage-web-port 8787
  "TCP port for the server. When 0, kernel picks an ephemeral port."
  :type 'integer :group 'carriage-web)

(defcustom carriage-web-auth-token nil
  "Optional shared token; when set, API and stream require it.
For SSE, token may be passed via query (?token=...)."
  :type '(choice (const nil) string) :group 'carriage-web)

(defcustom carriage-web-close-delay 0.5
  "Delay (seconds) before sending EOF for non-SSE responses."
  :type 'number :group 'carriage-web)

(defcustom carriage-web-max-json-bytes 131072
  "Cap for JSON responses; payloads are truncated beyond this limit."
  :type 'integer :group 'carriage-web)

(defcustom carriage-web-sse-idle-timeout 180
  "Idle timeout (seconds) for SSE clients; idle ones are pruned."
  :type 'integer :group 'carriage-web)

;; Internal state
(defvar carriage-web--server-proc nil)
(defvar carriage-web--hb-timer nil)
(defvar carriage-web--heartbeat-timer nil) ;; alias for legacy tests
(defvar carriage-web--clients nil) ; list of plists: (:proc P :last-ts TS :filter DOC|nil)

(defvar carriage-web--last-apply-summary (make-hash-table :test 'equal))
(defvar carriage-web--last-report-summary (make-hash-table :test 'equal))

(defvar carriage-web--ephemeral-seq 0
  "Sequence for generating stable ephemeral buffer IDs.")

(defvar-local carriage-web--ephemeral-id nil)

(defun carriage-web--log (fmt &rest args)
  "Lightweight logger."
  (let ((msg (apply #'format fmt args)))
    (when (featurep 'carriage-logging)
      (ignore-errors (carriage-log "%s" msg)))
    (ignore-errors (message "web: %s" msg))))

;; ---------- Utilities

(defun carriage-web--http-date ()
  "Return HTTP date string."
  (format-time-string "%a, %d %b %Y %T GMT" (current-time) t))

(defun carriage-web--status-line (status)
  (format "HTTP/1.1 %s\r\n" status))

(defun carriage-web--security-headers ()
  (concat
   "X-Frame-Options: DENY\r\n"
   "X-Content-Type-Options: nosniff\r\n"
   "Referrer-Policy: no-referrer\r\n"))

(defun carriage-web--json (obj)
  "Return JSON string for OBJ, truncated to carriage-web-max-json-bytes."
  (let* ((s (json-encode obj))
         (b (string-bytes s)))
    (if (> b carriage-web-max-json-bytes)
        (substring s 0 carriage-web-max-json-bytes)
      s)))

(defun carriage-web--send-raw (proc s)
  (when (and (process-live-p proc) (stringp s))
    (condition-case _e
        (process-send-string proc s)
      (error nil))))

(defun carriage-web--send-response (proc status content-type content &optional keep-open)
  "Send HTTP RESPONSE with STATUS and CONTENT-TYPE/CONTENT.
When KEEP-OPEN non-nil, do not schedule EOF (SSE)."
  (let* ((body (or content "")) ;; ensure string
         (bytes (string-bytes body))
         (hdr (concat
               (carriage-web--status-line status)
               (format "Date: %s\r\n" (carriage-web--http-date))
               "Server: carriage-web\r\n"
               (carriage-web--security-headers)
               (format "Content-Type: %s\r\n" content-type)
               (format "Content-Length: %d\r\n" bytes)
               (format "Connection: %s\r\n" (if keep-open "keep-alive" "close"))
               "\r\n")))
    (carriage-web--log "send-response: %s ct=%s bytes=%d keep=%s"
                       status content-type bytes (if keep-open "y" "n"))
    (carriage-web--send-raw proc hdr)
    (carriage-web--send-raw proc body)
    (unless keep-open
      (carriage-web--graceful-close proc))
    t))

(defun carriage-web--send-json (proc status payload)
  "Send JSON envelope PAYLOAD with STATUS."
  (let* ((json (carriage-web--json payload)))
    (carriage-web--send-response proc status "application/json; charset=utf-8" json)))

(defun carriage-web--send-sse-headers (proc)
  (let ((hdr (concat
              (carriage-web--status-line "200 OK")
              (format "Date: %s\r\n" (carriage-web--http-date))
              "Server: carriage-web\r\n"
              (carriage-web--security-headers)
              "Content-Type: text/event-stream\r\n"
              "Cache-Control: no-cache\r\n"
              "Connection: keep-alive\r\n\r\n")))
    (carriage-web--send-raw proc hdr)))

(defun carriage-web--graceful-close (proc)
  "Schedule EOF for PROC after carriage-web-close-delay."
  (run-at-time
   carriage-web-close-delay nil
   (lambda (p)
     (when (process-live-p p)
       (carriage-web--log "close: eof after %.2fs" carriage-web-close-delay)
       (ignore-errors (process-send-eof p)))))
  t)

;; ---------- Parsing

(defun carriage-web--parse-start-line (line)
  "Parse 'METHOD PATH HTTP/1.1' and return (METHOD PATH QUERY-ALIST|nil)."
  (when (and line (string-match "\\`\\([A-Z]+\\) +\\([^ ]+\\) +HTTP/1\\.[01]\\'" line))
    (let* ((method (match-string 1 line))
           (path+q (match-string 2 line))
           (qpos (string-match-p "\\?" path+q))
           (path (if qpos (substring path+q 0 qpos) path+q))
           (qstr (and qpos (substring path+q (1+ qpos))))
           (qalist (when qstr
                     (let ((pairs (url-parse-query-string qstr)))
                       (mapcar (lambda (kv)
                                 (cons (car kv) (car (cdr kv))))
                               pairs)))))
      (list method path qalist))))

(defun carriage-web--parse-headers (text-or-lines)
  "Parse HTTP headers; accept STRING or list of header lines.
Return an alist of (downcased-name . value)."
  (let* ((lines (cond
                 ((stringp text-or-lines)
                  (split-string text-or-lines "\\r?\\n"))
                 ((listp text-or-lines) text-or-lines)
                 (t nil)))
         (acc '()))
    (dolist (ln lines)
      (when (and (stringp ln)
                 (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" ln))
        (let ((k (downcase (string-trim (match-string 1 ln))))
              (v (string-trim (match-string 2 ln))))
          (push (cons k v) acc))))
    (nreverse acc)))

;; ---------- Buffer/ID helpers

(defun carriage-web--buffer-id (&optional buffer)
  "Stable ID for BUFFER: repo-relative file path or 'ephemeral:<n>'."
  (with-current-buffer (or buffer (current-buffer))
    (if buffer-file-name
        (or (ignore-errors
              (let* ((abs (expand-file-name buffer-file-name))
                     (root (or (and (fboundp 'carriage-project-root)
                                    (carriage-project-root))
                               default-directory)))
                (file-relative-name abs (file-name-as-directory root))))
            (file-name-nondirectory buffer-file-name))
      (unless carriage-web--ephemeral-id
        (setq carriage-web--ephemeral-id
              (format "ephemeral:%d" (cl-incf carriage-web--ephemeral-seq))))
      carriage-web--ephemeral-id)))

;; ---------- HTML

(defun carriage-web--html-root ()
  "Return minimal HTML page."
  (concat
   "<!doctype html><html><head><meta charset='utf-8'>"
   "<meta http-equiv='X-Content-Type-Options' content='nosniff'/>"
   "<title>Carriage — Dashboard</title>"
   "<style>body{font-family:sans-serif;margin:1rem} header{font-weight:bold;margin-bottom:1rem}</style>"
   "</head><body><header>Carriage — Local Dashboard</header>"
   "<p>Server is running. Open console for details.</p>"
   "<script>(function(){"
   "console.log('Carriage dashboard loaded');"
   "})();</script>"
   "</body></html>"))

;; ---------- SSE

(defun carriage-web--sse-add (proc &optional filter-doc)
  (let ((cli (list :proc proc :last-ts (float-time) :filter filter-doc)))
    (push cli carriage-web--clients)
    (carriage-web--log "sse: client added (filter=%s)" (or filter-doc "-"))
    cli))

(defun carriage-web--sse-send (proc event payload)
  (when (process-live-p proc)
    (let* ((json (carriage-web--json payload))
           (msg (concat "event: " event "\n"
                        "data: " json "\n\n")))
      (ignore-errors (process-send-string proc msg)))))

(defun carriage-web--broadcast (event payload)
  (let ((now (float-time)))
    (setq carriage-web--clients
          (cl-remove-if-not
           (lambda (cli)
             (let* ((proc (plist-get cli :proc)))
               (if (not (process-live-p proc))
                   nil
                 (setf (plist-get cli :last-ts) now)
                 (let ((flt (plist-get cli :filter)))
                   (when (or (null flt)
                             (equal (plist-get payload :doc) flt))
                     (carriage-web--sse-send proc event payload)))
                 t)))
           carriage-web--clients))))

(defun carriage-web--heartbeat ()
  (let* ((now (float-time))
         (before (length carriage-web--clients)))
    (setq carriage-web--clients
          (cl-remove-if
           (lambda (cli)
             (> (- now (or (plist-get cli :last-ts) now))
                carriage-web-sse-idle-timeout))
           carriage-web--clients))
    (let ((after (length carriage-web--clients)))
      (when (< after before)
        (carriage-web--log "sse: pruned %d idle clients" (- before after))))
    (carriage-web--broadcast "heartbeat" (list :type "heartbeat" :ts now))))

;; ---------- Authorization

(defun carriage-web--auth-ok (headers query &optional allow-query-token)
  (if (not carriage-web-auth-token)
      t
    (let ((hdr (assoc-default "x-auth" headers))
          (qtok (and allow-query-token
                     (assoc-default "token" query))))
      (and (stringp (or hdr qtok))
           (string= carriage-web-auth-token (or hdr qtok))))))

;; ---------- Dispatchers (public API signature preserved)

(defun carriage-web--handle-root (proc _req)
  (carriage-web--send-response proc "200 OK" "text/html; charset=utf-8" (carriage-web--html-root)))

(defun carriage-web--handle-health (proc _req)
  (let ((engine (cond
                 ((fboundp 'carriage-apply-engine)
                  (format "%s" (carriage-apply-engine)))
                 (t "emacs"))))
    (carriage-web--send-json proc "200 OK"
                             (list :ok t :data (list :version "v1" :engine engine :ts (float-time))))))

(defun carriage-web--handle-sessions (proc _req)
  (carriage-web--send-json proc "200 OK" (list :ok t :data '())))

(defun carriage-web--handle-session (proc req)
  (let* ((path (plist-get req :path))
         (id (substring path (length "/api/session/"))))
    (ignore id)
    (carriage-web--send-json proc "404 Not Found"
                             (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))))

(defun carriage-web--handle-report-last (proc req)
  (let* ((q (plist-get req :query))
         (doc (and (listp q) (assoc-default "doc" q))))
    (if (and (stringp doc) (> (length doc) 0))
        (carriage-web--send-json proc "404 Not Found"
                                 (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))
      (carriage-web--send-json proc "400 Bad Request"
                               (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD")))))

(defun carriage-web--handle-cmd (proc req)
  (let* ((body (or (plist-get req :body) ""))
         (obj (ignore-errors (json-read-from-string body)))
         (cmd (and (hash-table-p obj) (gethash "cmd" obj)))
         (doc (and (hash-table-p obj) (gethash "doc" obj)))
         (key (and (hash-table-p obj) (gethash "key" obj))))
    (cond
     ((not (and cmd (stringp cmd)))
      (carriage-web--send-json proc "400 Bad Request"
                               (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD")))
     ((string= cmd "toggle")
      (if (not key)
          (carriage-web--send-json proc "400 Bad Request"
                                   (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD"))
        (carriage-web--send-json proc "200 OK" (list :ok t))))
     ((member cmd '("apply_last_iteration" "abort" "report_open" "commit_last" "commit_all" "wip"))
      ;; For MVP: unsupported → WEB_E_CMD (except 'toggle' handled above)
      (carriage-web--send-json proc "400 Bad Request"
                               (list :ok json-false :error "unsupported" :code "WEB_E_CMD")))
     (t
      (carriage-web--send-json proc "400 Bad Request"
                               (list :ok json-false :error "unknown" :code "WEB_E_CMD"))))))

(defun carriage-web--handle-stream (proc req)
  (let* ((q (plist-get req :query)))
    (unless (carriage-web--auth-ok (plist-get req :headers) q t)
      (carriage-web--send-json proc "401 Unauthorized"
                               (list :ok json-false :error "auth required" :code "WEB_E_AUTH"))
      (cl-return-from carriage-web--handle-stream t))
    (carriage-web--send-sse-headers proc)
    (carriage-web--sse-add proc (assoc-default "doc" q))
    (carriage-web--sse-send proc "hello" (list :type "hello" :ts (float-time)))
    t))

(defun carriage-web--dispatch (proc method path query headers body)
  "Internal router."
  (carriage-web--log "dispatch: %s %s auth=%s"
                     method path (if (carriage-web--auth-ok headers query t) "ok" "deny"))
  (cond
   ((and (string= method "GET") (string= path "/"))
    (carriage-web--handle-root proc (list :method method :path path :query query :headers headers :body body)))

   ;; APIs
   ((and (string= method "GET") (string= path "/api/health"))
    (if (carriage-web--auth-ok headers query nil)
        (carriage-web--handle-health proc nil)
      (carriage-web--send-json proc "401 Unauthorized"
                               (list :ok json-false :error "auth required" :code "WEB_E_AUTH"))))

   ((and (string= method "GET") (string= path "/api/sessions"))
    (if (carriage-web--auth-ok headers query nil)
        (carriage-web--handle-sessions proc nil)
      (carriage-web--send-json proc "401 Unauthorized"
                               (list :ok json-false :error "auth required" :code "WEB_E_AUTH"))))

   ((and (string= method "GET") (string-prefix-p "/api/session/" path))
    (if (carriage-web--auth-ok headers query nil)
        (carriage-web--handle-session proc (list :path path :query query :headers headers :body body))
      (carriage-web--send-json proc "401 Unauthorized"
                               (list :ok json-false :error "auth required" :code "WEB_E_AUTH"))))

   ((and (string= method "GET") (string= path "/api/report/last"))
    (if (carriage-web--auth-ok headers query nil)
        (carriage-web--handle-report-last proc (list :query query))
      (carriage-web--send-json proc "401 Unauthorized"
                               (list :ok json-false :error "auth required" :code "WEB_E_AUTH"))))

   ((and (string= method "POST") (string= path "/api/cmd"))
    (if (carriage-web--auth-ok headers query nil)
        (carriage-web--handle-cmd proc (list :query query :headers headers :body body))
      (carriage-web--send-json proc "401 Unauthorized"
                               (list :ok json-false :error "auth required" :code "WEB_E_AUTH"))))

   ;; SSE
   ((and (string= method "GET") (string= path "/stream"))
    (carriage-web--handle-stream proc (list :query query :headers headers)))

   (t
    (carriage-web--send-json proc "404 Not Found"
                             (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND")))))

(defun carriage-web--dispatch-request (proc req-or-method &optional path query headers body)
  "Public dispatcher kept for tests; supports 2-arity (PROC REQ-PLIST) and 6-arity."
  (if (and (consp req-or-method) (plist-get req-or-method :method))
      (let* ((req req-or-method)
             (method (plist-get req :method))
             (ppath  (plist-get req :path))
             (q      (plist-get req :query))
             (hdrs   (plist-get req :headers))
             (bdy    (or (plist-get req :body) "")))
        (carriage-web--dispatch proc method ppath q hdrs bdy))
    (carriage-web--dispatch proc req-or-method (or path "/")
                            (or query nil) (or headers nil) (or body ""))))

;; ---------- Accept / Filter / Sentinel

(defun carriage-web--ensure-client-buffer (proc)
  (or (process-buffer proc)
      (let ((b (generate-new-buffer (format " *%s*" (process-name proc)))))
        (set-process-buffer proc b)
        b)))

(defun carriage-web--accept (_server proc _msg)
  (set-process-coding-system proc 'binary 'binary)
  (set-process-query-on-exit-flag proc nil)
  (set-process-filter proc #'carriage-web--process-filter)
  (set-process-sentinel proc #'carriage-web--sentinel)
  (let ((buf (carriage-web--ensure-client-buffer proc)))
    (carriage-web--log "accept: client=%s"
                       (process-name proc))))

(defun carriage-web--sentinel (proc msg)
  (carriage-web--log "sentinel: %s %s" (process-name proc) (string-trim msg)))

(defun carriage-web--process-filter (proc chunk)
  (condition-case err
      (with-current-buffer (carriage-web--ensure-client-buffer proc)
        (goto-char (point-max))
        (insert chunk)
        (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
               head rest)
          (cond
           ((string-match "\r\n\r\n" data)
            (setq head (substring data 0 (match-beginning 0))
                  rest (substring data (match-end 0))))
           ((string-match "\n\n" data)
            (setq head (substring data 0 (match-beginning 0))
                  rest (substring data (match-end 0)))))
          (when head
            (let* ((lines (split-string head "\\r?\\n"))
                   (start (car lines))
                   (hdr-lines (cdr lines))
                   (triple (carriage-web--parse-start-line start)))
              (if (null triple)
                  (progn
                    (carriage-web--send-json proc "400 Bad Request"
                                             (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD"))
                    (erase-buffer))
                (pcase-let* ((`(,method ,path ,q) triple)
                             (hdrs (carriage-web--parse-headers hdr-lines))
                             (content-length (let ((v (assoc-default "content-length" hdrs)))
                                               (if v (string-to-number v) 0)))
                             (body rest))
                  (when (> content-length (string-bytes body))
                    ;; Wait for more data
                    nil)
                  ;; Dispatch
                  (carriage-web--dispatch-request
                   proc (list :method method :path path :query q :headers hdrs :body body))
                  (erase-buffer)))))))
    (error
     (carriage-web--log "filter error: %s" (error-message-string err))
     (carriage-web--send-json proc "500 Internal Server Error"
                              (list :ok json-false :error "internal" :code "WEB_E_INTERNAL"))
     (ignore-errors (carriage-web--graceful-close proc)))))

;; ---------- Start / Stop

(defun carriage-web-start ()
  "Start server if not already running; return process."
  (interactive)
  (if (and carriage-web--server-proc (process-live-p carriage-web--server-proc))
      (progn
        (carriage-web--log "server already running on %s"
                           (or (process-contact carriage-web--server-proc :service) "?"))
        carriage-web--server-proc)
    (let* ((host carriage-web-bind)
           (port carriage-web-port)
           (proc nil))
      (condition-case _e
          (setq proc (make-network-process
                      :name "carriage-web"
                      :server t
                      :host host
                      :service port
                      :noquery t
                      :family 'ipv4
                      :filter #'carriage-web--process-filter
                      :sentinel #'carriage-web--sentinel
                      :log #'carriage-web--accept))
        (error
         ;; fallback to ephemeral port
         (setq proc (make-network-process
                     :name "carriage-web"
                     :server t
                     :host host
                     :service 0
                     :noquery t
                     :family 'ipv4
                     :filter #'carriage-web--process-filter
                     :sentinel #'carriage-web--sentinel
                     :log #'carriage-web--accept))))
      (setq carriage-web--server-proc proc)
      (let* ((real-port (or (process-contact proc :service) carriage-web-port)))
        (setq carriage-web-port real-port))
      (carriage-web--log "server started on %s:%s" carriage-web-bind carriage-web-port)
      ;; Heartbeat
      (when (timerp carriage-web--hb-timer)
        (cancel-timer carriage-web--hb-timer))
      (setq carriage-web--hb-timer
            (run-at-time 1.0 3.0 #'carriage-web--heartbeat))
      (setq carriage-web--heartbeat-timer carriage-web--hb-timer)
      proc)))

(defun carriage-web-stop ()
  "Stop the server and all SSE clients."
  (interactive)
  (when (timerp carriage-web--hb-timer)
    (cancel-timer carriage-web--hb-timer)
    (setq carriage-web--hb-timer nil
          carriage-web--heartbeat-timer nil))
  (dolist (cli carriage-web--clients)
    (let ((p (plist-get cli :proc)))
      (when (process-live-p p)
        (ignore-errors (process-send-eof p))
        (ignore-errors (delete-process p)))))
  (setq carriage-web--clients nil)
  (when (and carriage-web--server-proc (process-live-p carriage-web--server-proc))
    (ignore-errors (delete-process carriage-web--server-proc)))
  (setq carriage-web--server-proc nil)
  (carriage-web--log "server stopped")
  t)

(provide 'carriage-web)
;;; carriage-web.el ends here
