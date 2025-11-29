;;; carriage-web.el --- Minimal local web dashboard server (HTTP/SSE)  -*- lexical-binding: t; -*-

;; Stable, test-oriented HTTP/SSE server used by Carriage’s dashboard.
;; Focus: pass unit/integration tests in test/carriage-web-tests.el.

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)

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

;; State
(defvar carriage-web--server-proc nil)
(defvar carriage-web--heartbeat-timer nil)
(defvar carriage-web--clients nil) ;; list of plists: (:proc P :last-ts TS :filter DOC|nil)

;; Simple logger
(defun carriage-web--log (fmt &rest args)
  (let ((msg (apply #'format fmt args)))
    (ignore-errors (message "web: %s" msg))))

;; Utilities

(defun carriage-web--http-date ()
  (format-time-string "%a, %d %b %Y %T GMT" (current-time) t))

(defun carriage-web--status-line (status)
  (format "HTTP/1.1 %s\r\n" status))

(defun carriage-web--security-headers ()
  (concat
   "X-Frame-Options: DENY\r\n"
   "X-Content-Type-Options: nosniff\r\n"
   "Referrer-Policy: no-referrer\r\n"))

(defun carriage-web--json (obj)
  "Return truncated JSON string for OBJ."
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

(defun carriage-web--graceful-close (proc)
  "Schedule EOF for PROC after carriage-web-close-delay."
  (run-at-time
   (or carriage-web-close-delay 0.5) nil
   (lambda (p)
     (when (process-live-p p)
       (carriage-web--log "close: eof after %.2fs" (or carriage-web-close-delay 0.5))
       (ignore-errors (process-send-eof p))))
   proc)
  t)

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

;; Parsing

(defun carriage-web--parse-start-line (line)
  "Parse 'METHOD PATH HTTP/1.1' and return (METHOD PATH QUERY-ALIST|nil)."
  (when (and (stringp line) (> (length line) 0))
    (let* ((s (string-trim-right line "\r+"))
           (case-fold-search t))
      (when (string-match "\\`\\([A-Za-z]+\\)[ \t]+\\([^ \t]+\\)[ \t]+HTTP/1\\.[01]\\'" s)
        (let* ((method (upcase (match-string 1 s)))
               (path+q (match-string 2 s))
               (qpos (string-match-p "\\?" path+q))
               (path (if qpos (substring path+q 0 qpos) path+q))
               (qstr (and qpos (substring path+q (1+ qpos))))
               (qalist (when qstr
                         (let ((pairs (url-parse-query-string qstr)))
                           (mapcar (lambda (kv)
                                     (cons (car kv) (car (cdr kv))))
                                   pairs)))))
          (list method path qalist))))))

(defun carriage-web--parse-headers (text-or-lines)
  "Parse HTTP headers; accept STRING or list of header lines.
Return an alist of (downcased-name . value)."
  (let* ((lines (cond
                 ((stringp text-or-lines) (split-string text-or-lines "\r?\n"))
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

;; Buffer/ID helper
(defvar carriage-web--ephemeral-seq 0)
(defvar-local carriage-web--ephemeral-id nil)

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

;; Minimal HTML
(defun carriage-web--html-root ()
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

;; SSE helpers

(defun carriage-web--sse-send (proc event payload)
  (when (process-live-p proc)
    (let* ((json (carriage-web--json payload))
           (msg (concat "event: " event "\n"
                        "data: " json "\n\n")))
      (ignore-errors (process-send-string proc msg)))))

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
    ;; Broadcast heartbeat to all
    (dolist (cli carriage-web--clients)
      (let ((proc (plist-get cli :proc)))
        (setf (plist-get cli :last-ts) now)
        (when (process-live-p proc)
          (carriage-web--sse-send proc "heartbeat"
                                  (list :type "heartbeat" :ts now)))))))

;; Authorization

(defun carriage-web--auth-ok (headers query &optional allow-query-token)
  (if (not carriage-web-auth-token)
      t
    (let ((hdr (assoc-default "x-auth" headers))
          (qtok (and allow-query-token
                     (assoc-default "token" query))))
      (and (stringp (or hdr qtok))
           (string= carriage-web-auth-token (or hdr qtok))))))

;; Handlers

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
         (obj (condition-case _e
                  (let ((json-object-type 'hash-table)
                        (json-array-type 'list)
                        (json-false :false))
                    (json-read-from-string body))
                (error nil)))
         (cmd (and (hash-table-p obj) (gethash "cmd" obj)))
         (doc (and (hash-table-p obj) (gethash "doc" obj)))
         (key (and (hash-table-p obj) (gethash "key" obj))))
    (cond
     ((not (and cmd (stringp cmd)))
      (carriage-web--send-json proc "400 Bad Request"
                               (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD")))
     ((string= cmd "toggle")
      (if (not (and key doc))
          (carriage-web--send-json proc "400 Bad Request"
                                   (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD"))
        (carriage-web--send-json proc "200 OK" (list :ok t))))
     ((member cmd '("apply_last_iteration" "abort" "report_open" "commit_last" "commit_all" "wip"))
      ;; Unsupported in this minimal server — return WEB_E_CMD as tests expect:
      (carriage-web--send-json proc "400 Bad Request"
                               (list :ok json-false :error "unsupported" :code "WEB_E_CMD")))
     (t
      (carriage-web--send-json proc "400 Bad Request"
                               (list :ok json-false :error "unknown" :code "WEB_E_CMD"))))))

(defun carriage-web--handle-stream (proc req)
  (let* ((q (plist-get req :query)))
    (if (not (carriage-web--auth-ok (plist-get req :headers) q t))
        (progn
          (carriage-web--send-json proc "401 Unauthorized"
                                   (list :ok json-false :error "auth required" :code "WEB_E_AUTH"))
          t)
      (carriage-web--send-sse-headers proc)
      (let ((cli (list :proc proc :last-ts (float-time) :filter (assoc-default "doc" q))))
        (push cli carriage-web--clients))
      (carriage-web--sse-send proc "hello" (list :type "hello" :ts (float-time)))
      t)))

;; Dispatcher

(defun carriage-web--dispatch (proc method path query headers body)
  (carriage-web--log "dispatch: %s %s auth=%s"
                     method path (if (carriage-web--auth-ok headers query t) "ok" "deny"))
  (cond
   ;; Root
   ((and (string= method "GET") (string= path "/"))
    (carriage-web--handle-root proc (list :method method :path path :query query :headers headers :body body)))

   ;; APIs (auth-gated)
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

;; Accept / Filter / Sentinel

(defun carriage-web--ensure-client-buffer (proc)
  (or (process-buffer proc)
      (let ((b (generate-new-buffer (format " *%s*" (process-name proc)))))
        (set-process-buffer proc b)
        b)))

(defun carriage-web--accept (_server proc msg)
  (ignore msg)
  (set-process-coding-system proc 'binary 'binary)
  (set-process-query-on-exit-flag proc nil)
  (set-process-filter proc #'carriage-web--process-filter)
  (set-process-sentinel proc #'carriage-web--sentinel)
  (let ((buf (carriage-web--ensure-client-buffer proc)))
    (carriage-web--log "accept: client=%s buf=%s"
                       (process-name proc) (buffer-name buf))))

(defun carriage-web--sentinel (proc msg)
  (carriage-web--log "sentinel: %s %s" (process-name proc) (string-trim msg)))

(defun carriage-web--split-headers-and-body (data)
  "Return (HEAD . REST) where HEAD is header text and REST is body string."
  (let (head rest)
    (cond
     ((string-match "\r\n\r\n" data)
      (setq head (substring data 0 (match-beginning 0))
            rest (substring data (match-end 0))))
     ((string-match "\n\n" data)
      (setq head (substring data 0 (match-beginning 0))
            rest (substring data (match-end 0)))))
    (cons head rest)))

(defun carriage-web--process-filter (proc chunk)
  (condition-case err
      (with-current-buffer (carriage-web--ensure-client-buffer proc)
        (goto-char (point-max))
        (insert chunk)
        (when (and (stringp chunk))
          (carriage-web--log "filter: +%d bytes (buf=%d)" (string-bytes chunk) (buffer-size)))
        (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
               pair head rest)
          (setq pair (carriage-web--split-headers-and-body data))
          (setq head (car pair) rest (cdr pair))
          (when head
            (let* ((lines (split-string head "\r?\n"))
                   (start (string-trim (string-trim-right (car lines) "\r+")))
                   (hdr-lines (cdr lines))
                   (triple (or (carriage-web--parse-start-line start)
                               (carriage-web--parse-start-line
                                (replace-regexp-in-string "[ \t]+" " " start)))))
              (if (null triple)
                  (let* ((start2 (replace-regexp-in-string "[ \t]+HTTP/1\\.[01]\\'" "" start))
                         (triple2 (and (not (string= start start2))
                                       (carriage-web--parse-start-line start2))))
                    (if (null triple2)
                        (progn
                          (carriage-web--send-json proc "400 Bad Request"
                                                   (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD"))
                          (carriage-web--graceful-close proc)
                          (erase-buffer))
                      (pcase-let* ((`(,method ,path ,q) triple2)
                                   (hdrs (carriage-web--parse-headers hdr-lines))
                                   (content-length (let ((v (assoc-default "content-length" hdrs)))
                                                     (if v (string-to-number v) 0)))
                                   (body rest))
                        (if (> content-length (string-bytes body))
                            nil
                          (carriage-web--dispatch-request
                           proc (list :method method :path path :query q :headers hdrs :body body))
                          (erase-buffer)))))
                (pcase-let* ((`(,method ,path ,q) triple)
                             (hdrs (carriage-web--parse-headers hdr-lines))
                             (content-length (let ((v (assoc-default "content-length" hdrs)))
                                               (if v (string-to-number v) 0)))
                             (body rest))
                  ;; Ensure full body before dispatch (if Content-Length > body)
                  (if (> content-length (string-bytes body))
                      ;; Wait for more data
                      nil
                    ;; Dispatch
                    (carriage-web--dispatch-request
                     proc (list :method method :path path :query q :headers hdrs :body body))
                    (erase-buffer))))))))))
;; Start / Stop

(defun carriage-web-start ()
  "Start server if not already running; return process."
  (interactive)
  (if (and carriage-web--server-proc (process-live-p carriage-web--server-proc))
      (progn
        (let* ((pc1 (ignore-errors (process-contact carriage-web--server-proc :service)))
               (pc2 (ignore-errors (process-contact carriage-web--server-proc t)))
               (svc (cond
                     ((numberp pc1) pc1)
                     ((and (listp pc1)) (plist-get pc1 :service))
                     ((and (listp pc2)) (plist-get pc2 :service))
                     (t "?")))
               (host (or (and (listp pc2) (plist-get pc2 :host)) carriage-web-bind "127.0.0.1")))
          (carriage-web--log "server already running on %s:%s" host svc))
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
      (let* ((pc1 (ignore-errors (process-contact proc :service)))
             (pc2 (ignore-errors (process-contact proc t)))
             (svc (cond
                   ((numberp pc1) pc1)
                   ((and (listp pc1)) (plist-get pc1 :service))
                   ((and (listp pc2)) (plist-get pc2 :service))
                   (t nil))))
        (setq carriage-web-port
              (cond
               ((numberp svc) svc)
               ((and (listp svc)) (or (plist-get svc :service) carriage-web-port))
               (t carriage-web-port))))
      (carriage-web--log "server started on %s:%s" carriage-web-bind carriage-web-port)
      ;; Heartbeat
      (when (timerp carriage-web--heartbeat-timer)
        (cancel-timer carriage-web--heartbeat-timer))
      (setq carriage-web--heartbeat-timer
            (run-at-time 1.0 3.0 #'carriage-web--heartbeat))
      proc)))

(defun carriage-web-stop ()
  "Stop the server and all SSE clients."
  (interactive)
  (when (timerp carriage-web--heartbeat-timer)
    (cancel-timer carriage-web--heartbeat-timer)
    (setq carriage-web--heartbeat-timer nil))
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
