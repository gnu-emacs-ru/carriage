;;; carriage-web.el --- Local HTTP/SSE dashboard (health, stream, pub/sub)  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Carriage Team <dev@carriage>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5") (json "1.5"))
;; Version: 0.1
;; Keywords: tools, ui, http, sse
;;
;; Specifications:
;;   spec/spec-on-specs.org
;;   spec/web-dashboard-v1.org
;;   spec/security-v2.org
;;   spec/errors-v2.org
;;   spec/logging-v2.org
;;   spec/observability-v2.org
;;   spec/apply-pipeline-v2.org
;;
;;; Commentary:
;; Minimal, non-blocking HTTP + SSE server for a local (loopback) web dashboard.
;; This MVP provides:
;;   - A pub/sub bus (carriage-web-pub) with debounce and payload limits.
;;   - GET /api/health returning basic status JSON.
;;   - GET /stream (SSE) delivering events (heartbeat and published ones).
;; It binds to 127.0.0.1 by default and supports an optional X-Auth token.
;;
;; Next steps (see spec/web-dashboard-v1.org):
;;   - Sessions snapshots API (/api/sessions, /api/session/<id>).
;;   - Commands POST /api/cmd (whitelist).
;;   - Static HTML/JS page.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'url-util)
(require 'carriage-logging nil t)

(defgroup carriage-web nil
  "Carriage local web dashboard (HTTP/SSE) settings."
  :group 'carriage)

(defcustom carriage-web-enabled t
  "When non-nil, allow starting the local dashboard server."
  :type 'boolean :group 'carriage-web)

(defcustom carriage-web-bind "127.0.0.1"
  "Bind address for the dashboard HTTP server (loopback by default)."
  :type 'string :group 'carriage-web)

(defcustom carriage-web-port 8787
  "TCP port for the dashboard HTTP server."
  :type 'integer :group 'carriage-web)

(defcustom carriage-web-auth-token nil
  "Optional shared token; when non-nil, requests must include header X-Auth with this value."
  :type '(choice (const nil) string) :group 'carriage-web)

(defcustom carriage-web-max-json-bytes 131072
  "Maximum JSON payload size sent to clients; larger payloads are truncated."
  :type 'integer :group 'carriage-web)

(defcustom carriage-web-publish-debounce-ms 50
  "Debounce window (ms) to batch pub/sub events before streaming to clients."
  :type 'integer :group 'carriage-web)

(defcustom carriage-web-max-sse-clients 16
  "Maximum number of concurrent SSE clients."
  :type 'integer :group 'carriage-web)

(defcustom carriage-web-heartbeat-interval 12
  "Interval in seconds for sending heartbeat events to SSE clients."
  :type 'number :group 'carriage-web)

(defvar carriage-web--server-proc nil
  "Server process for dashboard HTTP listener.")

(defvar carriage-web--clients nil
  "List of SSE clients: each is a plist (:proc :id :filter :last-ts).")

(defvar carriage-web--next-event-id 1
  "Monotonic SSE event id.")

(defvar carriage-web--queue nil
  "Pending publish queue (list of plists).")

(defvar carriage-web--last-apply-summary (make-hash-table :test 'equal)
  "Last apply summary per doc id for session snapshots.")

(defvar carriage-web--last-report-summary (make-hash-table :test 'equal)
  "Last report summary per doc id for session snapshots.")

(defvar carriage-web--debounce-timer nil
  "Timer used to debounce pub/sub deliveries to SSE clients.")

(defvar carriage-web--heartbeat-timer nil
  "Timer for periodic heartbeat SSE events.")

(defun carriage-web--log (fmt &rest args)
  (when (fboundp 'carriage-log)
    (apply #'carriage-log (concat "web: " fmt) args)))

(defun carriage-web--now ()
  (float-time))

(defun carriage-web--truncate-bytes (s max-bytes)
  "Return string S truncated to MAX-BYTES (UTF-8 byte size)."
  (let* ((b (encode-coding-string (or s "") 'utf-8))
         (len (string-bytes b)))
    (if (<= len (max 0 max-bytes))
        s
      (decode-coding-string (substring b 0 (max 0 max-bytes)) 'utf-8))))

(defun carriage-web--json (obj)
  "Encode OBJ to JSON, truncating if needed by carriage-web-max-json-bytes."
  (let* ((json-encoding-pretty-print nil)
         (raw (json-encode obj)))
    (carriage-web--truncate-bytes raw carriage-web-max-json-bytes)))

(defun carriage-web--sse-write (proc event-id event-name data-json)
  "Write one SSE event to PROC."
  (when (process-live-p proc)
    (condition-case _e
        (progn
          (process-send-string proc (format "id: %d\r\n" event-id))
          (process-send-string proc (format "event: %s\r\n" event-name))
          ;; SSE 'data' must be single line or split. We send compact JSON (no newlines).
          (process-send-string proc (format "data: %s\r\n\r\n" data-json)))
      (error
       (ignore-errors (delete-process proc))))))

(defun carriage-web--broadcast (event-name payload)
  "Send EVENT-NAME with PAYLOAD (plist or alist) to all SSE clients.
Respects per-client filter (:filter (\"doc\" . ID)) when present."
  ;; Drop dead processes first.
  (setq carriage-web--clients
        (cl-remove-if-not (lambda (c) (process-live-p (plist-get c :proc)))
                          carriage-web--clients))
  (when carriage-web--clients
    (let* ((eid carriage-web--next-event-id)
           (json (carriage-web--json payload))
           (now (carriage-web--now))
           ;; Try to extract doc id from payload (plist or alist)
           (doc (cond
                 ((and (listp payload) (plist-member payload :doc)) (plist-get payload :doc))
                 ((and (listp payload)) (alist-get 'doc payload nil nil #'equal))
                 (t nil))))
      (cl-incf carriage-web--next-event-id)
      (dolist (cli carriage-web--clients)
        (let* ((proc (plist-get cli :proc))
               (flt  (plist-get cli :filter)))
          ;; Skip when client asked for a specific doc and this event is for a different doc.
          (when (or (null flt)
                    (not (consp flt))
                    (not (string= (car flt) "doc"))
                    (null doc)
                    (string= (cdr flt) doc))
            (carriage-web--sse-write proc eid event-name json)
            ;; Mark client as recently active.
            (plist-put cli :last-ts now)))))))

(defun carriage-web--debounce-push ()
  "Debounced delivery of queued events."
  (setq carriage-web--debounce-timer nil)
  (let ((batch (prog1 (nreverse carriage-web--queue) (setq carriage-web--queue nil))))
    (dolist (ev batch)
      (let ((name (plist-get ev :event))
            (payload (plist-get ev :payload)))
        (carriage-web--broadcast (or name "ui") payload)))))

(defun carriage-web--schedule-push ()
  "Schedule a debounced push if not already scheduled."
  (unless (timerp carriage-web--debounce-timer)
    (setq carriage-web--debounce-timer
          (run-at-time (/ (max 1 carriage-web-publish-debounce-ms) 1000.0) nil
                       #'carriage-web--debounce-push))))

;;; Public publish API

;;;###autoload
(defun carriage-web-pub (event payload)
  "Publish EVENT (symbol/string) with PAYLOAD (plist/alist) to the SSE stream (debounced)."
  (let* ((ename (cond ((symbolp event) (symbol-name event))
                      ((stringp event) event)
                      (t "ui")))
         (p (if (listp payload) payload (list :message (format "%s" payload)))))
    (push (list :event ename :payload p) carriage-web--queue)
    (carriage-web--schedule-push)
    t))

;;; HTTP parsing helpers

(defun carriage-web--parse-start-line (line)
  "Parse HTTP start LINE like: GET /path?query HTTP/1.1.
Return (METHOD PATH QUERY) where QUERY is alist."
  (when (string-match "\\`\\([A-Z]+\\) \\([^ ]+\\) HTTP/" line)
    (let* ((method (match-string 1 line))
           (uri (match-string 2 line))
           (qpos (string-match-p "\\?" uri))
           (path (if qpos (substring uri 0 qpos) uri))
           (qstr (if qpos (substring uri (1+ qpos)) ""))
           (query
            (when (and qstr (> (length qstr) 0))
              (mapcar (lambda (kv)
                        (let* ((sp (string-match "=" kv))
                               (k (url-unhex-string (if sp (substring kv 0 sp) kv)))
                               (v (and sp (url-unhex-string (substring kv (1+ sp)))))) ; may need (require 'url-util), available in Emacs
                          (cons k v)))
                      (split-string qstr "&" t)))))
      (list method path query))))

(defun carriage-web--parse-headers (lines)
  "Parse HTTP header LINES (list of strings) into alist of downcased header -> value."
  (let ((tbl '()))
    (dolist (ln lines)
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" ln)
        (let ((k (downcase (string-trim (match-string 1 ln))))
              (v (string-trim (match-string 2 ln))))
          (push (cons k v) tbl))))
    tbl))

(defun carriage-web--auth-ok (headers)
  "Return non-nil if request HEADERS pass X-Auth validation (or token unset)."
  (if (not (stringp carriage-web-auth-token))
      t
    (let ((h (alist-get "x-auth" headers nil nil #'string=)))
      (and (stringp h) (string= h carriage-web-auth-token)))))

(defun carriage-web--send-response (proc status content &optional content-type extra-headers)
  "Send HTTP response on PROC with STATUS and CONTENT (string).
CONTENT-TYPE defaults to application/json."
  (let* ((ct (or content-type "application/json; charset=utf-8"))
         (len (string-bytes (or content "")))
         (headers
          (concat
           (format "HTTP/1.1 %s\r\n" status)
           (format "Content-Type: %s\r\n" ct)
           (format "Content-Length: %d\r\n" len)
           "X-Frame-Options: DENY\r\n"
           "X-Content-Type-Options: nosniff\r\n"
           "Referrer-Policy: no-referrer\r\n"
           (or extra-headers "")
           "\r\n")))
    (process-send-string proc headers)
    (when content
      (process-send-string proc content))))

(defun carriage-web--send-json (proc status payload)
  "Send JSON PAYLOAD with STATUS to PROC."
  (carriage-web--send-response proc status (carriage-web--json payload)
                               "application/json; charset=utf-8"))

;;; Route handlers

(defun carriage-web--handle-health (proc _method _path _query _headers)
  (let* ((ver (or (get 'carriage-web--version 'value) "0.1"))
         (engine (condition-case _e
                     (if (boundp 'carriage-apply-engine)
                         (format "%s" carriage-apply-engine)
                       "git")
                   (error "git")))
         (payload (list :ok t :data (list :version ver :engine engine :ts (carriage-web--now)))))
    (carriage-web--send-json proc "200 OK" payload)))

(defun carriage-web--sse-accept (proc)
  "Finish SSE handshake on PROC and register client."
  (let* ((headers
          (concat
           "HTTP/1.1 200 OK\r\n"
           "Content-Type: text/event-stream\r\n"
           "Cache-Control: no-cache\r\n"
           "Connection: keep-alive\r\n"
           "X-Accel-Buffering: no\r\n\r\n")))
    (process-send-string proc headers)
    (let ((cli (list :proc proc :id (format "cli-%d" (random 1000000))
                     :filter nil :last-ts (carriage-web--now))))
      (push cli carriage-web--clients)
      (carriage-web--log "sse: client added (total=%d)" (length carriage-web--clients)))))

(defun carriage-web--handle-stream (proc _method _path query headers)
  (cond
   ((not (carriage-web--auth-ok headers))
    (carriage-web--send-json proc "401 Unauthorized"
                             (list :ok json-false :error "auth required" :code "WEB_E_AUTH"))
    (ignore-errors (delete-process proc)))
   ((>= (length carriage-web--clients) carriage-web-max-sse-clients)
    (carriage-web--send-json proc "503 Service Unavailable"
                             (list :ok json-false :error "too many clients" :code "WEB_E_LIMIT"))
    (ignore-errors (delete-process proc)))
   (t
    ;; Optional filter by ?doc=... (store for future fan-out filtering)
    (let ((doc (cdr (cl-find "doc" query :key #'car  :test #'string=))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel
       proc
       (lambda (p _s)
         (setq carriage-web--clients
               (cl-remove-if (lambda (c) (or (eq (plist-get c :proc) p)
                                             (not (process-live-p (plist-get c :proc)))))
                             carriage-web--clients))))
      (carriage-web--sse-accept proc)
      (when doc
        (let ((cli (cl-find proc carriage-web--clients :key (lambda (c) (plist-get c :proc)))))
          (when cli (plist-put cli :filter (cons "doc" doc)))))
      ;; Immediately send a hello + heartbeat tick (so the client knows stream is live)
      (let* ((eid carriage-web--next-event-id)
             (hello (carriage-web--json (list :type "hello" :ts (carriage-web--now)))))
        (cl-incf carriage-web--next-event-id)
        (carriage-web--sse-write proc eid "hello" hello))))))

;;; HTTP dispatcher

(defun carriage-web--dispatch-request (proc req)
  "Dispatch parsed REQ on PROC.
REQ is a plist (:method :path :query :headers [:body])."
  (let* ((method  (plist-get req :method))
         (path    (plist-get req :path))
         (query   (plist-get req :query))
         (headers (plist-get req :headers))
         (body    (plist-get req :body)))
    (cond
     ;; Root: static HTML dashboard
     ((and (string= method "GET") (string= path "/"))
      (carriage-web--handle-root proc method path query headers))
     ;; Health
     ((and (string= method "GET") (string= path "/api/health"))
      (if (carriage-web--auth-ok headers)
          (carriage-web--handle-health proc method path query headers)
        (carriage-web--send-json proc "401 Unauthorized"
                                 (list :ok :false :error "auth required" :code "WEB_E_AUTH"))))
     ;; Sessions list
     ((and (string= method "GET") (string= path "/api/sessions"))
      (if (carriage-web--auth-ok headers)
          (carriage-web--handle-sessions proc method path query headers)
        (carriage-web--send-json proc "401 Unauthorized"
                                 (list :ok :false :error "auth required" :code "WEB_E_AUTH"))))
     ;; Per-session details: /api/session/<id>
     ((and (string= method "GET") (string-prefix-p "/api/session/" path))
      (if (carriage-web--auth-ok headers)
          (let ((sid (substring path (length "/api/session/"))))
            (carriage-web--handle-session proc method path sid query headers))
        (carriage-web--send-json proc "401 Unauthorized"
                                 (list :ok :false :error "auth required" :code "WEB_E_AUTH"))))
     ;; Last report summary/items (lightweight): /api/report/last?doc=<id>&limit=N
     ((and (string= method "GET") (string= path "/api/report/last"))
      (if (carriage-web--auth-ok headers)
          (carriage-web--handle-report-last proc method path query headers)
        (carriage-web--send-json proc "401 Unauthorized"
                                 (list :ok :false :error "auth required" :code "WEB_E_AUTH"))))
     ;; Command endpoint
     ((and (string= method "POST") (string= path "/api/cmd"))
      (carriage-web--handle-cmd proc method path query headers body))
     ;; SSE stream
     ((and (string= method "GET") (string= path "/stream"))
      (carriage-web--handle-stream proc method path query headers))
     ;; Fallback 404
     (t
      (carriage-web--send-json proc "404 Not Found"
                               (list :ok :false :error "not found" :code "WEB_E_NOT_FOUND"))))))

;;; Process filter for simple HTTP

(defun carriage-web--process-filter (proc chunk)
  "Accumulate CHUNK in PROC buffer and process HTTP request when complete (supports POST body)."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert chunk)
      ;; Try to find header/body separator
      (let* ((raw (buffer-substring-no-properties (point-min) (point-max)))
             (sep-idx (or (string-match "\r\n\r\n" raw) (string-match "\n\n" raw))))
        (when sep-idx
          (let* ((hdr (substring raw 0 (+ sep-idx (if (eq (aref raw sep-idx) ?\r) 4 2))))
                 (body (substring raw (+ sep-idx (if (eq (aref raw sep-idx) ?\r) 4 2))))
                 (lines (split-string hdr "\r?\n" t))
                 (start (car lines))
                 (head-rest (cdr lines))
                 (parsed (carriage-web--parse-start-line start)))
            (if (not parsed)
                (progn
                  (erase-buffer)
                  (carriage-web--send-json proc "400 Bad Request"
                                           (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD"))
                  (ignore-errors (delete-process proc)))
              (let* ((method (nth 0 parsed))
                     (path   (nth 1 parsed))
                     (query  (nth 2 parsed))
                     (headers (carriage-web--parse-headers head-rest))
                     (clen (or (let ((v (alist-get "content-length" headers nil nil #'string=)))
                                 (and v (string-to-number v)))
                               0)))
                ;; If POST with body and not yet complete, wait for more data
                (if (and (string= method "POST")
                         (> clen 0)
                         (< (string-bytes body) clen))
                    ;; Not enough body yet: keep buffer (do not erase), wait next chunk
                    nil
                  ;; We have enough to dispatch; clear buffer before dispatch to avoid reprocessing
                  (erase-buffer)
                  (carriage-web--dispatch-request
                   proc (list :method method :path path :query query :headers headers :body body))
                  ;; Close connection for non-SSE
                  (unless (string= path "/stream")
                    (ignore-errors (delete-process proc))))))))))))

(defun carriage-web--accept (server proc _msg)
  "Accept callback for SERVER; attach buffer/filter/sentinel to PROC."
  (ignore server)
  (let ((buf (generate-new-buffer (format " *carriage-web:%s*" (process-id proc)))))
    (set-process-buffer proc buf)
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc #'carriage-web--process-filter)
    (set-process-sentinel
     proc
     (lambda (p _s)
       (when (buffer-live-p (process-buffer p))
         (kill-buffer (process-buffer p)))))))

;;; Heartbeat

(defun carriage-web--heartbeat ()
  "Emit a heartbeat SSE event to all clients and prune idle ones."
  (let* ((now (carriage-web--now))
         ;; Idle timeout in seconds for SSE clients (close stale ones).
         (idle 180.0))
    (when carriage-web--clients
      ;; Prune non-live or idle clients.
      (setq carriage-web--clients
            (cl-remove-if
             (lambda (c)
               (let ((p (plist-get c :proc))
                     (last (or (plist-get c :last-ts) now)))
                 (when (or (not (process-live-p p))
                           (> (- now last) idle))
                   (ignore-errors (when (process-live-p p) (delete-process p)))
                   t)))
             carriage-web--clients))
      ;; Emit heartbeat to remaining clients (also updates :last-ts via broadcast).
      (carriage-web--broadcast "heartbeat" (list :type "heartbeat" :ts now)))))

;;; Public control API

;;;###autoload
(defun carriage-web-start ()
  "Start the local dashboard HTTP/SSE server if enabled."
  (interactive)
  (unless carriage-web-enabled
    (user-error "carriage-web is disabled (Customize carriage-web-enabled)"))
  (when (process-live-p carriage-web--server-proc)
    (carriage-web--log "server already running on %s:%s" carriage-web-bind carriage-web-port)
    (cl-return-from carriage-web-start t))
  (let ((proc (make-network-process
               :name "carriage-web"
               :family 'ipv4
               :server t
               :noquery t
               :host carriage-web-bind
               :service carriage-web-port
               :log #'carriage-web--accept)))
    (setq carriage-web--server-proc proc)
    (carriage-web--log "server started on %s:%s" carriage-web-bind carriage-web-port)
    ;; Start heartbeat
    (when (timerp carriage-web--heartbeat-timer)
      (cancel-timer carriage-web--heartbeat-timer))
    (setq carriage-web--heartbeat-timer
          (run-at-time carriage-web-heartbeat-interval
                       carriage-web-heartbeat-interval
                       #'carriage-web--heartbeat))
    ;; Install advices for live publications
    (carriage-web-install-advices)
    t))

;;;###autoload
(defun carriage-web-stop ()
  "Stop the dashboard server and disconnect all SSE clients."
  (interactive)
  (when (timerp carriage-web--debounce-timer)
    (cancel-timer carriage-web--debounce-timer))
  (setq carriage-web--debounce-timer nil)
  (when (timerp carriage-web--heartbeat-timer)
    (cancel-timer carriage-web--heartbeat-timer))
  (setq carriage-web--heartbeat-timer nil)
  (dolist (cli carriage-web--clients)
    (let ((p (plist-get cli :proc)))
      (ignore-errors (delete-process p))))
  (setq carriage-web--clients nil)
  (when (process-live-p carriage-web--server-proc)
    (ignore-errors (delete-process carriage-web--server-proc)))
  (setq carriage-web--server-proc nil)
  ;; Uninstall advices to avoid duplicate publications
  (carriage-web-uninstall-advices)
  (carriage-web--log "server stopped")
  t)

;;;###autoload
(defun carriage-web-restart ()
  "Restart the dashboard server."
  (interactive)
  (carriage-web-stop)
  (carriage-web-start))

;; -----------------------------------------------------------------------------
;; Static HTML and API handlers (MVP)

(defun carriage-web--html-root ()
  "Return minimal HTML for the dashboard."
  (concat
   "<!doctype html><html><head><meta charset='utf-8'/>"
   "<meta http-equiv='X-Frame-Options' content='DENY'/>"
   "<meta http-equiv='Referrer-Policy' content='no-referrer'/>"
   "<title>Carriage Dashboard</title>"
   "<style>"
   "body{font-family:system-ui,-apple-system,Segoe UI,Roboto,Ubuntu,Arial,sans-serif;margin:0;padding:0;}"
   "header{background:#222;color:#fff;padding:10px 16px;font-weight:600}"
   ".wrap{display:flex;min-height:calc(100vh - 48px)}"
   "aside{width:34%;max-width:520px;border-right:1px solid #ddd;padding:12px;box-sizing:border-box}"
   "main{flex:1;padding:12px;box-sizing:border-box}"
   ".small{color:#666;font-size:12px}"
   "table{border-collapse:collapse;width:100%}"
   "th,td{padding:6px 8px;border-bottom:1px solid #eee;text-align:left;font-size:13px}"
   ".phase{font-weight:600}"
   "button{margin:4px 6px 4px 0;padding:6px 10px}"
   "</style>"
   "</head><body>"
   "<header>Carriage — Local Dashboard</header>"
   "<div class='wrap'>"
   "<aside>"
   "<div class='small'>Sessions (live)</div>"
   "<table id='tbl'><thead><tr>"
   "<th>Id</th><th>Phase</th><th>Ctx</th><th>Patches</th></tr></thead><tbody></tbody></table>"
   "</aside>"
   "<main>"
   "<div id='card'><div class='small'>Select a session</div></div>"
   "<div style='margin-top:10px'>"
   "<button onclick='cmd(\"apply_last_iteration\")'>Apply last</button>"
   "<button onclick='cmd(\"abort\")'>Abort</button>"
   "<button onclick='cmd(\"report_open\")'>Open report</button>"
   "</div>"
   "</main>"
   "</div>"
   "<script>"
   "const state={sessions:new Map(), selected:null};"
   "function h(t){return document.createTextNode(t)}"
   "function renderSessions(){"
   " const tb=document.querySelector('#tbl tbody');"
   " tb.innerHTML='';"
   " for(const s of state.sessions.values()){"
   "  const tr=document.createElement('tr'); tr.onclick=()=>{state.selected=s.id; renderCard();};"
   "  const td1=document.createElement('td'); td1.appendChild(h(s.id));"
   "  const td2=document.createElement('td'); td2.appendChild(h((s.state&&s.state.phase)||'-'));"
   "  const td3=document.createElement('td'); td3.appendChild(h(String((s.ctx&&s.ctx.count)||0)));"
   "  const td4=document.createElement('td'); td4.appendChild(h(String((s.patches&&s.patches.count)||0)));"
   "  tr.appendChild(td1); tr.appendChild(td2); tr.appendChild(td3); tr.appendChild(td4);"
   "  tb.appendChild(tr);"
   " }"
   "}"
   "function renderCard(){"
   " const el=document.getElementById('card');"
   " if(!state.selected){el.innerHTML='<div class=small>Select a session</div>';return;}"
   " const s=state.sessions.get(state.selected);"
   " if(!s){el.innerHTML='<div class=small>Unknown session</div>';return;}"
   " el.innerHTML='';"
   " const d=document.createElement('div');"
   " d.innerHTML='<div><b>'+s.id+'</b></div>' +"
   "  '<div class=small>'+(s.title||'')+' • '+(s.mode||'')+' • '+(s.engine||'')+'</div>' +"
   "  '<div class=small>phase='+(s.state&&s.state.phase||'-')+' tooltip='+(s.state&&s.state.tooltip||'-')+'</div>';"
   " el.appendChild(d);"
   "}"
   "async function fetchSessions(){"
   " try{"
   "  const r=await fetch('/api/sessions',{headers:{'Accept':'application/json'}});"
   "  const j=await r.json();"
   "  if(j&&j.ok){"
   "    const arr=j.data||[]; state.sessions.clear();"
   "    for(const s of arr){ state.sessions.set(s.id,s); }"
   "    renderSessions(); if(state.selected) renderCard();"
   "  }"
   " }catch(e){console.error(e)}"
   "}"
   "function applyEvent(ev){"
   "  const doc=ev.doc||ev.payload&&ev.payload.doc; const tp=ev.type||ev.event||'';"
   "  // Minimal optimistic merge: update phase tooltip when available"
   "  if(doc && state.sessions.has(doc)){"
   "    const s=state.sessions.get(doc);"
   "    if(tp==='ui' && ev.state){ s.state=ev.state; }"
   "    if(tp==='apply' && ev.summary){ s._apply=ev.summary; }"
   "    state.sessions.set(doc,s); renderSessions(); if(state.selected===doc) renderCard();"
   "  }"
   "}"
   "async function boot(){"
   " await fetchSessions();"
   " try{"
   "  const es=new EventSource('/stream');"
   "  const on=(t)=>(e)=>{ try{ applyEvent(Object.assign({type:t}, JSON.parse(e.data))); }catch(_e){} };"
   "  es.addEventListener('ui', on('ui'));"
   "  es.addEventListener('apply', on('apply'));"
   "  es.addEventListener('report', on('report'));"
   "  es.addEventListener('transport', on('transport'));"
   "  es.addEventListener('heartbeat', on('heartbeat'));"
   "  es.addEventListener('hello', on('hello'));"
   " }catch(e){ console.error('SSE fail',e); }"
   "}"
   "async function cmd(name){"
   " if(!state.selected){ alert('Select a session first'); return; }"
   " try{"
   "  const r=await fetch('/api/cmd',{method:'POST',headers:{'Content-Type':'application/json','Accept':'application/json'},body:JSON.stringify({cmd:name,doc:state.selected})});"
   "  const j=await r.json();"
   "  if(!(j&&j.ok)){ console.warn('cmd failed',j); alert('Command failed: '+((j&&j.error)||'error')); }"
   " }catch(e){ console.error('cmd error',e); alert('Command error'); }"
   "}"
   "boot();"
   "</script>"
   "</body></html>"))

(defun carriage-web--handle-root (proc _method _path _query _headers)
  "Serve minimal static HTML dashboard."
  (carriage-web--send-response proc "200 OK" (carriage-web--html-root) "text/html; charset=utf-8"))

;; -----------------------------------------------------------------------------
;; Sessions collection and helpers

(defun carriage-web--buffer-file-root (buf)
  "Best-effort project root for BUF using carriage-project-root when available."
  (with-current-buffer buf
    (or (and (fboundp 'carriage-project-root)
             (ignore-errors (carriage-project-root)))
        (and default-directory (expand-file-name default-directory))
        default-directory)))

(defun carriage-web--buffer-id (buf)
  "Compute stable session id for BUF: repo-relative path or ephemeral:<uid>."
  (with-current-buffer buf
    (let* ((bf buffer-file-name))
      (if (and (stringp bf) (file-exists-p bf))
          (let* ((root (carriage-web--buffer-file-root buf))
                 (rel (ignore-errors (file-relative-name bf root))))
            (if (and (stringp rel) (not (string-match-p "\\`\\.\\." rel)))
                (concat (file-name-nondirectory (directory-file-name (or (and root (directory-file-name root)) ""))) "/" rel)
              (or rel bf)))
        (format "ephemeral:%s:%s"
                (buffer-name buf)
                (or (and (fboundp 'buffer-hash) (buffer-hash buf))
                    (format "%x" (sxhash buf))))))))

(defun carriage-web--buffer-title (buf)
  "Short title for BUF."
  (with-current-buffer buf
    (or (and buffer-file-name (file-name-nondirectory buffer-file-name))
        (buffer-name buf))))

(defun carriage-web--buffer-project (buf)
  "Project name (folder name of root) for BUF."
  (with-current-buffer buf
    (let* ((root (carriage-web--buffer-file-root buf))
           (dir (and root (directory-file-name root))))
      (and dir (file-name-nondirectory dir)))))

(defun carriage-web--ctx-count (buf)
  "Return integer context count for BUF via carriage-context-count when available."
  (with-current-buffer buf
    (condition-case _e
        (if (fboundp 'carriage-context-count)
            (or (plist-get (carriage-context-count buf (point)) :count) 0)
          0)
      (error 0))))

(defun carriage-web--buffer-engine-policy (buf)
  "Compose engine string like git:<policy>|emacs for BUF."
  (with-current-buffer buf
    (let* ((eng (if (boundp 'carriage-apply-engine) carriage-apply-engine 'git))
           (policy (and (eq eng 'git)
                        (boundp 'carriage-git-branch-policy)
                        carriage-git-branch-policy)))
      (if (eq eng 'git)
          (format "git:%s" (or (and policy (symbol-name policy)) "in-place"))
        "emacs"))))

(defun carriage-web--buffer-state (buf)
  "Return state plist {:phase .. :tooltip ..} if available, otherwise idle."
  (with-current-buffer buf
    (let ((phase "idle")
          (tip ""))
      (list :phase phase :tooltip tip))))

(defun carriage-web--collect-sessions ()
  "Return a list of session snapshot plists for active Carriage buffers."
  (let ((acc '()))
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (and (buffer-live-p b)
                   (not (minibufferp b))
                   ;; Consider buffers with carriage-mode or files as candidates.
                   (or (and (boundp 'carriage-mode) carriage-mode)
                       buffer-file-name))
          (let* ((id (carriage-web--buffer-id b))
                 (title (carriage-web--buffer-title b))
                 (project (or (carriage-web--buffer-project b) ""))
                 (mode (symbol-name major-mode))
                 (intent (or (and (boundp 'carriage-intent) (symbol-name carriage-intent)) "Code"))
                 (suite (or (and (boundp 'carriage-suite) (symbol-name carriage-suite)) "udiff"))
                 (engine (carriage-web--buffer-engine-policy b))
                 (branch nil)
                 (ctxc (carriage-web--ctx-count b))
                 (patches 0)
                 (state (carriage-web--buffer-state b)))
            (push (list :id id :title title :project project :mode mode
                        :intent intent :suite suite :engine engine :branch branch
                        :ctx (list :count ctxc)
                        :patches (list :count patches)
                        :state state)
                  acc)))))
    (nreverse acc)))

;; -----------------------------------------------------------------------------
;; Command handler and advices

(defvar carriage-web--advices-installed nil
  "Non-nil when Carriage Web dashboard advices are installed.")

(defun carriage-web--doc-id-of-current ()
  "Compute document/session id for current buffer."
  (carriage-web--buffer-id (current-buffer)))

(defun carriage-web--pub-ui (summary)
  "Publish a UI event with SUMMARY for the current buffer."
  (let ((doc (carriage-web--doc-id-of-current)))
    (carriage-web-pub "ui" (list :type "ui" :doc doc :state summary))))

(defun carriage-web--adv-ui-note (pl &rest _)
  "Advice after carriage-ui-note-apply-summary to publish UI event and cache summaries."
  (ignore-errors
    ;; Publish UI event
    (carriage-web--pub-ui pl)
    ;; Cache apply/dry-run summary if available
    (let* ((phase (and (listp pl) (plist-get pl :phase))))
      (when (memq phase '(apply dry-run))
        (let* ((ok   (or (plist-get pl :ok) 0))
               (fail (or (plist-get pl :fail) 0))
               (skip (or (plist-get pl :skip) 0))
               (tot  (or (plist-get pl :total) (+ ok fail skip)))
               (sum  (list :ok ok :fail fail :skipped skip :total tot
                           :ts (format-time-string "%FT%T%z"))))
          (puthash (carriage-web--doc-id-of-current)
                   sum
                   carriage-web--last-apply-summary))))))

(defun carriage-web--adv-report-open (report &rest _)
  "Advice after carriage-report-open to publish report summary event and cache it."
  (when (and (listp report))
    (let* ((sum (plist-get report :summary))
           (doc (carriage-web--doc-id-of-current)))
      (when sum
        (puthash doc sum carriage-web--last-report-summary)
        (carriage-web-pub "report" (list :type "report" :doc doc :summary sum))))))

(defun carriage-web--adv-transport-begin (&rest _)
  (let ((doc (carriage-web--doc-id-of-current)))
    (carriage-web-pub "transport" (list :type "transport" :doc doc :phase "begin" :meta (list)))))

(defun carriage-web--adv-transport-streaming (&rest _)
  (let ((doc (carriage-web--doc-id-of-current)))
    (carriage-web-pub "transport" (list :type "transport" :doc doc :phase "streaming" :meta (list)))))

(defun carriage-web--adv-transport-complete (&rest _)
  (let ((doc (carriage-web--doc-id-of-current)))
    (carriage-web-pub "transport" (list :type "transport" :doc doc :phase "complete" :meta (list)))))

(defun carriage-web-install-advices ()
  "Install publication advices once."
  (unless carriage-web--advices-installed
    (when (fboundp 'carriage-ui-note-apply-summary)
      (advice-add 'carriage-ui-note-apply-summary :after #'carriage-web--adv-ui-note))
    (when (fboundp 'carriage-report-open)
      (advice-add 'carriage-report-open :after #'carriage-web--adv-report-open))
    (when (fboundp 'carriage-transport-begin)
      (advice-add 'carriage-transport-begin :after #'carriage-web--adv-transport-begin))
    (when (fboundp 'carriage-transport-streaming)
      (advice-add 'carriage-transport-streaming :after #'carriage-web--adv-transport-streaming))
    (when (fboundp 'carriage-transport-complete)
      (advice-add 'carriage-transport-complete :after #'carriage-web--adv-transport-complete))
    (setq carriage-web--advices-installed t)))

(defun carriage-web-uninstall-advices ()
  "Remove publication advices if installed."
  (when carriage-web--advices-installed
    (ignore-errors (advice-remove 'carriage-ui-note-apply-summary #'carriage-web--adv-ui-note))
    (ignore-errors (advice-remove 'carriage-report-open #'carriage-web--adv-report-open))
    (ignore-errors (advice-remove 'carriage-transport-begin #'carriage-web--adv-transport-begin))
    (ignore-errors (advice-remove 'carriage-transport-streaming #'carriage-web--adv-transport-streaming))
    (ignore-errors (advice-remove 'carriage-transport-complete #'carriage-web--adv-transport-complete))
    (setq carriage-web--advices-installed nil)))

(defun carriage-web--find-buffer-by-id (doc)
  "Return buffer corresponding to DOC id, or nil."
  (cl-find-if
   (lambda (b) (string= (carriage-web--buffer-id b) doc))
   (buffer-list)))

(defun carriage-web--toggle-in-buffer (buf key)
  "Toggle context/profile KEY in BUF safely."
  (with-current-buffer buf
    (pcase key
      ("ctx"
       (cond
        ((boundp 'carriage-mode-include-doc-context)
         (setq-local carriage-mode-include-doc-context (not carriage-mode-include-doc-context)))
        ((boundp 'carriage-mode-context-attach-files)
         (setq-local carriage-mode-context-attach-files (not carriage-mode-context-attach-files)))))
      ("files"
       (when (boundp 'carriage-mode-context-attach-files)
         (setq-local carriage-mode-context-attach-files (not carriage-mode-context-attach-files))))
      ("visible"
       (when (boundp 'carriage-mode-include-visible-context)
         (setq-local carriage-mode-include-visible-context (not carriage-mode-include-visible-context))))
      ("patched"
       (when (boundp 'carriage-mode-include-patched-files)
         (setq-local carriage-mode-include-patched-files (not carriage-mode-include-patched-files))))
      ("profile"
       (when (fboundp 'carriage-context-profile-set)
         (carriage-context-profile-set (if (and (boundp 'carriage-context-profile)
                                                (eq carriage-context-profile 'p1))
                                           'p3 'p1)))))
    t))

(defun carriage-web--json-parse (s)
  "Parse JSON string S into an alist/plist (prefers json-read-from-string for broad compat)."
  (condition-case _e
      (if (fboundp 'json-parse-string)
          (json-parse-string s :object-type 'alist :array-type 'list)
        (json-read-from-string s))
    (error nil)))

(defun carriage-web--handle-cmd (proc method path _query headers body)
  "Handle POST /api/cmd with JSON BODY."
  (unless (and (string= method "POST") (string= path "/api/cmd"))
    (carriage-web--send-json proc "404 Not Found"
                             (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND")))
  (if (not (carriage-web--auth-ok headers))
      (carriage-web--send-json proc "401 Unauthorized"
                               (list :ok json-false :error "auth required" :code "WEB_E_AUTH"))
    (let* ((obj (and (stringp body) (carriage-web--json-parse body)))
           (cmd (and obj (or (alist-get 'cmd obj) (plist-get obj :cmd))))
           (doc (and obj (or (alist-get 'doc obj) (plist-get obj :doc)))))
      (cond
       ((not (and (stringp cmd) (> (length cmd) 0)))
        (carriage-web--send-json proc "400 Bad Request"
                                 (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD")))
       ((string= cmd "toggle")
        (let ((key (or (and obj (alist-get 'key obj)) (plist-get obj :key))))
          (if (not (and (stringp key) doc))
              (carriage-web--send-json proc "400 Bad Request"
                                       (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD"))
            (let ((buf (carriage-web--find-buffer-by-id doc)))
              (if (not (buffer-live-p buf))
                  (carriage-web--send-json proc "404 Not Found"
                                           (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))
                (carriage-web--toggle-in-buffer buf key)
                (carriage-web--send-json proc "200 OK" (list :ok t)))))))
       ((member cmd '("report_open" "apply_last_iteration" "abort" "commit_all" "commit_last" "wip"))
        (pcase cmd
          ("report_open"
           (let ((buf (and doc (carriage-web--find-buffer-by-id doc))))
             (if (not (buffer-live-p buf))
                 (carriage-web--send-json proc "404 Not Found"
                                          (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))
               (with-current-buffer buf
                 (when (fboundp 'carriage-report-open)
                   (ignore-errors (carriage-report-open))))
               (carriage-web--send-json proc "200 OK" (list :ok t)))))
          ("apply_last_iteration"
           (let ((buf (and doc (carriage-web--find-buffer-by-id doc))))
             (if (not (buffer-live-p buf))
                 (carriage-web--send-json proc "404 Not Found"
                                          (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))
               (with-current-buffer buf
                 (cond
                  ((fboundp 'carriage-apply-last-iteration)
                   (ignore-errors (carriage-apply-last-iteration)))
                  (t
                   (carriage-web--send-json proc "400 Bad Request"
                                            (list :ok json-false :error "unsupported" :code "WEB_E_CMD")))))
               (carriage-web--send-json proc "200 OK" (list :ok t)))))
          ("abort"
           (cond
            ((fboundp 'carriage-abort-current)
             (ignore-errors (carriage-abort-current))
             (carriage-web--send-json proc "200 OK" (list :ok t)))
            ((and (boundp 'carriage--abort-handler) (functionp carriage--abort-handler))
             (ignore-errors (funcall carriage--abort-handler))
             (carriage-web--send-json proc "200 OK" (list :ok t)))
            (t
             (carriage-web--send-json proc "400 Bad Request"
                                      (list :ok json-false :error "unsupported" :code "WEB_E_CMD")))))
          (_
           (carriage-web--send-json proc "400 Bad Request"
                                    (list :ok json-false :error "unsupported" :code "WEB_E_CMD")))))
       (t
        (carriage-web--send-json proc "400 Bad Request"
                                 (list :ok json-false :error "unsupported" :code "WEB_E_CMD")))))))


(defun carriage-web--handle-sessions (proc _method _path _query headers)
  "Return sessions list."
  (let ((data (carriage-web--collect-sessions)))
    (carriage-web--send-json proc "200 OK" (list :ok t :data data))))

(defun carriage-web--handle-session (proc _method _path sid _query _headers)
  "Return per-session snapshot for SID."
  (let ((buf (carriage-web--find-buffer-by-id sid)))
    (if (not (buffer-live-p buf))
        (carriage-web--send-json proc "404 Not Found"
                                 (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))
      (with-current-buffer buf
        (let* ((id id) ; shadow-safe
               (id (carriage-web--buffer-id buf))
               (title (carriage-web--buffer-title buf))
               (project (or (carriage-web--buffer-project buf) ""))
               (mode (symbol-name major-mode))
               (intent (or (and (boundp 'carriage-intent) (symbol-name carriage-intent)) "Code"))
               (suite (or (and (boundp 'carriage-suite) (symbol-name carriage-suite)) "udiff"))
               (engine (carriage-web--buffer-engine-policy buf))
               (branch nil)
               (ctxc (carriage-web--ctx-count buf))
               (patches 0)
               (state (carriage-web--buffer-state buf))
               (apply-last (gethash id carriage-web--last-apply-summary))
               (data (list :id id :title title :project project :mode mode
                           :intent intent :suite suite :engine engine :branch branch
                           :ctx (list :count ctxc :sources (list :doc 0 :gptel 0 :visible 0 :both 0))
                           :patches (list :count patches)
                           :state state
                           :apply (list :last apply-last))))
          (carriage-web--send-json proc "200 OK" (list :ok t :data data)))))))

(defun carriage-web--handle-report-last (proc _method _path query _headers)
  "Return last report summary/items for a doc from QUERY."
  (let* ((doc (cdr (cl-find "doc" query :key #'car  :test #'string=))))
    (cond
     ((not doc)
      (carriage-web--send-json proc "400 Bad Request"
                               (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD")))
     (t
      (let ((sum (gethash doc carriage-web--last-report-summary)))
        (if (not sum)
            (carriage-web--send-json proc "404 Not Found"
                                     (list :ok json-false :error "no-report" :code "WEB_E_NOT_FOUND"))
          (carriage-web--send-json proc "200 OK"
                                   (list :ok t :data (list :summary sum :items '())))))))))

(provide 'carriage-web)
;;; carriage-web.el ends here
