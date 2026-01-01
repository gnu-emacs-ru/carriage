;;; carriage-hub.el --- Swarm Hub: registry + proxy + unified dashboard  -*- lexical-binding: t; -*-

;; Hub runs as a separate Emacs process:
;; - reads swarm registry
;; - lists agents
;; - proxies browser requests to agents, injecting X-Auth server-side
;; - browser never sees agent tokens
;;
;; Specifications:
;;   spec/swarm-v1.org
;;   spec/hub-http-api-v1.org
;;   spec/registry-v1.org

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)

(require 'carriage-swarm-registry)

(defgroup carriage-hub nil
  "Carriage Swarm Hub."
  :group 'carriage)

(defcustom carriage-hub-bind "127.0.0.1"
  "Hub bind address."
  :type 'string
  :group 'carriage-hub)

(defcustom carriage-hub-port 8787
  "Hub port."
  :type 'integer
  :group 'carriage-hub)

(defcustom carriage-hub-max-request-bytes 2097152
  "Max request body size."
  :type 'integer
  :group 'carriage-hub)

(defcustom carriage-hub-proxy-connect-timeout-sec 0.5
  "Bounded upstream connect timeout (seconds) for Hub proxy.
If the upstream (Agent) does not connect/respond at all within this time, the proxy MUST fail.
This corresponds to Swarm FREEZE proxy-connect-timeout-sec (spec/swarm-v1.org#INV)."
  :type 'number
  :group 'carriage-hub)

(defcustom carriage-hub-proxy-read-timeout-sec 2.0
  "Bounded upstream read timeout (seconds) for Hub proxy for non-SSE requests.
If a non-SSE upstream stalls (no bytes received) longer than this timeout, the proxy MUST fail.
This corresponds to Swarm FREEZE proxy-read-timeout-sec (spec/swarm-v1.org#INV)."
  :type 'number
  :group 'carriage-hub)

(defcustom carriage-hub-max-proxy-active 32
  "Maximum number of concurrent proxy upstream connections."
  :type 'integer
  :group 'carriage-hub)

(defcustom carriage-hub-probe-interval 5.0
  "Seconds between background liveness probes that update registry alive/last_seen_ts."
  :type 'number
  :group 'carriage-hub)

(defcustom carriage-hub-probe-timeout 0.5
  "Seconds to wait for an Agent /api/health response during liveness probe."
  :type 'number
  :group 'carriage-hub)

(defcustom carriage-hub-probe-max-per-interval 12
  "Maximum number of agents to probe per `carriage-hub-probe-interval' tick."
  :type 'integer
  :group 'carriage-hub)

(defvar carriage-hub--server nil)
(defvar carriage-hub--probe-timer nil)
(defvar carriage-hub--proxy-active 0)
(defvar carriage-hub--probe-cursor 0
  "Round-robin cursor for bounded liveness probing.")

(defun carriage-hub--http-date ()
  (format-time-string "%a, %d %b %Y %T GMT" (current-time) t))

(defun carriage-hub--status-line (status)
  (format "HTTP/1.1 %s\r\n" status))

(defun carriage-hub--security-headers ()
  (concat
   "X-Frame-Options: DENY\r\n"
   "X-Content-Type-Options: nosniff\r\n"
   "Referrer-Policy: no-referrer\r\n"))

(defun carriage-hub--send-raw (proc s)
  (when (and (process-live-p proc) (stringp s))
    (ignore-errors (process-send-string proc s))))

(defun carriage-hub--send-response (proc status content-type body &optional keep-open)
  (let* ((b (encode-coding-string (or body "") 'utf-8 t))
         (bytes (string-bytes b))
         (hdr (concat
               (carriage-hub--status-line status)
               (format "Date: %s\r\n" (carriage-hub--http-date))
               "Server: carriage-hub\r\n"
               (carriage-hub--security-headers)
               (format "Content-Type: %s\r\n" content-type)
               (format "Content-Length: %d\r\n" bytes)
               (format "Connection: %s\r\n" (if keep-open "keep-alive" "close"))
               "\r\n")))
    (carriage-hub--send-raw proc hdr)
    (carriage-hub--send-raw proc b)
    (unless keep-open
      (ignore-errors (process-send-eof proc)))
    t))

(defun carriage-hub--send-json (proc status obj)
  (carriage-hub--send-response proc status "application/json; charset=utf-8" (json-encode obj)))

(defun carriage-hub--parse-start-line (line)
  (when (and (stringp line) (> (length line) 0))
    (let* ((s (string-trim-right line "\r+"))
           (case-fold-search t))
      (when (string-match "\\`\\([A-Za-z]+\\)[ \t]+\\([^ \t]+\\)[ \t]+HTTP/1\\.[01]\\'" s)
        (list (upcase (match-string 1 s))
              (match-string 2 s))))))

(defun carriage-hub--parse-headers (lines)
  (let (acc)
    (dolist (ln lines)
      (when (and (stringp ln)
                 (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" ln))
        (push (cons (downcase (string-trim (match-string 1 ln)))
                    (string-trim (match-string 2 ln)))
              acc)))
    (nreverse acc)))

(defun carriage-hub--split-head-body (data)
  (cond
   ((string-match "\r\n\r\n" data)
    (cons (substring data 0 (match-beginning 0))
          (substring data (match-end 0))))
   ((string-match "\n\n" data)
    (cons (substring data 0 (match-beginning 0))
          (substring data (match-end 0))))
   (t (cons nil nil))))

(defun carriage-hub--agent-by-id (id)
  "Return registry entry alist for agent ID (or nil)."
  (cl-find-if (lambda (e) (equal (alist-get 'id e) id))
              (carriage-swarm-registry-read)))

(defun carriage-hub--agent-token (id)
  (carriage-swarm-registry-agent-read-token id))

(defun carriage-hub--agent-port (id)
  (carriage-swarm-registry-agent-read-port id))

(defun carriage-hub--agent-health-probe (id)
  "Best-effort health probe for agent ID with bounded timeout.
Returns non-nil on 200 OK from /api/health, nil otherwise."
  (let* ((port (carriage-hub--agent-port id))
         (tok (carriage-hub--agent-token id)))
    (when (and (integerp port) (> port 0) (stringp tok) (> (length tok) 0))
      (let* ((done nil)
             (ok nil)
             (deadline (+ (float-time) (max 0.05 (or carriage-hub-probe-timeout 0.5))))
             (buf ""))
        (condition-case _e
            (let* ((up (make-network-process
                        :name "carriage-hub-probe"
                        :buffer nil
                        :host "127.0.0.1"
                        :service port
                        :noquery t
                        :family 'ipv4
                        :coding 'binary)))
              (set-process-filter
               up
               (lambda (p chunk)
                 (setq buf (concat buf chunk))
                 ;; Quick check: status line present?
                 (when (and (not done)
                            (string-match-p "\\`HTTP/1\\.[01] 200\\b" buf))
                   (setq ok t done t))
                 ;; Any response ends the probe early (even if non-200).
                 (when (and (not done)
                            (or (string-match-p "\\`HTTP/1\\.[01] [0-9][0-9][0-9]\\b" buf)
                                (string-match-p "\r\n\r\n" buf)))
                   (setq done t))))
              (set-process-sentinel
               up
               (lambda (_p _e) (setq done t)))
              (process-send-string
               up
               (concat
                "GET /api/health HTTP/1.1\r\n"
                (format "Host: 127.0.0.1:%d\r\n" port)
                "Connection: close\r\n"
                (format "X-Auth: %s\r\n" tok)
                "\r\n"))
              (process-send-eof up)
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output up 0.05))
              (ignore-errors (delete-process up))
              ok)
          (error nil))))))

(defun carriage-hub--probe-tick ()
  "Probe a bounded number of agents and update registry liveness.

Uses a round-robin cursor (`carriage-hub--probe-cursor') so all agents get
probed over time, bounded by `carriage-hub-probe-max-per-interval'."
  (let* ((entries (carriage-swarm-registry-read))
         (len (length entries))
         (maxn (max 1 (or carriage-hub-probe-max-per-interval 12)))
         (start (if (> len 0) (mod (or carriage-hub--probe-cursor 0) len) 0))
         (n 0))
    (when (= len 0)
      (setq carriage-hub--probe-cursor 0))
    (while (and (> len 0) (< n (min maxn len)))
      (let* ((idx (mod (+ start n) len))
             (e (nth idx entries))
             (id (alist-get 'id e)))
        (when (stringp id)
          (let ((alive (and (carriage-swarm-registry-agent-alive-p id)
                            (carriage-hub--agent-health-probe id))))
            (ignore-errors
              (carriage-swarm-registry-set-liveness id alive (float-time))))))
      (setq n (1+ n)))
    (when (> len 0)
      (setq carriage-hub--probe-cursor (mod (+ start n) len)))))

(defun carriage-hub--html-root ()
  (concat
   "<!doctype html><html><head><meta charset='utf-8'/>"
   "<meta name='viewport' content='width=device-width, initial-scale=1'/>"
   "<title>Carriage — Swarm Hub</title>"
   "<style>"
   "html,body{margin:0;padding:0;background:#0f1117;color:#e6e6e6;font-family:system-ui,Segoe UI,Roboto,Helvetica,Arial,sans-serif}"
   "header{padding:.8rem 1rem;border-bottom:1px solid #222;display:flex;gap:1rem;align-items:center}"
   ".wrap{display:grid;grid-template-columns:360px 1fr;min-height:calc(100vh - 56px)}"
   ".side{border-right:1px solid #222;min-height:0;overflow:auto}"
   ".row{padding:.6rem 1rem;border-bottom:1px solid #171923;cursor:pointer}"
   ".row:hover{background:#151823}"
   ".row.active{background:#1a1f2e}"
   ".main{padding:1rem;min-width:0}"
   ".muted{color:#9aa4b2}"
   "</style></head><body>"
   "<header><div style='font-weight:600'>Carriage — Swarm Hub</div><div class='muted' id='st'>offline</div></header>"
   "<div class='wrap'>"
   "<div class='side'><div class='row muted' style='cursor:default;font-weight:600'>Agents</div><div id='list'></div></div>"
   "<div class='main'>"
   "<div style='font-weight:600;margin-bottom:.4rem'>Selected</div><div id='sel' class='muted'>—</div>"
   "<div style='margin-top:1rem;font-weight:600'>Sessions</div><pre id='sessions' class='muted' style='white-space:pre-wrap'></pre>"
   "</div></div>"
   "<script>(function(){"
   "const S={id:null}; const $=s=>document.querySelector(s);"
   "function j(r){return r.ok?r.json():r.text().then(t=>{throw new Error(t||('HTTP '+r.status))});}"
   "function setSt(t){$('#st').textContent=t;}"
   "function render(list){ const box=$('#list'); box.innerHTML=''; (list||[]).forEach(a=>{"
   " const r=document.createElement('div'); r.className='row'; r.textContent=a.id+'  '+(a.alive?'(alive)':'(dead)')+'  '+(a.label||'');"
   " r.onclick=()=>select(a.id); box.appendChild(r); }); }"
   "function refresh(){ fetch('/hub/agents').then(j).then(o=>{ if(o&&o.ok) render(o.data||[]); setSt('online'); }).catch(()=>setSt('error')); }"
   "function select(id){ S.id=id; $('#sel').textContent=id; fetch('/hub/agent/'+encodeURIComponent(id)+'/api/sessions').then(j).then(o=>{"
   "  $('#sessions').textContent=JSON.stringify(o,null,2); }).catch(e=>{$('#sessions').textContent=String(e);}); }"
   "document.addEventListener('DOMContentLoaded',()=>{refresh(); setInterval(refresh,5000);});"
   "})();</script>"
   "</body></html>"))

(defun carriage-hub--agents-payload ()
  (let ((entries (carriage-swarm-registry-read)))
    (mapcar
     (lambda (e)
       (let* ((id (alist-get 'id e))
              (pid (alist-get 'pid e))
              (bind (or (alist-get 'bind e) "127.0.0.1"))
              (port (alist-get 'port e))
              (meta (alist-get 'meta e))
              (label (and (listp meta) (alist-get 'label meta)))
              (project (and (listp meta) (alist-get 'project meta)))
              (alive (and (stringp id) (carriage-swarm-registry-agent-alive-p id))))
         `((id . ,id)
           (pid . ,pid)
           (bind . ,bind)
           (port . ,port)
           (label . ,label)
           (project . ,project)
           (alive . ,(if alive t :false))
           (last_seen . ,(or (alist-get 'last_seen_ts e) :null)))))
     entries)))

(defun carriage-hub--proxy--send-error (client status error code)
  (carriage-hub--send-json client status (list :ok :false :error error :code code)))

(defun carriage-hub--proxy--overloaded-p ()
  (and (integerp carriage-hub-max-proxy-active)
       (>= carriage-hub--proxy-active carriage-hub-max-proxy-active)))

(defun carriage-hub--proxy--body-too-large-p (body)
  (and (integerp carriage-hub-max-request-bytes)
       (> (string-bytes (or body "")) carriage-hub-max-request-bytes)))

(defun carriage-hub--proxy--agent-conn (id)
  "Return plist (:port N :tok STRING) or nil."
  (let* ((port (carriage-hub--agent-port id))
         (tok (carriage-hub--agent-token id)))
    (when (and (integerp port) (> port 0) (stringp tok) (> (length tok) 0))
      (list :port port :tok tok))))

(defun carriage-hub--proxy--is-sse-p (path)
  (string-prefix-p "/stream" path))

(defun carriage-hub--proxy--build-request (method path headers body port tok is-sse)
  (let* ((len (string-bytes (encode-coding-string (or body "") 'utf-8 t)))
         (host (format "127.0.0.1:%d" port))
         (ctype (or (assoc-default "content-type" headers)
                    (and (string= method "POST") "application/json; charset=utf-8")))
         (extra
          (mapconcat
           (lambda (kv)
             (let ((k (car kv)) (v (cdr kv)))
               (cond
                ((member k '("host" "connection" "content-length" "x-auth")) "")
                (t (format "%s: %s\r\n" k v)))))
           headers
           "")))
    (concat
     (format "%s %s HTTP/1.1\r\n" method path)
     (format "Host: %s\r\n" host)
     (format "Connection: %s\r\n" (if is-sse "keep-alive" "close"))
     (when is-sse "Accept: text/event-stream\r\n")
     (format "X-Auth: %s\r\n" tok)
     (if ctype (format "Content-Type: %s\r\n" ctype) "")
     (format "Content-Length: %d\r\n" len)
     extra
     "\r\n"
     (encode-coding-string (or body "") 'utf-8 t))))

(defun carriage-hub--proxy--connect-and-stream (client port req is-sse connect-timeout read-timeout)
  "Connect to upstream PORT and stream response bytes to CLIENT.
REQ is a full HTTP request string already containing injected X-Auth."
  (cl-incf carriage-hub--proxy-active)
  (let* ((up nil)
         (cl-proc client)
         (connect-timer nil)
         (read-timer nil)
         (sentinel-installed nil)
         (decremented nil)
         (first-byte-seen nil))
    (cl-labels
        ((dec-active ()
           (unless decremented
             (setq decremented t)
             (when (and (integerp carriage-hub--proxy-active)
                        (> carriage-hub--proxy-active 0))
               (cl-decf carriage-hub--proxy-active))))
         (cleanup-timers ()
           (when (timerp connect-timer) (cancel-timer connect-timer))
           (setq connect-timer nil)
           (when (timerp read-timer) (cancel-timer read-timer))
           (setq read-timer nil))
         (kill-upstream ()
           (when (processp up)
             (ignore-errors (delete-process up))))
         (close-downstream ()
           (when (process-live-p cl-proc)
             (ignore-errors (process-send-eof cl-proc))))
         (arm-connect-timeout ()
           (setq connect-timer
                 (run-at-time
                  connect-timeout nil
                  (lambda ()
                    (unless first-byte-seen
                      ;; IMPORTANT: send envelope BEFORE killing upstream, otherwise upstream sentinel
                      ;; may close downstream first and the client sees an empty response.
                      (ignore-errors
                        (carriage-hub--send-json cl-proc "504 Gateway Timeout"
                                                 (list :ok :false :error "upstream timeout" :code "WEB_E_NOT_FOUND")))
                      ;; Do not rely on upstream sentinel to close the client connection:
                      ;; when upstream is missing or sentinel doesn't run promptly, tests may see an empty response.
                      (close-downstream)
                      (dec-active)
                      (kill-upstream))))))
         (arm-read-timeout ()
           (when (and (not is-sse)
                      (numberp read-timeout)
                      (> read-timeout 0))
             (when (timerp read-timer)
               (cancel-timer read-timer))
             (setq read-timer
                   (run-at-time
                    read-timeout nil
                    (lambda ()
                      ;; Same ordering rule as connect-timeout: respond first.
                      (ignore-errors
                        (carriage-hub--send-json cl-proc "504 Gateway Timeout"
                                                 (list :ok :false :error "upstream read timeout" :code "WEB_E_NOT_FOUND")))
                      (dec-active)
                      (kill-upstream)
                      (close-downstream)))))))
      (unwind-protect
          (condition-case _e
              (progn
                (setq up (make-network-process
                          :name "carriage-hub-proxy-up"
                          :buffer nil
                          :host "127.0.0.1"
                          :service port
                          :noquery t
                          :family 'ipv4
                          :coding 'binary))
                (process-put cl-proc :hub-upstream up)

                ;; If absolutely no bytes arrive quickly, fail deterministically with an envelope.
                (arm-connect-timeout)

                (set-process-filter
                 up
                 (lambda (_p chunk)
                   (setq first-byte-seen t)
                   ;; First byte cancels connect timeout.
                   (when (timerp connect-timer)
                     (cancel-timer connect-timer)
                     (setq connect-timer nil))
                   ;; For non-SSE: enforce bounded upstream read timeout as "no-bytes" watchdog.
                   (arm-read-timeout)
                   (if (process-live-p cl-proc)
                       ;; SSE: forward raw bytes; no rebuffer/parse/repackage. If downstream is slow
                       ;; enough to overflow process output buffer, process-send-string errors → we disconnect.
                       (condition-case _send
                           (process-send-string cl-proc chunk)
                         (error
                          (kill-upstream)
                          (close-downstream)))
                     (kill-upstream))))

                (set-process-sentinel
                 up
                 (lambda (p _e)
                   (cleanup-timers)
                   (dec-active)
                   (close-downstream)
                   (ignore-errors (delete-process p))))
                (setq sentinel-installed t)

                (process-send-string up req)
                ;; IMPORTANT:
                ;; Do NOT call `process-send-eof' for upstream connections.
                ;; In practice (and in tests) it may close the TCP connection entirely,
                ;; causing the upstream sentinel to fire immediately (before any bytes
                ;; are received) and thus cancelling our timeout logic and closing the
                ;; downstream with an empty response.
                ;;
                ;; For non-SSE requests we already send "Connection: close" in the
                ;; proxied HTTP request; upstream will close after replying.
                t)
            (error
             ;; Fail fast with bounded cleanup (avoid active slot leaks).
             (cleanup-timers)
             (unless sentinel-installed
               (dec-active))
             (kill-upstream)
             ;; IMPORTANT: respond before closing downstream.
             (ignore-errors
               (carriage-hub--send-json cl-proc "502 Bad Gateway"
                                        (list :ok :false :error "proxy failure" :code "WEB_E_NOT_FOUND")))
             (close-downstream)
             t))
        (unless sentinel-installed
          (cleanup-timers)
          (dec-active)
          (kill-upstream))))))

(defun carriage-hub--proxy (client id method path headers body)
  "Proxy a request to an Agent, injecting X-Auth from registry token file.
PATH is the agent-side path (e.g. /api/health, /stream).

Hard requirements (Swarm v1):
- bounded upstream connect timeout
- bounded upstream read timeout for non-SSE
- hard cap on concurrent upstream connections
- no token leakage to browser (Hub injects X-Auth; browser never sees tokens)"
  (cl-block carriage-hub--proxy
    ;; Hard cap (must apply before any agent lookup so overload is deterministic).
    (when (carriage-hub--proxy--overloaded-p)
      (carriage-hub--proxy--send-error client "503 Service Unavailable" "proxy overloaded" "WEB_E_CAP")
      (cl-return-from carriage-hub--proxy t))

    ;; Payload cap (Hub must reject large bodies early).
    (when (carriage-hub--proxy--body-too-large-p body)
      (carriage-hub--proxy--send-error client "400 Bad Request" "payload too large" "WEB_E_PAYLOAD")
      (cl-return-from carriage-hub--proxy t))

    (let* ((conn (carriage-hub--proxy--agent-conn id)))
      (unless conn
        (carriage-hub--proxy--send-error client "502 Bad Gateway" "agent not available" "WEB_E_NOT_FOUND")
        (cl-return-from carriage-hub--proxy t))

      (let* ((port (plist-get conn :port))
             (tok (plist-get conn :tok))
             (is-sse (carriage-hub--proxy--is-sse-p path))
             (connect-timeout (max 0.05 (or carriage-hub-proxy-connect-timeout-sec 0.5)))
             (read-timeout (max 0.10 (or carriage-hub-proxy-read-timeout-sec 2.0)))
             (req (carriage-hub--proxy--build-request method path headers body port tok is-sse)))
        (carriage-hub--proxy--connect-and-stream client port req is-sse connect-timeout read-timeout)))))

(defun carriage-hub--dispatch (proc method path headers body)
  (cond
   ((and (string= method "GET") (string= path "/"))
    (carriage-hub--send-response proc "200 OK" "text/html; charset=utf-8" (carriage-hub--html-root)))
   ((and (string= method "GET") (string= path "/hub/health"))
    (carriage-hub--send-json proc "200 OK" (list :ok t :data (list :version "v1" :ts (float-time)))))
   ((and (string= method "GET") (string= path "/hub/metrics"))
    (carriage-hub--send-json proc "200 OK"
                             (list :ok t :data (list :agents (length (carriage-swarm-registry-read))
                                                     :proxy_active carriage-hub--proxy-active
                                                     :ts (float-time)))))
   ((and (string= method "GET") (string= path "/hub/agents"))
    (carriage-hub--send-json proc "200 OK" (list :ok t :data (carriage-hub--agents-payload))))
   ((and (string= method "POST") (string= path "/hub/cleanup"))
    (let ((res (ignore-errors (carriage-swarm-registry-gc-stale nil))))
      (carriage-hub--send-json proc "200 OK" (list :ok t :data res))))
   ((and (string-prefix-p "/hub/agent/" path))
    (let* ((rest (substring path (length "/hub/agent/")))
           (slash (string-match-p "/" rest))
           (id (if slash (substring rest 0 slash) rest))
           (tail (if slash (substring rest slash) "/"))
           ;; tail begins with /api/... or /stream
           (agent-path tail))
      (carriage-hub--proxy proc id method agent-path headers body)))
   (t
    (carriage-hub--send-json proc "404 Not Found" (list :ok :false :error "not found" :code "WEB_E_NOT_FOUND")))))

(defun carriage-hub--ensure-client-buffer (proc)
  (or (process-buffer proc)
      (let ((b (generate-new-buffer (format " *%s*" (process-name proc)))))
        (set-process-buffer proc b)
        b)))

(defun carriage-hub--sentinel (proc _msg)
  (let ((up (process-get proc :hub-upstream)))
    (when (processp up)
      (ignore-errors (delete-process up)))))

(defun carriage-hub--process-filter (proc chunk)
  (with-current-buffer (carriage-hub--ensure-client-buffer proc)
    (goto-char (point-max))
    (insert chunk)
    (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
           (pair (carriage-hub--split-head-body data))
           (head (car pair))
           (rest (cdr pair)))
      (when head
        (let* ((lines (split-string head "\r?\n"))
               (start (car lines))
               (hdrs (carriage-hub--parse-headers (cdr lines)))
               (tr (carriage-hub--parse-start-line start)))
          (if (not tr)
              (progn
                (carriage-hub--send-json proc "400 Bad Request" (list :ok :false :error "bad request" :code "WEB_E_PAYLOAD"))
                (erase-buffer))
            (pcase-let ((`(,method ,path) tr))
              (let ((clen (let ((v (assoc-default "content-length" hdrs)))
                            (if v (string-to-number v) 0))))
                (cond
                 ;; Early reject oversized bodies based on Content-Length (avoid buffering spikes).
                 ((and (integerp carriage-hub-max-request-bytes)
                       (> clen carriage-hub-max-request-bytes))
                  (carriage-hub--send-json proc "413 Payload Too Large"
                                           (list :ok :false :error "payload too large" :code "WEB_E_PAYLOAD"))
                  (erase-buffer))
                 ;; Wait for more bytes when body not complete yet.
                 ((> clen (string-bytes rest))
                  nil)
                 (t
                  (carriage-hub--dispatch proc method path hdrs rest)
                  (erase-buffer)))))))))))

(defun carriage-hub-start ()
  "Start the Hub server."
  (interactive)
  (when (and carriage-hub--server (process-live-p carriage-hub--server))
    (user-error "carriage-hub already running"))
  (setq carriage-hub--server
        (make-network-process
         :name "carriage-hub"
         :server t
         :host carriage-hub-bind
         :service carriage-hub-port
         :noquery t
         :family 'ipv4
         :coding 'binary
         :filter #'carriage-hub--process-filter
         :sentinel #'carriage-hub--sentinel))
  ;; Background probes update registry liveness; keeps request handlers O(1).
  (when (timerp carriage-hub--probe-timer)
    (cancel-timer carriage-hub--probe-timer))
  (setq carriage-hub--probe-timer
        (run-at-time
         (max 1.0 (or carriage-hub-probe-interval 5.0))
         (max 1.0 (or carriage-hub-probe-interval 5.0))
         (lambda () (ignore-errors (carriage-hub--probe-tick)))))
  carriage-hub--server)

(defun carriage-hub-stop ()
  "Stop the Hub server."
  (interactive)
  (when (timerp carriage-hub--probe-timer)
    (cancel-timer carriage-hub--probe-timer))
  (setq carriage-hub--probe-timer nil)
  (when (and carriage-hub--server (process-live-p carriage-hub--server))
    (ignore-errors (delete-process carriage-hub--server)))
  (setq carriage-hub--server nil)
  t)

(defun carriage-hub-main (&rest _plist)
  "Hub entrypoint for --eval."
  (carriage-hub-start)
  (princ (format "hub: started bind=%s port=%s\n" carriage-hub-bind carriage-hub-port))
  (while t (sleep-for 3600)))

(provide 'carriage-hub)
;;; carriage-hub.el ends here
