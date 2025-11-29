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

(defcustom carriage-web-api-commands-enabled nil
  "When non-nil, enable handling of whitelisted /api/cmd operations.
Default is nil to keep test suite expectations (WEB_E_CMD for most ops)."
  :type 'boolean :group 'carriage-web)

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

;; Lightweight metrics (best-effort)
(defvar carriage-web--metrics-published 0
  "Total number of SSE event deliveries (best-effort).")
(defvar carriage-web--metrics-truncated 0
  "Total number of JSON truncations due to carriage-web-max-json-bytes.")
(defvar carriage-web--metrics-clients 0
  "Current number of connected SSE clients (best-effort).")

;; Simple logger
(defun carriage-web--log (fmt &rest args)
  (let ((msg (apply #'format fmt args)))
    (ignore-errors (message "web: %s" msg))))

;; Maintenance: remove accidental/leftover debug advices on the process filter.
(defun carriage-web-clear-debug-advices ()
  "Remove known carriage-web debug advices from `carriage-web--process-filter'.
Returns the number of removed advices."
  (interactive)
  (let ((removed 0))
    (condition-case _e
        (progn
          (advice-mapc
           (lambda (a _p)
             (when (and (symbolp a)
                        (string-match-p "\\`carriage-web--adv-" (symbol-name a)))
               (ignore-errors
                 (advice-remove 'carriage-web--process-filter a)
                 (setq removed (1+ removed)))))
           'carriage-web--process-filter)
          (carriage-web--log "clear-adv: removed=%d" removed)
          removed)
      (error
       (carriage-web--log "clear-adv: error while removing advices")
       removed))))

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
  "Return truncated JSON string for OBJ and bump truncation metric."
  (let* ((s (json-encode obj))
         (b (string-bytes s)))
    (if (> b carriage-web-max-json-bytes)
        (progn
          (cl-incf carriage-web--metrics-truncated)
          (substring s 0 carriage-web-max-json-bytes))
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
   "<meta name='viewport' content='width=device-width, initial-scale=1'/>"
   "<title>Carriage — Dashboard</title>"
   "<style>"
   "html,body{margin:0;padding:0;background:#0f1117;color:#e6e6e6;font-family:system-ui,Segoe UI,Roboto,Helvetica,Arial,sans-serif}"
   "header{padding:.8rem 1rem;border-bottom:1px solid #222;display:flex;gap:1rem;align-items:center}"
   "header .title{font-weight:600;letter-spacing:.2px}"
   "header .right{margin-left:auto;display:flex;gap:.5rem;align-items:center;font-size:.9rem}"
   ".wrap{display:grid;grid-template-columns:320px 1fr;min-height:calc(100vh - 56px)}"
   ".side{border-right:1px solid #222;min-height:0;display:flex;flex-direction:column}"
   ".side .row{padding:.5rem 1rem;border-bottom:1px solid #171923;cursor:pointer;display:flex;gap:.5rem;align-items:center}"
   ".side .row:hover{background:#151823}"
   ".side .row.active{background:#1a1f2e}"
   ".main{padding:1rem;min-width:0}"
   ".muted{color:#9aa4b2}"
   ".badge{display:inline-block;padding:0 .4rem;border-radius:.4rem;background:#222;margin-left:.3rem;font-size:.85rem}"
   ".btn{background:#1f6feb;color:#fff;border:none;padding:.4rem .6rem;border-radius:.35rem;cursor:pointer}"
   ".btn:disabled{opacity:.6;cursor:not-allowed}"
   ".btn.alt{background:#30363d}"
   ".grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:.6rem;margin:.6rem 0}"
   ".card{background:#0f1320;border:1px solid #1b2133;border-radius:.5rem;padding:.7rem}"
   "input[type=text]{background:#0b0e18;border:1px solid #1b2133;color:#e6e6e6;border-radius:.35rem;padding:.3rem .5rem}"
   "code,pre{font-family:ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace}"
   "a{color:#8ab4f8;text-decoration:none}"
   "</style>"
   "</head><body>"
   "<header>"
   "<div class='title'>Carriage — Local Dashboard</div>"
   "<div class='right'>"
   "<span class='muted'>Token:</span>"
   "<input id='tok' type='text' placeholder='X-Auth (optional)' size='18'/>"
   "<button class='btn alt' id='saveTok'>Save</button>"
   "<button class='btn alt' id='clearTok'>Clear</button>"
   "<span class='muted' id='status' title='stream status'>offline</span>"
   "</div>"
   "</header>"
   "<div class='wrap'>"
   "<div class='side'>"
   "<div class='row muted' style='font-weight:600;cursor:default;'>Sessions</div>"
   "<div id='list' style='overflow:auto;flex:1 1 auto'></div>"
   "</div>"
   "<div class='main'>"
   "<div class='grid'>"
   "<div class='card'>"
   "<div style='font-weight:600'>Selected</div>"
   "<div id='selId' class='muted'>—</div>"
   "</div>"
   "<div class='card'>"
   "<div style='font-weight:600'>State</div>"
   "<div><span id='statePhase'>idle</span><span id='stateTip' class='badge muted'></span></div>"
   "</div>"
   "<div class='card'>"
   "<div style='font-weight:600'>Context</div>"
   "<div>Files: <span id='ctxCount'>0</span>, Patches: <span id='patchCount'>0</span></div>"
   "</div>"
   "<div class='card'>"
   "<div style='font-weight:600'>Engine</div>"
   "<div>Suite: <span id='suite'>-</span> · Engine: <span id='engine'>-</span></div>"
   "</div>"
   "</div>"
   "<div class='card'>"
   "<div style='font-weight:600;margin-bottom:.4rem'>Actions</div>"
   "<div style='display:flex;gap:.4rem;flex-wrap:wrap'>"
   "<button class='btn' id='applyLast' title='Apply last iteration'>Apply</button>"
   "<button class='btn alt' id='abort' title='Abort current'>Abort</button>"
   "<button class='btn alt' id='report' title='Open report'>Report</button>"
   "<button class='btn alt' id='commitLast' title='Commit last'>Commit Last</button>"
   "<button class='btn alt' id='commitAll' title='Commit all'>Commit All</button>"
   "<button class='btn alt' id='wip' title='Create WIP branch'>WIP</button>"
   "<span class='muted' style='margin-left:auto'>Toggles:</span>"
   "<button class='btn alt' id='tgCtx' title='Toggle GPT ctx'>Ctx</button>"
   "<button class='btn alt' id='tgFiles' title='Toggle doc ctx'>Files</button>"
   "<button class='btn alt' id='tgVisible' title='Toggle visible ctx'>Visible</button>"
   "<button class='btn alt' id='tgPatched' title='Toggle patched files'>Patched</button>"
   "<button class='btn alt' id='tgProfile' title='Toggle profile P1/P3'>Profile</button>"
   "</div>"
   "</div>"
   "<div class='card' id='lastRep'>"
   "<div style='font-weight:600;margin-bottom:.4rem'>Last report summary</div>"
   "<div id='repSummary' class='muted'>—</div>"
   "</div>"
   "<div style='margin-top:.6rem' class='muted'>"
   "Tip: save token to use auth‑gated APIs; SSE accepts ?token= as well."
   "</div>"
   "</div>"
   "</div>"
   "<script>(function(){"
   "'use strict';"
   "const S = { doc:null, token:'', es:null, list:[], byId:{} };"
   "const $ = sel => document.querySelector(sel);"
   "function setStatus(t){ $('#status').textContent = t; }"
   "function headers(){ const h={}; if(S.token) h['X-Auth']=S.token; return h; }"
   "function api(path,opt={}){ opt.headers=Object.assign({},opt.headers||{},headers()); return fetch(path,opt); }"
   "function json(r){ return r.ok ? r.json() : r.text().then(t=>{throw new Error(t||('HTTP '+r.status))}); }"
   "function el(tag,cls,text){ const e=document.createElement(tag); if(cls) e.className=cls; if(text!=null) e.textContent=text; return e; }"
   "function choose(doc){ if(!doc) return; S.doc=doc; $('#selId').textContent = doc; highlight(doc); loadSession(doc); connect(); }"
   "function highlight(doc){ const rows=document.querySelectorAll('.side .row.item'); rows.forEach(r=>{ if(r.dataset.doc===doc) r.classList.add('active'); else r.classList.remove('active'); }); }"
   "function renderList(items){ const box=$('#list'); box.innerHTML=''; S.list=items||[]; S.byId={};"
   " S.list.forEach(it=>{ S.byId[it.id]=it; const r=el('div','row item'); r.dataset.doc=it.id;"
   "  const t=el('div',null,it.title||it.id); const meta=el('div','muted'); meta.style.marginLeft='auto';"
   "  const ph=(it.state&&it.state.phase)||'idle'; const ctx=(it.ctx&&it.ctx.count)||0; const pc=(it.patches&&it.patches.count)||0;"
   "  meta.textContent = ph + ' · Ctx:' + ctx + ' · P:' + pc; r.appendChild(t); r.appendChild(meta);"
   "  r.onclick=()=>choose(it.id); box.appendChild(r); });"
   " if(S.list.length && !S.doc) choose(S.list[0].id); }"
   "function updateState(st){ if(!st) return; $('#statePhase').textContent=st.phase||'idle'; $('#stateTip').textContent=(st.tooltip||''); }"
   "function updateSessionCard(x){ if(!x) return;"
   " $('#suite').textContent = x.suite||'-'; $('#engine').textContent = x.engine||'-';"
   " $('#ctxCount').textContent = (x.ctx&&x.ctx.count)||0; $('#patchCount').textContent = (x.patches&&x.patches.count)||0;"
   " updateState(x.state||{});"
   " const ap = (x.apply&&x.apply.last)||null;"
   " $('#repSummary').textContent = ap?('ok:'+ (ap.ok||0)+' fail:'+(ap.fail||0)+' skipped:'+(ap.skipped||0)+' total:'+(ap.total||0)):'—'; }"
   "function refreshList(){ api('/api/sessions').then(json).then(o=>{ if(o&&o.ok) renderList(o.data||[]); }).catch(()=>{}); }"
   "function loadSession(doc){ api('/api/session/'+encodeURIComponent(doc)).then(json).then(o=>{ if(o&&o.ok) updateSessionCard(o.data); }).catch(()=>{}); }"
   "function connect(){ try{ if(S.es){ try{S.es.close();}catch(e){} S.es=null; }"
   " const params = []; if (S.doc) params.push('doc='+encodeURIComponent(S.doc)); if (S.token) params.push('token='+encodeURIComponent(S.token));"
   " const url = '/stream' + (params.length?('?'+params.join('&')):'');"
   " const es = new EventSource(url); S.es=es; setStatus('connecting');"
   " es.addEventListener('open', ()=>setStatus('online'));"
   " es.addEventListener('error', ()=>setStatus('error'));"
   " function onAny(ev){ try{ const d=JSON.parse(ev.data||'{}'); const doc=d.doc; const tp=d.type||ev.type;"
   "   if(tp==='ui' && S.doc===doc){ updateState(d.state||{}); }"
   "   if((tp==='apply'||tp==='report') && S.doc===doc){ const ap=d.summary||null; if(ap){"
   "       $('#repSummary').textContent = 'ok:'+(ap.ok||0)+' fail:'+(ap.fail||0)+' skipped:'+(ap.skipped||0)+' total:'+(ap.total||0); } }"
   " }catch(e){} }"
   " ;['hello','ui','apply','report','transport','heartbeat','message'].forEach(t=>es.addEventListener(t,onAny));"
   " } catch(e){ setStatus('error'); } }"
   "function cmd(name, payload){ if(!S.doc && (!payload||!payload.doc)) return;"
   " const body = Object.assign({cmd:name, doc:S.doc}, payload||{});"
   " return api('/api/cmd',{method:'POST',headers:Object.assign({'Content-Type':'application/json'},headers()),body:JSON.stringify(body)}).then(json); }"
   "function bind(){"
   " $('#saveTok').onclick=()=>{ const v=$('#tok').value.trim(); S.token=v; try{localStorage.setItem('cw_token',v);}catch(e){}; connect(); refreshList(); };"
   " $('#clearTok').onclick=()=>{ S.token=''; $('#tok').value=''; try{localStorage.removeItem('cw_token');}catch(e){}; connect(); refreshList(); };"
   " $('#applyLast').onclick=()=>cmd('apply_last_iteration').catch(()=>{});"
   " $('#abort').onclick=()=>cmd('abort').catch(()=>{});"
   " $('#report').onclick=()=>cmd('report_open').catch(()=>{});"
   " $('#commitLast').onclick=()=>cmd('commit_last').catch(()=>{});"
   " $('#commitAll').onclick=()=>cmd('commit_all').catch(()=>{});"
   " $('#wip').onclick=()=>cmd('wip').catch(()=>{});"
   " $('#tgCtx').onclick=()=>cmd('toggle',{key:'ctx'}).catch(()=>{});"
   " $('#tgFiles').onclick=()=>cmd('toggle',{key:'files'}).catch(()=>{});"
   " $('#tgVisible').onclick=()=>cmd('toggle',{key:'visible'}).catch(()=>{});"
   " $('#tgPatched').onclick=()=>cmd('toggle',{key:'patched'}).catch(()=>{});"
   " $('#tgProfile').onclick=()=>cmd('toggle',{key:'profile'}).catch(()=>{});"
   " const saved = (function(){ try{return localStorage.getItem('cw_token')||'';}catch(e){return '';} })();"
   " S.token = saved; $('#tok').value = saved; }"
   "function init(){ bind(); refreshList(); connect(); setInterval(refreshList, 5000); }"
   "document.addEventListener('DOMContentLoaded', init);"
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

;; Event bus (debounced) and lightweight last-report cache

(defcustom carriage-web-publish-debounce 0.07
  "Debounce (seconds) before flushing queued SSE events."
  :type 'number :group 'carriage-web)

(defvar carriage-web--event-queue nil)
(defvar carriage-web--pub-timer nil)
(defvar carriage-web--last-report-by-doc (make-hash-table :test 'equal))
(defvar carriage-web--last-report-full-by-doc (make-hash-table :test 'equal))

(defun carriage-web--queue-event (ev)
  "Enqueue EV for later SSE broadcast."
  (setq carriage-web--event-queue (nconc carriage-web--event-queue (list ev))))

(defun carriage-web--flush-events ()
  "Flush queued events to SSE clients with doc filter support."
  (setq carriage-web--pub-timer nil)
  (when carriage-web--event-queue
    (let ((batch carriage-web--event-queue))
      (setq carriage-web--event-queue nil)
      (dolist (ev batch)
        (let* ((etype (or (plist-get ev :type) "event"))
               (doc   (plist-get ev :doc))
               (now   (float-time)))
          (dolist (cli carriage-web--clients)
            (let ((proc (plist-get cli :proc))
                  (flt  (plist-get cli :filter)))
              (when (process-live-p proc)
                ;; Filter by doc when client requested it
                (when (or (null flt) (null doc) (string= flt doc))
                  (setf (plist-get cli :last-ts) now)
                  (carriage-web--sse-send proc etype ev)
                  (cl-incf carriage-web--metrics-published))))))))))

(defun carriage-web-pub (type payload)
  "Publish SSE event with TYPE and PAYLOAD plist.
Adds :type TYPE to payload and debounces delivery."
  (let ((ev (copy-sequence payload)))
    (setq ev (plist-put ev :type (if (stringp type) type (format "%s" type))))
    (carriage-web--queue-event ev)
    (unless carriage-web--pub-timer
      (setq carriage-web--pub-timer
            (run-at-time (or carriage-web-publish-debounce 0.07)
                         nil #'carriage-web--flush-events)))))

(defun carriage-web--publish-ui ()
  "Publish current buffer UI state to SSE clients."
  (let* ((doc (ignore-errors (carriage-web--buffer-id (current-buffer))))
         (phase (let* ((st (and (boundp 'carriage--ui-state) carriage--ui-state)))
                  (if (symbolp st) (format "%s" st) (or (and st (format "%s" st)) "idle"))))
         (tooltip (and (boundp 'carriage--ui-state-tooltip) carriage--ui-state-tooltip)))
    (when doc
      (carriage-web-pub "ui" (list :doc doc :state (list :phase phase :tooltip tooltip))))))

;; Install best-effort advices to publish events (UI/transport/report)

(with-eval-after-load 'carriage-ui
  (when (fboundp 'carriage-ui-set-state)
    (advice-add 'carriage-ui-set-state :after
                (lambda (&rest _)
                  (ignore-errors (carriage-web--publish-ui)))))
  (when (fboundp 'carriage-ui-note-apply-summary)
    (advice-add 'carriage-ui-note-apply-summary :after
                (lambda (pl &rest _)
                  (let ((doc (ignore-errors (carriage-web--buffer-id (current-buffer)))))
                    (when doc
                      (carriage-web-pub "apply" (list :doc doc :summary pl))))))))

(with-eval-after-load 'carriage-transport
  (when (fboundp 'carriage-transport-begin)
    (advice-add 'carriage-transport-begin :after
                (lambda (&rest _)
                  (let ((doc (ignore-errors (carriage-web--buffer-id (current-buffer)))))
                    (when doc
                      (carriage-web-pub "transport" (list :doc doc :phase "begin")))))))
  (when (fboundp 'carriage-transport-streaming)
    (advice-add 'carriage-transport-streaming :after
                (lambda (&rest _)
                  (let ((doc (ignore-errors (carriage-web--buffer-id (current-buffer)))))
                    (when doc
                      (carriage-web-pub "transport" (list :doc doc :phase "streaming")))))))
  (when (fboundp 'carriage-transport-complete)
    (advice-add 'carriage-transport-complete :after
                (lambda (&rest _)
                  (let ((doc (ignore-errors (carriage-web--buffer-id (current-buffer)))))
                    (when doc
                      (carriage-web-pub "transport" (list :doc doc :phase "complete"))))))))

(with-eval-after-load 'carriage-report
  (when (fboundp 'carriage-report-open)
    (advice-add 'carriage-report-open :after
                (lambda (report &rest _)
                  (let* ((doc (ignore-errors (carriage-web--buffer-id (current-buffer))))
                         (sum (and (listp report) (plist-get report :summary))))
                    (when (and doc sum)
                      ;; Cache last report (full) and a compact summary by doc
                      (puthash doc report carriage-web--last-report-full-by-doc)
                      (puthash doc sum carriage-web--last-report-by-doc)
                      (carriage-web-pub "report" (list :doc doc :summary sum))))))))

;; Authorization

(defun carriage-web--auth-ok (headers query &optional allow-query-token)
  (if (not carriage-web-auth-token)
      t
    (let ((hdr (assoc-default "x-auth" headers))
          (qtok (and allow-query-token
                     (assoc-default "token" query))))
      (and (stringp (or hdr qtok))
           (string= carriage-web-auth-token (or hdr qtok))))))

;; -------- Snapshot helpers (refactor of large handlers) --------

(defun carriage-web--count-patches-in-buffer (&optional buffer)
  "Count occurrences of #+begin_patch in BUFFER (case-insensitive)."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search t) (n 0))
          (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
            (setq n (1+ n)))
          n)))))

(defun carriage-web--buffer-ui-state (&optional buffer)
  "Return plist (:phase :tooltip) for BUFFER UI state."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((st (and (boundp 'carriage--ui-state) carriage--ui-state))
           (phase (if (symbolp st) (format "%s" st) (or (and st (format "%s" st)) "idle")))
           (tooltip (and (boundp 'carriage--ui-state-tooltip) carriage--ui-state-tooltip)))
      (list :phase phase :tooltip tooltip))))

(defun carriage-web--buffer-engine-string (&optional buffer)
  "Return engine string for BUFFER based on carriage-apply-engine and policy."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((eng (and (boundp 'carriage-apply-engine) carriage-apply-engine)))
      (cond
       ((eq eng 'git)
        (let ((pol (and (boundp 'carriage-git-branch-policy) carriage-git-branch-policy)))
          (format "git:%s" (if pol (format "%s" pol) ""))))
       ((symbolp eng) (format "%s" eng))
       ((stringp eng) eng)
       (t "emacs")))))

(defun carriage-web--gather-context-info (&optional buffer)
  "Return plist (:count N :sources list-or-nil) describing BUFFER context."
  (with-current-buffer (or buffer (current-buffer))
    (let ((count 0) (sources nil))
      (condition-case _e
          (progn
            (require 'carriage-context nil t)
            (when (fboundp 'carriage-context-count)
              (let* ((res (carriage-context-count (current-buffer) nil)))
                (setq count (or (and (listp res) (plist-get res :count)) 0))
                (setq sources (and (listp res) (plist-get res :sources))))))
        (error nil))
      (list :count count :sources sources))))

(defun carriage-web--buffer-session-snapshot (buffer)
  "Build lightweight snapshot plist for BUFFER for /api/sessions."
  (with-current-buffer buffer
    (let* ((id (ignore-errors (carriage-web--buffer-id buffer))))
      (when id
        (let* ((title (or (and buffer-file-name (file-name-nondirectory buffer-file-name))
                          (buffer-name buffer)))
               (root  (or (and (fboundp 'carriage-project-root)
                               (ignore-errors (carriage-project-root)))
                          default-directory))
               (project (ignore-errors
                          (file-name-nondirectory (directory-file-name (or root default-directory)))))
               (mode  (format "%s" major-mode))
               (intent (cond
                        ((and (boundp 'carriage-mode-intent) carriage-mode-intent)
                         (format "%s" carriage-mode-intent))
                        (t "Ask")))
               (suite  (cond
                        ((and (boundp 'carriage-mode-suite) carriage-mode-suite)
                         (format "%s" carriage-mode-suite))
                        (t "udiff")))
               (engine (carriage-web--buffer-engine-string buffer))
               (branch nil)
               (ctx (carriage-web--gather-context-info buffer))
               (patches-count (carriage-web--count-patches-in-buffer buffer))
               (state (carriage-web--buffer-ui-state buffer)))
          (list :id id
                :title title
                :project project
                :mode mode
                :intent intent
                :suite suite
                :engine engine
                :branch branch
                :ctx (list :count (plist-get ctx :count))
                :patches (list :count patches-count)
                :state state))))))

(defun carriage-web--find-buffer-by-doc (doc)
  "Find a live buffer whose carriage-web buffer id equals DOC."
  (cl-find-if
   (lambda (b)
     (and (buffer-live-p b)
          (string= (ignore-errors (carriage-web--buffer-id b)) doc)))
   (buffer-list)))

(defun carriage-web--buffer-session-detail (buffer)
  "Build detailed snapshot plist for BUFFER for /api/session/:id."
  (with-current-buffer buffer
    (let* ((id (ignore-errors (carriage-web--buffer-id buffer)))
           (title (or (and buffer-file-name (file-name-nondirectory buffer-file-name))
                      (buffer-name buffer)))
           (root  (or (and (fboundp 'carriage-project-root)
                           (ignore-errors (carriage-project-root)))
                      default-directory))
           (project (ignore-errors
                      (file-name-nondirectory (directory-file-name (or root default-directory)))))
           (mode  (format "%s" major-mode))
           (intent (cond
                    ((and (boundp 'carriage-mode-intent) carriage-mode-intent)
                     (format "%s" carriage-mode-intent))
                    (t "Ask")))
           (suite  (cond
                    ((and (boundp 'carriage-mode-suite) carriage-mode-suite)
                     (format "%s" carriage-mode-suite))
                    (t "udiff")))
           (engine (carriage-web--buffer-engine-string buffer))
           (branch nil)
           (ctx (carriage-web--gather-context-info buffer))
           (patches-count (carriage-web--count-patches-in-buffer buffer))
           (state (carriage-web--buffer-ui-state buffer))
           (last-summary (and (hash-table-p carriage-web--last-report-by-doc)
                              (gethash id carriage-web--last-report-by-doc))))
      (list
       :id id
       :title title
       :project project
       :mode mode
       :intent intent
       :suite suite
       :engine engine
       :branch branch
       :ctx (list :count (or (plist-get ctx :count) 0)
                  :sources (plist-get ctx :sources))
       :patches (list :count patches-count)
       :state state
       :apply (and last-summary (list :last last-summary))))))

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

(defun carriage-web--handle-metrics (proc _req)
  "Return lightweight counters for observability."
  (let ((published (or carriage-web--metrics-published 0))
        (truncated (or carriage-web--metrics-truncated 0))
        (clients   (condition-case _e
                       (length carriage-web--clients)
                     (error 0))))
    (carriage-web--send-json
     proc "200 OK"
     (list :ok t
           :data (list
                  :published published
                  :truncated truncated
                  :clients clients
                  :ts (float-time))))))

(defun carriage-web--handle-sessions (proc _req)
  "Collect lightweight session snapshots for visible Carriage-related buffers."
  (let* ((res (cl-loop for b in (buffer-list)
                       when (buffer-live-p b)
                       for snap = (ignore-errors (carriage-web--buffer-session-snapshot b))
                       when snap collect snap))
         (data (cl-remove-if-not #'identity res)))
    (carriage-web--send-json proc "200 OK" (list :ok t :data data))))

(defun carriage-web--handle-session (proc req)
  "Return a detailed snapshot for a single session by its doc id."
  (let* ((path (plist-get req :path))
         (id (substring path (length "/api/session/")))
         (buf (carriage-web--find-buffer-by-doc id)))
    (if (not (buffer-live-p buf))
        (carriage-web--send-json proc "404 Not Found"
                                 (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))
      (let ((payload (carriage-web--buffer-session-detail buf)))
        (carriage-web--send-json proc "200 OK" (list :ok t :data payload))))))

(defun carriage-web--handle-report-last (proc req)
  "Return last report summary/items for ?doc= with optional ?limit=N."
  (let* ((q (plist-get req :query))
         (doc (and (listp q) (assoc-default "doc" q)))
         (limit-str (and (listp q) (assoc-default "limit" q)))
         (lim (and (stringp limit-str) (string-to-number limit-str))))
    (if (not (and (stringp doc) (> (length doc) 0)))
        (carriage-web--send-json proc "400 Bad Request"
                                 (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD"))
      (let ((rep (and (hash-table-p carriage-web--last-report-full-by-doc)
                      (gethash doc carriage-web--last-report-full-by-doc))))
        (if (not rep)
            (carriage-web--send-json proc "404 Not Found"
                                     (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))
          (let* ((summary (plist-get rep :summary))
                 (items   (or (plist-get rep :items) '()))
                 (count   (length items))
                 (nitems  (if (and (numberp lim) (> lim 0))
                              (cl-subseq items 0 (min lim count))
                            items))
                 (payload (list :summary summary :items nitems)))
            (carriage-web--send-json proc "200 OK" (list :ok t :data payload))))))))

(defun carriage-web--handle-cmd (proc req)
  "Dispatch POST /api/cmd with strict whitelist semantics.
Accepts JSON body: {\"cmd\":\"...\",\"doc\":\"...\",...} and returns a structured envelope."
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
     ;; Bad payload: missing or non-string cmd
     ((not (and (stringp cmd) (> (length cmd) 0)))
      (carriage-web--send-json proc "400 Bad Request"
                               (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD")))

     ;; Toggle commands (no eval; guarded by fboundp)
     ((string= cmd "toggle")
      (if (not (and (stringp key) (stringp doc) (> (length doc) 0)))
          (carriage-web--send-json proc "400 Bad Request"
                                   (list :ok json-false :error "bad request" :code "WEB_E_PAYLOAD"))
        (let* ((buf (carriage-web--find-buffer-by-doc doc))
               (ok nil))
          (if (not (buffer-live-p buf))
              (carriage-web--send-json proc "404 Not Found"
                                       (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))
            (with-current-buffer buf
              (setq ok
                    (pcase key
                      ("ctx"
                       (when (fboundp 'carriage-toggle-include-gptel-context)
                         (carriage-toggle-include-gptel-context) t))
                      ("files"
                       (when (fboundp 'carriage-toggle-include-doc-context)
                         (carriage-toggle-include-doc-context) t))
                      ("visible"
                       (when (fboundp 'carriage-toggle-include-visible-context)
                         (carriage-toggle-include-visible-context) t))
                      ("patched"
                       (when (fboundp 'carriage-toggle-include-patched-files)
                         (carriage-toggle-include-patched-files) t))
                      ("profile"
                       (let ((val (and (hash-table-p obj) (gethash "value" obj))))
                         (cond
                          ((and (stringp val) (fboundp 'carriage-context-profile-set))
                           (carriage-context-profile-set (if (string= (downcase val) "p3") 'p3 'p1))
                           t)
                          ((fboundp 'carriage-toggle-context-profile)
                           (carriage-toggle-context-profile) t)
                          (t nil))))
                      (_ nil))))
            (if ok
                (carriage-web--send-json proc "200 OK" (list :ok t))
              (carriage-web--send-json proc "400 Bad Request"
                                       (list :ok json-false :error "unsupported" :code "WEB_E_CMD")))))))

     ;; Whitelisted action commands
     ((or (string= cmd "apply_last_iteration")
          (string= cmd "abort")
          (string= cmd "report_open")
          (string= cmd "commit_last")
          (string= cmd "commit_all")
          (string= cmd "wip"))
      (if (not carriage-web-api-commands-enabled)
          (carriage-web--send-json proc "400 Bad Request"
                                   (list :ok json-false :error "unsupported" :code "WEB_E_CMD"))
        (let* ((buf (carriage-web--find-buffer-by-doc doc)))
          (if (not (buffer-live-p buf))
              (carriage-web--send-json proc "404 Not Found"
                                       (list :ok json-false :error "not found" :code "WEB_E_NOT_FOUND"))
            (let ((ok nil))
              (with-current-buffer buf
                (pcase cmd
                  ("apply_last_iteration"
                   (setq ok (and (fboundp 'carriage-apply-last-iteration)
                                 (progn (run-at-time 0 nil #'carriage-apply-last-iteration) t))))
                  ("abort"
                   (setq ok (and (fboundp 'carriage-abort-current)
                                 (progn (run-at-time 0 nil #'carriage-abort-current) t))))
                  ("report_open"
                   (setq ok (and (fboundp 'carriage-report-open)
                                 (progn (run-at-time 0 nil #'carriage-report-open) t))))
                  ("commit_last"
                   (setq ok (and (fboundp 'carriage-commit-last-iteration)
                                 (progn (run-at-time 0 nil #'carriage-commit-last-iteration) t))))
                  ("commit_all"
                   (setq ok (and (fboundp 'carriage-commit-all)
                                 (progn (run-at-time 0 nil #'carriage-commit-all) t))))
                  ("wip"
                   (setq ok (and (fboundp 'carriage-wip)
                                 (progn (run-at-time 0 nil #'carriage-wip) t))))
                  (_ (setq ok nil))))
              (if ok
                  (carriage-web--send-json proc "200 OK" (list :ok t))
                (carriage-web--send-json proc "400 Bad Request"
                                         (list :ok json-false :error "unsupported" :code "WEB_E_CMD"))))))))

     ;; Unknown command
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
        (push cli carriage-web--clients)
        (setq carriage-web--metrics-clients (length carriage-web--clients)))
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

   ((and (string= method "GET") (string= path "/api/metrics"))
    (if (carriage-web--auth-ok headers query nil)
        (carriage-web--handle-metrics proc nil)
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

   ;; Favicon — return 204 No Content to avoid noisy 404s
   ((and (string= method "GET") (string= path "/favicon.ico"))
    (carriage-web--send-response proc "204 No Content" "image/x-icon" ""))

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
