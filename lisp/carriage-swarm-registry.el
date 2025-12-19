;;; carriage-swarm-registry.el --- Swarm registry (agents inventory)  -*- lexical-binding: t; -*-

;; Forward-only Swarm registry for Carriage:
;; - Base dir: $XDG_RUNTIME_DIR/carriage-swarm/ or /tmp/carriage-swarm-$USER/
;; - registry.ndjson: atomically rewritten catalog (one JSON object per line)
;; - agents/<id>/{pid,port,token,meta.json}: per-agent runtime files (token is 0600)
;;
;; Specifications:
;;   spec/registry-v1.org
;;   spec/swarm-v1.org
;;   spec/security-v2.org
;;   spec/errors-v2.org

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup carriage-swarm-registry nil
  "Registry for Carriage Swarm agents."
  :group 'carriage)

(defcustom carriage-swarm-registry-runtime-dir nil
  "Override runtime dir for the swarm registry (directory).
When nil, resolved from XDG_RUNTIME_DIR or /tmp fallback."
  :type '(choice (const :tag "Auto" nil) directory)
  :group 'carriage-swarm-registry)

(defcustom carriage-swarm-registry-file "registry.ndjson"
  "File name for the registry NDJSON catalog, relative to runtime dir."
  :type 'string
  :group 'carriage-swarm-registry)

(defcustom carriage-swarm-registry-bind "127.0.0.1"
  "Default bind address recorded for Agents."
  :type 'string
  :group 'carriage-swarm-registry)

(defun carriage-swarm-registry--user ()
  (or (getenv "USER") (ignore-errors (user-login-name)) "user"))

(defun carriage-swarm-registry--runtime-dir ()
  "Return runtime dir (absolute, with trailing slash)."
  (file-name-as-directory
   (or carriage-swarm-registry-runtime-dir
       (let* ((xdg (getenv "XDG_RUNTIME_DIR"))
              (cand (and xdg (expand-file-name "carriage-swarm" xdg))))
         (cond
          ((and cand (file-directory-p xdg)) cand)
          (t (expand-file-name (format "carriage-swarm-%s" (carriage-swarm-registry--user)) "/tmp")))))))

(defun carriage-swarm-registry--ensure-dir (dir)
  (unless (file-directory-p dir)
    (make-directory dir t))
  dir)

(defun carriage-swarm-registry-base-dir ()
  "Ensure and return the base registry directory."
  (carriage-swarm-registry--ensure-dir (carriage-swarm-registry--runtime-dir)))

(defun carriage-swarm-registry-file ()
  "Return absolute registry.ndjson path."
  (expand-file-name carriage-swarm-registry-file (carriage-swarm-registry-base-dir)))

(defun carriage-swarm-registry-agents-dir ()
  "Return absolute agents/ directory path (ensure it exists)."
  (carriage-swarm-registry--ensure-dir
   (expand-file-name "agents" (carriage-swarm-registry-base-dir))))

(defun carriage-swarm-registry-agent-dir (id)
  "Return absolute per-agent runtime dir for ID (ensure it exists)."
  (unless (and (stringp id) (not (string-empty-p id)))
    (error "carriage-swarm-registry: invalid id"))
  (carriage-swarm-registry--ensure-dir
   (expand-file-name id (carriage-swarm-registry-agents-dir))))

(defun carriage-swarm-registry--rand-hex (nbytes)
  (let ((alphabet "0123456789abcdef")
        (acc (make-string (* 2 nbytes) ?0)))
    (dotimes (i (* 2 nbytes))
      (aset acc i (aref alphabet (random (length alphabet)))))
    acc))

(defun carriage-swarm-registry-generate-id ()
  "Generate a reasonably unique agent id (hex)."
  (format "agent-%s-%s"
          (format-time-string "%Y%m%d%H%M%S" (current-time) t)
          (carriage-swarm-registry--rand-hex 6)))

(defun carriage-swarm-registry--random-token ()
  (let ((alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (len 40))
    (apply #'concat
           (cl-loop repeat len
                    collect (string (aref alphabet (random (length alphabet))))))))

(defun carriage-swarm-registry-generate-token ()
  "Generate a new random token for an Agent (opaque string)."
  (carriage-swarm-registry--random-token))

(defun carriage-swarm-registry--write-file (file content &optional mode)
  (carriage-swarm-registry--ensure-dir (file-name-directory file))
  (with-temp-file file
    (insert (or content "")))
  (when (integerp mode)
    (ignore-errors (set-file-modes file mode)))
  file)

(defun carriage-swarm-registry--read-file (file)
  (when (file-readable-p file)
    (string-trim
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun carriage-swarm-registry-agent-pid-file (id)
  (expand-file-name "pid" (carriage-swarm-registry-agent-dir id)))

(defun carriage-swarm-registry-agent-port-file (id)
  (expand-file-name "port" (carriage-swarm-registry-agent-dir id)))

(defun carriage-swarm-registry-agent-token-file (id)
  (expand-file-name "token" (carriage-swarm-registry-agent-dir id)))

(defun carriage-swarm-registry-agent-meta-file (id)
  (expand-file-name "meta.json" (carriage-swarm-registry-agent-dir id)))

(defun carriage-swarm-registry--json-parse (s)
  (when (and (stringp s) (not (string-empty-p (string-trim s))))
    (condition-case _e
        (if (fboundp 'json-parse-string)
            (json-parse-string s :object-type 'alist :array-type 'list :null-object nil :false-object :false)
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-false :false))
            (json-read-from-string s)))
      (error nil))))

(defun carriage-swarm-registry--json-encode (obj)
  (condition-case _e
      (if (fboundp 'json-serialize)
          (json-serialize obj)
        (json-encode obj))
    (error "{}")))

(defun carriage-swarm-registry--atomic-write (file contents)
  "Atomically write CONTENTS to FILE (temp + rename)."
  (let* ((dir (file-name-directory file))
         (tmp (expand-file-name (format ".%s.%s.tmp"
                                        (file-name-nondirectory file)
                                        (carriage-swarm-registry--rand-hex 6))
                                dir)))
    (carriage-swarm-registry--ensure-dir dir)
    (with-temp-file tmp
      (insert contents))
    ;; Best-effort fsync is not portable; rely on rename atomicity.
    (rename-file tmp file t)
    file))

(defun carriage-swarm-registry-read ()
  "Read registry.ndjson and return list of alists (one per agent)."
  (let ((f (carriage-swarm-registry-file)))
    (if (not (file-readable-p f))
        '()
      (with-temp-buffer
        (insert-file-contents f)
        (let (acc)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((ln (string-trim (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position)))))
              (when (and (not (string-empty-p ln))
                         (not (string-prefix-p "#" ln)))
                (let ((obj (carriage-swarm-registry--json-parse ln)))
                  (when obj (push obj acc)))))
            (forward-line 1))
          (nreverse acc))))))

(defun carriage-swarm-registry--index-by-id (entries)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let ((id (alist-get 'id e)))
        (when (stringp id)
          (puthash id e ht))))
    ht))

(defun carriage-swarm-registry-write (entries)
  "Atomically rewrite registry file with ENTRIES (list of alists)."
  (let* ((f (carriage-swarm-registry-file))
         (txt (mapconcat (lambda (e) (carriage-swarm-registry--json-encode e))
                         (or entries '())
                         "\n")))
    (carriage-swarm-registry--atomic-write f (concat txt (if (string-empty-p txt) "" "\n")))))

(defun carriage-swarm-registry-upsert (entry)
  "Upsert ENTRY (alist) into registry, rewrite atomically.
ENTRY must contain (id . STRING). Returns the updated full entries list."
  (let* ((id (alist-get 'id entry))
         (entries (carriage-swarm-registry-read))
         (ht (carriage-swarm-registry--index-by-id entries)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "carriage-swarm-registry: entry missing id"))
    (puthash id entry ht)
    (let (out)
      (maphash (lambda (_id e) (push e out)) ht)
      (setq out (sort out (lambda (a b)
                            (let ((ta (or (alist-get 'started_ts a) 0))
                                  (tb (or (alist-get 'started_ts b) 0)))
                              (< (if (numberp ta) ta 0)
                                 (if (numberp tb) tb 0))))))
      (carriage-swarm-registry-write out)
      out)))

(defun carriage-swarm-registry-set-liveness (id alive &optional last-seen-ts)
  "Update liveness fields for agent ID in registry and rewrite atomically.

ALIVE is t or nil.
LAST-SEEN-TS is float unix time; when nil uses (float-time).

Returns updated entries list."
  (unless (and (stringp id) (not (string-empty-p id)))
    (error "carriage-swarm-registry: invalid id"))
  (let* ((ts (or (and (numberp last-seen-ts) last-seen-ts) (float-time)))
         (entries (carriage-swarm-registry-read))
         (updated nil)
         (out
          (mapcar
           (lambda (e)
             (if (equal (alist-get 'id e) id)
                 (progn
                   (setq updated t)
                   (setf (alist-get 'last_seen_ts e) ts)
                   (setf (alist-get 'alive e) (if alive t :false))
                   e)
               e))
           entries)))
    ;; If missing entry, do not create a new one (liveness cannot invent agents).
    (when updated
      (carriage-swarm-registry-write out))
    out))

(defun carriage-swarm-registry-touch (id &optional ts)
  "Mark agent ID as alive and update last_seen_ts (default now)."
  (carriage-swarm-registry-set-liveness id t (or ts (float-time))))

(defun carriage-swarm-registry--pid-alive-p (pid)
  "Return non-nil when PID looks alive (best-effort)."
  (and (integerp pid)
       (> pid 0)
       (condition-case _e
           (progn (signal-process pid 0) t)
         (error nil))))

(defun carriage-swarm-registry-agent-alive-p (id)
  "Return non-nil if agent ID pid looks alive."
  (let* ((pid-str (carriage-swarm-registry--read-file (carriage-swarm-registry-agent-pid-file id)))
         (pid (and pid-str (string-match-p "^[0-9]+$" pid-str) (string-to-number pid-str))))
    (carriage-swarm-registry--pid-alive-p pid)))

(defun carriage-swarm-registry-agent-read-token (id)
  "Read agent token from token file (string or nil)."
  (carriage-swarm-registry--read-file (carriage-swarm-registry-agent-token-file id)))

(defun carriage-swarm-registry-agent-read-port (id)
  "Read agent port from port file (integer or nil)."
  (let ((s (carriage-swarm-registry--read-file (carriage-swarm-registry-agent-port-file id))))
    (and (stringp s) (string-match-p "^[0-9]+$" s) (string-to-number s))))

(defun carriage-swarm-registry-agent-register (&rest plist)
  "Register an agent runtime dir + registry entry.

PLIST keys:
:id (string, optional)           ; if nil, generated
:pid (int, required)
:port (int, required)
:bind (string, optional)         ; default 127.0.0.1
:project (string|null, optional)
:label (string|null, optional)
:version (string, optional)      ; default \"v1\"
:token (string, optional)        ; if nil, generated and written to token file

Writes runtime files and upserts registry entry. Returns the id."
  (let* ((id (or (plist-get plist :id) (carriage-swarm-registry-generate-id)))
         (pid (plist-get plist :pid))
         (port (plist-get plist :port))
         (bind (or (plist-get plist :bind) carriage-swarm-registry-bind "127.0.0.1"))
         (project (plist-get plist :project))
         (label (plist-get plist :label))
         (version (or (plist-get plist :version) "v1"))
         (token (or (plist-get plist :token) (carriage-swarm-registry--random-token)))
         (adir (carriage-swarm-registry-agent-dir id))
         (meta `((bind . ,bind)
                 (started_ts . ,(float-time))
                 (project . ,project)
                 (label . ,label)
                 (version . ,version))))
    (unless (and (integerp pid) (> pid 0)) (error "carriage-swarm-registry: bad pid"))
    (unless (and (integerp port) (>= port 0)) (error "carriage-swarm-registry: bad port"))
    (carriage-swarm-registry--write-file (expand-file-name "pid" adir) (number-to-string pid))
    (carriage-swarm-registry--write-file (expand-file-name "port" adir) (number-to-string port))
    ;; token: 0600
    (carriage-swarm-registry--write-file (expand-file-name "token" adir) token #o600)
    (carriage-swarm-registry--write-file (expand-file-name "meta.json" adir)
                                         (carriage-swarm-registry--json-encode meta))
    (carriage-swarm-registry-upsert
     `((id . ,id)
       (pid . ,pid)
       (bind . ,bind)
       (port . ,port)
       (runtime_dir . ,adir)
       (meta . ((project . ,project) (label . ,label) (version . ,version)))
       (started_ts . ,(float-time))
       (last_seen_ts . :null)
       (alive . :null)))
    id))

(defun carriage-swarm-registry-remove (id)
  "Remove agent ID from registry (does not delete runtime dir)."
  (let* ((entries (carriage-swarm-registry-read))
         (entries2 (cl-remove-if (lambda (e) (equal (alist-get 'id e) id)) entries)))
    (carriage-swarm-registry-write entries2)
    entries2))

(defun carriage-swarm-registry-gc-stale (&optional delete-dirs)
  "Garbage-collect stale entries (pid not alive).
When DELETE-DIRS non-nil, also delete agents/<id>/ directories (best-effort).
Returns plist (:removed N :kept M)."
  (let* ((entries (carriage-swarm-registry-read))
         (removed 0)
         (kept 0)
         (out '()))
    (dolist (e entries)
      (let* ((id (alist-get 'id e))
             (pid (alist-get 'pid e))
             (alive (carriage-swarm-registry--pid-alive-p pid)))
        (if alive
            (progn (setq kept (1+ kept)) (push e out))
          (setq removed (1+ removed))
          (when (and delete-dirs (stringp id))
            (ignore-errors (delete-directory (carriage-swarm-registry-agent-dir id) t t))))))
    (setq out (nreverse out))
    (carriage-swarm-registry-write out)
    (list :removed removed :kept kept)))

(provide 'carriage-swarm-registry)
;;; carriage-swarm-registry.el ends here
