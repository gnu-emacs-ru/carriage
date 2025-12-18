;;; carriage-agent.el --- Headless Carriage Agent (Swarm)  -*- lexical-binding: t; -*-

;; Agent process entrypoint:
;; - starts Carriage + local HTTP/SSE server (carriage-web)
;; - registers itself in Swarm registry (pid/port/token/meta)
;; - never serves remote; loopback only
;;
;; Specs:
;;   spec/swarm-v1.org
;;   spec/agent-http-api-v1.org
;;   spec/registry-v1.org

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-web)
(require 'carriage-swarm-registry)

(defgroup carriage-agent nil
  "Carriage Swarm Agent."
  :group 'carriage)

(defcustom carriage-agent-bind "127.0.0.1"
  "Bind address for the agent HTTP/SSE server."
  :type 'string
  :group 'carriage-agent)

(defcustom carriage-agent-port 0
  "Port for the agent HTTP/SSE server. 0 means ephemeral."
  :type 'integer
  :group 'carriage-agent)

(defcustom carriage-agent-enable-cmd t
  "When non-nil, enable /api/cmd whitelist on the agent."
  :type 'boolean
  :group 'carriage-agent)

(defcustom carriage-agent-project-root nil
  "Project root for this agent session. When nil, uses default-directory."
  :type '(choice (const :tag "Use default-directory" nil) directory)
  :group 'carriage-agent)

(defcustom carriage-agent-label nil
  "Optional human label for this agent."
  :type '(choice (const nil) string)
  :group 'carriage-agent)

(defcustom carriage-agent-id nil
  "Optional explicit agent id. When nil, a unique id is generated."
  :type '(choice (const nil) string)
  :group 'carriage-agent)

(defun carriage-agent--sleep-forever ()
  (while t (sleep-for 3600)))

;;;###autoload
(defun carriage-agent-main (&rest plist)
  "Agent entrypoint for --eval.

PLIST keys:
:id (string)         ; optional
:project (string)    ; optional project root
:label (string)      ; optional label
:bind (string)       ; optional bind (default 127.0.0.1)
:port (int)          ; optional port (default 0)

Example:
  emacs -Q --batch -L lisp -l carriage-agent.el \\
    --eval '(carriage-agent-main :project \"/path\" :label \"p\" :port 0)'"
  (let* ((id (or (plist-get plist :id) carriage-agent-id (carriage-swarm-registry-generate-id)))
         (root (or (plist-get plist :project) carriage-agent-project-root default-directory))
         (label (or (plist-get plist :label) carriage-agent-label))
         (bind (or (plist-get plist :bind) carriage-agent-bind "127.0.0.1"))
         (port (or (plist-get plist :port) carriage-agent-port 0)))
    (setq default-directory (file-name-as-directory (expand-file-name root)))
    ;; Agent must act like a dedicated server process (cache discipline).
    (setq carriage-web-daemon-p t)
    (setq carriage-web-bind bind)
    (setq carriage-web-port port)
    ;; Token is required by swarm spec: per-agent token stored in registry runtime dir.
    (setq carriage-web-auth-token (carriage-swarm-registry-generate-token))
    (setq carriage-web-api-commands-enabled (and carriage-agent-enable-cmd t))
    ;; Start HTTP/SSE
    (carriage-web-start)
    ;; Register in registry (writes runtime files, including token 0600)
    (carriage-swarm-registry-agent-register
     :id id
     :pid (or (ignore-errors (emacs-pid)) 0)
     :port (or carriage-web-port 0)
     :bind bind
     :project (ignore-errors (file-name-nondirectory (directory-file-name default-directory)))
     :label label
     :version "v1"
     :token carriage-web-auth-token)
    (princ (format "agent: started id=%s bind=%s port=%s\n" id bind carriage-web-port))
    (carriage-agent--sleep-forever)))

(provide 'carriage-agent)
;;; carriage-agent.el ends here
