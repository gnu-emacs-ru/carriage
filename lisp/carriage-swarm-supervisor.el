;;; carriage-swarm-supervisor.el --- Supervisor for Swarm (Agents + Hub)  -*- lexical-binding: t; -*-

;; Forward-only supervisor:
;; - main Emacs never serves HTTP/SSE
;; - starts/stops agent and hub Emacs processes
;; - registry is the source of truth for discovery
;;
;; Specifications:
;;   spec/swarm-v1.org
;;   spec/registry-v1.org
;;   spec/hub-http-api-v1.org

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)

(require 'carriage-swarm-registry)

(defgroup carriage-swarm-supervisor nil
  "Manage Carriage Swarm processes from main Emacs."
  :group 'carriage)

(defcustom carriage-swarm-emacs-program nil
  "Emacs executable path used to spawn agents/hub. When nil, uses (executable-find \"emacs\")."
  :type '(choice (const nil) string)
  :group 'carriage-swarm-supervisor)

(defcustom carriage-swarm-hub-port 8787
  "Default hub port."
  :type 'integer
  :group 'carriage-swarm-supervisor)

(defvar carriage-swarm--procs (make-hash-table :test 'equal)
  "Map id -> process for started agents (best-effort, may be nil if started externally).")

(defvar carriage-swarm--hub-proc nil)

(defun carriage-swarm--repo-lisp-dir ()
  (let* ((start default-directory)
         (root (or (locate-dominating-file start "lisp")
                   (locate-dominating-file start ".git")
                   default-directory))
         (lisp (expand-file-name "lisp" root)))
    (unless (file-exists-p (expand-file-name "carriage-agent.el" lisp))
      (error "Cannot find lisp/carriage-agent.el under %s" lisp))
    lisp))

(defun carriage-swarm--emacs ()
  (or carriage-swarm-emacs-program
      (executable-find "emacs")
      "emacs"))

(defun carriage-swarm-agent-start (&optional project-root label)
  "Start a headless Agent for PROJECT-ROOT (default: current project or default-directory).
Returns agent id."
  (interactive)
  (let* ((root (file-name-as-directory (expand-file-name (or project-root default-directory))))
         (id (carriage-swarm-registry-generate-id))
         (lab (or label (file-name-nondirectory (directory-file-name root))))
         (lispdir (carriage-swarm--repo-lisp-dir))
         (cmd (list (carriage-swarm--emacs)
                    "-Q" "--quick" "--batch"
                    "-L" lispdir
                    "-l" "carriage-swarm-registry.el"
                    "-l" "carriage-web.el"
                    "-l" "carriage-agent.el"
                    "--eval"
                    (format "(carriage-agent-main :id %S :project %S :label %S :bind %S :port %S)"
                            id root lab "127.0.0.1" 0)))
         (out (get-buffer-create (format "*carriage-agent:%s*" id)))
         (err (get-buffer-create (format "*carriage-agent:%s-err*" id)))
         (proc (make-process
                :name (format "carriage-agent:%s" id)
                :buffer out
                :stderr err
                :command cmd
                :noquery t
                :sentinel (lambda (p e)
                            (with-current-buffer out
                              (goto-char (point-max))
                              (insert (format "\nagent sentinel: %s\n" (string-trim e))))
                            (remhash id carriage-swarm--procs)))))
    (puthash id proc carriage-swarm--procs)
    (message "carriage-swarm: agent starting id=%s root=%s" id root)
    id))

(defun carriage-swarm-agent-stop (id)
  "Stop agent process for ID (best-effort).
Also removes ID from registry (runtime dir remains unless GC)."
  (interactive
   (list (completing-read "Agent id: "
                          (mapcar (lambda (e) (alist-get 'id e))
                                  (carriage-swarm-registry-read))
                          nil t)))
  (let ((proc (gethash id carriage-swarm--procs)))
    (when (processp proc)
      (ignore-errors (delete-process proc)))
    (remhash id carriage-swarm--procs)
    (ignore-errors (carriage-swarm-registry-remove id))
    (message "carriage-swarm: agent stop requested id=%s" id)
    t))

(defun carriage-swarm-gc-stale (&optional delete-dirs)
  "GC stale registry entries (pid not alive)."
  (interactive "P")
  (let* ((res (carriage-swarm-registry-gc-stale delete-dirs)))
    (message "carriage-swarm: gc stale removed=%s kept=%s"
             (plist-get res :removed) (plist-get res :kept))
    res))

(defun carriage-swarm-hub-start (&optional port)
  "Start Hub process on PORT (default: `carriage-swarm-hub-port')."
  (interactive)
  (when (and (processp carriage-swarm--hub-proc)
             (process-live-p carriage-swarm--hub-proc))
    (user-error "Hub already running"))
  (let* ((lispdir (carriage-swarm--repo-lisp-dir))
         (p (or port carriage-swarm-hub-port))
         (cmd (list (carriage-swarm--emacs)
                    "-Q" "--quick" "--batch"
                    "-L" lispdir
                    "-l" "carriage-swarm-registry.el"
                    "-l" "carriage-hub.el"
                    "--eval"
                    (format "(let ((carriage-hub-bind %S) (carriage-hub-port %S)) (carriage-hub-main))"
                            "127.0.0.1" p)))
         (out (get-buffer-create "*carriage-hub*"))
         (err (get-buffer-create "*carriage-hub-err*")))
    (setq carriage-swarm--hub-proc
          (make-process
           :name "carriage-hub"
           :buffer out
           :stderr err
           :command cmd
           :noquery t
           :sentinel (lambda (_p e)
                       (with-current-buffer out
                         (goto-char (point-max))
                         (insert (format "\nhub sentinel: %s\n" (string-trim e))))
                       (setq carriage-swarm--hub-proc nil)))))
  (message "carriage-swarm: hub starting on 127.0.0.1:%s" (or port carriage-swarm-hub-port))
  t)

(defun carriage-swarm-hub-stop ()
  "Stop hub process."
  (interactive)
  (when (processp carriage-swarm--hub-proc)
    (ignore-errors (delete-process carriage-swarm--hub-proc)))
  (setq carriage-swarm--hub-proc nil)
  (message "carriage-swarm: hub stopped")
  t)

(defun carriage-swarm-open-dashboard (&optional port)
  "Open hub dashboard in browser."
  (interactive)
  (let ((p (or port carriage-swarm-hub-port)))
    (browse-url (format "http://127.0.0.1:%d/" p))))

(provide 'carriage-swarm-supervisor)
;;; carriage-swarm-supervisor.el ends here
