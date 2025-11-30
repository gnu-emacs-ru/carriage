;;; carriage-web-supervisor.el --- Supervisor for Carriage web daemon (webd)  -*- lexical-binding: t; -*-

;; This file provides Elisp-only supervisor commands to run the HTTP/SSE server
;; in a separate Emacs process ("webd") and manage its lifecycle.
;;
;; Specifications:
;;   spec/web-dashboard-v1.org
;;   spec/webd-ops-v1.org
;;   spec/code-style-v2.org
;;   spec/code-style-essentials-v2.org

;;; Commentary:
;; - Start/stop/restart/status for external Emacs process hosting carriage-web.el.
;; - PID/PORT files in a runtime dir (XDG_RUNTIME_DIR or /tmp fallback).
;; - Logs wired to *carriage-webd* and *carriage-webd-err* buffers.
;; - Health probe: GET /api/health with short timeouts.
;;
;; Assumptions:
;; - The repository layout has "lisp/" with carriage-web.el in it.
;; - The child process is started with: emacs -Q --quick -L <repo>/lisp -l carriage-web.el -e <entry>.
;; - The child sets carriage-web-daemon-p to t and calls (carriage-web-start).
;;
;; Security:
;; - webd binds 127.0.0.1 and requires X-Auth for /api/push.
;;
;; This supervisor does not modify user UI thread beyond short health checks.

;;; Code:

(require 'url)

(defgroup carriage-webd nil
  "Supervisor for Carriage web daemon."
  :group 'applications)

(defcustom carriage-webd-default-bind "127.0.0.1"
  "Bind address for webd."
  :type 'string
  :group 'carriage-webd)

(defcustom carriage-webd-default-port 0
  "Default port for webd. 0 means ephemeral."
  :type 'integer
  :group 'carriage-webd)

(defcustom carriage-webd-auth-token nil
  "Auth token for webd private endpoints (e.g., /api/push).
If nil, a random token will be generated on start."
  :type '(choice (const :tag "Generate on start" nil) string)
  :group 'carriage-webd)

(defcustom carriage-webd-wait-health-ms 2000
  "Total time (ms) to wait for webd health after start."
  :type 'integer :group 'carriage-webd)

(defcustom carriage-webd-health-interval-ms 150
  "Interval (ms) between health probes while waiting."
  :type 'integer :group 'carriage-webd)

(defcustom carriage-webd-runtime-dir nil
  "Directory for runtime files (PID/PORT). If nil, autodetect.
Resolution order:
1) $XDG_RUNTIME_DIR/carriage-webd
2) /tmp/carriage-webd-$USER"
  :type '(choice (const :tag "Autodetect" nil) directory)
  :group 'carriage-webd)

(defconst carriage-webd--proc-name "carriage-webd")
(defconst carriage-webd--out-buf "*carriage-webd*")
(defconst carriage-webd--err-buf "*carriage-webd-err*")

(defun carriage-webd--user ()
  (or (getenv "USER") (user-login-name) "user"))

(defun carriage-webd--runtime-dir ()
  (file-name-as-directory
   (or carriage-webd-runtime-dir
       (let* ((xdg (getenv "XDG_RUNTIME_DIR"))
              (cand (and xdg (expand-file-name "carriage-webd" xdg))))
         (cond
          ((and cand (file-directory-p xdg)) cand)
          (t (expand-file-name (format "carriage-webd-%s" (carriage-webd--user)) "/tmp")))))))

(defun carriage-webd--ensure-runtime-dir ()
  (let ((dir (carriage-webd--runtime-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun carriage-webd--pid-file ()
  (expand-file-name "webd.pid" (carriage-webd--ensure-runtime-dir)))

(defun carriage-webd--port-file ()
  (expand-file-name "webd.port" (carriage-webd--ensure-runtime-dir)))

(defun carriage-webd--token-file ()
  (expand-file-name "webd.token" (carriage-webd--ensure-runtime-dir)))

(defun carriage-webd--repo-lisp-dir ()
  "Heuristically find project lisp/ directory containing carriage-web.el."
  (let* ((start default-directory)
         (root (or (locate-dominating-file start "lisp")
                   (locate-dominating-file start "carriage.el")
                   (locate-dominating-file start ".git")
                   default-directory))
         (lisp (expand-file-name "lisp" root)))
    (unless (file-exists-p (expand-file-name "carriage-web.el" lisp))
      (error "Cannot find carriage-web.el under %s" lisp))
    lisp))

(defun carriage-webd--random-token ()
  (let ((alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (len 40))
    (apply #'concat
           (cl-loop repeat len
                    collect (string (aref alphabet (random (length alphabet))))))))

(defun carriage-webd--write-file (file content)
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert content)))

(defun carriage-webd--read-file (file)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (string-trim (buffer-string)))))

(defun carriage-webd--live-proc ()
  (get-process carriage-webd--proc-name))

(defun carriage-webd--health-url (port)
  (format "http://127.0.0.1:%d/api/health" (or port 0)))

(defun carriage-webd--probe-health (port)
  "Return t if /api/health returns HTTP 200 quickly, else nil."
  (let* ((url-request-method "GET")
         (url-show-status nil)
         (url-request-extra-headers nil)
         (url (carriage-webd--health-url port))
         (url-automatic-caching nil)
         (url-request-timeout 0.6)
         (buf (condition-case _e (url-retrieve-synchronously url t 0.7) (error nil))))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (and (re-search-forward "^HTTP/1\\.[01] +\\([0-9]+\\)" nil t)
                 (string= (match-string 1) "200")))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defun carriage-webd--wait-health (port)
  "Wait for health up to carriage-webd-wait-health-ms."
  (let* ((deadline (+ (float-time) (/ carriage-webd-wait-health-ms 1000.0)))
         (intv (/ carriage-webd-health-interval-ms 1000.0))
         (ok nil))
    (while (and (not ok) (< (float-time) deadline))
      (setq ok (carriage-webd--probe-health port))
      (unless ok (sleep-for intv)))
    ok))

(defun carriage-webd--compute-entrypoint (bind port token)
  "Produce Lisp sexp string to evaluate in child Emacs."
  (let* ((bind-s (format "%S" bind))
         (port-s (format "%S" port))
         (tok-s  (format "%S" token)))
    (format "(progn
 (setq carriage-web-daemon-p t
       carriage-web-bind %s
       carriage-web-port %s
       carriage-web-auth-token %s)
 (carriage-web-start)
 (with-temp-file %S (insert (number-to-string (or carriage-web-port 0))))
 (while t (sleep-for 3600)))"
            bind-s port-s tok-s
            (carriage-webd--port-file))))

(defun carriage-webd--start-process (bind port token)
  "Start external Emacs hosting webd, return process object."
  (let* ((lispdir (carriage-webd--repo-lisp-dir))
         (cmd (list (or (executable-find "emacs") "emacs")
                    "-Q" "--quick"
                    "-L" lispdir
                    "-l" "carriage-web.el"
                    "--eval" (carriage-webd--compute-entrypoint bind port token))))
    (make-process
     :name carriage-webd--proc-name
     :buffer (get-buffer-create carriage-webd--out-buf)
     :stderr (get-buffer-create carriage-webd--err-buf)
     :command cmd
     :noquery t)))

(defun carriage-webd--ensure-token (token)
  (or token
      (or (carriage-webd--read-file (carriage-webd--token-file))
          (let ((tkn (carriage-webd--random-token)))
            (carriage-webd--write-file (carriage-webd--token-file) tkn)
            tkn))))

;;;###autoload
(defun carriage-webd-start (&optional bind port token)
  "Start Carriage web daemon (webd) as a separate Emacs process.

BIND defaults to `carriage-webd-default-bind'.
PORT defaults to `carriage-webd-default-port' (0 = ephemeral).
TOKEN defaults to `carriage-webd-auth-token' or generated."
  (interactive)
  (when (carriage-webd--live-proc)
    (user-error "webd process already running"))
  (let* ((bind (or bind carriage-webd-default-bind))
         (port (or port carriage-webd-default-port))
         (token (carriage-webd--ensure-token (or token carriage-webd-auth-token)))
         (proc (carriage-webd--start-process bind port token)))
    (when-let ((pid (process-id proc)))
      (carriage-webd--write-file (carriage-webd--pid-file) (number-to-string pid)))
    ;; Wait for health quickly (best effort).
    (let* ((resolved-port
            (or (and (numberp port) (> port 0) port)
                (let ((tries 15) (pval nil))
                  (while (and (not pval) (> tries 0))
                    (setq pval (string-to-number (or (carriage-webd--read-file (carriage-webd--port-file)) "0")))
                    (unless (> pval 0) (setq tries (1- tries)) (sleep-for 0.1)))
                  (and (> (or pval 0) 0) pval))))
           (ok (and resolved-port (carriage-webd--wait-health resolved-port))))
      (message (if ok "webd: started on %s:%s" "webd: started (health not confirmed)")
               bind resolved-port)
      ;; If daemon is healthy, switch publisher to 'push and seed state
      (when ok
        (with-demoted-errors "webd-start: %S"
          ;; Configure publish backend and connection params for the main Emacs
          (setq carriage-web-publish-backend 'push)
          (setq carriage-web-bind bind)
          (setq carriage-web-port resolved-port)
          (setq carriage-web-auth-token token)
          ;; Start idle snapshot publisher to keep webd sessions cache warm
          (when (fboundp 'carriage-web-snapshot-start)
            (ignore-errors (carriage-web-snapshot-start)))
          ;; Seed initial snapshot immediately (best-effort, fire-and-forget)
          (when (fboundp 'carriage-web--snapshot-publish-now)
            (ignore-errors (carriage-web--snapshot-publish-now))))))
    proc))

(defun carriage-webd--signal-pid (pid sig)
  (condition-case _e
      (progn (signal-process pid sig) t)
    (error nil)))

(defun carriage-webd--read-pid ()
  (let ((s (carriage-webd--read-file (carriage-webd--pid-file))))
    (and s (string-match-p "^[0-9]+$" s) (string-to-number s))))

;;;###autoload
(defun carriage-webd-stop ()
  "Stop Carriage web daemon if running."
  (interactive)
  (let ((proc (carriage-webd--live-proc)))
    (cond
     (proc
      (delete-process proc)
      (message "webd: process deleted"))
     (t
      (let ((pid (carriage-webd--read-pid)))
        (if (not pid)
            (message "webd: no live process and no PID file")
          (if (or (carriage-webd--signal-pid pid 15) ; SIGTERM
                  (carriage-webd--signal-pid pid 9)) ; SIGKILL
              (message "webd: sent signal to PID %d" pid)
            (message "webd: failed to signal PID %d" pid)))))))
  ;; Cleanup files best-effort
  (ignore-errors (delete-file (carriage-webd--pid-file)))
  (ignore-errors (delete-file (carriage-webd--port-file))))

;;;###autoload
(defun carriage-webd-restart ()
  "Restart Carriage web daemon."
  (interactive)
  (carriage-webd-stop)
  (sleep-for 0.2)
  (carriage-webd-start))

(defun carriage-webd--alive-pid-p (pid)
  (and pid (carriage-webd--signal-pid pid 0)))

;;;###autoload
(defun carriage-webd-status ()
  "Show Carriage web daemon status."
  (interactive)
  (let* ((pid (carriage-webd--read-pid))
         (alive (and pid (carriage-webd--alive-pid-p pid)))
         (port (let ((s (carriage-webd--read-file (carriage-webd--port-file))))
                 (and s (string-match-p "^[0-9]+$" s) (string-to-number s))))
         (health (and port (carriage-webd--probe-health port))))
    (message "webd status: pid=%s alive=%s port=%s health=%s runtime=%s"
             (or pid 'nil) (if alive 't 'nil) (or port 'nil) (if health 't 'nil)
             (carriage-webd--runtime-dir))
    (list :pid pid :alive alive :port port :health health :runtime (carriage-webd--runtime-dir))))

;;;###autoload
(defun carriage-webd-tail-log ()
  "Show live logs of webd."
  (interactive)
  (let ((b (get-buffer-create carriage-webd--out-buf)))
    (pop-to-buffer b)
    (with-current-buffer b
      (view-mode 1)
      (goto-char (point-max)))))

;;;###autoload
(defun carriage-webd-kill-stale ()
  "Clean up stale webd PID files and try to kill leftover processes."
  (interactive)
  (let* ((pid (carriage-webd--read-pid))
         (alive (and pid (carriage-webd--alive-pid-p pid))))
    (unless alive
      (ignore-errors (delete-file (carriage-webd--pid-file))))
    (when (and pid (not alive))
      (message "webd: PID %d not alive, cleaned pidfile" pid))
    (when (file-readable-p (carriage-webd--port-file))
      (ignore-errors (delete-file (carriage-webd--port-file))))
    (message "webd: stale cleanup done"))


(provide 'carriage-web-supervisor)

;;; carriage-web-supervisor.el ends here
