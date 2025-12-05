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
(require 'browse-url)

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
         (ok nil)
         (tries 0))
    (while (and (not ok) (< (float-time) deadline))
      (setq ok (carriage-webd--probe-health port))
      (unless ok
        (setq tries (1+ tries))
        (message "webd: health probe #%d failed, retrying..." tries)
        (sleep-for intv)))
    ok))

(defun carriage-webd--compute-entrypoint (bind port token)
  "Produce Lisp sexp string to evaluate in child Emacs.
The sexp logs all major stages to stdout so that the supervisor can capture them."
  (let* ((bind-s (format "%S" bind))
         (port-s (format "%S" port))
         (tok-s  (format "%S" token)))
    (format "(progn
 (setq message-log-max 10000
       debug-on-error t
       url-show-status nil)
 (princ \"webd: init starting\\n\")
 (setq carriage-web-daemon-p t
       carriage-web-bind %s
       carriage-web-port %s
       carriage-web-auth-token %s)
 (condition-case e
     (progn
       (princ \"webd: calling carriage-web-start\\n\")
       (carriage-web-start)
       (princ (format \"webd: started on %%s:%%s\\n\" carriage-web-bind carriage-web-port)))
   (error
    (princ (format \"webd: start error: %%S\\n\" e))
    (kill-emacs 1)))
 (with-temp-file %S
   (princ (format \"webd: writing port file (%%s)\\n\" %S))
   (insert (number-to-string (or carriage-web-port 0))))
 (with-temp-file %S
   (princ (format \"webd: writing pid file (%%s)\\n\" %S))
   (insert (number-to-string (or (emacs-pid) 0))))
 (princ \"webd: entering sleep loop\\n\")
 (while t (sleep-for 3600)))"
            bind-s port-s tok-s
            (carriage-webd--port-file) (carriage-webd--port-file)
            (carriage-webd--pid-file) (carriage-webd--pid-file))))

(defun carriage-webd--start-process (bind port token)
  "Start external Emacs hosting webd, return process object."
  (let* ((lispdir (carriage-webd--repo-lisp-dir))
         (cmd (list (or (executable-find "emacs") "emacs")
                    "-Q" "--quick" "--batch"
                    "-L" lispdir
                    "-l" "carriage-web.el"
                    "--eval" (carriage-webd--compute-entrypoint bind port token))))
    (message "webd: spawning %S" cmd)
    (make-process
     :name carriage-webd--proc-name
     :buffer (get-buffer-create carriage-webd--out-buf)
     :stderr (get-buffer-create carriage-webd--err-buf)
     :command cmd
     :noquery t
     :sentinel (lambda (proc event)
                 ;; Mirror child lifecycle into supervisor buffers and *Messages*
                 (let ((out (get-buffer-create carriage-webd--out-buf)))
                   (when (buffer-live-p out)
                     (with-current-buffer out
                       (goto-char (point-max))
                       (insert (format "webd: child sentinel: %s %s" (process-name proc) event)))))
                 (message "webd: child %s" (string-trim event))))))

(defun carriage-webd--ensure-token (token)
  (or token
      (or (carriage-webd--read-file (carriage-webd--token-file))
          (let ((tkn (carriage-webd--random-token)))
            (carriage-webd--write-file (carriage-webd--token-file) tkn)
            tkn))))

;;;###autoload
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
                (let ((tries 50) (pval nil))
                  (while (and (not pval) (> tries 0))
                    (let ((s (carriage-webd--read-file (carriage-webd--port-file))))
                      (setq pval (and s (string-to-number s))))
                    (message "webd: waiting port file (%d)" tries)
                    (unless (and (numberp pval) (> pval 0))
                      (setq tries (1- tries))
                      (sleep-for 0.1)))
                  (and (numberp pval) (> pval 0) pval))))
           (ok (and resolved-port (carriage-webd--wait-health resolved-port))))
      (message (if ok "webd: started on %s:%s" "webd: started (health not confirmed) on %s:%s")
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
;;;###autoload
(defun carriage-webd-status ()
  "Show Carriage web daemon status."
  (interactive)
  (let* ((pid (carriage-webd--read-pid))
         (alive (and pid (carriage-webd--alive-pid-p pid)))
         (bind carriage-webd-default-bind)
         (port (let ((s (carriage-webd--read-file (carriage-webd--port-file))))
                 (and s (string-match-p "^[0-9]+$" s) (string-to-number s))))
         (health (and port (carriage-webd--probe-health port))))
    (message "webd status: pid=%s alive=%s bind=%s port=%s health=%s runtime=%s"
             (or pid 'nil) (if alive 't 'nil) bind (or port 'nil) (if health 't 'nil)
             (carriage-webd--runtime-dir))
    (list :pid pid :alive alive :bind bind :port port :health health :runtime (carriage-webd--runtime-dir))))

;;;###autoload
;;;###autoload
(defun carriage-webd-tail-log ()
  "Show live logs (stdout) of the web daemon (webd)."
  (interactive)
  (let ((b (get-buffer-create carriage-webd--out-buf)))
    (pop-to-buffer b)
    (with-current-buffer b
      (view-mode 1)
      (goto-char (point-max)))))

(defun carriage-webd-tail-errors ()
  "Show live error logs (stderr) of the web daemon (webd)."
  (interactive)
  (let ((b (get-buffer-create carriage-webd--err-buf)))
    (pop-to-buffer b)
    (with-current-buffer b
      (view-mode 1)
      (goto-char (point-max)))))

;;;###autoload
;;;###autoload
(defun carriage-webd-kill-stale ()
  "Clean up stale webd PID/PORT files and try to kill leftover processes."
  (interactive)
  (let* ((pid (carriage-webd--read-pid))
         (alive (and pid (carriage-webd--alive-pid-p pid)))
         (pidf (carriage-webd--pid-file))
         (portf (carriage-webd--port-file)))
    ;; If PID is not alive, remove pidfile.
    (unless alive
      (when (file-readable-p pidf)
        (ignore-errors (delete-file pidf))
        (message "webd: cleaned stale pidfile")))
    (when (and pid (not alive))
      (message "webd: PID %d not alive, cleaned pidfile" pid))
    ;; Remove stale port file as well.
    (when (file-readable-p portf)
      (ignore-errors (delete-file portf))
      (message "webd: cleaned stale portfile"))
    (message "webd: stale cleanup done")))


;;;###autoload
(defun carriage-webd-open-dashboard ()
  "Open the web daemon (webd) dashboard root page in the default browser.
Uses the last known bind/port from runtime files when available."
  (interactive)
  (let* ((bind (or carriage-webd-default-bind "127.0.0.1"))
         (port (or (let ((s (carriage-webd--read-file (carriage-webd--port-file))))
                     (and s (string-match-p "^[0-9]+$" s)
                          (string-to-number s)))
                   carriage-webd-default-port
                   8787))
         (url (format "http://%s:%s/" bind port)))
    (browse-url url)))

;; Autostart (optional)
(defcustom carriage-webd-autostart nil
  "When non-nil, attempt to start the web daemon (webd) automatically after Emacs startup.
Runs only in interactive sessions and skips when a webd process is already alive."
  :type 'boolean
  :group 'carriage-webd)

(defcustom carriage-webd-autostart-delay 1.0
  "Delay in seconds before attempting webd autostart."
  :type 'number
  :group 'carriage-webd)


(defcustom carriage-webd-autostart-delay 1.0
  "Delay in seconds before attempting webd autostart."
  :type 'number
  :group 'carriage-webd)

(defun carriage-webd--maybe-autostart ()
  "Autostart webd when `carriage-webd-autostart' is non-nil."
  (when (and (not noninteractive)
             carriage-webd-autostart
             (not (carriage-webd--live-proc)))
    (run-at-time (or carriage-webd-autostart-delay 1.0) nil
                 (lambda ()
                   (ignore-errors
                     (carriage-webd-start))))))

(add-hook 'emacs-startup-hook #'carriage-webd--maybe-autostart)

;; Utilities: copy token to kill-ring (without printing it)
(defun carriage-webd-copy-token ()
  "Copy current webd auth token (from runtime token file) to the kill-ring.
For safety, the token is not printed to *Messages*."
  (interactive)
  (let ((tf (carriage-webd--token-file)))
    (if (and tf (file-readable-p tf))
        (let ((tok (carriage-webd--read-file tf)))
          (when (and tok (> (length tok) 0))
            (kill-new tok)
            (message "webd: token copied to kill-ring")))
      (message "webd: no token file found"))))

(defun carriage-webd--api-cmd (command &optional payload)
  "Send a command to webd via POST /api/cmd."
  (let* ((bind (or carriage-webd-default-bind "127.0.0.1"))
         (port-str (carriage-webd--read-file (carriage-webd--port-file)))
         (port (and port-str (string-to-number port-str)))
         (token (carriage-webd--ensure-token carriage-webd-auth-token))
         (body (json-encode (append (list :cmd command) payload)))
         (url (format "http://%s:%d/api/cmd" bind port))
         (url-request-method "POST")
         (url-show-status nil)
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("X-Auth" . ,token)))
         (url-request-data body))
    (unless (and port token)
      (error "webd not running or token not found"))
    (url-retrieve
     url
     (lambda (status)
       (with-current-buffer (current-buffer)
         (let ((ok (string-match-p "^HTTP/1\\.[01] 200" (buffer-string))))
           (message "webd cmd '%s': %s" command (if ok "ok" "failed"))
           (kill-buffer (current-buffer))))))))

;;;###autoload
(defun carriage-webd-drop-sse-clients ()
  "Tell webd to drop all connected SSE clients."
  (interactive)
  (if (not (carriage-webd--live-proc))
      (message "webd is not running")
    (carriage-webd--api-cmd "drop_sse_clients")
    (message "webd: sent drop_sse_clients command")))

;;;###autoload
(defun carriage-webd-seed-snapshot ()
  "Send a full sessions snapshot from the main Emacs to webd immediately.
Useful when Sessions list is empty: seeds the cache in the daemon."
  (interactive)
  (if (fboundp 'carriage-web--snapshot-publish-now)
      (progn
        (message "webd: seeding snapshot…")
        (ignore-errors (carriage-web--snapshot-publish-now))
        (message "webd: snapshot seed triggered"))
    (message "webd: snapshot function not available (carriage-web not loaded?)"))


(provide 'carriage-web-supervisor)

;;; carriage-web-supervisor.el ends here
