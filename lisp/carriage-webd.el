;;; carriage-webd.el --- Supervisor for external Emacs web daemon  -*- lexical-binding: t; -*-

;; Minimal Elisp-only supervisor to run carriage-web HTTP/SSE server
;; in a separate Emacs process (prevents UI freezes in the main Emacs).
;;
;; Commands:
;;  - carriage-webd-start    — start web daemon (emacs --quick --batch …)
;;  - carriage-webd-stop     — stop it
;;  - carriage-webd-restart  — restart
;;  - carriage-webd-status   — show status plist
;;  - carriage-webd-tail-log — view daemon log buffer

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-webd nil
  "Supervisor for Carriage web daemon (external Emacs process)."
  :group 'applications)

(defcustom carriage-webd-program "emacs"
  "Program to spawn as the web daemon (another Emacs)."
  :type 'string :group 'carriage-webd)

(defcustom carriage-webd-bind "127.0.0.1"
  "Bind address for web daemon."
  :type 'string :group 'carriage-webd)

(defcustom carriage-webd-port 8787
  "Preferred TCP port for web daemon (0 = ephemeral)."
  :type 'integer :group 'carriage-webd)

(defcustom carriage-webd-token nil
  "Auth token for web daemon (X-Auth)."
  :type '(choice (const nil) string) :group 'carriage-webd)

(defvar carriage-webd--process nil
  "Process handle of the running web daemon, if any.")

(defconst carriage-webd--log-buffer "*carriage-webd*"
  "Name of the buffer used to capture daemon stdout/stderr.")

(defun carriage-webd--alive-p ()
  "Return non-nil if the web daemon process is alive."
  (and (processp carriage-webd--process) (process-live-p carriage-webd--process)))

(defun carriage-webd--build-eval-form (bind port token)
  "Return a Lisp form string to start carriage-web in a child Emacs."
  (let ((binds (prin1-to-string (or bind carriage-webd-bind)))
        (portn (or port carriage-webd-port))
        (tok   (if token (prin1-to-string token) "nil")))
    (format
     "(progn
        (setq carriage-web-enabled t)
        (setq carriage-web-daemon-p t)
        (setq carriage-web-bind %s)
        (setq carriage-web-port %d)
        (setq carriage-web-auth-token %s)
        (require 'carriage-web)
        (carriage-web-start)
        (while t (sleep-for 1)))"
     binds portn tok)))

;;;###autoload
(defun carriage-webd-start (&optional bind port token)
  "Start external web daemon in another Emacs process.
Optional BIND, PORT and TOKEN override defaults."
  (interactive)
  (when (carriage-webd--alive-p)
    (user-error "carriage-webd already running"))
  (let* ((buf (get-buffer-create carriage-webd--log-buffer))
         (args (list "--quick" "--batch"
                     "-l" "carriage.el"
                     "--eval" (carriage-webd--build-eval-form
                               (or bind carriage-webd-bind)
                               (or port carriage-webd-port)
                               (or token carriage-webd-token)))))
    (setq carriage-webd--process
          (make-process
           :name "carriage-webd"
           :buffer buf
           :stderr buf
           :command (cons carriage-webd-program args)
           :noquery t))
    (message "carriage-webd: starting (%s %s)" carriage-webd-program
             (mapconcat #'identity args " "))
    ;; After spawn: await /api/health and, when ready, switch publish backend to 'push
    (run-at-time
     0.5 nil
     (lambda ()
       (let* ((bind (or carriage-webd-bind "127.0.0.1"))
              (port (or carriage-webd-port 8787))
              (url (format "http://%s:%s/api/health" bind port))
              (url-request-method "GET")
              (url-request-extra-headers
               (when carriage-webd-token `(("X-Auth" . ,carriage-webd-token)))))
         (condition-case _e
             (url-retrieve
              url
              (lambda (buf)
                (unwind-protect
                    (with-current-buffer buf
                      (goto-char (point-min))
                      (when (re-search-forward "^HTTP/1\\.[01] 200" nil t)
                        ;; Switch publisher and start snapshot idle sender in main Emacs
                        (setq carriage-web-publish-backend 'push)
                        (setq carriage-web-bind carriage-webd-bind)
                        (setq carriage-web-port carriage-webd-port)
                        (setq carriage-web-auth-token carriage-webd-token)
                        (ignore-errors (carriage-web-snapshot-start))))
                  (when (buffer-live-p buf) (kill-buffer buf))))
              nil t)
           (error nil)))))
    carriage-webd--process))

;;;###autoload
(defun carriage-webd-stop ()
  "Stop the running web daemon process, if any."
  (interactive)
  (if (not (carriage-webd--alive-p))
      (message "carriage-webd: not running")
    (ignore-errors (delete-process carriage-webd--process))
    (setq carriage-webd--process nil)
    (message "carriage-webd: stopped")
    t))

;;;###autoload
(defun carriage-webd-restart ()
  "Restart the web daemon with current settings."
  (interactive)
  (carriage-webd-stop)
  (run-at-time 0.2 nil #'carriage-webd-start)
  (message "carriage-webd: restarting…"))

;;;###autoload
(defun carriage-webd-status ()
  "Return a plist describing current daemon status; show message interactively."
  (interactive)
  (let* ((live (carriage-webd--alive-p))
         (proc carriage-webd--process)
         (cmd  (and live (process-command proc)))
         (pid  (and live (process-id proc)))
         (pl   (list :alive (and live t)
                     :pid pid
                     :command cmd
                     :bind carriage-webd-bind
                     :port carriage-webd-port)))
    (when (called-interactively-p 'interactive)
      (message "carriage-webd: %s%s%s"
               (if live "running" "stopped")
               (if pid (format " pid=%s" pid) "")
               (if cmd (format " cmd=%S" cmd) "")))
    pl))

;;;###autoload
(defun carriage-webd-tail-log ()
  "Show the daemon log buffer."
  (interactive)
  (let ((buf (get-buffer-create carriage-webd--log-buffer)))
    (pop-to-buffer buf)))

(provide 'carriage-webd)
;;; carriage-webd.el ends here
