;;; carriage-web-publish.el --- Helpers to switch publish backend to webd and seed snapshot  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Carriage Team
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: tools, ui, http
;;
;; Commentary:
;; Utilities to configure Carriage main Emacs to publish events/snapshots to the external
;; web daemon (webd) via /api/push, using runtime PID/PORT/TOKEN files.
;;
;; This is a non-intrusive helper: it detects available symbols at runtime and
;; degrades gracefully when modules are not loaded yet.
;;
;;; Code:

(require 'subr-x)

;; Safety shim: ensure normalizer exists even if carriage-web.el isn't loaded yet.
;; This avoids timer crashes (void-function carriage-web--payload-normalize)
;; in mixed/older load orders. Raw HTTP helper will still normalize properly.
(unless (fboundp 'carriage-web--payload-normalize)
  (defun carriage-web--payload-normalize (payload)
    (condition-case _e
        payload
      (error payload))))

(defgroup carriage-web-publish nil
  "Configure Carriage main Emacs to publish to webd."
  :group 'carriage)

(defcustom carriage-webd-runtime-dir nil
  "Runtime directory where webd writes webd.port and webd.token.
When nil, the helper will derive it from XDG_RUNTIME_DIR or /run/user/UID/carriage-webd."
  :type '(choice (const :tag "Auto" nil) directory)
  :group 'carriage-web-publish)

(defcustom carriage-web-publish-verbose nil
  "When non-nil, emit helper progress messages to *Messages*.
Keep nil by default to avoid echo-area noise on startup."
  :type 'boolean
  :group 'carriage-web-publish)

(defun carriage-web-publish--xdg-runtime-dir ()
  "Return XDG_RUNTIME_DIR or best-effort /run/user/UID."
  (or (getenv "XDG_RUNTIME_DIR")
      (let* ((uid (or (ignore-errors (user-uid)) (ignore-errors (string-to-number (user-real-uid))) 1000)))
        (format "/run/user/%s" uid))))

(defun carriage-web-publish--runtime-dir ()
  "Return absolute runtime dir for webd."
  (let* ((rd (or carriage-webd-runtime-dir
                 (let ((base (file-name-as-directory (carriage-web-publish--xdg-runtime-dir))))
                   (expand-file-name "carriage-webd" base)))))
    (file-name-as-directory rd)))

(defun carriage-web-publish--port-file ()
  "Return absolute path to webd.port."
  (expand-file-name "webd.port" (carriage-web-publish--runtime-dir)))

(defun carriage-web-publish--token-file ()
  "Return absolute path to webd.token."
  (expand-file-name "webd.token" (carriage-web-publish--runtime-dir)))

(defun carriage-web-publish--read-file (path)
  "Read PATH as a string (trimmed) or return nil."
  (condition-case _
      (when (and (stringp path) (file-readable-p path))
        (string-trim (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string))))
    (error nil)))

(defun carriage-web-publish--set-if-bound (sym val)
  "Set SYM to VAL when SYM is bound, return non-nil on success."
  (when (and (symbolp sym) (boundp sym))
    (set sym val)
    t))

(defun carriage-web-publish--maybe-message (fmt &rest args)
  "Best-effort message for diagnostics; avoid noise in batch."
  (when (and carriage-web-publish-verbose (not noninteractive) (fboundp 'message))
    (apply #'message fmt args)))

;;;###autoload
(defun carriage-web-publish-configure-from-runtime ()
  "Configure publish target (bind/port/token) from webd runtime files.
- Reads webd.port and webd.token from the runtime dir.
- Sets carriage-web-bind, carriage-web-port and carriage-web-auth-token when they are bound.
Return plist (:bind STR :port INT :token STR) on success or nil."
  (interactive)
  (let* ((bind "127.0.0.1")
         (port-str (carriage-web-publish--read-file (carriage-web-publish--port-file)))
         (port (and port-str (string-match-p "^[0-9]+$" port-str) (string-to-number port-str)))
         (token (carriage-web-publish--read-file (carriage-web-publish--token-file))))
    (unless (and (numberp port) (> port 0))
      (carriage-web-publish--maybe-message "carriage-web-publish: runtime port not available yet")
      (cl-return-from carriage-web-publish-configure-from-runtime nil))
    (carriage-web-publish--set-if-bound 'carriage-web-bind bind)
    (carriage-web-publish--set-if-bound 'carriage-web-port port)
    (when (and (stringp token) (> (length token) 0))
      (carriage-web-publish--set-if-bound 'carriage-web-auth-token token))
    (carriage-web-publish--maybe-message "carriage-web-publish: target %s:%s configured" bind port)
    (list :bind bind :port port :token token)))

;;;###autoload
(defun carriage-web-publish-switch-to-push ()
  "Switch Carriage publish backend to 'push and configure target from runtime files.
Sets carriage-web-publish-backend when present; otherwise prints a hint.
Returns non-nil (runtime plist) on success, nil otherwise."
  (interactive)
  (let* ((conf (carriage-web-publish-configure-from-runtime)))
    (unless conf
      (carriage-web-publish--maybe-message "carriage-web-publish: cannot configure target (runtime files missing?)"))
    (cond
     ((boundp 'carriage-web-publish-backend)
      (setq carriage-web-publish-backend 'push)
      (carriage-web-publish--maybe-message "carriage-web-publish: backend set to 'push"))
     (t
      (carriage-web-publish--maybe-message "carriage-web-publish: backend var not found; ensure carriage-web.el is loaded")))
    conf))

;;;###autoload
(defun carriage-web-publish-seed-snapshot ()
  "Publish an immediate snapshot to webd when snapshot publisher is available."
  (interactive)
  (cond
   ((fboundp 'carriage-web--snapshot-publish-now)
    (ignore-errors (carriage-web--snapshot-publish-now))
    (carriage-web-publish--maybe-message "carriage-web-publish: snapshot seed published"))
   (t
    (carriage-web-publish--maybe-message "carriage-web-publish: snapshot function not found; load carriage-web.el"))))



;;;###autoload
(defun carriage-web-publish-setup-now ()
  "Switch to 'push publish backend (from webd runtime) and seed a snapshot soon.
Intended to be called right after webd health becomes OK."
  (interactive)
  (when (carriage-web-publish-switch-to-push)
    (run-at-time 0.1 nil #'carriage-web-publish-seed-snapshot)))

(provide 'carriage-web-publish)
;;; carriage-web-publish.el ends here
