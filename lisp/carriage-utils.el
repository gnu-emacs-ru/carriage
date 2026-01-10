;;; carriage-utils.el --- Utility helpers and I/O  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: utils
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/git-integration-v2.org
;;   spec/security-v2.org
;;   spec/async-workflow-v2.org
;;
;;; Commentary:
;; Common helpers: project-root, normalize-path, git call wrappers.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'carriage-errors)
(require 'carriage-logging)

(defcustom carriage-mode-git-timeout-seconds 15
  "Max seconds to wait for git commands before timing out.
When the timeout elapses, the process is terminated and a non-zero exit
code (124) is returned with :stderr set to \"timeout\"."
  :type 'integer
  :group 'carriage)

;; Silence byte-compiler about project.el functions when not loaded at compile time.
(declare-function project-current "project" (&optional dir))
(declare-function project-root "project" (project))

(defun carriage--call-git (default-dir &rest args)
  "Call git with ARGS in DEFAULT-DIR, return (:exit :stdout :stderr).
Respects `carriage-mode-git-timeout-seconds'. Uses `make-process' and
`accept-process-output' in a loop to avoid blocking the UI. Emits detailed logs
about the lifecycle (spawn, wait ticks, timeout/exit) to help diagnose stalls."
  (let* ((default-directory (file-name-as-directory (expand-file-name default-dir))))
    (let* ((stdout-buf (generate-new-buffer " *carriage-git-stdout*"))
           (stderr-buf (generate-new-buffer " *carriage-git-stderr*"))
           (exit-code nil)
           (done nil)
           (proc nil)
           (timeout (or carriage-mode-git-timeout-seconds 15))
           (deadline (+ (float-time) (max 0 timeout)))
           (start (float-time))
           (tick 0))
      (carriage-log "git: exec dir=%s cmd=%s"
                    default-directory
                    (mapconcat #'identity (cons "git" (mapcar (lambda (x) (format "%s" x)) args)) " "))
      (unwind-protect
          (progn
            ;; Inject a safe fallback identity for 'git commit' when test repos lack config.
            (let* ((args2 (if (and args (stringp (car args)) (string= (car args) "commit"))
                              (append '("-c" "user.email=carriage@example.com"
                                        "-c" "user.name=Carriage")
                                      args)
                            args)))
              (setq proc
                    (make-process
                     :name "carriage-git"
                     :command (append (list "git") args2)
                     :buffer stdout-buf
                     :stderr stderr-buf
                     :noquery t
                     :connection-type 'pipe
                     :sentinel (lambda (p _e)
                                 (when (memq (process-status p) '(exit signal))
                                   (setq exit-code (condition-case _
                                                       (process-exit-status p)
                                                     (error exit-code)))
                                   (setq done t))))))
            (when (process-live-p proc)
              (carriage-log "git: pid=%s spawned" (process-id proc)))
            ;; Wait loop with timeout, emit 1s ticks (only if process started)
            (if (process-live-p proc)
                (progn
                  (while (and (not done) (< (float-time) deadline))
                    (accept-process-output proc 0.05)
                    (let ((elapsed (- (float-time) start)))
                      (when (>= elapsed (1+ tick))
                        (setq tick (1+ tick))
                        (carriage-log "git: waiting pid=%s elapsed=%.2fs"
                                      (and (process-live-p proc) (process-id proc))
                                      elapsed))))
                  ;; Timeout handling
                  (when (and (not done) (process-live-p proc))
                    (let ((elapsed (- (float-time) start)))
                      (carriage-log "git: timeout after %.2fs; interrupt/kill pid=%s"
                                    elapsed (process-id proc)))
                    (ignore-errors (interrupt-process proc))
                    (ignore-errors (kill-process proc))
                    (setq exit-code 124)
                    (setq done t)))
              ;; Process failed to start: exit early to avoid UI stalls
              (progn
                (setq exit-code (or exit-code -1))
                (setq done t)
                (carriage-log "git: process not started; skipping wait loop")))
            ;; Collect outputs
            (let* ((stdout (with-current-buffer stdout-buf (buffer-string)))
                   (stderr (with-current-buffer stderr-buf (buffer-string))))
              (when (and (= (or exit-code -1) 124)
                         (or (null stderr) (string-empty-p stderr)))
                (setq stderr "timeout"))
              (carriage-log "git: exit=%s stdout-bytes=%d stderr-bytes=%d"
                            (or exit-code -1)
                            (length (or stdout "")) (length (or stderr "")))
              (list :exit (or exit-code -1)
                    :stdout stdout
                    :stderr stderr)))
        (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
        (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))))))

(defun carriage-project-root ()
  "Detect project root directory. Return absolute path or nil."
  (let* ((default-dir (expand-file-name default-directory))
         (root nil))
    (when (file-remote-p default-dir)
      (signal (carriage-error-symbol 'IO_E_PATH) (list "TRAMP is not supported in v1")))
    (condition-case _e
        (when (require 'project nil t)
          (let* ((proj (project-current nil))
                 (pr (and proj (project-root proj))))
            (when (and pr (file-directory-p pr))
              (setq root (expand-file-name pr)))))
      (error nil))
    (unless root
      (condition-case _e
          (let ((found (locate-dominating-file default-dir ".git")))
            (when (and found (file-directory-p found))
              (setq root (expand-file-name found))))
        (error nil)))
    ;; IMPORTANT:
    ;; Do NOT spawn external git processes on file open / mode enable path.
    ;; When not inside a repo (no project.el root and no .git found), return nil
    ;; and let higher-level features operate in "limited" mode.
    root))

(defun carriage--path-looks-unsafe-p (path)
  "Return non-nil if PATH looks unsafe (absolute or has .. or starts with ~)."
  (or (file-name-absolute-p path)
      (string-prefix-p "~" path)
      (seq-some (lambda (seg) (string= seg "..")) (split-string path "/" t))))

(defun carriage-normalize-path (root relpath)
  "Normalize RELPATH under ROOT, ensuring it stays within ROOT. Return truename."
  (when (file-remote-p root)
    (signal (carriage-error-symbol 'IO_E_PATH) (list "TRAMP is not supported in v1")))
  (when (carriage--path-looks-unsafe-p relpath)
    (signal (carriage-error-symbol 'OPS_E_PATH) (list (format "Недопустимый путь: %s" relpath))))
  (let* ((abs (expand-file-name relpath root))
         (tru (file-truename abs))
         (root-tru (file-truename (file-name-as-directory (expand-file-name root)))))
    (unless (string-prefix-p root-tru (file-name-as-directory tru))
      (signal (carriage-error-symbol 'OPS_E_PATH) (list (format "Выход за корень: %s" relpath))))
    tru))

(defun carriage-generate-delim ()
  "Generate a random 6-hex lowercase token."
  (let* ((b1 (random 256))
         (b2 (random 256))
         (b3 (random 256)))
    (format "%02x%02x%02x" b1 b2 b3)))

(defun carriage-read-file-string (path)
  "Read file at PATH and return its contents as string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun carriage-write-file-string (path content &optional mkdir)
  "Write CONTENT to PATH. When MKDIR non-nil, create parent dirs."
  (let ((dir (file-name-directory path)))
    (when (and mkdir dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file path
    (insert content))
  t)

(defun carriage--plist-get* (plist &rest keys)
  "Get nested KEYS from PLIST treating it as plist."
  (let* ((cur plist))
    (while (and keys cur)
      (setq cur (plist-get cur (car keys)))
      (setq keys (cdr keys)))
    cur))

(defun carriage--alist (plist)
  "Convert PLIST to an alist with symbol keys."
  (let* ((res nil) (p plist))
    (while p
      (let ((k (car p)) (v (cadr p)))
        (setq res (cons (cons k v) res)))
      (setq p (cddr p)))
    (nreverse res)))

;; DELIM markers were removed in v1.1: :op create uses raw body content between begin/end.

(provide 'carriage-utils)
;;; carriage-utils.el ends here
