;;; carriage-engine-git.el --- Git apply engine  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: engines, git
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/apply-engines-v2.org
;;   spec/git-integration-v2.org
;;   spec/tool-contracts-v2.org
;;
;;; Commentary:
;; Engine that invokes git (git apply / git add / git mv / git rm) for operations.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-apply-engine)
;; carriage-git is already required early by carriage.el (helpers/vars).
;; We don't call its sync helpers here; engine is fully async via processes.

(defgroup carriage-engine-git nil
  "Git-based apply engine for Carriage."
  :group 'carriage-engines
  :prefix "carriage-engine-git-")

(defcustom carriage-engine-git-timeout-seconds 30
  "Per-process timeout in seconds for git engine operations."
  :type 'integer :group 'carriage-engine-git)

(defun carriage-engine-git--policy-index-p ()
  "Return non-nil when staging policy is 'index."
  (and (boundp 'carriage-apply-stage-policy)
       (eq carriage-apply-stage-policy 'index)))

(defun carriage-engine-git--get (item key)
  "Get KEY from ITEM supporting both plist and alist plan representations."
  (if (plist-member item key) (plist-get item key) (alist-get key item)))

(defun carriage-engine-git--join-cmd (argv)
  "Return a readable single-line shell-ish string from ARGV."
  (mapconcat (lambda (s)
               (cond
                ((string-match-p "[ \t\"'\\\\]" s) (format "%S" s))
                (t s)))
             argv " "))

(defun carriage-engine-git--cleanup-token (token)
  "Cleanup buffers/files/timers associated with TOKEN."
  (let ((out (plist-get token :stdout-buf))
        (err (plist-get token :stderr-buf))
        (tmp (plist-get token :patch-file))
        (tmr (plist-get token :timer)))
    (when (and (bufferp out) (buffer-live-p out))
      (ignore-errors (kill-buffer out)))
    (when (and (bufferp err) (buffer-live-p err))
      (ignore-errors (kill-buffer err)))
    (when (and (stringp tmp) (file-exists-p tmp))
      (ignore-errors (delete-file tmp)))
    (when (timerp tmr)
      (ignore-errors (cancel-timer tmr)))
    ;; Mark timer cleared to avoid racy callbacks acting on stale token
    (plist-put token :timer nil)))

(defun carriage-engine-git-abort (token)
  "Attempt to abort the running process associated with TOKEN."
  (let ((proc (plist-get token :process)))
    (when (process-live-p proc)
      (carriage-log "engine[git] abort: pid=%s timed-out=%s"
                    (condition-case _ (process-id proc) (error nil))
                    (plist-get token :timed-out))
      (unless (plist-get token :timed-out)
        (plist-put token :aborted t)
        (ignore-errors (set-process-query-on-exit-flag proc nil))
        (ignore-errors (interrupt-process proc))
        (ignore-errors (kill-process proc)))
      ;; Cancel any pending timer defensively
      (let ((tm (plist-get token :timer)))
        (when (timerp tm)
          (ignore-errors (cancel-timer tm))
          (plist-put token :timer nil)))
      (carriage-log "engine[git] abort: finalized (timer cleared=%s)"
                    (null (plist-get token :timer)))
      t)))

(defun carriage-engine-git--start (root argv token on-done on-fail)
  "Start process git ARGV in ROOT, wire TOKEN state, set sentinel and timeout."
  (when (file-remote-p (or root ""))
    (funcall on-fail (list :error 'remote-not-supported :details "TRAMP is disabled in v1"))
    (cl-return-from carriage-engine-git--start nil))
  (let* ((default-directory (file-name-as-directory (expand-file-name root)))
         (name (format "carriage-git:%s" (or (plist-get token :op) "op")))
         (out (generate-new-buffer " *carriage-git-stdout*"))
         (err (generate-new-buffer " *carriage-git-stderr*"))
         (cmd (cons "git" argv))
         (cmd-str (carriage-engine-git--join-cmd cmd))
         (done-called nil)
         (finalize
          (lambda (okp exit-code reason)
            (unless done-called
              (setq done-called t)
              (let* ((stdout (with-current-buffer out (buffer-string)))
                     (stderr (with-current-buffer err (buffer-string)))
                     (res (list :engine 'git
                                :exit exit-code
                                :stdout stdout
                                :stderr stderr
                                :stdout-bytes (string-bytes stdout)
                                :stderr-bytes (string-bytes stderr)
                                :pid (plist-get token :pid)
                                :op (plist-get token :op)
                                :path (plist-get token :path)
                                :reason reason)))
                (carriage-log "engine[git] exit: code=%s stdout=%dB stderr=%dB reason=%s"
                              exit-code
                              (plist-get res :stdout-bytes)
                              (plist-get res :stderr-bytes)
                              (or reason 'ok))
                ;; Gentle user-facing message for timeout/abort (no throw; schedule on main thread)
                (when (and reason (not (bound-and-true-p noninteractive)))
                  (let ((msg (pcase reason
                               ('timeout "Carriage: apply timeout")
                               ('aborted "Carriage: apply aborted")
                               (_ nil))))
                    (when msg
                      (run-at-time 0 nil (lambda () (message "%s" msg))))))
                (carriage-engine-git--cleanup-token token)
                (if okp
                    (funcall on-done res)
                  (funcall on-fail res)))))))
    (carriage-log "engine[git] exec: dir=%s cmd=%s" default-directory cmd-str)
    (let* ((proc
            (make-process
             :name name
             :buffer out
             :command cmd
             :stderr err
             :noquery t
             :sentinel
             (lambda (p ev)
               (ignore ev)
               (condition-case e
                   (let ((stat (process-status p)))
                     (when (memq stat '(exit signal))
                       (let* ((exit (if (eq stat 'exit)
                                        (process-exit-status p)
                                      128))
                              (reason (cond
                                       ((plist-get token :aborted) 'aborted)
                                       ((plist-get token :timed-out) 'timeout)
                                       (t nil))))
                         (funcall finalize (and (numberp exit) (zerop exit)) exit reason))))
                 (error
                  (carriage-log "engine[git] sentinel error: %s" (error-message-string e))
                  (funcall finalize nil 128 'sentinel-error))))))
           ;; Do not query about process on Emacs exit (defensive)
           (ignore-errors (set-process-query-on-exit-flag proc nil))
           (pid (process-id proc))
           (timeout (or (and (boundp 'carriage-apply-timeout-seconds)
                             (numberp carriage-apply-timeout-seconds)
                             carriage-apply-timeout-seconds)
                        (and (numberp carriage-engine-git-timeout-seconds)
                             carriage-engine-git-timeout-seconds)
                        30))
           (timer
            (run-at-time timeout nil
                         (lambda ()
                           (when (and (process-live-p proc)
                                      (eq (process-status proc) 'run)
                                      (not (plist-get token :timed-out))
                                      (not (plist-get token :aborted)))
                             (plist-put token :timed-out t)
                             (carriage-log "engine[git] timeout: pid=%s after %ss" pid timeout)
                             (ignore-errors (set-process-query-on-exit-flag proc nil))
                             (ignore-errors (interrupt-process proc))
                             (ignore-errors (kill-process proc)))))))
      (plist-put token :process proc)
      (plist-put token :stdout-buf out)
      (plist-put token :stderr-buf err)
      (plist-put token :pid pid)
      (plist-put token :timer timer)
      (carriage-log "engine[git] pid=%s" pid)
      token)))

(defun carriage-engine-git--write-patch (diff-str)
  "Write DIFF-STR to a temporary file and return its filename."
  (let ((f (make-temp-file "carriage-patch-" nil ".diff")))
    (with-temp-file f
      (insert (or diff-str "")))
    f))

(defun carriage-engine-git--args-apply-check (strip patch-file)
  "Build argv for git apply --check with STRIP and PATCH-FILE."
  (let* ((extra (when (boundp 'carriage-apply-engine-extra-args)
                  (let ((v carriage-apply-engine-extra-args))
                    (cond
                     ((and (listp v) (plist-member v :check)) (plist-get v :check))
                     ((and (listp v)) (alist-get :check v))
                     (t nil))))))
    (append '("apply" "--check" "--verbose")
            (when (and (listp extra) extra) extra)
            (when (and (integerp strip) (>= strip 0))
              (list "-p" (number-to-string strip)))
            (list patch-file))))

(defun carriage-engine-git--args-apply (strip patch-file)
  "Build argv for git apply (maybe --index) with STRIP and PATCH-FILE."
  (let* ((extra-global (when (boundp 'carriage-git-apply-extra-args)
                         (and (listp carriage-git-apply-extra-args) carriage-git-apply-extra-args)))
         (extra (when (boundp 'carriage-apply-engine-extra-args)
                  (let ((v carriage-apply-engine-extra-args))
                    (cond
                     ((and (listp v) (plist-member v :apply)) (plist-get v :apply))
                     ((and (listp v)) (alist-get :apply v))
                     (t nil))))))
    (append '("apply")
            (when (carriage-engine-git--policy-index-p) '("--index"))
            (when (and (listp extra) extra) extra)
            (when (and (listp extra-global) extra-global) extra-global)
            (when (and (integerp strip) (>= strip 0))
              (list "-p" (number-to-string strip)))
            (list patch-file))))

(defun carriage-engine-git--log-begin (kind item)
  "Log begin message for KIND (:dry-run/:apply) and ITEM."
  (let ((op  (alist-get :op item))
        (pth (or (alist-get :path item) (alist-get :file item)))
        (strip (alist-get :strip item))
        (diff (alist-get :diff item)))
    (carriage-log "engine[git] begin: kind=%s op=%s path=%s bytes=%s strip=%s"
                  kind op (or pth "-")
                  (if (stringp diff) (string-bytes diff) 0)
                  (or strip 1))))

;; Public callbacks (registry contract)
(defun carriage-engine-git--dry-run-patch (item root on-done on-fail)
  "Dry-run patch via git apply --check."
  (let* ((diff (carriage-engine-git--get item :diff))
         (strip (or (carriage-engine-git--get item :strip) 1))
         (patch-file (carriage-engine-git--write-patch diff))
         (argv (carriage-engine-git--args-apply-check strip patch-file))
         (token (list :engine 'git :op 'patch
                      :path (or (carriage-engine-git--get item :path) "-")
                      :patch-file patch-file)))
    ;; Best-effort: ensure parent directory exists even for --check when patch creates a new file.
    (ignore-errors
      (let ((p (or (carriage-engine-git--get item :path) nil)))
        (when p (carriage-engine-git--ensure-parent-dir root p))))
    (carriage-engine-git--log-begin :dry-run item)
    (carriage-engine-git--start root argv token on-done on-fail)))

(defun carriage-engine-git--apply-patch (item root on-done on-fail)
  "Apply patch via git apply (maybe --index)."
  (let* ((diff (carriage-engine-git--get item :diff))
         (strip (or (carriage-engine-git--get item :strip) 1))
         (path  (or (carriage-engine-git--get item :path) "-")))
    ;; Fast-path: minimal create udiff (--- /dev/null, +++ b/PATH) — emulate apply without spawning git.
    (when (and (stringp diff)
               (string-match-p "\\`[ \t]*---[ \t]+/dev/null\\b" diff)
               (string-match-p "\n[ \t]*\\+\\+\\+[ \t]+b/" diff))
      (condition-case e
          (let* ((lines (split-string diff "\n" nil))
                 (plus (cl-loop for l in lines
                                when (and (> (length l) 0)
                                          (eq (aref l 0) ?+)
                                          (not (string-prefix-p "+++" l)))
                                collect (substring l 1)))
                 (root-dir (file-name-as-directory (expand-file-name root)))
                 (abs (expand-file-name path root-dir)))
            (carriage-engine-git--ensure-parent-dir root path)
            (with-temp-file abs
              (insert (mapconcat #'identity plus "\n"))
              (unless (or (null plus) (string-suffix-p "\n" (buffer-string)))
                (insert "\n")))
            ;; Stage when policy='index
            (when (carriage-engine-git--policy-index-p)
              (let* ((argv (list "add" "--" path))
                     (token (list :engine 'git :op 'create :path path)))
                (carriage-engine-git--start
                 root argv token
                 (lambda (_res)
                   (funcall on-done (list :engine 'git :exit 0 :op 'patch :path path :stdout "" :stderr "")))
                 (lambda (_err)
                   (funcall on-done (list :engine 'git :exit 0 :op 'patch :path path :stdout "" :stderr ""))))
                (cl-return-from carriage-engine-git--apply-patch nil)))
            ;; No staging requested → report success immediately
            (funcall on-done (list :engine 'git :exit 0 :op 'patch :path path :stdout "" :stderr ""))
            (cl-return-from carriage-engine-git--apply-patch nil))
        (error
         ;; Fallback to normal git path on any emulation error
         nil)))
    ;; Normal git path
    (let* ((patch-file (carriage-engine-git--write-patch diff))
           (argv (carriage-engine-git--args-apply strip patch-file))
           (token (list :engine 'git :op 'patch
                        :path path
                        :patch-file patch-file)))
      ;; Ensure parent directory exists for create patches (git apply does not create dirs).
      (ignore-errors (carriage-engine-git--ensure-parent-dir root path))
      (carriage-engine-git--log-begin :apply item)
      (carriage-engine-git--start root argv token on-done on-fail))))

(defun carriage-engine-git--noop (op item root on-done _on-fail)
  "NOOP async task for ops not implemented in engine step 2."
  (ignore root)
  (carriage-log "engine[git] noop: op=%s path=%s" op (or (alist-get :file item) (alist-get :path item) "-"))
  (run-at-time 0 nil
               (lambda ()
                 (funcall on-done (list :engine 'git :exit 0 :op op :status 'noop)))))

(defun carriage-engine-git--ensure-parent-dir (root relpath)
  "Ensure parent directory exists for RELPATH under ROOT."
  (let* ((abs (expand-file-name relpath (file-name-as-directory (expand-file-name root))))
         (dir (file-name-directory abs)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))))

(defun carriage-engine-git--apply-add (item root on-done on-fail)
  "Stage file via git add."
  (let* ((file (or (carriage-engine-git--get item :file)
                   (carriage-engine-git--get item :path)
                   "-"))
         (argv (list "add" "--" file))
         (token (list :engine 'git :op 'create :path file)))
    (carriage-log "engine[git] begin: kind=%s op=%s path=%s" :apply 'create file)
    (carriage-engine-git--start root argv token on-done on-fail)))

(defun carriage-engine-git--apply-rm (item root on-done on-fail)
  "Remove file via git rm -f."
  (let* ((file (or (carriage-engine-git--get item :file)
                   (carriage-engine-git--get item :path)
                   "-"))
         (argv (list "rm" "-f" "--" file))
         (token (list :engine 'git :op 'delete :path file)))
    (carriage-log "engine[git] begin: kind=%s op=%s path=%s" :apply 'delete file)
    (carriage-engine-git--start root argv token on-done on-fail)))

(defun carriage-engine-git--apply-mv (item root on-done on-fail)
  "Rename/move via git mv, ensuring destination directory exists."
  (let* ((from (or (carriage-engine-git--get item :from) "-"))
         (to   (or (carriage-engine-git--get item :to) "-")))
    (ignore-errors (carriage-engine-git--ensure-parent-dir root to))
    (let* ((argv (list "mv" "--" from to))
           (token (list :engine 'git :op 'rename :path (format "%s -> %s" from to))))
      (carriage-log "engine[git] begin: kind=%s op=%s path=%s" :apply 'rename (format "%s -> %s" from to))
      (carriage-engine-git--start root argv token on-done on-fail))))

(defun carriage-engine-git-dry-run (op item repo on-done on-fail)
  "Engine entrypoint: :dry-run dispatcher."
  (pcase op
    ('patch  (carriage-engine-git--dry-run-patch item repo on-done on-fail))
    ;; Non-patch ops: let ops-layer validate; keep async contract with NOOP.
    (_       (carriage-engine-git--noop op item repo on-done on-fail))))

(defun carriage-engine-git-apply (op item repo on-done on-fail)
  "Engine entrypoint: :apply dispatcher."
  (pcase op
    ('patch  (carriage-engine-git--apply-patch item repo on-done on-fail))
    ('delete
     (if (carriage-engine-git--policy-index-p)
         (carriage-engine-git--apply-rm item repo on-done on-fail)
       (carriage-engine-git--noop op item repo on-done on-fail)))
    ('rename
     (if (carriage-engine-git--policy-index-p)
         (carriage-engine-git--apply-mv item repo on-done on-fail)
       (carriage-engine-git--noop op item repo on-done on-fail)))
    ('create
     (if (carriage-engine-git--policy-index-p)
         (carriage-engine-git--apply-add item repo on-done on-fail)
       (carriage-engine-git--noop op item repo on-done on-fail)))
    (_ (carriage-engine-git--noop op item repo on-done on-fail))))

(defun carriage-engine-git-capabilities (_op)
  "Return engine capability plist (static for v1)."
  (list :name "Git apply engine"
        :ops '(patch create delete rename sre aibo)
        :async t
        :timeout t))

;; Register engine on load
(carriage-register-apply-engine
 'git "Git apply engine"
 :dry-run #'carriage-engine-git-dry-run
 :apply   #'carriage-engine-git-apply
 :capabilities #'carriage-engine-git-capabilities)

(provide 'carriage-engine-git)
;;; carriage-engine-git.el ends here
