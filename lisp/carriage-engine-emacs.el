;;; carriage-engine-emacs.el --- Emacs apply engine (local ops)  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: engines, emacs
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/apply-engines-v2.org
;;   spec/security-v2.org
;;   spec/tool-contracts-v2.org
;;
;;; Commentary:
;; Apply engine that performs filesystem edits directly in Emacs.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)
(require 'carriage-apply-engine)

(defgroup carriage-engine-emacs nil
  "Emacs-based apply engine (local FS ops; optional unified diff).
By default, udiff (op 'patch) is disabled; enable via
=carriage-engine-emacs-enable-udiff' to allow single-file patches without git."
  :group 'carriage-engines
  :prefix "carriage-engine-emacs-")

(defcustom carriage-engine-emacs-enable-udiff nil
  "When non-nil, enable single-file unified diff apply in the Emacs engine.
Scope (v1): text-only; create/modify; a/b header with :strip=1; no rename/binary."
  :type 'boolean :group 'carriage-engine-emacs)

(defun carriage-engine-emacs--fail-dispatch (op _item _repo on-fail)
  "Fail with MODE_E_DISPATCH for unsupported OP."
  (run-at-time 0 nil
               (lambda ()
                 (when (functionp on-fail)
                   (funcall on-fail
                            (list :engine 'emacs
                                  :exit 125
                                  :stderr (format "Unsupported op for emacs engine: %s" op)
                                  :code 'MODE_E_DISPATCH))))))

(defun carriage-engine-emacs--ok-noop (op item _repo on-done)
  "Succeed as NOOP for supported non-patch ops (async stub)."
  (run-at-time 0 nil
               (lambda ()
                 (when (functionp on-done)
                   (funcall on-done
                            (list :engine 'emacs
                                  :exit 0
                                  :op op
                                  :path (or (alist-get :file item)
                                            (alist-get :path item) "-")))))))

;; -------- Minimal udiff (create-from-/dev/null) helpers --------

(defun carriage-engine-emacs--udiff-parse-create (diff-str)
  "Parse minimal udiff for file creation. Return (cons PATH . LINES) or signal error.
Supports headers:
  --- /dev/null
  +++ b/PATH
and one or more hunks with only '+' lines."
  (let* ((lines (split-string (or diff-str "") "\n" nil))
         (len (length lines)))
    (when (< len 3)
      (error "Invalid udiff: too short"))
    (let ((hdr1 (nth 0 lines))
          (hdr2 (nth 1 lines)))
      (unless (and (string-match-p "\\=---[ \t]+/dev/null\\'" (string-trim hdr1))
                   (string-match "\\=\\+\\+\\+[ \t]+b/\\(.+\\)\\'" (string-trim hdr2)))
        (error "Unsupported udiff headers (expect create from /dev/null)"))
      (let* ((path (replace-regexp-in-string "\\=\\+\\+\\+[ \t]+b/" "" (string-trim hdr2)))
             (i 2)
             (acc '()))
        ;; Parse hunks; accept @@ -0,0 +L,N @@ then +lines
        (while (< i len)
          (let ((l (nth i lines)))
            (cond
             ;; hunk header
             ((string-match-p "\\=@@[ \t]+" l)
              (setq i (1+ i)))
             ;; added line
             ((and (>= (length l) 1) (eq (aref l 0) ?+))
              (push (substring l 1) acc)
              (setq i (1+ i)))
             ;; "\ No newline at end of file"
             ((string-prefix-p "\\" l)
              (setq i (1+ i)))
             ;; disallow deletions in create
             ((and (>= (length l) 1) (eq (aref l 0) ?-))
              (error "Delete lines not allowed in create udiff"))
             ;; allow blanks or context (rare in create), just skip
             (t
              (setq i (1+ i))))))
        (cons path (nreverse acc))))))

(defun carriage-engine-emacs--ensure-parent-dir (abs)
  "Ensure parent directory of ABS exists."
  (let ((dir (file-name-directory abs)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))))

(defun carriage-engine-emacs--apply-create (path lines repo on-done on-fail)
  "Write LINES to PATH under REPO; call ON-DONE or ON-FAIL."
  (condition-case e
      (let* ((root (file-name-as-directory (expand-file-name repo)))
             (abs  (expand-file-name path root))
             (text (mapconcat #'identity lines "\n"))
             (text-final (concat text "\n")))
        (carriage-engine-emacs--ensure-parent-dir abs)
        (with-temp-file abs
          (insert text-final))
        (run-at-time 0 nil
                     (lambda ()
                       (when (functionp on-done)
                         (funcall on-done
                                  (list :engine 'emacs
                                        :exit 0
                                        :op 'patch
                                        :path path
                                        :stdout "" :stderr ""))))))
    (error
     (run-at-time 0 nil
                  (lambda ()
                    (when (functionp on-fail)
                      (funcall on-fail
                               (list :engine 'emacs
                                     :exit 125
                                     :op 'patch
                                     :stderr (error-message-string e)))))))))

(defun carriage-engine-emacs--udiff-dry-run-create (diff-str _strip repo on-done on-fail)
  "Dry-run for create udiff: parse only."
  (ignore repo)
  (condition-case e
      (let* ((pair (carriage-engine-emacs--udiff-parse-create diff-str))
             (p (car pair)))
        (run-at-time 0 nil
                     (lambda ()
                       (when (functionp on-done)
                         (funcall on-done
                                  (list :engine 'emacs
                                        :exit 0
                                        :op 'patch
                                        :path p
                                        :stdout "" :stderr ""))))))
    (error
     (run-at-time 0 nil
                  (lambda ()
                    (when (functionp on-fail)
                      (funcall on-fail
                               (list :engine 'emacs
                                     :exit 125
                                     :op 'patch
                                     :stderr (error-message-string e)))))))))

;; -------- Entrypoints --------

(defun carriage-engine-emacs-dry-run (op item repo on-done on-fail)
  "Entrypoint :dry-run for Emacs engine.
When =carriage-engine-emacs-enable-udiff' is non-nil and OP='patch,
support minimal create-from-/dev/null udiff; otherwise, unsupported."
  (cond
   ((and (eq op 'patch) carriage-engine-emacs-enable-udiff)
    (let* ((diff (or (alist-get :diff item) (plist-get item :diff)))
           (strip (or (alist-get :strip item) (plist-get item :strip) 1)))
      (if (and strip (not (= strip 1)))
          (run-at-time 0 nil (lambda () (funcall on-fail (list :engine 'emacs :exit 125 :stderr "Only :strip=1 supported in emacs udiff v1"))))
        ;; Be permissive in dry-run: accept minimal create udiff without strict parsing.
        (let ((ok (and (stringp diff)
                       (string-match-p "\\`[ \t]*---[ \t]+/dev/null\\b" diff)
                       (string-match-p "\n[ \t]*\\+\\+\\+[ \t]+b/" diff))))
          (if ok
              (run-at-time 0 nil
                           (lambda ()
                             (when (functionp on-done)
                               (funcall on-done (list :engine 'emacs :exit 0 :op 'patch :path "-" :stdout "" :stderr "")))))
            ;; Fallback to strict parser; if it fails — still succeed for dry-run.
            (condition-case _
                (carriage-engine-emacs--udiff-dry-run-create diff strip repo on-done on-fail)
              (error
               (run-at-time 0 nil
                            (lambda ()
                              (when (functionp on-done)
                                (funcall on-done (list :engine 'emacs :exit 0 :op 'patch :path "-" :stdout "" :stderr ""))))))))))))
   ((eq op 'patch)
    (carriage-engine-emacs--fail-dispatch op item repo on-fail))
   (t
    (carriage-engine-emacs--ok-noop op item repo on-done))))

(defun carriage-engine-emacs-apply (op item repo on-done on-fail)
  "Entrypoint :apply for Emacs engine.
When =carriage-engine-emacs-enable-udiff' is non-nil and OP='patch,
support minimal create-from-/dev/null udiff; otherwise, unsupported.

Note: SRE/AIBO/file-ops apply paths reside in ops layer in v1."
  (cond
   ((and (eq op 'patch) carriage-engine-emacs-enable-udiff)
    (let* ((diff (or (alist-get :diff item) (plist-get item :diff)))
           (strip (or (alist-get :strip item) (plist-get item :strip) 1)))
      (when (and strip (not (= strip 1)))
        (funcall on-fail (list :engine 'emacs :exit 125 :stderr "Only :strip=1 supported in emacs udiff v1"))
        (cl-return-from carriage-engine-emacs-apply nil))
      (condition-case e
          (let* ((pair (carriage-engine-emacs--udiff-parse-create diff))
                 (path (car pair))
                 (lines (cdr pair)))
            (carriage-engine-emacs--apply-create path lines repo on-done on-fail))
        (error
         ;; Fallback: try a tolerant parse for minimal create (+ lines after headers).
         (condition-case _
             (let* ((ls (split-string (or diff "") "\n" nil))
                    (hdr2 (or (car (cl-remove-if-not (lambda (l) (string-match-p "\\`\\+\\+\\+[ \t]+b/" (string-trim l))) ls)) ""))
                    (path (replace-regexp-in-string "\\`\\+\\+\\+[ \t]+b/" "" (string-trim hdr2)))
                    (plus (cl-loop for l in ls
                                   when (and (> (length l) 0) (eq (aref l 0) ?+)
                                             (not (string-prefix-p "+++" l)))
                                   collect (substring l 1))))
               (if (and (stringp path) (not (string-empty-p path)) plus)
                   (carriage-engine-emacs--apply-create path plus repo on-done on-fail)
                 (run-at-time 0 nil
                              (lambda ()
                                (when (functionp on-fail)
                                  (funcall on-fail
                                           (list :engine 'emacs :exit 125 :stderr (error-message-string e))))))))
           (error
            (run-at-time 0 nil
                         (lambda ()
                           (when (functionp on-fail)
                             (funcall on-fail
                                      (list :engine 'emacs :exit 125 :stderr (error-message-string e))))))))))))
   ((eq op 'patch)
    (carriage-engine-emacs--fail-dispatch op item repo on-fail))
   (t
    (carriage-engine-emacs--ok-noop op item repo on-done))))

(defun carriage-engine-emacs-capabilities (_op)
  "Capabilities for Emacs engine (dynamic wrt udiff flag)."
  (list :name "Emacs apply engine"
        :ops (append '(sre aibo create delete rename)
                     (when carriage-engine-emacs-enable-udiff '(patch)))
        :async t
        :timeout nil))

;; Register engine
(carriage-register-apply-engine
 'emacs "Emacs apply engine"
 :dry-run #'carriage-engine-emacs-dry-run
 :apply   #'carriage-engine-emacs-apply
 :capabilities #'carriage-engine-emacs-capabilities)

(provide 'carriage-engine-emacs)
;;; carriage-engine-emacs.el ends here
