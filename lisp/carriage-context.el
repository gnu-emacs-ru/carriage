;;; carriage-context.el --- Context collector and formatter  -*- lexical-binding: t; -*-
;;
;; (file body unchanged below)
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: tools, context
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/context-integration-v2.org
;;   spec/logging-v2.org
;;   spec/security-v2.org
;;   spec/observability-v2.org
;;   spec/data-structures-v2.org
;;   spec/rag-indexing-v2.org
;;
;;; Commentary:
;; Collects context files and document paths for inclusion into LLM requests,
;; formats context snippets, and provides a lightweight cache for file reads.
;;
;;; Code:
(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-project-root "carriage-utils" ())
(declare-function carriage--call-git "carriage-utils" (default-dir &rest args))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org" (&optional arg invisible-ok to-heading))
(declare-function org-up-heading-safe "org" (&optional arg))

(defgroup carriage-context nil
  "Context collection and formatting for Carriage."
  :group 'applications
  :prefix "carriage-")

(defcustom carriage-context-debug nil
  "When non-nil, emit debug logs for context collection/counting."
  :type 'boolean
  :group 'carriage-context)

(defcustom carriage-context-file-cache-ttl 5.0
  "TTL in seconds for cached file contents used during context collection.
When nil, cache entries are considered valid until file size or mtime changes."
  :type '(choice (const :tag "Unlimited (until file changes)" nil) number)
  :group 'carriage-context)

(defcustom carriage-context-project-map-cache-ttl 2.0
  "TTL in seconds for cached Project Map (begin_map) generation.
The map is computed via `git ls-files -co --exclude-standard' and is only used
during request building (never on redisplay)."
  :type 'number
  :group 'carriage-context)

(defcustom carriage-context-project-map-git-timeout-seconds 1.5
  "Timeout in seconds for Project Map's git-based file listing.

This is intentionally shorter than the global git timeout, because Project Map
is an optional context add-on and MUST not stall UI for long."
  :type 'number
  :group 'carriage-context)

(defcustom carriage-context-project-map-fd-timeout-seconds 1.5
  "Timeout in seconds for Project Map's `fd` fallback file listing."
  :type 'number
  :group 'carriage-context)

(defcustom carriage-context-project-map-emacs-walk-max-seconds 0.8
  "Soft time budget in seconds for the Emacs directory-walk fallback.

When the budget is exceeded, traversal stops and the map is truncated deterministically."
  :type 'number
  :group 'carriage-context)

(defcustom carriage-context-project-map-skip-dirs
  '(".git" ".hg" ".svn" "node_modules" ".venv" "venv" "__pycache__" "dist" "build" "target")
  "Directory base-names to skip in the Emacs directory-walk fallback."
  :type '(repeat string)
  :group 'carriage-context)

(defcustom carriage-context-project-map-max-bytes 200000
  "Maximum number of bytes allowed for the rendered Project Map (begin_map) block.
When exceeded, the block is truncated deterministically and a tail marker is added."
  :type 'integer
  :group 'carriage-context)

(defcustom carriage-context-project-map-max-paths 10000
  "Maximum number of repo-relative paths included in the Project Map (begin_map) tree.
When exceeded, extra paths are omitted deterministically."
  :type 'integer
  :group 'carriage-context)

(defcustom carriage-visible-ignore-modes
  '(exwm-mode image-mode pdf-view-mode doc-view-mode dired-mode help-mode special-mode
              context-navigator-view-mode test-flow-panel-mode test-flow-status-mode)
  "List of major-modes to ignore when collecting visible buffers."
  :type '(repeat symbol)
  :group 'carriage-context)

(defcustom carriage-visible-ignore-buffer-regexps
  '("^\\*carriage-" "^\\*Warnings\\*\\'" "^\\*Compile-Log\\*\\'" "^\\*Help\\*\\'" "^\\*Backtrace\\*\\'")
  "Regexps for buffer names to ignore when collecting visible buffers."
  :type '(repeat string)
  :group 'carriage-context)

(defcustom carriage-visible-terminal-tail-lines 256
  "Number of last lines to include for terminal/comint/messages-like buffers."
  :type 'integer
  :group 'carriage-context)

(defcustom carriage-visible-exclude-current-buffer t
  "When non-nil, exclude the buffer that initiated context collection from the 'visible source.
This helps avoid self-duplication and reduces noise/budget usage."
  :type 'boolean
  :group 'carriage-context)

(defcustom carriage-doc-context-scope 'all
  "Scope for document (#+begin_context) collection: 'all or 'last.
When 'all, collect paths from all #+begin_context blocks in the buffer.
When 'last, collect paths only from the last #+begin_context block in the buffer
(always the last block in the buffer; independent of point)."
  :type '(choice (const all) (const last))
  :group 'carriage-context)
(make-variable-buffer-local 'carriage-doc-context-scope)

(defcustom carriage-mode-include-patched-files nil
  "When non-nil, include files referenced by #+begin_patch blocks in the current buffer into the document context.

This source is about including the *current contents of files* referenced by patch
headers (subject to limits). Patch block bodies are never used as prompt context."
  :type 'boolean
  :group 'carriage-context)
(make-variable-buffer-local 'carriage-mode-include-patched-files)

(defvar-local carriage-context--doc-paths-cache nil
  "Cache of doc-context paths for the current buffer.
Plist keys:
  :tick  — `buffer-chars-modified-tick' at the time of computation
  :scope — value of `carriage-doc-context-scope'
  :paths — list of path strings extracted from doc context blocks.

This cache exists to make context counting cheap when UI refreshes frequently.
It is invalidated automatically when the buffer changes (tick changes) or when scope changes.")

(defvar-local carriage-context--patched-files-cache nil
  "Cache of patched-files paths (from applied begin_patch blocks) for the current buffer.
Plist keys:
  :paths — list of path strings extracted from patches in current buffer.")

(defvar-local carriage-context--patched-files-dirty t
  "When non-nil, patched-files cache must be recomputed for the current buffer.

Important perf invariant:
- We do NOT want patched-files collection to depend on `buffer-chars-modified-tick',
  because ordinary typing changes tick constantly and would force O(buffer) rescans.
- Instead, we mark dirty only when edits touch begin_patch/end_patch lines.")

(defun carriage-context--patched-files-mark-dirty (beg end _len)
  "Mark patched-files cache dirty if edit touches begin_patch/end_patch lines.

This function is designed to be O(1) per edit."
  (when (and (number-or-marker-p beg) (number-or-marker-p end))
    (save-excursion
      (let ((case-fold-search t)
            (hit nil))
        (goto-char beg)
        (beginning-of-line)
        (setq hit (or (looking-at-p "^[ \t]*#\\+begin_patch\\b")
                      (looking-at-p "^[ \t]*#\\+end_patch\\b")))
        (unless hit
          (goto-char end)
          (beginning-of-line)
          (setq hit (or (looking-at-p "^[ \t]*#\\+begin_patch\\b")
                        (looking-at-p "^[ \t]*#\\+end_patch\\b"))))
        (when hit
          (setq carriage-context--patched-files-dirty t))))))

(add-hook 'carriage-mode-hook
          (lambda ()
            ;; Buffer-local hook; O(1) per edit and only marks dirty when patch lines touched.
            (add-hook 'after-change-functions
                      #'carriage-context--patched-files-mark-dirty
                      nil t)))

;;;###autoload
(defun carriage-toggle-include-patched-files ()
  "Toggle inclusion of files referenced by applied begin_patch blocks (:applied t) for this buffer."
  (interactive)
  (setq-local carriage-mode-include-patched-files (not carriage-mode-include-patched-files))
  ;; Invalidate caches so [Ctx:N] and the modeline reflect changes immediately.
  (when (fboundp 'carriage-ui--reset-context-cache)
    (carriage-ui--reset-context-cache))
  (when (fboundp 'carriage-ui--invalidate-ml-cache)
    (carriage-ui--invalidate-ml-cache))
  (force-mode-line-update))

(defun carriage-context--patched-files (buffer)
  "Return list of file paths referenced by #+begin_patch headers in BUFFER.

Semantics (Variant A):
- Includes files referenced by *all* patch blocks in the document, regardless of :applied.
- The purpose is to include the current contents of these files as external context
  when `carriage-mode-include-patched-files' is enabled.

Path extraction rules by :op:
- patch  → :path
- rename → :from and :to
- other  → :file  (create/delete/sre/aibo/replace)

Returns a deduplicated list of non-empty strings, in buffer order.
Uses a lightweight cache and a dirty flag to avoid rescanning the buffer on every edit."
  (with-current-buffer buffer
    (if (and (not carriage-context--patched-files-dirty)
             (listp carriage-context--patched-files-cache)
             (listp (plist-get carriage-context--patched-files-cache :paths)))
        (plist-get carriage-context--patched-files-cache :paths)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t)
              (acc '()))
          (while (re-search-forward "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" nil t)
            (let* ((sexp-str (match-string 1))
                   (plist (condition-case _e
                              (car (read-from-string sexp-str))
                            (error nil)))
                   (op (and (listp plist) (plist-get plist :op)))
                   (opstr (and op (replace-regexp-in-string "^:" "" (format "%s" op)))))
              (when (listp plist)
                (pcase opstr
                  ("patch"
                   (let ((p (plist-get plist :path)))
                     (when (and (stringp p) (not (string-empty-p p)))
                       (push p acc))))
                  ("rename"
                   (dolist (k '(:from :to))
                     (let ((p (plist-get plist k)))
                       (when (and (stringp p) (not (string-empty-p p)))
                         (push p acc)))))
                  (_
                   (let ((p (plist-get plist :file)))
                     (when (and (stringp p) (not (string-empty-p p)))
                       (push p acc))))))))
          (setq acc (nreverse (delete-dups acc)))
          (setq carriage-context--patched-files-cache (list :paths acc))
          (setq carriage-context--patched-files-dirty nil)
          acc)))))

;; Commands to switch scope (used by UI/keyspec)
;;;###autoload
(defun carriage-select-doc-context-all ()
  "Use all #+begin_context blocks for document context in this buffer."
  (interactive)
  (setq-local carriage-doc-context-scope 'all)
  ;; Make Ctx badge reflect scope change immediately (no waiting for 1Hz refresh).
  (when (fboundp 'carriage-ui--ctx-invalidate)
    (ignore-errors (carriage-ui--ctx-invalidate)))
  (when (fboundp 'carriage-ui--invalidate-ml-cache)
    (ignore-errors (carriage-ui--invalidate-ml-cache)))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-select-doc-context-last ()
  "Use only the last #+begin_context block for document context in this buffer."
  (interactive)
  (setq-local carriage-doc-context-scope 'last)
  ;; Make Ctx badge reflect scope change immediately (no waiting for 1Hz refresh).
  (when (fboundp 'carriage-ui--ctx-invalidate)
    (ignore-errors (carriage-ui--ctx-invalidate)))
  (when (fboundp 'carriage-ui--invalidate-ml-cache)
    (ignore-errors (carriage-ui--invalidate-ml-cache)))
  (force-mode-line-update t))

;;;###autoload
(defun carriage-toggle-doc-context-scope ()
  "Toggle document context scope between 'all and 'last for this buffer."
  (interactive)
  (setq-local carriage-doc-context-scope
              (if (eq carriage-doc-context-scope 'last) 'all 'last))
  (when (fboundp 'carriage-ui--reset-context-cache)
    (ignore-errors (carriage-ui--reset-context-cache)))
  (when (fboundp 'carriage-ui--invalidate-ml-cache)
    (ignore-errors (carriage-ui--invalidate-ml-cache)))
  (force-mode-line-update)
  (message "Doc context scope: %s" (if (eq carriage-doc-context-scope 'last) "last" "all")))

(defvar carriage-context--normalize-cache (make-hash-table :test 'equal)
  "Memo table for carriage-context--normalize-path keyed by (ROOT . PATH).")

(defvar carriage-context--file-cache (make-hash-table :test 'equal)
  "Cache of file reads keyed by truename.
Each value is a plist: (:mtime MT :size SZ :time TS :ok BOOL :data STRING-OR-REASON).")

(defvar carriage-context--root-tru-cache (make-hash-table :test 'equal)
  "Cache mapping project ROOT → truenamed directory (with trailing slash).")

(defun carriage-context--dbg (fmt &rest args)
  "Internal debug logger for context layer (respects =carriage-context-debug')."
  (when carriage-context-debug
    (condition-case _
        (if (require 'carriage-logging nil t)
            (apply #'carriage-log (concat "Context: " fmt) args)
          (apply #'message (concat "[carriage-context] " fmt) args))
      (error nil))))

(defcustom carriage-context-project-map-trace nil
  "When non-nil, emit detailed logs for Project Map generation (begin_map).

This is intended for troubleshooting why begin_map is empty or slow.
Logs are written via `carriage-log' when available; never signals."
  :type 'boolean
  :group 'carriage-context)

(defvar carriage-context--project-map-last-debug nil
  "Last Project Map debug plist (best-effort).

This is a troubleshooting aid. Typical keys:
  :ts :fn :root :elapsed-ms :kind :len :note")

(defun carriage-context--pm-log (fmt &rest args)
  "Log Project Map diagnostics. Never signals."
  (condition-case _
      (when (require 'carriage-logging nil t)
        (apply #'carriage-log (concat "ProjectMap: " fmt) args))
    (error nil)))

(defun carriage-context--pm-note (plist &optional note)
  "Store PLIST as last Project Map debug info, optionally with NOTE."
  (setq carriage-context--project-map-last-debug
        (append plist
                (when note (list :note note))
                (list :ts (float-time))))
  carriage-context--project-map-last-debug)

(defun carriage-context--pm--len (x)
  "Best-effort length metric for debug logs."
  (cond
   ((null x) 0)
   ((stringp x) (length x))
   ((listp x) (length x))
   ((hash-table-p x) (hash-table-count x))
   (t 1)))

(defun carriage-context--project-root ()
  "Return project root directory, or default-directory."
  (or (and (fboundp 'carriage-project-root) (carriage-project-root))
      default-directory))

(defun carriage-context--inside-root-p (truename root)
  "Return non-nil if TRUENAME lies within ROOT.
Assumes TRUENAME is already a truename; avoids re-normalizing it.
Uses a small memo to avoid repeated (file-truename root) calls."
  (let* ((rt (or (gethash root carriage-context--root-tru-cache)
                 (let ((v (file-name-as-directory (file-truename root))))
                   (puthash root v carriage-context--root-tru-cache)
                   v)))
         (pt (file-name-as-directory truename)))
    (string-prefix-p rt pt)))

(defun carriage-context--normalize-path (path root)
  "Normalize PATH relative to ROOT; reject unsafe/TRAMP paths.
Paths outside ROOT are allowed (for context collection only); REL is the absolute truename.
Apply pipeline still enforces project-root boundaries (see `carriage-normalize-path' and spec/security-v2.org).
Return cons (ok . (rel . truename)) or (nil . reason-symbol). Uses memoization."
  (let* ((key (cons root path))
         (hit (and carriage-context--normalize-cache
                   (gethash key carriage-context--normalize-cache))))
    (if hit
        hit
      (let ((res
             (cond
              ((or (null path) (string-empty-p path))
               (cons nil 'empty))
              ((file-remote-p path)
               (cons nil 'remote))
              (t
               (let* ((abs (if (file-name-absolute-p path)
                               path
                             (expand-file-name path root)))
                      (true (ignore-errors (file-truename abs))))
                 (cond
                  ((null true) (cons nil 'unresolvable))
                  (t
                   (let* ((rel (if (carriage-context--inside-root-p true root)
                                   (file-relative-name true root)
                                 true)))
                     (cons t (cons rel true))))))))))
        (puthash key res carriage-context--normalize-cache)
        res))))

(defun carriage-context--read-file-safe (truename)
  "Read file contents from TRUENAME; return (ok . string-or-reason).
Uses a small cache with TTL and invalidation by file size/mtime."
  (let* ((attrs (ignore-errors (file-attributes truename)))
         (mtime (and attrs (nth 5 attrs)))
         (size  (and attrs (nth 7 attrs)))
         (now   (float-time))
         (ttl   carriage-context-file-cache-ttl)
         (ce    (and carriage-context--file-cache (gethash truename carriage-context--file-cache)))
         (fresh (and ce
                     (equal (plist-get ce :mtime) mtime)
                     (equal (plist-get ce :size) size)
                     (or (null ttl)
                         (< (- now (or (plist-get ce :time) 0)) (or ttl 0))))))
    (if fresh
        (let ((ok (plist-get ce :ok))
              (data (plist-get ce :data)))
          (if ok (cons t data) (cons nil data)))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents truename)
            (let ((s (buffer-substring-no-properties (point-min) (point-max))))
              ;; crude binary check: NUL-byte
              (let* ((ok (not (string-match-p "\0" s)))
                     (data (if ok s 'binary)))
                (puthash truename
                         (list :mtime mtime :size size :time (float-time)
                               :ok ok :data data)
                         carriage-context--file-cache)
                (if ok (cons t s) (cons nil 'binary)))))
        (error
         (let ((reason (format "read-error:%s" (error-message-string err))))
           (puthash truename
                    (list :mtime mtime :size size :time (float-time)
                          :ok nil :data reason)
                    carriage-context--file-cache)
           (cons nil reason)))))))

(defun carriage-context--find-context-block-in-region (beg end)
  "Return list of path lines found between #+begin_context and #+end_context within BEG..END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((paths '()))
        (while (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t)
          (let ((block-beg (line-end-position)))
            (if (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
                (let ((block-end (line-beginning-position)))
                  (save-excursion
                    (goto-char block-beg)
                    (while (< (point) block-end)
                      (let ((ln (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position))))
                        (unless (or (string-match-p "^[ \t]*\\(#\\|$\\)" ln))
                          (push (string-trim ln) paths)))
                      (forward-line 1))))
              ;; no end marker, consume till end
              (save-excursion
                (goto-char block-beg)
                (while (not (eobp))
                  (let ((ln (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
                    (unless (or (string-match-p "^[ \t]*\\(#\\|$\\)" ln))
                      (push (string-trim ln) paths)))
                  (forward-line 1))))))
        (nreverse (let ((res (delete-dups (delq nil paths))))
                    (carriage-context--dbg "doc-paths: %s (first=%s)" (length res) (car res))
                    res))))))

(defun carriage-context--doc-paths (buffer)
  "Collect paths from #+begin_context blocks in BUFFER per `carriage-doc-context-scope'.

Scope rules:
- 'all  (default): collect paths from all #+begin_context blocks in the buffer.
- 'last: collect paths only from the LAST #+begin_context block in the buffer
         (tail-most; independent of point)."
  (with-current-buffer buffer
    (save-excursion
      (let* ((scope (or (and (boundp 'carriage-doc-context-scope)
                             carriage-doc-context-scope)
                        'all))
             (tick (buffer-chars-modified-tick))
             (cache carriage-context--doc-paths-cache)
             (cache-ok (and (listp cache)
                            (eq (plist-get cache :scope) scope)
                            (numberp (plist-get cache :tick))
                            (= (plist-get cache :tick) tick)
                            (listp (plist-get cache :paths)))))
        (if cache-ok
            (plist-get cache :paths)
          (let ((paths
                 (pcase scope
                   ('all
                    (delete-dups
                     (delq nil
                           (carriage-context--find-context-block-in-region (point-min) (point-max)))))
                   (_
                    ;; Tail-most block in the buffer (independent of point).
                    ;; Optimization: scan from end to find the last begin_context.
                    (let (last-beg last-end)
                      (save-excursion
                        (goto-char (point-max))
                        (let ((case-fold-search t))
                          (when (re-search-backward "^[ \t]*#\\+begin_context\\b" nil t)
                            (setq last-beg (match-beginning 0))
                            (goto-char last-beg)
                            (setq last-end
                                  (if (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
                                      (line-end-position)
                                    (point-max))))))
                      (if (and (numberp last-beg) (numberp last-end) (> last-end last-beg))
                          (delete-dups
                           (delq nil
                                 (carriage-context--find-context-block-in-region last-beg last-end)))
                        '()))))))
            (setq carriage-context--doc-paths-cache
                  (list :tick tick :scope scope :paths paths))
            paths))))))


(defun carriage-context--maybe-gptel-files ()
  "Collect absolute file paths from gptel context (best-effort).

- Prefer =gptel-context--collect' (when available from gptel-context.el).
- Fallback to =gptel-context' variable when the collector is unavailable.
- For buffer entries include only buffers visiting a file (BUFFER_FILE_NAME).
- Ignore non-existent/TRAMP/unknown sources here; higher-level filters apply."
  (let ((files '()))
    (condition-case _e
        (progn
          ;; Preferred path: use gptel-context--collect if present
          (when (require 'gptel-context nil t)
            (when (fboundp 'gptel-context--collect)
              (dolist (entry (gptel-context--collect))
                (pcase-let* ((`(,src . ,_props) (ensure-list entry)))
                  (cond
                   ((stringp src)
                    (push src files))
                   ((bufferp src)
                    (with-current-buffer src
                      (when buffer-file-name
                        (push buffer-file-name files)))))))))
          ;; Fallback: walk gptel-context alist if nothing was gathered
          (when (and (null files) (boundp 'gptel-context))
            (dolist (entry (symbol-value 'gptel-context))
              (pcase-let* ((`(,src . ,_props) (ensure-list entry)))
                (cond
                 ((stringp src)
                  (push src files))
                 ((bufferp src)
                  (with-current-buffer src
                    (when buffer-file-name
                      (push buffer-file-name files)))))))))
      (error nil))
    (let* ((res (delete-dups (cl-remove-if-not #'file-exists-p files))))
      (carriage-context--dbg "gptel-files: %s %s" (length res) (car res))
      res)))

;; Internal helpers for context collection and counting

(defun carriage-context--include-flags (buf)
  "Return plist with include toggles for BUF: (:gptel BOOL :doc BOOL :visible BOOL).
Defaults: gptel/doc ON when variables are unbound; visible OFF by default."
  (with-current-buffer buf
    (list
     :gptel (if (boundp 'carriage-mode-include-gptel-context)
                (buffer-local-value 'carriage-mode-include-gptel-context buf)
              nil)
     :doc   (if (boundp 'carriage-mode-include-doc-context)
                (buffer-local-value 'carriage-mode-include-doc-context buf)
              t)
     :visible (and (boundp 'carriage-mode-include-visible-context)
                   (buffer-local-value 'carriage-mode-include-visible-context buf)))))

(defun carriage-context--limits (buf)
  "Return plist with limits for BUF: (:max-files N :max-bytes N)."
  (with-current-buffer buf
    (list
     :max-files (or (and (boundp 'carriage-mode-context-max-files)
                         (buffer-local-value 'carriage-mode-context-max-files buf))
                    100)
     :max-bytes (or (and (boundp 'carriage-mode-context-max-total-bytes)
                         (buffer-local-value 'carriage-mode-context-max-total-bytes buf))
                    (* 1024 1024)))))

(defun carriage-context--gather-candidates (buf include-doc include-gptel)
  "Gather and deduplicate candidate paths from BUF according to toggles.
Order of preference MUST be doc > gptel. Visible is handled separately."
  (let* ((doc (when include-doc (carriage-context--doc-paths buf)))
         (gpf (when include-gptel (carriage-context--maybe-gptel-files)))
         ;; Prefer doc first, then gptel to align with INV-ctx-001 (doc > visible > gptel).
         (cands (delete-dups (append (or doc '()) (or gpf '())))))
    (carriage-context--dbg "collect: doc-paths=%s gptel-files=%s candidates=%s"
                           (and doc (length doc)) (and gpf (length gpf)) (length cands))
    cands))

(defun carriage-context--normalize-candidate (path root)
  "Normalize PATH relative to ROOT and return plist:
  (:ok BOOL :rel REL :true TRUE :reason SYMBOL|nil)."
  (let ((norm (carriage-context--normalize-path path root)))
    (if (car norm)
        (list :ok t :rel (cadr norm) :true (cddr norm))
      (list :ok nil :reason (cdr norm)))))

(defun carriage-context--unique-truenames-under-root (paths root)
  "Return a deduped list of truenames from PATHS (inside or outside ROOT)."
  (let ((acc '()))
    (dolist (p paths)
      (let* ((norm (carriage-context--normalize-candidate p root)))
        (when (plist-get norm :ok)
          (push (plist-get norm :true) acc))))
    (delete-dups acc)))

(defun carriage-context--source-counts-from-items (items)
  "Count items by :source and return list: ((doc . N) (gptel . N) (both . N) (visible . N))."
  (let ((d 0) (g 0) (b 0) (v 0))
    (dolist (it items)
      (pcase (plist-get it :source)
        ('doc (setq d (1+ d)))
        ('gptel (setq g (1+ g)))
        ('both (setq b (1+ b)))
        ('visible (setq v (1+ v)))
        (_ nil)))
    (list (cons 'doc d) (cons 'gptel g) (cons 'both b) (cons 'visible v))))

(defun carriage-context--collect-config (buf root)
  "Build config plist for collection for BUF and ROOT.
Keys: :root :include-gptel :include-doc :include-visible :max-files :max-bytes."
  (let* ((r (or root (carriage-context--project-root)))
         (flags (carriage-context--include-flags buf))
         (lims (carriage-context--limits buf)))
    (list :root r
          :include-gptel (plist-get flags :gptel)
          :include-doc (plist-get flags :doc)
          :include-visible (plist-get flags :visible)
          :max-files (plist-get lims :max-files)
          :max-bytes (plist-get lims :max-bytes))))

(defun carriage-context--collect-init-state (config)
  "Initial mutable state plist for collection based on CONFIG.
Keys: :files :warnings :seen :total-bytes :included :skipped
and the CONFIG keys are copied in as well."
  (let ((seen (make-hash-table :test 'equal)))
    (append (list :files '()
                  :warnings '()
                  :seen seen
                  :total-bytes 0
                  :included 0
                  :skipped 0)
            config)))

(defun carriage-context--state-under-file-limit-p (state)
  "Return non-nil if we can still consider more files given STATE limits."
  (< (+ (plist-get state :included)
        (plist-get state :skipped))
     (plist-get state :max-files)))

(defun carriage-context--state-mark-seen (true state)
  "Mark TRUE as seen in STATE. Return non-nil if it was not seen before."
  (let ((seen (plist-get state :seen)))
    (unless (gethash true seen)
      (puthash true t seen)
      t)))

(defun carriage-context--push-warning (msg state)
  "Push warning MSG into STATE."
  (plist-put state :warnings (cons msg (plist-get state :warnings))))

(defun carriage-context--push-file (entry state)
  "Push file ENTRY plist into STATE."
  (plist-put state :files (cons entry (plist-get state :files))))

(defun carriage-context--collect-process-path (p state)
  "Process a candidate path P and update STATE accordingly.
Returns updated STATE."
  (let* ((root (plist-get state :root))
         (max-bytes (plist-get state :max-bytes))
         (norm (carriage-context--normalize-candidate p root)))
    (if (not (plist-get norm :ok))
        (progn
          (setq state (plist-put state :skipped (1+ (plist-get state :skipped))))
          (setq state (carriage-context--push-warning
                       (format "skip %s: %s" p (plist-get norm :reason))
                       state))
          (carriage-context--dbg "collect: skip %s → %s" p (plist-get norm :reason))
          state)
      (let* ((rel (plist-get norm :rel))
             (true (plist-get norm :true)))
        (if (not (carriage-context--state-mark-seen true state))
            state
          (let* ((attrs (ignore-errors (file-attributes true)))
                 (sb0 (and attrs (nth 7 attrs)))
                 (total (plist-get state :total-bytes)))
            ;; If we know the file size and it would exceed the budget, skip reading content.
            (if (and (numberp sb0) (> (+ total sb0) max-bytes))
                (progn
                  (setq state (carriage-context--push-warning
                               (format "limit reached, include path only: %s" rel)
                               state))
                  (setq state (carriage-context--push-file
                               (list :rel rel :true true :content nil :reason 'size-limit)
                               state))
                  (setq state (plist-put state :skipped (1+ (plist-get state :skipped))))
                  (carriage-context--dbg "collect: size-limit (pre) for %s (sb=%s total=%s)" rel sb0 total)
                  state)
              ;; Otherwise read (cached) content and re-check with actual bytes.
              (let* ((rd (carriage-context--read-file-safe true)))
                (if (not (car rd))
                    (progn
                      (setq state (carriage-context--push-file
                                   (list :rel rel :true true :content nil :reason (cdr rd))
                                   state))
                      (setq state (plist-put state :skipped (1+ (plist-get state :skipped))))
                      (carriage-context--dbg "collect: omit %s reason=%s" rel (cdr rd))
                      state)
                  (let* ((s (cdr rd))
                         (sb (string-bytes s))
                         (total2 (plist-get state :total-bytes)))
                    (if (> (+ total2 sb) max-bytes)
                        (progn
                          (setq state (carriage-context--push-warning
                                       (format "limit reached, include path only: %s" rel)
                                       state))
                          (setq state (carriage-context--push-file
                                       (list :rel rel :true true :content nil :reason 'size-limit)
                                       state))
                          (setq state (plist-put state :skipped (1+ (plist-get state :skipped))))
                          (carriage-context--dbg "collect: size-limit for %s (sb=%s total=%s)" rel sb total2)
                          state)
                      (setq state (plist-put state :total-bytes (+ total2 sb)))
                      (setq state (carriage-context--push-file
                                   (list :rel rel :true true :content s)
                                   state))
                      (setq state (plist-put state :included (1+ (plist-get state :included))))
                      (carriage-context--dbg "collect: include %s (bytes=%s total=%s)" rel sb (+ total2 sb))
                      state)))))))))))

(defun carriage-context--collect-finalize (state)
  "Finalize STATE into result plist compatible with carriage-context-collect."
  (let ((files (nreverse (plist-get state :files)))
        (warnings (nreverse (plist-get state :warnings)))
        (skipped (plist-get state :skipped))
        (included (plist-get state :included))
        (total-bytes (plist-get state :total-bytes)))
    (carriage-context--dbg "collect: done files=%s included=%s skipped=%s total-bytes=%s warnings=%s"
                           (length files) included skipped total-bytes (length warnings))
    (list :files files
          :warnings warnings
          :omitted skipped
          :stats (list :total-bytes total-bytes :included included :skipped skipped))))

(defun carriage-context--collect-iterate (candidates state)
  "Iterate over CANDIDATES updating STATE until limits are reached.
Return updated STATE."
  (dolist (p candidates state)
    (when (carriage-context--state-under-file-limit-p state)
      (setq state (carriage-context--collect-process-path p state)))))

(defun carriage-context--collect-exec (buf config)
  "Run collection for BUF using CONFIG and return the finalized result plist."
  (let* ((root (plist-get config :root))
         (include-gptel (plist-get config :include-gptel))
         (include-doc (plist-get config :include-doc))
         (include-visible (plist-get config :include-visible))
         (max-files (plist-get config :max-files))
         (max-bytes (plist-get config :max-bytes))
         (state (carriage-context--collect-init-state config)))
    (carriage-context--dbg "collect: root=%s include{gptel=%s,doc=%s,vis=%s} limits{files=%s,bytes=%s}"
                           root include-gptel include-doc include-visible max-files max-bytes)
    ;; 1a) Patched files (independent toggle; treated as 'doc' source for precedence)
    (when (and (boundp 'carriage-mode-include-patched-files)
               carriage-mode-include-patched-files
               (carriage-context--state-under-file-limit-p state))
      (let ((pat-cands (ignore-errors (carriage-context--patched-files buf))))
        (when pat-cands
          (setq state (carriage-context--collect-iterate pat-cands state)))))
    ;; 1b) DOC candidates (highest preference among primary sources)
    (when (and include-doc (carriage-context--state-under-file-limit-p state))
      (let ((doc-cands (ignore-errors (carriage-context--doc-paths buf))))
        (when doc-cands
          (setq state (carriage-context--collect-iterate doc-cands state)))))
    ;; 2) Visible buffers (files + non-file buffers) second
    (when (and include-visible (carriage-context--state-under-file-limit-p state))
      (let ((seen (make-hash-table :test 'eq))
            (ignored-modes (and (boundp 'carriage-visible-ignore-modes) carriage-visible-ignore-modes))
            (ignored-names (and (boundp 'carriage-visible-ignore-buffer-regexps) carriage-visible-ignore-buffer-regexps))
            (tail (or (and (boundp 'carriage-visible-terminal-tail-lines) carriage-visible-terminal-tail-lines) 256)))
        (walk-windows
         (lambda (w)
           (let ((b (window-buffer w)))
             (unless (gethash b seen)
               (puthash b t seen)
               (with-current-buffer b
                 (let* ((nm (buffer-name b))
                        (mm major-mode)
                        (skip
                         (or (and (boundp 'carriage-visible-exclude-current-buffer)
                                  carriage-visible-exclude-current-buffer
                                  (eq b buf))
                             (minibufferp b)
                             (eq mm 'exwm-mode)
                             (and (listp ignored-modes) (memq mm ignored-modes))
                             (and (listp ignored-names)
                                  (seq-some (lambda (rx) (and (stringp rx) (string-match-p rx nm)))
                                            ignored-names)))))
                   (unless skip
                     (let ((bf (buffer-file-name b)))
                       (cond
                        ;; File-visiting (local) buffers: reuse file pipeline
                        ((and (stringp bf) (not (file-remote-p bf)))
                         (when (carriage-context--state-under-file-limit-p state)
                           (setq state (carriage-context--collect-process-path bf state))))
                        ;; Non-file buffers (or TRAMP): include content (tail for terminals)
                        (t
                         (when (carriage-context--state-under-file-limit-p state)
                           (let* ((rel (format "visible:/%s" nm))
                                  (is-term (or (derived-mode-p 'comint-mode)
                                               (derived-mode-p 'eshell-mode)
                                               (derived-mode-p 'term-mode)
                                               (ignore-errors (derived-mode-p 'vterm-mode))
                                               (derived-mode-p 'compilation-mode)
                                               (eq mm 'messages-buffer-mode)
                                               (string= nm "*Messages*")))
                                  (text
                                   (save-excursion
                                     (save-restriction
                                       (widen)
                                       (if (not is-term)
                                           (buffer-substring-no-properties (point-min) (point-max))
                                         (goto-char (point-max))
                                         (forward-line (- tail))
                                         (buffer-substring-no-properties (point) (point-max))))))
                                  (sz (string-bytes text))
                                  (total (plist-get state :total-bytes)))
                             (if (> (+ total sz) max-bytes)
                                 (progn
                                   (setq state (carriage-context--push-warning
                                                (format "limit reached, include path only: %s" rel) state))
                                   (setq state (carriage-context--push-file
                                                (list :rel rel :true nil :content nil :reason 'size-limit)
                                                state))
                                   (setq state (plist-put state :skipped (1+ (plist-get state :skipped)))))
                               (setq state (carriage-context--push-file
                                            (list :rel rel :true nil :content text)
                                            state))
                               (setq state (plist-put state :total-bytes (+ total sz)))
                               (setq state (plist-put state :included (1+ (plist-get state :included))))))))))))))))
         nil (selected-frame))))
    ;; 3) GPTEL candidates last (lowest preference)
    (when (and include-gptel (carriage-context--state-under-file-limit-p state))
      (let ((gpt-cands (ignore-errors (carriage-context--maybe-gptel-files))))
        (when gpt-cands
          (setq state (carriage-context--collect-iterate gpt-cands state)))))
    (carriage-context--collect-finalize state)))

(defun carriage-context-collect (&optional buffer root)
  "Collect context from sources into a plist:
:files — list of plists (:rel :true :content :reason)
:warnings — list of strings
:omitted — count of omitted files due to limits
:stats — plist (:total-bytes N :included M :skipped K)
This function respects buffer-local toggles:
- carriage-mode-include-gptel-context
- carriage-mode-include-doc-context
and size limits:
- carriage-mode-context-max-files
- carriage-mode-context-max-total-bytes"
  (let* ((buf (or buffer (current-buffer)))
         (config (carriage-context--collect-config buf root)))
    (carriage-context--collect-exec buf config)))

(defun carriage-context--guess-lang (path)
  "Guess language token for Org src block by PATH extension."
  (let ((ext (downcase (file-name-extension path ""))))
    (pcase ext
      ((or "el" "elisp") "emacs-lisp")
      ((or "md" "markdown") "markdown")
      ("org" "org")
      ((or "js" "mjs" "cjs") "js")
      ((or "ts" "tsx") "ts")
      ((or "py") "python")
      ((or "json") "json")
      ((or "sh" "bash" "zsh") "sh")
      ((or "c" "h") "c")
      ((or "cpp" "hpp" "cc" "hh") "cpp")
      ((or "java") "java")
      ((or "rs") "rust")
      ((or "go") "go")
      ((or "rb") "ruby")
      ((or "yml" "yaml") "yaml")
      (_ "text"))))

(defun carriage-context--collapse-applied-patches-in-text (text)
  "Return TEXT with bodies of applied #+begin_patch blocks collapsed.

Keeps the begin_patch header line so the LLM sees what was already applied,
but replaces the body with a one-line summary comment and preserves (or
synthesizes) the closing #+end_patch."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" nil t)
        (let* ((beg-line-end (line-end-position))
               (sexp-str (match-string 1))
               (plist (condition-case _e
                          (car (read-from-string sexp-str))
                        (error nil))))
          (if (and (listp plist) (plist-get plist :applied))
              (let* ((desc (or (plist-get plist :description)
                               (plist-get plist :result)
                               "Applied"))
                     (desc (string-trim (format "%s" desc)))
                     (desc (if (string-empty-p desc) "Applied" desc))
                     (summary (format ";; applied: %s (content omitted)\n" desc))
                     (body-beg (min (point-max) (1+ beg-line-end)))
                     (end-pos (save-excursion
                                (goto-char body-beg)
                                (re-search-forward "^[ \t]*#\\+end_patch\\b.*$" nil t))))
                (if end-pos
                    (let ((end-line-beg (save-excursion
                                          (goto-char end-pos)
                                          (line-beginning-position))))
                      (delete-region body-beg end-line-beg)
                      (goto-char body-beg)
                      (insert summary)
                      ;; Continue after the real end_patch line.
                      (when (re-search-forward "^[ \t]*#\\+end_patch\\b.*$" nil t)
                        (goto-char (min (point-max) (1+ (line-end-position))))))
                  ;; No end marker: truncate to end and synthesize it.
                  (delete-region body-beg (point-max))
                  (goto-char body-beg)
                  (insert summary "#+end_patch\n")
                  (goto-char (point-max))))
            ;; Not applied: continue scanning from next line to avoid loops.
            (goto-char (min (point-max) (1+ (line-end-position))))))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun carriage-context-format (ctx &key where)
  "Format CTX (plist from carriage-context-collect) into a string for insertion.
WHERE is 'system or 'user (affects only label string)."
  (let* ((files (or (plist-get ctx :files) '()))
         (warnings (or (plist-get ctx :warnings) '()))
         (stats (or (plist-get ctx :stats) '()))
         (hdr (format ";; Context (%s): files=%d included=%s omitted=%s total-bytes=%s\n"
                      (or (and where (symbol-name where)) "system")
                      (length files)
                      (or (plist-get stats :included) 0)
                      (or (plist-get stats :skipped) 0)
                      (or (plist-get stats :total-bytes) 0)))
         (warn-str (mapconcat (lambda (w) (concat ";; " w)) warnings "\n"))
         (sections
          (mapcar
           (lambda (f)
             (let* ((rel (plist-get f :rel))
                    (content (plist-get f :content))
                    (reason (plist-get f :reason))
                    (lang (carriage-context--guess-lang rel)))
               (if (stringp content)
                   (concat (format "In file %s:\n" rel)
                           (format "#+begin_src %s\n" lang)
                           content
                           "\n#+end_src\n")
                 (format "In file %s: [content omitted]%s\n"
                         rel
                         (if reason (format " (%s)" reason) "")))))
           files)))
    (string-join (delq nil (list hdr (and warnings warn-str)
                                 (mapconcat #'identity sections "\n")))
                 "\n")))

;; -----------------------------------------------------------------------------
;; Context counter for modeline/tooltips

(defun carriage-context--count-include-flags (buf)
  "Return cons of inclusion toggles for COUNT: (INC-GPT . INC-DOC).

Legacy context variables are no longer supported.
Defaults: doc ON, gptel OFF."
  (with-current-buffer buf
    (let* ((inc-gpt (and (boundp 'carriage-mode-include-gptel-context)
                         (buffer-local-value 'carriage-mode-include-gptel-context buf)))
           (inc-doc (if (boundp 'carriage-mode-include-doc-context)
                        (buffer-local-value 'carriage-mode-include-doc-context buf)
                      t)))
      (cons inc-gpt inc-doc))))

(defun carriage-context--count-root+toggles (buf)
  "Return plist with root and inclusion toggles for BUF: (:root R :inc-gpt B :inc-doc B :inc-vis B :inc-patched B)."
  (let* ((root (carriage-context--project-root))
         (inc-gpt (with-current-buffer buf
                    (and (boundp 'carriage-mode-include-gptel-context)
                         (buffer-local-value 'carriage-mode-include-gptel-context buf))))
         (inc-doc (with-current-buffer buf
                    (if (boundp 'carriage-mode-include-doc-context)
                        (buffer-local-value 'carriage-mode-include-doc-context buf)
                      t)))
         (inc-vis (with-current-buffer buf
                    (and (boundp 'carriage-mode-include-visible-context)
                         (buffer-local-value 'carriage-mode-include-visible-context buf))))
         (inc-patched (with-current-buffer buf
                        (and (boundp 'carriage-mode-include-patched-files)
                             (buffer-local-value 'carriage-mode-include-patched-files buf)))))
    (list :root root :inc-gpt inc-gpt :inc-doc inc-doc :inc-vis inc-vis :inc-patched inc-patched)))

(defun carriage-context--true-map (trues)
  "Build and return a hash-table mapping TRUES for quick membership checks."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (tru trues) (puthash tru tru h))
    h))

(defun carriage-context--count-gather-trues (buf root inc-gpt inc-doc inc-vis inc-patched)
  "Collect truenames and maps for DOC, PATCHED, GPTEL and VISIBLE sources given BUF and ROOT.
Patched files are treated as 'doc' for source classification."
  (let* ((doc-trues (if inc-doc
                        (carriage-context--unique-truenames-under-root
                         (ignore-errors (carriage-context--doc-paths buf)) root)
                      '()))
         (patched-trues (if inc-patched
                            (carriage-context--unique-truenames-under-root
                             (ignore-errors (carriage-context--patched-files buf)) root)
                          '()))
         (gpt-trues (if inc-gpt
                        (carriage-context--unique-truenames-under-root
                         (ignore-errors (carriage-context--maybe-gptel-files)) root)
                      '()))
         (vis-trues
          (if inc-vis
              (let ((acc '())
                    (seen (make-hash-table :test 'eq))
                    (ignored-modes (and (boundp 'carriage-visible-ignore-modes) carriage-visible-ignore-modes))
                    (ignored-names (and (boundp 'carriage-visible-ignore-buffer-regexps) carriage-visible-ignore-buffer-regexps)))
                (walk-windows
                 (lambda (w)
                   (let ((b (window-buffer w)))
                     (unless (gethash b seen)
                       (puthash b t seen)
                       (with-current-buffer b
                         (let* ((nm (buffer-name b))
                                (mm major-mode)
                                (skip (or (and (boundp 'carriage-visible-exclude-current-buffer)
                                               carriage-visible-exclude-current-buffer
                                               (eq b buf))
                                          (minibufferp b)
                                          (eq mm 'exwm-mode)
                                          (and (listp ignored-modes) (memq mm ignored-modes))
                                          (and (listp ignored-names)
                                               (seq-some (lambda (rx) (and (stringp rx) (string-match-p rx nm)))
                                                         ignored-names)))))
                           (unless skip
                             (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
                               (let* ((norm (carriage-context--normalize-candidate buffer-file-name root)))
                                 (when (plist-get norm :ok)
                                   (push (plist-get norm :true) acc))))))))))

                 nil (selected-frame))
                (delete-dups acc))
            '()))
         ;; doc-map should include patched as doc
         (doc-all (delete-dups (append doc-trues patched-trues))))
    (list :doc-trues doc-trues
          :patched-trues patched-trues
          :gpt-trues gpt-trues
          :vis-trues vis-trues
          :doc-map (carriage-context--true-map doc-all)
          :gpt-map (carriage-context--true-map gpt-trues)
          :vis-map (carriage-context--true-map vis-trues))))

(defun carriage-context--count-build-items (files doc-map gpt-map vis-map)
  "Build item list for count from FILES using DOC-MAP, GPT-MAP and VIS-MAP."
  (mapcar
   (lambda (f)
     (let* ((rel (plist-get f :rel))
            (tru (plist-get f :true))
            (content (plist-get f :content))
            (incl (plist-get f :included))
            (reason (plist-get f :reason))
            (docp (and tru (gethash tru doc-map)))
            (gptp (and tru (gethash tru gpt-map)))
            (visp (or (and tru (gethash tru vis-map))
                      (and (stringp rel) (string-prefix-p "visible:/" rel))))
            (src (cond
                  ((and docp gptp) 'both)
                  (docp 'doc)
                  (gptp 'gptel)
                  (visp 'visible)
                  (t nil))))
       (list :path rel
             :true tru
             :source src
             :included (or incl (stringp content))
             :reason reason)))
   files))

(defun carriage-context--count-empty-result ()
  "Return an empty counter result plist."
  (list :count 0 :items '() :sources '() :warnings '() :stats '()))

(defun carriage-context--count-assemble (items warnings stats)
  "Assemble final count plist from ITEMS, WARNINGS and STATS."
  (let* ((srcs (carriage-context--source-counts-from-items items))
         (count (length items)))
    (list :count count :items items :sources srcs :warnings warnings :stats stats)))

(defun carriage-context--count-fast-init (buf root inc-gpt inc-doc inc-vis inc-patched)
  "Return initial state plist for fast context count (no file reads).

Additionally maintains per-source membership maps (hash tables):
- :doc-map — files originating from doc-context (including patched files)
- :gpt-map — files originating from gptel context
- :vis-map — files originating from visible file-visiting buffers

These maps are built during the fast pass, avoiding a second normalization/scan."
  (let* ((lims (carriage-context--limits buf))
         (seen (make-hash-table :test 'equal)))
    (list :root (or root (carriage-context--project-root))
          :include-gptel inc-gpt
          :include-doc inc-doc
          :include-visible inc-vis
          :include-patched inc-patched
          :max-files (plist-get lims :max-files)
          :max-bytes (plist-get lims :max-bytes)
          :seen seen
          :doc-map (make-hash-table :test 'equal)
          :gpt-map (make-hash-table :test 'equal)
          :vis-map (make-hash-table :test 'equal)
          :files '()
          :warnings '()
          :total-bytes 0
          :included 0
          :skipped 0)))

(defun carriage-context--count-fast-under-file-limit-p (state)
  "Return non-nil if we can still consider more files given STATE limits."
  (< (+ (plist-get state :included)
        (plist-get state :skipped))
     (plist-get state :max-files)))

(defun carriage-context--count-fast-push-warning (msg state)
  "Push warning MSG into STATE."
  (plist-put state :warnings (cons msg (plist-get state :warnings))))

(defun carriage-context--count-fast-push-file (entry state)
  "Push file ENTRY plist into STATE."
  (plist-put state :files (cons entry (plist-get state :files))))

(defun carriage-context--count-fast-mark-seen (true state)
  "Mark TRUE as seen in STATE. Return non-nil if it was not seen before."
  (let ((seen (plist-get state :seen)))
    (unless (gethash true seen)
      (puthash true t seen)
      t)))

(defun carriage-context--count-fast--mark-source (source true state)
  "Mark TRUE as belonging to SOURCE in STATE source maps.
SOURCE is one of: 'doc 'gptel 'visible. Patched files must be passed as 'doc."
  (when (and (stringp true) (not (string-empty-p true)))
    (pcase source
      ('doc
       (let ((m (plist-get state :doc-map)))
         (when (hash-table-p m) (puthash true t m))))
      ('gptel
       (let ((m (plist-get state :gpt-map)))
         (when (hash-table-p m) (puthash true t m))))
      ('visible
       (let ((m (plist-get state :vis-map)))
         (when (hash-table-p m) (puthash true t m))))
      (_ nil)))
  state)

(defun carriage-context--count-fast-process-path (p state &optional source)
  "Process candidate path P and update STATE (no file reads; size-based budgeting).
SOURCE is used only for source classification maps."
  (let* ((root (plist-get state :root))
         (max-bytes (plist-get state :max-bytes))
         (norm (carriage-context--normalize-candidate p root)))
    (if (not (plist-get norm :ok))
        (progn
          (setq state (plist-put state :skipped (1+ (plist-get state :skipped))))
          (setq state (carriage-context--count-fast-push-warning
                       (format "skip %s: %s" p (plist-get norm :reason))
                       state))
          state)
      (let* ((rel (plist-get norm :rel))
             (true (plist-get norm :true)))
        (if (not (carriage-context--count-fast-mark-seen true state))
            state
          ;; Source classification must apply even when the file is omitted by limits,
          ;; so we mark it immediately after dedupe.
          (setq state (carriage-context--count-fast--mark-source source true state))
          (let* ((attrs (ignore-errors (file-attributes true)))
                 (sb (and attrs (nth 7 attrs)))
                 (total (plist-get state :total-bytes)))
            (cond
             ;; Missing/unstat'able file.
             ((null attrs)
              (setq state (carriage-context--count-fast-push-file
                           (list :rel rel :true true :included nil :reason 'missing)
                           state))
              (setq state (plist-put state :skipped (1+ (plist-get state :skipped))))
              state)
             ;; Size limit (pre-check only; no reads).
             ((and (numberp sb) (> (+ total sb) max-bytes))
              (setq state (carriage-context--count-fast-push-warning
                           (format "limit reached, include path only: %s" rel)
                           state))
              (setq state (carriage-context--count-fast-push-file
                           (list :rel rel :true true :included nil :reason 'size-limit)
                           state))
              (setq state (plist-put state :skipped (1+ (plist-get state :skipped))))
              state)
             ;; Include (planned).
             (t
              (when (numberp sb)
                (setq state (plist-put state :total-bytes (+ total sb))))
              (setq state (carriage-context--count-fast-push-file
                           (list :rel rel :true true :included t)
                           state))
              (setq state (plist-put state :included (1+ (plist-get state :included))))
              state))))))))

(defun carriage-context--count-fast-process-visible-buffer (buf state)
  "Process a visible non-file buffer BUF into STATE (size-based budgeting, no file reads)."
  (with-current-buffer buf
    (let* ((nm (buffer-name buf))
           (rel (format "visible:/%s" nm))
           (max-bytes (plist-get state :max-bytes))
           (total (plist-get state :total-bytes))
           ;; Approximation: buffer-size is chars, not bytes. It's good enough for UI budgeting.
           (sz (buffer-size)))
      (if (> (+ total sz) max-bytes)
          (progn
            (setq state (carriage-context--count-fast-push-warning
                         (format "limit reached, include path only: %s" rel) state))
            (setq state (carriage-context--count-fast-push-file
                         (list :rel rel :true nil :included nil :reason 'size-limit)
                         state))
            (setq state (plist-put state :skipped (1+ (plist-get state :skipped))))
            state)
        (setq state (carriage-context--count-fast-push-file
                     (list :rel rel :true nil :included t)
                     state))
        (setq state (plist-put state :total-bytes (+ total sz)))
        (setq state (plist-put state :included (1+ (plist-get state :included))))
        state))))

(defun carriage-context--count-fast-finalize (state)
  "Finalize fast count STATE into plist: (:files :warnings :stats)."
  (let ((files (nreverse (plist-get state :files)))
        (warnings (nreverse (plist-get state :warnings)))
        (included (plist-get state :included))
        (skipped (plist-get state :skipped))
        (total-bytes (plist-get state :total-bytes)))
    (list :files files
          :warnings warnings
          :stats (list :total-bytes total-bytes :included included :skipped skipped))))

(defun carriage-context--count-fast-exec (buf root inc-gpt inc-doc inc-vis inc-patched)
  "Compute a fast count-compatible result for BUF/ROOT/toggles without reading file contents."
  (let* ((state (carriage-context--count-fast-init buf root inc-gpt inc-doc inc-vis inc-patched))
         (max-files (plist-get state :max-files))
         (max-bytes (plist-get state :max-bytes))
         (_ (carriage-context--dbg "count-fast: root=%s include{gpt=%s,doc=%s,vis=%s,patched=%s} limits{files=%s,bytes=%s}"
                                   (plist-get state :root) inc-gpt inc-doc inc-vis inc-patched max-files max-bytes)))
    ;; 1a) Patched files (treated as doc precedence)
    (when (and inc-patched (carriage-context--count-fast-under-file-limit-p state))
      (let ((pat (ignore-errors (carriage-context--patched-files buf))))
        (dolist (p pat)
          (when (carriage-context--count-fast-under-file-limit-p state)
            (setq state (carriage-context--count-fast-process-path p state 'doc))))))
    ;; 1b) Doc paths
    (when (and inc-doc (carriage-context--count-fast-under-file-limit-p state))
      (let ((doc (ignore-errors (carriage-context--doc-paths buf))))
        (dolist (p doc)
          (when (carriage-context--count-fast-under-file-limit-p state)
            (setq state (carriage-context--count-fast-process-path p state 'doc))))))
    ;; 2) Visible buffers
    (when (and inc-vis (carriage-context--count-fast-under-file-limit-p state))
      (let ((seen (make-hash-table :test 'eq))
            (ignored-modes (and (boundp 'carriage-visible-ignore-modes) carriage-visible-ignore-modes))
            (ignored-names (and (boundp 'carriage-visible-ignore-buffer-regexps) carriage-visible-ignore-buffer-regexps)))
        (walk-windows
         (lambda (w)
           (let ((b (window-buffer w)))
             (unless (gethash b seen)
               (puthash b t seen)
               (with-current-buffer b
                 (let* ((nm (buffer-name b))
                        (mm major-mode)
                        (skip
                         (or (and (boundp 'carriage-visible-exclude-current-buffer)
                                  carriage-visible-exclude-current-buffer
                                  (eq b buf))
                             (minibufferp b)
                             (eq mm 'exwm-mode)
                             (and (listp ignored-modes) (memq mm ignored-modes))
                             (and (listp ignored-names)
                                  (seq-some (lambda (rx) (and (stringp rx) (string-match-p rx nm)))
                                            ignored-names)))))
                   (unless skip
                     (cond
                      ((and (stringp buffer-file-name) (not (file-remote-p buffer-file-name)))
                       (when (carriage-context--count-fast-under-file-limit-p state)
                         (setq state (carriage-context--count-fast-process-path buffer-file-name state 'visible))))
                      (t
                       (when (carriage-context--count-fast-under-file-limit-p state)
                         (setq state (carriage-context--count-fast-process-visible-buffer b state))))))))))))
        nil (selected-frame)))
    ;; 3) GPTel context last
    (when (and inc-gpt (carriage-context--count-fast-under-file-limit-p state))
      (let ((gpt (ignore-errors (carriage-context--maybe-gptel-files))))
        (dolist (p gpt)
          (when (carriage-context--count-fast-under-file-limit-p state)
            (setq state (carriage-context--count-fast-process-path p state 'gptel))))))
    (carriage-context--count-fast-finalize state)))

(defun carriage-context-count (&optional buffer _point)
  "Вернуть plist со счётчиком элементов контекста для BUFFER.

ВНИМАНИЕ (perf):
- Эта функция должна быть дешёвой и безопасной для использования из UI/redisplay.
- Она НЕ читает содержимое файлов; бюджетирование делается по file-attributes (size) и по buffer-size для visible buffers.

Формат результата:
  (:count N
   :items  ((:path REL :true TRU :source doc|gptel|both|visible :included t|nil :reason REASON) ...)
   :sources ((doc . ND) (gptel . NG) (both . NB) (visible . NV))
   :warnings (STR ...)
   :stats (:total-bytes N :included M :skipped K))"
  (let* ((buf (or buffer (current-buffer)))
         (cfg (carriage-context--count-root+toggles buf))
         (root (plist-get cfg :root))
         (inc-gpt (plist-get cfg :inc-gpt))
         (inc-doc (plist-get cfg :inc-doc))
         (inc-vis (plist-get cfg :inc-vis))
         (inc-patched (plist-get cfg :inc-patched)))
    (if (not (or inc-gpt inc-doc inc-vis inc-patched))
        (progn
          (carriage-context--dbg "count: all sources OFF → 0")
          (carriage-context--count-empty-result))
      (let* ((true-data (carriage-context--count-gather-trues buf root inc-gpt inc-doc inc-vis inc-patched))
             (doc-map (plist-get true-data :doc-map))
             (gpt-map (plist-get true-data :gpt-map))
             (vis-map (plist-get true-data :vis-map))
             (fast (carriage-context--count-fast-exec buf root inc-gpt inc-doc inc-vis inc-patched))
             (files (or (plist-get fast :files) '()))
             (warnings (or (plist-get fast :warnings) '()))
             (stats (or (plist-get fast :stats) '()))
             (items (carriage-context--count-build-items files doc-map gpt-map vis-map)))
        (carriage-context--dbg "count-fast: files=%s items=%s warnings=%s bytes=%s"
                               (length files) (length items) (length warnings)
                               (or (plist-get stats :total-bytes) 0))
        (carriage-context--count-assemble items warnings stats)))))
;; -----------------------------------------------------------------------------
;; Context profile (P1/P3) — defaults, setter and toggle

(defgroup carriage-context-profile nil
  "Context profiles (P1-core, P3-debug) and their defaults."
  :group 'carriage-context
  :prefix "carriage-context-")

(defcustom carriage-context-p1-defaults
  '(:max-files 100 :max-bytes 1048576)
  "Default limits for P1-core profile (small, focused context).
Keys: :max-files (int), :max-bytes (int, bytes)."
  :type '(plist :key-type (choice (const :max-files) (const :max-bytes))
                :value-type integer)
  :group 'carriage-context-profile)

(defcustom carriage-context-p3-defaults
  '(:max-files 400 :max-bytes 4194304)
  "Default limits for P3-debug profile (extended, opt-in).
Keys: :max-files (int), :max-bytes (int, bytes)."
  :type '(plist :key-type (choice (const :max-files) (const :max-bytes))
                :value-type integer)
  :group 'carriage-context-profile)

(defvar-local carriage-doc-context-profile 'p1
  "Current context profile for this buffer: 'p1 or 'p3.")

(defun carriage-context--apply-profile-defaults (profile)
  "Apply default limits for PROFILE into buffer-local Carriage context settings."
  (let* ((pl (if (eq profile 'p3) carriage-context-p3-defaults carriage-context-p1-defaults))
         (mf (plist-get pl :max-files))
         (mb (plist-get pl :max-bytes)))
    (when (boundp 'carriage-mode-context-max-files)
      (setq-local carriage-mode-context-max-files (or mf (and (local-variable-p 'carriage-mode-context-max-files) carriage-mode-context-max-files))))
    (when (boundp 'carriage-mode-context-max-total-bytes)
      (setq-local carriage-mode-context-max-total-bytes (or mb (and (local-variable-p 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes))))))

;;;###autoload
(defun carriage-context-profile-set (profile)
  "Set context PROFILE to 'p1 or 'p3 for the current buffer, adjust limits and refresh UI.
Writes CAR_CONTEXT_PROFILE on save via doc-state."
  (interactive
   (list (intern (completing-read "Context profile: " '("p1" "p3") nil t nil nil "p1"))))
  (setq-local carriage-doc-context-profile (if (memq profile '(p1 p3)) profile 'p1))
  (carriage-context--apply-profile-defaults carriage-doc-context-profile)
  ;; UI warning/notice on profile switch
  (when (eq carriage-doc-context-profile 'p3)
    (message "Context profile switched to P3-debug — files=%s bytes=%s (extended budget; may impact cost/quality)"
             (or (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files) "-")
             (or (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes) "-")))
  (when (eq carriage-doc-context-profile 'p1)
    (message "Context profile switched to P1-core — files=%s bytes=%s"
             (or (and (boundp 'carriage-mode-context-max-files) carriage-mode-context-max-files) "-")
             (or (and (boundp 'carriage-mode-context-max-total-bytes) carriage-mode-context-max-total-bytes) "-")))
  ;; UI refresh: reset caches if available
  (when (fboundp 'carriage-ui--reset-context-cache)
    (ignore-errors (carriage-ui--reset-context-cache)))
  (when (fboundp 'carriage-ui--invalidate-ml-cache)
    (ignore-errors (carriage-ui--invalidate-ml-cache)))
  (force-mode-line-update)
  ;; Persist on next save; optionally update doc-state immediately when available
  (when (fboundp 'carriage-doc-state-write)
    (ignore-errors
      (carriage-doc-state-write
       (list :CAR_CONTEXT_PROFILE (if (eq carriage-doc-context-profile 'p3) "P3" "P1")))))
  (when (require 'carriage-logging nil t)
    (ignore-errors
      (carriage-metrics-note 'ctx-profile
                             (if (eq carriage-doc-context-profile 'p3) "P3" "P1"))))
  carriage-doc-context-profile)

;;;###autoload
(defun carriage-toggle-context-profile ()
  "Toggle context profile between P1 and P3 in the current buffer."
  (interactive)
  (carriage-context-profile-set (if (eq carriage-doc-context-profile 'p3) 'p1 'p3)))

(defvar carriage--perf--patched-files-dirty-guard-installed nil)

(defun carriage--perf--context--after-change-snippet (beg end &optional max-chars)
  (let* ((max-chars (or max-chars 900))
         (lo (save-excursion (goto-char beg) (line-beginning-position)))
         (hi (save-excursion (goto-char end) (line-end-position)))
         (hi2 (min hi (+ lo max-chars))))
    (buffer-substring-no-properties lo hi2)))

(defun carriage--perf--context--snippet-matches-p (beg end re)
  (string-match-p re (carriage--perf--context--after-change-snippet beg end)))

(unless carriage--perf--patched-files-dirty-guard-installed
  (setq carriage--perf--patched-files-dirty-guard-installed t)
  (when (fboundp 'carriage-context--patched-files-mark-dirty)
    (advice-add
     'carriage-context--patched-files-mark-dirty
     :around
     (lambda (orig beg end len)
       ;; This source is relevant only when the "patched files" context is enabled
       ;; and the edit touches patch blocks content/markers.
       (if (and (boundp 'carriage-mode-include-patched-files)
                carriage-mode-include-patched-files
                (carriage--perf--context--snippet-matches-p
                 beg end
                 "^[ \t]*#\\+begin_patch\\b\\|^[ \t]*#\\+end_patch\\b\\|^[ \t]*#\\+begin_from\\b\\|^[ \t]*#\\+end_from\\b\\|^[ \t]*#\\+begin_to\\b\\|^[ \t]*#\\+end_to\\b"))
           (funcall orig beg end len)
         nil)))))

(defun carriage-context--project-map--cache-get (root)
  "Return cached Project Map record for ROOT, or nil.
Cache record plist keys: :time :text :paths :truncated."
  (when (and (hash-table-p carriage-context--project-map-cache)
             (stringp root) (not (string-empty-p root)))
    (gethash root carriage-context--project-map-cache)))

(defun carriage-context--project-map--cache-put (root rec)
  "Store Project Map REC for ROOT in cache."
  (when (and (hash-table-p carriage-context--project-map-cache)
             (stringp root) (not (string-empty-p root))
             (listp rec))
    (puthash root rec carriage-context--project-map-cache)))

(defun carriage-context--project-map--cache-fresh-p (rec ttl)
  "Return non-nil when REC cache entry is fresh per TTL seconds."
  (let* ((t0 (and (listp rec) (plist-get rec :time)))
         (now (float-time)))
    (and (numberp t0)
         (numberp ttl)
         (>= ttl 0.0)
         (< (- now t0) ttl))))

(defvar carriage-context--project-map-cache (make-hash-table :test 'equal)
  "Cache mapping project root → Project Map record plist.
Record keys: :time :text :paths :truncated.")

(defvar carriage-context--project-map-allow-compute nil
  "When non-nil, Project Map is allowed to run external processes / directory traversal.

This MUST be nil on redisplay/modeline/timer paths. The send pipeline binds it to t
around Project Map generation.")

(defun carriage-context--project-map--node-make ()
  "Make an empty trie node.
Node is a plist: (:dirs HASH :files HASH)."
  (list :dirs (make-hash-table :test 'equal)
        :files (make-hash-table :test 'equal)))

(defun carriage-context--project-map--node-valid-p (node)
  "Return non-nil when NODE is a valid project-map trie node."
  (and (listp node)
       (hash-table-p (plist-get node :dirs))
       (hash-table-p (plist-get node :files))))

(defun carriage-context--project-map--node-ensure (node)
  "Return a valid trie NODE, repairing/creating it if needed."
  (let ((n (if (listp node) node (carriage-context--project-map--node-make))))
    (unless (hash-table-p (plist-get n :dirs))
      (setq n (plist-put n :dirs (make-hash-table :test 'equal))))
    (unless (hash-table-p (plist-get n :files))
      (setq n (plist-put n :files (make-hash-table :test 'equal))))
    ;; If it still isn't valid (e.g. dotted list), replace completely.
    (unless (carriage-context--project-map--node-valid-p n)
      (setq n (carriage-context--project-map--node-make)))
    n))

(defun carriage-context--project-map--node-dirs (node)
  "Return NODE's :dirs hash-table (repairing NODE if malformed)."
  (plist-get (carriage-context--project-map--node-ensure node) :dirs))

(defun carriage-context--project-map--node-files (node)
  "Return NODE's :files hash-table (repairing NODE if malformed)."
  (plist-get (carriage-context--project-map--node-ensure node) :files))

(defun carriage-context--project-map--insert (root-node relpath)
  "Insert RELPATH into ROOT-NODE trie."
  (let* ((segs (split-string relpath "/" t))
         (nseg (length segs)))
    (when (> nseg 0)
      (let ((node (carriage-context--project-map--node-ensure root-node))
            (i 0))
        (while (< i nseg)
          (setq node (carriage-context--project-map--node-ensure node))
          (let* ((seg (nth i segs))
                 (last (= i (1- nseg))))
            (if last
                (let ((files (carriage-context--project-map--node-files node)))
                  (when (hash-table-p files)
                    (puthash seg t files)))
              (let* ((dirs (carriage-context--project-map--node-dirs node))
                     (child (and (hash-table-p dirs) (gethash seg dirs))))
                (setq child (carriage-context--project-map--node-ensure child))
                (when (hash-table-p dirs)
                  (puthash seg child dirs))
                (setq node child))))
          (setq i (1+ i)))))))

(defun carriage-context--project-map--sorted-keys (h)
  "Return hash-table keys of H sorted lexicographically."
  (when (hash-table-p h)
    (sort (hash-table-keys h) #'string<)))

(defun carriage-context--project-map--render (node depth)
  "Render NODE trie to a list of lines (strings), with DEPTH indentation."
  (let* ((indent (make-string (* 2 (max 0 depth)) ?\s))
         (dirs (carriage-context--project-map--node-dirs node))
         (files (carriage-context--project-map--node-files node))
         (dirnames (carriage-context--project-map--sorted-keys dirs))
         (filenames (carriage-context--project-map--sorted-keys files))
         (acc '()))
    ;; Directories first.
    (dolist (d dirnames)
      (push (concat indent d "/") acc)
      (let ((child (gethash d dirs)))
        (when (listp child)
          (setq acc (nconc (nreverse (carriage-context--project-map--render child (1+ depth))) acc)))))
    ;; Then files.
    (dolist (f filenames)
      (push (concat indent f) acc))
    (nreverse acc)))

(defun carriage-context--project-map--call (default-dir program args timeout)
  "Call PROGRAM with ARGS in DEFAULT-DIR, return plist (:exit :stdout :stderr :reason).

Nonblocking semantics:
- Waits via `accept-process-output` in short slices so timers/UI can run.
- Enforces TIMEOUT seconds; on timeout returns :exit 124 and :reason 'timeout.

Best-effort: never signals."
  (condition-case _e
      (let* ((default-directory (file-name-as-directory (expand-file-name default-dir)))
             (stdout-buf (generate-new-buffer " *carriage-projmap-stdout*"))
             (stderr-buf (generate-new-buffer " *carriage-projmap-stderr*"))
             (proc nil)
             (done nil)
             (exit-code nil)
             (deadline (+ (float-time) (max 0.0 (float (or timeout 0.0))))))
        (unwind-protect
            (progn
              (setq proc
                    (make-process
                     :name "carriage-projmap"
                     :command (append (list program) args)
                     :buffer stdout-buf
                     :stderr stderr-buf
                     :noquery t
                     :connection-type 'pipe
                     :sentinel (lambda (p _e)
                                 (when (memq (process-status p) '(exit signal failed))
                                   (setq exit-code (condition-case _
                                                       (process-exit-status p)
                                                     (error exit-code)))
                                   (setq done t)))))
              (while (and (processp proc) (not done) (< (float-time) deadline))
                (accept-process-output proc 0.03)
                (when (and (not done)
                           (memq (process-status proc) '(exit signal failed)))
                  (setq exit-code (condition-case _
                                      (process-exit-status proc)
                                    (error exit-code)))
                  (setq done t)))
              (when (and (processp proc) (not done) (process-live-p proc))
                (ignore-errors (interrupt-process proc))
                (ignore-errors (kill-process proc))
                (setq exit-code 124)
                (setq done t))
              (let ((stdout (with-current-buffer stdout-buf (buffer-string)))
                    (stderr (with-current-buffer stderr-buf (buffer-string))))
                (list :exit (or exit-code -1)
                      :stdout stdout
                      :stderr stderr
                      :reason (cond
                               ((= (or exit-code -1) 124) 'timeout)
                               ((and (numberp exit-code) (zerop exit-code)) nil)
                               (t 'error)))))
          (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
          (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))))
    (error (list :exit -1 :stdout "" :stderr "" :reason 'error))))

(defun carriage-context--project-map--git-ls-files (root)
  "Return list of repo-relative files for ROOT via git ls-files, or nil on failure.
Never signals. Returns nil also when list is empty (to enable fallbacks)."
  (cl-block carriage-context--project-map--git-ls-files
    (condition-case _e
        (progn
          (when (or (file-remote-p root) (not (file-directory-p root)))
            (cl-return-from carriage-context--project-map--git-ls-files nil))
          ;; Fast repo check without spawning git.
          (unless (locate-dominating-file root ".git")
            (cl-return-from carriage-context--project-map--git-ls-files nil))
          (when (not (fboundp 'carriage--call-git))
            (cl-return-from carriage-context--project-map--git-ls-files nil))
          (let* ((carriage-mode-git-timeout-seconds (or carriage-context-project-map-git-timeout-seconds 1.5))
                 (res (carriage--call-git root "ls-files" "-co" "--exclude-standard"))
                 (exit (plist-get res :exit))
                 (out (plist-get res :stdout)))
            (when (and (numberp exit) (zerop exit) (stringp out))
              (let* ((lines (split-string out "\n" t))
                     (files
                      (cl-remove-if
                       (lambda (p)
                         (or (string-empty-p p)
                             (string-prefix-p ".git/" p)
                             (string= p ".git")))
                       (mapcar (lambda (s) (replace-regexp-in-string "\\\\" "/" (string-trim s)))
                               lines))))
                (when (> (length files) 0)
                  files)))))
      (error nil))))

(defun carriage-context--project-map--fd-files (root)
  "Return list of repo-relative files for ROOT using `fd`, or nil.

Notes:
- `fd` is optional; this is a best-effort speed fallback.
- Runs with `-H` to include hidden files but still respects ignore files."
  (condition-case _e
      (when (and (stringp root) (file-directory-p root) (not (file-remote-p root))
                 (executable-find "fd"))
        (let* ((res (carriage-context--project-map--call
                     root "fd" (list "-t" "f" "-H" ".") carriage-context-project-map-fd-timeout-seconds))
               (exit (plist-get res :exit))
               (out (plist-get res :stdout)))
          (when (and (numberp exit) (zerop exit) (stringp out))
            (let* ((lines (split-string out "\n" t))
                   (paths
                    (cl-remove-if
                     (lambda (p)
                       (or (string-empty-p p)
                           (string-prefix-p ".git/" p)
                           (string= p ".git")))
                     (mapcar (lambda (s) (replace-regexp-in-string "\\\\" "/" (string-trim s)))
                             lines))))
              (when (> (length paths) 0)
                paths)))))
    (error nil)))

(defun carriage-context--project-map--skip-dir-p (name)
  "Return non-nil when directory NAME should be skipped in Emacs fallback walk."
  (and (stringp name)
       (member name (or carriage-context-project-map-skip-dirs '()))))

(defun carriage-context--project-map--emacs-files (root)
  "Return list of repo-relative file paths under ROOT via Emacs directory walk.

This is the last-resort fallback when git/fd are unavailable or failed.
It enforces a soft time budget and stops when max-paths is reached.

Best-effort: never signals."
  (condition-case _e
      (when (and (stringp root) (file-directory-p root) (not (file-remote-p root)))
        (let* ((start (float-time))
               (budget (max 0.05 (float (or carriage-context-project-map-emacs-walk-max-seconds 0.8))))
               (maxp (max 0 (or carriage-context-project-map-max-paths 0)))
               (acc '())
               (stack (list (file-name-as-directory (expand-file-name root)))))
          (while (and stack
                      (or (<= maxp 0) (< (length acc) maxp))
                      (< (- (float-time) start) budget))
            (let ((dir (pop stack)))
              (when (and (stringp dir) (file-directory-p dir))
                (dolist (ent (directory-files dir t nil t))
                  (let ((bn (file-name-nondirectory (directory-file-name ent))))
                    (cond
                     ((member bn '("." "..")) nil)
                     ((file-symlink-p ent) nil)
                     ((file-directory-p ent)
                      (unless (carriage-context--project-map--skip-dir-p bn)
                        (push ent stack)))
                     ((file-regular-p ent)
                      (push (file-relative-name ent root) acc))
                     (t nil)))))))
          (setq acc (delete-dups (mapcar (lambda (s) (replace-regexp-in-string "\\\\" "/" s)) acc)))
          (setq acc (sort acc #'string<))
          (when (> (length acc) 0)
            acc)))
    (error nil)))

(defun carriage-context--project-map--files (root)
  "Return plist describing Project Map file listing for ROOT.

Result plist keys:
- :ok       t|nil
- :files    list|nil
- :method   symbol (git|fd|emacs|none)
- :reason   symbol|string|nil
- :elapsed  float seconds

Order (fastest-first, with short timeouts):
1) git ls-files (gitignore-aware)
2) fd (usually fast; respects ignore files)
3) Emacs directory walk (last resort; may be slower)."
  (let* ((t0 (float-time)))
    (cl-labels
        ((mk (ok files method reason)
           (list :ok ok
                 :files files
                 :method method
                 :reason reason
                 :elapsed (- (float-time) t0)))
         (ok-files-p (x) (and (listp x) (> (length x) 0)))
         (try (method thunk)
           (let* ((t1 (float-time))
                  (files (ignore-errors (funcall thunk)))
                  (dt (- (float-time) t1)))
             (when (ok-files-p files)
               (carriage-context--dbg "project-map: method=%s ok files=%d elapsed=%.3fs"
                                      method (length files) dt))
             files)))
      ;; Important: do not use cl-return-from here (can produce no-catch in byte-compiled code).
      (let* ((g (try 'git (lambda () (carriage-context--project-map--git-ls-files root)))))
        (if (ok-files-p g)
            (mk t g 'git nil)
          (let* ((f (try 'fd (lambda () (carriage-context--project-map--fd-files root)))))
            (if (ok-files-p f)
                (mk t f 'fd nil)
              (let* ((e (try 'emacs (lambda () (carriage-context--project-map--emacs-files root)))))
                (if (ok-files-p e)
                    (mk t e 'emacs nil)
                  (mk nil nil 'none 'empty))))))))))

(defun carriage-context--project-map--truncate-bytes (s max-bytes)
  "Return (cons TEXT TRUNCATEDP) truncating S to MAX-BYTES bytes deterministically."
  (let* ((mx (max 0 (or max-bytes 0))))
    (if (or (<= mx 0) (<= (string-bytes s) mx))
        (cons s nil)
      (with-temp-buffer
        (insert s)
        ;; Approximate: truncate by chars until bytes fit; O(n) but only on send.
        (while (and (> (buffer-size) 0)
                    (> (string-bytes (buffer-string)) mx))
          (delete-region (max (point-min) (- (point-max) 256)) (point-max)))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "…\n")
        (cons (buffer-string) t)))))

(defun carriage-context-project-map-build (&optional root)
  "Build Project Map record plist for ROOT (or project root).

Return plist keys:
  :ok t|nil
  :text string|nil
  :paths integer
  :truncated t|nil
  :reason symbol|string|nil

Best-effort; never signals.

Important perf rule:
- When `carriage-context--project-map-allow-compute` is nil, this function MUST NOT
  spawn processes or traverse directories. It may return a fresh cached value."
  (let* ((r (or root (carriage-context--project-root))))
    (cond
     ((or (null r) (not (stringp r)) (string-empty-p r))
      (list :ok nil :reason 'no-root))
     ((file-remote-p r)
      (list :ok nil :reason 'remote))
     (t
      (let* ((ttl (or carriage-context-project-map-cache-ttl 0.0))
             (ce (carriage-context--project-map--cache-get r)))
        ;; Always allow returning cached value (UI-safe).
        (when (and ce (carriage-context--project-map--cache-fresh-p ce ttl))
          (cl-return-from carriage-context-project-map-build
            (append (list :ok t) ce)))
        ;; Do not compute on redisplay/timers.
        (unless carriage-context--project-map-allow-compute
          (cl-return-from carriage-context-project-map-build
            (list :ok nil :reason 'not-allowed)))
        (let* ((lr (carriage-context--project-map--files r))
               (ok (plist-get lr :ok))
               (files (plist-get lr :files))
               (method (plist-get lr :method))
               (elapsed (plist-get lr :elapsed))
               (why (plist-get lr :reason)))
          (unless (and ok (listp files) (> (length files) 0))
            (carriage-context--dbg "project-map: omitted reason=%s method=%s elapsed=%.3fs root=%s"
                                   why method (or elapsed 0.0) r)
            (cl-return-from carriage-context-project-map-build
              (list :ok nil :reason (or why 'empty) :method method :elapsed elapsed)))
          (let* ((maxp (max 0 (or carriage-context-project-map-max-paths 0)))
                 (maxb (max 0 (or carriage-context-project-map-max-bytes 0)))
                 (all (sort (copy-sequence files) #'string<))
                 (paths (if (and (numberp maxp) (> maxp 0) (> (length all) maxp))
                            (cl-subseq all 0 maxp)
                          all))
                 (trunc-paths (and (numberp maxp) (> maxp 0) (> (length all) maxp)))
                 (root-node (carriage-context--project-map--node-make)))
            (dolist (p paths)
              (when (and (stringp p) (not (string-empty-p p)))
                (carriage-context--project-map--insert root-node p)))
            (let* ((lines (carriage-context--project-map--render root-node 0))
                   (body (string-join lines "\n"))
                   (block (concat "#+begin_map\n" body "\n#+end_map\n"))
                   (tb (carriage-context--project-map--truncate-bytes block maxb))
                   (text (car tb))
                   (trunc-bytes (cdr tb))
                   (trunc (or trunc-paths trunc-bytes))
                   (rec (list :time (float-time)
                              :text text
                              :paths (length paths)
                              :truncated (if trunc t nil)
                              :method method
                              :elapsed elapsed)))
              (when trunc
                (carriage-context--dbg "project-map: truncated (method=%s paths=%s bytes=%s)"
                                       method trunc-paths trunc-bytes))
              (carriage-context--dbg "project-map: built method=%s paths=%d bytes=%d elapsed=%.3fs"
                                     method (length paths) (string-bytes (or text "")) (or elapsed 0.0))
              (carriage-context--project-map--cache-put r rec)
              (list :ok t
                    :text text
                    :paths (length paths)
                    :truncated (if trunc t nil)
                    :method method
                    :elapsed elapsed)))))))))

(defun carriage-context-project-map-block (&optional root)
  "Return a `#+begin_map … #+end_map` block string for ROOT (or current project), or nil.

Best-effort: never signals. Returns nil when:
- map is not ok,
- map is empty,
- map computation is not allowed in current context."
  (let* ((res (ignore-errors (carriage-context-project-map-build root))))
    (when (and (listp res) (plist-get res :ok))
      (let ((s (plist-get res :text))
            (n (plist-get res :paths)))
        (when (and (integerp n) (> n 0)
                   (stringp s) (not (string-empty-p (string-trim s))))
          s)))))

(defvar carriage-context--project-map-trace-installed nil
  "Non-nil when Project Map trace advices were installed for this Emacs session.")

(defun carriage-context--pm--trace-wrap (fn)
  "Return an around-advice wrapper for FN that logs timing/result summary.

Always logs failures/empty results. Logs successes only when
`carriage-context-project-map-trace' is non-nil."
  (lambda (orig &rest args)
    (let* ((t0 (float-time))
           (root (ignore-errors (carriage-context--project-root)))
           (ret nil)
           (err nil))
      (condition-case e
          (setq ret (apply orig args))
        (error
         ;; Project-map helpers should be best-effort and must not break UI/send.
         (setq err e)
         (setq ret nil)))
      (let* ((elapsed (truncate (* 1000 (max 0.0 (- (float-time) t0)))))
             (len (carriage-context--pm--len ret))
             (kind (cond
                    (err 'error)
                    ((null ret) 'nil)
                    ((stringp ret) 'string)
                    ((listp ret) 'list)
                    ((hash-table-p ret) 'hash)
                    (t 'other)))
             (dbg (list :fn fn :root root :elapsed-ms elapsed :kind kind :len len)))
        (carriage-context--pm-note dbg)
        ;; Always log when empty/error; log successes only in trace mode.
        (when (or carriage-context-project-map-trace
                  err
                  (memq kind '(nil error))
                  ;; Special: empty begin_map block string still looks like "string" but is tiny.
                  (and (eq fn 'carriage-context-project-map-block)
                       (stringp ret)
                       (< (length ret) 40)))
          (carriage-context--pm-log "%s: root=%s elapsed=%sms kind=%s len=%s%s"
                                    fn (or root "-") elapsed kind len
                                    (if err
                                        (format " err=%s" (error-message-string err))
                                      ""))))
      ret)))

(unless carriage-context--project-map-trace-installed
  (setq carriage-context--project-map-trace-installed t)
  ;; Attach tracing to whichever functions exist in the current build.
  (dolist (fn '(carriage-context-project-map-block
                carriage-context--project-map--get-file-list
                carriage-context--project-map--git-ls-files
                carriage-context--project-map--fd-files
                carriage-context--project-map--emacs-files
                carriage-context--project-map--render
                carriage-context--project-map--build))
    (when (fboundp fn)
      (advice-add fn :around (carriage-context--pm--trace-wrap fn)))))

(provide 'carriage-context)
;;; carriage-context.el ends here
