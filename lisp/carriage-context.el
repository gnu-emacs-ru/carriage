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
  "Scope for document (# +begin_context) collection: 'all or 'last.
When 'all, collect paths from all #+begin_context blocks in the buffer.
When 'last, collect paths only from the nearest preceding block relative to point,
or the last block in the buffer if none precedes point."
  :type '(choice (const all) (const last))
  :group 'carriage-context)
(make-variable-buffer-local 'carriage-doc-context-scope)

(defcustom carriage-mode-include-patched-files nil
  "When non-nil, include files referenced by applied begin_patch blocks (:applied t) in the current buffer into the document context."
  :type 'boolean
  :group 'carriage-context)
(make-variable-buffer-local 'carriage-mode-include-patched-files)

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
  "Return list of file paths extracted from applied patches in BUFFER.

Only considers annotated begin_patch blocks with (:applied t ...):
- op 'patch → take :path
- op 'sre/'aibo → take :file"
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
            (acc '()))
        (while (re-search-forward "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" nil t)
          (let* ((sexp-str (match-string 1))
                 (plist (condition-case _e
                            (car (read-from-string sexp-str))
                          (error nil)))
                 (applied (and (listp plist) (plist-get plist :applied)))
                 (op      (and (listp plist) (plist-get plist :op)))
                 (opstr   (and op (format "%s" op))))
            (when applied
              (let* ((is-patch (and opstr (string= (replace-regexp-in-string "^:" "" opstr) "patch")))
                     (key (if is-patch :path :file))
                     (val (and (listp plist) (plist-get plist key))))
                (when (and (stringp val) (not (string-empty-p val)))
                  (push val acc))))))
        (nreverse (delete-dups acc))))))

;; Commands to switch scope (used by UI/keyspec)
;;;###autoload
(defun carriage-select-doc-context-all ()
  "Use all #+begin_context blocks for document context in this buffer."
  (interactive)
  (setq-local carriage-doc-context-scope 'all)
  (force-mode-line-update))

;;;###autoload
(defun carriage-select-doc-context-last ()
  "Use only the last/nearest #+begin_context block for document context in this buffer."
  (interactive)
  (setq-local carriage-doc-context-scope 'last)
  (force-mode-line-update))

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
- 'last: collect paths only from the last block above point; if none above,
         use the last block in the buffer."
  (with-current-buffer buffer
    (save-excursion
      (let* ((scope (or (and (boundp 'carriage-doc-context-scope)
                             carriage-doc-context-scope)
                        'all)))
        (pcase scope
          ('all
           (delete-dups
            (delq nil
                  (carriage-context--find-context-block-in-region (point-min) (point-max)))))
          (_
           ;; Find all block ranges, pick the nearest one starting before point.
           (let* ((pt (point))
                  (ranges
                   (save-excursion
                     (goto-char (point-min))
                     (let ((acc '())
                           (case-fold-search t))
                       (while (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t)
                         (let ((beg (match-beginning 0)))
                           (if (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
                               (let ((end (line-beginning-position)))
                                 (push (cons beg end) acc))
                             (push (cons beg (point-max)) acc))))
                       (nreverse acc))))
                  (choice
                   (or
                    ;; Best candidate: last range whose beg <= point
                    (let ((best nil)
                          (best-beg nil))
                      (dolist (rg ranges)
                        (when (<= (car rg) pt)
                          (when (or (null best-beg) (> (car rg) best-beg))
                            (setq best-beg (car rg))
                            (setq best rg))))
                      best)
                    ;; Fallback: last block in the buffer
                    (car (last ranges)))))
             (if (consp choice)
                 (delete-dups
                  (delq nil
                        (carriage-context--find-context-block-in-region (car choice) (cdr choice))))
               '()))))))))

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
              t)
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
Supports new and legacy variables; defaults ON."
  (with-current-buffer buf
    (let* ((have-gpt-new (boundp 'carriage-mode-include-gptel-context))
           (have-gpt-old (boundp 'carriage-mode-use-context))
           (have-doc-new (boundp 'carriage-mode-include-doc-context))
           (have-doc-old (boundp 'carriage-mode-context-attach-files))
           (inc-gpt (if (or have-gpt-new have-gpt-old)
                        (or (and have-gpt-new (buffer-local-value 'carriage-mode-include-gptel-context buf))
                            (and have-gpt-old (buffer-local-value 'carriage-mode-use-context buf)))
                      t))
           (inc-doc (if (or have-doc-new have-doc-old)
                        (or (and have-doc-new (buffer-local-value 'carriage-mode-include-doc-context buf))
                            (and have-doc-old (buffer-local-value 'carriage-mode-context-attach-files buf)))
                      t)))
      (cons inc-gpt inc-doc))))

(defun carriage-context--count-root+toggles (buf)
  "Return plist with root and inclusion toggles for BUF: (:root R :inc-gpt B :inc-doc B :inc-vis B :inc-patched B)."
  (let* ((root (carriage-context--project-root))
         (inc-gpt (with-current-buffer buf
                    (or (and (boundp 'carriage-mode-include-gptel-context)
                             (buffer-local-value 'carriage-mode-include-gptel-context buf))
                        (and (boundp 'carriage-mode-use-context)
                             (buffer-local-value 'carriage-mode-use-context buf))
                        t)))
         (inc-doc (with-current-buffer buf
                    (or (and (boundp 'carriage-mode-include-doc-context)
                             (buffer-local-value 'carriage-mode-include-doc-context buf))
                        (and (boundp 'carriage-mode-context-attach-files)
                             (buffer-local-value 'carriage-mode-context-attach-files buf))
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
             :included (stringp content)
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

(defun carriage-context-count (&optional buffer _point)
  "Вернуть plist со счётчиком элементов контекста для BUFFER.

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
             (doc-trues (plist-get true-data :doc-trues))
             (gpt-trues (plist-get true-data :gpt-trues))
             (vis-trues (plist-get true-data :vis-trues))
             (doc-map (plist-get true-data :doc-map))
             (gpt-map (plist-get true-data :gpt-map))
             (vis-map (plist-get true-data :vis-map))
             (col (carriage-context-collect buf root))
             (files (or (plist-get col :files) '()))
             (warnings (or (plist-get col :warnings) '()))
             (stats (or (plist-get col :stats) '()))
             (_ (carriage-context--dbg "count: collect: files=%s included=%s skipped=%s total-bytes=%s warnings=%s"
                                       (length files)
                                       (or (plist-get stats :included) 0)
                                       (or (plist-get stats :skipped) 0)
                                       (or (plist-get stats :total-bytes) 0)
                                       (length warnings)))
             (items (carriage-context--count-build-items files doc-map gpt-map vis-map)))
        (carriage-context--dbg "count: doc=%s gpt=%s vis=%s files=%s items=%s warns=%s"
                               (length doc-trues) (length gpt-trues) (length vis-trues)
                               (length files) (length items) (length warnings))
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

(provide 'carriage-context)
;;; carriage-context.el ends here
