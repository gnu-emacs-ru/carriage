;;; carriage-context-plan.el --- begin_context_plan compiler (v1)  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Carriage contributors
;; Author: Carriage Team
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: tools, context
;;
;; Specifications:
;;   spec/context-plan-v1.org
;;   spec/context-integration-v2.org
;;   spec/security-v2.org
;;   spec/code-style-v2.org
;;   spec/errors-v2.org
;;
;;; Commentary:
;; Implements begin_context_plan DSL compiler that materializes into begin_context.
;; The collector reads ONLY begin_context; plans are compiled explicitly by user action.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-utils)
(require 'carriage-logging)

(declare-function carriage-project-root "carriage-utils" ())
(declare-function carriage--call-git "carriage-utils" (default-dir &rest args))

(defgroup carriage-context-plan nil
  "Compile begin_context_plan blocks into begin_context blocks."
  :group 'carriage
  :prefix "carriage-context-plan-")

(defcustom carriage-context-plan-max-files 500
  "Maximum number of files to write into the materialized begin_context block."
  :type 'integer
  :group 'carriage-context-plan)

(defvar carriage-context-plan--repo-files-cache (make-hash-table :test 'equal)
  "Cache: root -> plist (:time float :files (list-of-relpaths)).")

(defcustom carriage-context-plan-repo-files-cache-ttl 2.0
  "TTL in seconds for repo file universe cache.
Compilation is user-triggered, so this is mostly a micro-optimization."
  :type 'number
  :group 'carriage-context-plan)

(defun carriage-context-plan--root-tru (root)
  "Return truenamed ROOT directory with trailing slash."
  (file-name-as-directory (file-truename (file-name-as-directory (expand-file-name root)))))

(defun carriage-context-plan--inside-root-tru-p (tru root-tru)
  "Return non-nil when TRU (truename file) is inside ROOT-TRU directory."
  (and (stringp tru)
       (string-prefix-p root-tru (file-name-as-directory tru))))

(defun carriage-context-plan--ensure-local (path what)
  "Signal user-error when PATH is remote. WHAT is a label string."
  (when (and (stringp path) (file-remote-p path))
    (user-error "CTXPLAN: remote/TRAMP path is not allowed (%s): %s" what path)))

(defun carriage-context-plan--ensure-git (root)
  "Ensure ROOT is a git repo; user-error otherwise."
  (carriage-context-plan--ensure-local root "root")
  (let* ((res (carriage--call-git root "rev-parse" "--is-inside-work-tree"))
         (exit (plist-get res :exit)))
    (unless (and (numberp exit) (zerop exit))
      (user-error "CTXPLAN_E_NOT_GIT: repository not detected at %s" root))
    t))

(defun carriage-context-plan--repo-files (root)
  "Return repo-relative file universe for ROOT (gitignore-aware).
Uses `git ls-files -co --exclude-standard'."
  (carriage-context-plan--ensure-git root)
  (let* ((now (float-time))
         (ttl (or carriage-context-plan-repo-files-cache-ttl 0.0))
         (ce (gethash root carriage-context-plan--repo-files-cache))
         (fresh (and (listp ce)
                     (listp (plist-get ce :files))
                     (numberp (plist-get ce :time))
                     (>= ttl 0.0)
                     (< (- now (plist-get ce :time)) ttl))))
    (if fresh
        (plist-get ce :files)
      (let* ((res (carriage--call-git root "ls-files" "-co" "--exclude-standard"))
             (exit (plist-get res :exit))
             (out (plist-get res :stdout))
             (err (string-trim (or (plist-get res :stderr) ""))))
        (unless (and (numberp exit) (zerop exit))
          (user-error "CTXPLAN_E_GIT: git ls-files failed (exit=%s): %s" exit err))
        (let* ((lines (split-string (or out "") "\n" t))
               ;; Defensive: exclude .git if ever present, and normalize separators.
               (files (cl-remove-if
                       (lambda (p)
                         (or (string-empty-p p)
                             (string-prefix-p ".git/" p)
                             (string= p ".git")))
                       (mapcar (lambda (s) (replace-regexp-in-string "\\\\" "/" (string-trim s)))
                               lines))))
          (puthash root (list :time now :files files) carriage-context-plan--repo-files-cache)
          files)))))

(defun carriage-context-plan--glob-normalize (pattern)
  "Normalize PATTERN per ctxplan rules (REQ-ctxplan-016)."
  (let* ((p (string-trim (or pattern ""))))
    (setq p (replace-regexp-in-string "\\`\\./+" "" p))
    ;; If no slash: treat as recursive in repo.
    (when (and (not (string-empty-p p))
               (not (string-match-p "/" p)))
      (setq p (concat "**/" p)))
    p))

(defun carriage-context-plan--glob->regexp (pattern)
  "Convert glob PATTERN into an Emacs regexp for repo-relative paths.

Semantics:
- '*' matches within segment (no '/')
- '?' matches one char within segment (no '/')
- '**/' matches zero or more directories (including empty)
- Remaining '**' (not followed by '/') matches any chars including '/'"
  (let* ((p (carriage-context-plan--glob-normalize pattern))
         (i 0)
         (n (length p))
         (acc (list "\\`")))
    (while (< i n)
      (let ((ch (aref p i)))
        (cond
         ;; "**/" → zero or more dirs (including empty)
         ((and (= ch ?*)
               (< (+ i 2) n)
               (= (aref p (1+ i)) ?*)
               (= (aref p (+ i 2)) ?/))
          (setq acc (cons "\\(?:[^/]+/\\)*" acc))
          (setq i (+ i 3)))
         ;; Bare "**" → anything (best-effort)
         ((and (= ch ?*)
               (< (1+ i) n)
               (= (aref p (1+ i)) ?*))
          (setq acc (cons ".*" acc))
          (setq i (+ i 2)))
         ;; "*"
         ((= ch ?*)
          (setq acc (cons "[^/]*" acc))
          (setq i (1+ i)))
         ;; "?"
         ((= ch ??)
          (setq acc (cons "[^/]" acc))
          (setq i (1+ i)))
         ;; "/" (path separator)
         ((= ch ?/)
          (setq acc (cons "/" acc))
          (setq i (1+ i)))
         (t
          (setq acc (cons (regexp-quote (char-to-string ch)) acc))
          (setq i (1+ i))))))
    (setq acc (cons "\\'" acc))
    (apply #'concat (nreverse acc))))

(defun carriage-context-plan--directive-keyword (s)
  "Return uppercase directive keyword (string) for line S, or nil."
  (let* ((ln (string-trim-left (or s ""))))
    (when (string-match "\\`\\([A-Za-z_]+\\)\\b" ln)
      (upcase (match-string 1 ln)))))

(defun carriage-context-plan--parse-line (line)
  "Parse a single DSL LINE into an internal directive plist, or nil (ignored line).
Directive plist keys:
- :type  symbol (glob|path|contains|regex)
- :arg   string
- :in    optional glob string"
  (let* ((s0 (string-trim (or line ""))))
    (cond
     ((string-empty-p s0) nil)
     ((or (string-prefix-p "#" s0) (string-prefix-p ";;" s0)) nil)
     (t
      (let* ((kw (carriage-context-plan--directive-keyword s0))
             (rest (if kw (string-trim (substring s0 (length kw))) s0)))
        (cond
         ((or (null kw) (not (member kw '("GLOB" "PATH" "CONTAINS" "REGEX"))))
          ;; Default directive: GLOB <line>
          (list :type 'glob :arg s0))
         ((string= kw "GLOB")
          (when (string-empty-p rest)
            (user-error "CTXPLAN_E_PARSE: GLOB requires a pattern"))
          (list :type 'glob :arg rest))
         ((string= kw "PATH")
          (when (string-empty-p rest)
            (user-error "CTXPLAN_E_PARSE: PATH requires a path"))
          (list :type 'path :arg rest))
         ((string= kw "CONTAINS")
          (when (string-empty-p rest)
            (user-error "CTXPLAN_E_PARSE: CONTAINS requires a substring"))
          ;; Optional "IN <glob>" split (case-sensitive IN keyword).
          (let* ((parts (split-string rest "\\s-+IN\\s-+" t))
                 (sub (string-trim (car parts)))
                 (in (and (cadr parts) (string-trim (cadr parts)))))
            (when (or (string-empty-p sub) (and in (string-empty-p in)))
              (user-error "CTXPLAN_E_PARSE: malformed CONTAINS ... IN ..."))
            (if in
                (list :type 'contains :arg sub :in in)
              (list :type 'contains :arg sub))))
         ((string= kw "REGEX")
          (when (string-empty-p rest)
            (user-error "CTXPLAN_E_PARSE: REGEX requires a regexp"))
          (let* ((parts (split-string rest "\\s-+IN\\s-+" t))
                 (re (string-trim (car parts)))
                 (in (and (cadr parts) (string-trim (cadr parts)))))
            (when (or (string-empty-p re) (and in (string-empty-p in)))
              (user-error "CTXPLAN_E_PARSE: malformed REGEX ... IN ..."))
            (if in
                (list :type 'regex :arg re :in in)
              (list :type 'regex :arg re))))
         (t
          (user-error "CTXPLAN_E_PARSE: unknown directive: %s" kw))))))))

(defun carriage-context-plan--parse-lines (lines)
  "Parse DSL LINES into a list of directives."
  (let ((acc '()))
    (dolist (ln lines (nreverse acc))
      (let ((d (carriage-context-plan--parse-line ln)))
        (when d (push d acc))))))

(defun carriage-context-plan--expand-path-to-rel (path root root-tru warnings)
  "Validate PATH and return repo-relative path or nil. May push to WARNINGS.
Returns (cons rel warnings)."
  (let ((ws warnings))
    (cond
     ((or (null path) (string-empty-p (string-trim path)))
      (cons nil ws))
     ((file-remote-p path)
      (push (format "CTXPLAN_W_REMOTE: rejected remote path: %s" path) ws)
      (cons nil ws))
     (t
      (let* ((abs (if (file-name-absolute-p path)
                      (expand-file-name path)
                    (expand-file-name path root)))
             (tru (ignore-errors (file-truename abs))))
        (cond
         ((null tru)
          (push (format "CTXPLAN_W_MISSING: path does not resolve: %s" path) ws)
          (cons nil ws))
         ((not (carriage-context-plan--inside-root-tru-p tru root-tru))
          (push (format "CTXPLAN_W_OUTSIDE_ROOT: %s" path) ws)
          (cons nil ws))
         ((not (file-regular-p tru))
          (push (format "CTXPLAN_W_NOT_FILE: %s" (file-relative-name tru root)) ws)
          (cons nil ws))
         (t
          (cons (file-relative-name tru root) ws))))))))

(defun carriage-context-plan--filter-universe-safe (universe root root-tru)
  "Filter UNIVERSE to files that exist and are truename-inside root.
Returns plist: (:files list :excluded int :warnings list)."
  (let ((acc '())
        (excluded 0)
        (warns '()))
    (dolist (rel universe)
      (let* ((abs (expand-file-name rel root))
             (tru (ignore-errors (file-truename abs))))
        (cond
         ((null tru)
          (setq excluded (1+ excluded))
          (push (format "CTXPLAN_W_MISSING: %s" rel) warns))
         ((not (carriage-context-plan--inside-root-tru-p tru root-tru))
          (setq excluded (1+ excluded))
          (push (format "CTXPLAN_W_OUTSIDE_ROOT: %s" rel) warns))
         ((not (file-regular-p tru))
          (setq excluded (1+ excluded))
          (push (format "CTXPLAN_W_NOT_FILE: %s" rel) warns))
         (t
          (push rel acc)))))
    (list :files (nreverse acc) :excluded excluded :warnings (nreverse warns))))

(defun carriage-context-plan--eval-directives (directives universe root root-tru)
  "Evaluate DIRECTIVES over UNIVERSE.
Return plist: (:files list :matched int :excluded int :warnings list)."
  (let* ((seen (make-hash-table :test 'equal))
         (warnings '())
         (matched 0)
         (excluded 0))
    (cl-labels
        ((add-file
          (rel)
          (when (and (stringp rel) (not (string-empty-p rel)))
            (unless (gethash rel seen)
              (puthash rel t seen)
              (setq matched (1+ matched)))))
         (warn
          (msg) (when (stringp msg) (push msg warnings)))
         (select-by-glob
          (glob files)
          (let* ((re (carriage-context-plan--glob->regexp glob))
                 (acc '()))
            (dolist (p files (nreverse acc))
              (when (string-match-p re p) (push p acc)))))
         (rule-no-match
          (d)
          (warn (format "CTXPLAN_W_NO_MATCH: %s %s"
                        (upcase (symbol-name (plist-get d :type)))
                        (string-trim (or (plist-get d :arg) ""))))))
      (dolist (d directives)
        (pcase (plist-get d :type)
          ('glob
           (let* ((glob (plist-get d :arg))
                  (hits (select-by-glob glob universe)))
             (when (null hits) (rule-no-match d))
             (dolist (p hits) (add-file p))))
          ('path
           (let* ((r (carriage-context-plan--expand-path-to-rel
                      (plist-get d :arg) root root-tru warnings))
                  (rel (car r)))
             (setq warnings (cdr r))
             (if (not (stringp rel))
                 (rule-no-match d)
               (add-file rel))))
          ('contains
           (let* ((sub (or (plist-get d :arg) ""))
                  (in (plist-get d :in))
                  (base (if (stringp in) (select-by-glob in universe) universe))
                  (hits (cl-remove-if-not (lambda (p) (string-match-p (regexp-quote sub) p)) base)))
             (when (null hits) (rule-no-match d))
             (dolist (p hits) (add-file p))))
          ('regex
           (let* ((rx (or (plist-get d :arg) ""))
                  (in (plist-get d :in))
                  (base (if (stringp in) (select-by-glob in universe) universe))
                  (hits (cl-remove-if-not (lambda (p) (ignore-errors (string-match-p rx p))) base)))
             (when (null hits) (rule-no-match d))
             (dolist (p hits) (add-file p))))
          (_
           (user-error "CTXPLAN_E_PARSE: unknown directive node: %S" d)))))
    (let* ((files (sort (hash-table-keys seen) #'string<)))
      (list :files files :matched matched :excluded excluded :warnings (nreverse warnings)))))

(defun carriage-context-plan--bounds-at-point ()
  "Return (beg . end) bounds of the begin_context_plan block around point, or nil.
BEG/END are buffer positions of the marker lines' beginnings."
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search t)
            (pt (point))
            beg end)
        (when (re-search-backward "^[ \t]*#\\+begin_context_plan\\b" nil t)
          (setq beg (line-beginning-position))
          (goto-char beg)
          (when (re-search-forward "^[ \t]*#\\+end_context_plan\\b" nil t)
            (setq end (line-beginning-position))
            (when (and (numberp beg) (numberp end) (>= pt beg) (<= pt (line-end-position)))
              ;; pt was on end line; accept.
              (cons beg end))
            (when (and (numberp beg) (numberp end) (>= pt beg) (<= pt (save-excursion (goto-char end) (line-end-position))))
              (cons beg end))
            (when (and (numberp beg) (numberp end) (>= pt beg) (< pt end))
              (cons beg end))))))))

(defun carriage-context-plan--plan-lines (beg end)
  "Return plan body lines between BEG (begin marker) and END (end marker) lines."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char beg)
      (forward-line 1)
      (let ((body-beg (point)))
        (goto-char end)
        (let ((body-end (line-beginning-position)))
          (split-string (buffer-substring-no-properties body-beg body-end) "\n" nil))))))

(defun carriage-context-plan--write-begin-context-body (files)
  "Return text for begin_context body from FILES (paths only, one per line).
Empty FILES yields empty string."
  (if (null files)
      ""
    (concat (mapconcat #'identity files "\n") "\n")))

(defun carriage-context-plan--upsert-materialized-block (plan-end files)
  "Insert/update begin_context immediately after plan block end (REQ-ctxplan-002).
PLAN-END is the position of the begin_context_plan end marker line (its BOL)."
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search t)
            (body (carriage-context-plan--write-begin-context-body files)))
        ;; Move to the first line after end_context_plan.
        (goto-char plan-end)
        (forward-line 1)
        ;; Allow only blank lines between plan and begin_context.
        (while (and (not (eobp))
                    (looking-at-p "^[ \t]*$"))
          (forward-line 1))
        (cond
         ;; Update existing immediate begin_context
         ((looking-at-p "^[ \t]*#\\+begin_context\\b")
          (let ((beg-line (line-beginning-position)))
            (forward-line 1)
            (let ((body-beg (point))
                  (body-end
                   (if (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
                       (line-beginning-position)
                     ;; Missing end marker: synthesize one at end of buffer.
                     (goto-char (point-max))
                     (unless (bolp) (insert "\n"))
                     (insert "#+end_context\n")
                     (forward-line -1)
                     (line-beginning-position))))
              (delete-region body-beg body-end)
              (goto-char body-beg)
              (insert body)
              (goto-char beg-line))))
         ;; Create a new begin_context block at the insertion point (current line).
         (t
          (let ((ins (point)))
            (goto-char ins)
            (unless (bolp) (insert "\n"))
            (insert "#+begin_context\n")
            (insert body)
            (insert "#+end_context\n"))))))))

;;;###autoload
(defun carriage-context-plan-compile (&optional buffer)
  "Compile the begin_context_plan at point in BUFFER into a begin_context block.
Return result plist:
  (:ok t|nil :files (list) :warnings (list) :stats (:matched N :excluded N :truncated N))"
  (with-current-buffer (or buffer (current-buffer))
    (carriage-context-plan--ensure-local default-directory "buffer")
    (let* ((root (or (and (fboundp 'carriage-project-root) (carriage-project-root))
                     default-directory)))
      (carriage-context-plan--ensure-git root)
      (let* ((bounds (carriage-context-plan--bounds-at-point)))
        (unless bounds
          (user-error "CTXPLAN: no begin_context_plan block at point"))
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (lines (carriage-context-plan--plan-lines beg end))
               (directives (carriage-context-plan--parse-lines lines))
               (root-tru (carriage-context-plan--root-tru root))
               (univ0 (carriage-context-plan--repo-files root))
               (u1 (carriage-context-plan--filter-universe-safe univ0 root root-tru))
               (universe (plist-get u1 :files))
               (excluded0 (or (plist-get u1 :excluded) 0))
               (warn0 (or (plist-get u1 :warnings) '()))
               (eval (carriage-context-plan--eval-directives directives universe root root-tru))
               (files0 (or (plist-get eval :files) '()))
               (matched (or (plist-get eval :matched) 0))
               (excluded (+ excluded0 (or (plist-get eval :excluded) 0)))
               (warnings (append warn0 (or (plist-get eval :warnings) '())))
               (maxf (max 0 (or carriage-context-plan-max-files 500)))
               (truncated 0)
               (files files0))
          (when (and (numberp maxf) (> maxf 0) (> (length files) maxf))
            (setq truncated (- (length files) maxf))
            (setq files (cl-subseq files 0 maxf))
            (push (format "CTXPLAN_W_LIMIT: truncated by max-files=%d" maxf) warnings))
          ;; Upsert begin_context immediately after plan.
          (carriage-context-plan--upsert-materialized-block end files)
          ;; Diagnostics.
          (message "Context plan: compiled %d files (excluded %d, truncated %d)"
                   (length files) excluded truncated)
          (when warnings
            (let* ((ws (nreverse warnings))
                   (head (cl-subseq ws 0 (min 5 (length ws))))
                   (more (- (length ws) (length head))))
              (message "Context plan warnings: %s%s"
                       (string-join head " | ")
                       (if (> more 0) (format " | … (+%d more)" more) ""))))
          (list :ok t
                :files files
                :warnings (nreverse warnings)
                :stats (list :matched matched :excluded excluded :truncated truncated)))))))

;;;###autoload
(defun carriage-context-plan-compile-at-point ()
  "Interactive: compile begin_context_plan at point into begin_context."
  (interactive)
  (carriage-context-plan-compile (current-buffer)))

(provide 'carriage-context-plan)
;;; carriage-context-plan.el ends here
