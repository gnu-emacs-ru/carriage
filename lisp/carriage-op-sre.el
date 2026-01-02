;;; carriage-op-sre.el --- SRE (search/replace) op  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: sre, replace
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/file-header-format-v2.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/sre-v2.org
;;   spec/parser-impl-v2.org
;;   spec/parser-registry-v2.org
;;
;;; Commentary:
;; Parser/dry-run/apply implementations for SRE v1 blocks.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-errors)
(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
(require 'carriage-format-registry)
;; Engine selector (used to gate staging for non-git engines)
(declare-function carriage-apply-engine "carriage-apply-engine" ())

(defvar carriage-mode-sre-preview-max 3
  "Default maximum number of SRE preview chunks when Customize is not loaded.")

;; Helpers to read plan items/pairs regardless of plist|alist representation.
(defun carriage--plan-get (item key)
  "Get KEY from ITEM supporting both plist and alist representations."
  (if (and (listp item) (plist-member item key))
      (plist-get item key)
    (alist-get key item)))

(defun carriage--pair-get (pair key)
  "Get KEY from PAIR supporting both plist and alist pair representations."
  (if (and (listp pair) (plist-member pair key))
      (plist-get pair key)
    (alist-get key pair)))

;;;; Prompt fragment (begin_from/begin_to)

(defun carriage-op-sre-prompt-fragment (_ctx)
  "Return prompt fragment for SRE v1 (begin_from/begin_to)."
  (concat
   "SRE (1..N pairs for one file):\n"
   "#+begin_patch (:version \"1\" :op \"sre\" :file \"RELATIVE/PATH\" :description \"Короткое описание\")\n"
   "#+pair (:occur all :expect K :match regex) ; optional, applies to the NEXT pair\n"
   "#+begin_from\nFROM text\n#+end_from\n"
   "#+begin_to\nTO text\n#+end_to\n"
   "#+end_patch\n"
   "- For :occur all, :expect is required.\n"
   "- Empty TO block is allowed (delete matched text).\n"
"- If a line inside a block is exactly \"#+end_from\" or \"#+end_to\", add one leading space to escape it.\n"
"- When enabled (carriage-mode-require-patch-description=t), include :description in #+begin_patch header.\n"))

;;;; Internal helpers (SRE core)

(defun carriage--sre-make-regexp (from match-kind)
  "Return regexp for FROM according to MATCH-KIND ('literal or 'regex)."
  (if (eq match-kind 'regex) from (regexp-quote (or from ""))))

(defun carriage--sre-count-nonoverlapping (text regexp)
  "Count non-overlapping matches of REGEXP in TEXT.
Guard against zero-length matches."
  (let ((pos 0) (cnt 0))
    (while (and (< pos (length text))
                (string-match regexp text pos))
      (setq cnt (1+ cnt))
      (let ((next (match-end 0)))
        (setq pos (if (= next pos) (1+ pos) next))))
    cnt))

(defun carriage--sre-slice-by-lines (text range-plist)
  "Return (PRE REGION POST) for TEXT restricted by RANGE-PLIST (:start-line N :end-line M)."
  (if (not range-plist)
      (list "" text "")
    (let* ((start (plist-get range-plist :start-line))
           (end   (plist-get range-plist :end-line)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let* ((start-line (max 1 (or start 1)))
               (end-line (max start-line (or end (line-number-at-pos (point-max)))))
               (beg (progn (goto-char (point-min)) (forward-line (1- start-line)) (point)))
               (endp (progn (goto-char (point-min)) (forward-line end-line) (point))))
          (list (buffer-substring-no-properties (point-min) beg)
                (buffer-substring-no-properties beg endp)
                (buffer-substring-no-properties endp (point-max))))))))

(defun carriage--sre-effective-range (text range-plist)
  "Return normalized range plist (:start-line N :end-line M :clamped t/nil) within TEXT."
  (if (not range-plist)
      nil
    (let* ((start0 (plist-get range-plist :start-line))
           (end0   (plist-get range-plist :end-line))
           (total-lines (with-temp-buffer
                          (insert text)
                          (goto-char (point-max))
                          (line-number-at-pos (point-max))))
           (s (max 1 (or start0 1)))
           (e (or end0 total-lines))
           (s1 (min s (max 1 total-lines)))
           (e1 (min (max s1 e) total-lines))
           (clamped (or (not (equal s1 s)) (not (equal e1 e)))))
      (list :start-line s1 :end-line e1 :clamped (and clamped t)))))

(defun carriage--sre-replace-first (text regexp to replacement-literal-p)
  "Replace first match of REGEXP in TEXT with TO. Return (NEW . COUNT)."
  (if (not (string-match regexp text))
      (cons text 0)
    ;; FIXEDCASE=t to prevent case-munging of replacement based on match
    (cons (replace-match to t replacement-literal-p text) 1)))

(defun carriage--sre-replace-all (text regexp to replacement-literal-p)
  "Replace all matches of REGEXP in TEXT with TO. Return (NEW . COUNT)."
  (let ((count (carriage--sre-count-nonoverlapping text regexp))
        ;; FIXEDCASE=t to prevent case-munging of replacement based on match
        (new (replace-regexp-in-string regexp to text t replacement-literal-p)))
    (cons new count)))

(defun carriage--sre-count-matches (text from opts)
  "Count matches of FROM in TEXT honoring OPTS (:match, :occur)."
  (let* ((match-kind (or (plist-get opts :match) 'literal))
         (occur (or (plist-get opts :occur) 'first))
         (rx (carriage--sre-make-regexp from match-kind)))
    (cond
     ((eq occur 'first)
      (if (string-match rx text) 1 0))
     ((eq occur 'all)
      (carriage--sre-count-nonoverlapping text rx))
     (t (carriage--sre-count-nonoverlapping text rx)))))

(defun carriage--sre-first-match-pos (text regexp)
  "Return cons of (START . END) for first match of REGEXP in TEXT, or nil."
  (let ((p (string-match regexp text)))
    (and p (cons p (match-end 0)))))

(defun carriage--sre-line-bounds-at (text pos)
  "Return cons (LINE-START . LINE-END) bounds around POS in TEXT (without newline)."
  (let ((start pos) (end pos)
        (len (length text)))
    (while (and (> start 0) (not (eq (aref text (1- start)) ?\n)))
      (setq start (1- start)))
    (while (and (< end len) (not (eq (aref text end) ?\n)))
      (setq end (1+ end)))
    (cons start end)))

(defun carriage--sre-prev-line-bounds (text ls)
  "Return bounds (START . END) of the previous line before LS in TEXT, or nil."
  (when (> ls 0)
    (let ((i (1- ls)))
      (while (and (>= i 0) (not (eq (aref text i) ?\n)))
        (setq i (1- i)))
      (when (>= i 0)
        (let ((j (1- i)))
          (while (and (>= j 0) (not (eq (aref text j) ?\n)))
            (setq j (1- j)))
          (cons (1+ j) i))))))

(defun carriage--sre-next-line-bounds (text le)
  "Return bounds (START . END) of the next line after LE in TEXT, or nil."
  (let ((len (length text)))
    (when (< le len)
      (let ((i le))
        (when (and (< i len) (eq (aref text i) ?\n))
          (setq i (1+ i)))
        (when (< i len)
          (let ((j i))
            (while (and (< j len) (not (eq (aref text j) ?\n)))
              (setq j (1+ j)))
            (cons i j)))))))

(defun carriage--sre-build-preview-at (region start end to match-kind)
  "Build a single-line preview for REGION by replacing the match [START,END) with TO."
  (ignore match-kind)
  (let* ((lb (carriage--sre-line-bounds-at region start))
         (ls (car lb)) (le (cdr lb))
         (line (substring region ls le))
         (end1 (min end le))
         (s (- start ls))
         (e (- end1 ls))
         (before (substring line 0 s))
         (after (substring line e))
         (mid-rep to))
    (format "-%s\n+%s" line (concat before mid-rep after))))

(defun carriage--sre-build-preview-with-context (region start end to match-kind context-lines)
  "Build preview with CONTEXT-LINES lines above and below the changed line."
  (let* ((lb (carriage--sre-line-bounds-at region start))
         (ls (car lb)) (le (cdr lb))
         (plus-minus (carriage--sre-build-preview-at region start end to match-kind))
         (before-bounds '())
         (after-bounds '())
         (prev-ls ls)
         (next-le le))
    (dotimes (_ context-lines)
      (let ((pb (carriage--sre-prev-line-bounds region prev-ls)))
        (when pb (push pb before-bounds) (setq prev-ls (car pb)))))
    (dotimes (_ context-lines)
      (let ((nb (carriage--sre-next-line-bounds region next-le)))
        (when nb (push nb after-bounds) (setq next-le (cdr nb)))))
    (let ((parts '()))
      (dolist (b (nreverse before-bounds))
        (push (substring region (car b) (cdr b)) parts))
      (push plus-minus parts)
      (dolist (b (nreverse after-bounds))
        (push (substring region (car b) (cdr b)) parts))
      (mapconcat #'identity (nreverse parts) "\n"))))

(defun carriage--sre-previews-for-region (region rx to match-kind occur count)
  "Return list of preview chunks for REGION using RX/TO; OCCUR is 'first or 'all."
  (ignore count)
  (let ((maxn (or (and (boundp 'carriage-mode-sre-preview-max) carriage-mode-sre-preview-max) 3))
        (ctx (or (and (boundp 'carriage-mode-sre-preview-context-lines)
                      carriage-mode-sre-preview-context-lines)
                 0)))
    (pcase occur
      ('first
       (let* ((mpos (carriage--sre-first-match-pos region rx))
              (pv (when mpos
                    (carriage--sre-build-preview-with-context region (car mpos) (cdr mpos) to match-kind ctx))))
         (if pv (list pv) '())))
      (_
       (let* ((pos-list (let ((pos 0) (acc '()))
                          (while (and (< pos (length region))
                                      (string-match rx region pos)
                                      (< (length acc) (max 0 (or maxn 0))))
                            (push (cons (match-beginning 0) (match-end 0)) acc)
                            (let ((next (match-end 0)))
                              (setq pos (if (= next pos) (1+ pos) next))))
                          (nreverse acc)))
              (previews (mapcar (lambda (p)
                                  (carriage--sre-build-preview-with-context region (car p) (cdr p) to match-kind ctx))
                                pos-list)))
         previews)))))

;;;; Parse SRE (begin_from/begin_to)

(defun carriage--sre-default-opts ()
  "Return default SRE pair options plist."
  '(:occur first :match literal))

(defun carriage--sre-merge-opts (pending)
  "Merge PENDING opts plist with defaults."
  (let ((defs (carriage--sre-default-opts)))
    (cl-loop for (k v) on pending by #'cddr do (setq defs (plist-put defs k v)))
    defs))

(defun carriage--sre-parse-pair-directive (line)
  "If LINE is a #+pair directive, return its plist; else nil."
  (when (string-match "\\=\\s-*#\\+pair\\s-+\\((.*)\\)\\s-*\\'" line)
    (car (read-from-string (match-string 1 line)))))

(defun carriage--sre-validate-header (hdr)
  "Validate SRE HDR plist for op='sre' and version '1'."
  (let ((version (plist-get hdr :version))
        (op (plist-get hdr :op))
        (file (plist-get hdr :file)))
    (unless (string= version "1")
      (signal (carriage-error-symbol 'SRE_E_VERSION) (list version)))
    (unless (member op '(sre 'sre "sre"))
      (signal (carriage-error-symbol 'SRE_E_OP) (list op)))
    (unless (and (stringp file) (not (string-empty-p file)))
      (signal (carriage-error-symbol 'SRE_E_PATH) (list file)))
    t))

(defun carriage--sre-extract-block (lines idx begin end)
  "Extract payload from LINES starting at IDX after BEGIN marker up to END marker.
Return cons (PAYLOAD . NEXT-INDEX). Applies single-space unescape for end markers."
  (let ((acc '())
        (i idx)
        (n (length lines)))
    (while (< i n)
      (let ((ln (nth i lines)))
        (cond
         ((string= (string-trim ln) begin)
          ;; Nested begin is not allowed; treat literally
          (push ln acc)
          (setq i (1+ i)))
         ((string-match (format "\\=[ \t]*%s[ \t]*\\'" (regexp-quote end)) ln)
          (cl-return-from carriage--sre-extract-block
            (cons (mapconcat #'identity (nreverse acc) "\n") (1+ i))))
         (t
          ;; Unescape: a line that is exactly one leading space + end marker becomes end marker
          (if (and (>= (length ln) (1+ (length end)))
                   (eq (aref ln 0) ?\s)
                   (string= (substring ln 1) end))
              (push end acc)
            (push ln acc))
          (setq i (1+ i))))))
    (signal (carriage-error-symbol 'SRE_E_UNCLOSED_BLOCK) (list (format "Unclosed %s" end)))))

(defun carriage--sre--ensure-segment-limits (from to)
  "Signal limits error if FROM/TO exceed size limits."
  (when (> (string-bytes from) (* 512 1024))
    (signal (carriage-error-symbol 'SRE_E_LIMITS) (list "FROM segment exceeds 512KiB")))
  (when (> (string-bytes to) (* 512 1024))
    (signal (carriage-error-symbol 'SRE_E_LIMITS) (list "TO segment exceeds 512KiB"))))

(defun carriage--sre--make-pair (from to pending)
  "Build normalized SRE pair with merged opts from PENDING."
  (let ((opts (carriage--sre-merge-opts (or pending '()))))
    (carriage--sre--ensure-segment-limits from to)
    ;; Empty FROM check (allow empty TO for deletions)
    (when (or (null from) (string-empty-p from))
      (signal (carriage-error-symbol 'SRE_E_EMPTY_SEGMENT) (list "Empty FROM segment")))
    ;; Reject unsupported PCRE-like constructs early (defensive).
    ;; This makes validator tests fail fast during parsing.
    (when (and (stringp from)
               (or (string-match-p (regexp-quote "(?<=") from)  ; lookbehind
                   (string-match-p (regexp-quote "(?<!") from)  ; negative lookbehind
                   (string-match-p (regexp-quote "(?>") from)   ; atomic group
                   (string-match-p (regexp-quote "(?|") from))) ; branch reset
      (signal (carriage-error-symbol 'SRE_E_REGEX_SYNTAX)
              (list "Unsupported regex construct (PCRE-style)")))
    (list (cons :from from) (cons :to to) (cons :opts opts))))

(defun carriage--sre-parse-linewise (lines)
  "Parse LINES and return cons (PAIRS . PENDING) where PAIRS is a list of pairs."
  (let ((i 0) (n (length lines))
        (pending nil)
        (pairs '()))
    (while (< i n)
      (let ((ln (nth i lines)))
        (cond
         ;; #+pair directive
         ((let ((pd (carriage--sre-parse-pair-directive ln)))
            (when pd (setq pending pd))
            pd)
          (setq i (1+ i)))
         ;; begin_from
         ((string-match "\\=\\s-*#\\+begin_from\\b" ln)
          (setq i (1+ i))
          (let* ((from-cons (carriage--sre-extract-block lines i "#+begin_from" "#+end_from"))
                 (from (car from-cons))
                 (i2 (cdr from-cons)))
            (unless (and (< i2 n)
                         (string-match "\\=\\s-*#\\+begin_to\\b" (nth i2 lines)))
              (signal (carriage-error-symbol 'SRE_E_UNPAIRED) (list "begin_from without following begin_to")))
            (let* ((i3 (1+ i2))
                   (to-cons (carriage--sre-extract-block lines i3 "#+begin_to" "#+end_to"))
                   (to (car to-cons))
                   (next (cdr to-cons)))
              (push (carriage--sre--make-pair from to pending) pairs)
              (setq pending nil)
              (setq i next))))
         (t
          (setq i (1+ i))))))
    (cons (nreverse pairs) pending)))

(defun carriage--sre-fallback-lines (lines pending)
  "Line-based tolerant fallback: extract first pair if all markers are present."
  (let* ((j1 (cl-position-if (lambda (s) (string-match "\\=\\s-*#\\+begin_from\\b" s)) lines))
         (j2 (and j1 (cl-position-if (lambda (s) (string-match "\\=\\s-*#\\+end_from\\b" s)) lines :start (1+ j1))))
         (j3 (and j2 (cl-position-if (lambda (s) (string-match "\\=\\s-*#\\+begin_to\\b" s)) lines :start (1+ j2))))
         (j4 (and j3 (cl-position-if (lambda (s) (string-match "\\=\\s-*#\\+end_to\\b" s)) lines :start (1+ j3)))))
    (when (and j1 j2 j3 j4 (< j1 j2) (< j3 j4))
      (let* ((from (mapconcat #'identity (cl-subseq lines (1+ j1) j2) "\n"))
             (to   (mapconcat #'identity (cl-subseq lines (1+ j3) j4) "\n")))
        (list (carriage--sre--make-pair from to pending))))))

(defun carriage--sre-fallback-string (body pending)
  "String-level tolerant fallback: extract first pair with CR/WS tolerance."
  (let* ((body-str body)
         (len (length body-str))
         (case-fold-search t)
         (re-begin-from "^[ \t]*#\\+begin_from\\b.*$")
         (re-end-from   "^[ \t]*#\\+end_from\\b.*$")
         (re-begin-to   "^[ \t]*#\\+begin_to\\b.*$")
         (re-end-to     "^[ \t]*#\\+end_to\\b.*$"))
    (save-match-data
      (let* ((b1  (string-match re-begin-from body-str))
             (b1e (and b1 (match-end 0)))                 ; end of begin_from line
             (ef  (and b1e (string-match re-end-from body-str b1e)))
             (efb (and ef (match-beginning 0)))           ; begin of end_from line
             (efe (and ef (match-end 0)))                 ; end of end_from line
             (bt  (and efe (string-match re-begin-to body-str efe)))
             (bte (and bt (match-end 0)))                 ; end of begin_to line
             (et  (and bte (string-match re-end-to body-str bte)))
             (etb (and et (match-beginning 0))))          ; begin of end_to line
        (when (and b1 ef bt et (< b1 efb) (< bt etb))
          (let* ((from-beg (min len (1+ b1e)))
                 (from-end (max from-beg (or efb from-beg)))
                 (to-beg   (min len (1+ bte)))
                 (to-end   (max to-beg (or etb to-beg)))
                 (from (substring body-str from-beg from-end))
                 (to   (substring body-str to-beg to-end)))
            (list (carriage--sre--make-pair from to pending))))))))

(defun carriage--sre-parse-body (body)
  "Parse BODY string into list of (:from STR :to STR :opts PLIST) pairs."
  (let* ((lines (split-string body "\n" nil nil))
         (res (carriage--sre-parse-linewise lines))
         (pairs (car res))
         (pending (cdr res)))
    (when (null pairs)
      (setq pairs (or (carriage--sre-fallback-lines lines pending)
                      (carriage--sre-fallback-string body pending))))
    (when (null pairs)
      (signal (carriage-error-symbol 'SRE_E_SEGMENTS_COUNT) (list 0)))
    pairs))

(defun carriage--sre-validate-pairs-opts (pairs)
  "Validate PAIRS options according to SRE rules; signal on violations."
  (dolist (p pairs)
    (let* ((opts (alist-get :opts p))
           (occur (plist-get opts :occur))
           (expect (plist-get opts :expect))
           (match-kind (plist-get opts :match))
           (mk (cond
                ((symbolp match-kind) match-kind)
                ((stringp match-kind) (intern (downcase match-kind)))
                (t nil))))
      ;; Strict :occur validation (v1.1)
      (let* ((oc (cond
                  ((symbolp occur) occur)
                  ((stringp occur) (intern (downcase occur)))
                  (t occur))))
        (unless (memq oc '(first all))
          (signal (carriage-error-symbol 'SRE_E_OCCUR_VALUE)
                  (list (format "Invalid :occur: %S (expected 'first|'all)" occur)))))
      (when (eq occur 'all)
        (unless (and (integerp expect) (>= expect 0))
          (signal (carriage-error-symbol 'SRE_E_OCCUR_EXPECT) (list "Missing :expect for :occur all"))))
      ;; Reject PCRE-only constructs (defensive: regardless of :match kind).
      (let ((pat (alist-get :from p)))
        (when (and (stringp pat)
                   (or (string-match-p (regexp-quote "(?<=") pat)  ; lookbehind
                       (string-match-p (regexp-quote "(?<!") pat)  ; negative lookbehind
                       (string-match-p (regexp-quote "(?>") pat)   ; atomic group
                       (string-match-p (regexp-quote "(?|") pat))) ; branch reset
          (signal (carriage-error-symbol 'SRE_E_REGEX_SYNTAX)
                  (list "Unsupported regex construct (PCRE-style)")))))))

(defun carriage-parse-sre (header body repo-root)
  "Parse SRE v1 (begin_from/begin_to) block. Return plan item alist."
  (carriage--sre-validate-header header)
  (let* ((file (plist-get header :file))
         (_ (when (> (string-bytes body) (* 4 1024 1024))
              (signal (carriage-error-symbol 'SRE_E_LIMITS) (list "Response body exceeds 4MiB limit"))))
         (pairs (carriage--sre-parse-body body))
         ;; enforce pair limit (FREEZE)
         (_ (let ((maxn (or (and (boundp 'carriage-mode-max-batch-pairs) carriage-mode-max-batch-pairs) 200)))
              (when (> (length pairs) maxn)
                (signal (carriage-error-symbol 'SRE_E_LIMITS)
                        (list (format "Too many pairs: %d (max %d)" (length pairs) maxn))))))
         ;; Extra guard: if BODY hints regex usage anywhere, reject PCRE-only constructs early.
         ;; This ensures parse-stage errors for tests that expect should-error on invalid regex features.
         (_ (when (and (stringp body)
                       (string-match-p ":match\\s-+regex" body)
                       (or (string-match-p (regexp-quote "(?<=") body)   ; lookbehind
                           (string-match-p (regexp-quote "(?<!") body)   ; negative lookbehind
                           (string-match-p (regexp-quote "(?>") body)    ; atomic group
                           (string-match-p (regexp-quote "(?|") body)))  ; branch reset
              (signal (carriage-error-symbol 'SRE_E_REGEX_SYNTAX)
                      (list "Unsupported regex construct (PCRE-style)"))))
         (_ (carriage--sre-validate-pairs-opts pairs))
         (norm-path (carriage-normalize-path repo-root file)))
    (list (cons :version "1")
          (cons :op 'sre)
          (cons :file (file-relative-name norm-path repo-root))
          (cons :pairs pairs))))

;;;; Dry-run and apply (SRE)

(defun carriage-sre-dry-run-on-text (plan-item text)
  "Dry-run SRE on TEXT content. Return report alist."
  (let* ((file (carriage--plan-get plan-item :file))
         (pairs (or (carriage--plan-get plan-item :pairs) '()))
         (total-matches 0)
         (errors nil)
         (warns nil)
         (previews '())
         (any-noop nil))
    (dolist (p pairs)
      (let* ((from (carriage--pair-get p :from))
             (to   (carriage--pair-get p :to))
             (opts (carriage--pair-get p :opts))
             (range (plist-get opts :range))
             (occur (or (plist-get opts :occur) 'first))
             (expect (plist-get opts :expect))
             (match-kind (or (plist-get opts :match) 'literal))
             (eff-range (and range (carriage--sre-effective-range text range)))
             (region (cond
                      (eff-range (cadr (carriage--sre-slice-by-lines text eff-range)))
                      (range     (cadr (carriage--sre-slice-by-lines text range)))
                      (t         text)))
             (rx (carriage--sre-make-regexp from match-kind))
             (count (condition-case e
                        (carriage--sre-count-matches region from opts)
                      (error
                       (push (format "Regex error: %s" (error-message-string e)) errors)
                       -1))))
        (when (and eff-range (plist-get eff-range :clamped))
          (push (format "range clamped to %d..%d"
                        (plist-get eff-range :start-line)
                        (plist-get eff-range :end-line))
                warns))
        (when (>= count 0)
          (setq total-matches (+ total-matches count))
          (let ((pvs (carriage--sre-previews-for-region region rx to match-kind occur count)))
            (dolist (pv pvs) (push pv previews))
            (when (and (eq occur 'all) (> count (length pvs)))
              (push (format "(+%d more)" (- count (length pvs))) previews)))
          (when (and (eq occur 'all) (integerp expect) (not (= count expect)))
            (push (format "Expect mismatch: have %d, expect %d" count expect) errors))
          (when (and (eq occur 'first) (= count 0))
            (cond
             ;; Для заданного :range (в т.ч. скорректированного) отсутствие совпадений не считается ошибкой.
             (range nil)
             ((and (boundp 'carriage-mode-sre-noop-on-zero-matches)
                   carriage-mode-sre-noop-on-zero-matches)
              (setq any-noop t))
             (t
              (push "No matches for :occur first" errors)))))))
    (let* ((preview (when previews (mapconcat #'identity (nreverse previews) "\n\n")))
           (warn-tail (when warns (format "; %s" (mapconcat #'identity (nreverse warns) "; "))))
           (itm-messages
            (let ((acc '()))
              (when warns
                (dolist (w (nreverse warns))
                  (push (list :code 'SRE_W_RANGE_CLAMP :severity 'warn :file file :details w) acc)))
              (nreverse acc))))
      (if errors
          (let ((base (list :op 'sre :status 'fail
                            :file file
                            :matches total-matches
                            :details (concat
                                      (format "fail: pairs:%d matches:%d; %s"
                                              (length pairs) total-matches
                                              (mapconcat #'identity (nreverse errors) "; "))
                                      (or warn-tail ""))
                            :diff (or preview ""))))
            (if itm-messages (append base (list :_messages itm-messages)) base))
        (let* ((status (if (and any-noop (= total-matches 0)) 'skip 'ok))
               (details (if (eq status 'skip)
                            (concat
                             (format "skip: noop (occur first); pairs:%d matches:%d"
                                     (length pairs) total-matches)
                             (or warn-tail ""))
                          (concat
                           (format "ok: pairs:%d matches:%d"
                                   (length pairs) total-matches)
                           (or warn-tail ""))))
               (base (list :op 'sre :status status
                           :file file
                           :matches total-matches
                           :details details
                           :diff (or preview ""))))
          (if itm-messages (append base (list :_messages itm-messages)) base))))))

(defun carriage-dry-run-sre (plan-item repo-root)
  "Dry-run SRE on file under REPO-ROOT."
  (let* ((file (carriage--plan-get plan-item :file))
         (abs (ignore-errors (carriage-normalize-path (or repo-root default-directory) file))))
    (if (not (and abs (file-exists-p abs)))
        (append (list :op 'sre :status 'fail) (list :file file :details "File not found"))
      (let* ((text (with-temp-buffer (insert-file-contents abs) (buffer-string))))
        (carriage-sre-dry-run-on-text plan-item text)))))

(defun carriage-sre-simulate-apply (plan-item repo-root)
  "Simulate apply for PLAN-ITEM under REPO-ROOT and return (:after STRING :count N)."
  (let* ((file (carriage--plan-get plan-item :file))
         (abs (and file (ignore-errors (carriage-normalize-path (or repo-root default-directory) file)))))
    (if (not (and abs (file-exists-p abs)))
        (list :after "" :count 0)
      (let* ((text (with-temp-buffer (insert-file-contents abs) (buffer-string))))
        (let* ((pairs (or (alist-get :pairs plan-item) '()))
               (changed 0)
               (new-text text))
          (dolist (p pairs)
            (let* ((from (carriage--pair-get p :from))
                   (to   (carriage--pair-get p :to))
                   (opts (carriage--pair-get p :opts))
                   (range (plist-get opts :range))
                   (match-kind (or (plist-get opts :match) 'literal))
                   (occur (or (plist-get opts :occur) 'first))
                   (rx (carriage--sre-make-regexp from match-kind))
                   (eff-range (and range (carriage--sre-effective-range new-text range)))
                   (slice (carriage--sre-slice-by-lines new-text (or eff-range range)))
                   (pre (car slice))
                   (region (cadr slice))
                   (post (caddr slice))
                   (rep (pcase occur
                          ('first (carriage--sre-replace-first region rx to (not (eq match-kind 'regex))))
                          ('all   (carriage--sre-replace-all region rx to (not (eq match-kind 'regex))))
                          (_      (carriage--sre-replace-all region rx to (not (eq match-kind 'regex))))))
                   (region-new (car rep))
                   (cnt (cdr rep)))
              (setq changed (+ changed cnt))
              (setq new-text (concat pre region-new post))))
          (list :after new-text :count changed))))))

(defun carriage-apply-sre (plan-item repo-root)
  "Apply SRE pairs by rewriting file. Optional staging per policy.
Implements NOOP→'skip when after==before and reports :matches and :changed-bytes."
  (let* ((file (carriage--plan-get plan-item :file))
         (abs (carriage-normalize-path (or repo-root default-directory) file))
         (stage (and (boundp 'carriage-apply-stage-policy) carriage-apply-stage-policy)))
    (unless (file-exists-p abs)
      (cl-return-from carriage-apply-sre
        (list :op 'sre :status 'fail :file file :details "File not found")))
    (let* ((before (with-temp-buffer (insert-file-contents abs) (buffer-string)))
           (sim    (carriage-sre-simulate-apply plan-item repo-root))
           (after  (or (plist-get sim :after) before))
           (matches (or (plist-get sim :count) 0))
           (changed-bytes (max 0 (abs (- (string-bytes after) (string-bytes before))))))
      (if (string= before after)
          ;; NOOP → skip
          (list :op 'sre :status 'skip :file file
                :matches matches :changed-bytes 0
                :details "No changes (noop)")
        ;; Write and optionally stage
        (progn
          (carriage-write-file-string abs after t)
          (when (and (eq stage 'index)
                     (fboundp 'carriage-apply-engine)
                     (eq (carriage-apply-engine) 'git))
            (carriage-git-add repo-root file))
          (list :op 'sre :status 'ok :file file
                :matches matches :changed-bytes changed-bytes
                :details (format "Applied %d replacements" matches)))))))

;;;; Registration

(carriage-format-register 'sre "1"
                          :parse #'carriage-parse-sre
                          :dry-run #'carriage-dry-run-sre
                          :apply #'carriage-apply-sre
                          :prompt-fragment #'carriage-op-sre-prompt-fragment)

(provide 'carriage-op-sre)
;;; carriage-op-sre.el ends here
