;;; carriage-pricing.el --- Pricing tables and cost computation  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: pricing, cost
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/pricing-v2.org
;;   spec/ui-cost-v2.org
;;
;;; Commentary:
;; Load model pricing CSV tables and compute request/document cost from token usage.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-pricing nil
  "Pricing tables and cost computation."
  :group 'carriage)

(defcustom carriage-pricing-user-csv-path "~/.carriage/model-prices.csv"
  "User-level pricing CSV path.
If present, it takes precedence over the project CSV."
  :type 'string
  :group 'carriage-pricing)

(defcustom carriage-pricing-project-csv-path "model-prices.csv"
  "Project-level pricing CSV path (relative to project root)."
  :type 'string
  :group 'carriage-pricing)

(defcustom carriage-pricing-currency-symbol "₽"
  "Currency symbol for UI display only (does not affect computations)."
  :type 'string
  :group 'carriage-pricing)

;; Money unit: micro-ruble (µ₽) = 1e-6 RUB
(defconst carriage-pricing--u-per-rub 1000000
  "Number of pricing money-units per 1 RUB (µ₽ per RUB).")

(defconst carriage-pricing--tokens-per-1m 1000000
  "Token denominator for rates stored as per 1,000,000 tokens.")

;; Built-in default table (CSV v2 schema).
(defconst carriage-pricing--default-csv
  (concat
   "model,input_rub_per_1m,output_rub_per_1m,input_audio_rub_per_1m,output_audio_rub_per_1m\n"
   "gpt-5.2,33.25,2660,,\n"
   "gpt-5.2-pro,399,31920,,\n"
   "gpt-5.2-chat,33.25,2660,,\n"
   "gpt-5.1,23.75,1900,,\n"
   "gpt-5.1-chat,23.75,1900,,\n"
   "gpt-5.1-codex,23.75,1900,,\n"
   "gpt-5.1-codex-max,23.75,1900,,\n"
   "gpt-5.1-codex-mini,28.5,1140,,\n"
   "gpt-5-nano,0.95,76,,\n"
   "gpt-5-mini,4.75,380,,\n"
   "gpt-5,23.75,1900,,\n"
   "gpt-5-pro,285,22800,,\n"
   "gpt-4.1-nano,9.5,76,,\n"
   "gpt-4.1-mini,38,304,,\n"
   "gpt-4.1,190,1520,,\n"
   "gpt-4o-mini,14.25,114,,\n"
   "gpt-4o-mini-search-preview,14.25,114,,\n"
   "gpt-4o-search-preview,237.5,1900,,\n"
   "gpt-4o,237.5,1900,,\n"
   "gpt-4o-audio-preview,475,1900,7600,15200\n"
   "gpt-4o-mini-audio-preview,28.5,114,1900,3800\n"
   "gemini-3-flash-preview,95,570,,\n"
   "gemini-3-pro-preview,380,2280,,\n"
   "gemini-3-pro-image-preview,380,2280,,\n"
   "gemini-2.5-flash-image,57,475,,\n"
   "gemini-2.5-flash-lite,19,76,,\n"
   "gemini-2.5-flash,57,475,,\n"
   "gemini-2.5-pro,237.5,1900,,\n"
   "claude-haiku-4.5,190,950,,\n"
   "claude-sonnet-4.5,570,2850,,\n"
   "claude-opus-4.5,950,4750,,\n"
   "deepseek-v3.2,53.2,79.8,,\n"
   "deepseek-v3.2-speciale,53.2,79.8,,\n"
   "deepseek-r1-0528,95,414.2,,\n"
   "deepseek-chat-v3.1,51.3,209,,\n"
   "llama-4-scout,15.2,85.5,,\n"
   "llama-4-maverick,38,114,,\n"
   "llama-3.3-70b-instruct,22.8,57,,\n"
   "sonar,190,190,,\n"
   "sonar-pro,570,2850,,\n"
   "sonar-pro-search,570,2850,,\n"
   "sonar-deep-research,380,1520,,\n"
   "grok-4.1-fast,38,95,,\n"
   "grok-4,570,2850,,\n"
   "grok-code-fast-1,38,285,,\n"
   "qwen3-235b-a22b-2507,14.82,59.28,,\n"
   "qwen3-30b-a3b,3.8,15.2,,\n"
   "qwen3-coder,38,152,,\n"
   "qwen3-coder-30b-a3b-instruct,11.4,47.5,,\n"
   "qwen3-max,228,1140,,\n"
   "mistral-large-2512,95,285,,\n"
   "mistral-small-3.2-24b-instruct,11.4,34.2,,\n"
   "mistral-medium-3.1,76,380,,\n"
   "codestral-2508,57,171,,\n"
   "devstral-small,13.3,53.2,,\n"
   "mistral-nemo,3.8,7.6,,\n"
   "kimi-k2-thinking,85.5,446.5,,\n"
   "kimi-k2-0905,74.1,361,,\n")
  "Built-in default pricing CSV table shipped with Carriage.")

(defun carriage-pricing--local-file-p (path)
  "Return non-nil when PATH is a local filesystem path (not TRAMP/remote)."
  (and (stringp path)
       (not (string-empty-p (string-trim path)))
       (not (file-remote-p path))))

(defun carriage-pricing--parse-number (s)
  "Parse strict decimal number from S and return it as a float, or nil."
  (let ((x (string-trim (or s ""))))
    (cond
     ((string-empty-p x) nil)
     ((string-match-p "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" x)
      (string-to-number x))
     (t nil))))

(defun carriage-pricing--rub-per-1m->u-per-1m (rub-per-1m)
  "Convert RUB-per-1M (float) to integer µ₽-per-1M with half-up rounding."
  (when (numberp rub-per-1m)
    (let* ((x (* rub-per-1m carriage-pricing--u-per-rub)))
      (truncate (+ x 0.5)))))

(defun carriage-pricing--norm-key (s)
  "Normalize pricing key string S for hash lookup."
  (downcase (string-trim (or s ""))))

(defun carriage-pricing--split-csv-line (line)
  "Split a simple CSV LINE by commas (no quoting support)."
  (split-string (or line "") "," nil))

(defun carriage-pricing-parse-csv-string (csv)
  "Parse pricing CSV string into a hash-table mapping key → rate plist.

Rates are stored as integers in µ₽-per-1M:
- :in-u-per-1m
- :out-u-per-1m
- :audio-in-u-per-1m
- :audio-out-u-per-1m"
  (let* ((lines (split-string (or csv "") "\n" t))
         (hdr (and lines (car lines)))
         (rows (cdr lines)))
    (if (not (and (stringp hdr)
                  (string= (string-trim hdr)
                           "model,input_rub_per_1m,output_rub_per_1m,input_audio_rub_per_1m,output_audio_rub_per_1m")))
        ;; Strict header required; return empty table.
        (make-hash-table :test 'equal)
      (let ((ht (make-hash-table :test 'equal)))
        (dolist (ln rows)
          (let* ((cols (carriage-pricing--split-csv-line ln))
                 (model (nth 0 cols))
                 (in  (carriage-pricing--parse-number (nth 1 cols)))
                 (out (carriage-pricing--parse-number (nth 2 cols)))
                 (ain (carriage-pricing--parse-number (nth 3 cols)))
                 (aout (carriage-pricing--parse-number (nth 4 cols)))
                 (k (carriage-pricing--norm-key model)))
            (when (and (stringp k) (not (string-empty-p k)))
              (puthash
               k
               (list :in-u-per-1m (carriage-pricing--rub-per-1m->u-per-1m in)
                     :out-u-per-1m (carriage-pricing--rub-per-1m->u-per-1m out)
                     :audio-in-u-per-1m (carriage-pricing--rub-per-1m->u-per-1m ain)
                     :audio-out-u-per-1m (carriage-pricing--rub-per-1m->u-per-1m aout))
               ht))))
        ht))))

(defun carriage-pricing--read-file (path)
  "Read PATH as UTF-8 string and return it, or nil."
  (when (and (carriage-pricing--local-file-p path)
             (file-exists-p path)
             (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun carriage-pricing--table-from-path (path)
  "Load pricing table from CSV file PATH, or nil if unreadable/invalid."
  (let* ((txt (carriage-pricing--read-file path)))
    (when (and (stringp txt) (not (string-empty-p (string-trim txt))))
      (carriage-pricing-parse-csv-string txt))))

(defvar carriage-pricing--table-cache nil
  "Cache plist for the last loaded pricing table.
Keys: :root :user-path :proj-path :table :ts :source")

(defun carriage-pricing-load-table (&optional root)
  "Load pricing table using deterministic precedence:
1) user file (carriage-pricing-user-csv-path)
2) project file (ROOT/model-prices.csv)
3) built-in default

Returns hash-table mapping keys to rate plists."
  (let* ((root (or root default-directory))
         (user (expand-file-name carriage-pricing-user-csv-path))
         (proj (expand-file-name carriage-pricing-project-csv-path root))
         (cache carriage-pricing--table-cache)
         (cached-root (plist-get cache :root))
         (cached-user (plist-get cache :user-path))
         (cached-proj (plist-get cache :proj-path))
         (cached-tab  (plist-get cache :table)))
    (if (and cached-tab (string= (or cached-root "") (or root ""))
             (string= (or cached-user "") (or user ""))
             (string= (or cached-proj "") (or proj "")))
        cached-tab
      (let* ((tab nil)
             (src nil))
        (cond
         ((and (carriage-pricing--local-file-p user)
               (file-exists-p user))
          (setq tab (carriage-pricing--table-from-path user))
          (setq src (and tab 'user)))
         ((and (carriage-pricing--local-file-p proj)
               (file-exists-p proj))
          (setq tab (carriage-pricing--table-from-path proj))
          (setq src (and tab 'project))))
        (unless (hash-table-p tab)
          (setq tab (carriage-pricing-parse-csv-string carriage-pricing--default-csv))
          (setq src 'builtin))
        (setq carriage-pricing--table-cache
              (list :root root :user-path user :proj-path proj :table tab :ts (float-time) :source src))
        tab))))

(defun carriage-pricing--fallback-keys (canonical-id)
  "Return a list of fallback keys for CANONICAL-ID."
  (let* ((s (string-trim (or canonical-id "")))
         (parts (split-string s ":" t)))
    (cond
     ((<= (length parts) 1) (list s))
     ((= (length parts) 2)
      (let ((model (nth 1 parts)))
        (delete-dups (delq nil (list s model)))))
     (t
      (let* ((backend (nth 0 parts))
             (provider (nth 1 parts))
             (model (car (last parts)))
             (prov-model (and provider model (concat provider ":" model))))
        (delete-dups (delq nil (list s prov-model model backend))))))))

(defun carriage-pricing-lookup (canonical-id table)
  "Lookup rate record for CANONICAL-ID in TABLE, using best-effort fallbacks.
Returns rate plist or nil."
  (when (and (hash-table-p table) (stringp canonical-id))
    (let* ((keys (carriage-pricing--fallback-keys canonical-id))
           (hit nil))
      (while (and keys (not hit))
        (setq hit (gethash (carriage-pricing--norm-key (car keys)) table))
        (setq keys (cdr keys)))
      hit)))

(defun carriage-pricing--cost-u (tokens rate-u-per-1m)
  "Compute cost in µ₽ from TOKENS (integer) and RATE-U-PER-1M (integer).
Uses half-up rounding at the 1-token boundary."
  (when (and (integerp tokens) (>= tokens 0)
             (integerp rate-u-per-1m) (>= rate-u-per-1m 0))
    (let* ((num (+ (* tokens rate-u-per-1m)
                   (/ carriage-pricing--tokens-per-1m 2))))
      (/ num carriage-pricing--tokens-per-1m))))

(defun carriage-pricing-compute (canonical-id usage table)
  "Compute cost plist for CANONICAL-ID given USAGE plist and TABLE.

Returns plist:
- :known boolean
- :model string (canonical id used for lookup)
- :tokens-in/:tokens-out/:audio-in/:audio-out
- :cost-*-u integers in µ₽, or nil when unknown."
  (let* ((model (or canonical-id ""))
         (rate (carriage-pricing-lookup model table))
         (tin (plist-get usage :tokens-in))
         (tout (plist-get usage :tokens-out))
         (ain (plist-get usage :audio-in))
         (aout (plist-get usage :audio-out)))
    (if (not (listp rate))
        (list :known nil :model model
              :tokens-in tin :tokens-out tout :audio-in ain :audio-out aout
              :cost-in-u nil :cost-out-u nil :cost-audio-in-u nil :cost-audio-out-u nil
              :cost-total-u nil)
      (let* ((in-rate (plist-get rate :in-u-per-1m))
             (out-rate (plist-get rate :out-u-per-1m))
             (ain-rate (plist-get rate :audio-in-u-per-1m))
             (aout-rate (plist-get rate :audio-out-u-per-1m))
             (c-in (carriage-pricing--cost-u tin in-rate))
             (c-out (carriage-pricing--cost-u tout out-rate))
             (c-ain (carriage-pricing--cost-u ain ain-rate))
             (c-aout (carriage-pricing--cost-u aout aout-rate))
             (total (let ((xs (delq nil (list c-in c-out c-ain c-aout))))
                      (when xs (apply #'+ xs)))))
        (list :known t :model model
              :tokens-in tin :tokens-out tout :audio-in ain :audio-out aout
              :cost-in-u c-in :cost-out-u c-out :cost-audio-in-u c-ain :cost-audio-out-u c-aout
              :cost-total-u total)))))

(defun carriage-pricing-format-money (amount-u &optional currency-symbol)
  "Format AMOUNT-U (µ₽ integer) as a string with two decimals in RUB, prefixed by CURRENCY-SYMBOL.
When AMOUNT-U is nil, returns \"—\"."
  (if (not (integerp amount-u))
      "—"
    (let* ((sym (if (stringp currency-symbol) currency-symbol carriage-pricing-currency-symbol))
           (kopecks (/ (+ amount-u 5000) 10000)) ;; 1 коп. = 10_000 µ₽, half-up
           (rub (/ kopecks 100))
           (kop (% kopecks 100)))
      (format "%s%d.%02d" (or sym "") rub kop))))

(defun carriage-pricing-generate-project-csv (root &optional overwrite)
  "Generate project pricing CSV file in ROOT according to `carriage-pricing-project-csv-path'.
This is an explicit user action (no implicit writes).

When OVERWRITE is non-nil, replace existing file.

Returns path string on success, or nil."
  (let* ((root (or root default-directory))
         (path (expand-file-name carriage-pricing-project-csv-path root)))
    (when (and (carriage-pricing--local-file-p path)
               (or overwrite (not (file-exists-p path))))
      (make-directory (file-name-directory path) t)
      (with-temp-buffer
        (insert carriage-pricing--default-csv)
        (write-region (point-min) (point-max) path))
      path)))

(provide 'carriage-pricing)
;;; carriage-pricing.el ends here
