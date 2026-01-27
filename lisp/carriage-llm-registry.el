;;; carriage-llm-registry.el --- LLM backend & model registry  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: llm, registry
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/llm-transport-v2.org
;;   spec/llm-registry-v2.org
;;   spec/prompt-profiles-v2.org
;;   spec/logging-v2.org
;;   spec/pack-recipes-v2.org
;;
;;; Commentary:
;; Registry of available LLM backends and model identifiers.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function gptel-backend-name "gptel" (backend))
(declare-function gptel-backend-models "gptel" (&optional backend))
(declare-function gptel-backend-default-model "gptel" (backend))

(defvar carriage-llm--registry nil
  "Alist registry of LLM backends and their models.
Shape: ((BACKEND . (:models (\"m1\" \"m2\") :models-fn FN)) ...).
BACKEND is a symbol (preferred) or string. :models-fn, when non-nil,
should be a zero-arg function returning a list of model strings.")

(defun carriage-llm--norm-backend (backend)
  "Normalize BACKEND to a symbol."
  (cond
   ((symbolp backend) backend)
   ((stringp backend) (intern backend))
   (t (intern (format "%s" backend)))))

(defun carriage-llm-register-backend (backend &rest kvs)
  "Register BACKEND with optional keys:
:models — list of model strings; :models-fn — function returning list of models.
Returns an unregister zero-arg lambda."
  (let* ((b (carriage-llm--norm-backend backend))
         (entry (list :models (plist-get kvs :models)
                      :models-fn (plist-get kvs :models-fn)))
         (cell (assoc b carriage-llm--registry)))
    (if cell
        (setcdr cell entry)
      (push (cons b entry) carriage-llm--registry))
    (lambda ()
      (setq carriage-llm--registry (assq-delete-all b carriage-llm--registry)))))

(defun carriage-llm-available-backends ()
  "Return list of backend names (strings) from the registry."
  (mapcar (lambda (cell) (symbol-name (car cell))) carriage-llm--registry))

(defun carriage-llm-available-models (&optional backend)
  "Return list of model strings for BACKEND (defaults to first registered)."
  (let* ((b (and backend (carriage-llm--norm-backend backend)))
         (cell (or (and b (assoc b carriage-llm--registry))
                   (car carriage-llm--registry))))
    (when cell
      (let* ((pl (cdr cell))
             (static (plist-get pl :models))
             (fn (plist-get pl :models-fn))
             (dyn (when (functionp fn)
                    (condition-case _ (funcall fn) (error nil)))))
        (or dyn static)))))

(defun carriage-llm--slug (s)
  "Return a lowercase slug for string S with non-alnum replaced by dashes."
  (let* ((s (format "%s" s))
         (s (downcase s)))
    (replace-regexp-in-string "[^a-z0-9]+" "-" s)))

(defun carriage-llm-candidates ()
  "Return combined candidates for model selection.

Preference order:
- If gptel is available, enumerate models from gptel’s registered backends
  and return BOTH:
    - \"gptel:PROVIDER:MODEL\" (provider is a slug from backend name)
    - \"gptel:MODEL\"          (back-compat)
- Otherwise, fall back to the internal registry and return \"backend:model\"."
  (cond
   ;; Prefer gptel as the source of truth when available
   ((and (boundp 'gptel--known-backends) gptel--known-backends)
    (let ((acc '()))
      (dolist (cell gptel--known-backends)
        (let* ((provider-name (car cell))          ;string key in the gptel registry
               (backend (cdr cell))
               (prov (carriage-llm--slug (or provider-name
                                             (ignore-errors (gptel-backend-name backend))
                                             "default")))
               (models (ignore-errors (gptel-backend-models backend))))
          (dolist (m (or models '()))
            (let* ((mstr (if (symbolp m) (symbol-name m) (format "%s" m))))
              (if (string-match-p ":" mstr)
                  ;; Model already qualified (e.g., "provider:model" or "gptel:provider:model"):
                  ;; avoid duplicating segments, only ensure the "gptel:" backend prefix is present once.
                  (let ((full (if (string-prefix-p "gptel:" mstr) mstr (format "gptel:%s" mstr))))
                    (push full acc))
                ;; Unqualified model name: offer both triple and double forms.
                (progn
                  (push (format "gptel:%s:%s" prov mstr) acc)
                  (push (format "gptel:%s" mstr) acc)))))))
      (delete-dups (nreverse acc))))
   (t
    ;; Fallback to internal simple registry
    (cl-loop for (b . pl) in carriage-llm--registry
             for bname = (symbol-name b)
             for models = (or (carriage-llm-available-models b) (plist-get pl :models))
             append (mapcar (lambda (m) (format "%s:%s" bname m))
                            (or models '()))))))

(defun carriage-llm-basename (model-or-id)
  "Return the last segment of MODEL-OR-ID after ':' separator.
Examples:
  \"gptel:ai-tunnel:gpt-5\" -> \"gpt-5\"
  \"backend:model\"         -> \"model\"
  \"model\"                 -> \"model\""
  (let* ((s (format "%s" model-or-id))
         (parts (split-string s ":" t))
         (last (car (last parts))))
    (or last s)))

(defun carriage-llm-display-name (model-or-id)
  "Return a friendly display name for MODEL-OR-ID.

Takes the last part after ':' and, if that part contains a provider prefix
separated by '/', returns only the final segment after '/'."
  (let* ((base (carriage-llm-basename model-or-id))
         (parts (and (stringp base) (split-string base "/" t))))
    (if (and parts (> (length parts) 0))
        (car (last parts))
      base)))

(defun carriage-llm-default-candidate (backend model pairs &optional provider)
  "Return best default candidate string for BACKEND MODEL using PAIRS.
When PROVIDER is non-nil, prefer \"backend:provider:model\" if present.

Preference:
  1) gptel:PROVIDER:MODEL (if present),
  2) gptel:MODEL (if present),
  3) BACKEND:PROVIDER:MODEL (if present),
  4) BACKEND:MODEL (if present),
  5) any candidate in PAIRS whose basename equals MODEL,
  6) gptel:PROVIDER:MODEL,
  7) gptel:MODEL,
  8) BACKEND:PROVIDER:MODEL,
  9) BACKEND:MODEL,
  10) MODEL."
  (let* ((b (cond
             ((symbolp backend) (symbol-name backend))
             ((stringp backend) backend)
             (t (format "%s" backend))))
         (prefer (delq nil
                       (list (and model provider (format "gptel:%s:%s" provider model))
                             (and model (format "gptel:%s" model))
                             (and b model provider (format "%s:%s:%s" b provider model))
                             (and b model (format "%s:%s" b model))))))
    (or (cl-find-if (lambda (c) (and pairs (member c pairs))) prefer)
        (when (and (listp pairs) (stringp model))
          (cl-find-if (lambda (c) (string= (carriage-llm-basename c) model)) pairs))
        (car prefer)
        model)))

(defun carriage-llm-make-full-id (backend provider model)
  "Compose full identifier from BACKEND, PROVIDER and MODEL.
Returns:
- \"backend:provider:model\" when BACKEND and PROVIDER are non-nil;
- \"backend:model\" when BACKEND is non-nil and PROVIDER is nil;
- MODEL when BACKEND is nil."
  (let* ((b (cond
             ((symbolp backend) (symbol-name backend))
             ((stringp backend) backend)
             (t (and backend (format "%s" backend))))))
    (cond
     ((and b provider model) (format "%s:%s:%s" b provider model))
     ((and b model)          (format "%s:%s" b model))
     (model))))

;; Small normalization helper to avoid duplicate leading backend prefixes,
;; e.g., "gptel:gptel:ai-tunnel:gpt-4.1" -> "gptel:ai-tunnel:gpt-4.1".
(defun carriage-llm--dedupe-leading-backend (s)
  "Return S with duplicate leading backend removed: \"be:be:rest\" -> \"be:rest\"."
  (if (and (stringp s)
           (string-match "\\`\\([^:]+\\):\\1:\\(.*\\)\\'" s))
      (concat (match-string 1 s) ":" (match-string 2 s))
    s))

;; -----------------------------------------------------------------------------
;; Model resolution helpers

(defun carriage-llm--coerce-string (value)
  "Return VALUE coerced to a non-empty string when possible."
  (cond
   ((stringp value) (unless (string-empty-p value) value))
   ((symbolp value) (symbol-name value))
   ((numberp value) (number-to-string value))
   ((null value) nil)
   (t (let ((s (ignore-errors (format "%s" value))))
        (unless (or (null s) (string-empty-p s)) s)))))

(defun carriage-llm--first-model-string (value)
  "Extract the first non-empty model string from VALUE."
  (cond
   ((stringp value) (unless (string-empty-p value) value))
   ((symbolp value) (symbol-name value))
   ((numberp value) (number-to-string value))
   ((vectorp value)
    (let ((len (length value))
          (idx 0)
          (res nil))
      (while (and (< idx len) (null res))
        (setq res (carriage-llm--first-model-string (aref value idx)))
        (setq idx (1+ idx)))
      res))
   ((listp value)
    (cl-loop for item in value
             for str = (carriage-llm--first-model-string item)
             when str return str))
   ((null value) nil)
   (t (carriage-llm--coerce-string value))))

(defun carriage-llm--provider-slug (provider)
  "Return normalized provider slug for PROVIDER."
  (let ((raw (cond
              ((stringp provider) provider)
              ((symbolp provider) (symbol-name provider))
              ((null provider) nil)
              (t (format "%s" provider)))))
    (when (and raw (not (string-empty-p raw)))
      (carriage-llm--slug raw))))

(defun carriage-llm--gptel-backend-slug (backend)
  "Return slug for GPTel BACKEND object when known."
  (when (and backend (boundp 'gptel--known-backends) gptel--known-backends)
    (cl-loop for cell in gptel--known-backends
             for provider-name = (car cell)
             for backend-obj = (cdr cell)
             when (eq backend backend-obj)
             return (carriage-llm--slug
                     (or provider-name
                         (and (fboundp 'gptel-backend-name)
                              (ignore-errors (gptel-backend-name backend)))
                         "default")))))

(defun carriage-llm--gptel-backend-by-provider (provider)
  "Return GPTel backend object that matches PROVIDER slug."
  (let ((slug (carriage-llm--provider-slug provider)))
    (when (and slug (boundp 'gptel--known-backends) gptel--known-backends)
      (cl-loop for cell in gptel--known-backends
               for provider-name = (car cell)
               for backend = (cdr cell)
               for prov-slug = (carriage-llm--slug
                                (or provider-name
                                    (and (fboundp 'gptel-backend-name)
                                         (ignore-errors (gptel-backend-name backend)))
                                    "default"))
               when (string= prov-slug slug)
               return backend))))

(defun carriage-llm--gptel-current-backend ()
  "Return current or default GPTel backend object."
  (or (and (boundp 'gptel-current-backend) gptel-current-backend)
      (and (boundp 'gptel-default-backend) gptel-default-backend)
      (and (boundp 'gptel--default-backend) gptel--default-backend)
      (and (boundp 'gptel--known-backends) gptel--known-backends
           (cdr (car gptel--known-backends)))))

(defun carriage-llm--gptel-resolve-default (provider)
  "Resolve \"gptel-default\" for PROVIDER to a concrete model identifier."
  (when (require 'gptel nil t)
    (let* ((backend (or (carriage-llm--gptel-backend-by-provider provider)
                        (carriage-llm--gptel-current-backend)))
           (model
            (or
             (when (and backend (fboundp 'gptel-backend-default-model))
               (carriage-llm--first-model-string
                (ignore-errors (gptel-backend-default-model backend))))
             (let* ((models-raw (and backend (fboundp 'gptel-backend-models)
                                     (ignore-errors (gptel-backend-models backend))))
                    (models (if (functionp models-raw)
                                (ignore-errors (funcall models-raw))
                              models-raw)))
               (carriage-llm--first-model-string models)))))
      (when (stringp model)
        (let* ((slug (or (carriage-llm--provider-slug provider)
                         (carriage-llm--gptel-backend-slug backend))))
          (if slug
              (carriage-llm-make-full-id 'gptel slug model)
            (carriage-llm-make-full-id 'gptel nil model)))))))

(defun carriage-llm-resolve-model (backend provider model)
  "Return canonical model string for BACKEND/PROVIDER/MODEL.
Expands placeholders such as \"gptel-default\" when possible."
  (let* ((model-str (cond
                     ((stringp model) model)
                     ((symbolp model) (symbol-name model))
                     ((null model) nil)
                     (t (format "%s" model))))
         (backend-sym (carriage-llm--norm-backend backend)))
    (cond
     ((and (stringp model-str)
           (string= model-str "gptel-default")
           (eq backend-sym 'gptel))
      (or (carriage-llm--gptel-resolve-default provider) model-str))
     (t model-str))))

(provide 'carriage-llm-registry)
;;; carriage-llm-registry.el ends here
