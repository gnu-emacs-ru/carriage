;;; carriage-format-registry.el --- (:op,:version) → parse/dry-run/apply + prompt-fragment  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: parser, registry
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/parser-registry-v2.org
;;   spec/parser-impl-v2.org
;;
;;; Commentary:
;; Registry mapping (op . version) to parser/dry-run/apply handlers and prompt
;; fragments. Central lookup for op implementations.
;;
;;; Code:
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/parser-registry-v2.org
;;   spec/parser-impl-v2.org
;;   spec/project-overview-v2.org

(require 'cl-lib)
(require 'subr-x)

(defvar carriage-format--registry nil
  "Alist registry mapping (OP . VERSION) to plist of handlers.
Shape: ((KEY . PLIST) ...) where:
  KEY   = (cons OP-SYM VERSION-STR)
  PLIST = (:parse FN :dry-run FN :apply FN :prompt-fragment FN)")

(defun carriage-format--key (op version)
  "Normalize OP/VERSION into registry key."
  (let ((op-sym (cond
                 ((symbolp op) op)
                 ((stringp op) (intern op))
                 (t (intern (format "%s" op)))))
        (ver-str (cond
                  ((stringp version) version)
                  (t (format "%s" version)))))
    (cons op-sym ver-str)))

(defun carriage-format-register (op version &rest kvs)
  "Register OP VERSION handlers: :parse, :dry-run, :apply, :prompt-fragment.
Returns an unregister zero-arg lambda."
  (let* ((key (carriage-format--key op version))
         (entry (list :parse            (plist-get kvs :parse)
                      :dry-run          (plist-get kvs :dry-run)
                      :apply            (plist-get kvs :apply)
                      :prompt-fragment  (plist-get kvs :prompt-fragment)))
         (cell (assoc key carriage-format--registry)))
    (if cell
        (setcdr cell entry)
      (push (cons key entry) carriage-format--registry))
    (lambda ()
      (setq carriage-format--registry (assq-delete-all key carriage-format--registry)))))

(defun carriage-format-get (op version)
  "Return plist for OP/VERSION or nil."
  (let* ((key (carriage-format--key op version))
         (cell (assoc key carriage-format--registry)))
    (and cell (cdr cell))))

(defun carriage-format-ops ()
  "Return list of unique OP symbols present in the registry."
  (delete-dups (mapcar (lambda (kv) (car (car kv))) carriage-format--registry)))

;; Helper: check if a handler is registered for OP/VERSION
(defun carriage-format-registered-p (op version)
  "Return non-nil when OP/VERSION has a registered handler in the registry."
  (and (carriage-format-get op version) t))

(provide 'carriage-format-registry)
;;; carriage-format-registry.el ends here
