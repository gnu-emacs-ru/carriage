;;; carriage-assist.el --- Assist validators and minimal APIs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Carriage Team <dev@carriage>
;; URL: https://gnu-emacs.ru/carriage
;; Keywords: assist, schema, ui
;;
;;; Commentary:
;; Schema-locked Assist validators and minimal stubs for Assist APIs.
;; Split out from transports to avoid hard deps (e.g., gptel) in tests.
;;
;;; Code:

(require 'cl-lib)

(defun carriage-assist--suggest-valid-p (lst)
  "Return non-nil when LST is a valid Suggest list: each element is a plist with :id/:label/:reason/:weight."
  (and (listp lst)
       (cl-every
        (lambda (it)
          (and (listp it)
               (plist-get it :id)
               (plist-get it :label)
               (plist-member it :reason)
               (plist-member it :weight)))
        lst)))

(defun carriage-assist--ctx-delta-valid-p (pl)
  "Return non-nil when PL is a valid Context-Delta plist {:add list :remove list :why string}."
  (and (listp pl)
       (plist-member pl :add)
       (plist-member pl :remove)
       (plist-member pl :why)
       (listp (plist-get pl :add))
       (listp (plist-get pl :remove))))

;;;###autoload
(defun carriage-assist-suggest (_ctx)
  "Return a schema-locked list of suggestions for the current document state.
Minimal stub: returns an empty list; transports may advise/override.
Errors are mapped to ASSIST_E_SCHEMA and never modify buffers."
  (condition-case _e
      '()
    (error
     (when (require 'carriage-errors nil t)
       (ignore-errors (signal (carriage-error-symbol 'ASSIST_E_SCHEMA)
                              (list "assist suggest failed"))))
     '())))

;;;###autoload
(defun carriage-assist-context-delta (_ctx)
  "Return a schema-locked context delta plist {:add [] :remove [] :why}.
Minimal stub: returns an empty delta; transports may advise/override."
  (condition-case _e
      (list :add '() :remove '() :why "")
    (error
     (when (require 'carriage-errors nil t)
       (ignore-errors (signal (carriage-error-symbol 'ASSIST_E_SCHEMA)
                              (list "assist context-delta failed"))))
     (list :add '() :remove '() :why ""))))

(provide 'carriage-assist)
;;; carriage-assist.el ends here
