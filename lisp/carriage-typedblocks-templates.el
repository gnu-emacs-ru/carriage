;;; carriage-typedblocks-templates.el --- Org structure templates for typed blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Carriage contributors
;; Author: Carriage Team <dev@carriage>
;; URL: https://gnu-emacs.ru/carriage
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.1"))
;; Keywords: outlines, org, templates
;;
;; Specifications:
;;   spec/typedblocks-v1.org
;;   spec/ui-v2.org
;;
;;; Commentary:
;; Provide quick Org structure-template abbreviations for Carriage typed blocks:
;;   <t   -> #+begin_task … #+end_task
;;   <an  -> #+begin_analysis … #+end_analysis
;;   <pl  -> #+begin_plan … #+end_plan
;;   <v   -> #+begin_verify … #+end_verify
;;   <c   -> #+begin_commands … #+end_commands
;;   <n   -> #+begin_notes … #+end_notes
;;   <q   -> #+begin_question … #+end_question   (overrides default 'quote')
;;   <ans -> #+begin_answer … #+end_answer
;;   <cx  -> #+begin_context … #+end_context
;;   <p   -> #+begin_patch … #+end_patch
;;
;; Notes:
;; - <q overrides Org’s default "quote" template key. If you prefer to keep <q
;;   for quote, either do not call the installer or change the key below.
;;
;;; Code:

(require 'org)

(defgroup carriage-typedblocks-templates nil
  "Org structure-template abbreviations for Carriage typed blocks."
  :group 'org)

(defcustom carriage-typedblocks-templates-override-existing t
  "When non-nil, installer removes existing entries with the same key before adding new ones.
This is useful to override default Org keys like <q (quote) with <q (question)."
  :type 'boolean
  :group 'carriage-typedblocks-templates)

(defun carriage-typedblocks--remove-template-key (key)
  "Remove any existing org-structure-template-alist entry with KEY."
  (setq org-structure-template-alist
        (cl-remove-if (lambda (cell) (string= (car cell) key))
                      org-structure-template-alist)))

;;;###autoload
(defun carriage-typedblocks-install-structure-templates (&optional override)
  "Install structure-template abbreviations for Carriage typed blocks.

When OVERRIDE (or `carriage-typedblocks-templates-override-existing') is non-nil,
remove any existing entries with the same keys before adding new ones."
  (interactive)
  (let* ((pairs
          '(("ta"   . "task")
            ("an"  . "analysis")
            ("pl"  . "plan")
            ("v"   . "verify")
            ("c"   . "commands")
            ("n"   . "notes")
            ("qu"   . "question")  ;; NOTE: overrides default 'quote'
            ("ans" . "answer")
            ("cx"  . "context")
            ("p"   . "patch")))
         (do-override (or override carriage-typedblocks-templates-override-existing)))
    (dolist (kv pairs)
      (when do-override
        (carriage-typedblocks--remove-template-key (car kv)))
      ;; Only add when missing (unless we just removed above).
      (unless (assoc (car kv) org-structure-template-alist)
        (push kv org-structure-template-alist))))
  ;; Keep the user-visible order stable: newest at front is acceptable; do not sort.
  org-structure-template-alist)

;;;###autoload
(defun carriage-typedblocks-ensure-structure-templates ()
  "Idempotent helper to ensure typed-block templates are present in Org buffers.
Safe to add to `org-mode-hook'."
  (when (derived-mode-p 'org-mode)
    (ignore-errors (carriage-typedblocks-install-structure-templates))))

(provide 'carriage-typedblocks-templates)
;;; carriage-typedblocks-templates.el ends here
