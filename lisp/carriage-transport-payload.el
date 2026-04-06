;;; carriage-transport-payload.el --- Payload processing and conversation state  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Payload processing and conversation state handling.
;; Extracted from carriage-transport.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom carriage-transport-debug-in-file-log nil
  "When non-nil, log a short summary of `In file <path>:' sections present in SYSTEM text."
  :type 'boolean
  :group 'carriage-transport)

(defvar carriage-transport--conversation-state nil
  "Conversation state summary for multi-turn awareness.
Plist keys:
  :iterations — number of send iterations
  :modified-files — list of paths modified in last iteration
  :last-ts — timestamp of last completion")

(defvar-local carriage-transport--last-progress-at nil)

(defun carriage-transport--payload-summarize-patch-blocks-fallback (text)
  "Transport-local fallback: summarize #+begin_patch blocks into one-line history markers."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let ((case-fold-search t)
          (rx-end "^[ \t]*#\\+end_patch\\b"))
      (while (re-search-forward "^[ \t]*#\\+begin_patch\\b" nil t)
        (let* ((beg (line-beginning-position))
               (hdr-line (buffer-substring-no-properties beg (line-end-position)))
               (hdr nil)
               (keep nil))
          (when (string-match "^[ \t]*#\\+begin_patch\\s-+\\((.*)\\)[ \t]*$" hdr-line)
            (let* ((sexp (match-string 1 hdr-line))
                   (obj (condition-case _e
                            (car (read-from-string sexp))
                          (error nil))))
              (when (listp obj)
                (setq hdr obj)
                (setq keep (plist-get obj :keep)))))
          (if keep
              (progn
                (forward-line 1)
                (if (re-search-forward rx-end nil t)
                    (forward-line 1)
                  (goto-char (point-max))))
            (let* ((end
                    (save-excursion
                      (goto-char (1+ (line-end-position)))
                      (if (re-search-forward rx-end nil t)
                          (min (point-max) (1+ (line-end-position)))
                        (point-max))))
                   (desc0 (or (and (listp hdr)
                                   (or (plist-get hdr :description)
                                       (plist-get hdr :result)))
                              (and (listp hdr) (plist-get hdr :applied) "Applied")
                              "(no description)"))
                   (desc (string-trim (format "%s" desc0)))
                   (desc (if (string-empty-p desc) "(no description)" desc))
                   (target
                    (cond
                     ((and (listp hdr) (plist-get hdr :path)) (plist-get hdr :path))
                     ((and (listp hdr) (plist-get hdr :file)) (plist-get hdr :file))
                     ((and (listp hdr)
                           (or (plist-get hdr :from) (plist-get hdr :to)))
                      (let ((f (plist-get hdr :from))
                            (t2 (plist-get hdr :to)))
                        (string-trim
                         (format "%s → %s"
                                 (or (and (stringp f) f) "-")
                                 (or (and (stringp t2) t2) "-")))))
                     (t "-")))
                   (line (format ";; patch history: %s — %s" target desc)))
              (delete-region beg end)
              (goto-char beg)
              (insert line "\n"))))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun carriage-transport--update-conversation-state (report &optional buffer)
  "Update conversation state from apply/dry-run REPORT."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((iter (or (and (listp report) (plist-get report :iterations)) 0))
           (files (or (and (listp report) (plist-get report :modified-files)) '()))
           (now (float-time)))
      (setq carriage-transport--conversation-state
            (list :iterations (1+ iter) :modified-files files :last-ts now)))))

(defun carriage-transport--format-conversation-summary ()
  "Format conversation state as system prompt fragment."
  (when (listp carriage-transport--conversation-state)
    (let* ((iter (plist-get carriage-transport--conversation-state :iterations))
           (files (plist-get carriage-transport--conversation-state :modified-files))
           (ts (plist-get carriage-transport--conversation-state :last-ts)))
      (when (and iter (> iter 0))
        (format "Iteration %d: %s"
                iter
                (if files
                    (mapconcat #'identity files ", ")
                  "no files modified"))))))

(defun carriage-transport--inject-conversation-summary (text)
  "Inject conversation summary into TEXT if present."
  (let ((summary (carriage-transport--format-conversation-summary)))
    (if (and summary (stringp text))
        (concat text "\n" summary)
      text)))

(defun carriage-transport--strip-internal-lines (text)
  "Remove internal-only lines from TEXT (lines starting with ;;; or ;; CARRIAGE_)."
  (when (stringp text)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "^;;; .*$\\|^;; CARRIAGE_" nil t)
          (delete-region (match-beginning 0) (line-end-position))
          (when (looking-at "\n") (delete-char 1))))
      (buffer-string))))

(provide 'carriage-transport-payload)
;;; carriage-transport-payload.el ends here
