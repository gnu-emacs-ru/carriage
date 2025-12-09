;;; carriage-iteration.el --- Iteration markers and helpers  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: iteration
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/iteration-markers-v2.org
;;   spec/iteration-markers-placement-v2.org
;;   spec/ui-v2.org
;;
;;; Commentary:
;; Marking and querying "last iteration" blocks in source buffers.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-logging)

(defvar-local carriage--last-iteration-id nil
  "Buffer-local identifier of the last iteration.
When set, only blocks whose text property =carriage-iteration-id' equals this value
are considered by `carriage-collect-last-iteration-blocks'.")

(defun carriage-iteration--generate-id ()
  "Generate a reasonably unique iteration id."
  (md5 (format "%s-%s-%s-%s"
               (float-time) (user-uid) (random most-positive-fixnum) (buffer-file-name))))

(defun carriage-iteration--org-property-line ()
  "Return (BEG . END) for existing CARRIAGE_ITERATION_ID property line, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_ITERATION_ID[ \t]+\\(.+\\)$" nil t)
      (cons (match-beginning 0) (line-end-position)))))

(defun carriage-iteration--write-org-id (id)
  "Best-effort write/update #+PROPERTY: CARRIAGE_ITERATION_ID with ID when in org-mode."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let* ((case-fold-search t)
             (pos-pair (carriage-iteration--org-property-line))
             (line (concat "#+PROPERTY: CARRIAGE_ITERATION_ID " id)))
        (if pos-pair
            (let* ((beg (car pos-pair))
                   (end (cdr pos-pair)))
              (goto-char beg)
              (delete-region beg end)
              (insert line))
          ;; Insert near top after common property lines
          (goto-char (point-min))
          (while (looking-at-p "^[ \t]*#\\+\\(TITLE\\|AUTHOR\\|PROPERTY\\|LANGUAGE\\|OPTIONS\\)\\b")
            (forward-line 1))
          (insert line "\n"))))))

(defun carriage-iteration-read-org-id ()
  "Read Org property CARRIAGE_ITERATION_ID or return nil."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_ITERATION_ID[ \t]+\\(.+\\)$" nil t)
        (string-trim (match-string 1))))))

(defun carriage-iteration-read-inline-id ()
  "Scan buffer for the last inline marker \"#+CARRIAGE_ITERATION_ID: <id>\" and return ID, or nil.
Best-effort; does not require Org mode."
  (save-excursion
    (goto-char (point-min))
    (let ((last nil))
      (while (re-search-forward "^[ \t]*#\\+CARRIAGE_ITERATION_ID:[ \t]+\\(.+\\)$" nil t)
        (setq last (string-trim (match-string 1))))
      last)))


;;;###autoload
(defun carriage-mark-last-iteration (beg end &optional id)
  "Mark all #+begin_patch blocks between BEG and END as the “last iteration”.

If called interactively without an active region, mark the whole buffer.
Sets (or regenerates) =carriage--last-iteration-id', writes it as text
property =carriage-iteration-id' on begin lines, and syncs Org property
#+PROPERTY: CARRIAGE_ITERATION_ID.

When optional ID is non-nil, reuse it instead of generating a new one."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let* ((id (or id (carriage-iteration--generate-id)))
         (cnt 0))
    (setq carriage--last-iteration-id id)
    (carriage-log "mark-last-iteration: region %d..%d id=%s"
                  beg end (substring id 0 8))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (re-search-forward "^[ \t]*#\\+begin_patch\\b" end t))
        (let ((start (match-beginning 0)))
          (save-excursion
            (goto-char start)
            (let* ((line-beg (line-beginning-position))
                   (line-end (line-end-position)))
              (add-text-properties line-beg line-end
                                   (list 'carriage-iteration-id id))
              (setq cnt (1+ cnt)))))
        ;; Skip to end of this block to avoid nested matches
        (when (re-search-forward "^[ \t]*#\\+end_patch\\b" end t)
          (forward-line 1))))
    ;; Best-effort sync to Org property
    (ignore-errors (carriage-iteration--write-org-id id))
    (message "Carriage: marked %d block(s) as last iteration (id=%s)" cnt (substring id 0 8))
    id))

;;;###autoload
(defun carriage-current-iteration-id ()
  "Return current buffer's last iteration id, reading markers if needed."
  (interactive)
  (let* ((id (carriage-iteration-read-id)))
    (when id (setq carriage--last-iteration-id id))
    (when (called-interactively-p 'any)
      (message (if id "Last iteration id: %s" "No last iteration id set")
               (or id "")))
    id))

;;;###autoload
(defun carriage-begin-iteration ()
  "Generate and set iteration id before streaming; write marker per placement policy."
  (interactive)
  (let ((id (carriage-iteration--generate-id)))
    (setq carriage--last-iteration-id id)
    (when (eq carriage-iteration-marker-placement 'property)
      (ignore-errors (carriage-iteration--write-org-id id)))
    id))

;;; Iteration marker placement and helpers

(defcustom carriage-iteration-marker-placement 'inline
  "Where to place visual iteration marker for last responses:
- 'inline    — insert a dedicated line just above the streamed response:
               blank line + \"#+CARRIAGE_ITERATION_ID: <id>\"
- 'property  — use only Org #+PROPERTY header and do not insert inline markers."
  :type '(choice (const inline) (const property))
  :group 'carriage)

(defun carriage-iteration--write-inline-marker (pos id)
  "Insert inline iteration marker at a safe position:
- Always at the beginning of a line (BOL).
- If POS is inside a #+begin_patch … #+end_patch block, place the marker
  above the block's begin line (so it never splits a patch block).
Returns buffer position after insertion."
  (when (and (stringp id) (> (length (string-trim id)) 0)
             (numberp pos))
    (save-excursion
      (goto-char pos)
      (let* ((case-fold-search t)
             (here (point))
             (beg-internal nil)
             (end-before nil)
             (end-forward nil))
        ;; Find nearest begin_patch above POS
        (save-excursion
          (when (re-search-backward "^[ \t]*#\\+begin_patch\\b" nil t)
            (setq beg-internal (line-beginning-position))))
        ;; Find nearest end_patch above POS
        (save-excursion
          (when (re-search-backward "^[ \t]*#\\+end_patch\\b" nil t)
            (setq end-before (line-beginning-position))))
        ;; If inside a patch block (last begin after last end), prefer placing BELOW the block:
        ;; jump to the end marker after POS and insert on the next line.
        (when (and beg-internal (or (null end-before) (> beg-internal end-before)))
          (save-excursion
            (when (re-search-forward "^[ \t]*#\\+end_patch\\b" nil t)
              (setq end-forward (line-end-position))))
          (setq here (or end-forward here)))
        (goto-char here)
        (let ((bol (line-beginning-position)))
          (goto-char bol)
          ;; Ensure a visual blank line above the marker (when not at bob or prev non-blank).
          (unless (or (bobp)
                      (save-excursion
                        (forward-line -1)
                        (looking-at-p "^[ \t]*$")))
            (insert "\n"))
          (insert (format "#+CARRIAGE_ITERATION_ID: %s\n" (downcase id)))
          (point))))))

(defun carriage-iteration-read-id ()
  "Read iteration id for the current buffer.

Precedence:
1) Org PROPERTY header anywhere in the buffer:
   \"#+PROPERTY: CARRIAGE_ITERATION_ID <id>\"
2) Inline marker line:
   \"#+CARRIAGE_ITERATION_ID: <id>\"

Returns the id string or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (rx-prop "^[ \t]*#\\+PROPERTY:[ \t]*CARRIAGE_ITERATION_ID[ \t]+\\([0-9a-fA-F-]+\\)")
          (rx-inline "^[ \t]*#\\+CARRIAGE_ITERATION_ID:[ \t]*\\([0-9a-fA-F-]+\\)"))
      (cond
       ((re-search-forward rx-prop nil t)
        (downcase (match-string 1)))
       ((progn (goto-char (point-min))
               (re-search-forward rx-inline nil t))
        (downcase (match-string 1)))
       (t nil)))))

(provide 'carriage-iteration)
;;; carriage-iteration.el ends here
