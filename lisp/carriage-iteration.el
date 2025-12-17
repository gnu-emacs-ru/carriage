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
             (line (concat "#+PROPERTY: CARRIAGE_ITERATION_ID " (downcase id))))
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
  "Generate and set iteration id before streaming; write marker per placement policy.

Idempotent: if `carriage--last-iteration-id' is already a non-empty string,
reuse it instead of generating a new one."
  (interactive)
  (let* ((existing (and (stringp carriage--last-iteration-id)
                        (not (string-empty-p (string-trim carriage--last-iteration-id)))
                        carriage--last-iteration-id))
         (id (or existing (carriage-iteration--generate-id))))
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

(defcustom carriage-mode-insert-separator-before-id t
  "When non-nil, insert a visual separator line just above the inline CARRIAGE_ID marker."
  :type 'boolean :group 'carriage)

(defcustom carriage-mode-separator-text "-----"
  "Separator line text inserted above the CARRIAGE_ID when enabled.
Keep it short to avoid wrapping."
  :type 'string :group 'carriage)

(defun carriage-iteration--write-inline-marker (pos id)
  "Insert inline iteration marker starting at beginning of current line, with optional separator.

Behavior:
- If POS is not at BOL, finish the line and insert a single newline (no extra blank lines at BOL).
- If `carriage-mode-insert-separator-before-id' is non-nil (default), ensure there is a
  separator line just above the marker; if the previous line is already the separator, do nothing.
  If the previous line is an empty line, replace that empty line with the separator instead of
  inserting yet another blank line.
- Insert the marker line \"#+CARRIAGE_ITERATION_ID: <id>\" and a trailing newline.
Return buffer position after the inserted marker (beginning of the next line)."
  (when (and (stringp id) (> (length (string-trim id)) 0) (numberp pos))
    (save-excursion
      (goto-char pos)
      ;; Normalize to BOL: only add a single newline when not at BOL.
      ;; Insert newline right at point (not at EOL) so the separator+ID appear
      ;; directly under the cursor position.
      (unless (bolp)
        (insert "\n"))
      ;; Optionally add a separator line above (without duplication).
      (when (if (boundp 'carriage-mode-insert-separator-before-id)
                carriage-mode-insert-separator-before-id
              t)
        (let* ((sep (if (and (boundp 'carriage-mode-separator-text)
                             (stringp carriage-mode-separator-text)
                             (> (length carriage-mode-separator-text) 0))
                        carriage-mode-separator-text
                      "-----"))
               (sep-trim (string-trim sep))
               (have-prev (save-excursion (> (point) (point-min))))
               (prev-line (save-excursion
                            (when have-prev
                              (forward-line -1)
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))
               (prev-trim (string-trim (or prev-line ""))))
          (let* ((marker (format "#+CARRIAGE_ITERATION_ID: %s" (downcase id)))
                 (marker-trim (string-trim marker)))
            ;; Idempotency guard: if we are called twice at the same place,
            ;; the previous line will already be the marker for this id.
            ;; In that case, do nothing and keep insertion position below the marker.
            (when (and have-prev (string= prev-trim marker-trim))
              (cl-return-from carriage-iteration--write-inline-marker (point)))
            (unless (string= prev-trim sep-trim)
              (if (and have-prev (string-empty-p prev-trim))
                  ;; Replace the empty previous line with the separator (avoid extra blank gap).
                  (save-excursion
                    (forward-line -1)
                    (delete-region (line-beginning-position) (line-end-position))
                    (insert sep))
                ;; Otherwise, just add the separator on a fresh line above.
                (insert sep "\n"))))))
      ;; Insert the marker and return position at start of next line.
      (insert (format "#+CARRIAGE_ITERATION_ID: %s\n" (downcase id)))
      (point))))

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

;; Ensure the separator line is present directly above the newly inserted
;; CARRIAGE_ITERATION_ID line, when enabled. We do it immediately after
;; the inline marker insertion, so the visual order is:
;;   separator (optional) -> CARRIAGE_ID -> spinner (on its own line).

;; -------------------------------------------------------------------
;; Robust inline marker writer (single source of truth + hard idempotency)
;;
;; Rationale:
;; - We may be called more than once per request due to timers/transient/advices.
;; - Duplicates typically manifest as:
;;     ----- + ID
;;     (preloader)
;;     ID
;; - Therefore we must make marker insertion *hermetically idempotent* based on
;;   buffer text, not on fragile flags/advices.
;;
;; Policy:
;; - Never insert duplicate marker for the same ID when it's already present at
;;   (or immediately around) POS.
;; - Guard works regardless of separator settings.
;; - Return position at the beginning of the line *after* the marker when the
;;   marker already exists or after insertion (for stream-origin / preloader).

(defun carriage-iteration--write-inline-marker (pos id)
  "Insert inline iteration marker at POS with optional separator, idempotently.

Rules:
- If POS is not at BOL, insert a single newline at POS (so marker appears directly below cursor).
- If a marker for the same ID is already present at/around POS, do not insert again.
  Return position after the existing marker.
- If separator is enabled, ensure exactly one separator line directly above the marker.
- Insert marker line \"#+CARRIAGE_ITERATION_ID: <id>\" (lowercased) and a trailing newline.
Return buffer position after the marker (BOL of the next line), or nil when not inserted."
  (when (and (stringp id)
             (> (length (string-trim id)) 0)
             (numberp pos))
    (save-excursion
      (goto-char pos)
      (let* ((marker (format "#+CARRIAGE_ITERATION_ID: %s" (downcase id)))
             (marker-trim (string-trim marker))
             (sep (if (and (boundp 'carriage-mode-separator-text)
                           (stringp carriage-mode-separator-text)
                           (> (length carriage-mode-separator-text) 0))
                      carriage-mode-separator-text
                    "-----"))
             (sep-trim (string-trim sep)))
        ;; Normalize insertion point: put marker on its own line directly below cursor.
        (unless (bolp)
          (insert "\n"))
        (catch 'carriage--inline-marker-done
          ;; Helper to read trimmed line at point (without moving permanently).
          (cl-labels
              ((line-trim-at (p)
                 (save-excursion
                   (goto-char p)
                   (string-trim
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))))
               (safe-forward-line (n)
                 (ignore-errors (forward-line n))))
            (let* ((bol (line-beginning-position))
                   (have-prev (> bol (point-min)))
                   (prev-trim (and have-prev (line-trim-at (save-excursion (safe-forward-line -1) (point)))))
                   (cur-trim  (line-trim-at (point)))
                   (next-trim (let ((p (save-excursion (safe-forward-line 1) (point))))
                                (and (< p (point-max)) (line-trim-at p)))))

              ;; --- HARD idempotency guards (independent of separator setting) ---

              ;; Case A: we are already on the marker line
              (when (string= cur-trim marker-trim)
                (safe-forward-line 1)
                (throw 'carriage--inline-marker-done (point)))

              ;; Case B: previous line is the marker (common after first insertion moves origin below marker)
              (when (and (stringp prev-trim) (string= prev-trim marker-trim))
                (throw 'carriage--inline-marker-done (point)))

              ;; Case C: we are on separator and the next line is the marker
              (when (and (string= cur-trim sep-trim)
                         (stringp next-trim)
                         (string= next-trim marker-trim))
                (safe-forward-line 2)
                (throw 'carriage--inline-marker-done (point)))

              ;; Case D: previous line is separator and current is marker (paranoia)
              (when (and (string= prev-trim sep-trim)
                         (string= cur-trim marker-trim))
                (safe-forward-line 1)
                (throw 'carriage--inline-marker-done (point)))

              ;; --- Ensure separator (optional), without duplication ---
              (when (if (boundp 'carriage-mode-insert-separator-before-id)
                        carriage-mode-insert-separator-before-id
                      t)
                (let* ((prev-line (when have-prev
                                    (save-excursion
                                      (safe-forward-line -1)
                                      (buffer-substring-no-properties
                                       (line-beginning-position) (line-end-position)))))
                       (prev-trim2 (string-trim (or prev-line ""))))
                  (unless (string= prev-trim2 sep-trim)
                    (if (and have-prev (string-empty-p prev-trim2))
                        ;; Replace empty previous line with separator.
                        (save-excursion
                          (safe-forward-line -1)
                          (delete-region (line-beginning-position) (line-end-position))
                          (insert sep))
                      (insert sep "\n")))))

              ;; --- Insert marker line ---
              (insert marker "\n")
              (throw 'carriage--inline-marker-done (point)))))))))

(provide 'carriage-iteration)
;;; carriage-iteration.el ends here
