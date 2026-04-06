;;; carriage-doc-state-fold.el --- Fold overlays for document state  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Fold overlays for CARRIAGE_STATE, FINGERPRINT and RESULT lines.
;; Extracted from carriage-doc-state.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-doc-state-fold nil
  "Fold UI for Carriage state/fingerprint lines."
  :group 'carriage-doc-state)

(defvar-local carriage-doc-state--fold-state-ov nil)
(defvar-local carriage-doc-state--fold-fp-ovs nil)
(defvar-local carriage-doc-state--fold-result-ovs nil)
(defvar-local carriage-doc-state--fold--active-fp-ov nil)

(defvar-local carriage-doc-state--fold--pending-kind nil
  "Which doc-state overlays need a refresh. Nil means full refresh.")

(defvar-local carriage-doc-state--fold-refresh-timer nil)
(defvar-local carriage-doc-state--fold--refresh-pending nil)
(defvar-local carriage-doc-state--fold-enabled nil)

(defvar-local carriage-doc-state--fold--last-scan-tick nil)
(defvar-local carriage-doc-state--fold--last-scan-had-lines :unknown)
(defvar-local carriage-doc-state--fold--last-scan-env nil)

(defun carriage-doc-state--fold--line-range-at-point ()
  "Return (BEG . END) for current logical line, excluding trailing newline."
  (save-excursion
    (cons (line-beginning-position)
          (line-end-position))))

(defun carriage-doc-state--fold--point-inside-ov-p (ov)
  "Non-nil if point is inside OV. End position is treated as inside."
  (when (overlayp ov)
    (let ((beg (overlay-start ov))
          (end (overlay-end ov)))
      (and (number-or-marker-p beg)
           (number-or-marker-p end)
           (<= beg (point))
           (<= (point) end)))))

(defun carriage-doc-state--fold--ov-set-folded (ov)
  "Set OV to folded display state."
  (when (overlayp ov)
    (overlay-put ov 'display (overlay-get ov 'carriage-fold-summary))
    (overlay-put ov 'help-echo (overlay-get ov 'carriage-fold-tooltip))
    (overlay-put ov 'mouse-face 'highlight)))

(defun carriage-doc-state--fold--ov-set-revealed (ov)
  "Set OV to revealed (full text) state."
  (when (overlayp ov)
    (overlay-put ov 'display nil)
    (overlay-put ov 'help-echo (overlay-get ov 'carriage-fold-tooltip))
    (overlay-put ov 'mouse-face nil)))

(defun carriage-doc-state--fold--apply-for-point ()
  "Apply reveal/fold depending on current point for fold overlays."
  (when (and carriage-doc-state--fold-enabled
             (get-buffer-window (current-buffer) t))
    (let ((state-ov carriage-doc-state--fold-state-ov))
      (when (overlayp state-ov)
        (if (carriage-doc-state--fold--point-inside-ov-p state-ov)
            (carriage-doc-state--fold--ov-set-revealed state-ov)
          (carriage-doc-state--fold--ov-set-folded state-ov))))
    (let ((active carriage-doc-state--fold--active-fp-ov))
      (when (overlayp active)
        (if (carriage-doc-state--fold--point-inside-ov-p active)
            nil
          (carriage-doc-state--fold--ov-set-folded active)
          (setq carriage-doc-state--fold--active-fp-ov nil))))
    (dolist (ov (append (if (listp carriage-doc-state--fold-fp-ovs)
                           carriage-doc-state--fold-fp-ovs)
                        (if (listp carriage-doc-state--fold-result-ovs)
                            carriage-doc-state--fold-result-ovs)))
      (when (and (overlayp ov)
                 (not (eq ov carriage-doc-state--fold--active-fp-ov)))
        (when (carriage-doc-state--fold--point-inside-ov-p ov)
          (carriage-doc-state--fold--ov-set-revealed ov)
          (setq carriage-doc-state--fold--active-fp-ov ov))))))

(defun carriage-doc-state--fold--maybe-summary-from-plist (pl kind)
  "Generate summary string from PLIST for KIND (state/fingerprint/result)."
  (let* ((imp (carriage-doc-state--important-plist pl))
         (intent (plist-get imp :CAR_INTENT))
         (suite (plist-get imp :CAR_SUITE))
         (model (plist-get imp :CAR_MODEL))
         (ctx-doc (plist-get imp :CAR_CTX_DOC))
         (ctx-patched (plist-get imp :CAR_CTX_PATCHED))
         (scope (plist-get imp :CAR_DOC_CTX_SCOPE))
         (profile (plist-get imp :CAR_CTX_PROFILE)))
    (cond
     ((eq kind 'state)
      (format "%s %s %s"
              (or (carriage-doc-state--ui-icon (carriage-doc-state--llm-display-name
                                                (plist-get imp :CAR_BACKEND)
                                                (plist-get imp :CAR_PROVIDER)
                                                model) "M")
                  "M")
              (or intent "")
              (or suite "")))
     ((eq kind 'fingerprint)
      (carriage-doc-state--ctx-flag-badge-with-label
       (cond
        ((and ctx-doc ctx-patched) "DP")
        (ctx-doc "Doc")
        (ctx-patched "Pat")
        (t "-"))
       t
       (carriage-doc-state--ui-icon 'ctx nil)))
     ((eq kind 'result)
      (let ((cost-u (plist-get pl :CAR_COST_TOTAL_U)))
        (if (integerp cost-u)
            (carriage-doc-state--format-money-suffix cost-u)
          "—")))
     (t ""))))

(defun carriage-doc-state--fold--tooltip (raw-line pl kind)
  "Generate tooltip for fold overlay from RAW-LINE, PLIST and KIND."
  (format "CARRIAGE_%s: %s"
          (upcase (symbol-name kind))
          (carriage-doc-state--as-string raw-line)))

(defun carriage-doc-state--fold--parse-sexp (s)
  "Parse S as sexp, return plist or nil."
  (when (and (stringp s) (not (string-empty-p s)))
    (ignore-errors (car (read-from-string s)))))

(defun carriage-doc-state--fold--scan-state-line ()
  "Scan for CARRIAGE_STATE line, return (beg . end) or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b\\(.*\\)$" nil t)
        (cons (match-beginning 0) (line-end-position))))))

(defun carriage-doc-state--fold--scan-fingerprint-lines ()
  "Scan for CARRIAGE_FINGERPRINT lines, return list of (beg . end)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((ranges '()))
        (while (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT:[ \t]*\\(.*\\)$" nil t)
          (push (cons (match-beginning 0) (line-end-position)) ranges))
        (nreverse ranges)))))

(defun carriage-doc-state--fold--scan-result-lines ()
  "Scan for CARRIAGE_RESULT lines, return list of (beg . end)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((ranges '()))
        (while (re-search-forward "^[ \t]*#\\+CARRIAGE_RESULT:[ \t]*\\(.*\\)$" nil t)
          (push (cons (match-beginning 0) (line-end-position)) ranges))
        (nreverse ranges)))))

(defun carriage-doc-state--fold--ov-upsert (ov beg end summary tooltip)
  "Update or create overlay OV at BEG..END with SUMMARY and TOOLTIP."
  (if (overlayp ov)
      (progn
        (move-overlay ov beg end)
        (overlay-put ov 'carriage-fold-summary summary)
        (overlay-put ov 'carriage-fold-tooltip tooltip)
        (carriage-doc-state--fold--ov-set-folded ov))
    (let ((new-ov (make-overlay beg end nil nil t)))
      (overlay-put new-ov 'carriage-fold-summary summary)
      (overlay-put new-ov 'carriage-fold-tooltip tooltip)
      (overlay-put new-ov 'evaporate t)
      (carriage-doc-state--fold--ov-set-folded new-ov)
      new-ov)))

(defun carriage-doc-state--fold--refresh-overlays ()
  "Refresh all fold overlays for state/fingerprint/result lines."
  (setq carriage-doc-state--fold--refresh-pending nil)
  (when (and carriage-doc-state--fold-enabled
             (derived-mode-p 'org-mode))
    (let* ((state-range (carriage-doc-state--fold--scan-state-line))
           (fp-ranges (carriage-doc-state--fold--scan-fingerprint-lines))
           (result-ranges (carriage-doc-state--fold--scan-result-lines)))
      (setq carriage-doc-state--fold-state-ov
            (if state-range
                (let* ((beg (car state-range))
                       (end (cdr state-range))
                       (raw (buffer-substring-no-properties beg end))
                       (pl (carriage-doc-state--fold--parse-sexp (car (last (split-string raw ":")))))
                       (summary (carriage-doc-state--fold--maybe-summary-from-plist pl 'state))
                       (tooltip (carriage-doc-state--fold--tooltip raw pl 'state)))
                  (carriage-doc-state--fold--ov-upsert
                   carriage-doc-state--fold-state-ov beg end summary tooltip))
              (when (overlayp carriage-doc-state--fold-state-ov)
                (delete-overlay carriage-doc-state--fold-state-ov)
                nil)))
      (setq carriage-doc-state--fold-fp-ovs
            (mapcar (lambda (rng)
                      (let* ((beg (car rng))
                             (end (cdr rng))
                             (raw (buffer-substring-no-properties beg end))
                             (pl (carriage-doc-state--fold--parse-sexp (car (last (split-string raw ":")))))
                             (summary (carriage-doc-state--fold--maybe-summary-from-plist pl 'fingerprint))
                             (tooltip (carriage-doc-state--fold--tooltip raw pl 'fingerprint)))
                        (carriage-doc-state--fold--ov-upsert nil beg end summary tooltip)))
                    fp-ranges))
      (setq carriage-doc-state--fold-result-ovs
            (mapcar (lambda (rng)
                      (let* ((beg (car rng))
                             (end (cdr rng))
                             (raw (buffer-substring-no-properties beg end))
                             (pl (carriage-doc-state--fold--parse-sexp (car (last (split-string raw ":")))))
                             (summary (carriage-doc-state--fold--maybe-summary-from-plist pl 'result))
                             (tooltip (carriage-doc-state--fold--tooltip raw pl 'result)))
                        (carriage-doc-state--fold--ov-upsert nil beg end summary tooltip)))
                    result-ranges))
      (setq carriage-doc-state--fold--last-scan-tick (buffer-chars-modified-tick)))))

(defun carriage-doc-state--fold--schedule-refresh (beg end _len)
  "Schedule debounced fold overlay refresh after change at BEG..END."
  (when carriage-doc-state--fold-enabled
    (unless carriage-doc-state--fold--refresh-pending
      (setq carriage-doc-state--fold--refresh-pending t)
      (when (timerp carriage-doc-state--fold-refresh-timer)
        (cancel-timer carriage-doc-state--fold-refresh-timer))
      (setq carriage-doc-state--fold-refresh-timer
            (run-at-time 0.1 nil
                         (lambda ()
                           (setq carriage-doc-state--fold-refresh-timer nil)
                           (ignore-errors (carriage-doc-state--fold--refresh-overlays))))))))

;;;###autoload
(defun carriage-doc-state-summary-enable (&optional buffer)
  "Enable fold overlays for state/fingerprint/result lines in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-doc-state--fold-enabled t)
    (add-hook 'after-change-functions #'carriage-doc-state--fold--schedule-refresh nil t)
    (add-hook 'post-command-hook #'carriage-doc-state--fold--apply-for-point nil t)
    (carriage-doc-state--fold--refresh-overlays)
    t))

;;;###autoload
(defun carriage-doc-state-summary-disable (&optional buffer)
  "Disable fold overlays for state/fingerprint/result lines in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (setq carriage-doc-state--fold-enabled nil)
    (remove-hook 'after-change-functions #'carriage-doc-state--fold--schedule-refresh t)
    (remove-hook 'post-command-hook #'carriage-doc-state--fold--apply-for-point t)
    (when (timerp carriage-doc-state--fold-refresh-timer)
      (cancel-timer carriage-doc-state--fold-refresh-timer))
    (dolist (ov (delq nil (list carriage-doc-state--fold-state-ov
                                carriage-doc-state--fold-fp-ovs
                                carriage-doc-state--fold-result-ovs)))
      (when (overlayp ov) (delete-overlay ov)))
    (setq carriage-doc-state--fold-state-ov nil
          carriage-doc-state--fold-fp-ovs nil
          carriage-doc-state--fold-result-ovs nil)
    t))

(provide 'carriage-doc-state-fold)
;;; carriage-doc-state-fold.el ends here
