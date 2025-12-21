;;; carriage-doc-state-tests.el --- Tests for CARRIAGE_STATE persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Ensure doc-state reads/writes CARRIAGE_STATE property (sexp) and save-hook normalization.

;;; Code:

(require 'ert)
(require 'org)
(require 'carriage-doc-state)

;; Optional vars from other modules: define for restore tests.
(defvar carriage-doc-context-scope nil)
(defvar carriage-mode-doc-context-scope nil)
(defvar carriage-context-profile nil)
(defvar carriage-mode-context-profile nil)
(defvar carriage-mode-include-patched-files nil)

(ert-deftest carriage-doc-state/write-inserts-carriage-state-property ()
  "Writing doc-state creates a single #+PROPERTY: CARRIAGE_STATE line in the header."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n#+PROPERTY: A x\n#+PROPERTY: B y\n\n* Note\nBody\n")
    (let ((pos-last-prop (save-excursion
                           (goto-char (point-min))
                           (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+B\\b" nil t)
                           (line-end-position))))
      (carriage-doc-state-write '(:CAR_MODE t :CAR_INTENT Code))
      (save-excursion
        (goto-char (point-min))
        (should (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b" nil t))
        (let ((pos-state (match-beginning 0)))
          ;; Must be located after the last pre-existing top-of-file PROPERTY line.
          (should (> pos-state pos-last-prop))))
      ;; Must parse as sexp into plist with :CAR_* keys
      (let ((pl (carriage-doc-state-read (current-buffer))))
        (should (plist-member pl :CAR_MODE))
        (should (eq (plist-get pl :CAR_MODE) t))
        (should (eq (plist-get pl :CAR_INTENT) 'Code))))))

(ert-deftest carriage-doc-state/write-is-idempotent ()
  "Second write produces identical buffer contents."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n#+PROPERTY: X 1\n\n* Note\n")
    (carriage-doc-state-write '(:CAR_MODE t :CAR_SUITE aibo))
    (let ((s1 (buffer-substring-no-properties (point-min) (point-max))))
      (carriage-doc-state-write '(:CAR_MODE t :CAR_SUITE aibo))
      (let ((s2 (buffer-substring-no-properties (point-min) (point-max))))
        (should (equal s1 s2))))))

(ert-deftest carriage-doc-state/before-save-normalizes-carriage-state ()
  "Installing before-save hook persists normalized CARRIAGE_STATE (sexp)."
  (with-temp-buffer
    (org-mode)
    (carriage-mode 1)
    (insert "#+title: Demo\n#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t :CAR_INTENT Ask)\n\n* Note\nBody\n")
    (setq-local carriage-doc-state-save-on-save t)
    (carriage-doc-state-install-save-hook)
    (run-hooks 'before-save-hook)
    (save-excursion
      (goto-char (point-min))
      (should (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b" nil t)))
    (let ((pl (carriage-doc-state-read (current-buffer))))
      (should (eq (plist-get pl :CAR_MODE) t)))))

(ert-deftest carriage-doc-state/invalid-carriage-state-never-breaks-and-can-be-rewritten ()
  "Invalid/unreadable CARRIAGE_STATE must not signal; defaults remain; writer can re-normalize."
  (with-temp-buffer
    (org-mode)
    ;; Broken sexp: unreadable.
    (insert "#+title: Demo\n#+PROPERTY: CARRIAGE_STATE (\n\n* Note\nBody\n")
    ;; Defaults (or whatever current buffer-local state is) must not be clobbered by failing restore.
    (setq-local carriage-mode-intent 'Ask)

    ;; Must not signal.
    (should (ignore-errors (carriage-doc-state-restore (current-buffer)) t))
    (should (eq carriage-mode-intent 'Ask))

    ;; Writer must not signal and must produce a readable CARRIAGE_STATE.
    (should (ignore-errors (carriage-doc-state-write-current (current-buffer)) t))
    (let ((pl (carriage-doc-state-read (current-buffer))))
      (should (listp pl))
      ;; At least the canonical keys should be representable/readable.
      (should (plist-member pl :CAR_MODE)))))

(ert-deftest carriage-doc-state/restore-applies-doc-context-scope-and-profile-when-vars-exist ()
  "Restore should apply :CAR_DOC_CTX_SCOPE and :CAR_CTX_PROFILE when corresponding vars exist."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n"
            "#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t :CAR_DOC_CTX_SCOPE LastCtx :CAR_CTX_PROFILE P1 :CAR_CTX_PATCHED t)\n"
            "\n* Note\nBody\n")
    ;; Ensure globals have non-matching values first, and check buffer-local after restore.
    (setq carriage-doc-context-scope 'AllCtx)
    (setq carriage-context-profile 'P0)
    (setq carriage-mode-include-patched-files nil)
    (carriage-doc-state-restore (current-buffer))
    (should (equal (buffer-local-value 'carriage-doc-context-scope (current-buffer)) 'LastCtx))
    (should (equal (buffer-local-value 'carriage-context-profile (current-buffer)) 'P1))
    (should (eq (buffer-local-value 'carriage-mode-include-patched-files (current-buffer)) t))))

(ert-deftest carriage-doc-state/summary-overlay-fold-reveal-and-tooltip-budgets ()
  "CARRIAGE_STATE summary overlay should fold to badges, reveal on cursor enter, and show budgets in tooltip."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n"
            "#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t :CAR_INTENT Code :CAR_SUITE udiff :CAR_BACKEND gptel :CAR_PROVIDER openai :CAR_MODEL gpt-4.1 "
            ":CAR_CTX_DOC t :CAR_CTX_GPTEL nil :CAR_CTX_VISIBLE t :CAR_DOC_CTX_SCOPE last :CAR_CTX_PROFILE p1 "
            ":CAR_CTX_MAX_FILES 10 :CAR_CTX_MAX_BYTES 1234 :CAR_CTX_INJECTION system)\n"
            "\n* Note\nBody\n")
    (setq-local carriage-doc-state-summary-enable t)

    ;; Hide should create a summary overlay (best-effort).
    (should (carriage-doc-state-hide (current-buffer)))
    (should (overlayp carriage-doc-state--overlay))

    ;; Folded state should have display (summary) and tooltip (help-echo) with budgets.
    (let* ((ov carriage-doc-state--overlay)
           (disp (overlay-get ov 'display))
           (he (overlay-get ov 'help-echo)))
      (should (stringp disp))
      (should (> (length (string-trim disp)) 0))
      (should (stringp he))
      (should (string-match-p "Budgets: max-files=10 max-bytes=1234" he)))

    ;; Move point onto the property line: should reveal (display becomes nil).
    (goto-char (overlay-start carriage-doc-state--overlay))
    (run-hooks 'post-command-hook)
    (should (null (overlay-get carriage-doc-state--overlay 'display)))

    ;; Move point away: should fold again (display restored).
    (goto-char (point-max))
    (run-hooks 'post-command-hook)
    (should (stringp (overlay-get carriage-doc-state--overlay 'display)))))

(ert-deftest carriage-doc-state/summary-overlay-updates-after-write ()
  "After carriage-doc-state-write, summary overlay should refresh (best-effort) and not duplicate."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n#+PROPERTY: X 1\n\n* Note\n")
    (setq-local carriage-doc-state-summary-enable t)
    (carriage-doc-state-write '(:CAR_MODE t :CAR_INTENT Ask :CAR_SUITE udiff :CAR_BACKEND gptel :CAR_PROVIDER openai :CAR_MODEL gpt-4.1
                                          :CAR_CTX_DOC t :CAR_CTX_GPTEL t :CAR_CTX_VISIBLE nil
                                          :CAR_CTX_MAX_FILES 10 :CAR_CTX_MAX_BYTES 1234))
    ;; Hide to ensure overlay exists
    (carriage-doc-state-hide (current-buffer))
    (let* ((ov carriage-doc-state--overlay)
           (s1 (overlay-get ov 'display)))
      (should (overlayp ov))
      (should (stringp s1))
      ;; Update state (intent/model), overlay must remain single and summary should change.
      (carriage-doc-state-write '(:CAR_MODE t :CAR_INTENT Code :CAR_SUITE udiff :CAR_BACKEND gptel :CAR_PROVIDER openai :CAR_MODEL gpt-4o-mini
                                            :CAR_CTX_DOC t :CAR_CTX_GPTEL t :CAR_CTX_VISIBLE nil
                                            :CAR_CTX_MAX_FILES 10 :CAR_CTX_MAX_BYTES 1234))
      (let* ((ov2 carriage-doc-state--overlay)
             (s2 (overlay-get ov2 'display)))
        (should (eq ov2 ov))
        (should (stringp s2))
        (should (not (equal s1 s2)))))))

(provide 'carriage-doc-state-tests)
;;; carriage-doc-state-tests.el ends here
