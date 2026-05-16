;;; carriage-doc-state-tests.el --- Tests for CARRIAGE_STATE persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Ensure doc-state reads/writes CARRIAGE_STATE property (sexp) and save-hook normalization.

;;; Code:

(require 'ert)
(require 'cl-lib)
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

(defun carriage--overlay-at-line-prefix-p (prefix)
  "Return non-nil when current line starts with PREFIX and has at least one overlay."
  (save-excursion
    (beginning-of-line)
    (and (looking-at (regexp-quote prefix))
         (overlays-at (line-beginning-position)))))

(ert-deftest carriage-doc-state-summary-refresh-creates-fingerprint-overlay ()
  "Fingerprint line should be folded by a display overlay in Org buffers."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n#+CARRIAGE_FINGERPRINT: (:CAR_INTENT Ask :CAR_SUITE aibo :CAR_MODEL gptel-default)\n\nBody\n")
    (carriage-doc-state-summary-enable (current-buffer))
    (carriage-doc-state-summary-refresh (current-buffer))
    (goto-char (point-min))
    (should (search-forward "#+CARRIAGE_FINGERPRINT:" nil t))
    (let ((ovs (overlays-at (line-beginning-position))))
      (should ovs)
      (should (cl-some (lambda (ov) (overlay-get ov 'display)) ovs))
      (should (cl-some (lambda (ov) (overlay-get ov 'help-echo)) ovs)))))

(ert-deftest carriage-doc-state-summary-refresh-creates-state-overlay ()
  "CARRIAGE_STATE line should be folded by a display overlay in Org buffers."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t :CAR_INTENT Ask)\n\nBody\n")
    (carriage-doc-state-summary-enable (current-buffer))
    (carriage-doc-state-summary-refresh (current-buffer))
    (goto-char (point-min))
    (should (search-forward "#+PROPERTY: CARRIAGE_STATE" nil t))
    (let ((ovs (overlays-at (line-beginning-position))))
      (should ovs)
      (should (cl-some (lambda (ov) (overlay-get ov 'display)) ovs))
      (should (cl-some (lambda (ov) (overlay-get ov 'help-echo)) ovs)))))

(ert-deftest carriage-doc-state-summary-refresh-creates-result-overlay ()
  "CARRIAGE_RESULT line should be folded by a display overlay in Org buffers."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n#+CARRIAGE_RESULT: (:CAR_STATUS done :CAR_COST_TOTAL_U 123)\n\nBody\n")
    (carriage-doc-state-summary-enable (current-buffer))
    (carriage-doc-state-summary-refresh (current-buffer))
    (goto-char (point-min))
    (should (search-forward "#+CARRIAGE_RESULT:" nil t))
    (let ((ovs (overlays-at (line-beginning-position))))
      (should ovs)
      (should (cl-some (lambda (ov) (overlay-get ov 'display)) ovs))
      (should (cl-some (lambda (ov) (overlay-get ov 'help-echo)) ovs)))))

(provide 'carriage-doc-state-tests)
;;; carriage-doc-state-tests.el ends here
