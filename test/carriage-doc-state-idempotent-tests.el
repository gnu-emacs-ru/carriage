;;; carriage-doc-state-idempotent-tests.el --- Idempotent before-save normalization (CARRIAGE_STATE) -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Ensure before-save normalization:
;; - keeps exactly one canonical #+PROPERTY: CARRIAGE_STATE line,
;; - positions it in the canonical top-of-file header slot (after last top PROPERTY),
;; - is idempotent across multiple invocations,
;; - never ends up inside any #+begin_* block (e.g., begin_context).

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'carriage-doc-state)

(defun carriage--cds--count-state-lines ()
  "Return number of #+PROPERTY: CARRIAGE_STATE lines in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((n 0)
          (case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b" nil t)
        (setq n (1+ n)))
      n)))

(defun carriage--cds--first-state-pos ()
  "Return position of the first #+PROPERTY: CARRIAGE_STATE line, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+PROPERTY:[ \t]+CARRIAGE_STATE\\b" nil t)
        (match-beginning 0)))))

(defun carriage--cds--pos-last-top-property ()
  "Return line-end position of the last top-of-file #+PROPERTY: line, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (pos nil)
          (done nil)
          (seen-hdr nil))
      (while (and (not done) (not (eobp)))
        (cond
         ((looking-at-p "^[ \t]*$") (if seen-hdr (setq done t) (forward-line 1)))
         ((looking-at-p "^[ \t]*#\\([^+]\\|$\\)") (if seen-hdr (setq done t) (forward-line 1)))
         ((looking-at-p "^[ \t]*#\\+begin_\\b") (setq done t))
         ((looking-at-p "^[ \t]*#\\+PROPERTY:")
          (setq seen-hdr t)
          (setq pos (line-end-position))
          (forward-line 1))
         ((looking-at-p "^[ \t]*#\\+[A-Za-z0-9_]+:")
          (setq seen-hdr t)
          (forward-line 1))
         ((looking-at-p "^[ \t]*#\\+") (setq done t))
         (t (setq done t))))
      pos)))

(defun carriage--cds--pos-begin-context ()
  "Return position of the first #+begin_context line, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+begin_context\\b" nil t)
        (match-beginning 0)))))

(ert-deftest carriage-doc-state/idempotent-before-save-normalizes-carriage-state ()
  "Before-save hook keeps exactly one CARRIAGE_STATE line in canonical header slot, idempotently."
  (with-temp-buffer
    (org-mode)
    ;; Header + a begin_context block near the top. Also include a misplaced CARRIAGE_STATE
    ;; line below begin_context to emulate the historical “printed somewhere” failure mode.
    (insert "#+title: Demo\n"
            "#+PROPERTY: A x\n"
            "#+PROPERTY: B y\n"
            "\n"
            "#+begin_context\n"
            " /tmp/file1\n"
            "#+end_context\n"
            "\n"
            "#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t :CAR_INTENT Ask)\n"
            "\n"
            "* Note\nBody\n")
    (setq-local carriage-doc-state-save-on-save t)
    (carriage-doc-state-install-save-hook)

    ;; Simulate two saves.
    (run-hooks 'before-save-hook)
    (should (= (carriage--cds--count-state-lines) 1))
    (let* ((pos-last-prop (carriage--cds--pos-last-top-property))
           (pos-state (carriage--cds--first-state-pos))
           (pos-ctx (carriage--cds--pos-begin-context)))
      (should (numberp pos-state))
      ;; Must be after the last top-of-file property line.
      (should (and (numberp pos-last-prop) (> pos-state pos-last-prop)))
      ;; Must be before begin_context (i.e., never inserted into/after the first begin_* block).
      (should (and (numberp pos-ctx) (< pos-state pos-ctx))))

    ;; Second invocation must be idempotent (buffer text stable).
    (let ((s1 (buffer-substring-no-properties (point-min) (point-max))))
      (run-hooks 'before-save-hook)
      (should (= (carriage--cds--count-state-lines) 1))
      (let ((s2 (buffer-substring-no-properties (point-min) (point-max))))
        (should (equal s1 s2))))

    ;; And it must remain readable as a plist with :CAR_* keys.
    (let ((pl (carriage-doc-state-read (current-buffer))))
      (should (plist-member pl :CAR_MODE)))))

(ert-deftest carriage-doc-state/idempotent-before-save-recovers-from-invalid-carriage-state ()
  "before-save normalization must not signal on invalid CARRIAGE_STATE and must re-canonicalize."
  (with-temp-buffer
    (org-mode)
    ;; Header + a begin_context block near the top; plus a broken state line below it.
    (insert "#+title: Demo\n"
            "#+PROPERTY: A x\n"
            "#+PROPERTY: B y\n"
            "\n"
            "#+begin_context\n"
            " /tmp/file1\n"
            "#+end_context\n"
            "\n"
            "#+PROPERTY: CARRIAGE_STATE (\n"
            "\n"
            "* Note\nBody\n")
    (setq-local carriage-doc-state-save-on-save t)
    (carriage-doc-state-install-save-hook)

    ;; Must not signal; must normalize to exactly one state line in canonical slot.
    (should (ignore-errors (run-hooks 'before-save-hook) t))
    (should (= (carriage--cds--count-state-lines) 1))

    (let* ((pos-last-prop (carriage--cds--pos-last-top-property))
           (pos-state (carriage--cds--first-state-pos))
           (pos-ctx (carriage--cds--pos-begin-context)))
      (should (numberp pos-state))
      ;; Must be after the last top-of-file property line.
      (should (and (numberp pos-last-prop) (> pos-state pos-last-prop)))
      ;; Must be before begin_context (never inside/after begin_* blocks).
      (should (and (numberp pos-ctx) (< pos-state pos-ctx))))

    ;; Must become readable again.
    (let ((pl (carriage-doc-state-read (current-buffer))))
      (should (listp pl))
      (should (plist-member pl :CAR_MODE)))

    ;; Second invocation must be idempotent (buffer text stable).
    (let ((s1 (buffer-substring-no-properties (point-min) (point-max))))
      (should (ignore-errors (run-hooks 'before-save-hook) t))
      (should (= (carriage--cds--count-state-lines) 1))
      (let ((s2 (buffer-substring-no-properties (point-min) (point-max))))
        (should (equal s1 s2))))))

(provide 'carriage-doc-state-idempotent-tests)
;;; carriage-doc-state-idempotent-tests.el ends here
