;;; carriage-fingerprint-tests.el --- Tests for per-send CARRIAGE_FINGERPRINT -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Ensure:
;; - Send pipeline can insert an inline CARRIAGE_FINGERPRINT next to the inline iteration marker.
;; - Transports (gptel/echo) MUST filter fingerprint/iteration markers out of outgoing prompts.

;;; Code:

(require 'ert)
(require 'org)
(require 'subr-x)

(require 'carriage-mode)
(require 'carriage-iteration)

;; Transport prompt builders are internal but stable enough for tests.
(require 'carriage-transport-gptel nil t)
(require 'carriage-transport-echo nil t)

(defun carriage--fp--line-at (rx)
  "Return the full matching line for RX in current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward rx nil t)
        (buffer-substring-no-properties
         (line-beginning-position) (line-end-position))))))

(ert-deftest carriage-fingerprint/insert-inline-fingerprint-next-to-iteration-marker ()
  "Inserting inline iteration marker then fingerprint should place fingerprint right after marker."
  (with-temp-buffer
    (org-mode)
    (insert "* Demo\nBody\n\n")
    ;; Ensure iteration id exists
    (setq-local carriage--last-iteration-id "deadbeefdeadbeefdeadbeefdeadbeef")
    ;; Make sure we insert at current point
    (carriage-stream-reset (copy-marker (point) t))

    ;; Configure a few response/context shaping vars to be present in the fingerprint.
    (setq-local carriage-mode-intent 'Ask)
    (setq-local carriage-mode-suite 'aibo)
    (setq-local carriage-mode-backend 'gptel)
    (setq-local carriage-mode-provider "openai")
    (setq-local carriage-mode-model "gpt-4.1")
    (setq-local carriage-mode-include-doc-context t)
    (setq-local carriage-mode-include-gptel-context nil)
    (setq-local carriage-mode-include-visible-context t)
    (setq-local carriage-mode-include-patched-files t)
    (setq-local carriage-mode-context-max-files 10)
    (setq-local carriage-mode-context-max-total-bytes 1234)
    (setq-local carriage-mode-context-injection 'system)

    ;; Insert marker + fingerprint at stream origin.
    (let ((carriage-mode-insert-separator-before-id nil))
      (should (carriage-insert-inline-iteration-marker-now)))
    (should (carriage-insert-inline-fingerprint-now))

    (let* ((idln (carriage--fp--line-at "^[ \t]*#\\+CARRIAGE_ITERATION_ID:[ \t]+\\([0-9a-f]+\\)[ \t]*$"))
           (fpln (carriage--fp--line-at "^[ \t]*#\\+CARRIAGE_FINGERPRINT:[ \t]+\\(.*\\)$")))
      (should (stringp idln))
      (should (stringp fpln))
      ;; Fingerprint should appear after the iteration marker.
      (should (< (string-match-p "^#\\+CARRIAGE_ITERATION_ID:" idln) 999999)) ;; sanity
      (should (< (string-match-p "^#\\+CARRIAGE_FINGERPRINT:" fpln) 999999)) ;; sanity
      (should (< (or (and (stringp idln)
                          (save-excursion
                            (goto-char (point-min))
                            (re-search-forward (regexp-quote idln) nil t)
                            (line-beginning-position)))
                     (point-min))
                 (or (and (stringp fpln)
                          (save-excursion
                            (goto-char (point-min))
                            (re-search-forward (regexp-quote fpln) nil t)
                            (line-beginning-position)))
                     (point-max))))

      ;; Fingerprint line must be readable sexp.
      (let* ((case-fold-search t)
             (pl (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT:[ \t]+\\(.*\\)$" nil t)
                   (car (read-from-string (match-string 1))))))
        (should (listp pl))
        (should (plist-member pl :CAR_ITERATION_ID))
        (should (plist-member pl :CAR_INTENT))
        (should (plist-member pl :CAR_SUITE))
        (should (plist-member pl :CAR_MODEL))
        (should (plist-member pl :CAR_CTX_DOC))
        (should (plist-member pl :CAR_CTX_VISIBLE))
        ;; Budgets captured (but these must never be sent to LLM prompt).
        (should (plist-member pl :CAR_CTX_MAX_FILES))
        (should (plist-member pl :CAR_CTX_MAX_BYTES))))))

(ert-deftest carriage-fingerprint/transports-filter-fingerprint-and-iteration-id ()
  "Transport prompt builders must remove fingerprint/iteration-id markers from outgoing text."
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Demo\n"
            "#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t)\n"
            "\n"
            "-----\n"
            "#+CARRIAGE_ITERATION_ID: deadbeef\n"
            "#+CARRIAGE_FINGERPRINT: (:CAR_INTENT Ask :CAR_SUITE aibo :CAR_CTX_MAX_FILES 10)\n"
            "\n"
            "* Body\nHello\n")
    (let* ((buf (current-buffer))
           ;; gptel
           (gptel-out (when (fboundp 'carriage--gptel--prompt)
                        (carriage--gptel--prompt 'buffer buf 'org-mode)))
           ;; echo
           (echo-out (when (fboundp 'carriage--echo--prompt)
                       (carriage--echo--prompt 'buffer buf 'org-mode))))
      (when (stringp gptel-out)
        (should (not (string-match-p "CARRIAGE_FINGERPRINT" gptel-out)))
        (should (not (string-match-p "CARRIAGE_ITERATION_ID" gptel-out)))
        (should (not (string-match-p "PROPERTY:[ \t]+CARRIAGE_STATE" gptel-out))))
      (when (stringp echo-out)
        (should (not (string-match-p "CARRIAGE_FINGERPRINT" echo-out)))
        (should (not (string-match-p "CARRIAGE_ITERATION_ID" echo-out)))
        (should (not (string-match-p "PROPERTY:[ \t]+CARRIAGE_STATE" echo-out)))))))

(provide 'carriage-fingerprint-tests)
;;; carriage-fingerprint-tests.el ends here
