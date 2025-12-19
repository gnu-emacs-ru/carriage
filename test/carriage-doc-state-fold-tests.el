;;; carriage-doc-state-fold-tests.el --- Tests for fold UI  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'carriage-doc-state)

(defun carriage-test--ov-at-line-containing (needle)
  (save-excursion
    (goto-char (point-min))
    (search-forward needle)
    (let ((pos (line-beginning-position)))
      (cl-find-if (lambda (ov)
                    (and (<= (overlay-start ov) pos)
                         (<= pos (overlay-end ov))
                         (overlay-get ov 'category)))
                  (overlays-in (line-beginning-position) (line-end-position))))))

(ert-deftest carriage-doc-state/fold-state-reveal-hide ()
  (with-temp-buffer
    (org-mode)
    (insert "* H\n")
    (insert "#+PROPERTY: CARRIAGE_STATE (:CAR_INTENT Ask :CAR_SUITE org :CAR_MODEL \"gpt-4.1\" :CAR_CTX_DOC t :CAR_CTX_GPTEL nil :CAR_CTX_VISIBLE nil :CAR_CTX_MAX_FILES 10 :CAR_CTX_MAX_BYTES 12345)\n")
    (goto-char (point-max))
    (carriage-doc-state-summary-enable)

    ;; Folded away from the line.
    (let ((ov (carriage-test--ov-at-line-containing "CARRIAGE_STATE")))
      (should (overlayp ov))
      (should (stringp (overlay-get ov 'display)))
      (should (> (length (string-trim (overlay-get ov 'display))) 0))
      (should (string-match-p "CARRIAGE_STATE" (overlay-get ov 'help-echo)))
      (should (string-match-p "MAX_FILES\\|max-files\\|max-files" (overlay-get ov 'help-echo))))

    ;; Reveal on the line (point inside).
    (goto-char (point-min))
    (search-forward "CARRIAGE_STATE")
    (run-hooks 'post-command-hook)
    (let ((ov (carriage-test--ov-at-line-containing "CARRIAGE_STATE")))
      (should (overlayp ov))
      (should (null (overlay-get ov 'display))))

    ;; Fold again after leaving.
    (goto-char (point-max))
    (run-hooks 'post-command-hook)
    (let ((ov (carriage-test--ov-at-line-containing "CARRIAGE_STATE")))
      (should (overlayp ov))
      (should (stringp (overlay-get ov 'display)))
      (should (> (length (string-trim (overlay-get ov 'display))) 0)))))

(ert-deftest carriage-doc-state/fold-fingerprint-reveal-hide ()
  (with-temp-buffer
    (org-mode)
    (insert "* H\n")
    (insert "#+CARRIAGE_FINGERPRINT: (:ts \"2025-12-19T00:00:00Z\" :iter \"it-001\" :CAR_INTENT Ask :CAR_SUITE org :CAR_MODEL \"gpt-4.1\" :CAR_CTX_DOC t :CAR_CTX_GPTEL t :CAR_CTX_VISIBLE nil :CAR_CTX_MAX_FILES 9 :CAR_CTX_MAX_BYTES 999)\n")
    (goto-char (point-max))
    (carriage-doc-state-summary-enable)

    ;; Folded away from the line.
    (let ((ov (carriage-test--ov-at-line-containing "CARRIAGE_FINGERPRINT")))
      (should (overlayp ov))
      (should (stringp (overlay-get ov 'display)))
      (should (> (length (string-trim (overlay-get ov 'display))) 0))
      (should (string-match-p "CARRIAGE_FINGERPRINT" (overlay-get ov 'help-echo)))
      (should (string-match-p "MAX_BYTES\\|max-bytes\\|MAX_FILES\\|max-files" (overlay-get ov 'help-echo))))

    ;; Reveal on the line.
    (goto-char (point-min))
    (search-forward "CARRIAGE_FINGERPRINT")
    (run-hooks 'post-command-hook)
    (let ((ov (carriage-test--ov-at-line-containing "CARRIAGE_FINGERPRINT")))
      (should (overlayp ov))
      (should (null (overlay-get ov 'display))))

    ;; Fold again after leaving.
    (goto-char (point-max))
    (run-hooks 'post-command-hook)
    (let ((ov (carriage-test--ov-at-line-containing "CARRIAGE_FINGERPRINT")))
      (should (overlayp ov))
      (should (stringp (overlay-get ov 'display)))
      (should (> (length (string-trim (overlay-get ov 'display))) 0)))))

(provide 'carriage-doc-state-fold-tests)
;;; carriage-doc-state-fold-tests.el ends here
