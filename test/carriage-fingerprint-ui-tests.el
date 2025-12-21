;;; carriage-fingerprint-ui-tests.el --- UI tests for CARRIAGE_FINGERPRINT overlays -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Ensure fingerprint lines render as icon/badge summary via overlay 'display,
;; reveal original on cursor enter, and fold back on leave. Also verify prompt
;; sanitizer strips both CARRIAGE_STATE and CARRIAGE_FINGERPRINT.

;;; Code:

(require 'ert)
(require 'org)
(require 'subr-x)
(require 'carriage-doc-state)
(require 'carriage-transport)

(defun carriage--fp--line-pos ()
  "Return beginning position of the first CARRIAGE_FINGERPRINT line, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT:" nil t)
        (line-beginning-position)))))

(defun carriage--fp--first-overlay-at (pos)
  "Return first overlay at POS with a string 'display, or nil."
  (let ((ovs (overlays-at pos)))
    (seq-find (lambda (ov)
                (stringp (overlay-get ov 'display)))
              ovs)))

(ert-deftest carriage-fingerprint/fold-reveal-basic ()
  "Fingerprint line folds to summary, reveals on cursor enter, re-folds on leave."
  (with-temp-buffer
    (org-mode)
    (setq-local carriage-fingerprint-badge-overlay-minimal nil)
    (insert "#+title: Demo\n"
            "#+CARRIAGE_FINGERPRINT: (:CAR_INTENT Code :CAR_SUITE udiff :CAR_BACKEND gptel :CAR_PROVIDER openai :CAR_MODEL gpt-4o-mini "
            ":CAR_CTX_DOC t :CAR_CTX_GPTEL nil :CAR_CTX_VISIBLE t :CAR_CTX_PROFILE p1)\n"
            "\n* Note\nBody\n")
    ;; Enable summary UI and refresh overlays.
    (setq-local carriage-doc-state-summary-enable t)
    (carriage-doc-state-summary-enable)
    (carriage-doc-state-summary-refresh)

    (let ((pos (carriage--fp--line-pos)))
      (should (numberp pos))
      ;; Folded state: must have an overlay with a non-empty 'display string.
      (let* ((ov (carriage--fp--first-overlay-at pos))
             (disp (and ov (overlay-get ov 'display))))
        (should (overlayp ov))
        (should (stringp disp))
        (should (> (length (string-trim disp)) 0))
        ;; Default (non-minimal) should include suite + ctx badges (not only model).
        (should (string-match-p "\\budiff\\b" disp))
        (should (string-match-p "\\bDoc\\b\\|\\bGpt\\b\\|\\bVis\\b\\|\\bPat\\b" disp)))

      ;; Minimal mode for fingerprint: collapse to model-only.
      (setq-local carriage-fingerprint-badge-overlay-minimal t)
      (carriage-doc-state-summary-refresh (current-buffer))
      (goto-char (point-max))
      (run-hooks 'post-command-hook)
      (let* ((ovm (carriage--fp--first-overlay-at pos))
             (dispm (and ovm (overlay-get ovm 'display))))
        (should (overlayp ovm))
        (should (stringp dispm))
        (should (string-match-p "gpt-4o-mini" dispm))
        (should-not (string-match-p "\\budiff\\b" dispm))
        (let ((case-fold-search nil))
          (should-not (string-match-p "\\bDoc\\b\\|\\bGpt\\b\\|\\bVis\\b\\|\\bPat\\b" dispm))))

      ;; Move point onto the fingerprint line → reveal (display=nil).
      (goto-char pos)
      (run-hooks 'post-command-hook)
      (let* ((ov2 (carriage--fp--first-overlay-at pos)))
        ;; When revealed, there should be no display overlay at this position.
        (should (null ov2)))

      ;; Move point away → fold back (display restored).
      (goto-char (point-max))
      (run-hooks 'post-command-hook)
      (let* ((ov3 (carriage--fp--first-overlay-at pos))
             (disp3 (and ov3 (overlay-get ov3 'display))))
        (should (overlayp ov3))
        (should (stringp disp3))
        (should (> (length (string-trim disp3)) 0))))))

(ert-deftest carriage-fingerprint/transport-strips-state-and-fingerprint ()
  "Prompt sanitizer must strip both CARRIAGE_STATE and CARRIAGE_FINGERPRINT (and carriage blocks)."
  (let* ((raw (concat
               "#+title: T\n"
               "#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t :CAR_INTENT Ask)\n"
               "#+CARRIAGE_FINGERPRINT: (:CAR_INTENT Code :CAR_MODEL gpt-4o-mini)\n"
               "#+begin_carriage\ninternal\n#+end_carriage\n"
               "* Note\nText\n"))
         (stripped (carriage-transport--strip-internal-lines raw)))
    (should (stringp stripped))
    (should-not (string-match-p "CARRIAGE_STATE" stripped))
    (should-not (string-match-p "CARRIAGE_FINGERPRINT" stripped))
    (should-not (string-match-p "#\\+begin_carriage" stripped))
    (should-not (string-match-p "#\\+end_carriage" stripped))))

(provide 'carriage-fingerprint-ui-tests)
;;; carriage-fingerprint-ui-tests.el ends here
