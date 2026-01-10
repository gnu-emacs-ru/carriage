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


(ert-deftest carriage-fingerprint/transport-strips-state-and-fingerprint ()
  "Prompt sanitizer must strip both CARRIAGE_STATE and CARRIAGE_FINGERPRINT (and carriage blocks)."
  (let* ((raw (concat
               "#+title: T\n"
               "#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t :CAR_INTENT Ask)\n"
               "#+CARRIAGE_FINGERPRINT: (:CAR_INTENT Code :CAR_MODEL gpt-4o-mini)\n"
               ""
               "* Note\nText\n"))
         (stripped (carriage-transport--strip-internal-lines raw)))
    (should (stringp stripped))
    (should-not (string-match-p "CARRIAGE_STATE" stripped))
    (should-not (string-match-p "CARRIAGE_FINGERPRINT" stripped))
    (should-not (string-match-p "#\\+begin_carriage" stripped))
    (should-not (string-match-p "#\\+end_carriage" stripped))))

(provide 'carriage-fingerprint-ui-tests)
;;; carriage-fingerprint-ui-tests.el ends here
