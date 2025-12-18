;;; tests/carriage-doc-state-save-tests.el --- Save hook persistence (CARRIAGE_STATE) -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
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

(ert-deftest carriage-doc-state/before-save-persists-and-hides-carriage-state ()
  "before-save hook should persist a single CARRIAGE_STATE line and hide it via overlay."
  (with-temp-buffer
    (org-mode)
    ;; Start from an existing state line so the hook doesn't depend on carriage-mode vars.
    (insert "#+title: Demo\n"
            "#+PROPERTY: X 1\n"
            "#+PROPERTY: CARRIAGE_STATE (:CAR_MODE t :CAR_INTENT Ask)\n"
            "\n"
            "* Note\nBody\n")
    (setq-local carriage-doc-state-save-on-save t)
    (carriage-doc-state-install-save-hook)

    (run-hooks 'before-save-hook)

    ;; Still exactly one state line.
    (should (= (carriage--cds--count-state-lines) 1))

    ;; It must remain readable.
    (let ((pl (carriage-doc-state-read (current-buffer))))
      (should (eq (plist-get pl :CAR_MODE) t)))

    ;; It should be folded (summary overlay enabled). We assert the overlay is present
    ;; and is in folded mode (overlay 'display is a non-empty string).
    (should (overlayp carriage-doc-state--overlay))
    (should (stringp (overlay-get carriage-doc-state--overlay 'display)))
    (should (> (length (string-trim (overlay-get carriage-doc-state--overlay 'display))) 0)))))

(provide 'carriage-doc-state-save-tests)
;;; carriage-doc-state-save-tests.el ends here
