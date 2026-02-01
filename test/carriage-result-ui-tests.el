;;; carriage-result-ui-tests.el --- Tests for CARRIAGE_RESULT aggregation  -*- lexical-binding: t; -*-

(require 'ert)

(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (repo-root (expand-file-name ".." this-dir))
       (lisp-dir (expand-file-name "lisp" repo-root))
       (transports-dir (expand-file-name "lisp/transports" repo-root)))
  (add-to-list 'load-path lisp-dir)
  (when (file-directory-p transports-dir)
    (add-to-list 'load-path transports-dir)))

(require 'carriage-ui nil t)

(ert-deftest carriage-doc-cost-from-result-lines-known-and-unknown ()
  "Doc-cost sums :CAR_COST_TOTAL_U from CARRIAGE_RESULT and counts known/unknown."
  (with-temp-buffer
    (org-mode)
    (insert (format "#+CARRIAGE_RESULT: %S\n"
                    (list :CAR_REQ_ID "r-1"
                          :CAR_STATUS 'done
                          :CAR_BACKEND 'gptel
                          :CAR_MODEL "gpt-5"
                          :CAR_TOKENS_OUT 2000
                          :CAR_COST_TOTAL_U 1000000
                          :CAR_COST_KNOWN t)))
    (insert (format "#+CARRIAGE_RESULT: %S\n"
                    (list :CAR_REQ_ID "r-2"
                          :CAR_STATUS 'done
                          :CAR_BACKEND 'gptel
                          :CAR_MODEL "gpt-5"
                          :CAR_TOKENS_OUT 3000
                          :CAR_COST_TOTAL_U nil
                          :CAR_COST_KNOWN nil)))
    (carriage-ui-doc-cost-refresh-now (current-buffer))
    (let* ((snap (carriage-ui-doc-cost-get)))
      (should (equal (plist-get snap :known-total-u) 1000000))
      (should (equal (plist-get snap :known-count) 1))
      (should (equal (plist-get snap :unknown-count) 1)))))

(ert-deftest carriage-doc-cost-fallback-to-fingerprint-when-no-result ()
  "When no CARRIAGE_RESULT present, fallback to fingerprint totals."
  (with-temp-buffer
    (org-mode)
    (insert "#+CARRIAGE_FINGERPRINT: (:CAR_COST_TOTAL_U 2000000)\n")
    (insert "#+CARRIAGE_FINGERPRINT: (:CAR_COST_TOTAL_U nil)\n")
    (carriage-ui-doc-cost-refresh-now (current-buffer))
    (let* ((snap (carriage-ui-doc-cost-get)))
      (should (equal (plist-get snap :known-total-u) 2000000))
      (should (equal (plist-get snap :known-count) 1))
      (should (equal (plist-get snap :unknown-count) 1)))))

(provide 'carriage-result-ui-tests)
;;; carriage-result-ui-tests.el ends here
