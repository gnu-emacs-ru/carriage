;;; carriage-pricing-test.el --- Tests for pricing, fingerprint upsert, and doc-cost  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Ensure local load-path for repository layout.
(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (repo-root (expand-file-name ".." this-dir))
       (lisp-dir (expand-file-name "lisp" repo-root))
       (transports-dir (expand-file-name "lisp/transports" repo-root)))
  (add-to-list 'load-path lisp-dir)
  (when (file-directory-p transports-dir)
    (add-to-list 'load-path transports-dir)))

(require 'carriage-pricing)
(require 'carriage-ui nil t)
(require 'carriage-mode nil t)

(defun carriage-test--extract-fingerprint-plist ()
  "Extract plist from the first #+CARRIAGE_FINGERPRINT line in current buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT:[ \t]*\\(.*\\)$" nil t)
      (ert-fail "No #+CARRIAGE_FINGERPRINT line found"))
    (let* ((s (match-string 1))
           (obj (car (read-from-string s))))
      (unless (listp obj)
        (ert-fail "Fingerprint payload is not a plist: %S" obj))
      obj)))

(defun carriage-test--plist-get-any (pl keys)
  "Return first non-nil value in PL for KEYS."
  (cl-loop for k in keys
           for v = (plist-get pl k)
           when (not (null v)) return v))

;; -----------------------------------------------------------------------------
;; Pricing: CSV parsing, cost computation, formatting

(ert-deftest carriage-pricing-parse-csv-strict-header ()
  (let* ((bad "model,input,output\nx,1,2\n")
         (ht (carriage-pricing-parse-csv-string bad)))
    (should (hash-table-p ht))
    (should (= (hash-table-count ht) 0))))

(ert-deftest carriage-pricing-compute-integer-safe-u ()
  (let* ((csv (concat
               "model,input_rub_per_1m,output_rub_per_1m,input_audio_rub_per_1m,output_audio_rub_per_1m\n"
               "gptel:ai-tunnel:gpt-5,1,2,,\n"))
         (tab (carriage-pricing-parse-csv-string csv))
         (usage (list :tokens-in 500000 :tokens-out 250000))
         (res (carriage-pricing-compute "gptel:ai-tunnel:gpt-5" usage tab)))
    (should (eq (plist-get res :known) t))
    ;; 1 RUB / 1M tokens => 1 µ₽ per token. 500k tokens => 500k µ₽.
    (should (= (plist-get res :cost-in-u) 500000))
    ;; 2 RUB / 1M tokens => 2 µ₽ per token. 250k tokens => 500k µ₽.
    (should (= (plist-get res :cost-out-u) 500000))
    (should (= (plist-get res :cost-total-u) 1000000))))

(ert-deftest carriage-pricing-format-money-rounding ()
  ;; 12.345678 RUB => should display 12.35 with half-up at kopeck boundary.
  (should (string= (carriage-pricing-format-money 12345678 "₽") "₽12.35"))
  (should (string= (carriage-pricing-format-money 12345000 "₽") "₽12.35"))
  (should (string= (carriage-pricing-format-money nil "₽") "—")))

(ert-deftest carriage-pricing-lookup-fallback-provider-model ()
  (let* ((csv (concat
               "model,input_rub_per_1m,output_rub_per_1m,input_audio_rub_per_1m,output_audio_rub_per_1m\n"
               "ai-tunnel:gpt-5,10,20,,\n"))
         (tab (carriage-pricing-parse-csv-string csv))
         (rate (carriage-pricing-lookup "gptel:ai-tunnel:gpt-5" tab)))
    (should (listp rate))
    (should (= (plist-get rate :in-u-per-1m) (* 10 1000000)))
    (should (= (plist-get rate :out-u-per-1m) (* 20 1000000)))))

;; -----------------------------------------------------------------------------
;; Fingerprint upsert: usage + cost

(ert-deftest carriage-fingerprint-upsert-usage-and-cost ()
  (unless (fboundp 'carriage-fingerprint-note-usage-and-cost)
    (ert-fail "carriage-fingerprint-note-usage-and-cost is not available"))
  (with-temp-buffer
    (org-mode)
    ;; Minimal fingerprint line; keep it realistic but do not depend on send pipeline.
    (insert "#+CARRIAGE_FINGERPRINT: (:CAR_BACKEND gptel :CAR_PROVIDER \"ai-tunnel\" :CAR_MODEL \"gpt-5\")\n")
    ;; Create a marker pointing at the fingerprint line beginning, if the implementation uses it.
    (when (boundp 'carriage--fingerprint-line-marker)
      (setq-local carriage--fingerprint-line-marker (copy-marker (point-min))))
    ;; Provide usage; pricing table is built-in and includes gpt-5.
    (let ((usage (list :tokens-in 1000 :tokens-out 2000)))
      (carriage-fingerprint-note-usage-and-cost usage 'gptel "ai-tunnel" "gpt-5"))
    (let* ((fp (carriage-test--extract-fingerprint-plist))
           (tokens-in (plist-get fp :CAR_TOKENS_IN))
           (tokens-out (plist-get fp :CAR_TOKENS_OUT))
           (known (plist-get fp :CAR_COST_KNOWN))
           (cost-total (plist-get fp :CAR_COST_TOTAL_U)))
      (should (equal tokens-in 1000))
      (should (equal tokens-out 2000))
      ;; Pricing for gpt-5 is present in built-in model-prices.csv => cost must be known.
      (should (eq known t))
      ;; 1000 * 23.75 µ₽ + 2000 * 1900 µ₽ = 23,750 + 3,800,000 = 3,823,750 µ₽
      (should (integerp cost-total))
      (should (= cost-total 3823750)))))

(ert-deftest carriage-fingerprint-upsert-unknown-model-cost-is-nil ()
  (unless (fboundp 'carriage-fingerprint-note-usage-and-cost)
    (ert-fail "carriage-fingerprint-note-usage-and-cost is not available"))
  (with-temp-buffer
    (org-mode)
    (insert "#+CARRIAGE_FINGERPRINT: (:CAR_BACKEND gptel :CAR_PROVIDER \"ai-tunnel\" :CAR_MODEL \"no-such-model\")\n")
    (when (boundp 'carriage--fingerprint-line-marker)
      (setq-local carriage--fingerprint-line-marker (copy-marker (point-min))))
    (let ((usage (list :tokens-in 1000 :tokens-out 2000)))
      (carriage-fingerprint-note-usage-and-cost usage 'gptel "ai-tunnel" "no-such-model"))
    (let* ((fp (carriage-test--extract-fingerprint-plist))
           (known (plist-get fp :CAR_COST_KNOWN))
           (has-total (plist-member fp :CAR_COST_TOTAL_U))
           (cost-total (plist-get fp :CAR_COST_TOTAL_U)))
      (should (eq known nil))
      ;; Even when unknown, key should be present and value should be nil (unknown cost).
      (should has-total)
      (should (null cost-total)))))

;; -----------------------------------------------------------------------------
;; Doc-cost cache: refresh-now + cached getter

(ert-deftest carriage-ui-doc-cost-refresh-and-get ()
  (unless (and (fboundp 'carriage-ui-doc-cost-refresh-now)
               (fboundp 'carriage-ui-doc-cost-get))
    (ert-fail "UI doc-cost API missing: expected carriage-ui-doc-cost-refresh-now and carriage-ui-doc-cost-get"))
  (with-temp-buffer
    (org-mode)
    ;; Two fingerprints: one known cost, one unknown (nil).
    (insert "#+CARRIAGE_FINGERPRINT: (:CAR_COST_TOTAL_U 1000000)\n")
    (insert "#+CARRIAGE_FINGERPRINT: (:CAR_COST_TOTAL_U 2000000)\n")
    (insert "#+CARRIAGE_FINGERPRINT: (:CAR_COST_TOTAL_U nil)\n")
    (carriage-ui-doc-cost-refresh-now (current-buffer))
    (let* ((snap (carriage-ui-doc-cost-get))
           (total (plist-get snap :known-total-u))
           (known-count (plist-get snap :known-count))
           (unknown-count (plist-get snap :unknown-count)))
      (should (integerp total))
      (should (= total 3000000))
      (should (= known-count 2))
      (should (= unknown-count 1)))))

(provide 'carriage-pricing-test)
;;; carriage-pricing-test.el ends here
