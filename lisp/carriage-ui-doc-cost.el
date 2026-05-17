;;; carriage-ui-doc-cost.el --- Document cost helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Code:

(require 'subr-x)

(declare-function carriage-ui--invalidate-ml-cache "carriage-ui" ())

(defvar-local carriage-ui--doc-cost-cache nil)
(defvar-local carriage-ui--doc-cost-version 0)
(defvar-local carriage-ui--doc-cost-refresh-timer nil)

(defun carriage-ui-doc-cost-get ()
  (or carriage-ui--doc-cost-cache
      (list :known-total-u 0 :known-count 0 :unknown-count 0 :ts nil)))

(defun carriage-ui--money-symbol ()
  (if (and (boundp 'carriage-pricing-currency-symbol)
           (stringp carriage-pricing-currency-symbol))
      carriage-pricing-currency-symbol
    "₽"))

(defun carriage-ui--format-money-suffix (amount-u &optional currency-symbol)
  (if (not (integerp amount-u))
      "—"
    (let* ((sym (or currency-symbol (carriage-ui--money-symbol)))
           (kopecks (/ (+ amount-u 5000) 10000))
           (rub (/ kopecks 100))
           (kop (% kopecks 100)))
      (format "%d.%02d%s" rub kop sym))))

(defun carriage-ui-doc-cost-refresh (&optional _event)
  (interactive "e")
  (ignore-errors (carriage-ui-doc-cost-refresh-now (current-buffer))))

(defun carriage-ui-doc-cost-refresh-now (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((sum 0) (known 0) (unknown 0) (case-fold-search t) (results-found 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+CARRIAGE_RESULT:[ \t]*\\(.*\\)$" nil t)
          (let* ((s (match-string 1))
                 (obj (condition-case _e (car (read-from-string s)) (error nil)))
                 (v (and (listp obj) (plist-get obj :CAR_COST_TOTAL_U))))
            (setq results-found (1+ results-found))
            (cond
             ((integerp v) (setq sum (+ sum v)) (setq known (1+ known)))
             ((and (listp obj) (plist-member obj :CAR_COST_TOTAL_U)) (setq unknown (1+ unknown)))))))
      (when (or (<= results-found 0) (<= known 0))
        (setq sum 0 known 0 unknown 0)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*#\\+CARRIAGE_FINGERPRINT:[ \t]*\\(.*\\)$" nil t)
            (let* ((s (match-string 1))
                   (obj (condition-case _e (car (read-from-string s)) (error nil)))
                   (v (and (listp obj) (plist-get obj :CAR_COST_TOTAL_U))))
              (cond
               ((integerp v) (setq sum (+ sum v)) (setq known (1+ known)))
               ((and (listp obj) (plist-member obj :CAR_COST_TOTAL_U)) (setq unknown (1+ unknown))))))))
      (setq carriage-ui--doc-cost-cache
            (list :known-total-u sum :known-count known :unknown-count unknown :ts (float-time)))
      (setq carriage-ui--doc-cost-version (1+ (or carriage-ui--doc-cost-version 0)))
      (carriage-ui--invalidate-ml-cache)
      (force-mode-line-update t)
      t)))

(defun carriage-ui-doc-cost-schedule-refresh (&optional delay)
  (when (timerp carriage-ui--doc-cost-refresh-timer)
    (ignore-errors (cancel-timer carriage-ui--doc-cost-refresh-timer)))
  (let* ((buf (current-buffer))
         (d (max 0.01 (float (or delay 0.08)))))
    (setq carriage-ui--doc-cost-refresh-timer
          (run-at-time d nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (setq carriage-ui--doc-cost-refresh-timer nil)
                             (ignore-errors (carriage-ui-doc-cost-refresh-now buf))))))))
  t)

(provide 'carriage-ui-doc-cost)
;;; carriage-ui-doc-cost.el ends here
