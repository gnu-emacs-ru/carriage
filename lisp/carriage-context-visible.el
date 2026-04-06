;;; carriage-context-visible.el --- Visible buffer collection for context  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Visible buffer collection for context.
;; Extracted from carriage-context.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom carriage-visible-ignore-modes
  '(exwm-mode image-mode pdf-view-mode doc-view-mode dired-mode help-mode special-mode
              context-navigator-view-mode test-flow-panel-mode test-flow-status-mode)
  "List of major-modes to ignore when collecting visible buffers."
  :type '(repeat symbol)
  :group 'carriage-context)

(defcustom carriage-visible-ignore-buffer-regexps
  '("^\\*carriage-" "^\\*Warnings\\*\\'" "^\\*Compile-Log\\*\\'" "^\\*Help\\*\\'" "^\\*Backtrace\\*\\'")
  "Regexps for buffer names to ignore when collecting visible buffers."
  :type '(repeat string)
  :group 'carriage-context)

(defcustom carriage-visible-terminal-tail-lines 256
  "Number of last lines to include for terminal/comint/messages-like buffers."
  :type 'integer
  :group 'carriage-context)

(defcustom carriage-visible-exclude-current-buffer t
  "When non-nil, exclude the buffer that initiated context collection from the 'visible source."
  :type 'boolean
  :group 'carriage-context)

(defun carriage-context--collect-async-visible-items (buf)
  "Return visible work items for BUF.
Items are either (:kind path :value STRING) or (:kind visible-buffer :buffer BUF)."
  (let ((items '()))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((seen (make-hash-table :test 'eq))
              (ignored-modes (and (boundp 'carriage-visible-ignore-modes) carriage-visible-ignore-modes))
              (ignored-names (and (boundp 'carriage-visible-ignore-buffer-regexps) carriage-visible-ignore-buffer-regexps)))
          (walk-windows
           (lambda (w)
             (let ((b (window-buffer w)))
               (unless (gethash b seen)
                 (puthash b t seen)
                 (with-current-buffer b
                   (let* ((nm (buffer-name b))
                          (mm major-mode)
                          (skip
                           (or (and (boundp 'carriage-visible-exclude-current-buffer)
                                    carriage-visible-exclude-current-buffer
                                    (eq b buf))
                               (minibufferp b)
                               (eq mm 'exwm-mode)
                               (and (listp ignored-modes) (memq mm ignored-modes))
                               (and (listp ignored-names)
                                    (seq-some (lambda (rx) (and (stringp rx) (string-match-p rx nm)))
                                              ignored-names)))))
                     (unless skip
                       (let ((bf (buffer-file-name b)))
                         (cond
                          ((and (stringp bf) (not (file-remote-p bf)))
                           (push (list :kind 'path :value bf) items))
                          (t
                           (push (list :kind 'visible-buffer :buffer b) items))))))))))
           nil (selected-frame)))))
    (nreverse items)))

(defun carriage-context--collect-async-process-visible-buffer (vb state)
  "Include visible non-file buffer VB into STATE and return updated STATE."
  (if (not (buffer-live-p vb))
      state
    (with-current-buffer vb
      (let* ((nm (buffer-name vb))
             (mm major-mode)
             (rel (format "visible:/%s" nm))
             (tail (or (and (boundp 'carriage-visible-terminal-tail-lines)
                            carriage-visible-terminal-tail-lines)
                       256))
             (is-term (or (derived-mode-p 'comint-mode)
                          (derived-mode-p 'eshell-mode)
                          (derived-mode-p 'term-mode)
                          (ignore-errors (derived-mode-p 'vterm-mode))
                          (derived-mode-p 'compilation-mode)
                          (eq mm 'messages-buffer-mode)
                          (string= nm "*Messages*")))
             (text
              (save-excursion
                (save-restriction
                  (widen)
                  (if (not is-term)
                      (buffer-substring-no-properties (point-min) (point-max))
                    (goto-char (point-max))
                    (forward-line (- tail))
                    (buffer-substring-no-properties (point) (point-max))))))
             (sz (string-bytes (or text "")))
             (max-bytes (plist-get state :max-bytes))
             (total (plist-get state :total-bytes)))
        (if (and (numberp max-bytes) (> (+ total sz) max-bytes))
            (progn
              (setq state (carriage-context--push-warning
                           (format "limit reached, include path only: %s" rel) state))
              (setq state (carriage-context--push-file
                           (list :rel rel :true nil :content nil :reason 'size-limit)
                           state))
              (setq state (plist-put state :skipped (1+ (plist-get state :skipped)))))
              (plist-put state :total-bytes (+ total sz)))
          (setq state (carriage-context--push-file
                       (list :rel rel :true nil :content text)
                       state))
          (setq state (plist-put state :total-bytes (+ total sz)))
          (plist-put state :included (1+ (plist-get state :included))))))))

(provide 'carriage-context-visible)
;;; carriage-context-visible.el ends here
