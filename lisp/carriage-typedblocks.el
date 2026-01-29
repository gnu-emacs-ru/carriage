;;; carriage-typedblocks.el --- Minimal typed-blocks extractor and payload builder  -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+

;;; Commentary:
;; Minimal "Typed Blocks v1" support:
;; - One-pass extractor of begin_<type> … end_<type> segments.
;; - Allowlist of recognized types (case-insensitive).
;; - Payload builder that includes only whitelisted types in document order,
;;   with optional inclusion of commands and plain-text segments.
;; - Patch bodies are never included.
;; - Optional header plist after begin_<type> (e.g., (#:nollm t)) is recognized; when
;;   :nollm is non-nil, the block is excluded from LLM payload.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-typedblocks nil
  "Typed blocks extractor and payload builder."
  :group 'applications
  :prefix "carriage-typedblocks-")

(defvar carriage-mode-include-plain-text-context)

(defcustom carriage-typedblocks-include-commands t
  "When non-nil, include begin_commands blocks into the LLM payload."
  :type 'boolean
  :group 'carriage-typedblocks)
(make-variable-buffer-local 'carriage-typedblocks-include-commands)

(defcustom carriage-typedblocks-allowed-types
  '(task notes analysis plan verify commands context patch question answer)
  "Recognized typed-block kinds (symbols), case-insensitive in buffers."
  :type '(repeat symbol)
  :group 'carriage-typedblocks)

(defcustom carriage-typedblocks-include-default
  '(task analysis plan verify context question answer)
  "Typed-block kinds to include by default into the payload (order-independent).
Notes are excluded; patch bodies are always excluded."
  :type '(repeat symbol)
  :group 'carriage-typedblocks)

(defcustom carriage-typedblocks-max-bytes nil
  "Optional hard cap (bytes) for the assembled payload (nil means unlimited).
When exceeded, payload is truncated deterministically with an ellipsis."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'carriage-typedblocks)

(defun carriage-typedblocks--read-header-plist (sexp-str)
  "Read SEXP-STR into a plist; return plist or nil on any read error."
  (when (and (stringp sexp-str) (not (string-empty-p (string-trim sexp-str))))
    (condition-case _e
        (let ((obj (car (read-from-string sexp-str))))
          (and (listp obj) obj))
      (error nil))))

(defun carriage-typedblocks--block-skip-p (type header-plist)
  "Return non-nil when block of TYPE with HEADER-PLIST must be excluded."
  (let ((nollm (and (listp header-plist) (plist-get header-plist :nollm))))
    (or nollm
        (eq type 'patch)))) ;; patch bodies are never included

(defun carriage-typedblocks--make-seg (type beg-body end-body)
  "Build segment plist for TYPE covering content BEG-BODY..END-BODY."
  (list :type type
        :beg beg-body
        :end end-body
        :text (buffer-substring-no-properties beg-body end-body)))

(defun carriage-typedblocks--normalize-type (s)
  "Normalize block type name string S to a symbol (downcased)."
  (let ((nm (downcase (or s ""))))
    (intern nm)))

(defun carriage-typedblocks--extract (buffer)
  "Extract typed-block segments and plain segments from BUFFER.
Return list of (:type T :text S :beg B :end E) in document order.
Plain segments use :type 'plain. Patch bodies are represented as segments
with empty text (and will be filtered out later). Blocks with :nollm t in
header are skipped entirely."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let* ((case-fold-search t)
               (allowed carriage-typedblocks-allowed-types)
               (rx-beg "^[ \t]*#\\+begin_\\([a-zA-Z0-9_]+\\)\\b\\(\\s-+\\((.*)\\)\\)?[ \t]*$")
               (rx-end-base "^[ \t]*#\\+end_%s\\b[ \t]*$")
               (acc '())
               (pos (point-min))
               (doc-end (point-max)))
          (goto-char (point-min))
          (while (< (point) doc-end)
            (let ((here (point)))
              (if (re-search-forward rx-beg nil t)
                  (let* ((match-beg (match-beginning 0))
                         (type-str (match-string 1))
                         (type (carriage-typedblocks--normalize-type type-str))
                         (hdr-str (match-string 3))
                         (hdr-pl (carriage-typedblocks--read-header-plist hdr-str))
                         ;; plain before this block
                         (plain-beg pos)
                         (plain-end (max (point-min) (1- match-beg))))
                    (when (> plain-end plain-beg)
                      (push (list :type 'plain
                                  :beg plain-beg
                                  :end plain-end
                                  :text (buffer-substring-no-properties plain-beg plain-end))
                            acc))
                    ;; block content
                    (let* ((rx-end (format rx-end-base (regexp-quote (downcase type-str))))
                           (body-beg (min doc-end (1+ (line-end-position)))))
                      (goto-char body-beg)
                      (if (re-search-forward rx-end nil t)
                          (let* ((end-line-beg (line-beginning-position))
                                 (body-end (max body-beg (1- end-line-beg))))
                            ;; Accept block when type is recognized; otherwise treat as plain.
                            (if (and (memq type allowed)
                                     (not (carriage-typedblocks--block-skip-p type hdr-pl)))
                                (push (carriage-typedblocks--make-seg type body-beg body-end) acc)
                              ;; If unknown type: treat whole region (including markers) as plain; but minimal v1 ignores unknown — do nothing.
                              )
                            (setq pos (1+ (line-end-position))))
                        ;; No end marker → treat up to end-of-buffer as block
                        (let ((body-end doc-end))
                          (when (and (memq type allowed)
                                     (not (carriage-typedblocks--block-skip-p type hdr-pl)))
                            (push (carriage-typedblocks--make-seg type body-beg body-end) acc))
                          (setq pos doc-end)
                          (goto-char doc-end)))))
                ;; no more blocks → trailing plain
                (let ((plain-beg pos)
                      (plain-end doc-end))
                  (when (> plain-end plain-beg)
                    (push (list :type 'plain
                                :beg plain-beg
                                :end plain-end
                                :text (buffer-substring-no-properties plain-beg plain-end))
                          acc))
                  (setq pos doc-end)
                  (goto-char doc-end)))))
          (nreverse acc))))))

(defun carriage-typedblocks--include-type-p (type)
  "Return non-nil when TYPE should be included into payload per defaults/flags."
  (cond
   ((eq type 'plain) carriage-mode-include-plain-text-context)
   ((eq type 'commands) carriage-typedblocks-include-commands)
   ((eq type 'patch) nil)
   ((memq type carriage-typedblocks-include-default) t)
   (t nil)))

(defun carriage-typedblocks-build-payload (buffer)
  "Assemble payload string from BUFFER typed blocks:
- Include only allowed types in document order.
- Prefix each included segment with \"In <type>:\\n\".
- Plain segments get \"In plain:\" when included.
- Patch bodies are never included.
- Blocks with :nollm t header are excluded at extraction stage."
  (let* ((segs (carriage-typedblocks--extract buffer))
         (out '()))
    (dolist (sg segs)
      (let* ((tp (plist-get sg :type))
             (txt (or (plist-get sg :text) "")))
        (when (and (carriage-typedblocks--include-type-p tp)
                   (or (not (eq tp 'patch)) (string-empty-p txt)))
          (let* ((label (format "In %s:" (if (eq tp 'plain) "plain" (symbol-name tp))))
                 (piece (concat label "\n" txt)))
            (push piece out)
            (push "" out))))) ;; blank line between segments
    (let* ((assembled (string-join (nreverse out) "\n")))
      (if (and (numberp carriage-typedblocks-max-bytes)
               (> carriage-typedblocks-max-bytes 0)
               (> (string-bytes assembled) carriage-typedblocks-max-bytes))
          (with-temp-buffer
            (insert assembled)
            (goto-char (point-max))
            (insert "\n…\n")
            (buffer-substring-no-properties (point-min) (point-max)))
        assembled))))

(provide 'carriage-typedblocks)
;;; carriage-typedblocks.el ends here
