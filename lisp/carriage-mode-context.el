;;; carriage-mode-context.el --- Context helpers extracted from carriage-mode  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;; Small focused helpers to build and format external context for prompts.
;; Extracted to keep carriage-mode.el smaller and enable independent tests.
;;;
;;; Code:

(require 'subr-x)
(require 'carriage-context)

(defun carriage--context-sources-enabled-p ()
  "Return non-nil when any context source is enabled in current buffer." 
  (let* ((inc-doc (if (boundp 'carriage-mode-include-doc-context)
                      carriage-mode-include-doc-context
                    t))
         (inc-gpt (and (boundp 'carriage-mode-include-gptel-context)
                       carriage-mode-include-gptel-context))
         (inc-vis (and (boundp 'carriage-mode-include-visible-context)
                       carriage-mode-include-visible-context))
         (inc-pat (and (boundp 'carriage-mode-include-patched-files)
                       carriage-mode-include-patched-files))
         (inc-map (and (boundp 'carriage-mode-include-project-map)
                       carriage-mode-include-project-map)))
    (or inc-doc inc-gpt inc-vis inc-pat inc-map)))

(defun carriage--context--infer-project-root-from-context (buffer)
  "Best-effort infer git project root from BUFFER document context.
Returns directory path or nil. Never signals." 
  (condition-case _e
      (with-current-buffer buffer
        (let ((cands '()))
          (when (fboundp 'carriage-context--doc-paths)
            (setq cands (append cands (ignore-errors (carriage-context--doc-paths buffer)))))
          (when (and (boundp 'carriage-mode-include-patched-files)
                     carriage-mode-include-patched-files
                     (fboundp 'carriage-context--patched-files))
            (setq cands (append cands (ignore-errors (carriage-context--patched-files buffer)))))
          (when (and cands (stringp (car cands)))
            (let ((abs (expand-file-name (car cands))))
              (when (and abs (file-directory-p (file-name-directory abs)))
                (file-name-directory abs))))))
    (error nil)))

(defun carriage--context-collect-and-format (buffer target)
  "Return formatted external context text for BUFFER injected at TARGET, or nil.
Best-effort: never signal." 
  (condition-case _e
      (when (and (require 'carriage-context nil t)
                 (fboundp 'carriage-context-collect)
                 (fboundp 'carriage-context-format))
        (let* ((root (or (carriage-project-root) default-directory))
               (col (carriage-context-collect buffer root))
               (ctx-text (when col (carriage-context-format col :where target))))
          (if (and (stringp ctx-text) (> (length ctx-text) 0)) ctx-text "")))
    (error "")))

(defun carriage--context-target ()
  "Return context target symbol for current buffer.
Depends on `carriage-mode-context-injection' setting." 
  (if (and (boundp 'carriage-mode-context-injection)
           (memq carriage-mode-context-injection '(system prompt)))
      carriage-mode-context-injection
    'system))

(defun carriage--context-meta-from-text (ctx-text)
  "Extract meta info from formatted CTX-TEXT. Return plist (:omitted N :limited BOOL).
Best-effort; never signals." 
  (condition-case _e
      (let* ((s (if (stringp ctx-text) ctx-text ""))
             (omitted (or (and (> (length s) 0) (when (string-match "omitted=\([0-9]+\)" s) (string-to-number (match-string 1 s)))) 0)))
        (list :omitted (max 0 (or omitted 0))
              :limited (and (> (or omitted 0) 0)
                            (or (string-match-p "limit reached" s)
                                (string-match-p "CTXPLAN_W_LIMIT" s)
                                (string-match-p "truncated by max-files" s)))))
    (error (list :omitted 0 :limited nil))))

(defun carriage--context-profile-symbol (&optional buffer)
  "Return profile symbol for context in BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (if (and (boundp 'carriage-mode-context-profile)
             (memq carriage-mode-context-profile '(P1-core P3-debug)))
        carriage-mode-context-profile
      'P1-core)))

(defun carriage--doc-context-scope-symbol (&optional buffer)
  "Return scope symbol for doc-context in BUFFER." 
  (with-current-buffer (or buffer (current-buffer))
    (if (and (boundp 'carriage-doc-context-scope)
             (memq carriage-doc-context-scope '(all last)))
        carriage-doc-context-scope
      'all)))

(provide 'carriage-mode-context)
;;; carriage-mode-context.el ends here
