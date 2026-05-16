;;; carriage-ui-context-badge.el --- Context badge helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Context badge computation and cache management.
;; Independent module - no dependencies on other carriage-ui-* vars.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-context-count "carriage-context" (&optional buffer point))


(defcustom carriage-ui-context-cache-ttl nil
  "Maximum age in seconds for cached context badge computations in the mode-line."
  :type '(choice (const :tag "Disable caching" 0)
                 (number :tag "TTL (seconds)")
                 (const :tag "Unlimited" nil))
  :group 'carriage-ui)

(defcustom carriage-ui-context-async-refresh t
  "When non-nil, compute the [Ctx:N] badge off the redisplay path on an idle timer."
  :type 'boolean
  :group 'carriage-ui)

(defcustom carriage-ui-context-refresh-delay 0.05
  "Idle delay (seconds) before recomputing context badge when cache is stale."
  :type 'number
  :group 'carriage-ui)

(defcustom carriage-ui-context-min-refresh-interval 1.0
  "Minimum interval in seconds between actual context badge recomputations per buffer."
  :type 'number
  :group 'carriage-ui)

(defcustom carriage-ui-context-tooltip-max-items 50
  "Maximum number of context items to list in the [Ctx:N] tooltip."
  :type 'integer
  :group 'carriage-ui)

(defvar-local carriage-ui--ctx-cache nil
  "Buffer-local cache for context badge computation.
Plist keys: :doc :gpt :tick :time :value.")

(defvar-local carriage-ui--ctx-refresh-timer nil
  "Timer for asynchronous context badge refresh (buffer-local).")

(defvar-local carriage-ui--ctx-pending-toggles nil
  "Last requested toggles snapshot for a pending async context badge refresh.")

(defvar-local carriage-ui--ctx-pending-tick nil
  "Last requested buffer tick for a pending async context badge refresh.")

(defvar-local carriage-ui--ctx-badge-version 0
  "Monotonic version of context badge state; bumps on real changes to avoid work on redisplay.")

(defvar-local carriage-ui--ctx-last-placeholder-time 0
  "Timestamp (float seconds) when a [Ctx:?] placeholder was last returned for this buffer.")

(defvar-local carriage-ui--ctx-placeholder-count 0
  "How many consecutive times [Ctx:?] was returned without a successful refresh.")

(defvar-local carriage-ui--ctx-badge-refresh-timer nil)
(defvar-local carriage-ui--ctx-badge-pending nil)
(defvar-local carriage-ui--ctx-badge-last-time 0.0)

(defun carriage-ui--context-item->line (item)
  "Format ITEM from carriage-context-count into a single tooltip line."
  (let* ((p   (plist-get item :path))
         (src   (pcase (plist-get item :source)
                  ('doc "doc") ('gptel "gptel") ('both "both") ('visible "visible") (_ "?")))
         (inc (plist-get item :included))
         (rsn (plist-get item :reason)))
    (concat " - [" src "] " (or p "-")
            (unless inc
              (format " (content omitted%s)" (if rsn (format ": %s" rsn) ""))))))

(defun carriage-ui--context-toggle-states ()
  "Return plist of current context toggle states and scope.
Keys: :doc :gpt :vis :patched :scope

Note: Avoids requiring carriage-context in the redisplay path; relies on boundp with defaults."
  (list
   :doc (if (boundp 'carriage-mode-include-doc-context)
            carriage-mode-include-doc-context
          t)
   :gpt (if (boundp 'carriage-mode-include-gptel-context)
            carriage-mode-include-gptel-context
          nil)
   :vis (and (boundp 'carriage-mode-include-visible-context)
            carriage-mode-include-visible-context)
   :patched (and (boundp 'carriage-mode-include-patched-files)
               carriage-mode-include-patched-files)
   :gptver (if (boundp 'carriage-ui--gptel-context-version)
               carriage-ui--gptel-context-version
             0)
    :scope (and (boundp 'carriage-doc-context-scope)
                carriage-doc-context-scope)))

(defun carriage-ui--ctx-cache-valid-p (cache toggles tick time _ignored)
  "Return non-nil when CACHE is fresh for TOGGLES/TICK at TIME.

This is intentionally conservative: anything missing or malformed is treated as stale."
  (and (listp cache)
       (equal (plist-get cache :toggles) toggles)
       (equal (plist-get cache :tick) tick)
       (equal (plist-get cache :time) time)
       (let ((val (plist-get cache :value)))
         (or (consp val) (stringp val) (null val)))))

(defun carriage-ui--ctx-build-cache (toggles tick time value)
  "Build context cache plist from TOGGLES, TICK, TIME and VALUE."
  (list :toggles toggles :tick tick :time time :value value))

(defun carriage-ui--ctx-invalidate ()
  "Invalidate all context badge caches for the current buffer."
  (setq carriage-ui--ctx-cache nil
        carriage-ui--ctx-pending-toggles nil
        carriage-ui--ctx-pending-tick nil
        carriage-ui--ctx-placeholder-count 0
        carriage-ui--ctx-last-placeholder-time 0
        carriage-ui--ctx-refresh-timer nil
        carriage-ui--ctx-badge-refresh-timer nil
        carriage-ui--ctx-badge-pending nil)
  (force-mode-line-update t))

(defun carriage-ui--reset-context-cache ()
  "Compatibility alias for cache invalidation."
  (carriage-ui--ctx-invalidate))

(defun carriage-ui--compute-context-badge (inc-doc inc-gpt inc-vis inc-patched)
  "Return a cons (LABEL . TOOLTIP) for the context badge.

The badge is based on `carriage-context-count' when available and falls back
to a compact label if context counting fails."
  (let* ((res (condition-case _e
                  (if (fboundp 'carriage-context-count)
                      (carriage-context-count (current-buffer) nil)
                    nil)
                (error nil)))
         (count (or (plist-get res :count) 0))
         (items (or (plist-get res :items) '()))
         (profile (or (and (boundp 'carriage-doc-context-profile)
                           carriage-doc-context-profile)
                      'p1))
         (tooltip (concat
                   (format "Context: %d item(s)\n" count)
                   (format "Профиль: %s\n" (upcase (symbol-name profile)))
                   (format "Doc: %s  GPTel: %s  Visible: %s  Patched: %s"
                           (if inc-doc "on" "off")
                           (if inc-gpt "on" "off")
                           (if inc-vis "on" "off")
                           (if inc-patched "on" "off"))
                   (when items
                     (concat "\n"
                             (mapconcat #'carriage-ui--context-item->line items "\n"))))))
    (cons (format "[Ctx:%d]" count) tooltip)))

(defun carriage-ui--context-badge ()
  "Return a (LABEL . TOOLTIP) for the context badge.

This is a lightweight, robust wrapper used from the modeline. It derives
current toggle states and delegates to `carriage-ui--compute-context-badge'.
Any errors are swallowed to avoid signalling from redisplay/modeline.
"
  (condition-case _err
      (let* ((toggles (carriage-ui--context-toggle-states))
             (inc-doc (plist-get toggles :doc))
             (inc-gpt (plist-get toggles :gpt))
             (inc-vis (plist-get toggles :vis))
             (inc-patched (plist-get toggles :patched)))
        (carriage-ui--compute-context-badge inc-doc inc-gpt inc-vis inc-patched))
    (error nil)))

(defun carriage-ui-refresh-context-badge (&optional _event)
  "Refresh the context badge now."
  (interactive "e")
  (when (fboundp 'carriage-ui--ctx-force-sync)
    (let ((toggles (carriage-ui--context-toggle-states)))
      (carriage-ui--ctx-force-sync toggles (buffer-chars-modified-tick) (float-time)))))

(provide 'carriage-ui-context-badge)
;;; carriage-ui-context-badge.el ends here
