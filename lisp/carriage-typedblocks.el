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

(defun carriage-typedblocks-prompt-fragment-v1 ()
  "Return canonical prompt fragment for Org Typed Blocks v1.

The fragment enforces:
- Allowed types: task, analysis, plan, verify, commands, context, question, answer, notes.
- Exact syntax: lines starting at column 0 with \"#+begin_<type>\" ... \"#+end_<type>\".
- Forbidden variations: \"*begin_<type>\"/\"*end_<type>\" headlines; \"#+ begin_<type>\" with a space; any \"#+begin <type>\".
- Guidance to keep plain prose minimal and avoid duplication."
  (mapconcat
   #'identity
   '("You MUST structure key parts using Org Typed Blocks v1."
     "- Allowed types: task, analysis, plan, verify, commands, context, question, answer, notes."
     "- Syntax: lines starting at column 0: \"#+begin_<type>\" … \"#+end_<type>\"."
     "- Forbidden: asterisk headlines like \"*begin_<type>\"/\"*end_<type>\"."
     "- Forbidden: spaces inside the marker, e.g., use \"#+begin_task\", NOT \"#+ begin_task\"."
     "- Keep plain prose minimal; avoid duplicating the same content inside and outside blocks.")
   "\n"))

;; -----------------------------------------------------------------------------
;; Org-structure compliance guidance (\"Соблюдать структуру\")
;;
;; This is a prompt-only hint. It is enabled via:
;;   `carriage-mode-typedblocks-structure-hint' (buffer-local, default ON).
;;
;; Policy (strict, machine-checkable):
;; - Answer MUST be formatted as Org-mode.
;; - First non-empty line MUST be an Org heading.
;; - If there is a heading above point, start at one level deeper (L+1).
;; - If there is no heading above point, start with a top-level heading \"* \".
;; - Heading line format is строго: \"<STARS><SPACE><TITLE>\"
;;   where TITLE is 4–12 words, no trailing punctuation.
;;
;; We inject this hint by post-processing typedblocks prompt fragments.
(require 'cl-lib)
(require 'subr-x)

(defun carriage-typedblocks--org-structure--target-stars ()
  "Return a string like \"***\" for the required response heading level at point."
  (let ((lvl nil))
    (when (derived-mode-p 'org-mode)
      (ignore-errors
        (require 'org nil t)
        ;; Find nearest heading above; if none, lvl stays nil.
        (save-excursion
          (save-restriction
            (widen)
            (let ((b (ignore-errors (org-back-to-heading t))))
              (when b
                (setq lvl (ignore-errors (org-outline-level)))))))))
    ;; If we found a heading level L, we want L (same level as the nearest heading).
    ;; Otherwise start at top level (1).
    (let ((n (max 1 (if (numberp lvl) lvl 1))))
      (make-string n ?*))))

(defun carriage-typedblocks--org-structure--hint ()
  "Return Org-structure compliance hint text for the model, or nil."
  (when (and (boundp 'carriage-mode-org-structure-hint)
             (bound-and-true-p carriage-mode-org-structure-hint))
    (let* ((stars (carriage-typedblocks--org-structure--target-stars))
           (prefix (concat stars " ")))
      (string-join
       (list
        "ORG FORMAT (STRICT):"
        "- Output must be valid Org-mode (no Markdown, no HTML)."
        "- Do NOT use Markdown fences like ```."
        "- Use Org src blocks for code/commands:"
        "  #+begin_src <lang>"
        "  ..."
        "  #+end_src"
        ""
        "STRUCTURE RULE (STRICT):"
        "- The FIRST non-empty line of your answer MUST be an Org heading."
        (format "- Start that heading with EXACTLY this prefix: %S" prefix)
        "- Heading line format must be: \"<STARS><SPACE><TITLE>\"."
        "- TITLE requirements:"
        "  - 4–12 words (or <= 80 chars),"
        "  - no trailing dot/colon/semicolon,"
        "  - short summary of this iteration's result."
        "- After the heading, add a blank line, then the body."
        ""
        "If headings of the same level exist just above, follow their style (tags/TODO/date).")
       "\n"))))

(defun carriage-typedblocks--append-org-structure-hint (s)
  "Append Org-structure hint to string S (best-effort)."
  (let ((hint (carriage-typedblocks--org-structure--hint)))
    (cond
     ((not (stringp s))
      (if (stringp hint) hint s))
     ((not (stringp hint))
      s)
     (t
      (concat s "\n\n" hint)))))

;; Inject into whichever typedblocks prompt fragment function exists.
(dolist (fn '(carriage-typedblocks--prompt-fragment
              carriage-typedblocks-prompt-fragment
              carriage-typedblocks--system-fragment
              carriage-typedblocks--structure-hint
              carriage-typedblocks--structure-hint-fragment))
  (when (fboundp fn)
    (ignore-errors
      (advice-add fn :filter-return #'carriage-typedblocks--append-org-structure-hint))))

;; -----------------------------------------------------------------------------
;; Org structure hint (“Соблюдать структуру”)
;;
;; Goals:
;; - When enabled, add a strict prompt rule: answer MUST be valid Org, start with an
;;   Org heading, and continue heading nesting deterministically.
;; - Default is ON (unless user customized the variable).
;; - Exposed as a modeline toggle (see carriage-ui.el segment).
;;
;; Policy (deterministic):
;; - If there is a heading at/above point → start response with a heading of THE SAME level.
;; - If there are no headings above → start with level 1 heading.
;; - First non-empty line must be exactly: "<stars> <short summary>".
;;
;; Note:
;; We keep the fragment small and explicit; this is intentionally not a “format everything”
;; mega-toggle. Additional rules should be added with care (token cost / conflicts).

(defconst carriage-typedblocks--org-structure-hint-marker
  "CARRIAGE_ORG_STRUCTURE_HINT"
  "Marker used to prevent duplicate structure-hint injection into prompt fragments.")

(defun carriage-typedblocks--org-last-heading-level (&optional pos)
  "Return Org heading level of the nearest heading at or before POS (default point), or nil.
Best-effort; never signals."
  (let ((p (or pos (point))))
    (when (number-or-marker-p p)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char p)
          (cond
           ;; Prefer Org API when available.
           ((and (derived-mode-p 'org-mode)
                 (require 'org nil t))
            (condition-case _e
                (progn
                  ;; If we're on a heading, use it.
                  ;; Otherwise move to the heading that contains point.
                  (unless (org-at-heading-p)
                    (ignore-errors (org-back-to-heading t)))
                  (let ((lvl (and (org-at-heading-p) (org-outline-level))))
                    ;; If not at any heading (e.g. before first), try previous heading.
                    (unless (and (integerp lvl) (> lvl 0))
                      (when (ignore-errors (org-previous-visible-heading 1) t)
                        (when (org-at-heading-p)
                          (setq lvl (org-outline-level)))))
                    (when (and (integerp lvl) (> lvl 0)) lvl)))
              (error nil)))
           ;; Regex fallback (works even without org loaded).
           (t
            (let ((case-fold-search t))
              (when (re-search-backward "^[ \t]*\\(\\*+\\)\\s-+" nil t)
                (length (match-string 1)))))))))))

(defun carriage-typedblocks--org-sample-heading-line (level &optional pos)
  "Return a sample heading line of LEVEL above POS (or point), or nil."
  (when (and (integerp level) (> level 0))
    (let ((p (or pos (point))))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char p)
          (let* ((case-fold-search t)
                 (re (format "^[ \t]*\\*\\{%d\\}\\s-+.*$" level)))
            (when (re-search-backward re nil t)
              (string-trim
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))))))))

(defun carriage-typedblocks--org-structure-hint-fragment ()
  "Return an Org-structure formatting rule fragment for prompt, or nil.
Applies only in Org buffers when `carriage-mode-typedblocks-structure-hint' is enabled."
  (when (and (derived-mode-p 'org-mode)
             (boundp 'carriage-mode-org-structure-hint)
             carriage-mode-org-structure-hint)
    (require 'subr-x nil t)
    (let* ((lvl (or (carriage-typedblocks--org-last-heading-level (point)) 1))
           (lvl (max 1 (or lvl 1)))
           (stars (make-string lvl ?*))
           (sample (carriage-typedblocks--org-sample-heading-line lvl (point))))
      (mapconcat
       #'identity
       (delq nil
             (list
              (format "[%s]" carriage-typedblocks--org-structure-hint-marker)
              "Ответ должен быть оформлен как часть Org-документа."
              "Не используй Markdown-ограждения вида ```."
              (format "Первая непустая строка ответа ДОЛЖНА быть заголовком Org уровня %d в точном формате:" lvl)
              (format "%s <краткое резюме итерации>" stars)
              "Требования к стартовому заголовку:"
              "- ровно N звёздочек (уровень), затем один пробел, затем текст;"
              "- заголовок кратко описывает суть текущей итерации/изменения;"
              "- никаких прелюдий/дисклеймеров/вводных строк до заголовка."
              (when (and (stringp sample) (not (string-empty-p (string-trim sample))))
                (format "Пример заголовка уровня %d выше по документу (следуй его стилю): %s" lvl sample))
              "Код/команды оформляй только через Org src-блоки:"
              "#+begin_src <lang>\n...\n#+end_src"))
       "\n"))))

(defun carriage-typedblocks--prompt-append-org-structure-hint (s)
  "Append Org structure hint fragment to S (prompt fragment) when applicable."
  (require 'subr-x nil t)
  (let* ((txt (or s "")))
    (if (or (string-match-p carriage-typedblocks--org-structure-hint-marker txt)
            (not (derived-mode-p 'org-mode))
            (not (boundp 'carriage-mode-org-structure-hint))
            (not carriage-mode-org-structure-hint))
        s
      (let ((frag (ignore-errors (carriage-typedblocks--org-structure-hint-fragment))))
        (if (and (stringp frag) (not (string-empty-p (string-trim frag))))
            (concat (string-trim-right txt) "\n\n" frag "\n")
          s)))))

(defun carriage-typedblocks--install-org-structure-hint-advices ()
  "Attach :filter-return advice to known typedblocks prompt fragment functions.
Best-effort and safe across refactors (fboundp-guarded)."
  (dolist (fn '(carriage-typedblocks-prompt-fragment
                carriage-typedblocks--prompt-fragment
                carriage-typedblocks--system-prompt-fragment
                carriage-typedblocks--user-prompt-fragment
                carriage-typedblocks--format-hints))
    (when (and (fboundp fn)
               (not (advice-member-p #'carriage-typedblocks--prompt-append-org-structure-hint fn)))
      (advice-add fn :filter-return #'carriage-typedblocks--prompt-append-org-structure-hint))))

(carriage-typedblocks--install-org-structure-hint-advices)

(defun carriage-typedblocks--maybe-set-structure-hint-default ()
  "Ensure default for `carriage-mode-typedblocks-structure-hint' is ON, unless user customized it."
  (when (boundp 'carriage-mode-typedblocks-structure-hint)
    ;; Respect user customization.
    (unless (or (get 'carriage-mode-typedblocks-structure-hint 'saved-value)
                (get 'carriage-mode-typedblocks-structure-hint 'customized-value))
      (setq-default carriage-mode-typedblocks-structure-hint t))))

(carriage-typedblocks--maybe-set-structure-hint-default)

;; Buffer-local default: enable when carriage-mode turns on, unless already set locally
;; (e.g. restored from doc-state or user toggled in this buffer).
(add-hook 'carriage-mode-hook
          (lambda ()
            (when (boundp 'carriage-mode-typedblocks-structure-hint)
              (unless (local-variable-p 'carriage-mode-typedblocks-structure-hint)
                (setq-local carriage-mode-typedblocks-structure-hint t)))))

;; Toggle command (used by modeline segment). Define only if missing.
(unless (fboundp 'carriage-toggle-typedblocks-structure-hint)
  (defun carriage-toggle-typedblocks-structure-hint ()
    "Toggle Org structure hint for typedblocks in the current buffer.
When enabled, Carriage adds a strict Org-format rule into the prompt."
    (interactive)
    (unless (boundp 'carriage-mode-typedblocks-structure-hint)
      (user-error "typedblocks: structure hint variable is not available"))
    (setq-local carriage-mode-typedblocks-structure-hint
                (not carriage-mode-typedblocks-structure-hint))
    ;; Persist best-effort (doc-state may also sync via its own hooks/advices).
    (when (fboundp 'carriage-doc-state-write-current)
      (ignore-errors (carriage-doc-state-write-current (current-buffer))))
    ;; Nudge UI.
    (when (fboundp 'carriage-ui--invalidate-ml-cache)
      (ignore-errors (carriage-ui--invalidate-ml-cache)))
    (force-mode-line-update)
    (message "Соблюдать структуру (Org): %s"
             (if carriage-mode-typedblocks-structure-hint "ON" "OFF"))))

;; -----------------------------------------------------------------------------
;; Org structure / outline compliance hint (modeline toggle)
;;
;; Purpose:
;; When enabled, inject strict Org-formatting requirements into the typedblocks
;; prompt fragment so the model produces an answer that continues the document.
;;
;; Policy:
;; - This is intentionally heuristic but deterministic: we compute a target heading
;;   level from the nearest Org heading above point in the current buffer.
;; - If none exists, the target is level 1.
;;
;; UI:
;; - Controlled by `carriage-mode-typedblocks-structure-hint' (buffer-local).
;; - Default is ON.
;;
;; Marker:
;; - We include a sentinel line to avoid duplicate injection on repeated builders.

(defgroup carriage-typedblocks-structure nil
  "Org structure compliance prompt rules."
  :group 'carriage
  :prefix "carriage-typedblocks-structure-")

(defcustom carriage-mode-typedblocks-structure-hint t
  "When non-nil, require the LLM answer to be formatted as Org Mode and to follow the document hierarchy.

This affects only prompt-building (typedblocks guidance); it does not change parsing/apply.

Rules (high level):
- Valid Org Mode (no Markdown fences).
- First non-empty line MUST be an Org heading.
- Heading level is aligned to the nearest heading above point (or level 1 if none)."
  :type 'boolean
  :group 'carriage-typedblocks-structure)
(setq-default carriage-mode-typedblocks-structure-hint t)
(make-variable-buffer-local 'carriage-mode-typedblocks-structure-hint)

;;;###autoload
(unless (fboundp 'carriage-toggle-typedblocks-structure-hint)
  (defun carriage-toggle-typedblocks-structure-hint ()
    "Toggle Org structure compliance hint for typedblocks in the current buffer."
    (interactive)
    (setq-local carriage-mode-typedblocks-structure-hint
                (not carriage-mode-typedblocks-structure-hint))
    ;; Persist best-effort (do not clobber other doc-state keys).
    (when (fboundp 'carriage-doc-state-write-current)
      (ignore-errors (carriage-doc-state-write-current (current-buffer))))
    ;; Refresh UI best-effort.
    (when (fboundp 'carriage-ui--invalidate-ml-cache)
      (ignore-errors (carriage-ui--invalidate-ml-cache)))
    (force-mode-line-update)
    (message "Org structure: %s" (if carriage-mode-typedblocks-structure-hint "ON" "OFF"))
    carriage-mode-typedblocks-structure-hint))

(defun carriage-typedblocks--org-structure-target-level ()
  "Return target Org heading level for the response based on nearest heading above point."
  (if (not (derived-mode-p 'org-mode))
      1
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search nil))
          (if (re-search-backward "^\\(\\*+\\)\\s-+" nil t)
              (max 1 (length (match-string 1)))
            1))))))

(defun carriage-typedblocks--org-structure-hint-fragment ()
  "Return a prompt fragment enforcing Org formatting and heading rules."
  (let* ((n (carriage-typedblocks--org-structure-target-level))
         (stars (make-string (max 1 n) ?*)))
    (mapconcat
     #'identity
     (list
      "CARRIAGE_ORG_STRUCTURE_HINT:"
      "Форматирование ответа (Org Mode, режим «Соблюдать структуру»):"
      "- Ответ должен быть валидным Org Mode (НЕ Markdown)."
      "- Не используй Markdown-ограждения для кода (``` ... ```)."
      (format "- Первая непустая строка ответа ОБЯЗАНА быть заголовком уровня %d в точном формате: \"%s <краткое резюме итерации>\"." n stars)
      "- <краткое резюме итерации>: 3–12 слов, без точки в конце; по возможности следуй стилю/языку соседних заголовков того же уровня (префиксы, теги), но не выдумывай лишнего."
      "- Для кода/команд используй Org src-блоки: #+begin_src <lang> … #+end_src."
      "- Не вставляй ответ внутрь существующих #+begin_* блоков; если нужно — создавай новые блоки корректно.")
     "\n")))

(defun carriage-typedblocks--maybe-append-org-structure-hint (text)
  "Append Org structure hint to TEXT when enabled; avoid duplicates via sentinel."
  (if (and (stringp text)
           (boundp 'carriage-mode-org-structure-hint)
           carriage-mode-org-structure-hint
           (derived-mode-p 'org-mode)
           (not (string-match-p "CARRIAGE_ORG_STRUCTURE_HINT:" text)))
      (concat text "\n\n" (carriage-typedblocks--org-structure-hint-fragment))
    text))

(defun carriage-typedblocks--org-structure-hint-advice (orig &rest args)
  "Around-advice: append Org structure hint to typedblocks prompt fragments."
  (carriage-typedblocks--maybe-append-org-structure-hint (apply orig args)))

(defun carriage-typedblocks--install-org-structure-hint-advices ()
  "Install prompt-fragment advices (best-effort)."
  (dolist (fn '(carriage-typedblocks-prompt-fragment
                carriage-typedblocks--prompt-fragment
                carriage-typedblocks--prompt-fragment-system
                carriage-typedblocks--prompt-fragment-user))
    (when (and (fboundp fn)
               (not (advice-member-p #'carriage-typedblocks--org-structure-hint-advice fn)))
      (advice-add fn :around #'carriage-typedblocks--org-structure-hint-advice))))

(carriage-typedblocks--install-org-structure-hint-advices)

(defgroup carriage-typedblocks-structure nil
  "Org-structure formatting guidance for Carriage typedblocks."
  :group 'carriage
  :prefix "carriage-typedblocks-structure-")

(defcustom carriage-mode-typedblocks-structure-hint t
  "When non-nil, Carriage asks the model to format the answer as a continuation of the current Org document.

Policy:
- Enforce Org Mode output (no Markdown fences).
- Require the first non-empty line to be an Org heading.
- Determine the heading level from the nearest Org heading above point; if none, use level 1."
  :type 'boolean
  :group 'carriage-typedblocks-structure)

(make-variable-buffer-local 'carriage-mode-typedblocks-structure-hint)

(defun carriage-typedblocks--org-nearest-heading-level (&optional pos)
  "Return the star level (integer) of the nearest Org heading above POS (or point), or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (or pos (point)))
      (let ((case-fold-search t))
        ;; Find the closest heading above point. Use a strict heading regexp to avoid false positives.
        (when (re-search-backward "^\\(\\*+\\)\\s-+\\S-" nil t)
          (length (match-string 1)))))))

(defun carriage-typedblocks--org-target-heading-level ()
  "Return target Org heading level for the next answer (>=1)."
  (max 1 (or (and (derived-mode-p 'org-mode)
                  (carriage-typedblocks--org-nearest-heading-level (point)))
             1)))

(defun carriage-typedblocks--org-structure-hint-text ()
  "Return strict Org formatting rules for the model, or nil when not applicable."
  (when (and (boundp 'carriage-mode-org-structure-hint)
             carriage-mode-org-structure-hint
             (derived-mode-p 'org-mode))
    (let* ((lvl (carriage-typedblocks--org-target-heading-level))
           (stars (make-string lvl ?*)))
      (mapconcat
       #'identity
       (list
        "ВАЖНО: Ответ должен быть оформлен как часть Org Mode документа."
        "Запрещено использовать Markdown fences вида ```."
        (format "Первая непустая строка ответа ДОЛЖНА быть заголовком Org уровня %d строго в формате:" lvl)
        (format "%s <краткое резюме итерации>" stars)
        (format "Требование строгое: начни строку ровно с \"%s \" (звёздочки + пробел) и НЕ добавляй никакого текста/преамбулы до заголовка." stars)
        "Заголовок должен кратко описывать суть этой итерации; без точки в конце."
        "Далее ответ оформляй как валидный Org:"
        "- код/команды — через #+begin_src <lang> ... #+end_src (без ```)"
        "- списки — обычными Org bullet'ами (-/+).")
       "\n"))))

(defun carriage-typedblocks--structure-hint--append (orig &rest args)
  "Around-advice: append Org structure hint to typedblocks prompt fragments."
  (let* ((res (apply orig args))
         (hint (ignore-errors (carriage-typedblocks--org-structure-hint-text))))
    (cond
     ((not (and (stringp hint) (> (length hint) 0)))
      res)
     ((stringp res)
      (concat res "\n\n" hint "\n"))
     ((listp res)
      ;; Some implementations may return a list of fragments.
      (append res (list hint)))
     (t res))))

(unless (fboundp 'carriage-toggle-typedblocks-structure-hint)
  ;;;###autoload
  (defun carriage-toggle-typedblocks-structure-hint ()
    "Toggle Org-structure formatting guidance for typedblocks in the current buffer."
    (interactive)
    (setq-local carriage-mode-typedblocks-structure-hint
                (not (and (boundp 'carriage-mode-typedblocks-structure-hint)
                          carriage-mode-typedblocks-structure-hint)))
    ;; Persist best-effort (doc-state v2).
    (when (fboundp 'carriage-doc-state-write-current)
      (ignore-errors (carriage-doc-state-write-current (current-buffer))))
    ;; UI refresh best-effort.
    (when (fboundp 'carriage-ui--invalidate-and-refresh)
      (ignore-errors (carriage-ui--invalidate-and-refresh)))
    (force-mode-line-update)
    (message "Org structure: %s"
             (if carriage-mode-typedblocks-structure-hint "ON" "OFF"))))

;; Attach the hint to whichever prompt-fragment function exists in this build.
(dolist (fn '(carriage-typedblocks-prompt-fragment
              carriage-typedblocks--prompt-fragment
              carriage-typedblocks--build-prompt-fragment))
  (when (and (fboundp fn)
             (not (advice-member-p #'carriage-typedblocks--structure-hint--append fn)))
    (advice-add fn :around #'carriage-typedblocks--structure-hint--append)))

;; -----------------------------------------------------------------------------
;; Org structure compliance hint (modeline toggle: \"Соблюдать структуру\").
;;
;; Policy:
;; - Default ON (can be customized / toggled per buffer).
;; - Prompt rule is strict: answer MUST be valid Org and MUST start with an Org heading
;;   at a computed target level (nearest heading above point + 1, else level 1).
;;
;; NOTE:
;; We keep the integration best-effort and non-invasive by advising
;; `carriage-typedblocks-prompt-fragment' (when it exists).
;; -----------------------------------------------------------------------------

;; Default ON (user Custom settings loaded later may override via setq-default).
(setq-default carriage-mode-typedblocks-structure-hint t)
(ignore-errors (make-variable-buffer-local 'carriage-mode-typedblocks-structure-hint))

(defun carriage-typedblocks--org-nearest-heading-level ()
  "Return the level (number of stars) of the nearest Org heading above point, or nil."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search t))
          (when (re-search-backward "^[ \t]*\\(\\*+\\)\\s-+" nil t)
            (length (match-string 1))))))))

(defun carriage-typedblocks--org-structure-target-level ()
  "Return target Org heading level for the next assistant answer."
  (let* ((lvl (or (carriage-typedblocks--org-nearest-heading-level) 0)))
    (cond
     ((> lvl 0) (min 20 lvl))
     (t 1))))

(defun carriage-typedblocks--org-structure-rules ()
  "Return a strict Org formatting rule string for prompt injection."
  (let* ((target (carriage-typedblocks--org-structure-target-level))
         (stars (make-string target ?*)))
    (mapconcat
     #'identity
     (list
      "Ответ должен быть оформлен как часть Org-документа (Org Mode)."
      "Запрещены Markdown fences вида ```."
      ""
      (format "Первая непустая строка ответа ДОЛЖНА быть заголовком Org уровня %d в точном формате:" target)
      (format "%s <краткое резюме итерации>" stars)
      ""
      "Требования к заголовку:"
      "- кратко описывает суть этой итерации;"
      "- не добавляй никакого текста/прелюдий до заголовка;"
      "- если в документе на этом уровне используются теги/префиксы/язык — следуй им; НЕ выдумывай новые теги/метки."
      ""
      "Для кода/команд/вывода используй Org src-блоки: #+begin_src <lang> ... #+end_src."
      "Сохраняй валидность Org (не ломай существующие begin_/end_ блоки; не вставляй маркеры typedblocks без необходимости).")
     "\n")))

(defun carriage-typedblocks--structure-hint-enabled-p ()
  "Return non-nil when the structure hint should be injected into the prompt."
  (and (boundp 'carriage-mode-org-structure-hint)
       carriage-mode-org-structure-hint))

(defun carriage-typedblocks--prompt-fragment-structure-around (orig &rest args)
  "Around-advice: append strict Org structure rules when enabled."
  (let* ((base (ignore-errors (apply orig args)))
         (base (if (stringp base) base "")))
    (if (carriage-typedblocks--structure-hint-enabled-p)
        (concat base "\n\n" (carriage-typedblocks--org-structure-rules))
      base)))

(when (fboundp 'carriage-typedblocks-prompt-fragment)
  (ignore-errors
    (advice-add 'carriage-typedblocks-prompt-fragment :around
                #'carriage-typedblocks--prompt-fragment-structure-around)))

(provide 'carriage-typedblocks)
;;; carriage-typedblocks.el ends here
