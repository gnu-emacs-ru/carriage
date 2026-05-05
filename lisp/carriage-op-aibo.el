;;; carriage-op-aibo.el --- AIBO literal-only op  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: ops, sre
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/file-header-format-v2.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/aibo-v2.org
;;   spec/parser-impl-v2.org
;;   spec/parser-registry-v2.org
;;
;;; Commentary:
;; Literal-only search/replace op implemented on top of SRE core.
;;
;;; Code:

;; AIBO = “жёсткий” формат поисково‑заменяющих патчей без регулярок.
;; Цель: максимальная надёжность и идемпотентность.
;;
;; Реализация:
;; - Парсинг тела делегируется толерантному парсеру SRE (begin_from/begin_to),
;;   затем выполняется строгая валидация AIBO:
;;     * :match в opts запрещён (regex недоступен) — ошибка SRE_E_REGEX_SYNTAX;
;;     * :occur ∈ {first, all}; иное — SRE_E_OCCUR_VALUE;
;;     * при :occur all обязателен :expect (целое ≥ 0) — SRE_E_OCCUR_EXPECT.
;; - Для каждой пары в итоговом плане :match принудительно устанавливается в 'literal.
;; - Dry-run и apply делегируются реализациям SRE (idempotent: NOOP → 'skip).
;;
;; Регистрация:
;; - carriage-format-register 'aibo "1" с :parse/:dry-run/:apply и prompt‑fragment.

(require 'cl-lib)
(require 'subr-x)

(require 'carriage-errors)
(require 'carriage-utils)
(require 'carriage-format-registry)

;; Нужны парсер тела и dry/apply из SRE
(require 'carriage-op-sre)

;;; Helpers

(defun carriage--aibo--normalize-occur (occur)
  "Вернуть нормализованное значение OCCUR ('first|'all) или исходное."
  (cond
   ((symbolp occur) occur)
   ((stringp occur) (intern (downcase occur)))
   (t occur)))

(defun carriage--aibo--validate-opts (pairs)
  "Проверить AIBO-ограничения для PAIRS.
PAIRS — список пар вида ((:from . STR) (:to . STR) (:opts . PLIST|ALIST))."
  (dolist (p pairs)
    (let* ((opts (if (and (listp p) (plist-member p :opts))
                     (plist-get p :opts)
                   (alist-get :opts p)))
           ;; Читаем :match из plist/alist — допускаем только 'literal
           (match-present (or (and (listp opts) (plist-member opts :match))
                              (assq :match opts)))
           (match-value   (cond
                           ((and (listp opts) (plist-member opts :match)) (plist-get opts :match))
                           ((assq :match opts) (cdr (assq :match opts)))
                           (t nil)))
           (occur (if (and (listp opts) (plist-member opts :occur))
                      (plist-get opts :occur)
                    (alist-get :occur opts)))
           (expect (if (and (listp opts) (plist-member opts :expect))
                       (plist-get opts :expect)
                     (alist-get :expect opts))))
      ;; 1) :match разрешён только как 'literal (может быть расставлен SRE по умолчанию)
      (when (and match-present (not (eq match-value 'literal)))
        (signal (carriage-error-symbol 'SRE_E_REGEX_SYNTAX)
                (list "AIBO forbids :match; regex not allowed")))
      ;; 2) Жёсткие значения :occur
      (let* ((oc (carriage--aibo--normalize-occur occur)))
        (unless (memq oc '(first all))
          (signal (carriage-error-symbol 'SRE_E_OCCUR_VALUE)
                  (list (format "Invalid :occur for AIBO: %S" occur))))
        (when (eq oc 'all)
          (unless (and (integerp expect) (>= expect 0))
            (signal (carriage-error-symbol 'SRE_E_OCCUR_EXPECT)
                    (list "Missing :expect for :occur all"))))))))

(defun carriage--aibo->sre-item (plan-item)
  "Преобразовать AIBO PLAN-ITEM (plist|alist) в SRE alist‑элемент.
Принудительно выставляет :match 'literal в opts каждой пары."
  (let* ((file (if (and (listp plan-item) (plist-member plan-item :file))
                   (plist-get plan-item :file)
                 (alist-get :file plan-item)))
         (pairs (or (if (and (listp plan-item) (plist-member plan-item :pairs))
                        (plist-get plan-item :pairs)
                      (alist-get :pairs plan-item))
                    '()))
         (pairs-lit
          (mapcar
           (lambda (p)
             (let* ((from (if (and (listp p) (plist-member p :from))
                              (plist-get p :from)
                            (alist-get :from p)))
                    (to   (if (and (listp p) (plist-member p :to))
                              (plist-get p :to)
                            (alist-get :to p)))
                    (opts (copy-sequence
                           (if (and (listp p) (plist-member p :opts))
                               (plist-get p :opts)
                             (alist-get :opts p)))))
               ;; Принудительно literal
               (setq opts (plist-put opts :match 'literal))
               (list (cons :from from) (cons :to to) (cons :opts opts))))
           pairs)))
    ;; Возвращаем строгий alist (единая форма)
    (list (cons :version "1")
          (cons :op 'sre)
          (cons :file file)
          (cons :pairs pairs-lit))))

;;; Parse

(defun carriage-parse-aibo (header body repo-root)
  "Разобрать блок AIBO v1 и вернуть plan item (alist).
Правила:
- :version = \"1\"
- :op = \"aibo\"
- :file — относительный путь внутри корня (TRAMP запрещён общей политикой)
- В теле: пары begin_from→begin_to; любые :match запрещены; :occur — только first|all;
  при :occur all обязателен :expect ≥ 0.
- Для каждой пары выставляется :match 'literal."
  ;; Заголовок
  (let ((version (plist-get header :version))
        (op (plist-get header :op))
        (file (plist-get header :file)))
    (unless (string= version "1")
      (signal (carriage-error-symbol 'SRE_E_VERSION) (list version)))
    (unless (member op '(aibo 'aibo "aibo"))
      (signal (carriage-error-symbol 'SRE_E_OP) (list op)))
    (unless (and (stringp file) (not (string-empty-p file)))
      (signal (carriage-error-symbol 'SRE_E_PATH) (list file)))
    ;; Pre-parse guards (explicit header-line opts) then SRE scan + strict validation
    ;; 1) Forbid any :match on the pair header (literal-only format)
    (when (string-match-p "^[ \t]*#\\+pair\\b[^\\n]*\\(:match\\)\\b" body)
      (signal (carriage-error-symbol 'SRE_E_REGEX_SYNTAX)
              (list "AIBO forbids :match; regex not allowed")))
    ;; 2) Require :expect when :occur all appears on the pair header
    (when (and (string-match-p "^[ \t]*#\\+pair\\b[^\\n]*\\(:occur[ \t]+all\\)\\b" body)
               (not (string-match-p "^[ \t]*#\\+pair\\b[^\\n]*\\(:expect\\)\\b" body)))
      (signal (carriage-error-symbol 'SRE_E_OCCUR_EXPECT) nil))
    ;; SRE scan and strict validation on parsed opts
    (let* ((pairs (carriage--sre-parse-body body)))
      (carriage--aibo--validate-opts pairs)
      ;; Принудительно literal в opts и нормализация пути
      (let* ((pairs-lit
              (mapcar
               (lambda (p)
                 (let* ((from (if (and (listp p) (plist-member p :from))
                                  (plist-get p :from)
                                (alist-get :from p)))
                        (to   (if (and (listp p) (plist-member p :to))
                                  (plist-get p :to)
                                (alist-get :to p)))
                        (opts (copy-sequence
                               (if (and (listp p) (plist-member p :opts))
                                   (plist-get p :opts)
                                 (alist-get :opts p)))))
                   (setq opts (plist-put opts :match 'literal))
                   (list (cons :from from) (cons :to to) (cons :opts opts))))
               pairs))
             (norm-path (carriage-normalize-path repo-root file)))
        (list (cons :version "1")
              (cons :op 'aibo)
              (cons :file (file-relative-name norm-path repo-root))
              (cons :pairs pairs-lit))))))

;;; Dry-run / Apply

(defun carriage-dry-run-aibo (plan-item repo-root)
  "Dry-run для AIBО: адаптация плана в SRE и делегирование."
  (let* ((sre-item (carriage--aibo->sre-item plan-item)))
    (carriage-dry-run-sre sre-item repo-root)))

(defun carriage-apply-aibo (plan-item repo-root)
  "Apply для AIBO: адаптация плана в SRE и делегирование (NOOP→'skip сохранён).
Меняет :op на 'aibo в возвращаемой строке отчёта."
  (let* ((sre-item (carriage--aibo->sre-item plan-item))
         (row (carriage-apply-sre sre-item repo-root)))
    (when (listp row)
      (setq row (plist-put row :op 'aibo)))
    row))

;;; Prompt fragment

(defun carriage-op-aibo-prompt-fragment (_ctx)
  "Фрагмент промпта для :op aibo (literal-only, reliable for weak/strong models)."
  (concat
   "DECISION TREE (READ FIRST — TOP 40 LINES):\n"
   "1) CREATE new file (user explicitly asks + path NOT in begin_map):\n"
   "   - ABSOLUTE: If path is NOT in begin_map -> use :op create immediately.\n"
   "   - NEVER request begin_context for files you intend to create.\n"
   "   - Hard rule: absence in begin_map + explicit create request = MUST create.\n"
   "   - If path IS in begin_map with exists=true -> create FORBIDDEN; use patch/sre/aibo.\n"
   "\n"
   "2) EDIT existing file (path in begin_map with exists=true):\n"
   "   - If text visible (In file <path>: with body) -> output patch block directly.\n"
   "   - If text NOT visible -> output ONLY #+begin_context with that path.\n"
   "   - NEVER use :op create for existing files (path in begin_map).\n"
   "\n"
   "3) begin_context is NOT a wishlist (CRITICAL):\n"
   "   - MUST list ONLY files that ALREADY exist and whose text you need to read.\n"
   "   - NEVER list files you intend to create (that's the #1 mistake).\n"
   "   - One begin_context block per iteration; full list of required paths.\n"
   "\n"
   "HARD CONTEXT VISIBILITY RULES:\n"
   "- `In file <path>:` with body = CURRENT TEXT present; treat as has_text=true.\n"
   "- If `In file <path>:` body is present, do NOT claim text is missing.\n"
   "- NEVER ask begin_context for paths with visible `In file <path>:` body.\n"
   "- begin_context is ONLY for existing files whose text is NOT visible.\n"
   "\n"
   "OPERATION ORDER (multiple ops):\n"
   "delete -> rename -> create -> patch -> sre/aibo\n"
   "========================================\n"
   "AIBO PATCH FORMAT — STRICT, LITERAL-ONLY\n"
   "========================================\n"
   "OUTPUT FORMAT (EXACT — COPY THIS STRUCTURE):\n"
   "1) First line MUST be history marker:\n"
   "   ;; patch history: RELATIVE/PATH — Short description\n"
   "2) Then ONE #+begin_patch block with EXACT structure:\n"
   "   #+begin_patch (:version \"1\" :op \"aibo\" :file \"RELATIVE/PATH\")\n"
   "   #+begin_from\n"
   "   EXACT source text to match (literal, no regex)\n"
   "   #+end_from\n"
   "   #+begin_to\n"
   "   EXACT replacement text (may be empty for deletion)\n"
   "   #+end_to\n"
   "   #+end_patch\n"
   "CRITICAL RULES (NON-NEGOTIABLE):\n"
   "- :match is FORBIDDEN (aibo is literal-only, no regex).\n"
   "- :occur must be 'first or 'all; if 'all, :expect is REQUIRED (integer >= 0).\n"
   "- Empty #+begin_to deletes matched text.\n"
   "- NO unified diff; NO :op \"patch\" blocks in aibo mode.\n"
   "- NO prose outside #+begin_patch block (Code mode).\n"
   "- Do NOT start with '*' or '**' (no Org heading before patch).\n"
   "- Do NOT emit no-op patches (FROM == TO).\n"
   "- If no changes needed -> output NOTHING (no patch block).\n"
   "SELF-CHECK BEFORE OUTPUT (6 items):\n"
   "[] op is exactly \"aibo\" (not \"patch\", \"sre\", \"create\")\n"
   "[] :version is \"1\" (string, not integer)\n"
   "[] :file is relative path (not :path key)\n"
   "[] begin_from/begin_to blocks used (not :from/:to header keys)\n"
   "[] no :match key anywhere (literal-only format)\n"
   "[] if :occur all -> :expect present and >= 0\n"
   "COMMON MISTAKES (AVOID THESE):\n"
   "- Missing parentheses in #+begin_patch header\n"
   "- Using :from/:to in header instead of begin_from/begin_to blocks\n"
   "- Forgetting ;; patch history line before the patch block\n"
   "- Adding prose or headings outside the patch block\n"
   "- Requesting begin_context for files you intend to create\n"
   "- Using :op create for files that exist in begin_map\n"
   "- Putting multiple operations in one patch block (one block per op)\n"
   "========================================\n"
   "FORMAT EXAMPLES (END OF PROMPT)\n"
   "========================================\n"
   "EXAMPLE 1 — CREATE (new file, path NOT in begin_map):\n"
   ";; patch history: config/vite.config.js — (no description)\n"
   "#+begin_patch (:version \"1\" :op \"create\" :file \"config/vite.config.js\")\n"
   "export default { root: '.' }\n"
   "#+end_patch\n"
   "EXAMPLE 2 — AIBO EDIT (existing file, text visible):\n"
   ";; patch history: lisp/example.el — Fix typo in docstring\n"
   "#+begin_patch (:version \"1\" :op \"aibo\" :file \"lisp/example.el\")\n"
   "#+begin_from\n"
   "(defun old-fn ()\n"
   "  \"Old docstring.\")\n"
   "#+end_from\n"
   "#+begin_to\n"
   "(defun new-fn ()\n"
   "  \"New docstring.\")\n"
   "#+end_to\n"
   "#+end_patch\n"
   "EXAMPLE 3 — CONTEXT REQUEST (existing file, text NOT visible):\n"
   "#+begin_context\n"
   "lisp/example.el\n"
   "#+end_context\n"))


;;;; Conflict Resolution Helpers

(defun carriage-aibo--conflict-context-payload (path reason)
  "Build begin_context payload for AIBO conflict resolution.
PATH is the file path, REASON is a short string explaining the conflict."
  (format "#+begin_context\n%s\n#+end_context\n;; Conflict: %s" path reason))

(defun carriage-aibo--should-fallback-to-context-p (error-sym plan-item)
  "Return non-nil if AIBO error should trigger begin_context fallback.
ERROR-SYM is the error symbol, PLAN-ITEM is the failed plan item."
  (memq error-sym
        '(SRE_E_REGEX_SYNTAX
          SRE_E_OCCUR_EXPECT
          SRE_E_SEGMENTS_COUNT
          SRE_E_UNPAIRED)))

(defun carriage-aibo--build-diagnostic-row (status file matches details &optional context-path)
  "Build diagnostic row with optional context suggestion.
STATUS is 'fail|'skip|'ok, FILE is the path, MATCHES is count,
DETAILS is string, CONTEXT-PATH suggests begin_context if non-nil."
  (let ((row (list :op 'aibo :status status :file file :matches matches :details details)))
    (when context-path
      (setq row (plist-put row :_suggest-context context-path)))
    row))

;;;; Registration

(carriage-format-register 'aibo "1"
                          :parse   #'carriage-parse-aibo
                          :dry-run #'carriage-dry-run-aibo
                          :apply   #'carriage-apply-aibo
                          :prompt-fragment #'carriage-op-aibo-prompt-fragment)

;; Robust AIBO opts readers (plist or alist)
(unless (fboundp 'carriage--aibo--opt)
  (defun carriage--aibo--opt (opts key)
    "Get KEY from OPTS whether it's a plist or alist."
    (cond
     ((and (listp opts) (plist-member opts key)) (plist-get opts key))
     ((and (listp opts)) (alist-get key opts))
     (t nil))))

(unless (fboundp 'carriage--aibo--opt-present-p)
  (defun carriage--aibo--opt-present-p (opts key)
    "Return non-nil if KEY is present in OPTS (plist or alist), even if value is nil."
    (or (and (listp opts) (plist-member opts key))
        (and (listp opts) (assq key opts)))))

;; (Validation implementation lives earlier in the file; avoid duplicate defs.)

;; Post-parse validator: ensure parse-aibo signals on invalid opts
(defun carriage--aibo--validate-parse-result (plan)
  "Filter-return advice for =carriage-parse-aibo' to validate PAIRS."
  (let ((pairs (carriage--plan-get plan :pairs)))
    (carriage--aibo--validate-opts pairs))
  plan)

(unless (advice-member-p #'carriage--aibo--validate-parse-result 'carriage-parse-aibo)
  (advice-add 'carriage-parse-aibo :filter-return #'carriage--aibo--validate-parse-result))

(provide 'carriage-op-aibo)
;;; carriage-op-aibo.el ends here
