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
  "Фрагмент промпта для :op aibo (literal-only)."
  (concat
   "AIBO (literal-only, one file):\n"
   "#+begin_patch (:version \"1\" :op \"aibo\" :file \"RELATIVE/PATH\")\n"
   "#+pair (:occur all :expect K) ; optional, applies to the NEXT pair\n"
   "#+begin_from\nFROM text\n#+end_from\n"
   "#+begin_to\nTO text\n#+end_to\n"
   "#+end_patch\n"
   "- No regex; :match is forbidden.\n"
   "- For :occur all, :expect is required.\n"
   "- Empty TO block is allowed (delete matched text).\n"
   "- Do NOT generate unified diff (udiff) or any :op \"patch\" blocks.\n"
   "- Answer ONLY with begin_patch blocks for :op \"aibo\"; prose is allowed, tool applies ONLY blocks.\n"))

;;; Registration

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

;; Strict validation: forbid any :match in AIBO; require :expect when :occur all
(unless (fboundp 'carriage--aibo--validate-opts)
  (defun carriage--aibo--validate-opts (pairs)
    "Validate PAIRS for AIBO literal-only constraints.
- Any presence of :match is forbidden (regex not allowed).
- :occur must be one of 'first or 'all; when 'all, :expect must be present."
    (let ((tail pairs))
      (while tail
        (let* ((p (car tail))
               (opts (cond ((and (listp p) (plist-member p :opts)) (plist-get p :opts))
                           ((and (listp p)) (alist-get :opts p))
                           (t nil)))
               (occur (and opts (carriage--aibo--opt opts :occur)))
               (expect (and opts (carriage--aibo--opt opts :expect))))
          ;; Any :match key is not allowed in AIBO (literal-only format)
          (when (carriage--aibo--opt-present-p opts :match)
            (signal (carriage-error-symbol 'SRE_E_REGEX_SYNTAX)
                    (list "AIBO forbids :match; regex not allowed")))
          ;; Strict :occur validation
          (when (and occur (not (memq occur '(first all))))
            (signal (carriage-error-symbol 'SRE_E_OCCUR_VALUE) (list occur)))
          ;; :occur all requires :expect
          (when (and (eq occur 'all) (not expect))
            (signal (carriage-error-symbol 'SRE_E_OCCUR_EXPECT) nil)))
        (setq tail (cdr tail))))
    pairs))

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
