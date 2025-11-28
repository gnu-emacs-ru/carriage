;;; carriage-i18n.el --- Simple i18n layer (ru/en) for UI strings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: i18n, l10n, ui
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/i18n-v2.org
;;   spec/ui-v2.org
;;
;;; Commentary:
;; Lightweight internationalization layer for UI strings (RU/EN) used by the
;; Carriage UI, which-key hints and transient titles.
;;
;;; Code:
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/i18n-v2.org
;;   spec/ui-v2.org

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-i18n nil
  "Internationalization (i18n) for Carriage UI."
  :group 'applications
  :prefix "carriage-i18n-")

(defcustom carriage-i18n-locale 'ru
  "Current locale for Carriage UI ('ru or 'en)."
  :type '(choice (const ru) (const en))
  :group 'carriage-i18n)

(defconst carriage-i18n--table
  '(
    ;; Menu, groups, which-key
    (:carriage-menu      . ((ru . "Carriage Menu")      (en . "Carriage Menu")))
    (:carriage-toggles   . ((ru . "Carriage Toggles")   (en . "Carriage Toggles")))
    (:navigate-title     . ((ru . "Навигация")          (en . "Navigate")))
    (:act-title          . ((ru . "Действия")           (en . "Actions")))
    (:session-title      . ((ru . "Сессия/Git")         (en . "Session/Git")))
    (:tools-title        . ((ru . "Инструменты")        (en . "Tools")))
    (:logs-title         . ((ru . "Логи/Отчёты")        (en . "Logs/Reports")))
    (:context-title      . ((ru . "Контекст")           (en . "Context")))

    ;; Model tooltip + common labels
    (:model-tooltip      . ((ru . "Модель: %s")         (en . "Model: %s")))

    ;; Actions (keyspec desc-keys)
    (:model-select       . ((ru . "Выбрать модель")            (en . "Select model")))
    (:toggle-ctx         . ((ru . "Переключить gptel-контекст") (en . "Toggle gptel-context")))
    (:toggle-doc         . ((ru . "Переключить файлы из документа") (en . "Toggle document files")))
    (:select-suite       . ((ru . "Выбрать Suite")             (en . "Select Suite")))
    (:toggle-intent      . ((ru . "Переключить Intent")        (en . "Toggle Intent")))
    (:dry-run            . ((ru . "Dry-run под точкой")        (en . "Dry-run at point")))
    (:apply              . ((ru . "Применить под точкой")      (en . "Apply at point")))
    (:apply-all          . ((ru . "Применить итерацию")        (en . "Apply last iteration")))
    (:abort              . ((ru . "Отменить")                  (en . "Abort")))
    (:report             . ((ru . "Открыть отчёт")             (en . "Open report")))
    (:wip                . ((ru . "Переключить на WIP")        (en . "Switch to WIP")))
    (:reset              . ((ru . "Soft reset")                (en . "Soft reset")))
    (:commit-all         . ((ru . "Коммит (все изменения)")    (en . "Commit all changes")))
    (:commit-last        . ((ru . "Коммит последней итерации") (en . "Commit last iteration")))
    (:engine             . ((ru . "Выбор движка")              (en . "Select engine")))
    (:menu               . ((ru . "Меню")                      (en . "Menu")))
    (:send-buffer        . ((ru . "Отправить буфер")           (en . "Send buffer")))
    (:send-subtree       . ((ru . "Отправить поддерево")       (en . "Send subtree")))
    (:open-buffer        . ((ru . "Открыть буфер Carriage")    (en . "Open Carriage buffer")))
    (:report-diff        . ((ru . "Показать дифф")             (en . "Show diff")))
    (:report-ediff       . ((ru . "Ediff для элемента")        (en . "Ediff for item")))
    (:report-apply       . ((ru . "Применить элемент")         (en . "Apply item")))
    (:show-log           . ((ru . "Показать лог")              (en . "Show log")))
    (:show-traffic       . ((ru . "Показать трафик")           (en . "Show traffic")))
    (:quit               . ((ru . "Закрыть окно")              (en . "Quit window")))
    (:clean               . ((ru . "Удалить блоки патчей")     (en . "Cleanup patches")))

    ;; UI v1.3 — доп. ключи
    (:suite              . ((ru . "Suite")                     (en . "Suite")))
    (:engine-label       . ((ru . "Engine")                    (en . "Engine")))
    (:suite-tooltip      . ((ru . "Выбрать Suite")             (en . "Select Suite")))
    (:engine-tooltip     . ((ru . "Выбрать движок применения") (en . "Select Engine")))
    (:engine-tooltip-branch . ((ru . "Движок: %s (ветки: %s)") (en . "Engine: %s (branches: %s)")))
    (:commit             . ((ru . "Коммит")                    (en . "Commit")))
    (:commit-tooltip     . ((ru . "Сделать коммит")            (en . "Make a commit")))
    (:intent-ask         . ((ru . "Вопрос")                    (en . "Ask")))
    (:intent-code        . ((ru . "Код")                       (en . "Code")))
    (:intent-hybrid      . ((ru . "Гибрид")                    (en . "Hybrid")))
    ;; Save settings
    (:save-settings      . ((ru . "Сохранить настройки")       (en . "Save settings")))
    (:ctx-toggle         . ((ru . "Контекст gptel")            (en . "gptel context")))
    (:files-toggle       . ((ru . "Файлы из документа")        (en . "Document files")))
    (:ctx-tooltip        . ((ru . "Переключить gptel-контекст") (en . "Toggle gptel-context")))
    (:files-tooltip      . ((ru . "Переключить файлы документа") (en . "Toggle document files")))
    (:toggle-patched     . ((ru . "Файлы применённых патчей")    (en . "Applied patch files")))
    (:patched-tooltip    . ((ru . "Переключить включение файлов из #+patch_done") (en . "Toggle including files from #+patch_done")))
    (:doc-scope-all      . ((ru . "Все begin_context")          (en . "All begin_context")))
    (:doc-scope-last     . ((ru . "Последний begin_context")    (en . "Last begin_context")))
    (:doc-scope-all-tip  . ((ru . "Собирать из всех блоков begin_context") (en . "Use all begin_context blocks")))
    (:doc-scope-last-tip . ((ru . "Собирать только из ближайшего/последнего") (en . "Use last/nearest begin_context block")))
    (:doc-scope-cycle    . ((ru . "Переключить Scope (All/Last)") (en . "Cycle Scope (All/Last)")))
    (:doc-scope-cycle-tip . ((ru . "Переключить режим begin_context: All ↔ Last") (en . "Cycle begin_context scope: All ↔ Last")))
    (:context-help       . ((ru . "Нажмите t, затем букву (g,f,p,v,a,l,s,P)") (en . "Press t, then letter (g,f,p,v,a,l,s,P)")))
    (:flash-enabled      . ((ru . "Мигание патчей по завершении") (en . "Flash patches on completion")))
    (:audio-enabled      . ((ru . "Звуковое уведомление")      (en . "Audio notification")))
    (:task-new           . ((ru . "Создать задачу из этого") (en . "New task from that")))

    ;; State tooltips (status help-echo)
    (:state-tt-error     . ((ru . "Ошибка: %s — %s (источник: %s)")
                            (en . "Error: %s — %s (source: %s)")))
    (:state-tt-streaming . ((ru . "Модель: %s; этап: %s; длительность: %ss; чанки=%d")
                            (en . "Model: %s; stage: %s; duration: %ss; chunks=%d")))
    (:state-tt-reasoning . ((ru . "Reasoning: %s")
                            (en . "Reasoning: %s")))
    (:state-tt-apply     . ((ru . "Результаты: ok=%d, skip=%d, fail=%d (всего %d)")
                            (en . "Results: ok=%d, skip=%d, fail=%d (total %d)")))
    (:state-tt-dry       . ((ru . "Предпросмотр: ok=%d, skip=%d, fail=%d (всего %d)")
                            (en . "Dry-run: ok=%d, skip=%d, fail=%d (total %d)")))
    ;; Visible toggle (source 'visible)
    (:visible-tooltip    . ((ru . "Переключить видимые буферы") (en . "Toggle visible buffers")))
    ;; Additional keys for menus and which-key
    (:toggle-profile     . ((ru . "Переключить профиль P1/P3")    (en . "Toggle P1/P3")))
    (:insert-assist      . ((ru . "Вставка/Assist")               (en . "Insert/Assist")))
    (:insert-assist-menu . ((ru . "Меню вставки/Assist")          (en . "Insert/Assist Menu")))
    (:file-chat          . ((ru . "Чат по файлу")                 (en . "File chat")))
    (:branch-doc         . ((ru . "Ветвление из шаблона")         (en . "Branch from template")))
    ;; Insert/Assist actions
    (:insert-plan        . ((ru . "Вставить план")                (en . "Insert Plan")))
    (:insert-step        . ((ru . "Вставить шаг")                 (en . "Insert Step")))
    (:insert-test        . ((ru . "Вставить тест")                (en . "Insert Test")))
    (:insert-retro       . ((ru . "Вставить ретро")               (en . "Insert Retro")))
    (:assist-context-delta . ((ru . "Assist: дельта контекста")   (en . "Assist Context Delta")))
    )
  "Translation table: KEY → ((ru . STR) (en . STR)).")

(defun carriage-i18n-known-keys ()
  "Return a list of known i18n keys."
  (mapcar #'car carriage-i18n--table))

(defun carriage-i18n (key &rest args)
  "Return localized string for KEY; ARGS are formatted with `format'.
Fallback policy: current locale → 'en → symbol-name of KEY."
  (let* ((cell (assq key carriage-i18n--table))
         (pair (and cell (cdr cell)))
         (lang carriage-i18n-locale)
         (txt (cond
               ((and pair (alist-get lang pair)) (alist-get lang pair))
               ((and pair (alist-get 'en pair))  (alist-get 'en pair))
               (t (symbol-name key)))))
    (if (and args (stringp txt))
        (apply #'format txt args)
      txt)))

(defun carriage-i18n-set-locale (locale)
  "Set LOCALE ('ru or 'en) for Carriage i18n and refresh UI."
  (setq carriage-i18n-locale (if (memq locale '(ru en)) locale 'en))
  (force-mode-line-update t)
  t)

(provide 'carriage-i18n)
;;; carriage-i18n.el ends here
