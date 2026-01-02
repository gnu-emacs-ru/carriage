;;; carriage.el --- Entry point and initialization  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: core, entry
;;
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/carriage-mode-v2.org
;;   spec/extensibility-points-v2.org
;;   spec/apply-pipeline-v2.org
;;   spec/apply-engines-v2.org
;;   spec/llm-transport-v2.org
;;
;;; Commentary:
;; Package entry: define-errors, add load-path rules and require core modules.
;;
;;; Code:

(require 'carriage-errors)
(carriage-define-errors)

(require 'carriage-logging)
(require 'carriage-utils)
(require 'carriage-git)
;; Registries and suite builder (ops modules register themselves)
(require 'carriage-format-registry)
(require 'carriage-intent-registry)
(require 'carriage-suite)
(require 'carriage-global-mode)
(require 'carriage-parser)
(require 'carriage-apply)

(require 'carriage-apply-engine)
(require 'carriage-engine-git)
(require 'carriage-engine-emacs)
(require 'carriage-mode)
(require 'carriage-transport)
(require 'carriage-task)
;; Transports are loaded lazily by carriage-transport-dispatch per spec.
;; Do not require adapters by default here.

(require 'carriage-announce)

(provide 'carriage)
;;; carriage.el ends here
