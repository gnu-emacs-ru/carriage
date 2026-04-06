;;; carriage-ui-faces.el --- UI faces and color palette  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Faces (defface) definitions for Carriage UI components.
;; Extracted from carriage-ui.el for modularity.

;;; Code:

(require 'carriage-utils)

(defgroup carriage-ui nil
  "UI components for Carriage."
  :group 'applications
  :prefix "carriage-")

;; Harmonious palette with explicit foregrounds (avoid theme desaturation to gray)
(defface carriage-ui-accent-blue-face
  '((t :inherit nil :foreground "#6fa8dc"))
  "Accent face (blue-ish) for UI icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-green-face
  '((t :inherit nil :foreground "#93c47d"))
  "Accent face (green) for success/apply icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-yellow-face
  '((t :inherit nil :foreground "#f1c232"))
  "Accent face (yellow) for caution/WIP icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-red-face
  '((t :inherit nil :foreground "#e06666"))
  "Accent face (red) for abort/error icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-purple-face
  '((t :inherit nil :foreground "#8e7cc3"))
  "Accent face (purple) for code/report/confirm icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-orange-face
  '((t :inherit nil :foreground "#f6b26b"))
  "Accent face (orange) for diff/dry icons."
  :group 'carriage-ui)

(defface carriage-ui-accent-cyan-face
  '((t :inherit nil :foreground "#76a5af"))
  "Accent face (cyan/teal) for model/reset/icons."
  :group 'carriage-ui)

(defface carriage-ui-muted-face
  '((t :inherit nil :foreground "#9e9e9e"))
  "Muted face for disabled toggle icons."
  :group 'carriage-ui)

;; State faces (UI v1.3)
(defface carriage-ui-state-idle-face
  '((t :inherit nil :foreground "#268bd2"))
  "Mode-line face for idle state (blue)."
  :group 'carriage-ui)

(defface carriage-ui-state-sending-face
  '((t :inherit nil :foreground "#93c47d"))
  "Mode-line face for sending/streaming states (green)."
  :group 'carriage-ui)

(defface carriage-ui-state-error-face
  '((t :inherit nil :foreground "#e06666"))
  "Mode-line face for error state (red)."
  :group 'carriage-ui)

;; New state faces for refined color mapping
(defface carriage-ui-state-success-face
  '((t :inherit nil :foreground "#93c47d"))
  "Mode-line face for success/idle/done states (green)."
  :group 'carriage-ui)

(defface carriage-ui-state-active-face
  '((t :inherit nil :foreground "#f6b26b"))
  "Mode-line face for active reasoning/waiting/streaming/dispatch states (orange)."
  :group 'carriage-ui)

;; Faces for patch block highlighting (spec/ui-v2.org)
(defface carriage-patch-valid-face
  '((t :inherit nil :background "#203a24"))
  "Face for visually marking valid patch blocks."
  :group 'carriage-ui)

(defface carriage-patch-warning-face
  '((t :inherit nil :background "#3a2f20"))
  "Face for visually marking suspicious patch blocks."
  :group 'carriage-ui)

(defface carriage-patch-error-face
  '((t :inherit nil :background "#3a2020"))
  "Face for visually marking erroneous patch blocks."
  :group 'carriage-ui)

;; Faces for report rows (OK/WARN/ERR)
(defface carriage-report-ok-face
  '((t :inherit success))
  "Face for OK rows in report."
  :group 'carriage-ui)

(defface carriage-report-warn-face
  '((t :inherit warning))
  "Face for WARN rows in report."
  :group 'carriage-ui)

(defface carriage-report-err-face
  '((t :inherit error))
  "Face for ERR rows in report."
  :group 'carriage-ui)

(provide 'carriage-ui-faces)
;;; carriage-ui-faces.el ends here
