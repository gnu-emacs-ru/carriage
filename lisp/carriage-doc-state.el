;;; carriage-doc-state.el --- Persist/restore Carriage buffer state via Org property -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Carriage contributors
;; Author: Carriage Team <dev@carriage>
;; License: GPL-3+
;;
;;; Commentary:
;;
;; Doc-state v2:
;; - Canonical storage is a single file-level Org property:
;;     #+PROPERTY: CARRIAGE_STATE <sexp>
;; - <sexp> is a readable Emacs Lisp plist (preferred) with :CAR_* keys
;;   (or an alist convertible to such plist).
;; - Legacy storages (begin_carriage blocks, drawers, multi-line CARRIAGE_* props)
;;   are not supported by this module.
;;
;; Goals:
;; - Deterministic header placement: right after the last top-of-file #+PROPERTY line
;;   if any exist in the header, otherwise after the last top-of-file #+KEY line.
;; - Never insert inside/after any #+begin_* block.
;; - Idempotent write/normalization.
;; - Robustness: invalid/unreadable CARRIAGE_STATE must not break anything; restore becomes
;;   best-effort and defaults remain active.
;; - UI: folded summary uses overlay 'display (no invisible/before-string); reveal on point.
;;
;;; Code:
;; Display-based fold: overlays show badges when cursor is away; reveal original when cursor enters the line.

(require 'cl-lib)
(require 'subr-x)
(require 'carriage-doc-state-core)
(require 'carriage-doc-state-sync)
(require 'carriage-doc-state-fold)

(provide 'carriage-doc-state)
;;; carriage-doc-state.el ends here
