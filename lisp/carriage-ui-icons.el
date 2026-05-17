;;; carriage-ui-icons.el --- Icon lookup/cache helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors

;;; Commentary:
;; Small icon-cache helpers extracted from carriage-ui.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-ui--invalidate-icon-cache-all-buffers "carriage-ui" ())

(defvar carriage-ui--icons-lib-available (featurep 'all-the-icons)
  "Cached availability of all-the-icons library.")

(defvar-local carriage-ui--icon-cache nil
  "Buffer-local cache of generated icon strings keyed by KEY or (toggle KEY ONP).")

(defvar-local carriage-ui--icon-cache-env nil
  "Environment snapshot for the icon cache to detect invalidation.
List of (gui use-icons height v-adjust themes).")

(defun carriage-ui--icon-cache-env-current ()
  "Return current environment signature for icon rendering."
  (list (display-graphic-p)
        (and (boundp 'carriage-mode-use-icons) carriage-mode-use-icons)
        carriage-mode-icon-height
        carriage-mode-icon-v-adjust
        custom-enabled-themes))

(defun carriage-ui--invalidate-icon-cache ()
  "Invalidate icon cache for the current buffer."
  (setq carriage-ui--icon-cache nil)
  (setq carriage-ui--icon-cache-env (carriage-ui--icon-cache-env-current)))

(defun carriage-ui--maybe-refresh-icon-cache-env ()
  "Ensure icon cache environment matches current UI; reset cache if not."
  (let ((cur (carriage-ui--icon-cache-env-current)))
    (unless (equal cur carriage-ui--icon-cache-env)
      (setq carriage-ui--icon-cache (make-hash-table :test 'equal))
      (setq carriage-ui--icon-cache-env cur))))

(defvar carriage-ui--icon-theme-hook-installed nil
  "Internal flag to install theme-change advice once.")

(unless carriage-ui--icon-theme-hook-installed
  (setq carriage-ui--icon-theme-hook-installed t)
  (advice-add 'load-theme :after (lambda (&rest _)
                                   (carriage-ui--invalidate-icon-cache-all-buffers))))

(defun carriage-ui--icons-available-p ()
  "Return non-nil when icons can be used in modeline."
  (let* ((use-flag (and (boundp 'carriage-mode-use-icons) carriage-mode-use-icons))
         (gui (display-graphic-p)))
    (unless carriage-ui--icons-lib-available
      (setq carriage-ui--icons-lib-available (require 'all-the-icons nil t)))
    (and use-flag gui carriage-ui--icons-lib-available)))

(defun carriage-ui--accent-hex (face)
  "Return final hexadecimal foreground color for FACE."
  (or (ignore-errors (face-foreground face nil 'default))
      (face-attribute face :foreground nil 'default)
      "#aaaaaa"))

(defun carriage-ui--icon (key)
  "Return icon string for KEY using all-the-icons, or nil if unavailable.
Results are cached per-buffer and invalidated when theme or UI parameters change."
  (when (carriage-ui--icons-available-p)
    (carriage-ui--maybe-refresh-icon-cache-env)
    (let* ((cache (or carriage-ui--icon-cache
                      (setq carriage-ui--icon-cache (make-hash-table :test 'equal))))
           (hit (gethash key cache)))
      (if (stringp hit)
          hit
        (let ((res
               (pcase key
                 ('ask  (when (fboundp 'all-the-icons-material)
                          (all-the-icons-material "chat"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face)))))
                 ('patch (when (fboundp 'all-the-icons-material)
                           (all-the-icons-material "code"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('hybrid (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "merge_type"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('model (cond
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "memory"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "cpu"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                          (t nil)))
                 ('project (cond
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "repo"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "folder"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            (t nil)))
                 ('file    (cond
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "file-text"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "description"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            (t nil)))
                 ('heading (cond
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "chevron_right"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face))))
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "bookmark"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face))))
                            (t nil)))
                 ('suite (cond
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "package"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "category"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          (t nil)))
                 ('engine (cond
                           ((fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "gear"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                           ((fboundp 'all-the-icons-material)
                            (all-the-icons-material "build"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face))))
                           (t nil)))
                 ('dry    (when (fboundp 'all-the-icons-faicon)
                            (all-the-icons-faicon "flask"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-orange-face)))))
                 ('apply  (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "check_circle"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-green-face)))))
                 ('all    (cond
                           ((fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "rocket"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                           ((fboundp 'all-the-icons-material)
                            (all-the-icons-material "play_arrow"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                           (t nil)))
                 ('abort  (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "stop"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-red-face)))))
                 ('report (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "file-text"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('save   (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "publish"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
                 ('wip    (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "git-branch"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('commit (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "git-commit"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('reset  (when (fboundp 'all-the-icons-octicon)
                            (all-the-icons-octicon "history"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust carriage-mode-icon-v-adjust
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
                 ('files (when (fboundp 'all-the-icons-material)
                           (all-the-icons-material "featured_play_list"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.14)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('ctx-limit (when (fboundp 'all-the-icons-material)
                               (all-the-icons-material "data_usage"
                                                       :height carriage-mode-icon-height
                                                       :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                       :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('ctx (when (fboundp 'all-the-icons-material)
                         (all-the-icons-material "toc"
                                                 :height carriage-mode-icon-height
                                                 :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                 :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face)))))
                 ('visible (cond
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "visibility"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "eye"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                            (t nil)))
                 ('patched (cond
                            ((fboundp 'all-the-icons-material)
                             (all-the-icons-material "code"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            ((fboundp 'all-the-icons-octicon)
                             (all-the-icons-octicon "checklist"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                            (t nil)))
                 ('map (cond
                        ((fboundp 'all-the-icons-material)
                         (all-the-icons-material "map"
                                                 :height carriage-mode-icon-height
                                                 :v-adjust carriage-mode-icon-v-adjust
                                                 :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                        ((fboundp 'all-the-icons-faicon)
                         (all-the-icons-faicon "map"
                                               :height carriage-mode-icon-height
                                               :v-adjust carriage-mode-icon-v-adjust
                                               :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                        ((fboundp 'all-the-icons-octicon)
                         (all-the-icons-octicon "tree"
                                                :height carriage-mode-icon-height
                                                :v-adjust carriage-mode-icon-v-adjust
                                                :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                        (t nil)))
                 ('plain (cond
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "subject"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "file-text"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-blue-face))))
                          (t nil)))
                 ('typed (cond
                          ((fboundp 'all-the-icons-material)
                           (all-the-icons-material "view_agenda"
                                                   :height carriage-mode-icon-height
                                                   :v-adjust (- carriage-mode-icon-v-adjust 0.12)
                                                   :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                          ((fboundp 'all-the-icons-octicon)
                           (all-the-icons-octicon "list-unordered"
                                                  :height carriage-mode-icon-height
                                                  :v-adjust carriage-mode-icon-v-adjust
                                                  :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face))))
                          (t nil)))
                 ('scope-all (when (fboundp 'all-the-icons-material)
                               (all-the-icons-material "layers"
                                                       :height carriage-mode-icon-height
                                                       :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                       :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('scope-all-off (when (fboundp 'all-the-icons-material)
                                   (all-the-icons-material "layers"
                                                           :height carriage-mode-icon-height
                                                           :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                           :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-muted-face)))))
                 ('scope-last (when (fboundp 'all-the-icons-material)
                                (all-the-icons-material "filter_1"
                                                        :height carriage-mode-icon-height
                                                        :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                        :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('scope-last-off (when (fboundp 'all-the-icons-material)
                                    (all-the-icons-material "filter_1"
                                                            :height carriage-mode-icon-height
                                                            :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                            :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-muted-face)))))
                 ('scope  (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "layers"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust (- carriage-mode-icon-v-adjust 0.1)
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-yellow-face)))))
                 ('profile (when (fboundp 'all-the-icons-material)
                             (all-the-icons-material "line_weight"
                                                     :height carriage-mode-icon-height
                                                     :v-adjust carriage-mode-icon-v-adjust
                                                     :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-purple-face)))))
                 ('inject (when (fboundp 'all-the-icons-material)
                            (all-the-icons-material "call_split"
                                                    :height carriage-mode-icon-height
                                                    :v-adjust carriage-mode-icon-v-adjust
                                                    :face (list :inherit nil :foreground (carriage-ui--accent-hex 'carriage-ui-accent-cyan-face)))))
                 (_ nil))))
          (when (stringp res)
            (puthash key res cache))
          res)))))

(provide 'carriage-ui-icons)
;;; carriage-ui-icons.el ends here
