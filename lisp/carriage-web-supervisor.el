;;; carriage-web-supervisor.el --- DEPRECATED: use Swarm supervisor  -*- lexical-binding: t; -*-

;; This module was part of the legacy "webd" design.
;; It is intentionally kept as a tiny shim to avoid hard load failures
;; in older entrypoints. Forward-only replacement:
;;   - lisp/carriage-swarm-supervisor.el
;;   - lisp/carriage-agent.el
;;   - lisp/carriage-hub.el
;;
;; Specs (current):
;;   spec/swarm-v1.org
;;   spec/swarm-ops-v1.org

(require 'subr-x)

(defun carriage-webd-start (&rest _)
  "DEPRECATED. Use `carriage-swarm-agent-start' and `carriage-swarm-hub-start'."
  (user-error "carriage-webd is deprecated; use Swarm (carriage-swarm-agent-start / carriage-swarm-hub-start)"))

(defun carriage-webd-stop (&rest _)
  "DEPRECATED. Use `carriage-swarm-hub-stop' and `carriage-swarm-agent-stop'."
  (user-error "carriage-webd is deprecated; use Swarm (carriage-swarm-hub-stop / carriage-swarm-agent-stop)"))

(defun carriage-webd-restart (&rest _)
  "DEPRECATED."
  (user-error "carriage-webd is deprecated; use Swarm supervisor"))

(defun carriage-webd-status (&rest _)
  "DEPRECATED."
  (user-error "carriage-webd is deprecated; use Swarm supervisor"))

(defun carriage-webd-open-dashboard (&rest _)
  "DEPRECATED. Use `carriage-swarm-open-dashboard'."
  (when (fboundp 'carriage-swarm-open-dashboard)
    (call-interactively #'carriage-swarm-open-dashboard)))

(provide 'carriage-web-supervisor)
;;; carriage-web-supervisor.el ends here
