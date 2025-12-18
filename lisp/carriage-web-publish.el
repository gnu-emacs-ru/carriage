;;; carriage-web-publish.el --- DEPRECATED: no webd push publisher  -*- lexical-binding: t; -*-

;; This module was part of the legacy "webd push" pipeline.
;; Swarm model replaces it:
;; - Agents own their local HTTP/SSE server and cached state.
;; - Hub proxies to agents and injects tokens.
;;
;; Specs (current):
;;   spec/swarm-v1.org
;;   spec/agent-http-api-v1.org
;;   spec/hub-http-api-v1.org
;;   spec/registry-v1.org
;;   spec/swarm-ops-v1.org

(defun carriage-web-publish (&rest _)
  "DEPRECATED. In Swarm, publish happens inside each Agent process."
  (user-error "carriage-web-publish is deprecated; use Swarm (agents/hub)"))

(defun carriage-web-snapshot-start (&rest _)
  "DEPRECATED."
  (user-error "snapshot publisher is deprecated; Swarm agents own their own state"))

(defun carriage-web-snapshot-stop (&rest _)
  "DEPRECATED."
  (user-error "snapshot publisher is deprecated; Swarm agents own their own state"))

(defun carriage-web-snapshot-seed (&rest _)
  "DEPRECATED."
  (user-error "snapshot publisher is deprecated; Swarm agents own their own state"))

(provide 'carriage-web-publish)
;;; carriage-web-publish.el ends here
