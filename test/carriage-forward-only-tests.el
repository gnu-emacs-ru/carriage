;;; carriage-forward-only-tests.el --- Forward-only Swarm invariants  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-forward-only--deleted-webd-files-are-gone ()
  "Forward-only invariant: legacy webd/web-supervisor modules must not exist."
  (should-not (locate-library "carriage-web-supervisor"))
  (should-not (locate-library "carriage-web-publish"))
  ;; Extra forward-only guards (no legacy webd/dashboard stacks).
  (should-not (locate-library "carriage-webd"))
  (should-not (locate-library "carriage-web-dashboard")))

(ert-deftest carriage-forward-only--carriage-loads-without-legacy-webd ()
  "Forward-only invariant: core entrypoint must load without legacy webd modules."
  (let ((err nil))
    (condition-case e
        (progn
          (require 'carriage)
          t)
      (error (setq err e) nil))
    (should-not err)))

(ert-deftest carriage-forward-only--swarm-modules-load ()
  "Forward-only invariant: Swarm modules load (no legacy webd dependency chain)."
  (let ((err nil))
    (condition-case e
        (progn
          (require 'carriage-swarm-registry)
          (require 'carriage-agent)
          (require 'carriage-hub)
          (require 'carriage-swarm-supervisor)
          t)
      (error (setq err e) nil))
    (should-not err)
    (should (fboundp 'carriage-swarm-agent-start))
    (should (fboundp 'carriage-swarm-hub-start))
    (should (fboundp 'carriage-hub-start))
    (should (fboundp 'carriage-agent-main))))

(provide 'carriage-forward-only-tests)
;;; carriage-forward-only-tests.el ends here
