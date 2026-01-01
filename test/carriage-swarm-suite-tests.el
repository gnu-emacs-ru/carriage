;;; carriage-swarm-suite-tests.el --- Aggregated Swarm test suite  -*- lexical-binding: t; -*-

;; Convenience loader for running the full Swarm-related ERT suite in batch:
;;
;;   emacs -Q --batch -L lisp -L test -l test/carriage-swarm-suite-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; This file intentionally only requires test files; it does not define new tests.

(require 'ert)

(require 'carriage-forward-only-tests)
(require 'carriage-swarm-registry-tests)
(require 'carriage-agent-server-tests)
(require 'carriage-hub-proxy-tests)

(provide 'carriage-swarm-suite-tests)
;;; carriage-swarm-suite-tests.el ends here
