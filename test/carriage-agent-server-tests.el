;;; carriage-agent-server-tests.el --- Minimal agent smoke tests  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-agent--loads ()
  "Agent module loads and exports entrypoint."
  (require 'carriage-agent)
  (should (fboundp 'carriage-agent-main)))

(provide 'carriage-agent-server-tests)
;;; carriage-agent-server-tests.el ends here
