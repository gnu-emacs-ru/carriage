;;; carriage-hub-proxy-tests.el --- Minimal hub smoke tests  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest carriage-hub--loads ()
  "Hub module loads and exports entrypoints."
  (require 'carriage-hub)
  (should (fboundp 'carriage-hub-start))
  (should (fboundp 'carriage-hub-stop)))

(provide 'carriage-hub-proxy-tests)
;;; carriage-hub-proxy-tests.el ends here
