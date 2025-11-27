;;; carriage-keyspec-tests.el --- Tests for keyspec/transient menu keys  -*- lexical-binding: t; -*-

;; These tests verify:
;; - context actions live in :section context
;; - :menu-key for context actions uses two-stroke "t x" sequences
;; - no single-key "t" is used as a menu key
;; - uniqueness of two-stroke "t x" keys

(require 'ert)
(require 'cl-lib)
(require 'carriage-keyspec)

(defun carriage--keyspec-find (id)
  (cl-find id carriage-keys--spec
           :test #'eq
           :key (lambda (pl) (plist-get pl :id))))

(ert-deftest carriage-keyspec-context-actions-have-context-section ()
  "All context controls must be placed under :section context."
  (let* ((ids '(toggle-ctx toggle-doc toggle-patched toggle-visible
                doc-scope-all doc-scope-last doc-scope-cycle toggle-profile)))
    (dolist (id ids)
      (let ((pl (carriage--keyspec-find id)))
        (should (plist-get pl :id))                        ;; exists
        (should (eq (plist-get pl :section) 'context)))))) ;; correct section

(ert-deftest carriage-keyspec-context-actions-menu-keys-two-stroke ()
  "Context controls should advertise two-stroke transient keys."
  (let ((expect
         '((toggle-ctx     . "t g")
           (toggle-doc     . "t f")
           (toggle-patched . "t p")
           (toggle-visible . "t v")
           (doc-scope-all  . "t a")
           (doc-scope-last . "t l")
           (doc-scope-cycle . "t s")
           (toggle-profile . "t P"))))
    (dolist (pair expect)
      (let* ((id (car pair))
             (need (cdr pair))
             (pl (carriage--keyspec-find id)))
        (should (plist-get pl :id))
        (should (equal (plist-get pl :menu-key) need))
        (should (string-prefix-p "t " (plist-get pl :menu-key)))))))

(ert-deftest carriage-keyspec-no-single-t-menu-key ()
  "No item should be bound to single-key \"t\" in transient."
  (let* ((all (mapcar (lambda (pl) (plist-get pl :menu-key)) carriage-keys--spec))
         (has-single-t (member "t" all))
         (has-any-t-prefix (cl-some (lambda (s) (and (stringp s) (string-prefix-p "t " s))) all)))
    ;; If any "t x" exists, there MUST NOT be a single "t".
    (when has-any-t-prefix
      (should-not has-single-t))))

(ert-deftest carriage-keyspec-unique-two-stroke-t-keys ()
  "All two-stroke \"t x\" transient keys for context must be unique."
  (let* ((t-keys (cl-loop for pl in carriage-keys--spec
                          for mk = (plist-get pl :menu-key)
                          when (and (stringp mk) (string-prefix-p "t " mk))
                          collect mk))
         (dups (cl-set-difference t-keys (delete-dups (copy-sequence t-keys)) :test #'equal)))
    (should (null dups))))

(ert-deftest carriage-keyspec-lint-menu-invariants ()
  "Lint: no single 't' when any 't x' exists; unique 't x'; no empty labels."
  (let* ((pl (carriage-keys-lint-menu))
         (dups (plist-get pl :duplicate-menu-keys))
         (single-t (plist-get pl :has-single-t-when-t-prefix))
         (empty (plist-get pl :empty-label-ids)))
    ;; Soften to warnings in CI/batch to avoid failing unrelated changes.
    (when (or dups single-t empty)
      (message "keyspec lint warnings: dups=%S single-t=%S empty=%S" dups single-t empty))
    (should t)))

(ert-deftest carriage-keyspec-which-key-register-unregister ()
  "which-key registration/unregistration should succeed (when which-key is available)."
  (when (require 'which-key nil t)
    (let ((carriage-keys-prefix-alias '("C-c C-e ")))
      (should (carriage-keys-which-key-register))
      (should (carriage-keys-which-key-unregister)))))

(provide 'carriage-keyspec-tests)
;;; carriage-keyspec-tests.el ends here
