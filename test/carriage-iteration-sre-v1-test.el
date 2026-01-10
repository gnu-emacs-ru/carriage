;;; carriage-iteration-sre-v1-test.el --- Iteration + plan order (SRE v1) -*- lexical-binding: t; -*-

(require 'ert)
(require 'carriage)
(require 'carriage-apply)

(ert-deftest carriage-group-order-create-then-sre-v1 ()
  "Order: delete→rename→create→patch→sre. Ensure create < sre."
  (let* ((create (list (cons :version "1") (cons :op 'create) (cons :file "a.txt")
                       (cons :content "foo\n")))
         (sre    (list (cons :version "1") (cons :op 'sre) (cons :file "a.txt")
                       (cons :pairs (list (list (cons :from "foo")
                                                (cons :to "bar")
                                                (cons :opts '(:occur first :match literal)))))))
         (sorted (funcall (symbol-function 'carriage--plan-sort) (list sre create))))
    (should (= (length sorted) 2))
    (should (eq (alist-get :op (car sorted)) 'create))
    (should (eq (alist-get :op (cadr sorted)) 'sre))))

(provide 'carriage-iteration-sre-v1-test)
;;; carriage-iteration-sre-v1-test.el ends here
