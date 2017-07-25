(in-package :cl-user)
(defpackage :defrest-system
  (:use :cl :asdf))

(in-package :defrest-system)

(defsystem :defrest
  :description "defrest: expose functions as REST webservices for ajax or other stuff"
  :version "1"
  :author "Mathias Menzel-Nielsen <(reverse \"ed.tfosztam@eztam\")>" ;nospam
  :license "BSD"
  :depends-on (:hunchentoot
	       :cl-ppcre
	       :split-sequence)
  :in-order-to ((test-op (test-op "defrest.test")))
  :components ((:static-file "defrest.asd")
	       (:file "defrest")))

(defsystem :defrest.test
  :description "defrest Test Suite"
  :depends-on (:defrest :fiveam :drakma)
  :components ((:module :tests
		:components ((:file "suite")))))
  

(defmethod perform ((op test-op) (c (eql (find-system :defrest.test))))
  (funcall (intern (symbol-name '#:run-all-tests) :it.bese.FiveAM)))
