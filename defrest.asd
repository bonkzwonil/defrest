(in-package :cl-user)
(defpackage :defrest-system
  (:use :cl :asdf))

(in-package :defrest-system)

(defsystem :defrest
  :description "defrest: expose functions as REST webservices for ajax or other stuff"
  :version "1"
  :author "Mathias Menzel-Nielsen <(reverse \"ed.tfosztam@eztam\")>" ;nospam
  ;; The SPDX identifier for what LICENSE actually says, rather than the
  ;; family name: "BSD" alone names several licences with different terms,
  ;; and this field is read by machines.
  :license "BSD-2-Clause"
  :depends-on (:hunchentoot
	       :cl-ppcre
	       :split-sequence
	       :quri)
  :in-order-to ((test-op (test-op "defrest/test")))
  :components ((:static-file "defrest.asd")
	       (:file "defrest")))

;; defrest/test, not defrest.test.  ASDF reserves the "primary/secondary"
;; spelling for a system defined in another system's file and warns about
;; anything else; the dot form reads as a primary system named
;; "defrest.test", which is why a distribution's collector — which keeps
;; only systems whose primary name matches the .asd it came from — left
;; the test suite out of the dist entirely.
(defsystem :defrest/test
  :description "defrest Test Suite"
  :license "BSD-2-Clause"
  :depends-on (:defrest :fiveam :drakma)
  :components ((:module :tests
		:components ((:file "suite")))))
  

(defmethod perform ((op test-op) (c (eql (find-system :defrest/test))))
  (funcall (intern (symbol-name '#:run-all-tests) :it.bese.FiveAM)))
