(in-package :cl-user)
(defpackage :defrest-system
  (:use :cl :asdf))

(in-package :defrest-system)

(defsystem "defrest"
  :description "defrest: expose functions as REST webservices"
  :version "0.1"
  :author "Mathias Menzel-Nielsen <(reverse \"ed.tfosztam@eztam\")>" ;nospam
  :license "BSD"
  :depends-on (:hunchentoot
	       :cl-ppcre
	       :split-sequence)
  :components ((:file "defrest.lisp")))
  
  
