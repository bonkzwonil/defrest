(in-package :cl-user)
(defpackage :defrest-test (:use :cl :defrest :fiveam :drakma))

(in-package :defrest-test)

(def-suite defrest-tests :description "Unit Tests for defrest")

(in-suite defrest-tests)


(def-test testurlfiddling ()
    (is (equal 
	 "/test/[0-9]+/gna"
	 (defrest::schema->regexpurl "/test/{id:[0-9]+}/gna")))
    (is (= 2
	   (length (defrest::split-on-placeholders "/this/url/{has:[^\/]*}/twoparts"))))
    (is (equal "[a-z]+"
	       (getf 
		(find "var" 
		     (defrest::preparse-uri-parameters->list 
		      "/music/lowercase/{var:[a-z]+}.mp3")
		     :key #'(lambda (x) (getf x :key))
		     :test #'equal)
		:regexp))))




