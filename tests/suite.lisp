(in-package :cl-user)
(defpackage :defrest-test (:use :cl :defrest :fiveam :hunchentoot)) ;; and :drakma but it clashes with hunchentoot namespace

(in-package :defrest-test)

(def-suite defrest-tests :description "Unit and runtime Tests for defrest")

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
(def-test testparse ()
  (let ((result (defrest::parse-schema "/test/{id:[a-z]}")))
    (is (= 2
	   (length result)))
    (is (equal "/test/"
	       (car result)))
    (is (equal "id"
	       (getf (cadr result) :key))))
  (let ((result (defrest::parse-uri "/test/{id:[0-9]+}.id" "/test/188.id")))
    (is (equal "188"
	       (gethash "id" result)))
    (is (= 1
	   (hash-table-count result)))))
(def-test testuriparse ()
  (let ((result (defrest::parse-uri "/test/{id:[0-5]}{name:.+}" "/test/3A%20B")))
    (is (equal "A B"
	       (gethash "name" result)))
    (is (equal "3"
	       (gethash "id" result)))
    (is (= 2
	   (hash-table-count result)))))

(def-test test-real-hunchentoot ()
  (let ((server (start (make-instance 'easy-acceptor :port 9876))))
    (unwind-protect 
	 (progn
	   (setq *dispatch-table* 
		 (list (create-rest-table-dispatcher)))
	   
	   
	   (defrest "/simple" :GET ()
	     "Hello World")
	   
	   (defrest "/greet/{name:.+}" :GET (name)
	     (concatenate 'string "Hello " name))
	   
	   (defrest "/post/{number:[0-9]+}.data" :POST (number)
	     (sqrt (parse-integer number)))

	   (is (equal "Hello World"
		      (drakma:http-request "http://localhost:9876/simple")))
	   (is (equal "Hello Bonk"
		      (drakma:http-request "http://localhost:9876/greet/Bonk")))
	   (is (equal "Hello Mr./Mrs. Freak"
		      (drakma:http-request "http://localhost:9876/greet/Mr./Mrs.%20Freak")))
	   (is (equal "3.0"
		      (drakma:http-request "http://localhost:9876/post/9.data" :method :POST)))
	   (is (= 404
		  (second (multiple-value-list 
			   (drakma:http-request "http://localhost:9876/post/9.data" :method :GET))))))
      (stop server))))
      





