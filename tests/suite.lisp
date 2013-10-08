(in-package :cl-user)
(defpackage :defrest-test (:use :cl :defrest :fiveam :hunchentoot)) ;; and :drakma but it clashes with hunchentoot namespace

(in-package :defrest-test)

(def-suite defrest-tests :description "Unit and runtime Tests for defrest")

(in-suite defrest-tests)


(test testurlfiddling ()
  (is (equal 
       "/test/[0-9]+/gna"
       (defrest::schema->regexpurl "/test/{id:[0-9]+}/gna"))))
(test testparse ()
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
(test testuriparse ()
  (let ((result (defrest::parse-uri "/test/{id:[0-5]}{name:.+}" "/test/3A%20B")))
    (is (equal "A B"
	       (gethash "name" result)))
    (is (equal "3"
	       (gethash "id" result)))
    (is (= 2
	   (hash-table-count result)))))

(test test-real-hunchentoot ()
  (let* ((port 9876)
	 (server (start (make-instance 'easy-acceptor :port port)))
	 (baseurl (format nil "http://localhost:~a" port)))
    (labels ((url (uri)
	       (concatenate 'string baseurl uri)))
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
		      (drakma:http-request (url "/simple"))))
	   (is (equal "Hello Bonk"
		      (drakma:http-request (url "/greet/Bonk"))))
	   (is (equal "Hello Mr./Mrs. Freak"
		      (drakma:http-request (url "/greet/Mr./Mrs.%20Freak"))))
	   (is (equal "3.0"
		      (drakma:http-request (url "/post/9.data") :method :POST)))
	   (is (= 404
		  (second (multiple-value-list 
			   (drakma:http-request (url "/post/9.data") :method :GET)))))
	   ;; Different Methods on same url should work, too

	   (let ((db nil)
		 (uri "/todo/{id:[0-9]+}" ))

	     (defrest uri :GET (id)
	       (let ((result (find id db :key #'car :test 'equal)))
		 (if result
		     (cdr result)
		     (setf (return-code*) +http-not-found+))))

	     (defrest uri :POST (id)
	       (push (cons id 
			   (raw-post-data :force-text T))
		     db))

	     (defrest uri :DELETE (id)
	       (setf db (remove-if #'(lambda (x) (equal id x))
			  db
			  :key #'car))
	       (setf (return-code*) +http-no-content+)))
			  
	   
	   ;; Simple CRUD Test
	   (is (= 404 
		  (second (multiple-value-list (drakma:http-request (url "/todo/42") :method :GET)))))
	   (is (= 200
		  (second (multiple-value-list (drakma:http-request (url "/todo/42") :method :POST :content "Sample Item" :external-format-out :ASCII :content-type "text/plain")))))
	   (is (equal "Sample Item"
		      (drakma:http-request (url "/todo/42") :method :GET)))
	   (is (= +http-no-content+
		  (second (multiple-value-list (drakma:http-request (url "/todo/42") :method :DELETE)))))
	   (is (= 404 
		  (second (multiple-value-list (drakma:http-request (url "/todo/42") :method :GET))))))
	     
	     
	(stop server)))))
      





