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
(test testuriparse-with-query-params ()
  (multiple-value-bind (result query) (defrest::parse-uri "/test/{id:[0-5]}{name:.+}" "/test/3A%20B?nickname=Freako")
    (is (equal "A B"
	       (gethash "name" result)))
    (is (equal "3"
	       (gethash "id" result)))
    (is (= 2
	   (hash-table-count result)))
    (is (= 1
	   (length query)))
    (is (string= "nickname" (car (first query))))
    (is (string= "Freako" (cdr (first query))))))

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

	   (defrest "/greetquery/{name:.+}" :GET (name :query nickname age)
	     (format nil "Hello ~a! Your nickname is ~a and your age is ~a" name nickname age))

	   (defrest "/greetqueryonly" :GET (:query name)
	     (format nil "Hello ~a" name))
	   
	   (defrest "/greetquerymandatory/{name:.+}" :GET (name :query (nickname :mandatory t))
	     (format nil "Hello ~a! Your nickname is ~a" name nickname))

	   (defrest "/greetquerydefault/{name:.+}" :GET (name :query nickname (age :default "21"))
	     (format nil "Hello ~a! Your nickname is ~a and your age is ~a" name nickname age))

	   (defrest "/greetquerypattern/{name:.+}" :GET (name :query nickname (age :pattern "[0-9]+"))
	     (format nil "Hello ~a! Your nickname is ~a and your age is ~a" name nickname age))

	   (defrest "/greetqueryparam/{name:.+}" :GET (name :query nickname (age :param "queryage" :pattern "[0-9]+"))
	     (format nil "Hello ~a! Your nickname is ~a and your age is ~a" name nickname age))

	   (defrest "/post/{number:[0-9]+}.data" :POST (number)
	     (sqrt (parse-integer number)))

	   (is (equal "Hello World"
		      (drakma:http-request (url "/simple"))))
	   (is (equal "Hello Bonk"
		      (drakma:http-request (url "/greet/Bonk"))))
	   (is (equal "Hello Bonk"
		      (drakma:http-request (url "/greetqueryonly?name=Bonk"))))
	   (is (equal "Hello Bonk! Your nickname is Freako and your age is NIL"
		      (drakma:http-request (url "/greetquery/Bonk?nickname=Freako"))))
	   (is (equal "Hello Bonk! Your nickname is Freako and your age is 21"
		      (drakma:http-request (url "/greetquery/Bonk?nickname=Freako&age=21"))))

	   (is (equal 400
		      (second (multiple-value-list(drakma:http-request (url "/greetquerymandatory/Bonk"))))))

	   (is (equal "Hello Bonk! Your nickname is Freako and your age is 21"
		      (drakma:http-request (url "/greetquerypattern/Bonk?nickname=Freako&age=21"))))
	   
	   (is (equal 400
		      (second (multiple-value-list(drakma:http-request (url "/greetquerypattern/Bonk?age=ABCD"))))))
	   
	   (is (equal "Hello Bonk! Your nickname is Freako and your age is 21"
		      (drakma:http-request (url "/greetquerydefault/Bonk?nickname=Freako"))))

	   (is (equal "Hello Bonk! Your nickname is Freako and your age is 21"
		      (drakma:http-request (url "/greetqueryparam/Bonk?nickname=Freako&queryage=21"))))

	   (is (equal 400
		      (second (multiple-value-list(drakma:http-request (url "/greetqueryparam/Bonk?queryage=ABCD"))))))
	   
	   (is (equal "Hello Bonk"
		      (drakma:http-request (url "/greet/Bonk?nickname=Freako"))))
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
      





