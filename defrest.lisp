(defpackage defrest (:use :cl :hunchentoot :cl-ppcre :split-sequence) (:nicknames rest))

(in-package :rest)

(defun preparse-uri-parameters->map (schema)
  "Turns st. like 'bla/{id:[0-9]+}/{name:.+}' in a parameter->regexpmap hashtable"
  (let ((map (make-hash-table)))
    (do-matches-as-strings (match "{([^\{]+):([^\}]+)}*" schema) 
      (multiple-value-bind (n/a found)
	  (scan-to-strings "{(.+):(.+)}" match)
	(setf (gethash (aref found 0) map) (aref found 1))))
    map))

(defun preparse-uri-parameters->list (schema)
  "Turns st. like 'bla/{id:[0-9]+}/{name:.+}' in a parameter->regexpmap list"
  (let ((lst))
    (do-matches-as-strings (match "{([^\{]+):([^\}]+)}*" schema) 
      (multiple-value-bind (n/a found)
	  (scan-to-strings "{(.+):(.+)}" match)
	(push (list :key (aref found 0) :regexp (aref found 1)) lst)))
    (reverse lst)))

(defun schema->regexpurl (schema)
  "rips out the template blocks and replaces them with their regexp part. eg: {id:[0-9]+} becomes [0-9]+"
  (let ((parameters (preparse-uri-parameters->list schema))
	(cont?))
    (loop do
	 (multiple-value-bind (result found?) 
	     (regex-replace "{([^\{]+):([^\}]+)}" schema (getf (pop parameters) :regexp))
	   (setf cont? found?)
	   (setf schema result))
	 
	   while cont?))
  schema)
      

(defun split-on-placeholders (uri)
  "splits a sequence on the template placeholder blocks"
  (let ((lst)
	(split-symbol #\U00046A76)) ;;could be anything
    (split-sequence split-symbol 
		    (regex-replace-all "{[^\{]+}" uri (format nil "~c" split-symbol)))))



(defun parse-schema (schema)
  (let ((parameters (preparse-uri-parameters->list schema))
	(holders (split-on-placeholders schema))
	(result))
    (loop for i in holders with n = -1 do
	 (push i result)
	 (push (nth (incf n) parameters) result))
    (remove-if #'null (nreverse result))))

(defun parse-uri (schema uri)
  (when (not 
	 (scan 
	  (schema->regexpurl schema)
	  uri))
    (error "Uri does not match schema"))
  (let ((parsed (parse-schema schema))
	(map (make-hash-table :test #'equalp)))
	
    (loop for token in parsed do
	 (if (listp token)
	     (let ((regexp (getf token :regexp))
		   (key (getf token :key)))
	       (multiple-value-bind (start end) (scan regexp uri)
		 (setf (gethash key map) (subseq uri start end))
		 (setf uri (subseq uri end))))
	     (multiple-value-bind (start end) (scan token uri)  ;;else
	       (setf uri (subseq uri end)))))
    map))
  

    
(defun create-rest-dispatcher (schema method fun)
  "Creates a hunchentoot compatible dispatcher for a given url SCHEMA and request METHOD which will call the FUN function on match and hands over a parameter map hashtable"
  (let* ((uri (schema->regexpurl schema))
  	 (dispatcher 
	  (create-regex-dispatcher uri 
	   #'(lambda ()
	      (let ((reqmethod (request-method *request*))
		    (parameters (parse-uri schema (request-uri *request*))))
		(when (equal reqmethod 
			     method)
		  (let ((result
			 (funcall fun parameters)))
		    (if (stringp result)
			result
			(format nil "~a" result)))))))))
    dispatcher))
    

;; A global rest table to be able to defrest on toplevel
(defvar *rest-dispatcher-table* (make-hash-table :test 'equal))



(defmacro defrest (pattern method varlist &body body)
  "Defines a new REST Method. It will listen to urls which match pattern, have all template blocks replaced and binds variables to varlist.
   
   Returns a dispatcher method, which can be put in the hunchentoot dispatcher table AND creates an entry in the *rest-dispatcher-table* to be able to defrest on toplevel.

   Usage Example:

   (defrest \"/greet/{name:.+}\" :GET (name) 
          (format nil \"Hello  ~a\" name))

   will create a Hello World Dispatcher which will greet GET /greet/Bonk with 'Hello Bonk'"
  (let ((letlist (loop for var in varlist collect `(,var (gethash (symbol-name (quote ,var)) map)))))
    `(setf (gethash ,pattern *rest-dispatcher-table*)
	  (create-rest-dispatcher ,pattern ,method 
				   (lambda (map)
				     (let ,letlist
				       ,@body))))))
			     


(defun create-rest-table-dispatcher (&optional (table *rest-dispatcher-table*))
  "builds a rest table dispatcher which can be added to the hunchentoot dispatchers to handle all defrest'ed functions"
  #'(lambda (request)
      (loop for dispatcher being the hash-values of table do
	   (let ((fun (funcall dispatcher request)))
	     (when fun (return fun))))))
    
