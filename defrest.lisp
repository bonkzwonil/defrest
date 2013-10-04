;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;; Copyright (c) 2013, Mathias Menzel-Nielsen
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 

;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; The views and conclusions contained in the software and documentation are those
;; of the authors and should not be interpreted as representing official policies, 
;; either expressed or implied, of the FreeBSD Project.

(in-package :cl-user)

(defpackage defrest 
  (:use :cl :hunchentoot :cl-ppcre :split-sequence) 
  (:nicknames rest)
  (:export defrest create-rest-table-dispatcher undefrest))

(in-package :rest)

#|
(defun preparse-uri-parameters->map (schema)
  "Turns st. like 'bla/{id:[0-9]+}/{name:.+}' in a parameter->regexpmap hashtable"
  (let ((map (make-hash-table)))
    (do-matches-as-strings (match "{([^\{]+):([^\}]+)}*" schema) 
      (multiple-value-bind (n/a found)
	  (scan-to-strings "{(.+):(.+)}" match)
	(declare (ignore n/a))
	(setf (gethash (aref found 0) map) (aref found 1))))
    map))
|#

(defun preparse-uri-parameters->list (schema)
  "Turns st. like 'bla/{id:[0-9]+}/{name:.+}' in a parameter->regexpmap list"
  (let ((lst))
    (do-matches-as-strings (match "{([^\{]+):([^\}]+)}*" schema) 
      (multiple-value-bind (n/a found)
	  (scan-to-strings "{(.+):(.+)}" match)
	(declare (ignore n/a))
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
  (let ((split-symbol #\U00046A76)) ;;could be anything
    (split-sequence split-symbol 
		    (regex-replace-all "{[^\{]+}" uri (format nil "~c" split-symbol)))))



(defun parse-schema (schema)
  (let ((parameters (preparse-uri-parameters->list schema))
	(holders (split-on-placeholders schema))
	(result))
    (loop for i in holders with n = -1 do
	 (push i result)
	 (push (nth (incf n) parameters) result))
    (remove-if #'(lambda (x)
		   (or (null x)
		       (= 0 (length x))))
	       (nreverse result))))

(defun parse-uri (schema uri)
  "Parses URI against SCHEMA and returns a hashtable with all pathvariable bindings"
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
		 (setf (gethash key map) (hunchentoot:url-decode (subseq uri start end)))
		 (setf uri (subseq uri end))))
	     (multiple-value-bind (start end) (scan token uri)  ;;else
	       (declare (ignore start))
	       (setf uri (subseq uri end)))))
    map))
  

    
(defun create-rest-dispatcher (schema method fun)
  "Creates a hunchentoot compatible dispatcher for a given url SCHEMA and request METHOD which will call the FUN function on match and hands over a parameter map hashtable"
  (let* ((uri (schema->regexpurl schema)))
    #'(lambda (request) ;return a dispatcher...
	(when (and
	       (equal method
		      (request-method request))
	       (scan uri (request-uri request)))
	  #'(lambda () ;... which returns a handler fun on match (or nil
	      
	      (let ((reqmethod (request-method *request*))
		    (parameters (parse-uri schema (request-uri *request*))))
		(when (equal reqmethod 
			     method)
		  (let ((result
			 (funcall fun parameters)))
		    (if (stringp result)
			result
			(format nil "~a" result))))))))))
    

;; A global rest table to be able to defrest on toplevel
(defvar *rest-dispatcher-table* (make-hash-table :test 'equal))



(defmacro defrest (pattern method varlist &body body)
  "Defines a new REST Method. It will listen to urls which match pattern, 
   have all template blocks replaced and binds variables to varlist.   
   Returns a dispatcher method, which can be put in the hunchentoot dispatcher 
   table AND creates an entry in the *rest-dispatcher-table* to be able to defrest 
   on toplevel.

   Usage Example:

   (defrest \"/greet/{name:.+}\" :GET (name) 
          (format nil \"Hello  ~a\" name))

   will create a Hello World Dispatcher which will greet GET /greet/Bonk with 'Hello Bonk'"

  (let ((letlist (mapcar #'(lambda (var)
			     `(,var (gethash (symbol-name (quote ,var)) map)))
			 varlist)))
    `(setf (gethash (cons ,method ,pattern) *rest-dispatcher-table*)
	   (create-rest-dispatcher ,pattern ,method 
				   (lambda (map)
				     (declare (ignorable map)) ; we dont want a not-used warning on empty lambda-list defrest's
				     (let ,letlist
				       ,@body))))))
			     


(defun undefrest (pattern)
  "Removes the rest service with the exact PATTERN from the rest-table-dispatcher"
  (remhash pattern *rest-dispatcher-table*))

(defun create-rest-table-dispatcher (&optional (table *rest-dispatcher-table*))
  "builds a rest table dispatcher which can be added to the hunchentoot dispatchers 
   to handle all defrest'ed functions"
  #'(lambda (request)
      (loop for dispatcher being the hash-values of table do
	   (let ((fun (funcall dispatcher request)))
	     (when fun (return fun))))))
    
