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
  (:export defrest create-rest-table-dispatcher undefrest start easy-acceptor))

(in-package :rest)


(defun schema->regexpurl (schema)
  "rips out the template blocks and replaces them with their regexp part. eg: {id:[0-9]+} becomes [0-9]+"
  (regex-replace-all "{([^\{]+):([^\}]+)}" schema "\\2"))

#|
(defun split-template-blocks (uri)
  "splits an uri to seperate the template placeholder blocks so that '/bla/{var:.+}' becomes ('/bla/' '{var:.+}'). loop version"
  (loop for char across uri
       with result
       with start = 0
       for pos from 0
       do
       (if (and (equal char #\{) (> pos start))
	   (push (subseq uri start (setf start pos)) result)
	   (when (equal char #\})
	     (push (subseq uri start (setf start (1+ pos))) result)))
     finally 
       (return (nreverse result))))

|#


(defun split-sequence-on-positions (seq poslist)
  "Splits SEQ on all positions listed in POSLIST"
  (let ((poslist (sort poslist #'<))) ;needs to be sorted
    (unless (member (length seq) poslist)
      (setf poslist (append poslist (list (length seq)))))  ; and we need a finishing move
    (loop for pos in poslist  ; now its simple
       with start = 0
       collect (subseq seq start (setf start pos)))))

(defun mark-template-splitpoints (schema)
  "Returns a list of all split positions to seperate templateblocks in SCHEMA."
  (remove-duplicates ;nah, we dont want empty strings
    (loop for char across schema
     for pos from 0
       when (member char '(#\{ #\}))
     collect (+ pos
		(if (eq char #\}) 1 0)))))

(defun split-template-blocks (uri)
  "splits an template uri to seperate the template placeholder blocks so that '/bla/{var:.+}' becomes ('/bla/' '{var:.+}'). best version"
  (split-sequence-on-positions uri (mark-template-splitpoints uri)))


(defun parse-schema  (schema)
  "splits a schema into blocks representing it's static parts and it's placeholders.

Example 1:
'/test/{id:[a-z]+}' => ('/test/' (:KEY 'id' :REGEXP '[a-z]+'))

Example 2:
'/test/{id:[a-z]?[0-9]+}' => ('/test/' (:KEY 'id' :REGEXP '[a-z]?[0-9]+'))

Example 3:
'/album/{album:[a-z]+}/track/{track:[0-9]+}' => ('/album/' (:KEY 'album' :REGEXP '[a-z]+') '/track/' (:KEY 'track' :REGEXP '[0-9]+'))

Example 4:
'/album/{album:[a-z]+}/track/{vol:[0-9]+}-{pos:[0-9]+}' => ('/album/' (:KEY 'album' :REGEXP '[a-z]+') '/track/'
 (:KEY 'vol' :REGEXP '[0-9]+') '-' (:KEY 'pos' :REGEXP '[0-9]+'))
"
  (mapcar #'(lambda (x)
	      (if (scan "{.+:.+}" x)
		  (multiple-value-bind (n/a found)
		      (scan-to-strings "{(.+):(.+)}" x)
		    (declare (ignorable n/a))
		    (list :key (aref found 0) :regexp (aref found 1)))
		  x))
	  (split-template-blocks schema))) 	
 
	   

(Defun parse-uri (schema uri)
  "Parses URI against SCHEMA and returns a hashtable with all pathvariable bindings"
  (let ((parsed-uri (quri:uri (hunchentoot:url-decode uri))))
    (setf uri (quri:uri-path parsed-uri))
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
	       (declare (ignore start))
	       (setf uri (subseq uri end)))))
    map)))
  

    
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
    
