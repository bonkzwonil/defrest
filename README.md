# defrest   

## Export functions via REST Webservices with hunchentoot in an easy way.

### Copyright (C) 2013 by Mathias Menzel-Nielsen




`defrest` provides a very easy way to expose functions via REST interfaces with the hunchentoot webserver.

Exposing a REST Interface is as simple as:

```lisp
(defrest "/hello" :GET ()
	 "Hello World!")
```

or more sophisticated with embedded Path-Parameters and json response

```lisp
(defrest "/length/{str:.+}" :GET (str)
	(with-output-to-string (*standard-output*) 
	 (cl-json:encode-json (list (cons 'name  str) (cons 'length (length str))))))
```

This will build a dispatcher which will listen to urls with the regexp `"/greet/.+"` , bind the (.+) to the `name` variable , runs the body with it and sends the result.
Path-Parameters are encoded as Template blocks {VARNAME:REGEXP}. defrest will do all the parsing and matching for you.
Additionally the hunchentoot handler environment is available, so you can access `hunchentoot:*request*` and so on.

The defrest body should return a string. All other values are format~a'ed to it.



You can add defined webservices to hunchentoot in two different ways:

1. Let defrest take care of it:

   just add `(create-rest-table-dispatcher)` to the hunchentoot dispatcher. 
   Like this:

    ```lisp
   (push (create-rest-table-dispatcher) hunchentoot:*dispatch-table*)
    ```


   and you are done

2.  Add them by yourself: 
  
    defrest returns a dispatcher which you can chain into hunchentoot.
    However, you will loose the ability to defrest on toplevel, that way...

    ```lisp
    (setq hunchentoot:*dispatch-table*
        (list (defrest "/sqrt/{number:[0-9]+}" :GET (number) (sqrt (parse-integer number)))))

    ```





See minimal-example.lisp  for exactly what its called




