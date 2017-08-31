# defrest   

## Export functions via REST Webservices with hunchentoot in an easy way.







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

This will build a dispatcher which will listen to urls with the regexp `"/length/.+"` , bind the (.+) to the `str` variable , runs the body with it and sends the result.

Path-Parameters are encoded as Template blocks {VARNAME:REGEXP}. defrest will do all the parsing and matching for you.
Additionally the hunchentoot handler environment is available, so you can access `hunchentoot:*request*` and so on.

`defrest` also supports query parameters:

```lisp
(defrest "/songs/{album-id:[0-9]+}" :GET (album-id :query offset limit)
        (get-songs album-id offset limit))
```

Query-Parameters are separated from the path-parameters by the keyword ```:query```. They are not used by the route matching
algorithm. Query-Parameters can be declared as simple variable names (where the names represents the names of the query parameters of the current request) or as lambda-lists with the following format:

query-param -- (binding &key (mandatory nil) (pattern nil) (param nil) (default nil))

* _binding_ -- Name of the variable bound to the value of the query-parameter in the execution context of the body.
* _mandatory_ -- A generalized boolean that indicates if the query-parameter is mandatory. If a query-parameter
    is mandatory but not provided by the current request the HTTP status code 400 (Bad Request) is returned.
    If a query-parameter is not mandatory and not provided by the current request it gets the value NIL (if
    no default value has been declared).
* _pattern_ A regexp that the query parameter must match. If the query parameter does not match the pattern,
    the HTTP status code 400 (Bad Request) is returned.
* _param_ Name of the query-parameter as provided by the current request. The default value is the value of _binding_.
* _default_ Default value of the query parameter. 

Query-Parameter examples:

```lisp
(defrest "/songs/{album-id:[0-9]+}" :GET (album-id :query (offset :default "0") limit)
        (get-songs album-id offset limit))
```

```lisp
(defrest "/songs/{album-id:[0-9]+}" :GET (album-id :query (offset :default "0") (limit :mandatory t))
        (get-songs album-id offset limit))
```

```lisp
(defrest "/songs/{album-id:[0-9]+}" :GET (album-id :query (offset :default "0") 
    (limit :mandatory t :pattern "[0-9]+"))
        (get-songs album-id offset limit))
```

```lisp
(defrest "/songs/{album-id:[0-9]+}" :GET (album-id :query (offset :default "0") 
    (count :mandatory t :param "limit"))
        (get-songs album-id offset count))
```



The defrest body should return a string. All other values are format~a'ed to it.


The Method can be replaced with `:PUT :POST :DELETE` etcpp


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




## Tests

The Test suite can be run automatically by asdf:

```lisp
(asdf:oos 'asdf:test-op 'defrest)
```



See minimal-example.lisp  for exactly what its called


defrest was called defajax 


 Copyright (C) 2013 by Mathias Menzel-Nielsen




