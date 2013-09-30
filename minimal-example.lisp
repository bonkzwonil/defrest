;; Simpliest runnable defrest example i could think of
(in-package :defrest)

(defvar *server* (start (make-instance 'easy-acceptor  :port 8000)))

(setq *dispatch-table* 
      (list
       (create-rest-table-dispatcher)))

(defrest "/greet/{name:.+}" :GET (name)
  (concatenate 'string "Hallo " name))






