; Pattern matching in function
(let ((f (lambda (x) 
           (match x
             (0 "zero")
             (1 "one")
             (_ "other")))))
  (io:print (f 0))
  (io:print (f 1))
  (io:print (f 2)))