; Simple pattern matching test

(let ((check-number (lambda (n)
                      (match n
                        (0 "zero")
                        (1 "one") 
                        (_ "other")))))
  (io:print "Testing pattern matching:")
  (io:print (check-number 0))
  (io:print (check-number 1))
  (io:print (check-number 2)))