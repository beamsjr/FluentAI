; Advanced Effect System Demo for ClaudeLang
;
; Demonstrates the new comprehensive effect handler system

; Simple IO effects
(print "=== Basic Effects ===")
(print "Hello, ClaudeLang!")

; State management with transactions
(print "\n=== State Management ===")

; Initialize some state
(let ((init-state (lambda ()
                    (effect state:set "users" [])
                    (effect state:set "counter" 0))))
  (init-state))

; Add users with state updates
(let ((add-user (lambda (name)
                  (effect state:update "users" 
                    (lambda (users) (append users [name])))
                  (effect state:update "counter"
                    (lambda (c) (+ c 1)))
                  (print (concat "Added user: " name)))))
  (add-user "Alice")
  (add-user "Bob")
  (add-user "Charlie"))

(print "Total users:" (effect state:get "counter"))
(print "User list:" (effect state:get "users"))

; Transaction example
(print "\n=== Transactions ===")
(effect state:begin-transaction)
(effect state:set "counter" 100)
(print "In transaction, counter:" (effect state:get "counter"))
(effect state:rollback-transaction)
(print "After rollback, counter:" (effect state:get "counter"))

; Time-based effects
(print "\n=== Time Effects ===")
(let ((benchmark (lambda (name thunk)
                   (let ((start (effect time:now)))
                     (let ((result (thunk)))
                       (let ((elapsed (- (effect time:now) start)))
                         (print (concat name " took " (to-string elapsed) " seconds"))
                         result))))))
  
  (benchmark "Fibonacci" 
    (lambda () 
      (let ((fib (lambda (n)
                   (if (<= n 1)
                       n
                       (+ (fib (- n 1)) (fib (- n 2)))))))
        (fib 10)))))

; Random number generation
(print "\n=== Random Effects ===")
(let ((dice-roll (lambda ()
                   (+ 1 (effect random:randint 0 5)))))
  (print "Rolling dice 5 times:")
  (map (lambda (i) 
         (print (concat "  Roll " (to-string i) ": " (to-string (dice-roll)))))
       [1 2 3 4 5]))

; Error handling
(print "\n=== Error Handling ===")
(let ((safe-divide (lambda (a b)
                     (if (== b 0)
                         (effect error:raise "divide-by-zero" "Cannot divide by zero" [a b])
                         (/ a b)))))
  
  ; Set up error handler
  (effect error:catch "divide-by-zero"
    (lambda (err) 
      (print "Handled division error!")
      -1))
  
  (print "10 / 2 =" (safe-divide 10 2))
  (print "10 / 0 =" (safe-divide 10 0)))

; Combining multiple effects
(print "\n=== Combined Effects ===")
(let ((log-and-count (lambda (message)
                       (effect state:update "log-count" 
                         (lambda (c) (if c (+ c 1) 1)))
                       (let ((count (effect state:get "log-count")))
                         (print (concat "[" (to-string count) "] " message))
                         count))))
  
  (log-and-count "First message")
  (log-and-count "Second message")
  (log-and-count "Third message"))

; Pure computation example
(print "\n=== Pure vs Effectful ===")
(let ((pure-factorial (lambda (n)
                        (if (<= n 1)
                            1
                            (* n (pure-factorial (- n 1))))))
      
      (logged-factorial (lambda (n)
                          (print (concat "Computing factorial of " (to-string n)))
                          (if (<= n 1)
                              1
                              (* n (logged-factorial (- n 1)))))))
  
  (print "Pure factorial(5):" (pure-factorial 5))
  (print "Logged factorial(5):")
  (logged-factorial 5))

(print "\n=== Effect System Demo Complete ===")