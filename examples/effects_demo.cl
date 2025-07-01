; Effect system demonstration
; Shows how ClaudeLang tracks and manages side effects

; Pure functions can be optimized aggressively
(let ((pure-double (lambda (x) (* x 2))))
  ; This can be evaluated at compile time
  (pure-double 21))

; IO effects must be preserved
(let ((greet (lambda (name)
              (effect io:print (concat "Hello, " name)))))
  ; This cannot be optimized away
  (greet "World"))

; State effects for mutable operations
(let ((counter (effect state:new 0)))
  (effect state:update counter (lambda (x) (+ x 1)))
  (effect state:update counter (lambda (x) (+ x 1)))
  (effect state:get counter))  ; Returns 2

; Error effects for exception handling
(let ((safe-divide (lambda (a b)
                    (if (== b 0)
                        (effect error:raise "Division by zero")
                        (/ a b)))))
  ; Handler for errors
  (effect error:handle
    (safe-divide 10 0)
    (lambda (err) -1)))  ; Returns -1 instead of crashing

; Time effects for temporal operations
(let ((timed-computation (lambda ()
                          (effect time:measure
                            (fold + 0 (range 1 1000))))))
  (timed-computation))  ; Returns [result, time_taken]

; Network effects (conceptual)
(let ((fetch-data (lambda (url)
                   (effect network:get url))))
  ; This would make an HTTP request
  ; (fetch-data "https://api.example.com/data")
  "Network effect example")

; Random effects for non-deterministic operations
(let ((random-between (lambda (min max)
                       (+ min (effect random:int (- max min))))))
  ; Generate random number between 1 and 10
  (random-between 1 10))

; Parallel computation with explicit effects
(let ((parallel-sum (lambda (lst)
                     (effect parallel:map
                       (lambda (x) (* x x))
                       lst))))
  ; Square each element in parallel
  (parallel-sum [1 2 3 4 5]))

; Effect composition
(let ((logged-computation (lambda (x)
                           (effect io:print (concat "Computing with: " (to-string x)))
                           (let ((result (* x x)))
                             (effect io:print (concat "Result: " (to-string result)))
                             result))))
  (logged-computation 7))

; Pure vs impure optimization example
(let ((pure-sum (lambda (lst) (fold + 0 lst)))
      (impure-sum (lambda (lst)
                   (effect io:print "Starting sum...")
                   (let ((result (fold + 0 lst)))
                     (effect io:print (concat "Sum is: " (to-string result)))
                     result))))
  ; pure-sum can be optimized if lst is constant
  ; impure-sum cannot be optimized away due to IO effects
  [(pure-sum [1 2 3 4 5])      ; Can be computed at compile time
   (impure-sum [1 2 3 4 5])])  ; Must be executed at runtime