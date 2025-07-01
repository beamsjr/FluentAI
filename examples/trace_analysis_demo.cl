; ClaudeLang Execution Trace Analysis Demo
; This demonstrates automatic documentation generation from execution traces

; Define some functions with different characteristics
(let ((fibonacci (lambda (n)
                  ; Recursive function - will show up in hot paths
                  (if (<= n 1)
                      n
                      (+ (fibonacci (- n 1))
                         (fibonacci (- n 2))))))
      
      (factorial (lambda (n)
                  ; Tail-recursive with accumulator
                  (let fact-iter ((n n) (acc 1))
                    (if (= n 0)
                        acc
                        (fact-iter (- n 1) (* n acc))))))
      
      (map (lambda (f lst)
             ; Higher-order function
             (if (empty? lst)
                 []
                 (cons (f (head lst))
                       (map f (tail lst))))))
      
      (filter (lambda (pred lst)
               ; Another higher-order function
               (if (empty? lst)
                   []
                   (if (pred (head lst))
                       (cons (head lst) (filter pred (tail lst)))
                       (filter pred (tail lst))))))
      
      (compose (lambda (f g)
                ; Function composition
                (lambda (x) (f (g x)))))
      
      (memoize (lambda (f)
                ; Memoization wrapper
                (let ((cache {}))
                  (lambda (x)
                    (if (contains? cache x)
                        (get cache x)
                        (let ((result (f x)))
                          (do
                            (set! cache x result)
                            result)))))))
      
      (pure-computation (lambda (x y)
                         ; Pure function - no effects
                         (+ (* x x) (* y y))))
      
      (impure-computation (lambda (x y)
                           ; Impure function - has effects
                           (do
                             (effect io print "Computing..." x y)
                             (+ (* x x) (* y y))))))

  ; Execute various patterns to generate interesting traces
  (do
    (print "=== Fibonacci Performance Test ===")
    ; This will show exponential call pattern
    (print "fib(10) =" (fibonacci 10))
    
    (print "\n=== Factorial Tail Recursion ===")
    ; This will show linear call pattern
    (print "10! =" (factorial 10))
    
    (print "\n=== Higher-Order Functions ===")
    ; Data flow between functions
    (let ((numbers [1 2 3 4 5 6 7 8 9 10])
          (square (lambda (x) (* x x)))
          (is-even (lambda (x) (= (mod x 2) 0))))
      
      ; Map shows function application pattern
      (print "Squares:" (map square numbers))
      
      ; Filter shows conditional branching
      (print "Even numbers:" (filter is-even numbers))
      
      ; Composition shows data flow
      (let ((square-then-double (compose (lambda (x) (* x 2)) square)))
        (print "Square then double:" (map square-then-double [1 2 3 4 5]))))
    
    (print "\n=== Pure vs Impure Functions ===")
    ; Effect analysis
    (print "Pure result:" (pure-computation 3 4))
    (print "Impure result:" (impure-computation 3 4))
    
    (print "\n=== Memoization Pattern ===")
    ; Repeated calls with same arguments
    (let ((slow-func (lambda (n)
                       (do
                         (effect io print "Computing slowly for" n)
                         (if (= n 0) 0 (+ n (slow-func (- n 1)))))))
          (fast-func (memoize slow-func)))
      
      ; First calls compute
      (print "First call:" (fast-func 5))
      ; Second call uses cache
      (print "Second call:" (fast-func 5)))
    
    (print "\n=== Branch Prediction Analysis ===")
    ; Heavily biased branches
    (let ((mostly-true (lambda (x) (> x 0)))
          (balanced (lambda (x) (= (mod x 2) 0)))
          (data (map (lambda (x) x) [1 2 3 4 5 6 7 8 9 10 -1])))
      
      ; This branch is mostly true (10/11)
      (print "Mostly positive:" (filter mostly-true data))
      
      ; This branch is balanced (5/10)
      (print "Even numbers:" (filter balanced (tail data))))
    
    (print "\n=== Call Graph Visualization ===")
    ; Complex call relationships
    (let ((a (lambda () (b)))
          (b (lambda () (do (c) (d))))
          (c (lambda () (e)))
          (d (lambda () (e)))
          (e (lambda () "done")))
      
      ; This creates a diamond-shaped call graph
      ;   a
      ;   |
      ;   b
      ;  / \
      ; c   d
      ;  \ /
      ;   e
      (print "Result:" (a)))))

; After running this with a tracing interpreter, you'll get:
; 
; 1. Function Profiles showing:
;    - fibonacci has high call count due to recursion
;    - map and filter are called multiple times
;    - pure-computation has no effects
;    - impure-computation has IO effects
;
; 2. Call Graph showing:
;    - Recursive patterns in fibonacci
;    - Higher-order function relationships
;    - The diamond pattern from a->b->c/d->e
;
; 3. Data Flow Analysis showing:
;    - How data flows through map/filter/compose
;    - Parameter types for each function
;
; 4. Performance Analysis showing:
;    - fibonacci as a bottleneck (exponential calls)
;    - Optimization opportunities for memoization
;
; 5. Branch Analysis showing:
;    - mostly-true has ~91% true rate (optimization opportunity)
;    - balanced has ~50% true rate (well-balanced)

(print "\n=== Trace Analysis Benefits ===")
(print "1. Automatic performance profiling")
(print "2. Call graph visualization") 
(print "3. Data flow documentation")
(print "4. Effect analysis for purity")
(print "5. Branch prediction insights")
(print "6. Optimization opportunity detection")