; ClaudeLang Basic Examples
; This file demonstrates fundamental ClaudeLang concepts

; 1. Simple Arithmetic
; Pure computation with no side effects
(+ 2 3)  ; => 5

; 2. Nested Expressions
; Expressions compose naturally
(* (+ 1 2) (- 5 1))  ; => 12

; 3. Variables with Let
; Let bindings create local scope
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30

; 4. Functions (Lambda)
; First-class functions with explicit parameters
(let ((add-one (lambda (x) (+ x 1))))
  (add-one 5))  ; => 6

; 5. Higher-Order Functions
; Functions can take and return functions
(let ((compose (lambda (f g)
                 (lambda (x) (f (g x)))))
      (inc (lambda (x) (+ x 1)))
      (double (lambda (x) (* x 2))))
  ((compose inc double) 3))  ; => 7 (inc(double(3)) = inc(6) = 7)

; 6. Conditionals
; If expressions with explicit then/else branches
(if (< 5 10)
    "less"
    "greater")  ; => "less"

; 7. Lists and Recursion
; Recursive list processing
(let ((sum-list (lambda (lst)
                  (if (empty? lst)
                      0
                      (+ (head lst) (sum-list (tail lst)))))))
  (sum-list (cons 1 (cons 2 (cons 3 [])))))  ; => 6

; 8. Effects: IO
; Explicit effect handling for IO operations
(do
  (effect :io (print "Hello, ClaudeLang!"))
  (effect :io (print "Effects are explicit")))

; 9. Effects: Error Handling
; Division by zero returns an error effect
(let ((safe-div (lambda (x y)
                  (if (== y 0)
                      (effect :error "Division by zero")
                      (/ x y)))))
  (safe-div 10 2))  ; => 5

; 10. Parallel Computation
; Explicit parallel evaluation
(parallel
  (+ 1 2)
  (* 3 4)
  (- 10 5))  ; => (3, 12, 5)

; 11. Uncertainty
; Probabilistic computation
(uncertain
  (0.7 "likely")
  (0.3 "unlikely"))  ; => "likely" with 70% probability

; 12. Sequential Composition
; Explicit sequencing with 'do'
(do
  (effect :io (print "Step 1"))
  (effect :io (print "Step 2"))
  42)  ; => 42 (after printing)

; 13. Pattern: Map Implementation
; Demonstrating how to build higher-level abstractions
(let ((map (lambda (f lst)
             (if (empty? lst)
                 []
                 (cons (f (head lst))
                       (map f (tail lst)))))))
  (map (lambda (x) (* x 2)) (cons 1 (cons 2 (cons 3 [])))))  ; => [2, 4, 6]

; 14. Temporal Constraints (future feature)
; Operations with time bounds
; (temporal :within-5s
;   (effect :network (fetch "https://api.example.com/data")))

; 15. Type Annotations (when implemented)
; Explicit type declarations for clarity
; (define factorial
;   :type (-> Int Int)
;   :effects #{:pure}
;   (lambda (n)
;     (if (<= n 1)
;         1
;         (* n (factorial (- n 1))))))