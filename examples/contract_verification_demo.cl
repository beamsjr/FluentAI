; ClaudeLang Runtime Contract Verification Demo
; This demonstrates runtime checking of function contracts

; Example 1: Division with precondition
(spec:contract divide
  :requires [(not= y 0)]
  :ensures [(= result (/ x y))]
  :complexity "O(1)")

; Function implementation
(let ((divide (lambda (x y) (/ x y))))
  
  ; Valid call - precondition satisfied
  (print "10 / 2 =" (divide 10 2))
  
  ; This would fail with ContractViolation: Precondition failed
  ; (divide 10 0)
)

; Example 2: Absolute value with postcondition
(spec:contract abs
  :ensures [(>= result 0)
            (or (= result x) (= result (- 0 x)))]
  :pure true)

(let ((abs (lambda (x) 
             (if (< x 0) (- 0 x) x))))
  
  (print "abs(-5) =" (abs -5))
  (print "abs(3) =" (abs 3)))

; Example 3: List operations with type contracts
(spec:contract sum-positive-list
  :requires [(list? lst)
             (all positive? lst)]
  :ensures [(>= result 0)]
  :complexity "O(n)")

(let ((sum-positive-list 
       (lambda (lst)
         (fold + 0 lst))))
  
  ; Valid: all positive numbers
  (print "Sum of [1,2,3,4,5] =" (sum-positive-list [1 2 3 4 5]))
  
  ; This would fail: negative number in list
  ; (sum-positive-list [1 2 -3 4 5])
)

; Example 4: Conditional contracts
(spec:contract safe-reciprocal
  :requires [(number? x)]
  :ensures [(implies (not= x 0) (= result (/ 1 x)))
            (implies (= x 0) (nil? result))])

(let ((safe-reciprocal
       (lambda (x)
         (if (= x 0) nil (/ 1 x)))))
  
  (print "1/2 =" (safe-reciprocal 2))
  (print "1/0 =" (safe-reciprocal 0)))

; Example 5: Contract with invariants
(spec:contract factorial
  :requires [(int? n) (>= n 0)]
  :ensures [(>= result 1)]
  :invariant [(>= n 0)]  ; Maintained during recursion
  :complexity "O(n)")

(let ((factorial
       (lambda (n)
         (if (= n 0)
             1
             (* n (factorial (- n 1)))))))
  
  (print "5! =" (factorial 5))
  
  ; This would fail: negative input
  ; (factorial -1)
)

; Example 6: Demonstrating contract violation handling
(print "\nDemonstrating contract violations:")

; Enable contract checking (default)
(enable-contract-checking!)

; Try division by zero with contract
(spec:contract checked-divide
  :requires [(not= divisor 0)]
  :ensures [(= result (/ dividend divisor))])

(let ((checked-divide
       (lambda (dividend divisor)
         (/ dividend divisor))))
  
  (print "Safe division: 20 / 4 =" (checked-divide 20 4))
  
  ; Catch contract violation
  (try
    (checked-divide 20 0)
    (catch ContractViolation e
      (print "Caught contract violation:" e))))

; Disable contracts for performance
(disable-contract-checking!)

; Now the same call won't check contracts
(print "With contracts disabled, we get arithmetic error instead")

; Example 7: Complex contract with multiple conditions
(spec:contract matrix-multiply-compatible
  :requires [(matrix? A)
             (matrix? B)
             (= (cols A) (rows B))]
  :ensures [(matrix? result)
            (= (rows result) (rows A))
            (= (cols result) (cols B))]
  :complexity "O(n^3)")

; Example 8: Using contracts for optimization
; The compiler can use contract information to:
; - Skip runtime checks (e.g., bounds checking)
; - Apply optimizations (e.g., loop unrolling for fixed-size arrays)
; - Generate better error messages
; - Prove program properties

(print "\nContracts serve multiple purposes:")
(print "1. Documentation - Clear specification of behavior")
(print "2. Testing - Runtime verification during development")
(print "3. Optimization - Compiler can use guarantees")
(print "4. Verification - Formal proofs of correctness")