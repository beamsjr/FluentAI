; ClaudeLang JIT Guard-based Specialization Demo
; This demonstrates how the JIT compiler creates specialized versions
; of functions based on observed runtime types

; Example 1: Monomorphic function (always receives same types)
(define (int-sum x y)
  (+ x y))

(print "Testing monomorphic specialization...")
(print "Warming up int-sum with integer arguments...")

; Call many times with integers - JIT will specialize for int+int
(let ((total 0))
  (dotimes (i 1000)
    (set! total (int-sum i (+ i 1))))
  (print "Sum total:" total))

; The JIT has now created a specialized version that:
; - Skips type checking
; - Uses direct integer addition
; - Has no overflow checks

; Example 2: Constant specialization
(define (multiply-by-constant x)
  (* x 10))

(print "\nTesting constant specialization...")

; Call many times with the same constant multiplier
(let ((results []))
  (dotimes (i 1000)
    (push! results (multiply-by-constant i)))
  (print "Last result:" (last results)))

; The JIT recognizes that one operand is always 10 and can:
; - Replace multiplication with shifts and adds
; - Inline the constant directly

; Example 3: Range-based optimization
(define (safe-mod x n)
  (if (> n 0)
      (mod x n)
      0))

(print "\nTesting range-based optimization...")

; Always call with positive n values
(let ((sum 0))
  (dotimes (i 1000)
    (set! sum (+ sum (safe-mod i 7))))
  (print "Modulo sum:" sum))

; The JIT learns that n is always positive and can:
; - Remove the zero check
; - Use optimized modulo operation

; Example 4: Shape specialization for lists
(define (dot-product-3d v1 v2)
  (if (and (= (length v1) 3) (= (length v2) 3))
      (+ (* (nth v1 0) (nth v2 0))
         (* (nth v1 1) (nth v2 1))
         (* (nth v1 2) (nth v2 2)))
      0))

(print "\nTesting shape specialization...")

; Always call with 3-element vectors
(let ((v1 [1 2 3])
      (v2 [4 5 6])
      (result 0))
  (dotimes (i 1000)
    (set! result (dot-product-3d v1 v2)))
  (print "Dot product:" result))

; The JIT learns that inputs are always length 3 and can:
; - Remove length checks
; - Unroll the loop completely
; - Access elements directly without bounds checks

; Example 5: Polymorphic specialization
(define (to-string x)
  (cond
    ((int? x) (int->string x))
    ((string? x) x)
    ((list? x) (list->string x))
    (else "unknown")))

(print "\nTesting polymorphic specialization...")

; Call with mix of integers and strings
(let ((results []))
  (dotimes (i 500)
    (push! results (to-string i)))
  (dotimes (i 500)
    (push! results (to-string "hello")))
  (print "Converted" (length results) "values"))

; The JIT may create multiple specializations:
; - One for integer inputs
; - One for string inputs
; Each avoiding unnecessary type checks

(print "\nJIT specialization complete!")
(print "Run with JIT statistics enabled to see specialization details.")