; ClaudeLang Optimization Proof Demo
; This shows how the proof system validates optimizations

; Example 1: Constant folding with proof
(spec:contract compute-area
  :requires [(> width 0) (> height 0)]
  :ensures [(= result (* width height))]
  :pure true)

; Original code
(let ((compute-area-original (lambda (width height)
                               (* width height))))
  ; If the optimizer sees a call like (compute-area 10 20)
  ; it can fold it to 200 and generate a proof:
  ; 
  ; Theorem: (compute-area 10 20) = 200
  ; Proof:
  ;   (compute-area 10 20)
  ;   = (* 10 20)           [beta reduction]
  ;   = 200                 [arithmetic]
  ; QED
  
  (print "Original:" (compute-area-original 10 20)))

; Example 2: Algebraic simplification with proof
(spec:contract algebraic-expression
  :ensures [(= result (+ (* 2 x) (* 3 x)))]
  :pure true)

; Original: 2*x + 3*x
; Optimized: 5*x
(let ((algebraic-original (lambda (x) (+ (* 2 x) (* 3 x))))
      (algebraic-optimized (lambda (x) (* 5 x))))
  ; Proof of equivalence:
  ;   2*x + 3*x
  ;   = x*2 + x*3         [commutativity]
  ;   = x*(2 + 3)         [distributivity]
  ;   = x*5               [arithmetic]
  ;   = 5*x               [commutativity]
  
  (print "Original:" (algebraic-original 7))
  (print "Optimized:" (algebraic-optimized 7)))

; Example 3: Dead code elimination with effect analysis
(spec:contract no-side-effects
  :ensures [(= result x)]
  :pure true)

; Original has dead code
(let ((with-dead-code (lambda (x)
                        (let ((unused (+ 1 2 3 4 5)))
                          x))))
  ; Proof that dead code can be eliminated:
  ; 1. (+ 1 2 3 4 5) is pure (no effects)
  ; 2. 'unused' is not referenced in body
  ; 3. Therefore can be eliminated
  ; 4. Result is semantically equivalent
  
  (print "Result:" (with-dead-code 42)))

; Example 4: Loop unrolling with invariant preservation
(spec:contract sum-list
  :requires [(list? lst) (all number? lst)]
  :ensures [(= result (fold + 0 lst))]
  :invariant [(number? accumulator)])

(let ((sum-list-loop (lambda (lst)
                       (let loop ((lst lst) (acc 0))
                         (if (empty? lst)
                             acc
                             (loop (tail lst) (+ acc (head lst))))))))
  ; For a known list [1,2,3], optimizer can unroll:
  ; (sum-list [1,2,3])
  ; = (+ 0 1 2 3)
  ; = 6
  ; 
  ; Proof of invariant preservation at each step:
  ; Initially: acc = 0 (number? ✓)
  ; Step 1: acc = 0 + 1 = 1 (number? ✓)
  ; Step 2: acc = 1 + 2 = 3 (number? ✓)
  ; Step 3: acc = 3 + 3 = 6 (number? ✓)
  
  (print "Sum:" (sum-list-loop [1 2 3 4 5])))

; Example 5: Common subexpression elimination
(spec:contract cse-example
  :ensures [(= result (+ (* x x) (* x x) 1))]
  :pure true)

(let ((cse-original (lambda (x)
                      (+ (* x x) (* x x) 1)))
      (cse-optimized (lambda (x)
                       (let ((x-squared (* x x)))
                         (+ x-squared x-squared 1)))))
  ; Proof of equivalence:
  ; Let t = (* x x)
  ; Original: (+ t t 1)
  ; Optimized: (let ((x-squared t)) (+ x-squared x-squared 1))
  ; By substitution: (+ t t 1)
  ; Therefore equivalent ✓
  
  (print "Original:" (cse-original 5))
  (print "Optimized:" (cse-optimized 5)))

; Example 6: Tail call optimization correctness
(spec:contract factorial
  :requires [(>= n 0)]
  :ensures [(if (= n 0) 
                (= result 1)
                (= result (* n (factorial (- n 1)))))]
  :invariant [(>= n 0)])

(let ((factorial-recursive (lambda (n)
                             (if (= n 0)
                                 1
                                 (* n (factorial-recursive (- n 1))))))
      (factorial-tail (lambda (n)
                        (let fact-iter ((n n) (acc 1))
                          (if (= n 0)
                              acc
                              (fact-iter (- n 1) (* n acc)))))))
  ; Proof of equivalence by induction:
  ; Base case: n=0
  ;   recursive: 1
  ;   tail: acc=1
  ;   Equal ✓
  ; 
  ; Inductive step: assume true for k, prove for k+1
  ;   recursive: (k+1) * factorial(k) = (k+1) * k!
  ;   tail: fact-iter(k+1, 1) = fact-iter(k, k+1) = ... = (k+1)!
  ;   Equal ✓
  
  (print "5! =" (factorial-tail 5)))

; Example 7: Strength reduction
(spec:contract power-of-two
  :requires [(>= n 0)]
  :ensures [(= result (expt 2 n))]
  :pure true)

(let ((power-multiply (lambda (n)
                        (if (= n 0)
                            1
                            (* 2 (power-multiply (- n 1))))))
      (power-shift (lambda (n)
                     (arithmetic-shift 1 n))))  ; Assuming we have bit-shift
  ; Proof that left-shift by n equals 2^n:
  ; By induction on n
  ; Base: shift(1, 0) = 1 = 2^0 ✓
  ; Step: shift(1, k+1) = 2 * shift(1, k) = 2 * 2^k = 2^(k+1) ✓
  
  (print "2^10 =" (power-multiply 10)))

(print "\nOptimization proofs ensure:")
(print "1. Semantic preservation")
(print "2. Contract preservation") 
(print "3. Effect preservation")
(print "4. Performance improvement validation")
(print "5. Bug-free transformations")