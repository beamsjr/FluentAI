; ClaudeLang Contract Specifications Demo
; This demonstrates how to define formal contracts for functions

; Example 1: Simple arithmetic contract
(spec:contract add
  :requires [(number? x) (number? y)]
  :ensures [(number? result) 
            (= result (+ x y))]
  :complexity "O(1)"
  :pure true)

(define (add x y)
  (+ x y))

; Example 2: Division with precondition
(spec:contract safe-divide
  :pre [(number? x) (number? y) (not= y 0)]
  :post [(= result (/ x y))]
  :complexity "O(1)")

(define (safe-divide x y)
  (/ x y))

; Example 3: List operations with size constraints
(spec:contract list-sum
  :requires [(list? lst)
             (all number? lst)]
  :ensures [(number? result)
            (= result (fold + 0 lst))]
  :complexity "O(n)")

(define (list-sum lst)
  (fold + 0 lst))

; Example 4: Binary search with invariants
(spec:contract binary-search
  :requires [(vector? arr)
             (sorted? arr)
             (comparable? target)]
  :ensures [(or (= result -1)
                (and (>= result 0)
                     (< result (length arr))
                     (= (nth arr result) target)))]
  :invariant [(>= high low)
              (or (< target (nth arr low))
                  (> target (nth arr high))
                  (exists? target arr))]
  :complexity "O(log n)")

(define (binary-search arr target)
  (let ((low 0)
        (high (- (length arr) 1)))
    (loop
      (if (> low high)
          -1
          (let ((mid (/ (+ low high) 2))
                (mid-val (nth arr mid)))
            (cond
              ((= mid-val target) mid)
              ((< mid-val target) (recur (+ mid 1) high))
              (else (recur low (- mid 1)))))))))

; Example 5: Impure function contract
(spec:contract read-config
  :requires [(string? filename)
             (file-exists? filename)]
  :ensures [(or (map? result)
                (error? result))]
  :pure false)

(define (read-config filename)
  (io:read-file filename))

; Example 6: Higher-order function contract
(spec:contract map-with-index
  :requires [(function? f)
             (= (arity f) 2)
             (list? lst)]
  :ensures [(list? result)
            (= (length result) (length lst))]
  :complexity "O(n * f)")

(define (map-with-index f lst)
  (let ((helper (lambda (lst idx acc)
                  (if (empty? lst)
                      (reverse acc)
                      (helper (tail lst) 
                              (+ idx 1)
                              (cons (f (head lst) idx) acc))))))
    (helper lst 0 [])))

; Example 7: Recursive function with decreasing measure
(spec:contract factorial
  :requires [(int? n) (>= n 0)]
  :ensures [(int? result) 
            (>= result 1)
            (= result (product 1..n))]
  :invariant [(decreasing n)]
  :complexity "O(n)")

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Example 8: Data structure invariant
(spec:contract balanced-tree?
  :requires [(tree? t)]
  :ensures [(boolean? result)]
  :invariant [(implies result
                      (forall node in t
                              (<= (abs (- (height (left node))
                                         (height (right node))))
                                  1)))]
  :complexity "O(n)")

; Example 9: Performance contract
(spec:contract quick-sort
  :requires [(list? lst)
             (comparable-elements? lst)]
  :ensures [(list? result)
            (sorted? result)
            (permutation? lst result)]
  :complexity "O(n log n)"  ; Average case
  :worst-case "O(n^2)")

; Example 10: Contract inheritance
(spec:contract sort
  :abstract true
  :requires [(list? lst)]
  :ensures [(sorted? result)
            (permutation? lst result)])

(spec:contract stable-sort
  :extends sort
  :ensures [(stable? result lst)])

; Using contracts for verification and optimization
; The compiler can:
; 1. Check contracts at runtime (development mode)
; 2. Prove contracts at compile time (when possible)
; 3. Use contracts for optimization (e.g., skip bounds checks)
; 4. Generate documentation from contracts
; 5. Create property-based tests from contracts