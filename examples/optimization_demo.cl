; Optimization demonstration
; Shows how ClaudeLang's optimizer improves performance

; Example 1: Constant folding
; This entire expression will be optimized to just 68
(+ (* 2 3) (* 4 5) (* 6 7))

; Example 2: Dead code elimination
; The optimizer will recognize that z is never used
(let ((x 10)
      (y 20)
      (z 30))  ; This binding will be eliminated
  (+ x y))

; Example 3: Branch elimination
; Since the condition is constant, only the true branch remains
(if (> 10 5)
    (* 2 50)      ; This will be kept and evaluated to 100
    (error "This will be eliminated"))

; Example 4: Complex constant expression
; The entire computation is done at compile time
(let ((a 5)
      (b 10))
  (+ (* a b)
     (- (* b b) (* a a))
     (/ (+ a b) 3)))

; Example 5: Partial evaluation
; Constants are folded even in presence of variables
(let ((x 10))
  (+ x (* 3 4)))  ; (* 3 4) becomes 12 at compile time

; Example 6: Nested conditionals with constant conditions
; Both conditions are constant, so this reduces to just 42
(if (and (> 10 5) (< 3 7))
    (if (== (+ 2 2) 4)
        42
        0)
    -1)

; Example 7: List operations that can be optimized
; Length of a constant list is computed at compile time
(+ (length [1 2 3 4 5])     ; Optimized to 5
   (length [6 7 8 9 10]))   ; Optimized to 5
; Result: 10

; Example 8: String operations
; Constant string operations are evaluated at compile time
(concat (string-upcase "hello")
        (concat ", " 
                (string-downcase "WORLD")))
; Result: "HELLO, world"

; Example 9: Type specialization opportunity
; The type inferencer recognizes all values are integers
; allowing use of specialized ADD_INT instruction
(let ((sum (lambda (a b c d)
            (+ (+ a b) (+ c d)))))
  (sum 1 2 3 4))

; Example 10: Demonstrating optimization limits
; This won't be fully optimized because it depends on runtime input
(let ((compute (lambda (x)
                (+ (* x x) (* 2 x) 1))))
  ; But if we call it with a constant...
  (compute 5))  ; This WILL be optimized to 36