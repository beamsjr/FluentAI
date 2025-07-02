;; Test fast local variable access opcodes
;; The first 4 locals (indices 0-3) should use fast opcodes

;; Test LoadLocal0
(let ((a 10))
  a)

;; Test LoadLocal0 and LoadLocal1
(let ((a 10) (b 20))
  (+ a b))

;; Test LoadLocal0, LoadLocal1, LoadLocal2
(let ((a 10) (b 20) (c 30))
  (+ (+ a b) c))

;; Test LoadLocal0, LoadLocal1, LoadLocal2, LoadLocal3
(let ((a 10) (b 20) (c 30) (d 40))
  (+ (+ (+ a b) c) d))

;; Test mix of fast and regular locals
(let ((a 1) (b 2) (c 3) (d 4) (e 5))
  (+ (+ (+ (+ a b) c) d) e))

;; Test in lambda
((lambda (x y z w)
   (+ (+ (+ x y) z) w))
 10 20 30 40)