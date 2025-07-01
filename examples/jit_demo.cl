; ClaudeLang JIT Compilation Demo
; This demonstrates JIT compilation kicking in for hot functions

; Simple arithmetic function that will be JIT compiled
(define (sum-to-n n)
  (if (= n 0)
      0
      (+ n (sum-to-n (- n 1)))))

; Call it many times to trigger JIT compilation
(print "Testing JIT compilation...")
(print "First call (cold):")
(print (sum-to-n 100))

(print "Warming up function (1000 calls)...")
(let ((i 0))
  (while (< i 1000)
    (sum-to-n 50)
    (set! i (+ i 1))))

(print "Function should now be JIT compiled!")
(print "Hot call:")
(print (sum-to-n 100))

; Test more complex arithmetic
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Warm up factorial
(let ((i 0))
  (while (< i 1000)
    (factorial 10)
    (set! i (+ i 1))))

(print "Factorial of 20 (JIT compiled):")
(print (factorial 20))

; Test comparison operations
(define (max-of-three a b c)
  (if (> a b)
      (if (> a c) a c)
      (if (> b c) b c)))

; Warm up max-of-three
(let ((i 0))
  (while (< i 1000)
    (max-of-three i (+ i 1) (- i 1))
    (set! i (+ i 1))))

(print "Max of (42, 99, 73):")
(print (max-of-three 42 99 73))