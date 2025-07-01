; Fibonacci sequence in ClaudeLang
; Demonstrates recursion, conditionals, and arithmetic

; Recursive implementation
(let ((fib (lambda (n)
            (if (<= n 1)
                n
                (+ (fib (- n 1))
                   (fib (- n 2)))))))
  ; Calculate 10th Fibonacci number
  (fib 10))

; Alternative iterative implementation
(let ((fib-iter (lambda (n)
                 (let ((loop (lambda (i a b)
                              (if (== i n)
                                  b
                                  (loop (+ i 1) b (+ a b))))))
                   (if (<= n 1)
                       n
                       (loop 1 0 1))))))
  (fib-iter 10))

; List of first 10 Fibonacci numbers
(let ((fib-list (lambda (n)
                 (let ((build (lambda (i acc)
                               (if (== i 0)
                                   acc
                                   (build (- i 1) 
                                         (cons (fib (- i 1)) acc))))))
                   (build n [])))))
  (fib-list 10))