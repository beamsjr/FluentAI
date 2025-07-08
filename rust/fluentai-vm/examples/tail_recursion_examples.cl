;; Tail Recursion Examples for FluentAI
;; 
;; This file demonstrates various tail-recursive algorithms
;; that run efficiently in constant stack space.

;; 1. Factorial with accumulator (tail-recursive)
(define factorial-tail
  (letrec ((fact-iter (lambda (n acc)
                        (if (= n 0)
                            acc
                            (fact-iter (- n 1) (* n acc))))))
    (lambda (n) (fact-iter n 1))))

;; Test factorial
(print "Factorial of 10: ")
(print (factorial-tail 10))
(print "\n")

;; 2. Fibonacci with accumulators (tail-recursive)
(define fibonacci-tail
  (letrec ((fib-iter (lambda (n a b)
                       (if (= n 0)
                           a
                           (fib-iter (- n 1) b (+ a b))))))
    (lambda (n) (fib-iter n 0 1))))

;; Test fibonacci
(print "Fibonacci of 20: ")
(print (fibonacci-tail 20))
(print "\n")

;; 3. List sum with accumulator (tail-recursive)
(define sum-list
  (letrec ((sum-iter (lambda (lst acc)
                       (if (null? lst)
                           acc
                           (sum-iter (cdr lst) (+ acc (car lst)))))))
    (lambda (lst) (sum-iter lst 0))))

;; Test list sum
(print "Sum of [1..10]: ")
(print (sum-list '(1 2 3 4 5 6 7 8 9 10)))
(print "\n")

;; 4. List reversal (tail-recursive)
(define reverse-list
  (letrec ((rev-iter (lambda (lst acc)
                       (if (null? lst)
                           acc
                           (rev-iter (cdr lst) (cons (car lst) acc))))))
    (lambda (lst) (rev-iter lst '()))))

;; Test list reversal
(print "Reverse of [1,2,3,4,5]: ")
(print (reverse-list '(1 2 3 4 5)))
(print "\n")

;; 5. List length (tail-recursive)
(define length-tail
  (letrec ((len-iter (lambda (lst acc)
                       (if (null? lst)
                           acc
                           (len-iter (cdr lst) (+ acc 1))))))
    (lambda (lst) (len-iter lst 0))))

;; Test list length
(print "Length of [1..100]: ")
(print (length-tail (letrec ((range (lambda (n acc)
                                      (if (= n 0)
                                          acc
                                          (range (- n 1) (cons n acc))))))
                      (range 100 '()))))
(print "\n")

;; 6. Deep recursion test - would overflow without TCO
(define count-down
  (letrec ((count (lambda (n)
                    (if (= n 0)
                        'done
                        (count (- n 1))))))
    count))

;; Test deep recursion
(print "Counting down from 50000... ")
(print (count-down 50000))
(print "\n")

;; 7. Mutual recursion example (not optimized yet)
(letrec ((is-even? (lambda (n)
                     (if (= n 0)
                         #t
                         (is-odd? (- n 1)))))
         (is-odd? (lambda (n)
                    (if (= n 0)
                        #f
                        (is-even? (- n 1))))))
  (begin
    (print "Is 100 even? ")
    (print (is-even? 100))
    (print "\n")))

;; 8. CPS (Continuation-Passing Style) example
(define factorial-cps
  (letrec ((fact-k (lambda (n k)
                     (if (= n 0)
                         (k 1)
                         (fact-k (- n 1)
                                (lambda (v) (k (* n v))))))))
    (lambda (n) (fact-k n (lambda (x) x)))))

;; Note: CPS with lambda creation is not tail-call optimized
;; but demonstrates the pattern

(print "All tail recursion examples completed successfully!\n")