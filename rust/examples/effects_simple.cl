;; Simple Effects Demo
;;
;; Basic effects that work with current implementation

;; Basic printing (if print is available in stdlib)
(print "=== FluentAI Simple Effects Demo ===")
(print "This demonstrates basic operations")

;; Simple arithmetic
(define square (lambda (x) (* x x)))
(define result (+ (square 3) (square 4)))
(print result)  ; 25

;; List operations
(define numbers (list 1 2 3 4 5))
(define first-num (car numbers))
(print first-num)  ; 1

;; Simple recursion
(define count-down
  (lambda (n)
    (if (= n 0)
        "done"
        (begin
          (print n)
          (count-down (- n 1))))))

(count-down 3)
; Prints: 3, 2, 1, then returns "done"

;; Working with boolean logic
(define check-range
  (lambda (x)
    (and (>= x 0) (<= x 100))))

(print (check-range 50))   ; true
(print (check-range 150))  ; false