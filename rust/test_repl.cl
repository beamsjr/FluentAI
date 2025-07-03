;; Test basic arithmetic
(+ 1 2)
(* 3 4)
(- 10 3)
(/ 20 4)

;; Test comparisons
(> 5 3)
(< 2 4)
(= 3 3)

;; Test let bindings
(let ((x 10)) (+ x 5))
(let ((x 5) (y 10)) (* x y))

;; Test lambdas
((lambda (x) (* x 2)) 5)
((lambda (x y) (+ x y)) 3 4)

;; Test if expressions
(if (> 5 3) "yes" "no")
(if (< 5 3) "yes" "no")

;; Test lists
[1 2 3]
(list 1 2 3)
(cons 0 (list 1 2 3))
(car (list 1 2 3))
(cdr (list 1 2 3))

;; Test nested expressions
(let ((square (lambda (x) (* x x))))
  (square 7))