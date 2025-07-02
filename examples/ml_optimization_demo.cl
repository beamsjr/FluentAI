; ML Optimization Demo Programs
; These programs demonstrate various optimization opportunities

; 1. Recursive Fibonacci - good candidate for memoization
(let ((fib (lambda (n)
             (if (<= n 1)
                 n
                 (+ (fib (- n 1))
                    (fib (- n 2)))))))
  (fib 35))

; 2. List mapping - good candidate for vectorization
(let ((square-all (lambda (lst)
                    (if (null? lst)
                        []
                        (cons (* (car lst) (car lst))
                              (square-all (cdr lst)))))))
  (square-all [1 2 3 4 5 6 7 8 9 10]))

; 3. Nested loops - good candidate for loop optimization
(let ((sum-matrix (lambda (matrix)
                    (let ((sum-row (lambda (row)
                                    (if (null? row)
                                        0
                                        (+ (car row) (sum-row (cdr row)))))))
                      (if (null? matrix)
                          0
                          (+ (sum-row (car matrix))
                             (sum-matrix (cdr matrix))))))))
  (sum-matrix [[1 2 3] [4 5 6] [7 8 9]]))

; 4. Small frequently-called function - good candidate for inlining
(let ((add1 (lambda (x) (+ x 1)))
      (process-list (lambda (lst)
                      (if (null? lst)
                          []
                          (cons (add1 (car lst))
                                (process-list (cdr lst)))))))
  (process-list [1 2 3 4 5]))

; 5. Map-reduce pattern - good for parallelization
(let ((map-square (lambda (lst)
                    (map (lambda (x) (* x x)) lst)))
      (sum (lambda (lst)
             (reduce + 0 lst))))
  (sum (map-square [1 2 3 4 5 6 7 8 9 10])))