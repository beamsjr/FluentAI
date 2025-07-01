; List operations in ClaudeLang
; Demonstrates functional list processing

; Map function - apply function to each element
(let ((map (lambda (f lst)
            (if (empty? lst)
                []
                (cons (f (head lst))
                      (map f (tail lst)))))))
  ; Double all numbers
  (map (lambda (x) (* x 2)) [1 2 3 4 5]))

; Filter function - keep elements that satisfy predicate
(let ((filter (lambda (pred lst)
               (if (empty? lst)
                   []
                   (if (pred (head lst))
                       (cons (head lst) (filter pred (tail lst)))
                       (filter pred (tail lst)))))))
  ; Keep only even numbers
  (filter (lambda (x) (== (mod x 2) 0)) [1 2 3 4 5 6 7 8 9 10]))

; Fold/reduce function - combine elements
(let ((fold (lambda (f init lst)
             (if (empty? lst)
                 init
                 (fold f (f init (head lst)) (tail lst))))))
  ; Sum all numbers
  (fold + 0 [1 2 3 4 5 6 7 8 9 10]))

; List comprehension-like operation
(let ((range (lambda (start end)
              (if (> start end)
                  []
                  (cons start (range (+ start 1) end)))))
      (square (lambda (x) (* x x))))
  ; Squares of numbers 1 to 10
  (map square (range 1 10)))

; Zip two lists together
(let ((zip (lambda (lst1 lst2)
            (if (or (empty? lst1) (empty? lst2))
                []
                (cons [(head lst1) (head lst2)]
                      (zip (tail lst1) (tail lst2)))))))
  (zip [1 2 3] ["a" "b" "c"]))

; Reverse a list
(let ((reverse (lambda (lst)
                (let ((rev-helper (lambda (lst acc)
                                   (if (empty? lst)
                                       acc
                                       (rev-helper (tail lst)
                                                  (cons (head lst) acc))))))
                  (rev-helper lst [])))))
  (reverse [1 2 3 4 5]))