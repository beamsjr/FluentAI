; ClaudeLang Standard Library - Collection Operations
; Higher-order functions for working with collections

; map: Apply function to each element
(define map
  (lambda (f lst)
    (if (empty? lst)
        []
        (cons (f (head lst))
              (map f (tail lst))))))

; filter: Keep elements that satisfy predicate
(define filter
  (lambda (pred lst)
    (if (empty? lst)
        []
        (if (pred (head lst))
            (cons (head lst) (filter pred (tail lst)))
            (filter pred (tail lst))))))

; reduce: Fold left with accumulator
(define reduce
  (lambda (f init lst)
    (if (empty? lst)
        init
        (reduce f (f init (head lst)) (tail lst)))))

; fold-right: Fold right 
(define fold-right
  (lambda (f init lst)
    (if (empty? lst)
        init
        (f (head lst) (fold-right f init (tail lst))))))

; any?: Check if any element satisfies predicate
(define any?
  (lambda (pred lst)
    (if (empty? lst)
        false
        (or (pred (head lst))
            (any? pred (tail lst))))))

; all?: Check if all elements satisfy predicate
(define all?
  (lambda (pred lst)
    (if (empty? lst)
        true
        (and (pred (head lst))
             (all? pred (tail lst))))))

; find: Find first element satisfying predicate
(define find
  (lambda (pred lst)
    (if (empty? lst)
        (effect :error "Not found")
        (if (pred (head lst))
            (head lst)
            (find pred (tail lst))))))

; partition: Split list into two based on predicate
(define partition
  (lambda (pred lst)
    (let ((satisfies (filter pred lst))
          (fails (filter (lambda (x) (not (pred x))) lst)))
      (tuple satisfies fails))))

; zip: Combine two lists into list of pairs
(define zip
  (lambda (lst1 lst2)
    (if (or (empty? lst1) (empty? lst2))
        []
        (cons (tuple (head lst1) (head lst2))
              (zip (tail lst1) (tail lst2))))))

; unzip: Split list of pairs into two lists
(define unzip
  (lambda (pairs)
    (if (empty? pairs)
        (tuple [] [])
        (let ((first-pair (head pairs))
              (rest-unzipped (unzip (tail pairs))))
          (tuple (cons (fst first-pair) (fst rest-unzipped))
                 (cons (snd first-pair) (snd rest-unzipped)))))))

; enumerate: Add indices to list elements
(define enumerate
  (lambda (lst)
    (let ((indices (range 0 (length lst))))
      (zip indices lst))))

; range: Generate list of integers
(define range
  (lambda (start end)
    (if (>= start end)
        []
        (cons start (range (+ start 1) end)))))

; flatten: Flatten nested lists
(define flatten
  (lambda (lst)
    (if (empty? lst)
        []
        (let ((head-elem (head lst)))
          (if (list? head-elem)
              (append (flatten head-elem) (flatten (tail lst)))
              (cons head-elem (flatten (tail lst))))))))

; group-by: Group elements by key function
(define group-by
  (lambda (key-fn lst)
    (reduce
      (lambda (groups elem)
        (let ((key (key-fn elem)))
          ; This would need a map/dictionary type
          ; For now, return a list of (key, elements) pairs
          groups))
      []
      lst)))

; sort: Sort list (simplified bubble sort for now)
(define sort
  (lambda (compare lst)
    (if (<= (length lst) 1)
        lst
        (let ((pivot (head lst))
              (rest (tail lst))
              (smaller (filter (lambda (x) (compare x pivot)) rest))
              (larger (filter (lambda (x) (not (compare x pivot))) rest)))
          (append (sort compare smaller)
                  (cons pivot (sort compare larger)))))))

; unique: Remove duplicates
(define unique
  (lambda (lst)
    (reduce
      (lambda (acc elem)
        (if (any? (lambda (x) (== x elem)) acc)
            acc
            (append acc [elem])))
      []
      lst)))