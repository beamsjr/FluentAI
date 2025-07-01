; Common algorithms implemented in ClaudeLang
; Demonstrates the language's expressiveness

; Binary search
(let ((binary-search (lambda (lst target)
                      (let ((search (lambda (low high)
                                     (if (> low high)
                                         -1
                                         (let ((mid (/ (+ low high) 2))
                                               (mid-val (nth lst mid)))
                                           (if (== mid-val target)
                                               mid
                                               (if (< mid-val target)
                                                   (search (+ mid 1) high)
                                                   (search low (- mid 1)))))))))
                        (search 0 (- (length lst) 1))))))
  ; Search for 7 in sorted list
  (binary-search [1 3 5 7 9 11 13] 7))

; Quicksort
(let ((quicksort (lambda (lst)
                  (if (<= (length lst) 1)
                      lst
                      (let ((pivot (head lst))
                            (rest (tail lst))
                            (less (filter (lambda (x) (< x pivot)) rest))
                            (greater (filter (lambda (x) (>= x pivot)) rest)))
                        (append (quicksort less)
                                (cons pivot (quicksort greater))))))))
  (quicksort [3 1 4 1 5 9 2 6 5]))

; Merge sort
(let ((merge (lambda (lst1 lst2)
              (if (empty? lst1)
                  lst2
                  (if (empty? lst2)
                      lst1
                      (if (<= (head lst1) (head lst2))
                          (cons (head lst1) (merge (tail lst1) lst2))
                          (cons (head lst2) (merge lst1 (tail lst2))))))))
      (merge-sort (lambda (lst)
                   (let ((len (length lst)))
                     (if (<= len 1)
                         lst
                         (let ((mid (/ len 2))
                               (left (take lst mid))
                               (right (drop lst mid)))
                           (merge (merge-sort left)
                                  (merge-sort right))))))))
  (merge-sort [3 1 4 1 5 9 2 6 5]))

; Prime number checker
(let ((is-prime (lambda (n)
                 (let ((check (lambda (i)
                               (if (> (* i i) n)
                                   #t
                                   (if (== (mod n i) 0)
                                       #f
                                       (check (+ i 1)))))))
                   (if (<= n 1)
                       #f
                       (if (<= n 3)
                           #t
                           (if (or (== (mod n 2) 0)
                                   (== (mod n 3) 0))
                               #f
                               (check 5))))))))
  ; Check if 17 is prime
  (is-prime 17))

; Generate prime numbers up to n (Sieve of Eratosthenes)
(let ((primes-up-to (lambda (n)
                     (let ((sieve (lambda (lst)
                                   (if (empty? lst)
                                       []
                                       (let ((p (head lst)))
                                         (if (> (* p p) n)
                                             lst
                                             (cons p (sieve (filter (lambda (x)
                                                                    (!= (mod x p) 0))
                                                                  (tail lst))))))))))
                       (sieve (range 2 n))))))
  (primes-up-to 30))

; Greatest common divisor (Euclidean algorithm)
(let ((gcd (lambda (a b)
            (if (== b 0)
                a
                (gcd b (mod a b))))))
  (gcd 48 18))

; Factorial with tail recursion
(let ((factorial (lambda (n)
                  (let ((fact-iter (lambda (n acc)
                                    (if (<= n 1)
                                        acc
                                        (fact-iter (- n 1) (* n acc))))))
                    (fact-iter n 1)))))
  (factorial 10))

; Tree operations (using nested lists)
(let ((tree-sum (lambda (tree)
                 (if (number? tree)
                     tree
                     (fold + 0 (map tree-sum tree))))))
  ; Sum all values in tree: [[1, 2], [3, [4, 5]]]
  (tree-sum [[1 2] [3 [4 5]]]))

; Memoization example
(let ((memoize (lambda (f)
                (let ((cache []))
                  (lambda (x)
                    (let ((cached (assoc x cache)))
                      (if cached
                          (cdr cached)
                          (let ((result (f x)))
                            (set! cache (cons (cons x result) cache))
                            result))))))))
      (fib (lambda (n)
            (if (<= n 1)
                n
                (+ (fib (- n 1)) (fib (- n 2)))))))
  ; Create memoized version
  (let ((memo-fib (memoize fib)))
    (memo-fib 20)))