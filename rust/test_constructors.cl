;; Test constructor patterns

;; Create a simple tagged value
(let ((x (Cons 1 2)))
  x)

;; Pattern matching on constructors
(let ((pair (Pair "hello" 42)))
  (match pair
    (Pair a b) (+ b 1)
    _ 0))

;; Nested constructors
(let ((tree (Node (Leaf 1) (Leaf 2))))
  (match tree
    (Node (Leaf x) (Leaf y)) (+ x y)
    _ 0))

;; Option-like pattern
(let ((maybe-value (Some 10)))
  (match maybe-value
    (Some x) (* x 2)
    (None) 0))

;; List-like constructors
(let ((my-list (Cons 1 (Cons 2 (Cons 3 Nil)))))
  (match my-list
    (Cons h t) h
    Nil 0))

;; Multiple pattern matching
(let ((result (Ok 42)))
  (match result
    (Ok value) value
    (Error msg) -1))