;; ClaudeLang Type Annotation Examples
;; Demonstrates various ways to annotate types

;; 1. Type ascription - annotate any expression
(: 42 Int)

;; 2. Function type ascription
(: (lambda (x) (+ x 1)) (Function Int Int))

;; 3. Complex type annotations
(: [] (List Int))
(: (lambda (xs) xs) (Function (List a) (List a)))

;; 4. Type ascription in let bindings
(let ((inc (: (lambda (x) (+ x 1)) (Function Int Int)))
      (double (: (lambda (x) (* x 2)) (Function Int Int))))
  (inc (double 5)))

;; 5. Using with ADTs
(data Option a
  (None)
  (Some a))

(: (Some 42) (Option Int))

;; 6. Nested type annotations
(: (: 3.14 Float) Number)

;; 7. List processing with types
(let ((map-int (: (lambda (f xs)
                    (if (empty? xs)
                        []
                        (cons (f (head xs)) (map-int f (tail xs)))))
                  (Function (Function Int Int) (List Int) (List Int)))))
  (map-int (lambda (x) (* x x)) [1 2 3 4 5]))

;; 8. Type annotations with effects
;; (Future: we could extend this to include effect annotations)
;; (: (lambda () (effect io:print "Hello")) (Function () () {IO}))

;; 9. Polymorphic identity function
(: (lambda (x) x) (Function a a))

;; 10. Multi-parameter function types
(: (lambda (x y z) (+ x (+ y z)))
   (Function Int Int Int Int))