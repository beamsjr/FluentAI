; ClaudeLang REPL Demo File
; 
; This file demonstrates various ClaudeLang features
; Run with: python3 -m src.repl
; Then type: :load examples/repl_demo.cl

; Basic arithmetic
(print "Basic arithmetic:")
(print (+ 2 3))
(print (* 4 5))
(print (/ 20 4))

; Variables and let bindings
(print "\nVariables:")
(let ((x 10) (y 20))
  (print (+ x y)))

; Functions
(print "\nFunctions:")
(let ((square (lambda (x) (* x x))))
  (print (square 5))
  (print (square 7)))

; Lists
(print "\nLists:")
(print [1 2 3 4 5])
(print (length [10 20 30 40]))
(print (head [100 200 300]))
(print (tail [100 200 300]))

; Conditionals
(print "\nConditionals:")
(print (if (> 10 5) "10 is greater" "5 is greater"))
(print (if (< 3 7) "correct" "wrong"))

; Nested expressions
(print "\nNested expressions:")
(print (+ (* 3 4) (- 10 5)))
(print (let ((a 5) (b 3))
         (* (+ a b) (- a b))))