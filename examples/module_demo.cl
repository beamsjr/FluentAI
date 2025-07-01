; Module System Demo
;
; This demonstrates ClaudeLang's module system

; Import specific functions from modules
(import "modules/math_utils" (square cube factorial-memo))
(import "modules/list_helpers" (sum-list avg-list max-list))
(import "modules/string_utils" (repeat-string capitalize word-count))

; Use imported functions
(io:print "=== Module System Demo ===\n")

(io:print "Math utilities:")
(io:print "  square(5) =" (square 5))
(io:print "  cube(3) =" (cube 3))
(io:print "  factorial(10) =" (factorial-memo 10))

(io:print "\nList helpers:")
(let ((numbers [1 2 3 4 5 6 7 8 9 10]))
  (io:print "  Numbers:" numbers)
  (io:print "  Sum:" (sum-list numbers))
  (io:print "  Average:" (avg-list numbers))
  (io:print "  Max:" (max-list numbers)))

(io:print "\nString utilities:")
(io:print "  repeat('Hi', 3) =" (repeat-string "Hi" 3))
(io:print "  capitalize('hello') =" (capitalize "hello"))
(io:print "  word-count('the quick brown fox') =" (word-count "the quick brown fox"))

; Import all exports from a module
(io:print "\nImporting all exports:")
(import "modules/math_utils" *)
(io:print "  All math functions available")

; Demonstrate module isolation
(io:print "\nModule isolation:")
(let ((local-square (lambda (x) (+ x x))))  ; Different from imported square
  (io:print "  Local square(5) =" (local-square 5))
  (io:print "  Imported square(5) =" (square 5)))

(io:print "\n=== Demo Complete ===")