; Simple module test
(import "modules/math_utils" (square))

(io:print "Testing module import:")
(io:print "square(5) =" (square 5))