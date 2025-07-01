; Pattern Matching Demo
;
; This demonstrates ClaudeLang's pattern matching capabilities

(io:print "=== Pattern Matching Demo ===\n")

; Simple literal matching
(let ((check-number (lambda (n)
                      (match n
                        (0 "zero")
                        (1 "one")
                        (2 "two")
                        (_ "other")))))
  (io:print "check-number tests:")
  (io:print "  0 =>" (check-number 0))
  (io:print "  1 =>" (check-number 1))
  (io:print "  5 =>" (check-number 5)))

; Variable binding
(io:print "\nVariable binding:")
(let ((describe (lambda (x)
                  (match x
                    (n (string-concat "The number is: " (to-string n)))))))
  (io:print "  42 =>" (describe 42)))

; List pattern matching
(io:print "\nList patterns:")
(let ((list-info (lambda (lst)
                   (match lst
                     ([] "empty list")
                     ([x] (string-concat "single element: " (to-string x)))
                     ([x, y] (string-concat "two elements: " (to-string x) " and " (to-string y)))
                     ([x, y, ...rest] (string-concat "at least two elements, rest has " 
                                                     (to-string (length rest)) " items"))))))
  (io:print "  [] =>" (list-info []))
  (io:print "  [1] =>" (list-info [1]))
  (io:print "  [1, 2] =>" (list-info [1 2]))
  (io:print "  [1, 2, 3, 4] =>" (list-info [1 2 3 4])))

; Nested patterns
(io:print "\nNested patterns:")
(let ((process-pair (lambda (pair)
                      (match pair
                        ([0, y] (string-concat "First is zero, second is " (to-string y)))
                        ([x, 0] (string-concat "First is " (to-string x) ", second is zero"))
                        ([x, y] (string-concat "Both non-zero: " (to-string x) ", " (to-string y)))))))
  (io:print "  [0, 5] =>" (process-pair [0 5]))
  (io:print "  [3, 0] =>" (process-pair [3 0]))
  (io:print "  [2, 3] =>" (process-pair [2 3])))

; Factorial with pattern matching
(io:print "\nFactorial with patterns:")
(let ((factorial (lambda (n)
                   (match n
                     (0 1)
                     (n (* n (factorial (- n 1))))))))
  (io:print "  factorial(5) =" (factorial 5)))

; List operations with patterns
(io:print "\nList operations:")
(let ((sum-list (lambda (lst)
                  (match lst
                    ([] 0)
                    ([x, ...rest] (+ x (sum-list rest)))))))
  (io:print "  sum([1, 2, 3, 4, 5]) =" (sum-list [1 2 3 4 5])))

; Constructor patterns (using tuples as tagged values)
(io:print "\nConstructor patterns:")
(let ((make-option (lambda (value)
                     (if value
                         (tuple "Some" value)
                         (tuple "None"))))
      (unwrap-or (lambda (opt default)
                   (match opt
                     ((Some v) v)
                     ((None) default)))))
  (let ((some-value (make-option 42))
        (none-value (make-option nil)))
    (io:print "  Some(42) unwraps to:" (unwrap-or some-value 0))
    (io:print "  None unwraps to:" (unwrap-or none-value 0))))

(io:print "\n=== Demo Complete ===")