; ClaudeLang Standard Library Demo
;
; This demonstrates the comprehensive standard library

(io:print "=== ClaudeLang Standard Library Demo ===\n")

; Math functions
(io:print "Math Functions:")
(io:print "  sin(π/4):" (sin (/ (pi) 4)))
(io:print "  sqrt(16):" (sqrt 16))
(io:print "  factorial(5):" (factorial 5))
(io:print "  gcd(48, 18):" (gcd 48 18))
(io:print "  mean([1,2,3,4,5]):" (mean [1 2 3 4 5]))

; String operations
(io:print "\nString Operations:")
(let ((text "  Hello, ClaudeLang!  "))
  (io:print "  Original:" text)
  (io:print "  Trimmed:" (string-trim text))
  (io:print "  Uppercase:" (string-upper text))
  (io:print "  Contains 'Claude':" (string-contains? text "Claude"))
  (io:print "  Split words:" (string-split (string-trim text) " ")))

; Data structures
(io:print "\nData Structures:")

; Dictionary operations
(let ((person (dict-new)))
  (let ((person (dict-set person "name" "Alice")))
    (let ((person (dict-set person "age" 30)))
      (let ((person (dict-set person "city" "Boston")))
        (io:print "  Person dict:" person)
        (io:print "  Name:" (dict-get person "name"))
        (io:print "  Keys:" (dict-keys person))))))

; Set operations  
(let ((set1 (set-from-list [1 2 3 4]))
      (set2 (set-from-list [3 4 5 6])))
  (io:print "  Set 1:" (set-to-list set1))
  (io:print "  Set 2:" (set-to-list set2))
  (io:print "  Union:" (set-to-list (set-union set1 set2)))
  (io:print "  Intersection:" (set-to-list (set-intersection set1 set2))))

; Functional programming
(io:print "\nFunctional Programming:")

; Function composition
(let ((add-10 (partial + 10))
      (double (partial * 2))
      (add-10-then-double (compose double add-10)))
  (io:print "  Composition (x + 10) * 2:")
  (io:print "    f(5) =" (add-10-then-double 5)))

; Higher-order list operations
(let ((numbers [1 2 3 4 5]))
  (io:print "  Numbers:" numbers)
  (io:print "  Mapped (* 2):" (map (lambda (x) (* x 2)) numbers))
  (io:print "  Filtered (> 2):" (filter (lambda (x) (> x 2)) numbers))
  (io:print "  Fold sum:" (fold + 0 numbers))
  (io:print "  Map-indexed:" (map-indexed (lambda (i x) (tuple i x)) numbers)))

; Advanced list operations
(io:print "\nAdvanced List Operations:")
(let ((lst1 [1 2 3])
      (lst2 ["a" "b" "c"]))
  (io:print "  Zip:" (list-zip lst1 lst2))
  (io:print "  Partition evens:" (list-partition (lambda (x) (== (mod x 2) 0)) [1 2 3 4 5 6]))
  (io:print "  Unique:" (list-unique [1 2 2 3 3 3 4])))

; Date/time operations
(io:print "\nDate/Time Operations:")
(let ((now (datetime:now)))
  (io:print "  Current time:" (datetime:iso-format now))
  (io:print "  Year:" (datetime:year now))
  (io:print "  Month:" (datetime:month now))
  (io:print "  Day:" (datetime:day now))
  (io:print "  Tomorrow:" (datetime:iso-format (datetime:add-days now 1)))
  (io:print "  Next week:" (datetime:iso-format (datetime:add-days now 7))))

; Grouping and chunking
(io:print "\nGrouping Operations:")
(let ((data [1 2 3 4 5 6 7 8 9 10]))
  (io:print "  Chunk by 3:" (chunk 3 data))
  (io:print "  Sliding window 3:" (sliding-window 3 data)))

(let ((people [{"name" "Alice" "age" 30}
               {"name" "Bob" "age" 25}
               {"name" "Carol" "age" 30}
               {"name" "Dave" "age" 25}]))
  (io:print "  Group by age:" (group-by (lambda (p) (dict-get p "age")) people)))

; Complex example: Word frequency counter
(io:print "\nWord Frequency Example:")
(let ((text "the quick brown fox jumps over the lazy dog the fox"))
  (let ((words (string-split text " ")))
    (let ((freq (fold (lambda (acc word)
                        (dict-set acc word (+ 1 (dict-get-or acc word 0))))
                      (dict-new)
                      words)))
      (io:print "  Text:" text)
      (io:print "  Word frequencies:" freq))))

(io:print "\n=== Demo Complete ===")