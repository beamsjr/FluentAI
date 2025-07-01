; Garbage Collection Demo
; This example demonstrates memory management in ClaudeLang

; Create a recursive data structure that would leak without GC
(define (make-cyclic-list n)
  (let ((lst (list)))
    (dotimes (i n)
      (set! lst (cons i lst)))
    ; Create a cycle by making the last element point to the list
    (set-car! (last-pair lst) lst)
    lst))

; Function that creates temporary objects
(define (memory-intensive-computation n)
  (let ((result 0))
    (dotimes (i n)
      ; Create temporary lists that should be GC'd
      (let ((temp-list (map (lambda (x) (* x x)) (range 0 100))))
        (set! result (+ result (fold + 0 temp-list)))))
    result))

; Demonstrate closure memory management
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

; Create multiple counters (each with its own environment)
(define counters
  (map (lambda (i) (make-counter)) (range 0 10)))

; Use them
(print "Counter test:")
(for-each (lambda (c) (print (c))) counters)

; Run memory intensive computation
(print "\nMemory intensive computation:")
(print (memory-intensive-computation 100))

; Create and discard many temporary objects
(print "\nCreating temporary objects...")
(dotimes (i 1000)
  (let ((temp (make-list 100 i)))
    ; temp goes out of scope and should be collected
    #t))

(print "Done - temporary objects should be collected")

; Demonstrate environment chain
(define (make-nested-envs depth)
  (if (= depth 0)
      (lambda () 'base)
      (let ((x depth))
        (let ((inner (make-nested-envs (- depth 1))))
          (lambda () (list x (inner)))))))

(define nested (make-nested-envs 10))
(print "\nNested environments result:" (nested))

; Demonstrate GC control primitives
(print "\n=== GC Control Demo ===")
(print "GC enabled?" (gc:is-enabled?))
(print "Current threshold:" (gc:get-threshold))

; Check stats before collection
(print "\nStats before collection:")
(print (gc:stats))

; Force a garbage collection
(gc:collect)
(print "\nStats after collection:")
(print (gc:stats))

; Disable GC temporarily
(gc:disable)
(print "\nGC disabled:" (not (gc:is-enabled?)))

; Create more objects while GC is disabled
(dotimes (i 100)
  (make-list 50 'temp))

(print "Created objects with GC disabled")
(print "Stats:" (gc:stats))

; Re-enable and collect
(gc:enable)
(gc:collect)
(print "\nGC re-enabled and collected")
(print "Final stats:" (gc:stats))

; Configure GC threshold
(gc:set-threshold 1000)
(print "\nNew GC threshold:" (gc:get-threshold))