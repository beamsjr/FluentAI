;; ClaudeLang debugger example
;; Run with: python -m src.debugger.cli examples/debug_example.cl --stop-on-entry

(def factorial (n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(def main ()
  (let ((n 5))
    (print "Calculating factorial of" n)
    (let ((result (factorial n)))
      (print "Result:" result)
      result)))

;; Run main
(main)