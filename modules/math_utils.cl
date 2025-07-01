; Math utilities module

(module math-utils (export square cube factorial-memo)
  
  ; Define square function
  (let ((square (lambda (x) (* x x))))
    
    ; Define cube function  
    (let ((cube (lambda (x) (* x x x))))
      
      ; Memoized factorial
      (let ((factorial-memo 
              (memoize 
                (lambda (n)
                  (if (<= n 1)
                      1
                      (* n (factorial-memo (- n 1))))))))
        
        ; Module exports these functions
        (tuple square cube factorial-memo)))))