; ClaudeLang Prelude - Common utilities
;
; This module provides commonly used functions

(module prelude (export 
  ; Function composition
  |> <| 
  ; Common predicates  
  even? odd? positive? negative? zero?
  ; List utilities
  second third last init
  ; Misc
  inc dec const-true const-false)
  
  ; Pipe forward operator
  (let ((|> (lambda (x f) (f x))))
    
    ; Pipe backward operator
    (let ((<| (lambda (f x) (f x))))
      
      ; Predicates
      (let ((even? (lambda (n) (== (mod n 2) 0))))
        (let ((odd? (lambda (n) (not (even? n)))))
          (let ((positive? (lambda (n) (> n 0))))
            (let ((negative? (lambda (n) (< n 0))))
              (let ((zero? (lambda (n) (== n 0))))
                
                ; List utilities
                (let ((second (lambda (lst) (head (tail lst)))))
                  (let ((third (lambda (lst) (head (tail (tail lst))))))
                    (let ((last (lambda (lst)
                                  (if (empty? (tail lst))
                                      (head lst)
                                      (last (tail lst))))))
                      (let ((init (lambda (lst)
                                    (if (empty? (tail lst))
                                        []
                                        (cons (head lst) (init (tail lst)))))))
                        
                        ; Misc utilities
                        (let ((inc (lambda (n) (+ n 1))))
                          (let ((dec (lambda (n) (- n 1))))
                            (let ((const-true (lambda () #t)))
                              (let ((const-false (lambda () #f)))
                                
                                ; Export all defined functions
                                (tuple |> <| even? odd? positive? negative? zero?
                                       second third last init inc dec 
                                       const-true const-false)))))))))))))))))))))