; List helper functions module

(module list-helpers (export sum-list avg-list max-list min-list)
  
  ; Sum all elements in a list
  (let ((sum-list (lambda (lst) (fold + 0 lst))))
    
    ; Average of list elements
    (let ((avg-list (lambda (lst)
                      (if (empty? lst)
                          0
                          (/ (sum-list lst) (length lst))))))
      
      ; Maximum element
      (let ((max-list (lambda (lst)
                        (if (empty? lst)
                            0
                            (fold (lambda (acc x) (if (> x acc) x acc))
                                  (head lst)
                                  (tail lst))))))
        
        ; Minimum element  
        (let ((min-list (lambda (lst)
                          (if (empty? lst)
                              0
                              (fold (lambda (acc x) (if (< x acc) x acc))
                                    (head lst)
                                    (tail lst))))))
          
          ; Export all functions
          (tuple sum-list avg-list max-list min-list))))))