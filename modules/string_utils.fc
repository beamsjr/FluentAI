; String utilities module

; Import math utilities for use
(import "math_utils" (square))

(module string-utils (export repeat-string capitalize word-count)
  
  ; Repeat a string n times
  (let ((repeat-string (lambda (s n)
                         (if (<= n 0)
                             ""
                             (fold string-concat "" (replicate n (const s)))))))
    
    ; Capitalize first letter
    (let ((capitalize (lambda (s)
                        (if (== (string-length s) 0)
                            s
                            (string-concat 
                              (string-upcase (string-slice s 0 1))
                              (string-slice s 1 (string-length s)))))))
      
      ; Count words in a string
      (let ((word-count (lambda (s)
                          (length (filter (lambda (w) (> (string-length w) 0))
                                         (string-split s " "))))))
        
        ; Export functions
        (tuple repeat-string capitalize word-count)))))