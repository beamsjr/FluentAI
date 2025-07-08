;; Simple Pattern Matching Examples
;; 
;; Basic pattern matching that works with current implementation

;; Simple literal matching
(define check-number
  (lambda (n)
    (if (= n 0)
        "zero"
        (if (= n 1)
            "one"
            (if (= n 2)
                "two"
                "other")))))

;; Test the function
(check-number 0)   ; "zero"
(check-number 1)   ; "one"
(check-number 5)   ; "other"

;; List operations using car/cdr
(define sum-list
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum-list (cdr lst))))))

;; Create and sum a list
(sum-list (list 1 2 3 4 5))  ; 15

;; Simple number description
(define describe-number
  (lambda (n)
    (if (< n 0)
        "negative"
        (if (= n 0)
            "zero"
            "positive"))))

(describe-number -5)  ; "negative"
(describe-number 0)   ; "zero"
(describe-number 5)   ; "positive"

;; Grade calculator
(define grade
  (lambda (score)
    (if (< score 60)
        "F"
        (if (< score 70)
            "D"
            (if (< score 80)
                "C"
                (if (< score 90)
                    "B"
                    "A"))))))

(grade 85)  ; "B"
(grade 95)  ; "A"