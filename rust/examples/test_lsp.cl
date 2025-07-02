; ClaudeLang test file for LSP features
(define factorial
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))

(+ 1 2)  ; Simple arithmetic

[1 2 3 4 5]  ; List literal

(str-concat "Hello, " "World!")  ; String operations

; Test undefined variable error
undefined_variable