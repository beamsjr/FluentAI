(let ((x 10))
  (let ((add-x (lambda (y) (+ x y))))
    (add-x 5)))