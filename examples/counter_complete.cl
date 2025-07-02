; Complete counter example
(define render-counter
  (lambda ()
    (let ((count (effect state:reactive-ref 0)))
      (let ((increment (lambda () 
                         (effect state:reactive-update count 
                           (lambda (n) (+ n 1)))))
            (decrement (lambda ()
                         (effect state:reactive-update count
                           (lambda (n) (- n 1))))))
        (effect dom:render
          (dom:h "div" [] 
            [(dom:h "h1" [] [(dom:text "Counter Demo")])
             (dom:h "p" [] 
               [(dom:text (concat "Count: " (to-string (effect state:reactive-get count))))])
             (dom:h "button" [] [(dom:text "+")])
             (dom:h "button" [] [(dom:text "-")])])
          "#app")))))

(render-counter)