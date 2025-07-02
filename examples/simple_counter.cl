; Simple counter example to test UI compilation

(define main
  (lambda ()
    (let ((count (effect state:reactive-ref 0)))
      (let ((increment 
             (lambda () 
               (effect state:reactive-update count 
                 (lambda (n) (+ n 1)))))
            (decrement
             (lambda ()
               (effect state:reactive-update count
                 (lambda (n) (- n 1))))))
        
        (let ((render
               (lambda ()
                 (effect dom:render
                   (dom:h "div" (get {"class" "counter"} "class")
                     [(dom:h "h1" {} [(dom:text "Counter Demo")])
                      (dom:h "p" {} 
                        [(dom:text (concat "Count: " 
                                          (to-string (effect state:reactive-get count))))])
                      (dom:h "button" {"onClick" increment} 
                        [(dom:text "+")])
                      (dom:h "button" {"onClick" decrement} 
                        [(dom:text "-")])])
                   "#app"))))
          
          ; Initial render
          (render)
          
          ; Re-render on count changes
          (effect state:reactive-watch [count] render))))))