; Interactive counter with event handlers
(let ((count (effect state:reactive-ref 0)))
  (let ((render 
         (lambda ()
           (effect dom:render
             (dom:h "div" []
               [(dom:h "h1" [] [(dom:text "Counter")])
                (dom:h "h2" [] 
                  [(dom:text (to-string (effect state:reactive-get count)))])
                (dom:h "button" 
                  [(cons "onClick" 
                     (lambda () 
                       (do
                         (effect state:reactive-update count (lambda (n) (+ n 1)))
                         (render))))]
                  [(dom:text "+")])
                (dom:h "button" 
                  [(cons "onClick" 
                     (lambda ()
                       (do
                         (effect state:reactive-update count (lambda (n) (- n 1)))
                         (render))))]
                  [(dom:text "-")])])
             "#app"))))
    (render)))