; Working counter - final version
((lambda ()
  (let ((count (effect state:reactive-ref 0)))
    (let ((update-and-render 
           (lambda (delta)
             (do
               (effect state:reactive-update count 
                 (lambda (n) (+ n delta)))
               (effect dom:render
                 (dom:h "div" []
                   [(dom:h "h1" [] [(dom:text "Counter")])
                    (dom:h "h2" [] 
                      [(dom:text (to-string (effect state:reactive-get count)))])
                    (dom:h "button" [] [(dom:text "+")])
                    (dom:h "button" [] [(dom:text "-")])])
                 "#app")))))
      ; Initial render  
      (update-and-render 0)))))