; Working counter example - using list for props
(let ((count (effect state:reactive-ref 0)))
  (let ((render (lambda ()
                  (effect dom:render
                    (dom:h "div" 
                      (list (list "class" "counter"))
                      (list
                        (dom:h "h1" (list) (list (dom:text "Counter")))
                        (dom:h "p" (list) 
                          (list (dom:text (concat "Count: " (to-string (effect state:reactive-get count))))))
                        (dom:h "button" 
                          (list (list "onClick" (lambda () (effect state:reactive-update count (lambda (n) (+ n 1))))))
                          (list (dom:text "+")))
                        (dom:h "button"
                          (list (list "onClick" (lambda () (effect state:reactive-update count (lambda (n) (- n 1))))))
                          (list (dom:text "-")))))
                    "#app"))))
    (do
      (render)
      (effect state:reactive-watch (list count) render))))