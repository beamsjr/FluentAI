; Counter app - not self-executing
(let ((count (effect state:reactive-ref 0))
      (app-root "#app"))
  (let ((render 
         (lambda ()
           (let ((current-count (effect state:reactive-get count)))
             (effect dom:render
               (dom:h "div" (set {} "class" "counter")
                 [(dom:h "h1" {} [(dom:text "ClaudeLang Counter")])
                  (dom:h "h2" {} 
                    [(dom:text (to-string current-count))])
                  (dom:h "button" 
                    (set {} "onClick" 
                      (lambda () 
                        (do
                          (effect state:reactive-update count (lambda (n) (+ n 1)))
                          (render))))
                    [(dom:text "+")])
                  (dom:h "button" 
                    (set {} "onClick"
                      (lambda ()
                        (do
                          (effect state:reactive-update count (lambda (n) (- n 1)))
                          (render))))
                    [(dom:text "-")])])
               app-root)))))
    (render)))