; Simple component demo

; Define a Button component
(ui:component "Button"
  {:text (prop :string :required true)
   :onClick (prop :function)}
  (lambda (props)
    (dom:h "button"
      (set {} "onClick" (get props "onClick"))
      [(dom:text (get props "text"))])))

; Define a Counter component that uses Button
(ui:component "Counter"
  {:initial (prop :number :default 0)}
  (lambda (props)
    (let ((count (effect state:reactive-ref (get props "initial"))))
      (let ((increment (lambda () 
                        (effect state:reactive-update count 
                          (lambda (n) (+ n 1)))))
            (decrement (lambda ()
                        (effect state:reactive-update count
                          (lambda (n) (- n 1))))))
        (dom:h "div" {}
          [(dom:h "h2" {} 
            [(dom:text (concat "Count: " (to-string (effect state:reactive-get count))))])
           (ui:create "Button" 
             (set {} "text" "+" "onClick" increment))
           (ui:create "Button"
             (set {} "text" "-" "onClick" decrement))])))))

; Main app
(effect dom:render 
  (ui:create "Counter" (set {} "initial" 10))
  "#app")