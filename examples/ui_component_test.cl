; Test UI component parsing
(ui:component "Button"
  {:text (prop :string :required true)
   :onClick (prop :function)}
  (lambda (props)
    (dom:h "button" 
      (set {} "onClick" (get props "onClick"))
      [(dom:text (get props "text"))])))