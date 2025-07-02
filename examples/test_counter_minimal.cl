; Minimal counter test
(let ((count (effect state:reactive-ref 0)))
  (effect dom:render 
    (dom:h "div" {} [(dom:text "Hello")])
    "#app"))