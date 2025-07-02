; Test onClick
(dom:h "button" 
  (set {} "onClick" (lambda () (effect io:print "clicked")))
  [(dom:text "Click me")])