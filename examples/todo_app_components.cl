; Todo App using ClaudeLang components

; Define TodoItem component
(ui:component "TodoItem"
  {:text (prop :string :required true)
   :completed (prop :bool :default false)
   :onToggle (prop :function)
   :onDelete (prop :function)}
  (lambda (props)
    (dom:h "li"
      (set {} "class" (ui:class "todo-item" {"completed" (get props "completed")}))
      [(dom:h "input"
         (set (set {} "type" "checkbox")
              "checked" (get props "completed")
              "onChange" (get props "onToggle"))
         [])
       (dom:h "span" {} [(dom:text (get props "text"))])
       (dom:h "button"
         (set {} "onClick" (get props "onDelete"))
         [(dom:text "Delete")])])))

; Main app
(let ((todos (effect state:reactive-ref []))
      (input-value (effect state:reactive-ref "")))
  
  (let ((add-todo
         (lambda ()
           (let ((text (effect state:reactive-get input-value)))
             (if (not (= text ""))
               (do
                 (effect state:reactive-update todos
                   (lambda (list)
                     (concat list [{:id (effect time:now) :text text :completed false}])))
                 (effect state:reactive-update input-value (lambda (_) "")))
               null))))
        
        (toggle-todo
         (lambda (id)
           (effect state:reactive-update todos
             (lambda (list)
               (map (lambda (todo)
                      (if (= (get todo "id") id)
                        (set todo "completed" (not (get todo "completed")))
                        todo))
                    list)))))
        
        (delete-todo
         (lambda (id)
           (effect state:reactive-update todos
             (lambda (list)
               (filter (lambda (todo) (not (= (get todo "id") id)))
                       list)))))
        
        (render
         (lambda ()
           (effect dom:render
             (dom:h "div" {:class "todo-app"}
               [(dom:h "h1" {} [(dom:text "Todo List")])
                (dom:h "div" {:class "add-todo"}
                  [(dom:h "input"
                     (set (set {} "type" "text")
                          "value" (effect state:reactive-get input-value)
                          "onChange" (lambda (e) 
                                      (effect state:reactive-update input-value 
                                        (lambda (_) (get (get e "target") "value")))))
                     [])
                   (dom:h "button"
                     (set {} "onClick" add-todo)
                     [(dom:text "Add")])])
                (dom:h "ul" {}
                  (ui:for (effect state:reactive-get todos)
                    (lambda (todo)
                      (dom:h TodoItem
                        (set (set (set (set {} 
                          "key" (get todo "id"))
                          "text" (get todo "text"))
                          "completed" (get todo "completed"))
                          "onToggle" (lambda () (toggle-todo (get todo "id")))
                          "onDelete" (lambda () (delete-todo (get todo "id"))))
                        []))))])
             "#app"))))
    
    ; Initial render
    (render)
    
    ; Re-render on changes
    (effect state:reactive-watch [todos input-value] render)))