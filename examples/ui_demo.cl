; ClaudeLang UI Demo Application
; This demonstrates building interactive web UIs with ClaudeLang

; Define a Todo item component
(ui:component "TodoItem"
  {:text (prop :string :required true)
   :completed (prop :bool :default false)
   :onToggle (prop :function)
   :onDelete (prop :function)}
  (lambda (props)
    (dom:h "li"
      (set {} "class" (ui:class "todo-item" 
                               {"completed" (get props "completed")}))
      [(dom:h "input"
         (set (set (set {} "type" "checkbox")
                   "checked" (get props "completed"))
              "onChange" (get props "onToggle"))
         [])
       (dom:h "span" {} [(dom:text (get props "text"))])
       (dom:h "button"
         (set {} "onClick" (get props "onDelete"))
         [(dom:text "Delete")])])))

; Define the main App component
(ui:component "TodoApp"
  {}
  (lambda (props)
    (let ((todos (effect state:reactive-ref []))
          (input-value (effect state:reactive-ref ""))
          (filter-mode (effect state:reactive-ref "all")))
      
      ; Computed value for filtered todos
      (let ((filtered-todos 
             (effect state:reactive-computed
               (lambda ()
                 (let ((all-todos (effect state:reactive-get todos))
                       (mode (effect state:reactive-get filter-mode)))
                   (if (= mode "active")
                       (filter (lambda (todo) (not (get todo "completed"))) 
                               all-todos)
                       (if (= mode "completed")
                           (filter (lambda (todo) (get todo "completed"))
                                   all-todos)
                           all-todos))))))
            
            ; Add todo function
            (add-todo
             (lambda ()
               (let ((text (effect state:reactive-get input-value)))
                 (if (not (empty? text))
                     (do
                       (effect state:reactive-update todos
                         (lambda (current)
                           (cons (set (set {} "id" (to-string (length current)))
                                      "text" text
                                      "completed" false)
                                 current)))
                       (effect state:reactive-set input-value ""))
                     null))))
            
            ; Toggle todo function
            (toggle-todo
             (lambda (id)
               (effect state:reactive-update todos
                 (lambda (current)
                   (map (lambda (todo)
                          (if (= (get todo "id") id)
                              (set todo "completed" 
                                   (not (get todo "completed")))
                              todo))
                        current)))))
            
            ; Delete todo function
            (delete-todo
             (lambda (id)
               (effect state:reactive-update todos
                 (lambda (current)
                   (filter (lambda (todo)
                            (not (= (get todo "id") id)))
                          current))))))
        
        ; Render the app
        (dom:h "div" (set {} "class" "todo-app")
          [(dom:h "h1" {} [(dom:text "ClaudeLang Todo App")])
           
           ; Input section
           (dom:h "div" (set {} "class" "input-section")
             [(dom:h "input"
                (set (set (set (set {} "type" "text")
                               "value" (effect state:reactive-get input-value))
                          "placeholder" "What needs to be done?")
                     "onInput" (lambda (e)
                               (effect state:reactive-set input-value
                                 (get (get e "target") "value"))))
                [])
              (dom:h "button"
                (set {} "onClick" add-todo)
                [(dom:text "Add")])])
           
           ; Filter buttons
           (dom:h "div" (set {} "class" "filters")
             [(dom:h "button"
                (set (set {} "class" (ui:class "filter-btn" {"active" (= (effect state:reactive-get filter-mode) "all")}))
                     "onClick" (lambda () (effect state:reactive-set filter-mode "all")))
                [(dom:text "All")])
              (dom:h "button"
                (set (set {} "class" (ui:class "filter-btn" {"active" (= (effect state:reactive-get filter-mode) "active")}))
                     "onClick" (lambda () (effect state:reactive-set filter-mode "active")))
                [(dom:text "Active")])
              (dom:h "button"
                (set (set {} "class" (ui:class "filter-btn" {"active" (= (effect state:reactive-get filter-mode) "completed")}))
                     "onClick" (lambda () (effect state:reactive-set filter-mode "completed")))
                [(dom:text "Completed")])])
           
           ; Todo list
           (dom:h "ul" (set {} "class" "todo-list")
             (ui:for (effect state:reactive-get filtered-todos)
               (lambda (todo)
                 (ui:create "TodoItem" 
                   (set (set (set (set {} "text" (get todo "text"))
                                  "completed" (get todo "completed"))
                             "onToggle" (lambda () (toggle-todo (get todo "id"))))
                        "onDelete" (lambda () (delete-todo (get todo "id"))))))))
           
           ; Stats
           (let ((active-count 
                  (length (filter (lambda (t) (not (get t "completed")))
                                (effect state:reactive-get todos)))))
             (dom:h "div" (set {} "class" "stats")
               [(dom:text (concat (to-string active-count) 
                                " item(s) left"))]))])))))

; Smart Counter component with simpler logic
(ui:component "SmartCounter"
  {}
  (lambda (props)
    (let ((count (effect state:reactive-ref 0))
          (clicks (effect state:reactive-ref 0)))
      
      ; Simple click rate calculation
      (let ((click-rate
             (effect state:reactive-computed
               (lambda ()
                 (let ((c (effect state:reactive-get clicks)))
                   (if (> c 0)
                       (concat "Click rate: " (to-string c) " clicks")
                       "No clicks yet"))))))
        
        ; Render
        (dom:h "div" (set {} "class" "smart-counter")
          [(dom:h "h2" {} [(dom:text "Smart Counter")])
           (dom:h "p" {} [(dom:text (concat "Count: " 
                                          (to-string (effect state:reactive-get count))))])
           (dom:h "button"
             (set {} "onClick" (lambda ()
                                (do
                                  (effect state:reactive-update count (lambda (c) (+ c 1)))
                                  (effect state:reactive-update clicks (lambda (c) (+ c 1))))))
             [(dom:text "Increment")])
           (dom:h "p" (set {} "class" "rate")
             [(dom:text (effect state:reactive-get click-rate))])])))))

; Main app that renders both components
(let ((app-container (dom:h "div" {}
                      [(ui:create "TodoApp" {})
                       (dom:h "hr" {} [])
                       (ui:create "SmartCounter" {})])))
  (effect dom:render app-container "#app"))