; ClaudeLang UI Demo Application
; This demonstrates building interactive web UIs with ClaudeLang

(module ui-demo
  (import std)
  (export [create-app]))

; Define a Todo item component
(define TodoItem
  (ui:component "TodoItem"
    {:text (prop :string :required true)
     :completed (prop :bool :default false)
     :onToggle (prop :function)
     :onDelete (prop :function)}
    (lambda (props)
      (dom:h "li"
        {:class (ui:class "todo-item" 
                         {"completed" (get props :completed)})}
        [(dom:h "input"
           {:type "checkbox"
            :checked (get props :completed)
            :onChange (get props :onToggle)})
         (dom:h "span" {} [(dom:text (get props :text))])
         (dom:h "button"
           {:onClick (get props :onDelete)}
           [(dom:text "Delete")])]))))

; Define the main App component
(define create-app
  (lambda ()
    (let ((todos (effect reactive:ref []))
          (input-value (effect reactive:ref ""))
          (filter-mode (effect reactive:ref "all")))
      
      ; Computed value for filtered todos
      (let ((filtered-todos 
             (effect reactive:computed
               (lambda ()
                 (let ((all-todos (effect reactive:get todos))
                       (mode (effect reactive:get filter-mode)))
                   (cond
                     ((= mode "active")
                      (filter (lambda (todo) (not (get todo :completed))) 
                              all-todos))
                     ((= mode "completed")
                      (filter (lambda (todo) (get todo :completed))
                              all-todos))
                     (else all-todos)))))))
        
        ; Add todo function
        (let ((add-todo
               (lambda ()
                 (let ((text (effect reactive:get input-value)))
                   (when (not (empty? text))
                     (effect reactive:update todos
                       (lambda (current)
                         (cons {:id (effect time:now)
                                :text text
                                :completed false}
                               current)))
                     (effect reactive:set input-value ""))))))
          
          ; Toggle todo function
          (let ((toggle-todo
                 (lambda (id)
                   (effect reactive:update todos
                     (lambda (current)
                       (map (lambda (todo)
                              (if (= (get todo :id) id)
                                  (set todo :completed 
                                       (not (get todo :completed)))
                                  todo))
                            current))))))
            
            ; Delete todo function
            (let ((delete-todo
                   (lambda (id)
                     (effect reactive:update todos
                       (lambda (current)
                         (filter (lambda (todo)
                                  (not= (get todo :id) id))
                                current))))))
              
              ; Render function
              (lambda ()
                (dom:h "div" {:class "todo-app"}
                  [(dom:h "h1" {} [(dom:text "ClaudeLang Todo App")])
                   
                   ; Input section
                   (dom:h "div" {:class "input-section"}
                     [(dom:h "input"
                        {:type "text"
                         :value (effect reactive:get input-value)
                         :placeholder "What needs to be done?"
                         :onInput (lambda (e)
                                   (effect reactive:set input-value
                                     (get (get e :target) :value)))
                         :onKeyPress (lambda (e)
                                      (when (= (get e :key) "Enter")
                                        (add-todo)))})
                      (dom:h "button"
                        {:onClick add-todo}
                        [(dom:text "Add")])])
                   
                   ; Filter buttons
                   (dom:h "div" {:class "filters"}
                     [(dom:h "button"
                        {:class (ui:class {"active" (= (effect reactive:get filter-mode) "all")})
                         :onClick (lambda () (effect reactive:set filter-mode "all"))}
                        [(dom:text "All")])
                      (dom:h "button"
                        {:class (ui:class {"active" (= (effect reactive:get filter-mode) "active")})
                         :onClick (lambda () (effect reactive:set filter-mode "active"))}
                        [(dom:text "Active")])
                      (dom:h "button"
                        {:class (ui:class {"active" (= (effect reactive:get filter-mode) "completed")})
                         :onClick (lambda () (effect reactive:set filter-mode "completed"))}
                        [(dom:text "Completed")])])
                   
                   ; Todo list
                   (dom:h "ul" {:class "todo-list"}
                     (ui:for (effect reactive:get filtered-todos)
                       (lambda (todo idx)
                         (TodoItem {:text (get todo :text)
                                   :completed (get todo :completed)
                                   :onToggle (lambda () (toggle-todo (get todo :id)))
                                   :onDelete (lambda () (delete-todo (get todo :id)))
                                   :key (get todo :id)}))))
                   
                   ; Stats
                   (let ((active-count 
                          (length (filter (lambda (t) (not (get t :completed)))
                                        (effect reactive:get todos)))))
                     (dom:h "div" {:class "stats"}
                       [(dom:text (concat (to-string active-count) 
                                        " item(s) left"))]))]))))))))))

; Example of using the app
(define main
  (lambda ()
    (let ((app (create-app)))
      ; Mount the app to the DOM
      (effect dom:render (app) "#app"))))

; Advanced example: Counter with learning optimization
(define SmartCounter
  (lambda ()
    (let ((count (effect reactive:ref 0))
          (click-times (effect reactive:ref [])))
      
      ; Learn user click patterns
      (effect reactive:watch [count]
        (lambda (values)
          (effect reactive:update click-times
            (lambda (times)
              (cons (effect time:now) times)))))
      
      ; Predict next click (simplified)
      (let ((predict-next-click
             (effect reactive:computed
               (lambda ()
                 (let ((times (effect reactive:get click-times)))
                   (if (>= (length times) 2)
                       (let ((intervals
                              (map (lambda (i)
                                    (- (nth times i)
                                       (nth times (+ i 1))))
                                  (range 0 (- (length times) 1)))))
                         (if (not (empty? intervals))
                             (/ (reduce + intervals) (length intervals))
                             0))
                       0))))))
        
        ; Render
        (lambda ()
          (dom:h "div" {:class "smart-counter"}
            [(dom:h "h2" {} [(dom:text "Smart Counter")])
             (dom:h "p" {} [(dom:text (concat "Count: " 
                                            (to-string (effect reactive:get count))))])
             (dom:h "button"
               {:onClick (lambda ()
                          (effect reactive:update count (lambda (c) (+ c 1))))}
               [(dom:text "Increment")])
             (ui:when (> (effect reactive:get predict-next-click) 0)
               (dom:h "p" {:class "prediction"}
                 [(dom:text (concat "Predicted interval: "
                                  (to-string (effect reactive:get predict-next-click))
                                  "ms"))]))]))))))