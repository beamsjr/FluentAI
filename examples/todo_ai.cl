; ClaudeLang AI-Powered Todo App
; This demonstrates real ClaudeLang code with learning capabilities

(module todo-ai
  (import std)
  (import effects)
  (export [create-app]))

; Learning model for task patterns
(define create-learning-model
  (lambda ()
    {:task-history []
     :completion-times []
     :patterns {:by-hour (make-array 24 0)
                :by-day (make-array 7 0)
                :keywords {}
                :avg-completion-time 0
                :total-completed 0
                :total-added 0}
     :predictions {:daily-capacity 5
                   :best-work-time "morning"
                   :velocity "stable"
                   :suggested-tasks []}}))

; Analyze patterns and update predictions
(define analyze-patterns
  (lambda (model)
    (let ((history (get model :task-history))
          (patterns (get model :patterns)))
      (if (> (length history) 5)
          ; Calculate average completion time
          (let ((completed (filter (lambda (t) (get t :completed)) history)))
            (if (not (empty? completed))
                (let ((times (map (lambda (t) (get t :completion-time)) completed))
                      (avg-time (/ (reduce + times) (length times))))
                  (set patterns :avg-completion-time avg-time))
                patterns))
          patterns))))

; Predict daily capacity based on history
(define predict-capacity
  (lambda (model)
    (let ((history (get model :task-history))
          (last-week (filter 
                      (lambda (t) 
                        (< (- (effect time:now) (get t :created-at)) 
                           (* 7 24 60 60 1000)))
                      history)))
      (if (> (length last-week) 0)
          (let ((by-day (group-by (lambda (t) (get t :created-day)) last-week))
                (daily-counts (map (lambda (day) (length (get by-day day []))) 
                                  (range 0 7))))
            (/ (reduce + daily-counts) 7))
          5))))

; Generate task suggestions based on patterns
(define generate-suggestions
  (lambda (model current-todos)
    (let ((keywords (get (get model :patterns) :keywords))
          (common-words (sort-by (lambda (kv) (- (cdr kv))) 
                                (to-list keywords))))
      (take 3 (map (lambda (kw) 
                    (concat "Consider: " (car kw)))
                  common-words)))))

; Main todo component with AI features
(define TodoApp
  (lambda (props)
    (let ((todos (effect reactive:ref []))
          (input-value (effect reactive:ref ""))
          (filter-mode (effect reactive:ref "all"))
          (model (effect reactive:ref (create-learning-model))))
      
      ; Add todo with learning
      (let ((add-todo
             (lambda ()
               (let ((text (effect reactive:get input-value)))
                 (when (not (empty? text))
                   (let ((new-todo {:id (effect time:now)
                                   :text text
                                   :completed false
                                   :created-at (effect time:now)
                                   :created-hour (time:hour (effect time:now))
                                   :created-day (time:day-of-week (effect time:now))}))
                     (do
                       ; Add to todos
                       (effect reactive:update todos 
                         (lambda (ts) (cons new-todo ts)))
                       
                       ; Update model
                       (effect reactive:update model
                         (lambda (m)
                           (let ((updated-model (set m :task-history 
                                                   (cons new-todo (get m :task-history)))))
                             (set updated-model :patterns 
                                  (analyze-patterns updated-model)))))
                       
                       ; Clear input
                       (effect reactive:set input-value "")))))))
        
        ; Toggle todo with learning
        (let ((toggle-todo
               (lambda (id)
                 (do
                   (effect reactive:update todos
                     (lambda (ts)
                       (map (lambda (todo)
                              (if (= (get todo :id) id)
                                  (let ((updated (set todo :completed 
                                                    (not (get todo :completed)))))
                                    (if (get updated :completed)
                                        (set updated :completion-time
                                             (- (effect time:now) 
                                                (get todo :created-at)))
                                        updated))
                                  todo))
                            ts)))
                   
                   ; Learn from completion
                   (effect reactive:update model
                     (lambda (m)
                       (let ((todo (find (lambda (t) (= (get t :id) id))
                                       (effect reactive:get todos))))
                         (if (and todo (get todo :completed))
                             (let ((patterns (get m :patterns)))
                               (set m :patterns
                                    (do
                                      (set patterns :total-completed 
                                           (+ (get patterns :total-completed) 1))
                                      patterns)))
                             m))))))))
          
          ; Render function
          (lambda ()
            (let ((all-todos (effect reactive:get todos))
                  (mode (effect reactive:get filter-mode))
                  (current-model (effect reactive:get model)))
              
              ; Filter todos
              (let ((filtered-todos
                     (cond
                       ((= mode "active") 
                        (filter (lambda (t) (not (get t :completed))) all-todos))
                       ((= mode "completed")
                        (filter (lambda (t) (get t :completed)) all-todos))
                       (else all-todos))))
                
                ; Calculate predictions
                (let ((predictions (get current-model :predictions))
                      (daily-capacity (predict-capacity current-model))
                      (suggestions (generate-suggestions current-model all-todos)))
                  
                  (dom:h "div" {:class "todo-app"}
                    [(dom:h "h1" {} [(dom:text "AI-Powered Todo App")])
                     
                     ; AI Insights Panel
                     (dom:h "div" {:class "insights-panel"}
                       [(dom:h "h3" {} 
                         [(dom:h "span" {:class "insight-icon"} [(dom:text "🤖")])
                          (dom:text "AI Insights")])
                        
                        ; Predictions
                        (dom:h "div" {:class "predictions"}
                          [(dom:h "div" {:class "prediction-card"}
                            [(dom:h "div" {:class "prediction-value"} 
                              [(dom:text (to-string (round daily-capacity)))])
                             (dom:h "div" {:class "prediction-label"} 
                              [(dom:text "Tasks you can complete today")])])
                           
                           (dom:h "div" {:class "prediction-card"}
                            [(dom:h "div" {:class "prediction-value"} 
                              [(dom:text (get predictions :best-work-time))])
                             (dom:h "div" {:class "prediction-label"} 
                              [(dom:text "Your most productive time")])])
                           
                           (dom:h "div" {:class "prediction-card"}
                            [(dom:h "div" {:class "prediction-value"} 
                              [(dom:text (length (filter (lambda (t) (get t :completed)) 
                                                       all-todos)))])
                             (dom:h "div" {:class "prediction-label"} 
                              [(dom:text "Completed this week")])])])
                        
                        ; Suggestions
                        (when (not (empty? suggestions))
                          (dom:h "div" {:class "suggestions"}
                            (map (lambda (suggestion)
                                  (dom:h "div" {:class "suggestion"}
                                    [(dom:text suggestion)]))
                                suggestions)))])
                     
                     ; Input section
                     (dom:h "div" {:class "input-section"}
                       [(dom:h "input"
                          {:id "todo-input"
                           :type "text"
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
                     
                     ; Filters
                     (dom:h "div" {:class "filters"}
                       [(dom:h "button"
                          {:class (if (= mode "all") "active" "")
                           :onClick (lambda () 
                                     (effect reactive:set filter-mode "all"))}
                          [(dom:text "All")])
                        (dom:h "button"
                          {:class (if (= mode "active") "active" "")
                           :onClick (lambda () 
                                     (effect reactive:set filter-mode "active"))}
                          [(dom:text "Active")])
                        (dom:h "button"
                          {:class (if (= mode "completed") "active" "")
                           :onClick (lambda () 
                                     (effect reactive:set filter-mode "completed"))}
                          [(dom:text "Completed")])])
                     
                     ; Todo list
                     (dom:h "ul" {:class "todo-list"}
                       (map (lambda (todo)
                              (dom:h "li" 
                                {:class (if (get todo :completed) 
                                          "todo-item completed" 
                                          "todo-item")
                                 :key (get todo :id)}
                                [(dom:h "input"
                                   {:type "checkbox"
                                    :checked (if (get todo :completed) "checked" nil)
                                    :onChange (lambda () (toggle-todo (get todo :id)))})
                                 (dom:h "span" {} [(dom:text (get todo :text))])
                                 (dom:h "button"
                                   {:onClick (lambda () 
                                              (effect reactive:update todos
                                                (lambda (ts)
                                                  (filter (lambda (t)
                                                           (not= (get t :id) 
                                                                 (get todo :id)))
                                                         ts))))}
                                   [(dom:text "Delete")])]))
                            filtered-todos))
                     
                     ; Stats
                     (let ((active-count (length (filter (lambda (t) 
                                                          (not (get t :completed)))
                                                       all-todos))))
                       (dom:h "div" {:class "stats"}
                         [(dom:text (concat (to-string active-count) 
                                          " item(s) left"))]))]))))))))))

; Create the app
(define create-app TodoApp)

; Main entry point
(define main
  (lambda ()
    (let ((app (create-app {})))
      (effect dom:render (app) "#app"))))