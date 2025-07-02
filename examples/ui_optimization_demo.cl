; ClaudeLang UI Optimization Demo
; This demonstrates how the UI optimizer learns from component render patterns

; Heavy data grid component that could benefit from optimization
(ui:component "DataGrid"
  {:data (prop :list :required true)
   :columns (prop :list :required true)
   :sortColumn (prop :string :default "")
   :filterText (prop :string :default "")}
  (lambda (props)
    (let ((data (get props "data"))
          (columns (get props "columns"))
          (sortCol (get props "sortColumn"))
          (filterText (get props "filterText")))
      
      ; Filter data if needed
      (let ((filtered-data 
             (if (empty? filterText)
                 data
                 (filter (lambda (row)
                          (reduce or false
                            (map (lambda (col)
                                  (let ((value (to-string (get row (get col "key")))))
                                    (> (length value) 0)))
                                 columns)))
                        data))))
        
        ; Sort data if needed
        (let ((sorted-data
               (if (empty? sortCol)
                   filtered-data
                   filtered-data))  ; Simplified - real sorting would be here
          
          ; Render the grid
          (dom:h "div" (set {} "class" "data-grid")
            [(dom:h "table" {}
               [(dom:h "thead" {}
                  [(dom:h "tr" {}
                     (map (lambda (col)
                            (dom:h "th" 
                              (set {} "onClick" 
                                   (lambda () (effect io:print (concat "Sort by " (get col "name")))))
                              [(dom:text (get col "name"))]))
                          columns))])
                (dom:h "tbody" {}
                  (map (lambda (row)
                         (dom:h "tr" {}
                           (map (lambda (col)
                                  (dom:h "td" {}
                                    [(dom:text (to-string (get row (get col "key"))))]))
                                columns)))
                       sorted-data))])]))))))

; Optimized version with memoization hints
(ui:component "OptimizedDataGrid"
  {:data (prop :list :required true)
   :columns (prop :list :required true)
   :sortColumn (prop :string :default "")
   :filterText (prop :string :default "")
   :theme (prop :string :default "light")  ; Rarely changes
   :locale (prop :string :default "en")}   ; Rarely changes
  (lambda (props)
    ; The optimizer will detect that theme and locale rarely change
    ; and suggest memoizing renders when only these props differ
    (ui:create "DataGrid" 
      (set (set (set (set {} "data" (get props "data"))
                     "columns" (get props "columns"))
                "sortColumn" (get props "sortColumn"))
           "filterText" (get props "filterText")))))

; Component with burst updates that could benefit from batching
(ui:component "LiveDataView"
  {:dataStream (prop :object :required true)}
  (lambda (props)
    (let ((data (effect state:reactive-ref []))
          (updateCount (effect state:reactive-ref 0)))
      
      ; Simulate receiving burst updates
      (effect state:reactive-watch [(get props "dataStream")]
        (lambda ()
          ; Multiple state updates in quick succession
          (effect state:reactive-update updateCount (lambda (c) (+ c 1)))
          (effect state:reactive-update data (lambda (d) 
            (cons (get props "dataStream") d)))))
      
      (dom:h "div" (set {} "class" "live-data")
        [(dom:h "h3" {} [(dom:text "Live Data Stream")])
         (dom:h "p" {} [(dom:text (concat "Updates: " 
                                        (to-string (effect state:reactive-get updateCount))))])
         (dom:h "ul" {}
           (ui:for (effect state:reactive-get data)
             (lambda (item)
               (dom:h "li" {} [(dom:text (to-string item))]))))]))))

; Component that could be split for better performance
(ui:component "Dashboard"
  {:userProfile (prop :object :required true)
   :analytics (prop :object :required true)
   :notifications (prop :list :default [])}
  (lambda (props)
    (dom:h "div" (set {} "class" "dashboard")
      [; Header - rarely changes
       (dom:h "header" {}
         [(dom:h "h1" {} [(dom:text (get (get props "userProfile") "name"))])
          (dom:h "img" (set (set {} "src" (get (get props "userProfile") "avatar"))
                           "class" "avatar") [])])
       
       ; Analytics - changes frequently
       (dom:h "section" (set {} "class" "analytics")
         [(dom:h "h2" {} [(dom:text "Analytics")])
          (dom:h "div" (set {} "class" "stats")
            [(dom:h "div" {} 
               [(dom:text (concat "Views: " 
                                (to-string (get (get props "analytics") "views"))))])
             (dom:h "div" {}
               [(dom:text (concat "Clicks: " 
                                (to-string (get (get props "analytics") "clicks"))))])])])
       
       ; Notifications - changes very frequently
       (dom:h "section" (set {} "class" "notifications")
         [(dom:h "h2" {} [(dom:text "Notifications")])
          (dom:h "ul" {}
            (map (lambda (notif)
                   (dom:h "li" {} [(dom:text (get notif "message"))]))
                 (get props "notifications")))])])))

; Demo app that uses these components
(let ((grid-data (effect state:reactive-ref 
                   [(set (set {} "id" 1) "name" "Item 1" "value" 100)
                    (set (set {} "id" 2) "name" "Item 2" "value" 200)
                    (set (set {} "id" 3) "name" "Item 3" "value" 150)]))
      (columns [{:key "id" :name "ID"}
                {:key "name" :name "Name"}
                {:key "value" :name "Value"}])
      (stream-data (effect state:reactive-ref {}))
      (user {:name "Demo User" :avatar "/avatar.png"})
      (analytics (effect state:reactive-ref {:views 1000 :clicks 50})))
  
  ; Simulate data updates
  (effect state:reactive-watch []
    (lambda ()
      ; Update analytics every second
      (effect time:set-interval
        (lambda ()
          (effect state:reactive-update analytics
            (lambda (a)
              (set (set a "views" (+ (get a "views") 1))
                   "clicks" (+ (get a "clicks") 
                             (if (> (effect math:random) 0.9) 1 0))))))
        1000)))
  
  ; Render the demo
  (dom:h "div" (set {} "class" "optimization-demo")
    [(dom:h "h1" {} [(dom:text "UI Optimization Demo")])
     
     ; Data grid that will be analyzed for optimization
     (ui:create "OptimizedDataGrid"
       (set (set (set {} "data" (effect state:reactive-get grid-data))
                 "columns" columns)
            "theme" "light"))
     
     ; Dashboard that could benefit from splitting
     (ui:create "Dashboard"
       (set (set {} "userProfile" user)
            "analytics" (effect state:reactive-get analytics)
            "notifications" []))
     
     ; Render metrics display
     (dom:h "div" (set {} "class" "metrics")
       [(dom:h "h2" {} [(dom:text "Render Metrics")])
        (dom:h "pre" {}
          [(dom:text "Check console for ClaudeLang.getRenderMetrics()")])])]))