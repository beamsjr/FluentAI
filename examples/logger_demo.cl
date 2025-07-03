;; logger_demo.cl - Demonstrates the ClaudeLang logging system

(import "logger" *)

;; Set log level to DEBUG to see all messages
(logger:set-log-level logger:DEBUG)

;; Log at different levels
(logger:debug "Application starting" {:version "1.0.0" :env "development"})
(logger:info "Configuration loaded successfully")
(logger:warn "Using default database connection" {:host "localhost" :port 5432})

;; Simulate processing with structured logging
(define process-user 
  (lambda (user-id)
    (logger:info "Processing user" {:user-id user-id :timestamp (effect time:now)})
    
    ;; Simulate some work
    (handler
      ((error (lambda (err)
                (logger:error "Failed to process user" 
                  {:user-id user-id 
                   :error (get err :message)
                   :stack-trace (get err :stack)})
                nil)))
      
      (if (< user-id 0)
          (effect error:raise "invalid-user" {:message "User ID must be positive"})
          (do
            (logger:debug "Fetching user data" {:user-id user-id})
            ;; Simulate processing
            (effect time:sleep 0.1)
            (logger:debug "User data fetched" {:user-id user-id})
            {:id user-id :name (str "User " user-id) :processed true})))))

;; Process some users
(logger:info "Starting batch processing")
(let ((users [1 2 -1 3 4]))
  (map process-user users))

;; Change log level to INFO (hiding debug messages)
(logger:info "Changing log level to INFO")
(logger:set-log-level logger:INFO)

;; These debug messages won't appear
(logger:debug "This debug message is hidden")
(logger:info "But info messages still appear")

;; Log with just a message (no structured data)
(logger:warn "Low memory warning")
(logger:error "Connection timeout")

;; Custom logging handler example
(define with-json-logger
  (lambda (body)
    (handler
      ((io (lambda (op . args)
             (if (and (= op "print-line") 
                      (string:starts-with? (first args) "["))
                 ;; Convert to JSON format
                 (let ((msg (first args)))
                   (effect io:print-line 
                     (json:encode {:log msg :source "claudelang"})))
                 ;; Pass through other IO
                 (apply effect io op args)))))
      (body))))

;; Use custom logger
(with-json-logger
  (lambda ()
    (logger:info "This will be wrapped in JSON")))