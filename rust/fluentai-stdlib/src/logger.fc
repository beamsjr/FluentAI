;; logger.cl - Simple logging module for FluentAi
;; Provides log levels and structured logging on top of the IO effect system

(module logger (export log debug info warn error with-logger set-log-level)
  
  ;; Log levels as constants
  (define DEBUG 0)
  (define INFO 1)
  (define WARN 2)
  (define ERROR 3)
  
  ;; Current log level (mutable state)
  (define *current-log-level* (ref INFO))
  
  ;; Format a log message with timestamp and level
  (define format-log-message
    (lambda (level message data)
      (let ((timestamp (effect time:now))
            (level-str (case level
                        (0 "DEBUG")
                        (1 "INFO")
                        (2 "WARN")
                        (3 "ERROR")
                        (_ "UNKNOWN"))))
        (str "[" timestamp "] [" level-str "] " message
             (if data (str " " (json:encode data)) "")))))
  
  ;; Core logging function
  (define log
    (lambda (level message . data)
      (when (>= level (deref *current-log-level*))
        (let ((formatted (format-log-message level message 
                                           (if (empty? data) nil (first data)))))
          (effect io:print-line formatted)))))
  
  ;; Convenience functions for each log level
  (define debug
    (lambda (message . data)
      (apply log DEBUG message data)))
  
  (define info
    (lambda (message . data)
      (apply log INFO message data)))
  
  (define warn
    (lambda (message . data)
      (apply log WARN message data)))
  
  (define error
    (lambda (message . data)
      (apply log ERROR message data)))
  
  ;; Set the current log level
  (define set-log-level
    (lambda (level)
      (ref-set! *current-log-level* level)))
  
  ;; Handler for custom logging behavior
  (define with-logger
    (lambda (handler body)
      (handler
        ((io (lambda (op . args)
               (if (= op "print-line")
                   (handler (first args))
                   (apply effect io op args)))))
        (body)))))

;; Usage examples:
;; (import "logger" *)
;; (logger:info "Application started")
;; (logger:debug "Processing item" {:id 123 :name "test"})
;; (logger:error "Failed to connect" {:host "example.com" :port 443})
;; (logger:set-log-level logger:DEBUG)