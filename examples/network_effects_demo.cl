; Network Effects Demo
; This demonstrates various network operations in ClaudeLang

; 1. Simple HTTP GET request
(let ((response (effect network:http-get "https://api.example.com/data")))
  (print "Status:" (get response "status"))
  (print "Body:" (get response "body")))

; 2. HTTP POST with JSON data
(let ((user-data {"name" "Alice" "email" "alice@example.com"})
      (response (effect network:http-post 
                       "https://api.example.com/users"
                       user-data)))
  (print "Created user with status:" (get response "status")))

; 3. WebSocket connection
(let ((ws-id (effect network:ws-connect "wss://echo.websocket.org/")))
  (do
    ; Send a message
    (effect network:ws-send ws-id "Hello, WebSocket!")
    
    ; Receive response
    (let ((msg (effect network:ws-receive ws-id 5.0)))
      (print "Received:" (get msg "data")))
    
    ; Close connection
    (effect network:ws-close ws-id)))

; 4. DNS lookup
(let ((ips (effect network:dns-lookup "example.com")))
  (print "IP addresses for example.com:" ips))

; 5. URL parsing
(let ((parsed (effect network:url-parse 
                     "https://example.com:8080/path?key=value#section")))
  (print "Scheme:" (get parsed "scheme"))
  (print "Host:" (get parsed "hostname"))
  (print "Port:" (get parsed "port"))
  (print "Path:" (get parsed "path"))
  (print "Query params:" (get parsed "query_params")))

; 6. TCP socket server (echo server)
(let ((server-socket (effect network:socket-create "tcp")))
  (do
    ; Bind to port
    (effect network:socket-bind server-socket "0.0.0.0" 8888)
    (effect network:socket-listen server-socket 5)
    (print "Server listening on port 8888")
    
    ; Accept one connection
    (let ((client-info (effect network:socket-accept server-socket)))
      (let ((client-socket (get client-info "socket_id"))
            (remote-addr (get client-info "remote_addr")))
        (print "Client connected from:" remote-addr)
        
        ; Echo received data
        (let ((data (effect network:socket-receive client-socket 1024)))
          (effect network:socket-send client-socket data))
        
        ; Close client
        (effect network:socket-close client-socket)))
    
    ; Close server
    (effect network:socket-close server-socket)))

; 7. HTTP with custom headers and error handling
(handler
  ((error (lambda (err)
            (print "Network error:" (get err "message"))
            nil)))
  
  (let ((headers {"Authorization" "Bearer token123"
                  "User-Agent" "ClaudeLang/1.0"})
        (response (effect network:http-get 
                         "https://api.example.com/protected"
                         headers)))
    (if response
        (print "Protected data:" (get response "body"))
        (print "Failed to fetch protected data"))))

; 8. Parallel HTTP requests
(let ((urls ["https://api1.example.com/data"
             "https://api2.example.com/data"
             "https://api3.example.com/data"])
      (fetch-all (lambda (url-list)
                   (if (null? url-list)
                       []
                       (parallel
                         (effect network:http-get (car url-list))
                         (fetch-all (cdr url-list)))))))
  (let ((responses (fetch-all urls)))
    (print "Fetched" (length responses) "responses in parallel")))

; 9. Rate-limited requests
(let ((make-request (lambda (n)
                      (do
                        (print "Request" n)
                        (effect network:http-get 
                               (string-concat "https://api.example.com/item/" 
                                            (to-string n)))
                        (effect time:sleep 0.1)))))  ; Rate limit
  (map make-request [1 2 3 4 5]))

; 10. Mock mode for testing
(do
  ; Set up mock response
  (effect network:mock-response "GET" "https://test.com/api" 
          {"status" 200 
           "body" "{\"result\": \"mocked\"}"
           "headers" {"Content-Type" "application/json"}})
  
  ; Make request that gets mocked response
  (let ((response (effect network:http-get "https://test.com/api")))
    (print "Mock response:" (get response "body"))))