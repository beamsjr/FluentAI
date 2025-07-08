# Network Effects in FluentAI

FluentAI provides comprehensive network effect handlers for building networked applications. All network operations are performed through the effect system, ensuring they are explicit, composable, and testable.

## Overview

Network effects in FluentAI cover:
- HTTP/HTTPS requests (GET, POST, PUT, DELETE, etc.)
- WebSocket connections
- TCP/UDP socket programming
- DNS operations
- URL parsing and encoding

## HTTP Operations

### Basic HTTP Requests

```lisp
; Simple GET request
(let ((response (effect network:http-get "https://api.example.com/data")))
  (print "Status:" (get response "status"))
  (print "Body:" (get response "body")))

; POST with JSON data
(let ((user {"name" "Alice" "email" "alice@example.com"})
      (response (effect network:http-post "https://api.example.com/users" user)))
  (print "Created with status:" (get response "status")))

; Custom headers
(let ((headers {"Authorization" "Bearer token123"
                "User-Agent" "FluentAI/1.0"})
      (response (effect network:http-get "https://api.example.com/protected" headers)))
  (print response))

; Full request control
(let ((request {"method" "PUT"
                "url" "https://api.example.com/users/123"
                "headers" {"Content-Type" "application/json"}
                "body" (json-stringify {"name" "Alice Updated"})
                "timeout" 10.0
                "follow_redirects" true})
      (response (effect network:http-request request)))
  (print response))
```

### Response Format

HTTP responses contain:
- `status`: HTTP status code (200, 404, etc.)
- `headers`: Response headers as a dictionary
- `body`: Response body as string
- `url`: Final URL after redirects
- `elapsed`: Time taken for request

## WebSocket Operations

### WebSocket Client

```lisp
; Connect to WebSocket
(let ((ws (effect network:ws-connect "wss://echo.websocket.org/")))
  (do
    ; Send message
    (effect network:ws-send ws "Hello, WebSocket!")
    
    ; Receive message (with optional timeout)
    (let ((msg (effect network:ws-receive ws 5.0)))
      (when msg
        (print "Type:" (get msg "type"))      ; "text" or "binary"
        (print "Data:" (get msg "data"))
        (print "Time:" (get msg "timestamp"))))
    
    ; Close connection
    (effect network:ws-close ws 1000 "Goodbye")))
```

### WebSocket Patterns

```lisp
; Pub/Sub pattern
(let ((subscribe-to-channel (lambda (channel handler)
                             (let ((ws (effect network:ws-connect "wss://pubsub.example.com")))
                               (do
                                 ; Subscribe
                                 (effect network:ws-send ws 
                                        (json-stringify {"action" "subscribe" 
                                                        "channel" channel}))
                                 
                                 ; Message loop
                                 (let ((loop (lambda ()
                                              (let ((msg (effect network:ws-receive ws)))
                                                (when msg
                                                  (handler (json-parse (get msg "data")))
                                                  (loop))))))
                                   (loop)))))))
  
  ; Use it
  (subscribe-to-channel "news" 
                       (lambda (data) 
                         (print "News update:" (get data "headline")))))
```

## Socket Programming

### TCP Server

```lisp
; Simple echo server
(let ((server (effect network:socket-create "tcp")))
  (do
    ; Bind and listen
    (effect network:socket-bind server "0.0.0.0" 8080)
    (effect network:socket-listen server 5)
    (print "Server listening on port 8080")
    
    ; Accept loop
    (let ((serve-client (lambda (client-info)
                         (let ((client (get client-info "socket_id"))
                               (addr (get client-info "remote_addr")))
                           (do
                             (print "Client connected from:" addr)
                             
                             ; Echo loop
                             (let ((echo-loop (lambda ()
                                               (let ((data (effect network:socket-receive client 1024)))
                                                 (when (> (length data) 0)
                                                   (effect network:socket-send client data)
                                                   (echo-loop))))))
                               (echo-loop))
                             
                             (effect network:socket-close client))))))
      
      ; Accept clients
      (let ((accept-loop (lambda (n)
                          (when (> n 0)
                            (let ((client (effect network:socket-accept server)))
                              (parallel
                                (serve-client client)  ; Handle in parallel
                                (accept-loop (- n 1))))))))
        (accept-loop 10)))  ; Handle 10 clients
    
    (effect network:socket-close server)))
```

### TCP Client

```lisp
; Connect to server
(let ((client (effect network:socket-create "tcp")))
  (do
    (effect network:socket-connect client "example.com" 80)
    
    ; Send HTTP request
    (effect network:socket-send client "GET / HTTP/1.0\r\n\r\n")
    
    ; Read response
    (let ((response (effect network:socket-receive client 4096)))
      (print "Received:" (bytes-to-string response)))
    
    (effect network:socket-close client)))
```

### UDP Socket

```lisp
; UDP client/server
(let ((sock (effect network:socket-create "udp")))
  (do
    ; Bind for receiving
    (effect network:socket-bind sock "0.0.0.0" 9999)
    
    ; Send datagram
    (effect network:socket-send-to sock "Hello UDP!" "localhost" 9999)
    
    ; Receive datagram
    (let ((data (effect network:socket-receive-from sock 1024)))
      (print "Data:" (get data "data"))
      (print "From:" (get data "address")))
    
    (effect network:socket-close sock)))
```

## DNS Operations

```lisp
; Forward DNS lookup
(let ((ips (effect network:dns-lookup "example.com")))
  (print "IP addresses:" ips))  ; ["93.184.216.34", ...]

; Reverse DNS lookup
(let ((hostname (effect network:dns-reverse "8.8.8.8")))
  (print "Hostname:" hostname))  ; "dns.google"
```

## URL Operations

```lisp
; Parse URL
(let ((parsed (effect network:url-parse 
                     "https://user:pass@example.com:8080/path?key=value#section")))
  (print "Scheme:" (get parsed "scheme"))        ; "https"
  (print "Host:" (get parsed "hostname"))        ; "example.com"
  (print "Port:" (get parsed "port"))            ; 8080
  (print "Path:" (get parsed "path"))            ; "/path"
  (print "Query:" (get parsed "query_params")))  ; {"key": ["value"]}

; Encode parameters
(let ((params {"name" "John Doe" "age" 30})
      (encoded (effect network:url-encode params)))
  (print encoded))  ; "name=John+Doe&age=30"

; Decode parameters
(let ((decoded (effect network:url-decode "name=John+Doe&age=30")))
  (print decoded))  ; {"name": ["John Doe"], "age": ["30"]}
```

## Error Handling

Network operations can fail. Use error handlers:

```lisp
(handler
  ((error (lambda (err)
            (let ((type (get err "type"))
                  (msg (get err "message")))
              (cond
                ((= type "network-error") (print "Network error:" msg))
                ((= type "dns-error") (print "DNS error:" msg))
                ((= type "socket-error") (print "Socket error:" msg))
                ((= type "rate-limit") (print "Rate limited:" msg))
                (else (print "Unknown error:" msg)))))))
  
  ; Network operations that might fail
  (effect network:http-get "https://invalid.example.com"))
```

## Security Features

### Host Restrictions

```lisp
; Create restricted network handler
(let ((handler (create-network-handler 
                 :allow-external true
                 :allowed-hosts ["api.mycompany.com" "*.trusted.com"])))
  ; Only allowed hosts will work
  (with-handler handler
    (effect network:http-get "https://api.mycompany.com/data")))
```

### Rate Limiting

The network handler includes automatic rate limiting:
- Default: 100 requests per minute per host
- Configurable per handler instance
- Returns rate-limit errors when exceeded

### SSL/TLS Verification

```lisp
; Control SSL verification
(let ((request {"method" "GET"
                "url" "https://self-signed.example.com"
                "verify_ssl" false}))  ; Disable for self-signed certs
  (effect network:http-request request))
```

## Testing with Mocks

### Mock HTTP Responses

```lisp
; Set up mock
(effect network:mock-response "GET" "https://api.test.com/users" 
        {"status" 200
         "headers" {"Content-Type" "application/json"}
         "body" "[{\"id\": 1, \"name\": \"Alice\"}]"})

; Request will use mock
(let ((response (effect network:http-get "https://api.test.com/users")))
  (print (json-parse (get response "body"))))
```

### Mock WebSocket Messages

```lisp
; Queue mock messages
(effect network:mock-ws-message "wss://test.com/ws" "Hello from mock!")
(effect network:mock-ws-message "wss://test.com/ws" "Another message")

; Connect and receive mocked messages
(let ((ws (effect network:ws-connect "wss://test.com/ws")))
  (print (effect network:ws-receive ws))  ; "Hello from mock!"
  (print (effect network:ws-receive ws))) ; "Another message"
```

## Advanced Patterns

### Parallel Requests

```lisp
; Fetch multiple URLs in parallel
(let ((urls ["https://api1.com/data" 
             "https://api2.com/data" 
             "https://api3.com/data"])
      (responses (parallel-map 
                   (lambda (url) (effect network:http-get url))
                   urls)))
  (print "Fetched" (length responses) "responses"))
```

### Request with Retry

```lisp
(let ((with-retry (lambda (request max-attempts delay)
                   (let ((attempt (lambda (n)
                                   (handler
                                     ((error (lambda (err)
                                              (if (< n max-attempts)
                                                  (do
                                                    (print "Retry" n "after" delay "seconds")
                                                    (effect time:sleep delay)
                                                    (attempt (+ n 1)))
                                                  (error err)))))
                                     (request)))))
                     (attempt 1)))))
  
  ; Use with exponential backoff
  (with-retry 
    (lambda () (effect network:http-get "https://flaky-api.com/data"))
    3
    2.0))
```

### Connection Pooling

```lisp
; Connection pool for reuse
(let ((pool (create-connection-pool 
              :max-connections 10
              :keepalive true)))
  
  ; Requests will reuse connections
  (with-pool pool
    (map (lambda (id)
           (effect network:http-get 
                  (string-concat "https://api.example.com/item/" id)))
         (range 1 100))))
```

## Performance Considerations

1. **Connection Reuse**: HTTP connections are reused when possible
2. **Parallel Requests**: Use `parallel` for concurrent requests
3. **Streaming**: Large responses can be streamed (not loaded fully into memory)
4. **Timeouts**: Always set appropriate timeouts
5. **Rate Limiting**: Built-in rate limiting prevents overwhelming servers

## Best Practices

1. **Always handle errors**: Network operations can fail
2. **Set timeouts**: Prevent hanging on slow networks
3. **Use HTTPS**: For security when possible
4. **Mock in tests**: Use mock responses for testing
5. **Rate limit**: Be respectful of API limits
6. **Log requests**: Use request history for debugging
7. **Validate inputs**: Check URLs and data before sending

## Example: REST API Client

```lisp
(let ((create-api-client (lambda (base-url api-key)
                          (let ((request (lambda (method path data)
                                          (let ((url (string-concat base-url path))
                                                (headers {"Authorization" 
                                                         (string-concat "Bearer " api-key)
                                                         "Content-Type" "application/json"})
                                                (req {"method" method
                                                      "url" url
                                                      "headers" headers
                                                      "body" (if data (json-stringify data) nil)
                                                      "timeout" 30.0}))
                                            (let ((response (effect network:http-request req)))
                                              (if (and (>= (get response "status") 200)
                                                      (< (get response "status") 300))
                                                  (json-parse (get response "body"))
                                                  (error (string-concat "API error: " 
                                                                       (get response "status")))))))))
                    
                    ; Return API methods
                    {"get" (lambda (path) (request "GET" path nil))
                     "post" (lambda (path data) (request "POST" path data))
                     "put" (lambda (path data) (request "PUT" path data))
                     "delete" (lambda (path) (request "DELETE" path nil))}))))
  
  ; Use the client
  (let ((api (create-api-client "https://api.example.com" "my-api-key")))
    (do
      ; Get users
      (let ((users ((get api "get") "/users")))
        (print "Users:" users))
      
      ; Create user
      (let ((new-user ((get api "post") "/users" {"name" "Bob" "email" "bob@example.com"})))
        (print "Created:" new-user))
      
      ; Update user
      ((get api "put") "/users/123" {"name" "Bob Updated"})
      
      ; Delete user
      ((get api "delete") "/users/123"))))