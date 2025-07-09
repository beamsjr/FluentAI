# Implement Network Effects (HTTP Client/Server)

## Overview

Add built-in networking capabilities through the effect system, enabling HTTP client requests and server creation without external dependencies.

## Design

### HTTP Client Effects

```lisp
;; Basic GET request
(effect http:get "https://api.example.com/users")
;; => {:status 200 :body "..." :headers {...}}

;; POST with JSON body
(effect http:post "https://api.example.com/users"
  {:headers {"Content-Type" "application/json"}
   :body {:name "Alice" :email "alice@example.com"}})

;; Other methods
(effect http:put url options)
(effect http:delete url options)
(effect http:patch url options)

;; With error handling
(handler
  ((error (lambda (err)
            (case (get err :type)
              "timeout" (retry-with-backoff)
              "network" {:status 0 :error err}
              _ (raise err)))))
  (effect http:get url {:timeout 5000}))
```

### HTTP Server Effects

```lisp
;; Create a simple server
(effect http:serve 8080
  (lambda (req)
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body "Hello, World!"}))

;; Router-based server
(let ((router (http:router)))
  (http:route router "GET" "/" 
    (lambda (req) {:status 200 :body "Home"}))
  
  (http:route router "GET" "/users/:id"
    (lambda (req)
      (let ((id (get-in req [:params :id])))
        {:status 200 
         :body (get-user id)})))
  
  (http:route router "POST" "/users"
    (lambda (req)
      (let ((user (parse-json (get req :body))))
        (create-user user)
        {:status 201 :body user})))
  
  (effect http:serve 8080 router))

;; Middleware support
(let ((app (http:middleware
             logging-middleware
             auth-middleware
             cors-middleware
             router)))
  (effect http:serve 8080 app))
```

### WebSocket Support

```lisp
;; WebSocket client
(let ((ws (effect ws:connect "ws://localhost:8080/socket")))
  (effect ws:on-message ws
    (lambda (msg) (print "Received:" msg)))
  
  (effect ws:send ws "Hello, server!")
  
  (effect ws:close ws))

;; WebSocket server
(effect ws:serve 8080
  {:on-connect (lambda (client)
                 (print "Client connected:" client))
   :on-message (lambda (client msg)
                 (effect ws:send client (str "Echo: " msg)))
   :on-close (lambda (client)
               (print "Client disconnected:" client))})
```

### Advanced Features

```lisp
;; Request streaming
(effect http:stream-get url
  (lambda (chunk)
    (process-chunk chunk)))

;; File uploads
(effect http:upload url
  {:file "/path/to/file.pdf"
   :field-name "document"
   :on-progress (lambda (percent)
                  (update-progress-bar percent))})

;; Connection pooling
(let ((client (http:client
                {:max-connections 10
                 :timeout 30000
                 :keep-alive true})))
  (effect http:get url {:client client}))

;; HTTPS with custom certificates
(effect http:get url
  {:tls {:ca-file "/path/to/ca.pem"
         :verify-peer true}})
```

## Implementation Tasks

### Phase 1: Basic HTTP Client
- [ ] Implement GET, POST, PUT, DELETE, PATCH
- [ ] Add request/response headers support
- [ ] JSON body serialization/deserialization
- [ ] Basic error handling
- [ ] Timeout support

### Phase 2: HTTP Server
- [ ] Basic request handler
- [ ] Router implementation
- [ ] Path parameter extraction
- [ ] Query string parsing
- [ ] Static file serving

### Phase 3: Advanced Client Features
- [ ] Connection pooling
- [ ] Request retries with backoff
- [ ] Streaming responses
- [ ] File uploads/downloads
- [ ] Cookie handling

### Phase 4: Advanced Server Features
- [ ] Middleware system
- [ ] WebSocket support
- [ ] Server-sent events (SSE)
- [ ] HTTP/2 support
- [ ] TLS/HTTPS configuration

### Phase 5: Integration
- [ ] Integration with async/await
- [ ] Effect handler customization
- [ ] Performance optimization
- [ ] Comprehensive testing
- [ ] Documentation and examples

## Technical Considerations

- Use tokio/hyper for async HTTP in Rust
- Ensure proper resource cleanup
- Handle backpressure for streaming
- Security considerations (CORS, CSRF, etc.)
- Connection pooling for performance

## Examples Needed

1. **REST API Client** - CRUD operations
2. **Web Server** - Full REST API implementation
3. **WebSocket Chat** - Real-time communication
4. **File Upload Service** - Multipart handling
5. **API Gateway** - Proxy and routing
6. **GraphQL Server** - Complex query handling

## Priority

**High** - Network capabilities are essential for modern applications

## Labels

- enhancement
- effects
- networking
- high-priority