# Concurrency in ClaudeLang

ClaudeLang provides Go-inspired concurrency primitives that enable safe, efficient concurrent programming. The concurrency model is based on communicating sequential processes (CSP) with channels for communication and goroutines for lightweight concurrent execution.

## Overview

Key concurrency features:
- **Goroutines**: Lightweight threads for concurrent execution
- **Channels**: Type-safe communication between goroutines
- **Select**: Non-blocking operations on multiple channels
- **Synchronization**: Mutexes, semaphores, and barriers
- **Patterns**: Producer-consumer, pipelines, fan-out/fan-in

## Goroutines

Goroutines are lightweight threads managed by the ClaudeLang runtime. They enable concurrent execution with minimal overhead.

### Launching Goroutines

Use the `go` form to launch a goroutine:

```lisp
; Launch a simple goroutine
(go (print "Hello from goroutine!"))

; Launch with more complex logic
(go (do
      (effect time:sleep 1.0)
      (print "Done sleeping")))
```

### Goroutine Example

```lisp
; Concurrent computation
(let ((results (chan 3)))
  (do
    ; Launch 3 goroutines
    (go (send! results (compute-task-1)))
    (go (send! results (compute-task-2)))
    (go (send! results (compute-task-3)))
    
    ; Collect results
    [(receive! results)
     (receive! results)
     (receive! results)]))
```

## Channels

Channels are the primary means of communication between goroutines. They provide type-safe, synchronized communication.

### Creating Channels

```lisp
; Unbuffered channel (synchronous)
(let ((ch (chan)))
  ...)

; Buffered channel (asynchronous up to capacity)
(let ((ch (chan 10)))  ; Buffer size 10
  ...)
```

### Channel Operations

#### Send

```lisp
; Send a value (blocks if channel is full)
(send! ch value)

; Returns true if sent, false if channel is closed
(let ((sent (send! ch value)))
  (if sent
      (print "Value sent")
      (print "Channel closed")))
```

#### Receive

```lisp
; Receive a value (blocks if channel is empty)
(let ((result (receive! ch)))
  (if (get result "ok")
      (print "Got:" (get result "value"))
      (print "Channel closed")))
```

#### Close

```lisp
; Close a channel
(effect concurrent:close ch)

; Closed channels:
; - Can't send to closed channel (error)
; - Can receive remaining values
; - Receive returns {"value": nil, "ok": false} when empty
```

### Channel Patterns

#### Unbuffered Channels (Synchronous)

```lisp
(let ((ch (chan)))
  (do
    ; Sender blocks until receiver is ready
    (go (send! ch "sync message"))
    
    ; Receiver blocks until sender sends
    (receive! ch)))
```

#### Buffered Channels (Asynchronous)

```lisp
(let ((ch (chan 3)))
  (do
    ; Can send up to 3 values without blocking
    (send! ch 1)
    (send! ch 2)
    (send! ch 3)
    ; (send! ch 4) would block
    
    ; Receive values
    (receive! ch)  ; 1
    (receive! ch)  ; 2
    (receive! ch))) ; 3
```

## Select Statement

Select enables non-blocking operations on multiple channels (not fully implemented in current version).

```lisp
; Select from multiple channel operations
(select
  ((receive! ch1) (lambda (val) (print "From ch1:" val)))
  ((receive! ch2) (lambda (val) (print "From ch2:" val)))
  ((send! ch3 value) (lambda () (print "Sent to ch3")))
  (default (print "No channel ready")))
```

## Synchronization Primitives

### Mutex

Mutexes provide mutual exclusion for protecting shared state:

```lisp
(let ((mu (effect concurrent:mutex))
      (counter 0))
  (do
    ; Critical section
    (effect concurrent:lock mu)
    (set! counter (+ counter 1))
    (effect concurrent:unlock mu)))
```

### Semaphore

Semaphores limit concurrent access to resources:

```lisp
; Allow max 3 concurrent operations
(let ((sem (effect concurrent:semaphore 3)))
  (do
    (effect concurrent:acquire sem)
    ; ... do work ...
    (effect concurrent:release sem)))
```

### Barrier

Barriers synchronize multiple goroutines at a rendezvous point:

```lisp
; Wait for 5 goroutines to reach barrier
(let ((barrier (effect concurrent:barrier 5)))
  (map (lambda (id)
        (go (do
              ; ... do work ...
              (effect concurrent:wait barrier)
              ; All 5 goroutines continue together
              )))
      [1 2 3 4 5]))
```

## Common Patterns

### Producer-Consumer

```lisp
(let ((jobs (chan 10))
      (results (chan 10)))
  (do
    ; Producer
    (go (do
          (map (lambda (i) (send! jobs i)) work-items)
          (effect concurrent:close jobs)))
    
    ; Consumers
    (map (lambda (id)
          (go (let ((consume (lambda ()
                              (let ((job (receive! jobs)))
                                (if (get job "ok")
                                    (do
                                      (send! results (process job))
                                      (consume))
                                    nil)))))
                (consume))))
        [1 2 3 4])  ; 4 workers
    
    ; Collect results
    (collect-results results)))
```

### Pipeline

```lisp
(let ((stage1 (chan))
      (stage2 (chan))
      (stage3 (chan)))
  (do
    ; Stage 1: Generate
    (go (pipeline-stage-1 stage1))
    
    ; Stage 2: Transform
    (go (pipeline-stage-2 stage1 stage2))
    
    ; Stage 3: Aggregate
    (go (pipeline-stage-3 stage2 stage3))
    
    ; Collect from final stage
    (collect-from stage3)))
```

### Fan-out/Fan-in

```lisp
; Fan-out: distribute work to multiple workers
(let ((work-ch (chan))
      (result-ch (chan)))
  (do
    ; Start workers
    (map (lambda (id)
          (go (worker id work-ch result-ch)))
        (range 1 num-workers))
    
    ; Send work
    (map (lambda (item) (send! work-ch item)) work-items)
    
    ; Collect results
    (map (lambda (_) (receive! result-ch)) work-items)))
```

### Worker Pool

```lisp
(let ((create-worker-pool (lambda (size task-fn)
                           (let ((tasks (chan 100))
                                 (results (chan 100)))
                             (do
                               ; Start workers
                               (map (lambda (id)
                                     (go (let ((work (lambda ()
                                                      (let ((task (receive! tasks)))
                                                        (if (get task "ok")
                                                            (do
                                                              (send! results 
                                                                    (task-fn (get task "value")))
                                                              (work))
                                                            nil)))))
                                           (work))))
                                   (range 1 size))
                               {"tasks" tasks "results" results})))))
  
  ; Use the pool
  (let ((pool (create-worker-pool 5 process-task)))
    (do
      ; Submit tasks
      (map (lambda (task) (send! (get pool "tasks") task)) all-tasks)
      (effect concurrent:close (get pool "tasks"))
      
      ; Collect results
      (collect-all (get pool "results")))))
```

## Best Practices

### 1. Channel Ownership

- Clearly define which goroutine owns sending/receiving
- Close channels from the sender side
- Multiple senders: use synchronization or a coordinator

### 2. Avoiding Deadlock

```lisp
; BAD: Potential deadlock
(let ((ch (chan)))
  (do
    (send! ch 1)    ; Blocks forever - no receiver
    (receive! ch))) ; Never reached

; GOOD: Use goroutine
(let ((ch (chan)))
  (do
    (go (send! ch 1))
    (receive! ch)))
```

### 3. Buffered vs Unbuffered

- **Unbuffered**: For synchronization
- **Buffered**: For performance when you know the capacity
- Don't use buffering to fix synchronization issues

### 4. Resource Cleanup

```lisp
; Always clean up resources
(let ((ch (chan))
      (mu (effect concurrent:mutex)))
  (handler
    ((finally (lambda ()
                (effect concurrent:close ch))))
    ; ... use resources ...
    ))
```

### 5. Goroutine Lifecycle

```lisp
; Ensure goroutines can terminate
(let ((done (chan))
      (work (chan)))
  (go (let ((loop (lambda ()
                   (select
                     ((receive! done) nil)  ; Exit signal
                     ((receive! work) 
                      (lambda (w) 
                        (process w)
                        (loop)))))))
        (loop))))
```

## Performance Considerations

1. **Goroutine overhead**: Very low, but don't create millions
2. **Channel operations**: Efficient but have synchronization cost
3. **Buffered channels**: Reduce contention but use memory
4. **Select**: More expensive than direct channel operations

## Debugging Concurrent Code

### 1. Race Detection

```lisp
; Use mutexes to protect shared state
(let ((mu (effect concurrent:mutex))
      (shared-state {}))
  ; All access must be synchronized
  (effect concurrent:lock mu)
  (let ((value (get shared-state "key")))
    (effect concurrent:unlock mu)
    value))
```

### 2. Deadlock Detection

Common causes:
- Circular channel dependencies
- Forgotten channel closes
- Lock ordering issues

### 3. Goroutine Leaks

```lisp
; Always provide a way to stop goroutines
(let ((stop (chan)))
  (go (let ((loop (lambda ()
                   (if (not (closed? stop))
                       (do
                         ; ... work ...
                         (loop))
                       nil))))
        (loop)))
  ; Later: stop the goroutine
  (effect concurrent:close stop))
```

## Examples

### Concurrent Web Scraper

```lisp
(let ((urls ["url1" "url2" "url3" ...])
      (results (chan (length urls))))
  (do
    ; Launch goroutine for each URL
    (map (lambda (url)
          (go (send! results (fetch-url url))))
        urls)
    
    ; Collect results
    (map (lambda (_) (receive! results)) urls)))
```

### Rate-Limited API Calls

```lisp
(let ((rate-limiter (chan 1))  ; 1 token
      (refill (lambda ()
               (effect time:sleep 1.0)
               (send! rate-limiter "token")
               (refill))))
  (do
    ; Start token refiller
    (go (refill))
    
    ; Make rate-limited calls
    (map (lambda (request)
          (do
            (receive! rate-limiter)  ; Wait for token
            (api-call request)))
        requests)))
```

### Parallel Map

```lisp
(let ((parallel-map (lambda (f items)
                     (let ((n (length items))
                           (ch (chan n)))
                       (do
                         ; Launch goroutine per item
                         (map (lambda (item)
                               (go (send! ch (f item))))
                             items)
                         ; Collect in order
                         (map (lambda (_)
                               (receive! ch))
                             items))))))
  (parallel-map expensive-function large-list))
```

## Limitations

Current implementation limitations:
- Select is simplified (not true non-blocking select)
- No channel direction types
- No goroutine IDs or management
- Limited debugging tools
- No built-in deadlock detection