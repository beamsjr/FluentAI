# Complete Actor Model Integration in FluentAI

## Overview

FluentAI has partial support for actor model concurrency primitives, but the implementation is incomplete. This issue tracks the work needed to provide full actor model support with async/await, channels, and message passing.

## Current State

### What Works ✅
- **Channels**: `(chan)` creates channels with proper ID tracking
- **Send/Receive**: Basic synchronous channel communication via `(send! channel value)` and `(recv! channel)`
- **Spawn**: `(spawn expr)` creates goroutines and returns promise IDs
- **Parser Support**: All async/await syntax is recognized

### What's Missing ❌
- **Await Bug**: `(await promise)` has identifier resolution bug (tries to resolve "promise:promise:1")
- **Async Blocks**: `(async expr)` doesn't create promises, executes immediately
- **Async Functions**: No support for defining async functions
- **Error Handling**: No proper error propagation for async operations
- **Advanced Patterns**: No support for select, timeout, or other concurrent patterns

## Implementation Tasks

### 1. Fix Core Async/Await Functionality
- [ ] Fix the await identifier bug in the VM
- [ ] Make `async` blocks create and return promises
- [ ] Implement proper continuation support for async execution
- [ ] Add async function definitions: `(async-fn (params) body)`

### 2. Enhanced Channel Operations
- [ ] Add buffered channels: `(chan capacity)`
- [ ] Implement non-blocking operations: `(try-send! ch val)`, `(try-recv! ch)`
- [ ] Add channel closing: `(close! ch)`
- [ ] Support for select/alt operations on multiple channels

### 3. Actor Model Primitives
- [ ] Implement actor creation: `(actor initial-state handler-fn)`
- [ ] Message sending to actors: `(! actor message)`
- [ ] Actor supervision and linking
- [ ] Mailbox pattern matching for actors

### 4. Error Handling & Supervision
- [ ] Try/catch for async operations
- [ ] Promise rejection and error propagation
- [ ] Supervisor trees for fault tolerance
- [ ] Restart strategies (one-for-one, one-for-all)

### 5. Advanced Concurrency Patterns
- [ ] Futures and promise combinators (all, race, etc.)
- [ ] Async streams and generators
- [ ] Concurrent data structures (async queues, etc.)
- [ ] Timeout operations: `(timeout duration promise)`

### 6. Integration with Effect System
- [ ] Complete Async effect handler implementation
- [ ] Proper effect isolation for concurrent operations
- [ ] Resource management for async contexts
- [ ] Cancellation tokens and cleanup

## Example API Design

```lisp
;; Async function definition
(define-async fetch-data (url)
  (let ((response (await (http-get url))))
    (parse-json response)))

;; Actor definition
(define-actor counter
  (state 0)
  (receive
    ((increment n) (become (+ state n)))
    ((get reply-to) (! reply-to state))
    (_ (error "Unknown message"))))

;; Using actors
(let ((my-counter (spawn-actor counter)))
  (! my-counter (increment 5))
  (let ((reply-ch (chan)))
    (! my-counter (get reply-ch))
    (recv! reply-ch))) ; Returns 5

;; Select on multiple channels
(select
  ((recv! ch1) (fn (val) (print "Got from ch1:" val)))
  ((recv! ch2) (fn (val) (print "Got from ch2:" val)))
  (timeout 1000 (print "Timeout!")))

;; Promise combinators
(await
  (promise-all
    (fetch-data "api/users")
    (fetch-data "api/posts")
    (fetch-data "api/comments")))
```

## Testing Requirements

- Unit tests for each async primitive
- Integration tests for actor communication
- Stress tests for concurrent operations
- Examples demonstrating common patterns
- Performance benchmarks

## Documentation Needs

- Actor model tutorial
- Async/await best practices
- Concurrency patterns cookbook
- Migration guide from current spawn/channel usage

## References

- Erlang/OTP actor model
- Akka actors (Scala)
- Go's goroutines and channels
- Rust's async/await and tokio
- JavaScript promises and async/await

## Priority

High - This is a fundamental feature for building scalable, concurrent applications in FluentAI.

## Labels

- enhancement
- concurrency
- actor-model
- async-await
- breaking-change (potentially)