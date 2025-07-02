# Async/Await in ClaudeLang

ClaudeLang provides first-class support for asynchronous programming through promises, async functions, and the await operator. These primitives enable concurrent execution while maintaining clean, sequential-looking code.

## Overview

Async programming in ClaudeLang is based on:
- **Promises**: Values that will be available in the future
- **Async functions**: Functions that return promises
- **Await**: Operator to extract values from promises
- **Concurrent operations**: Running multiple async operations in parallel

## Promises

### Creating Promises

Promises are created with the `promise` form, which takes an executor function:

```lisp
(promise (lambda (resolve reject)
  ; Do async work
  (if success
      (resolve result)
      (reject error))))
```

### Promise States

A promise can be in one of three states:
- **Pending**: Initial state, operation in progress
- **Fulfilled**: Operation completed successfully
- **Rejected**: Operation failed with an error

### Basic Promise Example

```lisp
; Create a promise that resolves after a delay
(let ((delayed-value (promise (lambda (resolve reject)
                               (effect time:sleep 0.1)
                               (resolve "Hello after 100ms!")))))
  (print (await delayed-value)))  ; Blocks until resolved
```

## Async Functions

Async functions automatically return promises and can use `await` internally:

```lisp
; Define an async function
(let ((fetch-data (async (url)
                    (let ((response (await (http-get url))))
                      (json-parse (get response "body"))))))
  ; Call returns a promise
  (let ((data (await (fetch-data "https://api.example.com/data"))))
    (print data)))
```

### Async Lambda Syntax

```lisp
(async (param1 param2 ...)
  ; Body can use await
  (await some-promise)
  ; Return value is automatically wrapped in promise
  result)
```

## Await Operator

The `await` operator blocks until a promise resolves and returns its value:

```lisp
; Simple await
(let ((result (await promise)))
  (print result))

; Await with timeout (optional)
(let ((result (await promise 5.0)))  ; 5 second timeout
  (print result))
```

### Error Handling with Await

If a promise is rejected, `await` triggers an error effect:

```lisp
(handler
  ((error (lambda (err)
            (print "Promise rejected:" (get err "message"))
            "default value")))
  (await failing-promise))
```

## Concurrent Operations

### Promise.all - Wait for All

Execute multiple async operations and wait for all to complete:

```lisp
(let ((p1 (async-op-1))
      (p2 (async-op-2))
      (p3 (async-op-3)))
  (let ((results (await (effect async:all [p1 p2 p3]))))
    ; results is [result1, result2, result3]
    (print results)))
```

### Promise.race - First to Complete

Get the result of the first promise to complete:

```lisp
(let ((fast (effect async:delay 50 "fast"))
      (slow (effect async:delay 100 "slow")))
  (print (await (effect async:race [fast slow]))))  ; "fast"
```

### Parallel Map

Map an async function over a collection in parallel:

```lisp
(let ((fetch-user (async (id)
                    (await (http-get (string-concat "/users/" id))))))
  (let ((user-ids [1 2 3 4 5]))
    (let ((promises (map fetch-user user-ids)))
      (await (effect async:all promises)))))
```

## Promise Chaining

Chain promise operations with `then`:

```lisp
(let ((initial (promise (lambda (resolve reject) (resolve 10)))))
  (let ((doubled (effect async:then initial (lambda (x) (* x 2))))
        (squared (effect async:then doubled (lambda (x) (* x x)))))
    (print (await squared))))  ; 400
```

## Advanced Patterns

### Async Reduce

Perform reduction with async operations:

```lisp
(let ((async-sum (lambda (numbers)
                   (reduce (lambda (acc-promise n)
                            (let ((acc (await acc-promise)))
                              (async-add acc n)))
                          (promise (lambda (resolve reject) (resolve 0)))
                          numbers))))
  (await (async-sum [1 2 3 4 5])))
```

### Retry Logic

Implement retry for failing operations:

```lisp
(let ((with-retry (lambda (operation max-attempts)
                   (let ((attempt (lambda (n)
                                   (handler
                                     ((error (lambda (err)
                                              (if (< n max-attempts)
                                                  (do
                                                    (effect time:sleep (* n 0.1))
                                                    (attempt (+ n 1)))
                                                  (error err)))))
                                     (await (operation))))))
                     (attempt 1)))))
  (with-retry flaky-operation 3))
```

### Timeout Wrapper

Add timeout to any async operation:

```lisp
(let ((with-timeout (lambda (operation timeout-ms)
                     (let ((timeout (effect async:delay timeout-ms "timeout")))
                       (await (effect async:race [operation timeout]))))))
  (handler
    ((error (lambda (err) "timed out")))
    (with-timeout slow-operation 1000)))
```

### Async Generator Pattern

Generate values asynchronously:

```lisp
(let ((async-range (async (start end)
                     (let ((generate (lambda (n acc)
                                      (if (> n end)
                                          acc
                                          (do
                                            (effect time:sleep 0.01)
                                            (generate (+ n 1) (cons n acc)))))))
                       (reverse (generate start []))))))
  (await (async-range 1 10)))
```

## Error Handling

### Try-Catch Pattern

```lisp
(let ((try-async (lambda (operation on-error)
                   (handler
                     ((error (lambda (err)
                              (on-error err))))
                     (await (operation))))))
  (try-async risky-operation
            (lambda (err) (print "Failed:" err) nil)))
```

### Promise Error Propagation

Errors propagate through promise chains:

```lisp
(let ((p1 (promise (lambda (resolve reject) (reject "error"))))
      (p2 (effect async:then p1 (lambda (x) (* x 2))))  ; Skipped
      (p3 (effect async:then p2 (lambda (x) (+ x 1))))) ; Skipped
  (handler
    ((error (lambda (err) (print "Caught:" err))))
    (await p3)))
```

## Performance Considerations

1. **Parallelism**: Use `Promise.all` for independent operations
2. **Sequential**: Chain with `await` only when order matters
3. **Batching**: Group related async operations
4. **Cancellation**: Clean up pending operations when needed

## Examples

### Fetching Data with Dependencies

```lisp
(let ((fetch-user-with-posts (async (user-id)
                               (let ((user (await (fetch-user user-id))))
                                 (let ((posts (await (fetch-posts user-id))))
                                   {"user" user "posts" posts})))))
  (await (fetch-user-with-posts 123)))
```

### Concurrent API Calls

```lisp
(let ((fetch-dashboard-data (async ()
                              (let ((user-promise (fetch-current-user))
                                    (stats-promise (fetch-stats))
                                    (notifications-promise (fetch-notifications)))
                                (let ((results (await (effect async:all
                                                            [user-promise
                                                             stats-promise
                                                             notifications-promise]))))
                                  {"user" (nth results 0)
                                   "stats" (nth results 1)
                                   "notifications" (nth results 2)})))))
  (await (fetch-dashboard-data)))
```

### Stream Processing

```lisp
(let ((process-stream (async (items processor)
                       (let ((process-batch (lambda (batch)
                                             (effect async:all
                                                    (map processor batch)))))
                         (let ((batches (chunk items 10)))
                           (flatten (await (effect async:all
                                                  (map process-batch batches)))))))))
  (await (process-stream large-list async-processor)))
```

## Best Practices

1. **Always handle errors**: Use error handlers with async operations
2. **Avoid nested awaits**: Flatten promise chains when possible
3. **Use Promise.all**: For independent concurrent operations
4. **Set timeouts**: Prevent indefinite waiting
5. **Clean up resources**: Ensure promises complete or are cancelled
6. **Type annotations**: Annotate async function return types
7. **Test async code**: Use deterministic time mocking in tests

## Integration with Effects

Async operations integrate seamlessly with other effects:

```lisp
(let ((async-with-logging (async (operation)
                            (do
                              (effect io:print "Starting operation...")
                              (let ((result (await (operation))))
                                (effect io:print "Operation complete!")
                                result)))))
  (await (async-with-logging some-async-task)))
```

## Limitations

- No built-in cancellation tokens (use error effects)
- No async generators (use recursive patterns)
- Limited debugging support for promise chains
- No automatic parallelization of independent awaits