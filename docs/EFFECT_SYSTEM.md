# FluentAI Effect System

FluentAI features a comprehensive effect system that makes side effects explicit and manageable. This enables powerful optimizations while maintaining correctness.

## Overview

Effects in FluentAI are first-class values that represent computations with side effects. The effect system:

- Makes all side effects explicit in the type system
- Enables effect handlers for custom effect interpretation
- Allows pure computations to be aggressively optimized
- Provides composable effect handling

## Effect Types

FluentAI defines seven core effect types:

### 1. PURE
No side effects - can be evaluated at any time, memoized, or eliminated if unused.

### 2. IO
Input/output operations:
- `io:print` - Print to stdout
- `io:read-line` - Read line from stdin
- `io:open-file` - Open a file
- `io:close-file` - Close a file
- `io:read-file` - Read file contents
- `io:write-file` - Write to file

### 3. STATE
Mutable state operations:
- `state:get` - Get state value
- `state:set` - Set state value
- `state:update` - Update state with function
- `state:delete` - Remove state value
- `state:begin-transaction` - Start transaction
- `state:commit-transaction` - Commit changes
- `state:rollback-transaction` - Rollback changes

Garbage collection operations:
- `gc:collect` - Force garbage collection
- `gc:stats` - Get GC statistics (returns dict with live_objects, total_allocated, etc.)
- `gc:enable` - Enable automatic garbage collection
- `gc:disable` - Disable automatic garbage collection
- `gc:set-threshold` - Set GC threshold for automatic collection
- `gc:get-threshold` - Get current GC threshold
- `gc:is-enabled?` - Check if GC is enabled

### 4. ERROR
Error handling:
- `error:raise` - Raise an error
- `error:catch` - Install error handler
- `error:get-errors` - Get error stack
- `error:clear-errors` - Clear error stack

### 5. TIME
Time-related operations:
- `time:now` - Current timestamp
- `time:sleep` - Sleep for duration
- `time:elapsed` - Time since start

### 6. RANDOM
Random number generation:
- `random:float` - Random float [0, 1)
- `random:int` - Random integer in range
- `random:choice` - Random choice from list
- `random:shuffle` - Shuffle list

### 7. NETWORK
Network operations:
- `network:fetch` - HTTP request (simplified)

## Using Effects

Effects can be used as regular function calls:

```lisp
; IO effects
(io:print "Hello, World!")

; State effects
(state:set "counter" 0)
(state:update "counter" (lambda (x) (+ x 1)))
(io:print "Counter:" (state:get "counter"))

; Time effects
(let ((start (time:now)))
  (time:sleep 1.0)
  (io:print "Elapsed:" (- (time:now) start)))

; Random effects
(io:print "Dice roll:" (+ 1 (random:int 0 5)))

; Garbage collection
(io:print "GC stats before:" (gc:stats))
(gc:collect)  ; Force collection
(io:print "GC stats after:" (gc:stats))

; Configure GC
(gc:set-threshold 1000)  ; Collect when 1000 objects allocated
(io:print "GC enabled?" (gc:is-enabled?))
```

## Effect Syntax (Legacy)

The `effect` keyword can be used for explicit effect invocation:

```lisp
(effect io:print "Explicit effect")
(effect state:set "x" 42)
```

## Effect Handlers

Effect handlers interpret effects and can modify their behavior:

```lisp
; Install custom error handler
(error:catch "my-error" 
  (lambda (err) 
    (io:print "Handled:" err)
    "default-value"))

; Raise error - will be caught
(error:raise "my-error" "Something went wrong")
```

## Transactions

State effects support transactions:

```lisp
(state:set "balance" 100)

(state:begin-transaction)
(state:update "balance" (lambda (b) (- b 50)))
(io:print "In transaction:" (state:get "balance"))  ; 50

(state:rollback-transaction)
(io:print "After rollback:" (state:get "balance"))  ; 100
```

## Pure vs Effectful

Pure functions can be optimized aggressively:

```lisp
; Pure - can be computed at compile time
(let ((sum (lambda (x y) (+ x y))))
  (sum 10 20))  ; Optimized to 30

; Effectful - must be executed at runtime
(let ((log-sum (lambda (x y)
                  (io:print "Adding" x "and" y)
                  (+ x y))))
  (log-sum 10 20))  ; Cannot be optimized away
```

## Effect Composition

Effects compose naturally:

```lisp
(let ((log-and-inc (lambda (msg)
                     (io:print msg)
                     (state:update "count" (lambda (x) (+ x 1)))
                     (state:get "count"))))
  (log-and-inc "First")   ; Prints "First", returns 1
  (log-and-inc "Second")  ; Prints "Second", returns 2
  (log-and-inc "Third"))  ; Prints "Third", returns 3
```

## Testing with Effects

The effect system supports testing through:

1. **Mock handlers** - Replace real effects with test doubles
2. **Effect logs** - Track which effects were performed
3. **Deterministic randomness** - Seed random effects for reproducibility
4. **Mock time** - Control time progression in tests

## Implementation Details

Effects are implemented through:

1. **Effect Handlers** - Interpret effect operations
2. **Effect Context** - Manages handler stack and effect log
3. **Effect Primitives** - Language primitives for effects
4. **Effect Types** - Track effects in the type system

## Best Practices

1. **Minimize effects** - Keep as much code pure as possible
2. **Isolate effects** - Group effects at the edges of your program
3. **Handle errors** - Install appropriate error handlers
4. **Use transactions** - For complex state updates
5. **Test effects** - Use mock handlers for testing

## Examples

See `examples/advanced_effects.ai` for comprehensive examples of the effect system in action.

## Future Extensions

Planned extensions to the effect system:

1. **Algebraic Effects** - More powerful effect composition
2. **Effect Polymorphism** - Generic effect handling
3. **Effect Inference** - Automatic effect tracking
4. **Custom Effects** - User-defined effect types
5. **Effect Contracts** - Specify allowed effects