# Ghost State in Contract Specifications

This document explains ghost state - specification-only variables and expressions that don't affect runtime behavior but enable more expressive contracts.

## Overview

Ghost state includes:
- **Ghost Variables**: Specification-only variables for tracking abstract properties
- **Old Expressions**: References to pre-state values using `old(expr)`
- **History Variables**: Track sequences of values over time
- **Model Fields**: Abstract fields representing conceptual properties

## Ghost State Types

### 1. Ghost Variables

Variables that exist only for verification:

```clojure
(spec:contract transfer
  :ghost [(total-transferred 0)]  ; Ghost variable initialized to 0
  :ensures [(= total-transferred (+ (old total-transferred) amount))])
```

Use cases:
- Track cumulative values
- Count operations
- Maintain abstract state

### 2. Old Expressions

Reference pre-state values in postconditions:

```clojure
(spec:contract withdraw
  :requires [(>= balance amount)]
  :ensures [(= balance (- (old balance) amount))]  ; old(balance)
  :ensures [(= (old balance) (+ balance amount))])  ; Equivalent
```

Common patterns:
- `old(x)` - Value of x before function execution
- `old(f(x))` - Result of f(x) in pre-state
- `old(obj.field)` - Field value in pre-state

### 3. History Variables

Track value sequences:

```clojure
(spec:contract update-sensor
  :history [(readings temperature)]  ; Track temperature history
  :ensures [(forall ((i (indices readings)))
              (implies (> i 0)
                       (< (abs (- (nth readings i) 
                                 (nth readings (- i 1)))) 
                          10)))]  ; No sudden jumps
```

Features:
- Bounded or unbounded history
- Access via indexing
- Useful for temporal properties

### 4. Model Fields

Abstract representation of data structures:

```clojure
(spec:contract push
  :model [(stack.size Int)]  ; Model field
  :ensures [(= stack.size (+ (old stack.size) 1))]
  :invariant [(>= stack.size 0)])
```

Benefits:
- Separate specification from implementation
- Express properties without exposing internals
- Support data abstraction

## Usage in Contracts

### Basic Syntax

```clojure
(spec:contract function-name
  :ghost [(var-name init-value) ...]      ; Ghost variables
  :history [(var-name tracked-expr) ...]   ; History variables
  :model [(object.field type) ...]         ; Model fields
  :requires [preconditions...]
  :ensures [postconditions...]
  :invariant [invariants...])
```

### Integration with FluentAI

```rust
use fluentai_contracts::{GhostStateBuilder, GhostStateManager};

// Build ghost state expressions
let mut builder = GhostStateBuilder::new(&mut graph);

// Create old(balance)
let old_balance = builder.old(balance);

// Create ghost variable
let ghost_counter = builder.ghost_var("counter", Some(zero));

// Create history tracking
let temp_history = builder.history(temperature, "temp_history");

// Create model field access
let size_field = builder.model_field(list, "size");
```

## Examples

### Example 1: Counter with Ghost Variable

```clojure
(define counter 0)

(spec:contract increment
  :ghost [(total-increments 0)]
  :ensures [(= counter (+ (old counter) 1))]
  :ensures [(= total-increments (+ (old total-increments) 1))])

(define (increment)
  (set! counter (+ counter 1)))
```

### Example 2: Bank Account with Complete Ghost State

```clojure
(spec:contract withdraw
  :ghost [(total-withdrawn 0)
          (transaction-count 0)]
  :history [(balance-history balance)]
  :model [(account.min-balance 0)]
  :requires [(>= balance amount)
             (>= (- balance amount) account.min-balance)]
  :ensures [(= balance (- (old balance) amount))
            (= total-withdrawn (+ (old total-withdrawn) amount))
            (= transaction-count (+ (old transaction-count) 1))]
  :invariant [(>= balance account.min-balance)])
```

### Example 3: Queue with Model Fields

```clojure
(spec:contract enqueue
  :model [(queue.size Int)
          (queue.capacity Int)]
  :requires [(< queue.size queue.capacity)]
  :ensures [(= queue.size (+ (old queue.size) 1))]
  :invariant [(and (>= queue.size 0)
                   (<= queue.size queue.capacity))])
```

### Example 4: Temperature Monitor with History

```clojure
(spec:contract update-temperature
  :history [(temp-readings temperature 100)]  ; Keep last 100
  :ensures [(implies (> (length temp-readings) 1)
                     (< (abs (- temperature 
                               (nth temp-readings 
                                    (- (length temp-readings) 2))))
                        5.0))]  ; Max 5 degree change
```

## Advanced Features

### Nested Old Expressions

```clojure
; Valid in postconditions
(= x (old (old x)))  ; Error - can't nest old
(= x (old (+ x y)))  ; OK - old of expression
```

### Ghost Functions

Pure functions for specifications:

```clojure
(ghost-function sum-list (lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

(spec:contract process-list
  :ensures [(= result (sum-list input))])
```

### Temporal Properties

Using history for temporal logic:

```clojure
; "Eventually always positive"
(exists ((i (indices history)))
  (forall ((j (range i (length history))))
    (> (nth history j) 0)))

; "Always eventually zero"
(forall ((i (indices history)))
  (exists ((j (range i (length history))))
    (= (nth history j) 0)))
```

## Implementation Details

### Memory Management

Ghost state is:
- Not allocated at runtime
- Only exists during verification
- Optimized away in production builds

### Verification Process

1. **Pre-state Capture**: Save values referenced by `old()`
2. **Ghost Updates**: Track ghost variable changes
3. **History Maintenance**: Append to history variables
4. **Post-state Check**: Verify all ghost state assertions

### Integration with SMT Solvers

Ghost state translates to:
- Additional SMT variables
- Equality constraints for old values
- Array theory for histories
- Uninterpreted functions for model fields

## Best Practices

### 1. Use Ghost Variables for Abstract Properties

```clojure
; Good: Track abstract property
:ghost [(items-processed 0)]

; Bad: Duplicate concrete state
:ghost [(ghost-balance balance)]  ; Redundant
```

### 2. Minimize History Size

```clojure
; Good: Bounded history
:history [(recent-values value 10)]

; Risky: Unbounded history
:history [(all-values value)]  ; May cause performance issues
```

### 3. Model Fields for Abstraction

```clojure
; Good: Abstract size property
:model [(collection.size Int)]

; Bad: Expose implementation
:model [(collection.internal-array Array)]
```

### 4. Clear Naming Conventions

- Ghost variables: `ghost_` prefix or `_total` suffix
- History variables: `_history` suffix
- Model fields: dot notation `object.property`

## Common Patterns

### Monotonic Ghost Variables

```clojure
:ghost [(total-ops 0)]
:ensures [(>= total-ops (old total-ops))]  ; Never decreases
```

### State Machine Tracking

```clojure
:ghost [(state 'initial)]
:ensures [(implies (= (old state) 'initial)
                   (or (= state 'initial)
                       (= state 'running)))]
```

### Purity via Ghost State

```clojure
:ghost [(call-count 0)]
:ensures [(= call-count (old call-count))]  ; Pure function
```

## Limitations

1. **No Runtime Effect**: Ghost state cannot influence program execution
2. **Verification Only**: Not available in production code
3. **Performance**: Large histories may slow verification
4. **Scope**: Ghost variables scope limited to contract

## Future Enhancements

1. **Two-State Invariants**: Relate consecutive states
2. **Prophecy Variables**: Depend on future states
3. **Auxiliary Code**: Ghost code blocks for complex specifications
4. **Refinement**: Relate concrete and abstract state

## Troubleshooting

### Common Errors

1. **"Unknown variable in old()"**
   - Ensure variable exists in pre-state
   - Check variable scope

2. **"Ghost variable used in code"**
   - Ghost variables are specification-only
   - Cannot be referenced in implementation

3. **"History overflow"**
   - Set reasonable history bounds
   - Use sliding windows for long executions

### Debugging Tips

- Print ghost state in debug mode
- Verify old() captures correct values
- Check history variable updates
- Validate model field consistency