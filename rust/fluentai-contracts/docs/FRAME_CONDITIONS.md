# Frame Conditions in Contract Specifications

This document explains frame conditions - specifications of what a function may modify, enabling modular reasoning about program behavior.

## Overview

Frame conditions answer the question: "What does this function NOT change?"

By explicitly stating what a function may modify, frame conditions:
- Enable local reasoning about function calls
- Support modular verification
- Prevent unintended side effects
- Document modification behavior

## Basic Concepts

### Frame Problem

In formal verification, the "frame problem" asks: when a function executes, what stays the same?

Without frame conditions:
```clojure
(define (increment-x)
  (set! x (+ x 1)))
  
; Questions:
; - Does y change? 
; - Does z change?
; - Do array elements change?
; - Do object fields change?
```

With frame conditions:
```clojure
(spec:contract increment-x
  :modifies [x]
  :ensures [(= x (+ (old x) 1))])
  
; Now we know: ONLY x changes, everything else preserved
```

### Syntax

```clojure
(spec:contract function-name
  :modifies [var1 var2 ...]           ; Variables
  :modifies-fields [(obj.field) ...]   ; Object fields  
  :modifies-indices [(arr[i]) ...]     ; Array elements
  :modifies-regions [region ...]       ; Heap regions
  :may-allocate true/false            ; Can create objects
  :may-deallocate true/false)         ; Can delete objects
```

## Types of Modifications

### 1. Variable Modifications

Direct assignment to variables:

```clojure
(spec:contract swap
  :modifies [x y]
  :ensures [(= x (old y))
            (= y (old x))])

(define (swap)
  (let ([temp x])
    (set! x y)
    (set! y temp)))
```

### 2. Field Modifications

Object field updates:

```clojure
(spec:contract deposit
  :modifies-fields [(account.balance)]
  :ensures [(= account.balance 
               (+ (old account.balance) amount))])

(define (deposit account amount)
  (set-field! account balance 
              (+ (get-field account balance) amount)))
```

### 3. Array/List Modifications

Element updates with index expressions:

```clojure
(spec:contract array-set
  :modifies-indices [(arr[i])]
  :ensures [(= (nth arr i) value)])

(spec:contract clear-range
  :modifies-indices [(arr[0..n])]
  :ensures [(forall ((j (range 0 n)))
              (= (nth arr j) 0))])
```

### 4. Heap Region Modifications

Modifications to memory regions:

```clojure
(spec:contract process-tree
  :modifies-regions [(reachable-from root)]
  :ensures [tree-property])

(spec:contract local-update
  :modifies-regions [(fresh)]  ; Only new allocations
  :may-allocate true)
```

## Special Frame Conditions

### Pure Functions

Functions that modify nothing:

```clojure
(spec:contract pure-max
  :modifies []  ; Empty frame
  :pure true
  :ensures [(or (= result a) (= result b))
            (>= result a)
            (>= result b)])
```

### Conservative Frame

When frame is unspecified, assume anything can change:

```clojure
(spec:contract unknown-function
  ; No :modifies clause means conservative frame
  :ensures [some-property])
  ; Verifier assumes any variable might change
```

## Examples

### Example 1: Stack Operations

```clojure
(spec:contract push
  :modifies-fields [(stack.elements) (stack.top)]
  :may-allocate true
  :ensures [(= stack.top (+ (old stack.top) 1))
            (= (nth stack.elements stack.top) item)])

(spec:contract pop
  :modifies-fields [(stack.top)]
  :requires [(> stack.top 0)]
  :ensures [(= stack.top (- (old stack.top) 1))
            (= result (nth stack.elements (old stack.top)))])
  ; Note: elements array not modified, only top pointer
```

### Example 2: Sorting with Frame

```clojure
(spec:contract sort-in-place
  :modifies-indices [(arr[*])]  ; All indices
  :ensures [(sorted? arr)
            (permutation? arr (old arr))])
  ; Frame ensures no other arrays affected

(spec:contract sort-range
  :modifies-indices [(arr[start..end])]
  :ensures [(sorted-subarray? arr start end)
            (forall ((i (indices arr)))
              (implies (or (< i start) (>= i end))
                       (= (nth arr i) (old (nth arr i)))))])
```

### Example 3: Object Update Pattern

```clojure
(spec:contract update-employee
  :modifies-fields [(emp.salary) (emp.last-modified)]
  :ensures [(= emp.salary new-salary)
            (= emp.last-modified (current-time))
            ; Frame implies these unchanged:
            (= emp.id (old emp.id))
            (= emp.name (old emp.name))
            (= emp.department (old emp.department))])
```

### Example 4: Complex Data Structure

```clojure
(spec:contract rebalance-tree
  :modifies-regions [(reachable-from root)]
  :may-allocate true
  :may-deallocate true
  :ensures [(balanced? root)
            (same-elements? root (old root))])

(spec:contract add-to-cache
  :modifies-fields [(cache.entries) (cache.size)]
  :modifies-regions [(fresh)]  ; Only new allocations
  :may-allocate true
  :ensures [(<= cache.size cache.max-size)])
```

## Frame Inference

When frame conditions are not explicit, they can be inferred:

```clojure
(define (compute x y)
  (+ x y))  ; No assignments → modifies []

(define (update-counter)
  (set! counter (+ counter 1)))  ; Assignment → modifies [counter]

(define (process obj)
  (set-field! obj status 'done))  ; Field update → modifies-fields [(obj.status)]
```

## Verification with Frames

### Modularity

Frame conditions enable modular verification:

```clojure
(spec:contract caller
  :ensures [(= important-var 42)])

(define (caller)
  (set! important-var 42)
  (helper)  ; What does helper modify?
  ; With frame: if helper doesn't modify important-var, we're done
  ; Without frame: must analyze helper's implementation
  )
```

### Frame-based Optimization

Compilers can optimize based on frames:

```clojure
(let ([x (expensive-computation)])
  (pure-function y)  ; Frame: modifies [] 
  (use x))  ; Compiler knows x unchanged, no need to recompute
```

## Advanced Features

### Conditional Frames

Different modifications based on conditions:

```clojure
(spec:contract conditional-update
  :modifies (if use-cache
                [(cache)]
                [(database)])
  :ensures [result-correct])
```

### Parameterized Frames

Frame depends on parameters:

```clojure
(spec:contract update-selected
  :modifies-indices [(arr[i]) | i ∈ selected-indices]
  :ensures [(forall ((i selected-indices))
              (= (nth arr i) new-value))])
```

### Two-State Frames

Relating pre- and post-state modifications:

```clojure
(spec:contract move-data
  :modifies [source target]
  :ensures [(empty? source)
            (= target (old source))])
```

## Best Practices

### 1. Be Precise

```clojure
; Good - precise frame
:modifies-fields [(account.balance)]

; Bad - overly conservative
:modifies-fields [(account.*)]  ; All fields
```

### 2. Document Allocations

```clojure
; Clear about memory effects
:may-allocate true
:modifies-regions [(fresh)]
```

### 3. Use Helper Functions

```clojure
; Define common frame patterns
(define-frame-alias cache-frame
  :modifies-fields [(cache.entries) (cache.size) (cache.lru)])

(spec:contract get-cached
  :frame cache-frame
  ...)
```

### 4. Verify Frame Minimality

Ensure frame is not larger than necessary:

```clojure
; Tool can check: does function actually modify everything in frame?
(spec:contract possibly-over-framed
  :modifies [x y z]  ; But maybe only modifies x
  ...)
```

## Common Patterns

### Read-Only Functions

```clojure
(spec:contract query
  :modifies []
  :ensures [(valid-result? result)])
```

### Single-Variable Update

```clojure
(spec:contract increment
  :modifies [counter]
  :ensures [(= counter (+ (old counter) 1))])
```

### Swap Pattern

```clojure
(spec:contract swap-fields
  :modifies-fields [(obj1.field) (obj2.field)]
  :ensures [(= obj1.field (old obj2.field))
            (= obj2.field (old obj1.field))])
```

### Builder Pattern

```clojure
(spec:contract build-step
  :modifies-fields [(builder.*)]
  :modifies-regions [(reachable-from builder)]
  :may-allocate true)
```

## Tool Support

### Static Checking

Verify code respects frame:
```clojure
(spec:contract limited-update
  :modifies [x]
  :body (begin
          (set! x 5)    ; OK
          (set! y 10))) ; ERROR: y not in frame
```

### Dynamic Checking

Runtime monitors for frame violations:
```clojure
; Runtime system tracks modifications
; Throws error if non-frame variable changed
```

### IDE Integration

- Highlight variables that may change
- Show frame on hover
- Warn about potential frame violations

## Related Concepts

### Separation Logic

Frame conditions relate to separation logic's frame rule:
```
{P} C {Q}
-------------------
{P * R} C {Q * R}
```

### Effect Systems

Frame conditions are a form of effect specification:
- Read effects: what the function accesses
- Write effects: what the function modifies (frame)
- Allocation effects: memory allocation behavior

### Rely-Guarantee Reasoning

In concurrent settings:
- Rely: what other threads may modify
- Guarantee: what this thread modifies (frame)

## Future Extensions

1. **Ownership Types**: Integrate with ownership for better frames
2. **Effect Polymorphism**: Parameterize functions by their frames
3. **Incremental Frames**: Specify relative modifications
4. **Probabilistic Frames**: For randomized algorithms
5. **Temporal Frames**: Changes over time sequences