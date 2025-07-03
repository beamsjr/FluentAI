# ClaudeLang Contract Semantics

This document provides a detailed explanation of how contracts work in ClaudeLang, including when they are evaluated, who is blamed for failures, and how they interact with the rest of the language.

## Table of Contents

1. [Overview](#overview)
2. [Preconditions](#preconditions)
3. [Postconditions](#postconditions)
4. [Invariants](#invariants)
5. [Purity Requirements](#purity-requirements)
6. [Special Contract Functions](#special-contract-functions)
7. [Contract Inheritance](#contract-inheritance)
8. [Performance Considerations](#performance-considerations)

## Overview

ClaudeLang uses design-by-contract to specify and verify the behavior of functions. Contracts serve three purposes:

1. **Documentation**: Contracts document the expected behavior of functions
2. **Static Verification**: Contracts can be verified at compile-time using Z3
3. **Runtime Checking**: Contracts are checked during execution to catch bugs

## Preconditions

### When Evaluated
- Before the function body begins execution
- After arguments are evaluated and bound to parameters
- Before any side effects from the function occur

### Blame Assignment
- **Failure blames**: The caller
- **Rationale**: The caller failed to meet the function's requirements

### Example
```claudelang
(spec:contract withdraw
  :requires [(>= balance amount)   ; Caller must ensure sufficient funds
             (> amount 0)]         ; Amount must be positive
  :ensures [(= balance (- (old balance) amount))])

(define (withdraw amount)
  (set! balance (- balance amount)))
```

## Postconditions

### When Evaluated
- After the function completes normally
- On ALL normal return paths:
  - Explicit return statements
  - Implicit returns (last expression)
  - Early returns from conditionals
  - Pattern match returns

### NOT Evaluated On
- Panic/error paths
- Exceptions propagated with `?` operator
- Process termination

### Special Variables
- `result`: The return value of the function
- `old(expr)`: The value of `expr` before function execution

### Blame Assignment
- **Failure blames**: The function implementation
- **Rationale**: The function failed to deliver its promised behavior

### Example
```claudelang
(spec:contract abs
  :ensures [(>= result 0)                    ; Result is non-negative
            (or (= result x) (= result (- x)))] ; Result is |x|
  :pure true)

(define (abs x)
  (if (< x 0) (- x) x))
```

## Invariants

### Types of Invariants

#### 1. Object/Class Invariants
- **When checked**: After every public method call
- **NOT checked**: During private method execution
- **Purpose**: Maintain consistent object state

```claudelang
(class BankAccount
  :invariant [(>= balance 0)              ; Balance never negative
              (= balance (- deposits withdrawals))])
```

#### 2. Loop Invariants
- **When checked**: 
  - Before loop entry (initialization)
  - After each iteration (maintenance)
  - After loop exit (conclusion)
- **Purpose**: Prove loop correctness

```claudelang
(define (sum-list lst)
  (let loop ([lst lst] [sum 0])
    ; Invariant: sum = sum of processed elements
    :invariant [(= sum (sum-of-processed lst))]
    (if (null? lst)
        sum
        (loop (cdr lst) (+ sum (car lst))))))
```

#### 3. Data Structure Invariants
- **When checked**: After any operation that modifies the structure
- **Purpose**: Ensure structural properties are maintained

```claudelang
(spec:contract binary-search-tree
  :invariant [(bst? tree)     ; Tree satisfies BST property
              (balanced? tree)]) ; Tree is balanced
```

### Blame Assignment
- **Failure blames**: The code that broke the invariant
- **For objects**: The method that corrupted state
- **For loops**: The loop body or initialization

## Purity Requirements

### Contract Expressions Must Be Pure

All expressions within contracts must be pure, meaning they:
- Have no side effects
- Are deterministic
- Don't perform I/O
- Don't modify state
- Only call other pure functions

### Why Purity Matters

1. **Verification Soundness**: Side effects in contracts would make static verification unsound
2. **Performance**: Contracts can be disabled in production without changing behavior
3. **Debugging**: Contract evaluation doesn't interfere with program execution
4. **Optimization**: Pure contracts can be evaluated in any order or cached

### Pure Function Annotations

```claudelang
(spec:contract fibonacci
  :requires [(>= n 0)]
  :ensures [(>= result 0)]
  :pure true)  ; This function is pure

; Contract expressions can safely call pure functions
(spec:contract valid-index
  :requires [(and (>= i 0) (< i (length lst)))]  ; length must be pure
  :pure false) ; This function might have side effects
```

## Special Contract Functions

### `old(expr)`
- Captures the value of `expr` at function entry
- Only available in postconditions
- Must be a pure expression
- Evaluated before any function side effects

```claudelang
(spec:contract push
  :ensures [(= (length stack) (+ 1 (old (length stack))))])
```

### Future Extensions

These special functions are planned but not yet implemented:

#### `forall(var, domain, predicate)`
- Universal quantification over a domain
- Example: `(forall x lst (>= x 0))` - all elements are non-negative

#### `exists(var, domain, predicate)`
- Existential quantification
- Example: `(exists x lst (= x target))` - target is in the list

#### `fresh(ptr)`
- Indicates a pointer/reference is newly allocated
- Used in postconditions for constructors

#### `valid(ptr)`
- Indicates a pointer/reference is valid (not null, not freed)

## Contract Inheritance

When contracts are inherited or refined:

### Liskov Substitution Principle (LSP)
- **Preconditions**: Can only be weakened (accept more inputs)
- **Postconditions**: Can only be strengthened (provide more guarantees)
- **Invariants**: Must be preserved

### Example
```claudelang
(class Shape
  :contract area
    :ensures [(>= result 0)])

(class Circle < Shape
  :contract area
    :ensures [(>= result 0)                      ; Inherited
              (= result (* pi radius radius))])  ; Strengthened
```

## Performance Considerations

### Static Verification
- Happens at compile time
- Uses Z3 SMT solver
- Can be slow for complex contracts
- Results are cached

### Runtime Checking
- Can be disabled in production builds
- Performance mode skips expensive checks
- Invariant checking can be costly
- Consider using `debug_assert!` style contracts

### Best Practices
1. Keep contract expressions simple
2. Avoid expensive computations in contracts
3. Use static verification when possible
4. Profile with contracts enabled and disabled
5. Consider sampling contracts in production

## Configuration

Contracts can be configured via:

```rust
// Enable/disable all runtime checking
runtime_verifier.set_enabled(true);

// Performance mode - skip expensive checks
runtime_verifier.set_performance_mode(true);

// Set verification timeout
static_verifier.set_timeout(30); // seconds
```

## Future Work

1. **Gradual Verification**: Mix static and dynamic checking
2. **Contract Inference**: Automatically infer simple contracts
3. **Refinement Types**: Express contracts in the type system
4. **Proof Obligations**: Generate proof obligations for manual verification
5. **Contract Debugging**: Step through contract evaluation