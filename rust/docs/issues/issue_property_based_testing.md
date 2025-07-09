# Implement Property-Based Testing Framework

## Overview

Implement automatic test generation using property-based testing techniques, similar to QuickCheck or Hypothesis. This would integrate with the existing contract system to automatically verify properties.

## Motivation

Property-based testing can:
- Find edge cases humans miss
- Verify contracts automatically
- Generate regression tests from failures
- Provide better coverage than example-based tests

## Design

### Core Components

1. **Generators** - Create random test inputs
2. **Properties** - Specifications to verify
3. **Shrinkers** - Minimize failing cases
4. **Runner** - Execute and report results

### Syntax Design

```lisp
;; Define generators
(define-generator int-gen
  (random-int -1000 1000))

(define-generator positive-int
  (random-int 1 1000))

(define-generator list-gen
  (random-list int-gen 0 100))

;; Define properties
(define-property reverse-twice-identity
  :forall [(lst list-gen)]
  :property (equal? lst (reverse (reverse lst))))

(define-property sort-idempotent
  :forall [(lst (list-of number?))]
  :property (equal? (sort lst) (sort (sort lst))))

;; Properties with preconditions
(define-property division-inverse
  :forall [(a positive-int) (b positive-int)]
  :when (not= b 0)
  :property (< (abs (- a (* (/ a b) b))) 0.0001))

;; Run property tests
(check-property reverse-twice-identity :tests 1000)
;; => ✓ Passed 1000 tests

(check-property broken-property :tests 100)
;; => ✗ Failed after 23 tests
;;    Counterexample: (lst = (1 2 1))
;;    Shrunk from: (lst = (5 -3 2 8 1 -9 2 1))
```

### Integration with Contracts

```lisp
;; Automatically generate property tests from contracts
(define-contract safe-div
  :requires [(number? a) (number? b) (not= b 0)]
  :ensures [(number? result)])

(generate-property-tests safe-div)
;; Generates:
;; - Tests for precondition violations
;; - Tests for postcondition satisfaction
;; - Edge case tests (b near 0, overflow values)

;; Contract-based fuzzing
(fuzz-contract factorial
  :strategy coverage-guided
  :time-limit 60)  ; seconds
;; => Found contract violation:
;;    Input: n = -1
;;    Precondition failed: (>= n 0)
```

### Generator Combinators

```lisp
;; Basic generators
(define-generator bool-gen (random-bool))
(define-generator char-gen (random-char))
(define-generator string-gen (random-string 0 50))

;; Combinators
(define-generator maybe-int
  (one-of null-gen int-gen))

(define-generator user-gen
  (record-gen
    :name string-gen
    :age (random-int 0 150)
    :email (regex-gen "[a-z]+@[a-z]+\\.[a-z]+")))

;; Recursive generators
(define-generator tree-gen
  (frequency
    (1 (const 'leaf))
    (3 (tuple-gen 'node int-gen tree-gen tree-gen))))

;; Custom generators
(define-generator sorted-list
  (map-gen sort list-gen))
```

### Shrinking

```lisp
;; Automatic shrinking strategies
(define-shrinker int-shrinker
  (lambda (n)
    (cond
      ((= n 0) '())
      ((> n 0) (list 0 (/ n 2) (- n 1)))
      (else (list 0 (/ n 2) (+ n 1))))))

;; Custom shrinkers
(define-shrinker list-shrinker
  (lambda (lst)
    (append
      ;; Try empty list
      (if (empty? lst) '() '(()))
      ;; Try removing each element
      (map (lambda (i) (remove-at lst i))
           (range 0 (length lst)))
      ;; Try shrinking each element
      (map (lambda (i) 
             (update-at lst i (shrink (nth lst i))))
           (range 0 (length lst))))))
```

## Implementation Tasks

### Phase 1: Core Framework
- [ ] Implement basic generators (int, bool, string)
- [ ] Create property definition syntax
- [ ] Build test runner with configurable iterations
- [ ] Add basic shrinking for primitives
- [ ] Implement result reporting

### Phase 2: Advanced Generators
- [ ] Add generator combinators (map, filter, bind)
- [ ] Implement recursive generators
- [ ] Add weighted/frequency-based generation
- [ ] Support custom generator definitions
- [ ] Add regex-based string generation

### Phase 3: Shrinking System
- [ ] Implement shrinking for all built-in types
- [ ] Add custom shrinker support
- [ ] Optimize shrinking strategies
- [ ] Support shrinking of recursive structures
- [ ] Add shrinking visualization

### Phase 4: Contract Integration
- [ ] Generate properties from contracts
- [ ] Add contract-aware generation
- [ ] Implement coverage-guided fuzzing
- [ ] Support incremental testing
- [ ] Add mutation testing

### Phase 5: Tooling
- [ ] IDE integration for property tests
- [ ] Debugging tools for failures
- [ ] Test case minimization UI
- [ ] Coverage reports
- [ ] Performance profiling

## Testing Strategy

The property-based testing framework itself needs extensive testing:
- Meta-properties (properties about the framework)
- Generator distribution tests
- Shrinker correctness tests
- Performance benchmarks
- Integration tests with contracts

## Priority

**Medium** - Valuable for ensuring correctness but not blocking core functionality

## Labels

- enhancement
- testing
- property-based-testing
- contracts