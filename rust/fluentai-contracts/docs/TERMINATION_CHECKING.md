# Termination Checking for Recursive Contracts

This document explains how the termination checker proves that recursive functions terminate, ensuring sound contract verification.

## Overview

Termination checking is crucial for contract verification because:
- Non-terminating functions can trivially satisfy any postcondition
- Verification of recursive contracts requires induction, which needs well-founded ordering
- Static verification with SMT solvers assumes termination

## Termination Proof Strategies

The termination checker employs multiple strategies:

### 1. Structural Recursion Detection

Automatically detects common patterns:

```clojure
;; List recursion
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))  ; Recursive on cdr - smaller structure
```

```clojure
;; Natural number recursion
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))  ; Recursive on n-1 - smaller number
```

### 2. Explicit Termination Measures

Users can provide ranking functions:

```clojure
(spec:contract ackermann
  :requires [(>= m 0) (>= n 0)]
  :ensures [(>= result 0)]
  :terminates-by (lexicographic m n))  ; Lexicographic ordering

(define (ackermann m n)
  (cond
    [(= m 0) (+ n 1)]
    [(= n 0) (ackermann (- m 1) 1)]
    [else (ackermann (- m 1) (ackermann m (- n 1)))]))
```

### 3. Parameter Analysis

Automatically finds decreasing parameters:

```clojure
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))  ; b decreases (when b > 0)
```

## Termination Measures

### Simple Measures

For single-parameter recursion:

```rust
// Numeric measure
let measure = measure_builder.numeric_measure("n");

// List length measure
let measure = measure_builder.list_length_measure("lst");

// Tree height measure (future)
let measure = measure_builder.tree_height_measure("tree");
```

### Lexicographic Measures

For multiple parameters:

```rust
// (m, n) lexicographic ordering
let measure = measure_builder.lexicographic_measure(vec![
    ("m", m_expr),
    ("n", n_expr),
]);
```

### Custom Measures

Any well-founded expression:

```rust
// Custom measure: 2^m + n
let measure = TerminationMeasure {
    measure_expr: custom_expr,
    depends_on: vec!["m".to_string(), "n".to_string()],
    is_lexicographic: false,
};
```

## Usage

### Basic Termination Check

```rust
use fluentai_contracts::TerminationChecker;

let mut checker = TerminationChecker::new(&graph);
let result = checker.analyze_contract(&contract)?;

match result {
    TerminationResult::Terminates { reason } => {
        println!("Function terminates: {}", reason);
    }
    TerminationResult::MayNotTerminate { reason } => {
        println!("Warning: {}", reason);
    }
    TerminationResult::Unknown { reason } => {
        println!("Cannot determine: {}", reason);
    }
}
```

### Adding Termination Measures

```rust
// Create measure builder
let mut measure_builder = TerminationMeasureBuilder::new(&mut graph);

// Build appropriate measure
let measure = measure_builder.numeric_measure("n");

// Add to checker
checker.add_termination_measure("factorial".to_string(), measure);
```

## Common Patterns

### 1. List Processing

```clojure
(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) 
            (map f (cdr lst)))))  ; Terminates: structural recursion on lst
```

### 2. Tree Traversal

```clojure
(define (tree-sum tree)
  (if (leaf? tree)
      (leaf-value tree)
      (+ (tree-sum (left tree))    ; Terminates: recursion on subtrees
         (tree-sum (right tree)))))
```

### 3. Numeric Recursion

```clojure
(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))        ; Terminates: n decreases
         (fibonacci (- n 2)))))
```

### 4. Mutual Recursion

```clojure
(define (even? n)
  (if (= n 0)
      #t
      (odd? (- n 1))))             ; Terminates: n decreases

(define (odd? n)
  (if (= n 0)
      #f
      (even? (- n 1))))            ; Terminates: n decreases
```

## Advanced Features

### Well-Founded Relations

The system supports various well-founded orderings:
- Natural numbers with <
- Lists with proper sublist relation
- Trees with subtree relation
- Lexicographic combinations
- Custom user-defined orderings

### Termination Contracts

Future syntax for termination specifications:

```clojure
(spec:contract quicksort
  :requires [(list? lst)]
  :ensures [(sorted? result)]
  :terminates-by (length lst)
  :decreases-on recursive-calls)
```

### Integration with Verification

Termination proofs enable:
1. **Induction**: Sound inductive proofs for recursive functions
2. **Total Correctness**: Guarantees function produces result
3. **Complexity Analysis**: Bounds on execution time

## Limitations

Current limitations:
- No support for higher-order recursion
- Limited analysis of mutual recursion
- Cannot handle general recursion schemes
- No integration with external termination provers

## Best Practices

1. **Write Structurally Recursive Functions**
   - Use pattern matching
   - Recurse on strict substructures
   - Make base cases explicit

2. **Provide Measures for Complex Recursion**
   - Use lexicographic ordering for nested loops
   - Make ranking functions explicit
   - Document why function terminates

3. **Avoid Complex Control Flow**
   - Minimize mutual recursion
   - Avoid recursion through data structures
   - Keep recursive structure simple

## Future Enhancements

1. **Size-Change Termination**: Automatic analysis of parameter size changes
2. **Dependency Pairs**: Advanced technique for mutual recursion
3. **Integration with External Tools**: Use specialized termination provers
4. **Termination Inference**: Automatically synthesize ranking functions
5. **Partial Termination**: Reason about functions that terminate on some inputs

## Examples

### Example 1: Binary Search

```clojure
(spec:contract binary-search
  :requires [(sorted? arr) (>= low 0) (<= high (length arr))]
  :ensures [(or (= result -1) 
                (and (>= result low) 
                     (< result high) 
                     (= (nth arr result) target)))]
  :terminates-by (- high low))  ; Search space decreases

(define (binary-search arr target low high)
  (if (>= low high)
      -1
      (let ([mid (quotient (+ low high) 2)])
        (cond
          [(= (nth arr mid) target) mid]
          [(< (nth arr mid) target) 
           (binary-search arr target (+ mid 1) high)]
          [else 
           (binary-search arr target low mid)]))))
```

### Example 2: Merge Sort

```clojure
(spec:contract merge-sort
  :requires [(list? lst)]
  :ensures [(sorted? result) (permutation? result lst)]
  :terminates-by (length lst))

(define (merge-sort lst)
  (if (<= (length lst) 1)
      lst
      (let ([mid (quotient (length lst) 2)])
        (merge (merge-sort (take mid lst))      ; Smaller lists
               (merge-sort (drop mid lst))))))   ; Smaller lists
```

## Related Documentation

- [Contract Semantics](CONTRACT_SEMANTICS.md) - How contracts work
- [Static Verification](../README.md#static-verification) - Verification process
- [Incremental Verification](INCREMENTAL_VERIFICATION.md) - Efficient re-verification