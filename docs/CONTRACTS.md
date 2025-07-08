# Contract Specifications in FluentAI

FluentAI supports formal contract specifications that define preconditions, postconditions, and invariants for functions. These contracts serve multiple purposes:

1. **Documentation** - Formally specify function behavior
2. **Verification** - Runtime checking and static proof generation
3. **Optimization** - Enable optimizations based on guarantees
4. **Testing** - Generate property-based tests automatically

## Syntax

```lisp
(spec:contract function-name
  :requires [precondition-1 precondition-2 ...]    ; or :pre
  :ensures [postcondition-1 postcondition-2 ...]   ; or :post
  :invariant [invariant-1 invariant-2 ...]
  :complexity "O(...)"
  :pure true/false)
```

## Contract Elements

### Preconditions (`:requires` or `:pre`)

Conditions that must be true when the function is called:

```lisp
(spec:contract divide
  :requires [(number? x) (number? y) (not= y 0)])
```

### Postconditions (`:ensures` or `:post`)

Conditions that must be true when the function returns:

```lisp
(spec:contract abs
  :ensures [(>= result 0)
            (or (= result x) (= result (- x)))])
```

The special variable `result` refers to the function's return value.

### Invariants (`:invariant`)

Conditions that remain true throughout function execution:

```lisp
(spec:contract binary-search
  :invariant [(>= high low)
              (sorted? array)])
```

### Complexity (`:complexity`)

Big-O complexity specification:

```lisp
(spec:contract sort
  :complexity "O(n log n)")
```

### Purity (`:pure`)

Whether the function has side effects:

```lisp
(spec:contract read-file
  :pure false)  ; Has IO effects
```

## Examples

### Simple Arithmetic Function

```lisp
(spec:contract add
  :requires [(number? x) (number? y)]
  :ensures [(= result (+ x y))]
  :complexity "O(1)")

(define (add x y)
  (+ x y))
```

### List Processing

```lisp
(spec:contract map
  :requires [(function? f) (list? lst)]
  :ensures [(list? result)
            (= (length result) (length lst))
            (forall i in 0..(length lst)
                    (= (nth result i) (f (nth lst i))))]
  :complexity "O(n * f)")
```

### Recursive Function with Measure

```lisp
(spec:contract factorial
  :requires [(int? n) (>= n 0)]
  :ensures [(>= result 1)]
  :invariant [(decreasing n)]  ; Termination proof
  :complexity "O(n)")
```

### Data Structure Invariant

```lisp
(spec:contract balanced-tree?
  :ensures [(implies result
                    (forall node in tree
                            (<= (height-diff node) 1)))])
```

## Special Contract Variables

- `result` - The function's return value (in postconditions)
- `old(expr)` - Value of expression at function entry
- `error` - Exception/error value (if function can fail)

## Contract Predicates

Common predicates used in contracts:

- Type predicates: `int?`, `string?`, `list?`, `function?`
- Comparison: `=`, `<`, `>`, `<=`, `>=`, `not=`
- Logic: `and`, `or`, `not`, `implies`, `iff`
- Quantifiers: `forall`, `exists`
- List operations: `all`, `any`, `sorted?`, `permutation?`
- Math: `even?`, `odd?`, `prime?`, `positive?`

## Advanced Features

### Contract Inheritance

```lisp
(spec:contract collection
  :abstract true
  :ensures [(>= (size result) 0)])

(spec:contract list
  :extends collection
  :ensures [(list? result)])
```

### Conditional Contracts

```lisp
(spec:contract process
  :requires [(or (string? x) (number? x))]
  :ensures [(implies (string? x) (string? result))
            (implies (number? x) (number? result))])
```

### Performance Contracts

```lisp
(spec:contract cache-get
  :complexity "O(1)"
  :amortized "O(1)"
  :worst-case "O(n)")
```

## Contract Verification

### Runtime Checking

In development mode, contracts are checked at runtime:

```lisp
(enable-contract-checking!)

(divide 10 0)  ; ContractViolation: Precondition failed: (not= y 0)
```

### Static Verification

The compiler attempts to prove contracts at compile time:

```lisp
(define (safe-head lst)
  (if (empty? lst)
      nil
      (head lst)))

; Compiler proves: postcondition (or (nil? result) (element? result lst))
```

### Optimization Based on Contracts

Contracts enable optimizations:

```lisp
(spec:contract vector-ref
  :requires [(vector? v) (int? i) (>= i 0) (< i (length v))])

; Compiler can skip bounds checking due to preconditions
```

## Integration with Other Features

### With Pattern Matching

```lisp
(spec:contract match-result
  :ensures [(not= result 'no-match)])

(define (match-result x)
  (match x
    (0 'zero)
    (_ 'other)))  ; Exhaustive, so contract is satisfied
```

### With Effect System

```lisp
(spec:contract pure-function
  :pure true
  :ensures [(no-effects result)])

(spec:contract io-function  
  :pure false
  :effects [IO])
```

### With Type System

```lisp
(spec:contract typed-add
  :requires [(: x Int) (: y Int)]
  :ensures [(: result Int)])
```

## Best Practices

1. **Start Simple** - Basic type checks and non-null guarantees
2. **Be Precise** - Specific contracts enable better optimization
3. **Document Intent** - Contracts serve as executable documentation
4. **Test Contracts** - Use property-based testing with contracts
5. **Incremental Verification** - Add contracts gradually

## Tools and Commands

```lisp
; Enable runtime contract checking
(enable-contract-checking!)

; Disable for performance
(disable-contract-checking!)

; Generate tests from contracts
(generate-tests-from-contracts 'my-function)

; Verify all contracts statically
(verify-all-contracts!)

; Extract documentation from contracts
(generate-docs-from-contracts)
```

## Future Extensions

- **Refinement Types** - Types with predicates
- **Dependent Contracts** - Contracts that depend on runtime values
- **Probabilistic Contracts** - For randomized algorithms
- **Resource Contracts** - Memory and time bounds
- **Relational Contracts** - Relations between multiple functions