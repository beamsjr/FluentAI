# Quantifiers in ClaudeLang Contracts

This document explains how to use universal (∀) and existential (∃) quantifiers in contract specifications.

## Overview

Quantifiers allow you to express properties that hold for all or some elements in a collection. They are essential for specifying properties of data structures like arrays, lists, and sets.

## Syntax

### Basic Syntax

```clojure
(forall ((var1 domain1) (var2 domain2) ...) body)
(exists ((var1 domain1) (var2 domain2) ...) body)
```

### Unicode Syntax

```clojure
(∀ ((x Int)) (>= x 0))
(∃ ((x lst)) (= x target))
```

## Domains

Quantified variables must specify their domain:

### Basic Types
- `Int` or `Integer` - Any integer value
- `Bool` or `Boolean` - Boolean values (true/false)

### Ranges
- `(range min max)` - Integer range [min, max]

### List-based Domains
- `(in list)` - Elements of a list
- `(indices list)` - Valid indices of a list (0 to length-1)

## Examples

### 1. All Positive Elements

```clojure
(spec:contract positive-array
  :ensures [(forall ((i (indices arr)))
              (> (nth arr i) 0))]
  :pure true)
```

This ensures every element in the array is positive.

### 2. Sorted Array

```clojure
(spec:contract is-sorted
  :ensures [(forall ((i (indices arr)))
              (implies (< (+ i 1) (length arr))
                       (<= (nth arr i) (nth arr (+ i 1)))))]
  :pure true)
```

This ensures the array is sorted in ascending order.

### 3. Element Exists

```clojure
(spec:contract contains
  :requires [(exists ((x (in lst)))
               (= x target))]
  :ensures [(>= result 0)]
  :pure true)
```

This requires that the target element exists in the list.

### 4. Bounded Values

```clojure
(spec:contract bounded-values
  :requires [(forall ((x (in arr)))
               (and (>= x min-val)
                    (<= x max-val)))]
  :pure true)
```

This requires all array values to be within specified bounds.

### 5. Unique Elements

```clojure
(spec:contract all-unique
  :ensures [(forall ((i (indices lst)) (j (indices lst)))
              (implies (not (= i j))
                       (not (= (nth lst i) (nth lst j)))))]
  :pure true)
```

This ensures all elements in the list are unique.

### 6. Matrix Properties

```clojure
(spec:contract symmetric-matrix
  :requires [(forall ((i (range 0 n)) (j (range 0 n)))
               (= (matrix-ref m i j)
                  (matrix-ref m j i)))]
  :pure true)
```

This requires the matrix to be symmetric.

## Nested Quantifiers

You can nest quantifiers for more complex properties:

```clojure
(forall ((i (indices arr)))
  (exists ((j (indices arr)))
    (and (> j i)
         (= (nth arr j) (* 2 (nth arr i))))))
```

This states that for every element, there exists a later element that is twice its value.

## Implementation Notes

### Z3 Translation

Quantified expressions are translated to Z3's quantifier constructs:
- `forall` → `z3::ast::forall_const`
- `exists` → `z3::ast::exists_const`

### Limitations

1. **Array Theory**: Full array reasoning requires Z3's array theory (planned enhancement)
2. **Triggers**: Automatic trigger generation is not yet implemented
3. **Bounded Quantification**: Unbounded quantifiers may lead to undecidability

### Performance Considerations

- Quantifiers can make verification slower or undecidable
- Use bounded domains when possible
- Consider using `range` instead of unbounded `Int`
- Avoid unnecessary quantifier nesting

## Best Practices

1. **Be Specific with Domains**
   ```clojure
   ; Good - bounded domain
   (forall ((i (range 0 100))) ...)
   
   ; Less ideal - unbounded
   (forall ((i Int)) ...)
   ```

2. **Use Helper Functions**
   ```clojure
   (define (all-positive? lst)
     (forall ((x (in lst))) (> x 0)))
   
   (spec:contract process-positives
     :requires [(all-positive? input)]
     ...)
   ```

3. **Combine with Other Operators**
   ```clojure
   ; Check if sorted and bounded
   (and (forall ((i (indices arr)))
          (implies (< i (- (length arr) 1))
                   (<= (nth arr i) (nth arr (+ i 1)))))
        (forall ((x (in arr)))
          (<= 0 x 100)))
   ```

## Future Enhancements

1. **Pattern-based Triggers**: Automatic trigger generation for better Z3 performance
2. **Array Theory Integration**: Direct support for Z3 array operations
3. **Set Comprehensions**: `{x | P(x)}` syntax
4. **Sequence Operators**: Built-in support for sequence properties
5. **Quantifier Elimination**: Automatic simplification of quantified formulas

## Debugging Quantified Contracts

When verification fails:

1. **Check Domain Bounds**: Ensure domains are correctly specified
2. **Simplify the Body**: Test with simpler quantified expressions
3. **Use Counterexamples**: Z3 may provide witness values
4. **Enable Logging**: Use Z3's verbose mode to see generated formulas

## Related Documentation

- [Contract Semantics](CONTRACT_SEMANTICS.md) - General contract behavior
- [Z3 Converter](Z3_CONVERTER.md) - SMT translation details