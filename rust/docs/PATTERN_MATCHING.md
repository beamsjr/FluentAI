# Pattern Matching in FluentAI

FluentAI provides ML-style pattern matching with exhaustiveness checking, supporting various pattern types for destructuring data.

## Table of Contents
- [Basic Syntax](#basic-syntax)
- [Pattern Types](#pattern-types)
- [List Pattern Matching](#list-pattern-matching)
- [Advanced Features](#advanced-features)
- [Implementation Details](#implementation-details)
- [Examples](#examples)

## Basic Syntax

Pattern matching in FluentAI uses the `match` expression:

```lisp
(match expression
  (pattern1 result1)
  (pattern2 result2)
  ...
  (patternN resultN))
```

## Pattern Types

### 1. Literal Patterns
Match exact values:

```lisp
(match x
  (42 "The answer")
  ("hello" "greeting")
  (#t "true value")
  (#f "false value"))
```

### 2. Variable Patterns
Bind matched values to variables:

```lisp
(match value
  (x x))  ; Binds value to x and returns it
```

### 3. Wildcard Pattern
Match anything without binding:

```lisp
(match value
  (42 "found it")
  (_ "something else"))  ; _ matches anything
```

### 4. Constructor Patterns
Destructure algebraic data types:

```lisp
(data Maybe a
  (Nothing)
  (Just a))

(match maybe-value
  ((Nothing) 0)
  ((Just x) x))
```

## List Pattern Matching

FluentAI provides special support for pattern matching on lists using `cons` and `nil` patterns:

### Cons Pattern
Destructures a non-empty list into head and tail:

```lisp
(match (list 1 2 3)
  ((cons x xs) x)    ; Returns 1 (the head)
  (_ "empty"))

(match (list 1 2 3)
  ((cons x xs) xs)   ; Returns (list 2 3) (the tail)
  (_ "empty"))
```

### Nil Pattern
Matches empty lists:

```lisp
(match (list)
  ((nil) "empty list")
  ((cons x xs) "non-empty list"))
```

### List Processing Examples

```lisp
;; Sum all elements in a list
(define sum
  (lambda (lst)
    (match lst
      ((nil) 0)
      ((cons x xs) (+ x (sum xs))))))

;; Get the length of a list
(define length
  (lambda (lst)
    (match lst
      ((nil) 0)
      ((cons _ xs) (+ 1 (length xs))))))

;; Get the first element (head) of a list
(define head
  (lambda (lst)
    (match lst
      ((nil) (error "empty list"))
      ((cons x _) x))))

;; Get all but the first element (tail)
(define tail
  (lambda (lst)
    (match lst
      ((nil) (error "empty list"))
      ((cons _ xs) xs))))

;; Check if a list contains an element
(define contains?
  (lambda (lst elem)
    (match lst
      ((nil) #f)
      ((cons x xs) 
        (if (= x elem)
            #t
            (contains? xs elem))))))
```

## Advanced Features

### Nested Patterns
Pattern matching can be nested for complex data structures:

```lisp
(match nested-list
  ((cons (cons a b) c) a)  ; Matches [[a, b], c...] and returns a
  (_ "no match"))
```

**Note**: Nested pattern matching is currently not fully implemented and will return `nil` for unbound variables in nested patterns.

### Pattern Guards (Future Feature)
Guards allow additional conditions on patterns:

```lisp
(match x
  ((cons h t) when (> h 0) "positive head")
  ((cons h t) "non-positive head")
  ((nil) "empty"))
```

### As-Patterns (Future Feature)
Bind the entire value while destructuring:

```lisp
(match lst
  ((cons x xs) as whole-list 
    (print "List:" whole-list "Head:" x)))
```

## Implementation Details

### Compilation Strategy

Pattern matching is compiled to efficient bytecode using a decision tree approach:

1. **Pattern Analysis**: Each pattern is analyzed to determine its type and structure
2. **Test Generation**: Appropriate tests are generated for each pattern type:
   - Literal patterns → equality checks
   - Constructor patterns → tag checks for ADTs, special handling for lists
   - Variable patterns → always match (with binding)
   - Wildcard patterns → always match (no binding)
3. **Variable Binding**: Matched values are bound to local variables in the correct scope
4. **Branch Selection**: The first matching pattern's expression is evaluated

### List Pattern Optimization

Lists in FluentAI are represented as `Value::List` rather than tagged ADT constructors, so pattern matching on lists requires special handling:

- `cons` patterns check if the list is non-empty, then extract head and tail
- `nil` patterns check if the list is empty
- The compiler generates specialized bytecode instructions for list operations

### Stack Management

The pattern matching compiler carefully manages the stack to ensure:
- Pattern variables are properly bound in their scope
- The result value is preserved on the stack
- Temporary values are cleaned up after pattern matching

## Examples

### Recursive List Functions

```lisp
;; Map function over a list
(define map
  (lambda (f lst)
    (match lst
      ((nil) (list))
      ((cons x xs) (cons (f x) (map f xs))))))

;; Filter elements that satisfy a predicate
(define filter
  (lambda (pred lst)
    (match lst
      ((nil) (list))
      ((cons x xs)
        (if (pred x)
            (cons x (filter pred xs))
            (filter pred xs))))))

;; Fold/reduce over a list
(define fold
  (lambda (f init lst)
    (match lst
      ((nil) init)
      ((cons x xs) (fold f (f init x) xs)))))
```

### Tree Pattern Matching

```lisp
(data Tree a
  (Leaf a)
  (Node (Tree a) (Tree a)))

(define tree-sum
  (lambda (tree)
    (match tree
      ((Leaf n) n)
      ((Node left right) 
        (+ (tree-sum left) (tree-sum right))))))
```

### Option Type Handling

```lisp
(data Option a
  (None)
  (Some a))

(define option-map
  (lambda (f opt)
    (match opt
      ((None) (None))
      ((Some x) (Some (f x))))))

(define option-or-else
  (lambda (opt default)
    (match opt
      ((None) default)
      ((Some x) x))))
```

## Performance Considerations

- Pattern matching is optimized to minimize unnecessary checks
- The compiler reorders patterns when safe to improve performance
- Tail-recursive pattern matching functions are optimized by the tail call optimizer
- List pattern matching uses efficient head/tail extraction operations

## Future Enhancements

1. **Exhaustiveness Checking**: Verify all cases are covered at compile time
2. **Redundancy Detection**: Warn about unreachable patterns
3. **Pattern Guards**: Add conditional guards to patterns
4. **View Patterns**: Allow patterns that apply functions before matching
5. **Or-Patterns**: Match multiple patterns with the same result
6. **Nested Pattern Optimization**: Full support for arbitrarily nested patterns