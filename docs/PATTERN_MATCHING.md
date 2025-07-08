# FluentAI Pattern Matching

FluentAI provides powerful pattern matching capabilities for destructuring and control flow.

## Basic Syntax

Pattern matching uses the `match` expression:

```lisp
(match expression
  (pattern1 result1)
  (pattern2 result2)
  ...
  (patternN resultN))
```

## Pattern Types

### Literal Patterns

Match exact values:

```lisp
(match x
  (0 "zero")
  (1 "one")
  (42 "the answer")
  (_ "something else"))
```

### Variable Patterns

Bind values to variables:

```lisp
(match x
  (n (string-concat "The number is: " (to-string n))))
```

### Wildcard Pattern

The underscore `_` matches anything without binding:

```lisp
(match x
  (0 "zero")
  (_ "not zero"))
```

### List Patterns

Destructure lists:

```lisp
(match lst
  ([] "empty")
  ([x] "single element")
  ([x, y] "two elements")
  ([x, y, ...rest] "at least two elements"))
```

### Constructor Patterns

Match tagged values (tuples with constructor name):

```lisp
; Define option type constructors
(let ((Some (lambda (x) (tuple "Some" x)))
      (None (tuple "None")))
  
  (match option-value
    ((Some v) v)
    ((None) default-value)))
```

## Examples

### Factorial with Pattern Matching

```lisp
(let ((factorial (lambda (n)
                   (match n
                     (0 1)
                     (n (* n (factorial (- n 1))))))))
  (factorial 5))  ; => 120
```

### List Processing

```lisp
(let ((sum-list (lambda (lst)
                  (match lst
                    ([] 0)
                    ([x, ...rest] (+ x (sum-list rest)))))))
  (sum-list [1 2 3 4 5]))  ; => 15
```

### Tree Processing

```lisp
; Binary tree as nested tuples
(let ((Leaf (lambda (x) (tuple "Leaf" x)))
      (Node (lambda (l r) (tuple "Node" l r))))
  
  (let ((sum-tree (lambda (tree)
                    (match tree
                      ((Leaf n) n)
                      ((Node left right) 
                       (+ (sum-tree left) (sum-tree right)))))))
    
    ; Sum a tree
    (sum-tree (Node (Leaf 1) 
                   (Node (Leaf 2) (Leaf 3))))))  ; => 6
```

## Pattern Matching Rules

1. **Patterns are tried in order**: First matching pattern wins
2. **Patterns must be exhaustive**: Cover all possible cases
3. **Variable patterns always match**: Use them as catch-all cases
4. **Nested patterns**: Patterns can be arbitrarily nested

## Integration with Effects

Pattern matching is pure by default but can trigger effects in result expressions:

```lisp
(match command
  ("read" (io:read-file filename))      ; IO effect
  ("write" (io:write-file filename data)) ; IO effect
  ("compute" (pure-computation data))     ; Pure
  (_ (error:raise "Unknown command")))    ; Error effect
```

## Performance Considerations

- Pattern matching is optimized at compile time when possible
- Literal patterns compile to efficient jump tables
- Complex patterns may require runtime checks
- The bytecode compiler generates specialized instructions for common patterns

## Future Enhancements

Planned improvements:
- Guard clauses: `(pattern when condition result)`
- Pattern aliases: `(pattern as name result)`
- View patterns for custom destructuring
- Exhaustiveness checking at compile time