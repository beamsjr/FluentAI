# Complex Pattern Matching in FluentAI

FluentAI now supports advanced pattern matching features that make it easier to work with complex data structures and conditions. This guide covers all the new pattern types and how to use them effectively.

## Table of Contents
1. [Or Patterns](#or-patterns)
2. [As Patterns](#as-patterns)
3. [Guard Patterns](#guard-patterns)
4. [Range Patterns](#range-patterns)
5. [View Patterns](#view-patterns)
6. [Combining Patterns](#combining-patterns)
7. [Builder API](#builder-api)

## Or Patterns

Or patterns allow you to match multiple alternatives in a single branch. If any of the patterns match, the branch is taken.

### Syntax
```lisp
(or pattern1 pattern2 ...)
```

### Examples

```lisp
; Match small numbers
(match x
  ((or 1 2 3) "small")
  ((or 4 5 6) "medium")
  (_ "large"))

; Match weekend days
(match day
  ((or "Saturday" "Sunday") "weekend")
  ((or "Monday" "Tuesday" "Wednesday" "Thursday" "Friday") "weekday"))

; Combine with other patterns
(match value
  ((or 0 (nil)) "empty or zero")
  (_ "has value"))
```

### Builder API
```rust
let pattern = Pattern::or(vec![
    Pattern::int(1),
    Pattern::int(2),
    Pattern::int(3),
]);
```

## As Patterns

As patterns (also known as alias patterns) allow you to bind a name to the entire matched value while also matching against a more specific pattern.

### Syntax
```lisp
(as binding pattern)
```

### Examples

```lisp
; Bind the whole list while matching its structure
(match lst
  ((as whole (cons head tail)) 
    (do
      (print "The whole list is: " whole)
      (print "First element: " head)
      (print "Rest: " tail))))

; Simple binding
(match value
  ((as x 42) x)  ; Matches 42 and binds it to x
  (_ 0))

; Bind any value
(match anything
  ((as x _) x))  ; Matches anything and binds it to x
```

### Builder API
```rust
let pattern = Pattern::as_pattern(
    "whole",
    Pattern::cons(Pattern::var("h"), Pattern::var("t"))
);
```

## Guard Patterns

Guard patterns add a boolean condition that must be satisfied for the pattern to match. The condition can reference variables bound by the pattern.

### Syntax
```lisp
(when pattern condition)
```

### Examples

```lisp
; Match positive numbers
(match x
  ((when n (> n 0)) "positive")
  ((when n (< n 0)) "negative")
  (0 "zero"))

; Match even numbers in a range
(match x
  ((when (as n (or 2 4 6 8)) (even? n)) "small even")
  (_ "other"))

; Guard with destructuring
(match point
  ((when (cons x y) (= x y)) "on diagonal")
  ((cons x y) "off diagonal"))
```

### Builder API
```rust
let condition = graph.add_node(Node::Application {
    function: graph.add_node(Node::Variable { name: ">".to_string() })?,
    args: vec![
        graph.add_node(Node::Variable { name: "n".to_string() })?,
        graph.add_node(Node::Literal(Literal::Integer(0)))?,
    ],
})?;

let pattern = Pattern::guard(Pattern::var("n"), condition);
```

## Range Patterns

Range patterns match numeric values within a specified range. FluentAI supports both inclusive and exclusive ranges.

### Syntax
```lisp
(..= start end)  ; Inclusive range (start <= value <= end)
(.. start end)   ; Exclusive range (start <= value < end)
```

### Examples

```lisp
; Grade calculation with inclusive ranges
(match score
  ((..= 90 100) "A")
  ((..= 80 89) "B")
  ((..= 70 79) "C")
  ((..= 60 69) "D")
  (_ "F"))

; Age groups with exclusive ranges
(match age
  ((.. 0 13) "child")
  ((.. 13 20) "teenager")
  ((.. 20 65) "adult")
  (_ "senior"))

; Single digit check
(match n
  ((.. 0 10) "single digit")
  ((.. 10 100) "double digit")
  (_ "three or more digits"))
```

### Builder API
```rust
// Inclusive range
let pattern = Pattern::range_inclusive(
    Literal::Integer(1),
    Literal::Integer(10)
);

// Exclusive range
let pattern = Pattern::range_exclusive(
    Literal::Integer(0),
    Literal::Integer(100)
);

// Convenience method for integer ranges
let pattern = Pattern::int_range(1, 10); // Inclusive by default
```

## View Patterns

View patterns apply a function to the value before matching. This allows you to match against a transformed version of the input.

### Syntax
```lisp
(view function pattern)
```

### Examples

```lisp
; Match absolute value
(letrec ((abs (lambda (x) (if (< x 0) (- 0 x) x))))
  (match n
    ((view abs 5) "absolute value is 5")
    (_ "other")))

; Match string length
(match str
  ((view length 0) "empty string")
  ((view length 1) "single character")
  ((view length (.. 2 10)) "short string")
  (_ "long string"))

; Match after normalization
(letrec ((normalize (lambda (s) (lowercase (trim s)))))
  (match input
    ((view normalize "yes") true)
    ((view normalize "no") false)
    (_ (error "Invalid input"))))
```

### Builder API
```rust
let abs_fn = graph.add_node(Node::Variable { name: "abs".to_string() })?;
let pattern = Pattern::view(abs_fn, Pattern::int(5));
```

## Combining Patterns

The real power of complex patterns comes from combining them. Here are some examples:

```lisp
; Guard with or-pattern
(match x
  ((when (or 2 4 6 8 10) (< x 7)) "small even")
  (_ "other"))

; As-pattern with guard
(match lst
  ((when (as whole (cons h t)) (> (length whole) 3)) 
    "long list")
  ((cons h t) "short list")
  (nil "empty"))

; Nested as-patterns
(match nested
  ((as outer (cons (as inner (cons a b)) rest))
    [outer inner a b rest]))

; Range with guard
(match score
  ((when (..= 90 100) (submitted-on-time?)) "A")
  ((..= 90 100) "A-late")
  (_ "other"))
```

## Builder API

The FluentAI AST provides a fluent builder API for constructing match expressions with complex patterns:

```rust
use fluentai_core::ast::{Graph, Pattern};

let mut graph = Graph::new();

// Create a match expression with the builder
let match_node = graph.build_match()
    .expr(value)
    .branch(
        Pattern::or(vec![
            Pattern::int(1),
            Pattern::int(2),
            Pattern::int(3),
        ]),
        small_result
    )
    .branch(
        Pattern::guard(
            Pattern::int_range(4, 10),
            is_even_condition
        ),
        even_medium_result
    )
    .branch(
        Pattern::as_pattern("x", Pattern::var("_")),
        default_result
    )
    .build()?;
```

### Pattern Builder Methods

```rust
// Basic patterns
Pattern::var("x")              // Variable pattern
Pattern::int(42)               // Integer literal
Pattern::string("hello")       // String literal
Pattern::bool(true)            // Boolean literal
Pattern::nil()                 // Nil pattern
Pattern::wildcard()            // Wildcard pattern (_)

// Complex patterns
Pattern::or(vec![...])         // Or pattern
Pattern::as_pattern("x", ...)  // As pattern
Pattern::guard(pattern, cond)  // Guard pattern
Pattern::int_range(1, 10)      // Range pattern (inclusive)
Pattern::range_inclusive(...)  // Inclusive range
Pattern::range_exclusive(...)  // Exclusive range
Pattern::view(func, pattern)   // View pattern

// List patterns
Pattern::cons(head, tail)      // Cons pattern
Pattern::list(vec![...])       // List pattern
```

## Performance Considerations

1. **Or patterns** are tested left-to-right and short-circuit on the first match
2. **Guard patterns** evaluate the condition only if the pattern matches
3. **Range patterns** compile to efficient comparison instructions
4. **View patterns** call the function once per match attempt
5. **As patterns** have no runtime overhead - they just add a binding

## Best Practices

1. **Order matters**: Place more specific patterns before general ones
2. **Use guards sparingly**: They can make patterns harder to understand
3. **Prefer or-patterns over multiple branches**: More concise and efficient
4. **Use range patterns for numeric conditions**: Clearer than guards
5. **Combine patterns thoughtfully**: Deep nesting can be hard to read

## Migration Guide

If you have existing pattern matching code, here's how to enhance it:

### Before
```lisp
(match x
  (1 "one")
  (2 "two")
  (3 "three")
  (_ "other"))
```

### After
```lisp
(match x
  ((or 1 2 3) "small number")
  (_ "other"))
```

### Before
```lisp
(match lst
  ((cons h t) 
    (let ((whole lst))
      (process whole h t))))
```

### After
```lisp
(match lst
  ((as whole (cons h t))
    (process whole h t)))
```

## Troubleshooting

### Common Issues

1. **Parser errors with complex patterns**: Ensure proper parentheses
2. **Guard conditions not working**: Check that referenced variables are bound
3. **Range patterns not matching**: Verify inclusive vs exclusive semantics
4. **View functions not found**: Ensure functions are in scope

### Debugging Tips

1. Use simple patterns first, then add complexity
2. Test patterns in the REPL
3. Use `(print pattern)` to debug bindings
4. Check compiler output with debug mode enabled

## Future Enhancements

Planned improvements to pattern matching:

1. **Literal patterns**: Direct pattern literals `#[1 2 3]`
2. **Pattern synonyms**: Define reusable patterns
3. **Pattern guards with multiple conditions**: `(when p (and c1 c2))`
4. **Negative patterns**: `(not pattern)`
5. **Pattern macros**: Generate patterns programmatically

## Conclusion

Complex pattern matching in FluentAI provides powerful tools for working with data. By combining different pattern types, you can express sophisticated matching logic concisely and efficiently. The builder API makes it easy to construct patterns programmatically, while the syntax remains readable and intuitive.

For more examples and advanced usage, see the pattern matching tests in `fluentai-vm/tests/pattern_matching_comprehensive_tests.rs`.