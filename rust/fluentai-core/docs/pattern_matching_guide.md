# Pattern Matching Guide

This guide demonstrates the improved pattern matching APIs in FluentAI that make it much easier to construct pattern matching expressions.

## Overview

Pattern matching in FluentAI supports:
- Literal patterns (integers, strings, booleans, nil)
- Variable binding patterns
- Constructor patterns (including list patterns)
- Wildcard patterns

## Pattern Builder API

The `Pattern` type now includes convenient builder methods:

```rust
// Variable pattern - binds matched value to a name
let x_pattern = Pattern::var("x");

// Literal patterns
let int_pattern = Pattern::int(42);
let string_pattern = Pattern::string("hello");
let bool_pattern = Pattern::bool(true);
let nil_pattern = Pattern::nil_lit();

// List patterns
let empty_list = Pattern::nil();
let cons_pattern = Pattern::cons(Pattern::var("head"), Pattern::var("tail"));

// Custom constructor patterns
let point_pattern = Pattern::constructor("Point", vec![
    Pattern::var("x"),
    Pattern::var("y")
]);

// Wildcard pattern (matches anything)
let wildcard = Pattern::wildcard();
```

## Match Builder API

The `Graph` type now includes a fluent builder for creating match expressions:

### Basic Example

```rust
let mut graph = Graph::new();

// Create values
let value = graph.add_node(Node::Literal(Literal::Integer(42)))?;
let result1 = graph.add_node(Node::Literal(Literal::String("forty-two".to_string())))?;
let result2 = graph.add_node(Node::Literal(Literal::String("not 42".to_string())))?;

// Build the match expression
let match_node = graph.build_match()
    .expr(value)
    .int_case(42, result1)
    .default(result2)
    .build()?;
```

### Multiple Cases

```rust
let match_node = graph.build_match()
    .expr(input)
    .int_case(1, result1)
    .int_case(2, result2)
    .string_case("hello", result3)
    .bool_case(true, result4)
    .default(default_result)
    .build()?;
```

### Pattern Matching with Constructors

```rust
let match_node = graph.build_match()
    .expr(list)
    .branch(Pattern::nil(), empty_result)
    .branch(
        Pattern::cons(Pattern::var("h"), Pattern::var("t")),
        non_empty_result
    )
    .build()?;
```

## Helper Functions

### `match_value` - Simple Literal Matching

```rust
let match_node = graph.match_value(
    input,
    vec![
        (Literal::Integer(1), result1),
        (Literal::Integer(2), result2),
        (Literal::String("hello".to_string()), result3),
    ],
    default_result
)?;
```

### `match_list` - List Pattern Matching

```rust
let match_node = graph.match_list(
    list,
    empty_result,
    |graph, head, tail| {
        // Process head and tail
        // Return the result expression
        Ok(head)
    }
)?;
```

## Comparison: Old vs New API

### Old Way (Manual Construction)

```rust
let match_node = graph.add_node(Node::Match {
    expr: value,
    branches: vec![
        (Pattern::Literal(Literal::Integer(42)), result1),
        (Pattern::Constructor {
            name: "cons".to_string(),
            patterns: vec![
                Pattern::Variable("h".to_string()),
                Pattern::Variable("t".to_string()),
            ],
        }, result2),
        (Pattern::Wildcard, result3),
    ],
})?;
```

### New Way (Builder API)

```rust
let match_node = graph.build_match()
    .expr(value)
    .int_case(42, result1)
    .branch(Pattern::cons(Pattern::var("h"), Pattern::var("t")), result2)
    .default(result3)
    .build()?;
```

## Complete Example: Processing Lists

```rust
use fluentai_core::ast::{Graph, Node, Literal, Pattern};

fn sum_list(graph: &mut Graph, list: NodeId) -> Result<NodeId> {
    // Base case: empty list returns 0
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)))?;
    
    // Recursive case: add head to sum of tail
    graph.match_list(list, zero, |g, head, tail| {
        // Recursively sum the tail
        let tail_sum = sum_list(g, tail)?;
        
        // Create addition node
        let plus = g.add_node(Node::Variable { name: "+".to_string() })?;
        g.add_node(Node::Application {
            function: plus,
            args: vec![head, tail_sum],
        })
    })
}
```

## Parser Support

For testing and simple cases, you can also use the parser which supports pattern matching syntax:

```scheme
(match expr
  (42 "forty-two")
  ("hello" "greeting")
  ((cons h t) (+ h (sum t)))
  (_ "default"))
```

## Future Extensions

The pattern system is designed to be extensible. Future additions may include:

- **Pattern Guards**: `(x when (> x 0))`
- **As-patterns**: `((cons h t) as list)`
- **Or-patterns**: `(1 | 2 | 3)`
- **Range patterns**: `(1..10)`
- **Deep destructuring**: nested pattern matching

## Best Practices

1. **Use specific helpers when possible**: `match_value` and `match_list` are simpler than the full builder
2. **Order matters**: Put more specific patterns before general ones
3. **Always include a default**: Use `.default()` or `Pattern::wildcard()` as the last branch
4. **Use meaningful variable names**: In patterns like `Pattern::var("x")`, choose descriptive names
5. **Leverage the parser for tests**: It's often easier to write pattern matching in the source language for tests

## Error Handling

The builder API provides clear error messages:

```rust
// Error: Match expression required
graph.build_match()
    .int_case(1, result)
    .build()  // Error!

// Error: Match expression must have at least one branch
graph.build_match()
    .expr(value)
    .build()  // Error!
```