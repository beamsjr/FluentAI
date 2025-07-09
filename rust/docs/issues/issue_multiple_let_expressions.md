# Support Multiple Expressions in Let Body

## Problem

Currently, `let` bindings only support a single expression in the body. Attempting to use multiple expressions causes parse errors, limiting the usefulness of let bindings.

## Current Behavior

```lisp
;; This works
(let ((x 5)) 
  (+ x 1))

;; This fails with parse error
(let ((x 5))
  (print x)
  (+ x 1))
```

## Expected Behavior

```lisp
(let ((x 5))
  (print x)      ; Side effect
  (+ x 1))       ; Return value
;; Should print 5 and return 6
```

## Implementation

The parser should wrap multiple expressions in a `Begin` node, similar to how it handles multiple top-level expressions or multiple expressions in function bodies.

## Tasks

- [ ] Update let body parsing to accept multiple expressions
- [ ] Wrap multiple expressions in implicit Begin node
- [ ] Ensure proper sequence evaluation (left to right)
- [ ] Preserve return value (last expression)
- [ ] Update error messages for clarity

## Parser Changes

```rust
// In parse_let_expression
let body = if body_exprs.len() == 1 {
    body_exprs[0]
} else {
    // Multiple expressions, wrap in Begin
    let node = Node::Begin { exprs: body_exprs };
    self.graph.add_node(node)?
};
```

## Test Cases

```lisp
;; Multiple expressions with side effects
(let ((x 10))
  (print "x is: ")
  (print x)
  (print-line "")
  (* x 2))  ; => 20, with output "x is: 10\n"

;; Nested let with multiple expressions
(let ((x 5))
  (let ((y 10))
    (print "calculating...")
    (+ x y)))  ; => 15

;; With effects
(let ((counter 0))
  (effect state:set "count" counter)
  (print "Counter initialized")
  counter)  ; => 0

;; Empty body should be error
(let ((x 5)))  ; => Error: let body cannot be empty
```

## Compatibility

This change is backward compatible - existing single-expression let bodies will continue to work exactly as before.

## Priority

**Medium** - Common pattern that improves code organization

## Labels

- enhancement
- parser
- backward-compatible