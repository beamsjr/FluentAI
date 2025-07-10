  1. Always run the full workspace test suite after making any changes, no matter how isolated they seem
  2. Before declaring any fix complete, verify that all tests across the entire codebase still pass
  3. If a change breaks tests in other modules, investigate why and either:
    - Fix the broken tests if they were relying on incorrect behavior
    - Reconsider the change if it breaks legitimate functionality
    - Find a solution that satisfies all parts of the system

 This is especially important in a complex system like FluentAI where modules are interconnected in subtle ways.
 The LoadGlobal change was a perfect example - what seemed like a simple fix in the VM module ended up  breaking functionality in the effects module.

  Please make sure to follow this workflow for all future changes:
  1. Make change
  2. Run local module tests
  3. Run full workspace tests
  4. Only proceed if ALL tests pass
  5. If any test fails, investigate and resolve before moving on

Also very important for development, if you change something in critical parts of the system that heavely depends on order, please leave a comment on what test you changed somthing for, this way If we find later that this change broke something else we know why we made the change so we can make sure to get both issues resolved. 

## Common Pitfalls to Watch For

### Catch-all Pattern in Match Statements
When adding new cases to match statements (especially in optimizers or analyzers), be aware of catch-all patterns like `_ => {}`. These can silently ignore new node types if they're placed before your new case. For example:

```rust
match node {
    Node::Application { .. } => { /* handle */ }
    Node::Lambda { .. } => { /* handle */ }
    _ => {} // This catches everything else!
    Node::Channel { .. } => { /* This will NEVER be reached! */ }
}
```

Always check if there's a catch-all pattern and ensure your new cases are added BEFORE it. This is particularly important in:
- Dead code elimination (mark_reachable methods)
- Effect analysis 
- Node copying/transformation functions
- Any visitor pattern implementation
 
