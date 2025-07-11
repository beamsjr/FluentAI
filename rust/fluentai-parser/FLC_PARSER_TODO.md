# FLC Parser Implementation Status

This document tracks which FLC language features are specified but not yet implemented in the parser.

## ✅ Implemented Features

- Basic literals (int, float, string, bool, nil)
- Arithmetic and comparison operators
- Function definitions (including async)
- Lambda expressions
- Method chaining
- Let bindings (without destructuring)
- If/else expressions
- Pattern matching (basic)
- Module imports (basic)
- Module definitions
- Struct definitions
- Enum definitions
- Trait definitions
- Actor definitions with handlers
- Try/catch/finally
- String interpolation (f-strings)
- Printable construct `$(...)`
- Pipe operator `|>`
- List literals `[...]`
- Assignment statements

## ❌ Not Yet Implemented

### Collections
- [x] Map literals: `{"key": "value"}` (converted to make_map calls)
- [x] Set literals: `#{1, 2, 3}` (converted to make_set calls)

### Pattern Matching
- [x] Destructuring in let bindings: `let {x, y} = point`
- [x] Or patterns: `case(1 | 2 | 3, "low")`
- [x] Guard patterns: `case(n when n > 0, "positive")`
- [x] Range patterns: `case(1..10, "single digit")`
- [x] As patterns: `case(Some(x) as whole, process(whole))`

### Module System
- [x] Import aliasing: `use http::client as http_client`
- [ ] Export statements
- [x] Import all: `use module::*`
- [ ] Relative imports

### Effects
- [x] Effect definitions
- [x] Effect usage with `.with()`: `function_name().with(EffectName)`
- [ ] Effect handlers (advanced feature)

### Concurrency
- [x] Channel operations:
  - [x] `channel()` and `Channel.new()` for channel creation
  - [x] `.send(value)` for sending values
  - [x] `.receive()` for receiving values
- [x] Spawn expressions:
  - [x] `spawn { expr }` for spawning async tasks
  - [x] `spawn(expr)` alternative syntax
  - [x] Integration with await: `spawn { expr }.await()`
- [ ] Parallel construct
- [ ] Select expressions for channel operations

### Advanced Features
- [x] Contracts (@requires/@ensures/@invariant/@complexity/@pure annotations)
- [ ] Macros
- [ ] Unsafe blocks
- [ ] Dynamic trait objects: `dyn<Trait>`
- [ ] Type aliases beyond basic support

### Expressions
- [x] Object/record literals: `StructName { field: value }` (for creating struct instances inline)
- [x] Spread operator in collections: `[1, ...other, 3]`, `#{...items}`
- [x] Optional chaining `.?`

## 🔍 Parser Limitations

1. **Keywords as methods**: Currently only `match`, `await`, and `case` can be used as method names. Other keywords require special handling.

2. **Error messages**: Basic error reporting exists but could be more descriptive.

3. **Operator precedence**: May need refinement for complex expressions.

4. **Type annotations**: Parser accepts type syntax but doesn't fully validate or use it.

## 📝 Notes

The s-expression parser tests covered many advanced features that haven't been ported to FLC tests yet. Before removing those tests entirely, we should either:

1. Implement the missing features in the FLC parser
2. Create FLC test stubs marked with `#[ignore]` to track what needs implementation
3. Decide which features are not needed for the initial FLC implementation

Features like channels, contracts, and effect handlers were extensively tested in s-expressions but may need design decisions for their FLC syntax.