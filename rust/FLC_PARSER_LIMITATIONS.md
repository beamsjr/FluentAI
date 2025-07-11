# FLC Parser Limitations

This document lists limitations discovered in the FLC parser during the VM test migration process.

## Missing Core Features

### 1. Variable Mutation ✅ IMPLEMENTED
- Variable mutation is now supported via the `:=` operator
- Syntax: `variable := new_value`
- Example: `x := x + 1`
- The mutation operator returns nil

### 2. Module System ✅ PARTIALLY IMPLEMENTED
- Export statements are now supported: `export { item1, item2 };`
- Module syntax supports exports within module blocks
- `use` statements still generate different AST nodes than expected by compiler
- Module system generates different opcodes than s-expression version

### 3. Effect System ✅ IMPLEMENTED
- Effects are now supported via the `perform` keyword
- Syntax: `perform EffectType.method(args)`
- Example: `perform IO.print("Hello")`
- Can be used as both expressions and statements

### 4. Symbol/Quote Support  
- No `quote` function or syntax for creating symbols
- `:symbol` syntax parses but may not create expected Symbol values

### 5. Standard Library Functions
- `print` function is not available
- String concatenation with `+` operator causes type errors
- Missing other common functions expected by tests

### 6. Undefined Variable References
- Cannot reference undefined variables even for assignment
- Makes it impossible to test error cases

### 7. Handler Expressions ✅ IMPLEMENTED
- Handler expressions are now supported
- Syntax: `handle { body } with { pattern => result }`
- Example: `handle { perform Error.raise(42) } with { Error.raise(e) => e + 1 }`
- Can be used in let bindings and as expressions

## Workarounds

### For Variable Mutation
- Use recursive functions with new bindings instead of mutation
- Redesign tests to avoid mutable state

### For Module System
- Keep module-related tests using s-expressions (blocked)
- Or rewrite tests to not depend on specific opcodes

### For Effects
- Need to determine correct effect syntax from parser implementation
- May need to use different effect invocation pattern

### For Missing Functions
- Define required functions at test level
- Or modify tests to use available functions

## Impact on Test Migration (UPDATED)

- `module_tests.rs` - 2/10 tests pass, 8 ignored (opcode differences)
- `begin_tests.rs` - 11/21 tests pass, 10 ignored (block return issues)
- `compiler_begin_tests.rs` - 10/10 tests pass or properly ignored
- `handler_let_test.rs` & `test_simple_handler.rs` - ✅ Now passing with FLC syntax
- Interpreter tests - 1 test ignored (doesn't support Begin nodes)

## Additional Findings

### Compiler Differences
- FLC generates `Halt` instead of `Return` for top-level code
- Function definitions don't generate `StoreGlobal` opcodes like `define` did
- Match expressions generate different numbers of `Pop` instructions

## Recommendations (UPDATED)

1. ✅ Variable mutation - DONE via `:=` operator
2. 🔄 Module system - Partially done, needs opcode alignment
3. ✅ Effect syntax - DONE via `perform` keyword
4. 🔴 Add commonly used stdlib functions (print, list, quote)
5. 🔴 Fix block return values (currently return nil)
6. 🔴 Consider allowing undefined variable references for error testing