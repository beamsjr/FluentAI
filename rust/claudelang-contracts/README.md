# ClaudeLang Contracts

This crate implements formal verification capabilities for ClaudeLang through preconditions, postconditions, and invariants.

## Features

- **Runtime Verification**: Check contracts during execution
- **Static Verification**: Prove contracts using SMT solving (optional, requires Z3)
- **Proof Generation**: Generate formal proofs for contracts (optional)
- **Contract Syntax**: Compatible with Python ClaudeLang contracts

## Usage

### Contract Syntax

```clojure
(spec:contract function-name
  :requires [preconditions...]    ; or :pre
  :ensures [postconditions...]    ; or :post
  :invariant [invariants...]
  :complexity "O(...)"
  :pure true/false)
```

### Example Contract

```clojure
(spec:contract factorial
  :requires [(>= n 0)]
  :ensures [(>= result 1)]
  :complexity "O(n)"
  :pure true)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

### VM Integration

To integrate contracts with the ClaudeLang VM:

```rust
use claudelang_contracts::{ContractRegistry, ContractVM};
use std::sync::Arc;

// During VM initialization
let mut contract_registry = ContractRegistry::new();
contract_registry.enable(Arc::new(ast_graph.clone()));
contract_registry.register_contracts_from_ast(&ast_graph);

// Before function calls
contract_registry.check_preconditions(&function_name, &args)?;

// After function returns
contract_registry.check_postconditions(&function_name, &args, &result)?;

// Check purity violations
if had_side_effects && contract_registry.is_pure_function(&function_name) {
    return Err(ContractError::PurityViolation(...));
}
```

### Contract Predicates

The following predicates are available in contract conditions:

**Comparison**: `=`, `!=`, `<`, `>`, `<=`, `>=`
**Arithmetic**: `+`, `-`, `*`, `/`
**Type predicates**: `int?`, `float?`, `number?`, `string?`, `list?`, `nil?`
**List operations**: `length`, `nth`, `empty?`
**Logical**: `and`, `or`, `not`
**Custom**: `sorted?`, `file-exists?`

## Features

- `runtime` (default): Runtime contract verification
- `static`: Static verification with Z3
- `proof`: Proof generation (requires `static`)
- `full`: All features enabled

## Performance

When contracts are disabled, the overhead is minimal (<5%). Runtime verification can be toggled on/off dynamically.

## Implementation Status

- ✅ Contract parsing
- ✅ Runtime verification engine
- ✅ AST-based condition evaluation
- ✅ VM integration framework
- ⚠️  Static verification (basic structure only)
- ⚠️  Proof generation (basic structure only)
- ❌ Contract inheritance
- ❌ Dependent contracts
- ❌ Resource contracts