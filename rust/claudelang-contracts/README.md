# ClaudeLang Contracts

This crate implements formal verification capabilities for ClaudeLang through preconditions, postconditions, and invariants.

## Features

- **Runtime Verification**: Check contracts during execution with blame tracking
- **Static Verification**: Prove contracts using Z3 SMT solver with expanded operator support
- **Quantifier Support**: Universal (∀) and existential (∃) quantifiers for collection properties
- **Purity Analysis**: Ensure contract expressions have no side effects
- **Contract Inheritance**: LSP-compliant contract refinement and composition
- **Performance Optimization**: LRU caching, resource limits, and timeout management
- **Error Enhancement**: Span tracking and blame labels for precise error reporting
- **Proof Generation**: Generate formal proofs for contracts (in development)

## Documentation

- [Contract Semantics](docs/CONTRACT_SEMANTICS.md) - Detailed explanation of when contracts are checked and how they work
- [Z3 Converter](docs/Z3_CONVERTER.md) - Guide to the Z3 SMT converter and supported operations
- [Quantifiers](docs/QUANTIFIERS.md) - Using forall and exists in contract specifications

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
use claudelang_contracts::{ContractRegistry, ContractVM, PurityChecker};
use std::sync::Arc;

// During VM initialization
let mut contract_registry = ContractRegistry::new();
contract_registry.enable(Arc::new(ast_graph.clone()));
contract_registry.register_contracts_from_ast(&ast_graph);

// Validate contract purity
let mut purity_checker = PurityChecker::new(&ast_graph);
for contract in contracts {
    purity_checker.validate_contract_purity(&contract)?;
}

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

**Arithmetic**: `+`, `-`, `*`, `/`, `mod`/`%`, `abs`, `min`, `max`
**Comparison**: `=`/`==`, `!=`/`<>`, `<`, `>`, `<=`, `>=`
**Logical**: `and`, `or`, `not`, `xor`, `implies`/`=>`
**Numeric Predicates**: `zero?`, `positive?`, `negative?`, `even?`, `odd?`
**Type predicates**: `int?`, `float?`, `number?`, `string?`, `list?`, `nil?`
**List operations**: `length`, `nth`, `member?`, `null?`/`empty?`
**Contract-specific**: `old` (access pre-state values)
**Quantifiers**: `forall`/`∀`, `exists`/`∃` with domains like `(range min max)`, `(in list)`, `(indices list)`

### Example: Complex Contract

```clojure
(spec:contract binary-search
  :requires [(forall ((i (indices arr)))
               (implies (< i (- (length arr) 1))
                        (<= (nth arr i) (nth arr (+ i 1)))))  ; sorted
             (>= target 0)]
  :ensures [(implies (>= result 0)
                     (= (nth arr result) target))
            (implies (< result 0)
                     (forall ((i (indices arr)))
                       (!= (nth arr i) target)))]  ; not found
  :complexity "O(log n)"
  :pure true)
```

## Features

- `runtime` (default): Runtime contract verification
- `static`: Static verification with Z3
- `proof`: Proof generation (requires `static`)
- `full`: All features enabled

## Key Features

### Purity Analysis
Contract expressions must be pure (no side effects). The purity checker validates that contract conditions only use:
- Pure arithmetic and comparison operations
- Immutable data access
- No I/O or state mutations

### Resource Management
- **Timeout control**: Set verification timeouts to prevent runaway solvers
- **Memory limits**: Constrain Z3 memory usage
- **Recursion depth**: Limit verification depth for recursive contracts
- **LRU caching**: Cache verification results to avoid redundant checks

### Error Reporting
- **Span tracking**: Precise source locations for contract violations
- **Blame labels**: Clear attribution of who violated the contract (caller vs implementation)
- **Contextual messages**: Helpful error messages with contract details

## Performance

When contracts are disabled, the overhead is minimal (<5%). Runtime verification can be toggled on/off dynamically. Static verification results are cached to improve performance on repeated checks.

## Implementation Status

- ✅ Contract parsing
- ✅ Runtime verification engine with blame tracking
- ✅ AST-based condition evaluation
- ✅ VM integration framework
- ✅ Static verification with Z3 SMT solver
- ✅ Purity analysis for contract expressions
- ✅ Contract inheritance with LSP compliance
- ✅ Caching and resource management
- ✅ Enhanced error messages with spans
- ✅ Quantifier support (forall/exists)
- ⚠️  Proof generation (basic structure)
- ❌ Ghost state and old() expressions
- ❌ Frame conditions
- ❌ Incremental verification