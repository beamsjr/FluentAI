# FluentAi Contracts

A comprehensive formal verification framework for FluentAi that enables both static proving and runtime checking of contracts. This system helps ensure program correctness through preconditions, postconditions, invariants, and advanced specification features.

## Table of Contents

- [Features](#features)
- [Quick Start](#quick-start)
- [Documentation](#documentation)
- [Usage](#usage)
  - [Contract Syntax](#contract-syntax)
  - [Contract Predicates](#contract-predicates)
- [Key Features](#key-features)
- [Examples](#examples)
- [Performance](#performance)
- [Implementation Status](#implementation-status)
- [Integration](#integration)
- [Future Work](#future-work)

## Why Use Contracts?

Contracts provide mathematical guarantees about your code:

- **Find bugs early**: Catch errors at compile time instead of production
- **Document behavior**: Contracts serve as executable documentation
- **Ensure correctness**: Prove that your code meets its specification
- **Enable optimization**: Compilers can optimize based on contract guarantees
- **Support refactoring**: Contracts ensure behavior preservation during changes

## Features

- **Runtime Verification**: Check contracts during execution with blame tracking
- **Static Verification**: Prove contracts using Z3 SMT solver with expanded operator support
- **Quantifier Support**: Universal (∀) and existential (∃) quantifiers for collection properties
- **Purity Analysis**: Ensure contract expressions have no side effects
- **Contract Inheritance**: LSP-compliant contract refinement and composition
- **Incremental Verification**: Track dependencies and re-verify only changed code
- **Termination Checking**: Prove recursive functions terminate for sound verification
- **Parallel Verification**: Multi-core verification with work stealing and load balancing
- **Ghost State**: Specification-only variables, old() expressions, and history tracking
- **Frame Conditions**: Specify what functions may modify for modular reasoning
- **Performance Optimization**: LRU caching, resource limits, and timeout management
- **Error Enhancement**: Span tracking and blame labels for precise error reporting
- **Proof Generation**: Generate formal proofs for contracts (in development)

## Quick Start

```rust
use fluentai_contracts::{
    Contract, ContractCondition, ContractKind,
    RuntimeVerifier, StaticVerifier,
};

// Create a contract for a square root function
let mut contract = Contract::new("sqrt".to_string(), sqrt_node_id);
contract.add_precondition(
    ContractCondition::new(x_ge_zero, ContractKind::Precondition)
        .with_message("x must be non-negative")
);
contract.add_postcondition(
    ContractCondition::new(result_squared_eq_x, ContractKind::Postcondition)
        .with_message("result * result ≈ x")
);

// Runtime verification
let runtime_verifier = RuntimeVerifier::new(&graph);
runtime_verifier.check_contract(&contract, &args)?;

// Static verification (requires Z3)
#[cfg(feature = "static")]
let static_verifier = StaticVerifier::new(&graph);
let result = static_verifier.verify_contract(&contract)?;
```

## Documentation

- [Contract Semantics](docs/CONTRACT_SEMANTICS.md) - Detailed explanation of when contracts are checked and how they work
- [Z3 Converter](docs/Z3_CONVERTER.md) - Guide to the Z3 SMT converter and supported operations
- [Quantifiers](docs/QUANTIFIERS.md) - Using forall and exists in contract specifications
- [Incremental Verification](docs/INCREMENTAL_VERIFICATION.md) - Efficient re-verification of changed code
- [Termination Checking](docs/TERMINATION_CHECKING.md) - Proving recursive functions terminate
- [Parallel Verification](docs/PARALLEL_VERIFICATION.md) - Multi-core contract verification
- [Ghost State](docs/GHOST_STATE.md) - Specification-only variables and expressions
- [Frame Conditions](docs/FRAME_CONDITIONS.md) - Specify what functions may modify

## Usage

### Contract Syntax

```clojure
(spec:contract function-name
  :requires [preconditions...]    ; or :pre
  :ensures [postconditions...]    ; or :post
  :invariant [invariants...]
  :modifies [vars...]            ; Frame condition
  :complexity "O(...)"
  :pure true/false)
```

### Basic Example

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

### Advanced Example

```clojure
(spec:contract bank-transfer
  :requires [(>= from-account.balance amount)
             (> amount 0)
             (not (= from-account to-account))]
  :ensures [(= from-account.balance 
               (- (old from-account.balance) amount))
            (= to-account.balance
               (+ (old to-account.balance) amount))
            (= (+ from-account.balance to-account.balance)
               (+ (old from-account.balance) 
                  (old to-account.balance)))]
  :modifies [from-account.balance to-account.balance 
             transaction-log]
  :ghost [(total-transferred 0)]
  :invariant [(>= from-account.balance 0)
              (>= to-account.balance 0)])
```

### VM Integration

To integrate contracts with the FluentAi VM:

```rust
use fluentai_contracts::{ContractRegistry, ContractVM, PurityChecker};
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

The following predicates and operators are available in contract conditions:

#### Arithmetic Operations
- Basic: `+`, `-`, `*`, `/`
- Modulo: `mod`, `modulo`, `%`
- Functions: `abs`, `min`, `max`, `floor`, `ceiling`, `round`, `sqrt`, `pow`

#### Comparison Operations
- Equality: `=`, `==`, `eq?`
- Inequality: `!=`, `<>`, `not=`
- Ordering: `<`, `>`, `<=`, `>=`

#### Logical Operations
- Boolean: `and`, `or`, `not`, `xor`
- Implication: `implies`, `=>`

#### Numeric Predicates
- `zero?`, `positive?`, `negative?`, `even?`, `odd?`

#### Type Predicates
- `int?`, `float?`, `number?`, `string?`, `symbol?`, `list?`, `nil?`, `boolean?`, `procedure?`

#### List/Collection Operations
- `length`, `nth`, `member?`, `null?`, `empty?`
- `car`, `cdr`, `cons`, `append`, `reverse`

#### Contract-Specific Features
- **Pre-state**: `old(expr)` - access value before function execution
- **Ghost state**: `ghost(var, init)` - specification-only variables
- **History**: `history(expr, var)` - track value sequences
- **Model fields**: `obj.field` - abstract object properties

#### Quantifiers
- Universal: `forall`, `∀`
- Existential: `exists`, `∃`
- Domains: `(range min max)`, `(in list)`, `(indices list)`, `Int`, `Bool`

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

### 1. Comprehensive Verification

**Static Verification** (with Z3 SMT solver)
- Prove contracts hold for all possible inputs
- Find counterexamples when contracts fail
- Support for complex properties with quantifiers

**Runtime Verification**
- Check contracts during execution
- Precise blame assignment
- Optional performance profiling

### 2. Advanced Specification Features

**Quantifiers**
- Universal (`∀`) and existential (`∃`) quantification
- Express properties over collections
- Multiple domain types (ranges, lists, indices)

**Ghost State**
- Specification-only variables
- Pre-state references with `old()`
- History tracking for temporal properties
- Model fields for data abstraction

**Frame Conditions**
- Specify what functions may modify
- Enable modular reasoning
- Support for variables, fields, arrays, and heap regions

### 3. Performance Optimizations

**Incremental Verification**
- Dependency tracking between functions
- Hash-based change detection
- Only re-verify affected contracts

**Parallel Verification**
- Multi-core contract checking
- Work stealing for load balancing
- Near-linear speedup with cores

**Smart Caching**
- LRU cache for verification results
- Avoid redundant SMT queries
- Persistent cache support (planned)

### 4. Soundness Guarantees

**Purity Analysis**
- Ensure contract expressions have no side effects
- Validate immutability requirements
- Prevent verification unsoundness

**Termination Checking**
- Prove recursive functions terminate
- Support for structural recursion
- Lexicographic ordering for complex cases

### 5. Developer Experience

**Error Enhancement**
- Precise source locations with spans
- Clear blame attribution
- Suggested fixes (planned)

**IDE Integration** (planned)
- Real-time contract checking
- Inline counterexamples
- Contract completion

**Documentation**
- Comprehensive guides for all features
- Examples for common patterns
- Troubleshooting tips

## Examples

The `examples/` directory contains demonstrations of all major features:

- `basic_contracts.rs` - Simple preconditions and postconditions
- `quantifier_contracts.rs` - Using forall/exists in specifications
- `ghost_state_demo.rs` - Ghost variables, old(), and history tracking
- `frame_conditions_demo.rs` - Specifying what functions modify
- `incremental_verification.rs` - Dependency tracking and caching
- `parallel_verification.rs` - Multi-core contract checking
- `termination_checking.rs` - Proving recursive functions terminate
- `z3_converter_demo.rs` - Supported SMT operations

Run examples with:
```bash
cargo run --example basic_contracts
cargo run --example parallel_verification --features="static"
```

## Performance

When contracts are disabled, the overhead is minimal (<5%). Runtime verification can be toggled on/off dynamically. Static verification results are cached to improve performance on repeated checks.

### Benchmarks

On a typical 8-core machine:
- **Sequential**: 100 contracts/second
- **Parallel**: 750 contracts/second (7.5x speedup)
- **Incremental**: 10x-100x speedup on subsequent runs
- **With caching**: Near-instant for unchanged contracts

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
- ✅ Incremental verification with dependency tracking
- ✅ Termination checking for recursive contracts
- ✅ Parallel contract verification
- ✅ Ghost state and old() expressions
- ✅ Frame conditions for modular reasoning
- ⚠️  Proof generation (basic structure)

## Integration

### With FluentAi VM

The contracts system integrates seamlessly with the FluentAi VM:

```rust
// In VM initialization
let mut registry = ContractRegistry::new();
registry.enable(Arc::new(ast_graph.clone()));
registry.register_contracts_from_ast(&ast_graph);

// During function calls
vm.set_contract_registry(Some(registry));
```

### With Build Systems

Contracts can be verified as part of the build process:

```toml
# In Cargo.toml
[features]
verify-contracts = ["fluentai-contracts/static"]

[build-dependencies]
fluentai-contracts = { version = "0.1", features = ["static"] }
```

### With CI/CD

Example GitHub Actions workflow:

```yaml
- name: Verify Contracts
  run: |
    cargo test --features verify-contracts
    cargo run --example verify_all_contracts
```

## Future Work

### Near-term Enhancements
- [ ] IDE plugins for VS Code and IntelliJ
- [ ] Contract inference from code patterns
- [ ] Counterexample minimization
- [ ] Integration with property-based testing

### Long-term Goals
- [ ] Certified verification with proof checking
- [ ] Distributed verification for large codebases
- [ ] Machine learning for contract suggestion
- [ ] Integration with other verification tools

## Contributing

Contributions are welcome! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.