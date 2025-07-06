# FluentAI Contracts Test Suite

This directory contains comprehensive tests for the fluentai-contracts module, organized into multiple test files based on functionality.

## Test Files

### basic_tests.rs
Basic integration tests for core contract functionality:
- Contract creation and initialization
- Adding preconditions, postconditions, and invariants
- Contract condition messages and spans
- Contract serialization/deserialization
- Pure contract marking
- Basic error handling

**8 tests** - All passing ✓

### advanced_tests.rs
Advanced tests for frame conditions and complex contract scenarios:
- Frame conditions with variable modifications
- Frame condition purity checking
- Field and index access modifications
- Heap region modifications
- Allocation flags (may_allocate, may_deallocate)
- Complex access paths and index expressions
- Frame condition builder pattern
- Multiple conditions per contract
- Contract complexity annotations

**13 tests** - All passing ✓

### symbolic_execution_tests.rs
Tests for symbolic execution functionality:
- Symbolic value variants (Concrete, Symbolic, Unknown)
- Symbolic state management and fresh symbol generation
- Binary and unary operations on symbolic values
- Conditional symbolic values
- List and map operations
- String concatenation
- Path constraints
- State forking for branching execution
- Typed symbolic values

**15 tests** - All passing ✓

### evaluator_tests.rs
Tests for contract condition evaluation:
- Evaluator creation and binding management
- Boolean condition evaluation
- Variable resolution  
- Type checking for conditions
- Multiple bindings and isolation
- Literal evaluation (integer, float, string, boolean, nil)
- Simple comparison operations

**12 tests** - All passing ✓

### component_integration_tests.rs
Integration tests between different contract system components:
- Contract evaluation with condition evaluator
- Frame conditions with contracts
- Symbolic execution with path constraints
- Frame condition manager extraction
- Complex condition evaluation with bindings
- Frame condition composition
- Symbolic value structure verification
- Contract serialization with all condition types
- Error propagation across components

**9 tests** - All passing ✓

## Total Test Coverage

- **57 tests** successfully passing across 5 test suites
- Comprehensive coverage of:
  - Basic contract operations
  - Frame conditions and access control
  - Symbolic execution engine
  - Contract condition evaluation
  - Component integration and interaction
  - Contract serialization
  - Error handling and propagation

## Running Tests

Run all tests for the contracts module:
```bash
cargo test -p fluentai-contracts
```

Run specific test suites:
```bash
cargo test -p fluentai-contracts --test basic_tests
cargo test -p fluentai-contracts --test advanced_tests  
cargo test -p fluentai-contracts --test symbolic_execution_tests
cargo test -p fluentai-contracts --test evaluator_tests
cargo test -p fluentai-contracts --test component_integration_tests
```

Run all five test suites:
```bash
cargo test -p fluentai-contracts --test basic_tests --test advanced_tests --test symbolic_execution_tests --test evaluator_tests --test component_integration_tests
```