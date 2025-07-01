# ClaudeLang Test Suite

This directory contains the comprehensive test suite for ClaudeLang, including both traditional unit tests and property-based tests.

## Test Organization

### Core Tests
- `test_parser.py` - Parser functionality
- `test_interpreter.py` - Interpreter and evaluation
- `test_ast.py` - AST node types and operations
- `test_effects.py` - Effect system
- `test_modules.py` - Module system and imports
- `test_pattern_matching.py` - Pattern matching functionality
- `test_adt.py` - Algebraic data types
- `test_type_annotations.py` - Type annotation syntax

### Optimization and Compilation Tests
- `test_optimizer.py` - Graph optimization
- `test_vm.py` - Virtual machine execution
- `test_jit.py` - JIT compilation
- `test_bytecode.py` - Bytecode generation
- `test_llvm_backend.py` - LLVM code generation

### Advanced Features
- `test_contracts.py` - Contract system
- `test_proof_generation.py` - Proof generation for optimizations
- `test_metaprogramming.py` - Graph transformation and metaprogramming
- `test_versioning.py` - Semantic versioning
- `test_ml_optimization.py` - Machine learning optimization

### Property-Based Tests
- `test_properties.py` - Property-based tests using Hypothesis
- See `README_properties.md` for detailed information

### Standard Library Tests
- `test_stdlib_*.py` - Tests for standard library modules

## Running Tests

### Run all tests:
```bash
python -m unittest discover tests
```

### Run specific test file:
```bash
python -m unittest tests.test_parser
```

### Run with verbose output:
```bash
python -m unittest discover tests -v
```

### Run property-based tests:
```bash
python -m unittest tests.test_properties
```

## Test Coverage

The test suite aims for comprehensive coverage of:
- All language features
- Edge cases and error conditions
- Performance characteristics
- Integration between components
- Properties that should hold for all inputs

## Writing Tests

When adding new features:
1. Write unit tests for specific functionality
2. Add integration tests for feature interactions
3. Consider adding property-based tests for invariants
4. Ensure error cases are tested
5. Document expected behavior in test names and docstrings