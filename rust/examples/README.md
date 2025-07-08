# FluentAI Examples

This directory contains example programs written in FluentAI (`.ai` files) that demonstrate various language features and capabilities.

## Getting Started

To run these examples, you need to have the FluentAI runtime installed. You can run an example using:

```bash
cargo run -p fluentai-cli -- run examples/hello.ai
```

Or if you have the REPL installed:

```bash
fluentai-repl examples/hello.ai
```

## Basic Examples

### hello.ai
The classic "Hello, World!" program - performs basic arithmetic.
```bash
cargo run -p fluentai-cli -- run hello.ai
# Output: 42
```

### factorial.ai / factorial_simple.ai
Different implementations of the factorial function, demonstrating basic recursion and function definitions.
```bash
cargo run -p fluentai-cli -- run factorial.ai
# Output: 120
```

### effects_simple.ai
Simple demonstration of the effect system with basic IO operations.

## Core Language Features

### let_letrec.ai
Demonstrates different binding forms:
- Basic `let` bindings
- Nested scopes and shadowing
- `letrec` for recursive definitions
- Mutually recursive functions
- Closure capture

### pattern_matching.ai / pattern_matching_simple.ai
Pattern matching examples:
- Literal patterns
- Variable binding
- Wildcard patterns
- Constructor patterns (ADTs)
- List patterns (Cons/Nil)
- As-patterns
- Guard patterns
- Nested patterns

### list_operations.ai
Working with lists:
- List construction with `cons`
- Pattern matching on lists
- Recursive list processing
- Common operations: map, filter, fold, reverse
- List traversal patterns

### recursion_tail_calls.ai
Recursion patterns and optimization:
- Simple vs tail recursion
- Accumulator patterns
- Mutual recursion
- Tree traversal
- Tail call optimization benefits

## Advanced Features

### higher_order_functions.ai
Functions as first-class values:
- Functions returning functions
- Function composition
- Currying and partial application
- Map, filter, fold implementations
- Memoization

### async_await.ai
Concurrent programming:
- Async functions and promises
- Using `await`
- Channels for communication
- `spawn` for concurrent tasks
- Producer-consumer patterns

### effects_demo.ai
Comprehensive effect system usage:
- IO effects (print, read)
- File operations
- State management
- Error handling
- Time effects
- Network effects (simulated)
- Custom effect handlers
- Effect composition

### error_handling.ai
Error handling patterns:
- Result types (Ok/Error)
- Pattern matching on results
- Effect handlers for recovery
- Error propagation
- Multiple error types
- Resource cleanup patterns

### modules_imports.ai
Module system demonstration:
- Defining modules with exports
- Importing specific functions
- Import all (`*`)
- Using imported functions
- Exporting from current module

## Performance Benchmarks

For performance benchmarks and tests, see the `rust/benchmarks/` directory:
- `throughput_benchmark.rs` - Measures operations per second (19.2M ops/sec average)
- `parser_benchmark.rs` - Parser performance tests (0.8-5.2 µs)
- `simd_benchmark.rs` - SIMD operations benchmarks (4-8x speedup)
- `vm_performance.rs` - Detailed VM performance analysis

To run benchmarks:
```bash
cd ../benchmarks
cargo run --release --bin throughput_benchmark
```

## Additional Resources

### IoT Pipeline Example
The `iot_pipeline/` subdirectory contains a more complex example of an IoT data processing pipeline written in FluentAI, demonstrating:
- Type definitions
- Stream processing
- Contract-based design
- Performance optimization

## Running All Examples

To run all FluentAI examples in sequence:

```bash
for example in *.ai; do
    echo "Running $example..."
    cargo run -p fluentai-cli -- run "$example"
    echo "---"
done
```

## Contributing

When adding new examples:
1. Use the `.ai` file extension
2. Include a header comment explaining what the example demonstrates
3. Keep examples focused on specific features
4. Add an entry to this README
5. Ensure the example actually runs without errors

## Notes

- These examples assume a working FluentAI implementation
- Some advanced features may not be fully implemented yet
- Error messages and exact syntax may vary with language evolution
- For Rust integration tests, see `rust/tests/integration/`