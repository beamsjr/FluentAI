# FluentAI Examples

This directory contains example programs written in FluentAI (`.ai` files) that demonstrate various language features and capabilities.

## Getting Started

To run these examples, navigate to the `rust` directory and use the FluentAI CLI:

```bash
cd rust
cargo run -p fluentai-cli -- run examples/hello.ai
```

## Current Implementation Status

**Working Features:**
- Basic arithmetic (`+`, `-`, `*`, `/`, `%`)
- Let bindings (`let`)
- Lambda functions (`lambda`)
- Recursive functions (`letrec`) - has bugs with recursion
- Lists (`list`, `cons`, `head`, `tail`)
- Pattern matching (`match`)
- Conditionals (`if`)
- Comparisons (`=`, `<`, `>`, `<=`, `>=`)
- Print function for output
- Effects (`effect`) - All default effect handlers work!
  - IO: `print`
  - Error: `raise`
  - State: `get`, `set`
  - Time: `now`
  - Random: `float`, `int`

**Not Yet Implemented:**
- Module system (`module`, `import`, `export`)
- Custom effect handlers (`handler` form has runtime bug)
- Async/await operations
- Channel operations (`channel`, `send`, `receive`)
- Define syntax (`define`)
- Begin blocks (`begin`)
- Many advanced features shown in the main README

## Working Examples

### hello.ai
Simple arithmetic expression.
```bash
cargo run -p fluentai-cli -- run examples/hello.ai
# Output: 42
```

### arithmetic.ai  
Basic arithmetic operations.
```bash
cargo run -p fluentai-cli -- run examples/arithmetic.ai
# Output: 30
```

### let_binding.ai
Demonstrates let bindings with multiple variables.
```bash
cargo run -p fluentai-cli -- run examples/let_binding.ai
# Output: 30
```

### lambda.ai
Lambda function definition and application.
```bash
cargo run -p fluentai-cli -- run examples/lambda.ai
# Output: 49
```

### lists.ai
Basic list creation and manipulation.
```bash
cargo run -p fluentai-cli -- run examples/lists.ai  
# Output: [0, 1, 2, 3, 4, 5]
```

### match_example.ai
Pattern matching on literal values.
```bash
cargo run -p fluentai-cli -- run examples/match_example.ai
# Output: "the answer"
```

### recursion.ai
Recursive function using letrec (note: factorial has a bug returning 1).
```bash
cargo run -p fluentai-cli -- run examples/recursion.ai
# Output: 1 (should be 720)
```

### higher_order.ai
Functions that return functions.
```bash
cargo run -p fluentai-cli -- run examples/higher_order.ai
# Output: 15
```

### pattern_match.ai
Pattern matching with a function.
```bash
cargo run -p fluentai-cli -- run examples/pattern_match.ai
# Output: "zero" "one" "other" "done"
```

### effects.ai
Demonstrates working effect system with default handlers.
```bash
cargo run -p fluentai-cli -- run examples/effects.ai
# Output: Prints "Hello, World!" and demonstrates various effects
```

### simple_list_ops.ai
Nested let expressions for list operations.
```bash
cargo run -p fluentai-cli -- run examples/simple_list_ops.ai
# Output: [0, 1, 2, 3, 4, 5]
```

## Examples from Original Files

The following files demonstrate syntax that is used in the codebase but may have limitations:

### factorial.ai / factorial_simple.ai
Factorial implementations using letrec - currently has a bug where it returns 1 instead of the correct result.

### effects_simple.ai
Contains examples using features not yet implemented (define, begin, etc.)

### pattern_matching_simple.ai  
Uses conditional-based pattern matching rather than the match expression.

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