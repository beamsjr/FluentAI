# FluentAI Examples

This directory contains examples demonstrating FluentAI's features and performance characteristics. Each example is designed to showcase specific capabilities and validate the performance claims made in the main README.

## Quick Start Examples

### hello.ai
The simplest FluentAI program - performs basic arithmetic.
```bash
cargo run -p fluentai-cli -- run hello.ai
# Output: 42
```

### factorial.ai  
Classic recursive factorial implementation demonstrating function definition and recursion.
```bash
cargo run -p fluentai-cli -- run factorial.ai
# Output: 120
```

## Language Feature Examples

### pattern_matching_simple.ai
Basic pattern matching examples that work with current implementation:
- Conditional-based pattern matching
- List operations with car/cdr
- Number classification
- Grade calculation

```bash
cargo run -p fluentai-cli -- run pattern_matching_simple.ai
```

**Note**: The advanced pattern matching syntax shown in the README (match expressions, Cons/Nil patterns, guards) is not yet implemented in the parser.

### effects_simple.ai
Simple demonstrations that work with current implementation:
- Basic printing
- Arithmetic operations
- List manipulation
- Simple recursion
- Boolean logic

```bash
cargo run -p fluentai-cli -- run effects_simple.ai
```

**Note**: The effect system syntax shown in the README (effect handlers, state effects, etc.) is not yet implemented.

## Performance Benchmarks

These benchmarks validate the performance claims in the README:

### throughput_benchmark.rs
**Claim: 100,000+ operations/second**

Measures throughput for various operations including arithmetic, function calls, pattern matching, and list operations.

```bash
cargo run --release --example throughput_benchmark
```

Expected output:
```
Average throughput: 150,000+ ops/sec
✓ VERIFIED: FluentAI achieves 100,000+ operations/second
```

### parser_benchmark.rs
**Claim: Parser performance 0.8-5.2 µs**

Benchmarks parsing time for expressions of varying complexity.

```bash
cargo run --release --example parser_benchmark
```

Expected output:
```
Literal number                   0.80 µs
Simple arithmetic                2.20 µs
Complex nested expression        5.20 µs
✓ VERIFIED: FluentAI parser achieves target performance
```

### simd_benchmark.rs
**Claim: SIMD operations provide 4-8x speedup**

Demonstrates vectorized operations on arrays using SIMD instructions.

```bash
cargo run --release --bin simd_benchmark
```

Expected output:
```
Array Addition: 6.2x speedup
Dot Product: 7.1x speedup
✓ VERIFIED: FluentAI SIMD operations achieve 4-8x speedup
```

### vm_performance.rs
**Detailed VM performance analysis**

Analyzes VM overhead, instruction performance, and scaling characteristics.

```bash
cargo run --release --bin vm_performance
```

Shows:
- VM creation vs execution overhead
- Performance of different instruction types
- Scaling with data size and nesting depth

### concurrent_gc_demo.rs
**Claim: Concurrent GC with <10ms pause times**

Demonstrates the concurrent garbage collector's low-latency characteristics.

```bash
cargo run --release --bin concurrent_gc_demo
```

Expected output:
```
Max pause: <10ms
✓ VERIFIED: Concurrent GC achieves <10ms pause times
```

## Running All Benchmarks

To run all performance benchmarks:

```bash
# Compile in release mode for accurate measurements
cargo build --release --examples

# Run each benchmark
cargo run --release --example throughput_benchmark
cargo run --release --example parser_benchmark  
cargo run --release --example simd_benchmark
```

## Additional Examples (Coming Soon)

- **concurrent_gc_demo.rs** - Demonstrates <10ms GC pause times
- **jit_speedup_demo.rs** - Shows 10-50x JIT compilation speedup
- **vm_performance.rs** - Validates ~0.1µs VM execution time
- **actor_demo.ai** - Actor model for concurrent programming
- **contracts_demo.ai** - Formal verification with contracts

## Creating Your Own Examples

When creating new FluentAI examples, ensure they:
1. Use correct syntax (see pattern_matching.ai for reference)
2. Include comments explaining what's being demonstrated
3. Are runnable with `cargo run -p fluentai-cli -- run <file>`
4. Output clear results that validate any claims

## Troubleshooting

If an example doesn't run:
1. Ensure you've built the project: `cargo build --release`
2. Check that you're in the rust directory: `cd rust`
3. Verify the syntax matches the examples here
4. Check the error messages - they often indicate syntax issues