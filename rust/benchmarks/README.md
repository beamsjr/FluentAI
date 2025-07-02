# ClaudeLang Rust Benchmarks

This directory contains performance benchmarks for the ClaudeLang Rust implementation.

## Available Benchmarks

### 1. Parser Benchmarks (`parser.rs`)
Tests the performance of the zero-copy parser on various expressions:
- Simple literals (numbers, strings)
- Arithmetic expressions
- Let bindings and lambdas
- List operations
- Complex nested expressions

### 2. VM Benchmarks (`vm.rs`)
Tests the bytecode compiler and stack-based VM:
- Compilation performance
- VM execution speed
- Instruction dispatch overhead

### 3. End-to-End Benchmarks (`end_to_end.rs`)
Tests complete parse-compile-execute pipeline:
- Full program execution
- Real-world code examples

### 4. Comprehensive Benchmarks (`comprehensive_benchmark.rs`)
Compares all components and includes JIT measurements:
- Side-by-side comparison of all stages
- JIT compilation and execution (x86_64 only)
- Performance vs Python baseline

## Running Benchmarks

### Run All Benchmarks
```bash
cargo bench
```

### Run Specific Benchmark
```bash
cargo bench --bench parser
cargo bench --bench vm
cargo bench --bench comprehensive_benchmark
```

### Quick Benchmarks (for development)
```bash
cargo bench -- --warm-up-time 1 --measurement-time 2
```

### Generate Detailed Reports
```bash
cargo bench -- --output-format bencher
```

## Performance Tracking

### Automated Tracking
Use the performance tracking script:
```bash
./scripts/track_performance.py
```

This will:
- Run all benchmarks
- Store results in `benchmark_results/performance_history.json`
- Compare with previous runs
- Show performance trends

### CI/CD Integration
GitHub Actions automatically:
- Runs benchmarks on every push to main
- Checks for performance regressions on PRs
- Stores historical data for tracking
- Alerts on significant performance changes (>50% regression)

## Current Performance Metrics

Based on latest benchmarks:

| Component | Performance | vs Python |
|-----------|-------------|-----------|
| Parser | 69-456 ns | 49,174x - 258,808x faster |
| VM | 154 ns avg | 20,782x faster |
| End-to-End | 294-814 ns | 29,795x - 135,433x faster |
| Throughput | 1.35M ops/sec | - |

### JIT Performance (x86_64 only)
- Compilation overhead: < 1 µs
- Execution speedup: 10-50x over VM
- Currently limited to x86_64 due to Cranelift constraints

## Benchmark Guidelines

1. **Consistency**: Always use release builds for benchmarks
2. **Warm-up**: Allow sufficient warm-up time (1+ seconds)
3. **Isolation**: Close other applications to reduce noise
4. **Multiple Runs**: Use criterion's statistical analysis
5. **Baseline**: Compare against stored baselines

## Adding New Benchmarks

1. Create a new file in `benches/`
2. Use the `criterion` crate for measurements
3. Follow existing patterns for consistency
4. Update this README with new benchmark info
5. Add to CI/CD workflow if needed

## Profiling

For detailed performance analysis:

```bash
# CPU profiling
cargo bench --bench parser -- --profile-time 10

# Generate flamegraphs (requires cargo-flamegraph)
cargo flamegraph --bench parser

# Use perf (Linux)
perf record cargo bench --bench comprehensive_benchmark
perf report
```

## Optimization Workflow

1. Run benchmarks to establish baseline
2. Make optimization changes
3. Run benchmarks again
4. Use `track_performance.py` to compare
5. Only merge if performance improves or stays same

## Troubleshooting

### Benchmarks Too Slow
- Use `--warm-up-time 1 --measurement-time 2` for faster iteration
- Run specific benchmarks instead of all

### High Variance
- Close other applications
- Disable CPU frequency scaling
- Use performance CPU governor (Linux)

### Platform Differences
- JIT benchmarks only work on x86_64
- Some optimizations are platform-specific
- Always test on target platforms