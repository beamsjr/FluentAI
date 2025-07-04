# Rust Integration Tests

This directory contains **integration tests** for the FluentAi Rust implementation, not user-facing examples.

## Purpose

These files test various aspects of the Rust implementation:
- VM functionality (`test_vm.rs`, `test_*_closure.rs`)
- Compiler features (`test_compiler_debug.rs`, `test_parser.rs`)
- Language features (`test_async_*.rs`, `test_effects_*.rs`, `test_pattern_*.rs`)
- Performance benchmarks (`optimization_benchmark.rs`, `real_performance.rs`)

## For User Examples

If you're looking for FluentAi language examples to learn from, please see:
- `/examples/` - User-facing demos and tutorials
- `/docs/QUICK_START.md` - Getting started guide
- `/docs/tutorial.md` - Comprehensive tutorial

## Running These Tests

These tests are meant to be run as part of the Rust development workflow:

```bash
# Run a specific test
cargo run --example test_vm

# Run with optimization
cargo run --release --example optimization_benchmark
```

## File Types

- `.rs` files - Rust integration tests that test the implementation
- `.cl` files - Simple FluentAi source files used as test fixtures
- `.md` files - Documentation for specific test suites

## Note

These are NOT examples of how to use FluentAi. They are tests that verify the Rust implementation works correctly. They often contain edge cases, error conditions, and implementation-specific details that are not relevant to FluentAi users.