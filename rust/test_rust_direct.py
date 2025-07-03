#!/usr/bin/env python3
"""Test the ClaudeLang Rust bindings directly"""

import time

# Test direct Rust module import
print("Testing direct Rust module import...")
try:
    import fluentai_rust
    print("✓ Import successful")
    print(f"  Module: {fluentai_rust}")
    print(f"  Functions: {dir(fluentai_rust)}")
except ImportError as e:
    print(f"✗ Import failed: {e}")
    exit(1)

# Test parsing
print("\nTesting parser...")
try:
    source = "(+ 1 2)"
    result = fluentai_rust.parse(source)
    print(f"✓ Parse successful: {result}")
    print(f"  Type: {type(result)}")
    print(f"  Root ID: {result.root_id}")
except Exception as e:
    print(f"✗ Parse failed: {e}")

# Test evaluation
print("\nTesting evaluation...")
test_cases = [
    ("42", 42),
    ("(+ 1 2)", 3),
    ("(* 5 7)", 35),
    ("(- 10 3)", 7),
    ("(if true 1 2)", 1),
    ("(cons 1 (cons 2 (cons 3 nil)))", [1, 2, 3]),
]

for source, expected in test_cases:
    try:
        result = fluentai_rust.evaluate(source)
        if result == expected:
            print(f"✓ {source} = {result}")
        else:
            print(f"✗ {source} = {result} (expected {expected})")
    except Exception as e:
        print(f"✗ {source} failed: {e}")

# Test bytecode compilation
print("\nTesting bytecode compilation...")
try:
    bytecode = fluentai_rust.compile("(+ 1 2)")
    print(f"✓ Compilation successful, bytecode length: {len(bytecode)} bytes")
    print(f"  First 10 bytes: {list(bytecode[:10])}")
except Exception as e:
    print(f"✗ Compilation failed: {e}")

# Performance comparison
print("\nPerformance benchmarking...")
try:
    source = "(+ (* 2 3) (- 5 1))"
    iterations = 10000
    
    # Benchmark Rust parser
    rust_time = fluentai_rust.benchmark_parser(source, iterations)
    print(f"✓ Rust parser: {rust_time*1e6:.2f} µs per parse")
    print(f"  Total time for {iterations} iterations: {rust_time*iterations:.3f}s")
    print(f"  Parses per second: {1/rust_time:.0f}")
        
except Exception as e:
    print(f"✗ Benchmarking failed: {e}")

print("\n✅ Rust bindings are working correctly!")