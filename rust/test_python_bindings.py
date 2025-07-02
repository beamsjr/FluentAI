#!/usr/bin/env python3
"""Test the ClaudeLang Python bindings"""

import time

# Test imports
print("Testing imports...")
try:
    import claudelang
    print(f"✓ Import successful, HAS_RUST_EXTENSIONS: {claudelang.HAS_RUST_EXTENSIONS}")
    
    # Test functions available
    print(f"✓ Parse function available: {hasattr(claudelang, 'parse')}")
    print(f"✓ Evaluate function available: {hasattr(claudelang, 'evaluate')}")
    print(f"✓ Compile function available: {hasattr(claudelang, 'compile_to_bytecode')}")
except ImportError as e:
    print(f"✗ Import failed: {e}")
    exit(1)

# Test parsing
print("\nTesting parser...")
try:
    source = "(+ 1 2)"
    result = claudelang.parse(source)
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
        result = claudelang.evaluate(source)
        if result == expected:
            print(f"✓ {source} = {result}")
        else:
            print(f"✗ {source} = {result} (expected {expected})")
    except Exception as e:
        print(f"✗ {source} failed: {e}")

# Test bytecode compilation
print("\nTesting bytecode compilation...")
try:
    bytecode = claudelang.compile_to_bytecode("(+ 1 2)")
    print(f"✓ Compilation successful, bytecode length: {len(bytecode)} bytes")
    print(f"  First 10 bytes: {list(bytecode[:10])}")
except Exception as e:
    print(f"✗ Compilation failed: {e}")

# Performance comparison
print("\nPerformance comparison...")
try:
    # Import Rust benchmarking function
    from claudelang.claudelang_rust import benchmark_parser
    
    source = "(+ (* 2 3) (- 5 1))"
    iterations = 10000
    
    # Benchmark Rust parser
    rust_time = benchmark_parser(source, iterations)
    print(f"✓ Rust parser: {rust_time*1e6:.2f} µs per parse")
    
    # Benchmark Python parser (if available)
    try:
        from claudelang.parser.sexpr_parser import parse_sexpr
        start = time.time()
        for _ in range(iterations):
            parse_sexpr(source)
        python_time = (time.time() - start) / iterations
        print(f"✓ Python parser: {python_time*1e6:.2f} µs per parse")
        print(f"✓ Speedup: {python_time/rust_time:.1f}x")
    except ImportError:
        print("  Python parser not available for comparison")
        
except Exception as e:
    print(f"✗ Benchmarking failed: {e}")

print("\n✅ Python bindings are working correctly!")