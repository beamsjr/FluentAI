#!/usr/bin/env python3
"""Test Rust bindings are working correctly"""

import sys
sys.path.insert(0, 'rust/target/debug')
import fluentai_rust

# Test simple parsing
test_cases = [
    "(+ 1 2)",
    "42",
    "(let ((x 10)) (+ x 5))",
    "[1 2 3 4 5]",
]

print("Testing Rust Python bindings...")
print("=" * 50)

for code in test_cases:
    try:
        result = fluentai_rust.parse(code)
        print(f"✓ Parse '{code}' -> {result}")
        
        # Check if we can access the root_id
        if hasattr(result, 'root_id'):
            print(f"  Root ID: {result.root_id}")
    except Exception as e:
        print(f"✗ Parse '{code}' failed: {e}")

print("\nAll tests completed!")