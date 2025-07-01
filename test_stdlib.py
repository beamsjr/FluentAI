#!/usr/bin/env python3
"""Test standard library functions"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from src.repl.repl import REPLEnvironment

# Test standard library
env = REPLEnvironment()
env.use_vm = False  # Use interpreter

test_cases = [
    # Math
    ("(sin 0)", "Math: sin(0)"),
    ("(sqrt 16)", "Math: sqrt(16)"),
    ("(pow 2 8)", "Math: 2^8"),
    ("(factorial 5)", "Math: 5!"),
    ("(mean [1 2 3 4 5])", "Math: mean"),
    
    # Strings
    ('(string-upcase "hello")', "String: uppercase"),
    ('(string-trim "  hello  ")', "String: trim"),
    ('(string-split "a,b,c" ",")', "String: split"),
    
    # Data structures
    ("(dict-set (dict-new) \"key\" \"value\")", "Dict: create and set"),
    ("(set-to-list (set-from-list [1 2 2 3]))", "Set: from list"),
    
    # Functional
    ("((compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 5)", "Compose functions"),
    ("(map-indexed (lambda (i x) (tuple i x)) [10 20 30])", "Map with index"),
    
    # DateTime
    ("(datetime:year (datetime:create 2024 3 15 10 30 0))", "DateTime: get year"),
]

print("Testing Standard Library\n")

for code, desc in test_cases:
    print(f"Test: {desc}")
    print(f"Code: {code}")
    try:
        result = env.evaluate(code)
        print(f"Result: {result}")
        print("✓ PASS\n")
    except Exception as e:
        print(f"✗ FAIL: {e}\n")

print("Standard library tests completed!")