#!/usr/bin/env python3
"""Test effect primitives as regular functions"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from src.repl.repl import REPLEnvironment

# Test effect primitives
env = REPLEnvironment()
env.use_vm = False  # Use interpreter

test_cases = [
    ("(io:print \"Testing primitives!\")", "IO primitive"),
    ("(state:set \"y\" 100)", "State set primitive"),
    ("(state:get \"y\")", "State get primitive"), 
    ("(time:now)", "Time primitive"),
    ("(random:float)", "Random primitive"),
    ("(+ (state:get \"y\") 50)", "Using state in expression"),
]

print("Testing Effect Primitives\n")

for code, desc in test_cases:
    print(f"Test: {desc}")
    print(f"Code: {code}")
    try:
        result = env.evaluate(code)
        print(f"Result: {result}")
        print("✓ PASS\n")
    except Exception as e:
        print(f"✗ FAIL: {e}\n")

print("Primitive tests completed!")