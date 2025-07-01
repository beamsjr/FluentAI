#!/usr/bin/env python3
"""Test the effect system in the REPL"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from src.repl.repl import REPLEnvironment

# Test effect system
env = REPLEnvironment()
env.use_vm = False  # Use interpreter for now

test_cases = [
    ("(effect io:print \"Hello from effects!\")", "IO effect"),
    ("(effect state:set \"x\" 42)", "State set"),
    ("(effect state:get \"x\")", "State get"),
    ("(effect time:now)", "Time effect"),
    ("(effect random:random)", "Random effect"),
    ("(let ((x (effect state:get \"x\"))) (+ x 8))", "Using effect result"),
]

print("Testing Effect System in REPL\n")

for code, desc in test_cases:
    print(f"Test: {desc}")
    print(f"Code: {code}")
    try:
        result = env.evaluate(code)
        print(f"Result: {result}")
        print("✓ PASS\n")
    except Exception as e:
        print(f"✗ FAIL: {e}\n")

print("Effect tests completed!")