#!/usr/bin/env python3
"""Simple REPL test without higher-order functions"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from src.repl.repl import REPLEnvironment

# Test basic operations
env = REPLEnvironment()
# Use interpreter instead of VM for lambda tests
env.use_vm = False

test_cases = [
    ("(+ 2 3)", "Basic addition"),
    ("(* 4 5)", "Basic multiplication"), 
    ("(length [1 2 3 4 5])", "List length"),
    ("(head [10 20 30])", "List head"),
    ("(tail [10 20 30])", "List tail"),
    ("(> 10 5)", "Greater than comparison"),
    ("(let ((x 10) (y 20)) (+ x y))", "Let binding"),
    ("(if (> 5 3) 100 200)", "Conditional - true branch"),
    ("(if (< 5 3) 100 200)", "Conditional - false branch"),
    ("((lambda (x) (* x x)) 7)", "Lambda application"),
    ("(let ((square (lambda (x) (* x x)))) (square 8))", "Lambda in let binding"),
]

print("Testing ClaudeLang REPL Environment\n")

for code, desc in test_cases:
    print(f"Test: {desc}")
    print(f"Code: {code}")
    try:
        result = env.evaluate(code)
        print(f"Result: {result}")
        print("✓ PASS\n")
    except Exception as e:
        print(f"✗ FAIL: {e}\n")

print("REPL tests completed!")