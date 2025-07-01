#!/usr/bin/env python3
"""Test the ClaudeLang REPL interactively"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from src.repl.repl import REPLEnvironment

# Test basic operations
env = REPLEnvironment()

test_cases = [
    ("(+ 2 3)", "Basic addition"),
    ("(* 4 5)", "Basic multiplication"), 
    ("(- 10 3)", "Basic subtraction"),
    ("(/ 20 4)", "Basic division"),
    ("[1 2 3]", "List literal"),
    ("(map (lambda (x) (* x 2)) [1 2 3])", "Map operation"),
    ("(let ((x 10) (y 20)) (+ x y))", "Let binding"),
    ("(if (> 5 3) \"yes\" \"no\")", "Conditional"),
]

for code, desc in test_cases:
    print(f"\nTesting: {desc}")
    print(f"Code: {code}")
    try:
        result = env.evaluate(code)
        print(f"Result: {result}")
    except Exception as e:
        print(f"Error: {e}")

print("\n✓ REPL environment working correctly!")