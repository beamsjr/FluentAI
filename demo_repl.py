#!/usr/bin/env python3
"""
Interactive demo of the ClaudeLang REPL

This demonstrates the REPL's features including:
- Basic arithmetic and operations
- Function definitions and applications
- List operations
- Effect handling
- Optimization toggles
"""

import subprocess
import sys

def run_repl_command(cmd):
    """Run a command in the REPL and return output"""
    proc = subprocess.Popen(
        [sys.executable, '-m', 'src.repl'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    output, error = proc.communicate(input=cmd + '\n:quit\n')
    return output, error

# Demo script
demo_commands = [
    ("Basic arithmetic", "(+ 10 20)"),
    ("Lists", "[1 2 3 4 5]"),
    ("Function definition", "(let ((square (lambda (x) (* x x)))) (square 7))"),
    ("Conditionals", "(if (> 10 5) \"bigger\" \"smaller\")"),
    ("Nested expressions", "(+ (* 3 4) (- 10 5))"),
]

print("ClaudeLang REPL Demo")
print("=" * 50)
print()

for desc, cmd in demo_commands:
    print(f"Demo: {desc}")
    print(f">>> {cmd}")
    output, error = run_repl_command(cmd)
    
    # Extract the result from output
    lines = output.strip().split('\n')
    for i, line in enumerate(lines):
        if line.startswith('claudelang>') and i + 1 < len(lines):
            result = lines[i + 1]
            if not result.startswith('claudelang>') and not result.startswith('Type :'):
                print(f"<<< {result}")
                break
    print()

print("=" * 50)
print("To run the REPL interactively:")
print("  python3 -m src.repl")
print()
print("REPL Commands:")
print("  :help     - Show help")
print("  :quit     - Exit REPL")
print("  :vm off   - Use interpreter (supports lambdas)")
print("  :ast on   - Show AST structure")
print("  :load <file> - Load and run a file")