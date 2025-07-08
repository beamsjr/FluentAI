#!/usr/bin/env python3
"""
Simple example runner for FluentAI
"""

import sys
from src.parser import parse
from src.interpreter import Interpreter
from src.stdlib import core, strings  # Import stdlib modules to register functions

def run_example():
    # Example 1: Simple arithmetic
    print("Example 1: Simple arithmetic")
    code1 = "(+ 2 3)"
    graph1 = parse(code1)
    interpreter = Interpreter()
    result1 = interpreter.interpret(graph1)
    print(f"Code: {code1}")
    print(f"Result: {result1.data}")
    print()
    
    # Example 2: Let binding
    print("Example 2: Let binding")
    code2 = "(let ((x 10) (y 20)) (+ x y))"
    graph2 = parse(code2)
    result2 = interpreter.interpret(graph2)
    print(f"Code: {code2}")
    print(f"Result: {result2.data}")
    print()
    
    # Example 3: Lambda function
    print("Example 3: Lambda function")
    code3 = "(let ((inc (lambda (x) (+ x 1)))) (inc 5))"
    graph3 = parse(code3)
    result3 = interpreter.interpret(graph3)
    print(f"Code: {code3}")
    print(f"Result: {result3.data}")
    print()
    
    # Example 4: Conditional
    print("Example 4: Conditional")
    code4 = "(if (< 5 10) \"less\" \"greater\")"
    graph4 = parse(code4)
    result4 = interpreter.interpret(graph4)
    print(f"Code: {code4}")
    print(f"Result: {result4.data}")
    print()
    
    # Example 5: Effects (IO)
    print("Example 5: Effects")
    code5 = "(let ((msg \"Hello from FluentAI!\")) (do msg))"
    graph5 = parse(code5)
    print(f"Code: {code5}")
    result5 = interpreter.interpret(graph5)
    print(f"Result: {result5.data}")
    print()

if __name__ == "__main__":
    run_example()