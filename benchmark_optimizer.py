#!/usr/bin/env python3
"""
Benchmark demonstrating graph optimizer improvements
"""

import time
from src.parser import parse
from src.optimizer import GraphOptimizer
from src.vm import BytecodeCompiler, VM
from src.interpreter import Interpreter
from src.stdlib import core


def show_optimization_example(code: str, description: str):
    """Show before/after optimization"""
    print(f"\n{description}")
    print("-" * 60)
    print(f"Code: {code}")
    
    # Parse
    graph = parse(code)
    print(f"\nBefore optimization: {len(graph.nodes)} nodes")
    
    # Optimize
    optimizer = GraphOptimizer()
    optimized = optimizer.optimize(graph)
    
    print(f"After optimization:  {len(optimized.nodes)} nodes")
    print(f"\nOptimizations applied:")
    print(f"  Constant folded: {optimizer.stats.constant_folded}")
    print(f"  Dead code eliminated: {optimizer.stats.dead_code_eliminated}")
    print(f"  Pure expressions evaluated: {optimizer.stats.pure_expressions_evaluated}")
    
    # Show performance impact
    compiler = BytecodeCompiler()
    vm = VM()
    
    # Time original
    orig_times = []
    for _ in range(100):
        bytecode = compiler.compile(graph)
        start = time.perf_counter()
        result = vm.execute(bytecode)
        orig_times.append(time.perf_counter() - start)
    
    # Time optimized
    opt_times = []
    for _ in range(100):
        bytecode = compiler.compile(optimized)
        start = time.perf_counter()
        result_opt = vm.execute(bytecode)
        opt_times.append(time.perf_counter() - start)
    
    avg_orig = sum(orig_times) / len(orig_times) * 1e6
    avg_opt = sum(opt_times) / len(opt_times) * 1e6
    
    print(f"\nExecution time:")
    print(f"  Original:  {avg_orig:.2f} μs")
    print(f"  Optimized: {avg_opt:.2f} μs")
    print(f"  Speedup:   {avg_orig/avg_opt:.1f}x")
    
    # Verify results match
    if hasattr(result, 'data'):
        result = result.data
    print(f"\nResult: {result} {'✓' if result == result_opt else '✗'}")


def benchmark_optimizer():
    """Run optimizer benchmarks"""
    print("ClaudeLang Graph Optimizer Demonstration")
    print("=" * 60)
    
    # Example 1: Constant folding
    show_optimization_example(
        "(+ 2 3)",
        "Example 1: Simple constant folding"
    )
    
    # Example 2: Complex constant expression
    show_optimization_example(
        "(* (+ 1 2) (- 10 5))",
        "Example 2: Complex constant expression"
    )
    
    # Example 3: Conditional with constant condition
    show_optimization_example(
        "(if (> 5 3) (* 2 10) (+ 100 200))",
        "Example 3: Conditional with constant condition"
    )
    
    # Example 4: Dead code elimination
    show_optimization_example(
        "(let ((x 10) (y 20) (z 30)) (+ x y))",
        "Example 4: Unused variable (dead code)"
    )
    
    # Example 5: Nested constant folding
    show_optimization_example(
        "(+ (* 2 3) (* 4 5) (* 6 7))",
        "Example 5: Multiple constant operations"
    )
    
    # Example 6: List operations
    show_optimization_example(
        "(length [1 2 3 4 5])",
        "Example 6: Constant list length"
    )
    
    # Example 7: String operations
    show_optimization_example(
        '(string-upcase "hello")',
        "Example 7: Constant string operation"
    )
    
    # Example 8: Mixed with variables
    show_optimization_example(
        "(let ((x 10)) (+ x (* 3 4)))",
        "Example 8: Partial constant folding"
    )
    
    # Performance comparison on larger expressions
    print("\n\n" + "=" * 60)
    print("Performance Impact on Larger Expressions")
    print("=" * 60)
    
    test_cases = [
        ("Arithmetic tree", "(+ (+ (+ 1 2) (+ 3 4)) (+ (+ 5 6) (+ 7 8)))"),
        ("Nested conditionals", "(if (> 10 5) (if (< 3 7) 100 200) 300)"),
        ("List construction", "(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 [])))))"),
        ("Complex calculation", "(* (+ 2 3) (+ 4 5) (- 10 2))"),
    ]
    
    print(f"\n{'Expression':<25} {'Nodes':<15} {'Speedup':<10}")
    print("-" * 50)
    
    for name, code in test_cases:
        graph = parse(code)
        optimizer = GraphOptimizer()
        optimized = optimizer.optimize(graph)
        
        compiler = BytecodeCompiler()
        vm = VM()
        
        # Time execution
        orig_bytecode = compiler.compile(graph)
        opt_bytecode = compiler.compile(optimized)
        
        orig_times = []
        opt_times = []
        
        for _ in range(100):
            start = time.perf_counter()
            vm.execute(orig_bytecode)
            orig_times.append(time.perf_counter() - start)
            
            start = time.perf_counter()
            vm.execute(opt_bytecode)
            opt_times.append(time.perf_counter() - start)
        
        avg_orig = sum(orig_times) / len(orig_times)
        avg_opt = sum(opt_times) / len(opt_times)
        speedup = avg_orig / avg_opt
        
        nodes_str = f"{len(graph.nodes)}→{len(optimized.nodes)}"
        print(f"{name:<25} {nodes_str:<15} {speedup:<10.1f}x")
    
    # Show bytecode difference
    print("\n\nBytecode Comparison for '(+ 2 3)':")
    print("-" * 60)
    
    graph = parse("(+ 2 3)")
    optimizer = GraphOptimizer()
    optimized = optimizer.optimize(graph)
    
    compiler = BytecodeCompiler()
    
    print("Original bytecode:")
    orig_bytecode = compiler.compile(graph)
    print(orig_bytecode.disassemble())
    
    print("\nOptimized bytecode:")
    opt_bytecode = compiler.compile(optimized)
    print(opt_bytecode.disassemble())


if __name__ == "__main__":
    benchmark_optimizer()