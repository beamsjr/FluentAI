#!/usr/bin/env python3
"""
Advanced optimizer benchmark showing more aggressive optimizations
"""

import time
from src.parser import parse
from src.optimizer import GraphOptimizer
from src.vm import BytecodeCompiler, VM
from src.interpreter import Interpreter


def show_ast_comparison(code: str):
    """Show detailed AST before and after optimization"""
    print(f"\nCode: {code}")
    print("-" * 60)
    
    # Parse
    graph = parse(code)
    optimizer = GraphOptimizer()
    optimized = optimizer.optimize(graph)
    
    print(f"Before optimization: {len(graph.nodes)} nodes")
    for node_id, node in graph.nodes.items():
        print(f"  {node_id}: {node.__class__.__name__}", end="")
        if hasattr(node, 'value'):
            print(f" = {node.value}", end="")
        elif hasattr(node, 'name'):
            print(f" '{node.name}'", end="")
        print()
    
    print(f"\nAfter optimization: {len(optimized.nodes)} nodes")
    for node_id, node in optimized.nodes.items():
        print(f"  {node_id}: {node.__class__.__name__}", end="")
        if hasattr(node, 'value'):
            print(f" = {node.value}", end="")
        elif hasattr(node, 'name'):
            print(f" '{node.name}'", end="")
        print()
    
    # Show bytecode difference
    compiler = BytecodeCompiler()
    
    print("\nOriginal bytecode:")
    orig_bytecode = compiler.compile(graph)
    print(orig_bytecode.disassemble())
    
    print("\nOptimized bytecode:")
    opt_bytecode = compiler.compile(optimized)
    print(opt_bytecode.disassemble())


def benchmark_complex_optimizations():
    """Test more complex optimization scenarios"""
    print("ClaudeLang Advanced Optimizer Analysis")
    print("=" * 80)
    
    # Example 1: Show actual optimization happening
    show_ast_comparison("(+ 2 3)")
    
    # Example 2: Nested constant expressions
    show_ast_comparison("(* (+ 1 2) (- 10 5))")
    
    # Example 3: Conditional with constant condition
    show_ast_comparison("(if (> 5 3) 100 200)")
    
    # Example 4: Complex expression mixing constants and variables
    print("\n" + "=" * 80)
    print("\nPerformance on mixed expressions:")
    print("-" * 60)
    
    compiler = BytecodeCompiler()
    vm = VM()
    
    test_cases = [
        # Mix of constant folding opportunities
        ("Partial folding", "(let ((x 10)) (+ x (* 3 (+ 2 2))))"),
        ("Nested arithmetic", "(+ 1 (+ 2 (+ 3 (+ 4 5))))"),
        ("List of constants", "(length (cons 1 (cons 2 (cons 3 []))))"),
        ("String operations", '(concat "Hello" (concat ", " "World!"))'),
    ]
    
    for name, code in test_cases:
        graph = parse(code)
        optimizer = GraphOptimizer()
        optimized = optimizer.optimize(graph)
        
        # Compile both
        orig_bytecode = compiler.compile(graph)
        opt_bytecode = compiler.compile(optimized)
        
        # Measure performance
        orig_times = []
        opt_times = []
        
        for _ in range(100):
            start = time.perf_counter()
            vm.execute(orig_bytecode)
            orig_times.append(time.perf_counter() - start)
            
            start = time.perf_counter()
            vm.execute(opt_bytecode)
            opt_times.append(time.perf_counter() - start)
        
        avg_orig = sum(orig_times) / len(orig_times) * 1e6
        avg_opt = sum(opt_times) / len(opt_times) * 1e6
        speedup = avg_orig / avg_opt
        
        print(f"\n{name}: {code}")
        print(f"  Original:  {len(orig_bytecode.instructions)} instructions, {avg_orig:.2f} μs")
        print(f"  Optimized: {len(opt_bytecode.instructions)} instructions, {avg_opt:.2f} μs")
        print(f"  Speedup:   {speedup:.1f}x")
        print(f"  Stats:     {optimizer.stats}")


def benchmark_optimization_limits():
    """Test the limits of what can be optimized"""
    print("\n" + "=" * 80)
    print("\nOptimization Limits:")
    print("-" * 60)
    
    compiler = BytecodeCompiler()
    vm = VM()
    
    # Create increasingly complex constant expressions
    sizes = [5, 10, 20, 50, 100]
    
    print(f"\n{'Expression Size':<20} {'Orig (μs)':<15} {'Opt (μs)':<15} {'Speedup':<10}")
    print("-" * 60)
    
    for size in sizes:
        # Create nested addition: (+ 1 (+ 2 (+ 3 ... )))
        expr = "1"
        for i in range(2, size + 1):
            expr = f"(+ {i} {expr})"
        
        graph = parse(expr)
        optimizer = GraphOptimizer()
        optimized = optimizer.optimize(graph)
        
        orig_bytecode = compiler.compile(graph)
        opt_bytecode = compiler.compile(optimized)
        
        # Time execution
        orig_times = []
        opt_times = []
        
        for _ in range(50):
            start = time.perf_counter()
            vm.execute(orig_bytecode)
            orig_times.append(time.perf_counter() - start)
            
            start = time.perf_counter()
            vm.execute(opt_bytecode)
            opt_times.append(time.perf_counter() - start)
        
        avg_orig = sum(orig_times) / len(orig_times) * 1e6
        avg_opt = sum(opt_times) / len(opt_times) * 1e6
        speedup = avg_orig / avg_opt
        
        print(f"{f'{size} nested adds':<20} {avg_orig:<15.2f} {avg_opt:<15.2f} {speedup:<10.1f}x")


if __name__ == "__main__":
    benchmark_complex_optimizations()
    benchmark_optimization_limits()