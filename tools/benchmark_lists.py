#!/usr/bin/env python3
"""
Benchmark comparing cons-based lists vs native Python lists
"""

import time
from src.parser import parse
from src.parser.optimized_parser import optimized_parse
from src.interpreter import Interpreter
from src.vm import BytecodeCompiler, VM
from src.stdlib import core, strings


def benchmark_list_operations():
    """Benchmark various list operations"""
    interpreter = Interpreter()
    compiler = BytecodeCompiler()
    vm = VM()
    
    print("List Operation Performance Comparison")
    print("=" * 80)
    print(f"{'Operation':<30} {'Cons-based (μs)':<20} {'Native list (μs)':<20} {'Speedup':<10}")
    print("-" * 80)
    
    test_cases = [
        # List creation
        ("Create [1 2 3 4 5]", "[1 2 3 4 5]"),
        
        # Length operation
        ("Length of 10 elements", "(length [1 2 3 4 5 6 7 8 9 10])"),
        ("Length of 50 elements", "(length [" + " ".join(str(i) for i in range(50)) + "])"),
        
        # List manipulation
        ("Cons to list", "(cons 0 [1 2 3 4 5])"),
        ("Head of list", "(head [1 2 3 4 5])"),
        ("Tail of list", "(tail [1 2 3 4 5])"),
        
        # Nested operations
        ("Nested length", "(length (cons 0 (cons 1 [2 3 4 5])))"),
        ("Multiple cons", "(cons 1 (cons 2 (cons 3 [])))"),
    ]
    
    for name, code in test_cases:
        # Warm up
        for _ in range(10):
            parse(code)
            optimized_parse(code)
        
        # Benchmark cons-based (original)
        cons_times = []
        for _ in range(100):
            start = time.perf_counter()
            graph = parse(code)
            bytecode = compiler.compile(graph)
            result = vm.execute(bytecode)
            cons_times.append(time.perf_counter() - start)
        avg_cons = sum(cons_times) / len(cons_times) * 1e6
        
        # Benchmark native list (optimized)
        native_times = []
        for _ in range(100):
            start = time.perf_counter()
            graph = optimized_parse(code)
            bytecode = compiler.compile(graph)
            result = vm.execute(bytecode)
            native_times.append(time.perf_counter() - start)
        avg_native = sum(native_times) / len(native_times) * 1e6
        
        speedup = avg_cons / avg_native
        print(f"{name:<30} {avg_cons:<20.2f} {avg_native:<20.2f} {speedup:<10.1f}x")
    
    print("\n" + "=" * 80)
    
    # Scaling analysis
    print("\nList Length Scaling Analysis:")
    print("-" * 60)
    print(f"{'Size':<10} {'Cons (μs)':<15} {'Native (μs)':<15} {'Speedup':<10}")
    print("-" * 60)
    
    for size in [10, 25, 50, 100, 200]:
        # Create list with 'size' elements
        list_literal = "[" + " ".join(str(i) for i in range(size)) + "]"
        code = f"(length {list_literal})"
        
        # Cons-based
        start = time.perf_counter()
        graph = parse(code)
        bytecode = compiler.compile(graph)
        result = vm.execute(bytecode)
        cons_time = (time.perf_counter() - start) * 1e6
        
        # Native
        start = time.perf_counter()
        graph = optimized_parse(code)
        bytecode = compiler.compile(graph)
        result = vm.execute(bytecode)
        native_time = (time.perf_counter() - start) * 1e6
        
        speedup = cons_time / native_time
        print(f"{size:<10} {cons_time:<15.2f} {native_time:<15.2f} {speedup:<10.1f}x")
    
    # Memory usage comparison
    print("\n" + "=" * 80)
    print("\nMemory Usage Analysis:")
    print("-" * 60)
    
    import sys
    
    # Cons-based list
    cons_graph = parse("[1 2 3 4 5 6 7 8 9 10]")
    cons_nodes = len(cons_graph.nodes)
    cons_size = sum(sys.getsizeof(node) for node in cons_graph.nodes.values())
    
    # Native list
    native_graph = optimized_parse("[1 2 3 4 5 6 7 8 9 10]")
    native_nodes = len(native_graph.nodes)
    native_size = sum(sys.getsizeof(node) for node in native_graph.nodes.values())
    
    print(f"List of 10 elements:")
    print(f"  Cons-based:  {cons_nodes} nodes, ~{cons_size/1024:.1f} KB")
    print(f"  Native list: {native_nodes} nodes, ~{native_size/1024:.1f} KB")
    print(f"  Memory reduction: {(1 - native_size/cons_size)*100:.1f}%")
    
    # Show AST difference
    print("\nAST Comparison for [1 2 3]:")
    print("-" * 40)
    
    print("\nCons-based AST:")
    cons_ast = parse("[1 2 3]")
    for i, (node_id, node) in enumerate(cons_ast.nodes.items()):
        if i < 10:  # Show first 10 nodes
            print(f"  {node.__class__.__name__}: {getattr(node, 'value', getattr(node, 'name', '...'))}") 
    if len(cons_ast.nodes) > 10:
        print(f"  ... ({len(cons_ast.nodes) - 10} more nodes)")
    
    print("\nNative list AST:")
    native_ast = optimized_parse("[1 2 3]")
    for i, (node_id, node) in enumerate(native_ast.nodes.items()):
        print(f"  {node.__class__.__name__}: {getattr(node, 'value', getattr(node, 'name', '...'))}")


def benchmark_real_world():
    """Benchmark real-world list operations"""
    print("\n" + "=" * 80)
    print("\nReal-World List Operations:")
    print("-" * 60)
    
    compiler = BytecodeCompiler()
    vm = VM()
    
    # Map operation
    map_code = """
    (let ((map (lambda (f lst)
                 (if (empty? lst)
                     []
                     (cons (f (head lst))
                           (map f (tail lst))))))
          (double (lambda (x) (* x 2))))
      (map double [1 2 3 4 5 6 7 8 9 10]))
    """
    
    print("Map double over 10 elements:")
    
    # Cons-based
    start = time.perf_counter()
    graph = parse(map_code)
    bytecode = compiler.compile(graph)
    result = vm.execute(bytecode)
    cons_time = (time.perf_counter() - start) * 1e3  # ms
    
    # Native (though map still uses cons internally)
    start = time.perf_counter()
    graph = optimized_parse(map_code)
    bytecode = compiler.compile(graph)
    result = vm.execute(bytecode)
    native_time = (time.perf_counter() - start) * 1e3  # ms
    
    print(f"  Cons-based:  {cons_time:.2f} ms")
    print(f"  Native list: {native_time:.2f} ms")
    print(f"  Speedup:     {cons_time/native_time:.1f}x")


if __name__ == "__main__":
    benchmark_list_operations()
    benchmark_real_world()