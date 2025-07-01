#!/usr/bin/env python3
"""
Detailed performance analysis of ClaudeLang
"""

import time
import matplotlib.pyplot as plt
import numpy as np
from src.parser import parse
from src.interpreter import Interpreter
from src.stdlib import core, strings, io


class PerformanceAnalyzer:
    def __init__(self):
        self.interpreter = Interpreter()
    
    def measure_scaling(self):
        """Measure how performance scales with input size"""
        print("=== SCALING ANALYSIS ===\n")
        
        # List operations scaling
        sizes = [1, 5, 10, 20, 50, 100]
        cl_times = []
        py_times = []
        
        print("List length scaling:")
        for size in sizes:
            # Generate list literal
            list_literal = "[" + " ".join(str(i) for i in range(size)) + "]"
            cl_code = f"(length {list_literal})"
            
            # Measure ClaudeLang
            start = time.perf_counter()
            graph = parse(cl_code)
            result = self.interpreter.interpret(graph)
            cl_time = time.perf_counter() - start
            cl_times.append(cl_time * 1000)  # Convert to ms
            
            # Measure Python
            test_list = list(range(size))
            start = time.perf_counter()
            len(test_list)
            py_time = time.perf_counter() - start
            py_times.append(py_time * 1000)  # Convert to ms
            
            print(f"  Size {size:3d}: CL={cl_time*1000:.3f}ms, Py={py_time*1000:.6f}ms")
        
        # Plot scaling
        plt.figure(figsize=(10, 6))
        plt.subplot(1, 2, 1)
        plt.plot(sizes, cl_times, 'b-o', label='ClaudeLang')
        plt.plot(sizes, py_times, 'r-o', label='Python')
        plt.xlabel('List Size')
        plt.ylabel('Time (ms)')
        plt.title('List Length Operation Scaling')
        plt.legend()
        plt.grid(True)
        
        # Recursion depth scaling
        print("\nRecursion depth scaling (factorial):")
        depths = [1, 5, 10, 15, 20]
        cl_fact_times = []
        py_fact_times = []
        
        for n in depths:
            # ClaudeLang factorial
            cl_code = f"""
            (let ((fact (lambda (n)
                          (if (<= n 1)
                              1
                              (* n (fact (- n 1)))))))
              (fact {n}))
            """
            
            start = time.perf_counter()
            graph = parse(cl_code)
            result = self.interpreter.interpret(graph)
            cl_time = time.perf_counter() - start
            cl_fact_times.append(cl_time * 1000)
            
            # Python factorial
            def factorial(n):
                return 1 if n <= 1 else n * factorial(n - 1)
            
            start = time.perf_counter()
            factorial(n)
            py_time = time.perf_counter() - start
            py_fact_times.append(py_time * 1000)
            
            print(f"  N={n:2d}: CL={cl_time*1000:.3f}ms, Py={py_time*1000:.6f}ms")
        
        plt.subplot(1, 2, 2)
        plt.plot(depths, cl_fact_times, 'b-o', label='ClaudeLang')
        plt.plot(depths, py_fact_times, 'r-o', label='Python')
        plt.xlabel('N (factorial)')
        plt.ylabel('Time (ms)')
        plt.title('Factorial Recursion Scaling')
        plt.legend()
        plt.grid(True)
        
        plt.tight_layout()
        plt.savefig('scaling_analysis.png', dpi=150)
        print("\nScaling plots saved to scaling_analysis.png")
    
    def analyze_overhead(self):
        """Analyze where the overhead comes from"""
        print("\n=== OVERHEAD ANALYSIS ===\n")
        
        # Measure different phases
        code = "(+ 1 2)"
        
        # Phase 1: Parsing
        start = time.perf_counter()
        graph = parse(code)
        parse_time = time.perf_counter() - start
        
        # Phase 2: Interpretation
        start = time.perf_counter()
        result = self.interpreter.interpret(graph)
        interpret_time = time.perf_counter() - start
        
        # Phase 3: Pure Python equivalent
        start = time.perf_counter()
        py_result = 1 + 2
        py_time = time.perf_counter() - start
        
        print(f"Simple addition (1 + 2):")
        print(f"  Parsing:        {parse_time*1e6:.2f} μs ({parse_time/interpret_time*100:.1f}% of interpretation)")
        print(f"  Interpretation: {interpret_time*1e6:.2f} μs")
        print(f"  Total CL:       {(parse_time + interpret_time)*1e6:.2f} μs")
        print(f"  Pure Python:    {py_time*1e6:.2f} μs")
        print(f"  Overhead:       {(parse_time + interpret_time)/py_time:.1f}x")
        
        # Analyze AST node creation overhead
        print("\nAST complexity analysis:")
        test_cases = [
            ("(+ 1 2)", "Simple addition"),
            ("(+ (* 2 3) 4)", "Nested expression"),
            ("(let ((x 5)) x)", "Variable binding"),
            ("(lambda (x) x)", "Function creation"),
            ("[1 2 3 4 5]", "List literal"),
        ]
        
        for code, desc in test_cases:
            graph = parse(code)
            node_count = len(graph.nodes)
            print(f"  {desc:<20}: {node_count:3d} nodes")
    
    def compare_implementations(self):
        """Compare different implementation strategies"""
        print("\n=== IMPLEMENTATION COMPARISON ===\n")
        
        # Compare list building strategies
        print("List construction methods:")
        
        # Method 1: Using cons (current)
        cl_cons = "[1 2 3 4 5]"
        start = time.perf_counter()
        graph = parse(cl_cons)
        result = self.interpreter.interpret(graph)
        cons_time = time.perf_counter() - start
        
        # Method 2: Direct list literal (if we had it)
        # This would require modifying the interpreter
        
        # Python equivalent
        start = time.perf_counter()
        py_list = [1, 2, 3, 4, 5]
        py_time = time.perf_counter() - start
        
        print(f"  Cons-based construction: {cons_time*1e6:.2f} μs")
        print(f"  Python list literal:     {py_time*1e6:.2f} μs")
        print(f"  Overhead:                {cons_time/py_time:.1f}x")
        
        # Graph analysis
        print(f"\nGraph complexity for [1 2 3 4 5]:")
        print(f"  Total nodes: {len(graph.nodes)}")
        print(f"  Node types:")
        node_types = {}
        for node in graph.nodes.values():
            node_type = node.__class__.__name__
            node_types[node_type] = node_types.get(node_type, 0) + 1
        for node_type, count in sorted(node_types.items()):
            print(f"    {node_type}: {count}")
    
    def suggest_optimizations(self):
        """Suggest optimizations based on analysis"""
        print("\n=== OPTIMIZATION RECOMMENDATIONS ===\n")
        
        print("1. **Bytecode Compilation** (~10-50x speedup)")
        print("   - Eliminate parsing overhead for repeated execution")
        print("   - Reduce AST traversal overhead")
        print("   - Enable instruction-level optimizations")
        
        print("\n2. **Native Implementation** (~100-1000x speedup)")
        print("   - Implement interpreter in Rust or C++")
        print("   - Use efficient data structures")
        print("   - Eliminate Python function call overhead")
        
        print("\n3. **JIT Compilation** (~50-500x speedup)")
        print("   - Compile hot paths to native code")
        print("   - Inline primitive operations")
        print("   - Optimize based on runtime types")
        
        print("\n4. **AST Optimizations**")
        print("   - Constant folding at parse time")
        print("   - Common subexpression elimination")
        print("   - Tail call optimization")
        
        print("\n5. **Built-in Data Structures**")
        print("   - Native list implementation instead of cons cells")
        print("   - Optimized string operations")
        print("   - Cached function closures")
        
        # Estimate potential performance
        current_slowdown = 800  # Average from benchmarks
        print(f"\nPerformance projections:")
        print(f"  Current (tree-walker):     {current_slowdown}x slower than Python")
        print(f"  With bytecode VM:          ~{current_slowdown/20}x slower")
        print(f"  With native interpreter:   ~{current_slowdown/200}x slower")
        print(f"  With JIT compilation:      ~{current_slowdown/400}x slower")
        print(f"  Theoretical optimum:       ~1-2x slower")


def main():
    analyzer = PerformanceAnalyzer()
    
    print("ClaudeLang Performance Analysis")
    print("===============================\n")
    
    analyzer.measure_scaling()
    analyzer.analyze_overhead()
    analyzer.compare_implementations()
    analyzer.suggest_optimizations()
    
    print("\n" + "="*50)
    print("CONCLUSION")
    print("="*50)
    print("""
The current ClaudeLang implementation is ~500-1500x slower than Python
due to:

1. Tree-walking interpretation (vs Python's bytecode VM)
2. Python implementation (vs Python's C implementation)
3. Unoptimized data structures (cons lists vs arrays)
4. AST overhead (many nodes for simple operations)
5. Dynamic dispatch for every operation

However, the design allows for significant optimization opportunities
while maintaining the core benefits of explicit effects, graph-based
representation, and AI-friendly structure.
""")


if __name__ == "__main__":
    main()