#!/usr/bin/env python3
"""
Test the advanced optimizer
"""

from src.parser import parse
from src.optimizer.advanced_optimizer import AdvancedGraphOptimizer
from src.vm import BytecodeCompiler, VM


def test_advanced_optimization():
    """Test various optimization scenarios"""
    test_cases = [
        ("Simple arithmetic", "(+ 2 3)"),
        ("Complex arithmetic", "(* (+ 1 2) (- 10 5))"),
        ("Conditional elimination", "(if (> 5 3) 100 200)"),
        ("Nested conditionals", "(if (< 1 2) (if (> 10 5) 42 0) -1)"),
        ("Let with constants", "(let ((x 10) (y 20)) (+ x y))"),
        ("Mixed expression", "(let ((x 5)) (+ x (* 3 4)))"),
        ("List operations", "(length (cons 1 (cons 2 [3 4 5])))"),
        ("String operations", '(concat "Hello" (concat ", " "World!"))'),
        ("Boolean logic", "(and (> 5 3) (< 2 10))"),
        ("Conditional with side effects", "(if #t 42 (print \"never\"))"),
    ]
    
    compiler = BytecodeCompiler()
    vm = VM()
    
    print("Advanced Optimizer Test Results")
    print("=" * 80)
    print(f"{'Expression':<40} {'Before':<10} {'After':<10} {'Result':<15}")
    print("-" * 80)
    
    for name, code in test_cases:
        # Parse and optimize
        graph = parse(code)
        optimizer = AdvancedGraphOptimizer()
        optimized = optimizer.optimize(graph)
        
        # Execute optimized code
        bytecode = compiler.compile(optimized)
        result = vm.execute(bytecode)
        
        # Format result
        if hasattr(result, 'data'):
            result = result.data
        
        print(f"{name:<40} {len(graph.nodes):<10} {len(optimized.nodes):<10} {str(result):<15}")
        
        # Show stats for interesting cases
        if optimizer.stats.branches_eliminated > 0 or optimizer.stats.pure_expressions_evaluated > 2:
            print(f"  → Branches eliminated: {optimizer.stats.branches_eliminated}, "
                  f"Pure expressions: {optimizer.stats.pure_expressions_evaluated}")
    
    # Show bytecode comparison for a key example
    print("\n" + "=" * 80)
    print("\nBytecode Comparison: (if (> 5 3) 100 200)")
    print("-" * 60)
    
    # Original
    graph = parse("(if (> 5 3) 100 200)")
    orig_bytecode = compiler.compile(graph)
    print("Original bytecode:")
    print(orig_bytecode.disassemble())
    
    # Optimized
    optimizer = AdvancedGraphOptimizer()
    optimized = optimizer.optimize(graph)
    opt_bytecode = compiler.compile(optimized)
    print("\nOptimized bytecode:")
    print(opt_bytecode.disassemble())
    
    print(f"\nOptimization stats: {optimizer.stats}")


if __name__ == "__main__":
    test_advanced_optimization()