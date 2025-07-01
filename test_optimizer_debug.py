#!/usr/bin/env python3
"""
Debug optimizer issue with conditionals
"""

from src.parser import parse
from src.optimizer.advanced_optimizer import AdvancedGraphOptimizer
from src.vm import BytecodeCompiler, VM
from src.core.primitives import PRIMITIVES
from src.stdlib import core  # This registers the primitives


def debug_conditional_optimization():
    """Debug why conditionals aren't being optimized"""
    
    # Make sure > is registered
    print("Is '>' registered?", PRIMITIVES.get_implementation('>') is not None)
    
    code = "(if (> 5 3) 100 200)"
    print(f"\nParsing: {code}")
    
    graph = parse(code)
    
    # Show the AST structure
    print("\nAST structure:")
    for node_id, node in graph.nodes.items():
        print(f"  {node_id[:8]}: {node.__class__.__name__}", end="")
        if hasattr(node, 'value'):
            print(f" = {node.value}", end="")
        elif hasattr(node, 'name'):
            print(f" '{node.name}'", end="")
        print()
    
    # Now optimize
    print("\nOptimizing...")
    optimizer = AdvancedGraphOptimizer()
    
    # Manually test evaluation of (> 5 3)
    app_node = None
    for node in graph.nodes.values():
        if hasattr(node, 'function_id'):
            app_node = node
            break
    
    if app_node:
        print(f"\nFound application node")
        func_node = graph.get_node(app_node.function_id)
        print(f"Function node: {func_node.__class__.__name__}", end="")
        if hasattr(func_node, 'name'):
            print(f" '{func_node.name}'")
        else:
            print()
        
        # Check if we can get the implementation
        if hasattr(func_node, 'name'):
            impl = PRIMITIVES.get_implementation(func_node.name)
            print(f"Implementation found: {impl is not None}")
            if impl:
                # Try to apply it
                try:
                    result = impl(5, 3)
                    print(f"Result of (> 5 3): {result}")
                except Exception as e:
                    print(f"Error evaluating: {e}")
    
    optimized = optimizer.optimize(graph)
    print(f"\nOptimization stats: {optimizer.stats}")
    
    # Check the optimized result
    compiler = BytecodeCompiler()
    vm = VM()
    bytecode = compiler.compile(optimized)
    result = vm.execute(bytecode)
    print(f"Result: {result}")


if __name__ == "__main__":
    debug_conditional_optimization()