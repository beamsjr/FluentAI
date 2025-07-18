#!/usr/bin/env python3
"""
FluentAI runner - Execute FluentAI files

Usage:
    python run_fluentai.py <filename> [options]
    
Options:
    --verbose    Show optimization statistics
    --bytecode   Display generated bytecode
    --ast        Display AST structure
    --optimize   Run with full optimization (default)
    --no-optimize Run without optimization
"""

import sys
import time
import argparse
from pathlib import Path

from src.parser import parse
from src.parser.optimized_parser import optimized_parse
from src.interpreter import Interpreter
from src.vm import BytecodeCompiler, VM
from src.optimizer.advanced_optimizer import AdvancedGraphOptimizer
from src.stdlib import core


def run_file(filename: str, args):
    """Run a FluentAI file with given options"""
    # Read file
    try:
        with open(filename, 'r') as f:
            source = f.read()
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found")
        return 1
    except Exception as e:
        print(f"Error reading file: {e}")
        return 1
    
    print(f"Running: {filename}")
    if args.verbose:
        print("=" * 60)
    
    try:
        # Parse
        start_time = time.perf_counter()
        if args.optimize:
            graph = optimized_parse(source)  # Use native lists
        else:
            graph = parse(source)
        parse_time = time.perf_counter() - start_time
        
        if args.verbose:
            print(f"Parsing took: {parse_time*1000:.2f} ms")
            print(f"AST nodes: {len(graph.nodes)}")
        
        if args.ast:
            print("\nAST Structure:")
            print("-" * 40)
            for node_id, node in graph.nodes.items():
                print(f"{node_id[:8]}: {node.__class__.__name__}")
        
        # Optimize
        if args.optimize:
            optimizer = AdvancedGraphOptimizer()
            start_time = time.perf_counter()
            optimized = optimizer.optimize(graph)
            opt_time = time.perf_counter() - start_time
            
            if args.verbose:
                print(f"\nOptimization took: {opt_time*1000:.2f} ms")
                print(f"Optimized nodes: {len(optimized.nodes)}")
                print(f"Node reduction: {(1 - len(optimized.nodes)/len(graph.nodes))*100:.1f}%")
                print(f"Optimization stats: {optimizer.stats}")
            
            graph = optimized
        
        # Compile
        compiler = BytecodeCompiler()
        start_time = time.perf_counter()
        bytecode = compiler.compile(graph)
        compile_time = time.perf_counter() - start_time
        
        if args.verbose:
            print(f"\nCompilation took: {compile_time*1000:.2f} ms")
            print(f"Bytecode instructions: {len(bytecode.instructions)}")
        
        if args.bytecode:
            print("\nGenerated Bytecode:")
            print("-" * 40)
            print(bytecode.disassemble())
        
        # Execute
        vm = VM()
        start_time = time.perf_counter()
        result = vm.execute(bytecode)
        exec_time = time.perf_counter() - start_time
        
        if args.verbose:
            print(f"\nExecution took: {exec_time*1000:.2f} ms")
            print(f"Total time: {(parse_time + opt_time + compile_time + exec_time)*1000:.2f} ms")
            print("=" * 60)
        
        # Print result
        print(f"\nResult: {result}")
        
        return 0
        
    except Exception as e:
        print(f"\nError: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="Run FluentAI programs",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python run_fluentai.py examples/fibonacci.ai
  python run_fluentai.py examples/optimization_demo.ai --verbose
  python run_fluentai.py test.ai --bytecode --no-optimize
        """
    )
    
    parser.add_argument('filename', help='FluentAI source file to run')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Show detailed execution information')
    parser.add_argument('--bytecode', '-b', action='store_true',
                       help='Display generated bytecode')
    parser.add_argument('--ast', '-a', action='store_true',
                       help='Display AST structure')
    parser.add_argument('--optimize', dest='optimize', action='store_true',
                       default=True, help='Run with optimization (default)')
    parser.add_argument('--no-optimize', dest='optimize', action='store_false',
                       help='Run without optimization')
    
    args = parser.parse_args()
    
    # Run the file
    exit_code = run_file(args.filename, args)
    sys.exit(exit_code)


if __name__ == "__main__":
    main()