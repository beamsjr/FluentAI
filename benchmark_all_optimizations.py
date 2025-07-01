#!/usr/bin/env python3
"""
Comprehensive benchmark showing all optimization improvements
"""

import time
import statistics
from src.parser import parse
from src.parser.optimized_parser import optimized_parse
from src.optimizer import GraphOptimizer
from src.optimizer.advanced_optimizer import AdvancedGraphOptimizer
from src.vm import BytecodeCompiler, VM
from src.interpreter import Interpreter
from src.core.cache import cached_parse
from src.stdlib import core, strings


def benchmark_full_stack():
    """Benchmark all optimization layers"""
    print("ClaudeLang Full Stack Performance Analysis")
    print("=" * 80)
    
    # Test programs
    programs = [
        ("Fibonacci(10)", """
            (let ((fib (lambda (n)
                        (if (<= n 1)
                            n
                            (+ (fib (- n 1))
                               (fib (- n 2)))))))
              (fib 10))
        """),
        
        ("List sum", """
            (let ((sum (lambda (lst)
                        (if (empty? lst)
                            0
                            (+ (head lst) (sum (tail lst))))))
                  (numbers [1 2 3 4 5 6 7 8 9 10]))
              (sum numbers))
        """),
        
        ("Factorial(8)", """
            (let ((fact (lambda (n)
                         (if (== n 0)
                             1
                             (* n (fact (- n 1)))))))
              (fact 8))
        """),
        
        ("Constant folding", """
            (+ (* 2 3) (* 4 5) (* 6 7))
        """),
        
        ("Complex conditional", """
            (if (and (> 10 5) (< 3 7))
                (+ 100 (* 2 50))
                (- 1000 500))
        """),
    ]
    
    # Configuration for each optimization level
    configs = [
        ("Baseline (Interpreter)", lambda code: run_interpreter(code)),
        ("+ Bytecode VM", lambda code: run_vm(code)),
        ("+ AST Cache", lambda code: run_vm_cached(code)),
        ("+ Native Lists", lambda code: run_vm_native_lists(code)),
        ("+ Graph Optimizer", lambda code: run_vm_optimized(code)),
        ("+ Advanced Optimizer", lambda code: run_vm_advanced(code)),
    ]
    
    results = {}
    
    for prog_name, prog_code in programs:
        print(f"\n{prog_name}:")
        print("-" * 60)
        
        prog_results = {}
        baseline_time = None
        
        for config_name, runner in configs:
            times = []
            
            # Warm up
            try:
                for _ in range(3):
                    runner(prog_code)
            except:
                pass
            
            # Benchmark
            for _ in range(20):
                try:
                    start = time.perf_counter()
                    result = runner(prog_code)
                    elapsed = time.perf_counter() - start
                    times.append(elapsed)
                except Exception as e:
                    print(f"  {config_name:<25} ERROR: {e}")
                    break
            
            if times:
                avg_time = statistics.mean(times) * 1000  # Convert to ms
                if baseline_time is None:
                    baseline_time = avg_time
                    speedup = 1.0
                else:
                    speedup = baseline_time / avg_time
                
                prog_results[config_name] = (avg_time, speedup)
                print(f"  {config_name:<25} {avg_time:>8.2f} ms  {speedup:>6.1f}x")
        
        results[prog_name] = prog_results
    
    # Summary
    print("\n" + "=" * 80)
    print("\nOverall Performance Summary:")
    print("-" * 60)
    print("Cumulative speedup from baseline interpreter:")
    
    total_speedups = {}
    for prog_name, prog_results in results.items():
        for config_name, (time, speedup) in prog_results.items():
            if config_name not in total_speedups:
                total_speedups[config_name] = []
            total_speedups[config_name].append(speedup)
    
    for config_name, speedups in total_speedups.items():
        avg_speedup = statistics.mean(speedups)
        print(f"  {config_name:<30} {avg_speedup:>6.1f}x average")


def run_interpreter(code):
    """Run with basic interpreter"""
    from src.interpreter import Interpreter
    interp = Interpreter()
    graph = parse(code)
    return interp.eval(graph)


def run_vm(code):
    """Run with bytecode VM"""
    compiler = BytecodeCompiler()
    vm = VM()
    graph = parse(code)
    bytecode = compiler.compile(graph)
    return vm.execute(bytecode)


def run_vm_cached(code):
    """Run with VM + AST caching"""
    compiler = BytecodeCompiler()
    vm = VM()
    graph = cached_parse(code)  # Use cached parser
    bytecode = compiler.compile(graph)
    return vm.execute(bytecode)


def run_vm_native_lists(code):
    """Run with VM + native lists"""
    compiler = BytecodeCompiler()
    vm = VM()
    graph = optimized_parse(code)  # Use optimized parser with native lists
    bytecode = compiler.compile(graph)
    return vm.execute(bytecode)


def run_vm_optimized(code):
    """Run with VM + basic graph optimizer"""
    compiler = BytecodeCompiler()
    vm = VM()
    optimizer = GraphOptimizer()
    graph = optimized_parse(code)
    optimized = optimizer.optimize(graph)
    bytecode = compiler.compile(optimized)
    return vm.execute(bytecode)


def run_vm_advanced(code):
    """Run with VM + advanced optimizer"""
    compiler = BytecodeCompiler()
    vm = VM()
    optimizer = AdvancedGraphOptimizer()
    graph = optimized_parse(code)
    optimized = optimizer.optimize(graph)
    bytecode = compiler.compile(optimized)
    return vm.execute(bytecode)


if __name__ == "__main__":
    benchmark_full_stack()