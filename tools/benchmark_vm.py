#!/usr/bin/env python3
"""
Benchmark comparing tree-walking interpreter vs bytecode VM
"""

import time
from src.parser import parse
from src.interpreter import Interpreter
from src.vm import BytecodeCompiler, VM
from src.stdlib import core, strings
from src.core.cache import cached_parse


def benchmark_interpreter(code: str, iterations: int = 1000):
    """Benchmark tree-walking interpreter"""
    interpreter = Interpreter()
    
    # Parse once
    graph = parse(code)
    
    start = time.perf_counter()
    for _ in range(iterations):
        result = interpreter.interpret(graph)
    end = time.perf_counter()
    
    return (end - start) / iterations, result


def benchmark_vm(code: str, iterations: int = 1000):
    """Benchmark bytecode VM"""
    compiler = BytecodeCompiler()
    vm = VM()
    
    # Parse and compile once
    graph = parse(code)
    bytecode = compiler.compile(graph)
    
    start = time.perf_counter()
    for _ in range(iterations):
        result = vm.execute(bytecode)
    end = time.perf_counter()
    
    return (end - start) / iterations, result


def benchmark_vm_with_compilation(code: str, iterations: int = 1000):
    """Benchmark VM including compilation time"""
    compiler = BytecodeCompiler()
    vm = VM()
    
    start = time.perf_counter()
    for _ in range(iterations):
        graph = cached_parse(code)  # Use cache to isolate VM performance
        bytecode = compiler.compile(graph)
        result = vm.execute(bytecode)
    end = time.perf_counter()
    
    return (end - start) / iterations, result


def main():
    print("ClaudeLang VM Performance Benchmark")
    print("=" * 70)
    
    test_cases = [
        ("Simple arithmetic", "(+ 1 2)"),
        ("Nested arithmetic", "(* (+ 1 2) (- 5 3))"),
        ("Multiple operations", "(+ (* 2 3) (* 4 5) (* 6 7))"),
        ("Comparison", "(and (> 5 3) (< 2 10))"),
        ("Conditional", "(if (> 5 3) 10 20)"),
        ("Let binding", "(let ((x 10) (y 20)) (+ x y))"),
        ("List operations", "(length (cons 1 (cons 2 (cons 3 []))))"),
        ("String operation", '(string-upcase "hello")'),
    ]
    
    print(f"\n{'Operation':<25} {'Interp (μs)':<12} {'VM (μs)':<12} {'Speedup':<10} {'Result':<15}")
    print("-" * 80)
    
    total_speedup = 0
    count = 0
    
    for name, code in test_cases:
        try:
            # Warm up
            for _ in range(10):
                graph = parse(code)
                Interpreter().interpret(graph)
                bytecode = BytecodeCompiler().compile(graph)
                VM().execute(bytecode)
            
            # Benchmark
            interp_time, interp_result = benchmark_interpreter(code, 1000)
            vm_time, vm_result = benchmark_vm(code, 1000)
            
            interp_us = interp_time * 1e6
            vm_us = vm_time * 1e6
            speedup = interp_us / vm_us
            
            # Verify results match
            result_str = str(vm_result)[:15]
            if interp_result.data != vm_result:
                result_str += " ⚠️"
            
            print(f"{name:<25} {interp_us:<12.2f} {vm_us:<12.2f} {speedup:<10.1f}x {result_str:<15}")
            
            total_speedup += speedup
            count += 1
            
        except Exception as e:
            print(f"{name:<25} Error: {e}")
    
    if count > 0:
        print("-" * 80)
        print(f"{'Average speedup:':<25} {'':<12} {'':<12} {total_speedup/count:<10.1f}x")
    
    # Detailed analysis
    print("\n\nDetailed Analysis:")
    print("=" * 50)
    
    code = "(+ (* 2 3) (* 4 5))"
    
    # Time different phases
    compiler = BytecodeCompiler()
    vm = VM()
    interpreter = Interpreter()
    
    # Parse time
    start = time.perf_counter()
    graph = parse(code)
    parse_time = time.perf_counter() - start
    
    # Compile time
    start = time.perf_counter()
    bytecode = compiler.compile(graph)
    compile_time = time.perf_counter() - start
    
    # VM execution time
    vm_times = []
    for _ in range(1000):
        start = time.perf_counter()
        result = vm.execute(bytecode)
        vm_times.append(time.perf_counter() - start)
    avg_vm_time = sum(vm_times) / len(vm_times)
    
    # Interpreter time
    interp_times = []
    for _ in range(1000):
        start = time.perf_counter()
        result = interpreter.interpret(graph)
        interp_times.append(time.perf_counter() - start)
    avg_interp_time = sum(interp_times) / len(interp_times)
    
    print(f"Expression: {code}")
    print(f"\nTiming breakdown:")
    print(f"  Parsing:          {parse_time*1e6:8.2f} μs")
    print(f"  Compilation:      {compile_time*1e6:8.2f} μs")
    print(f"  VM execution:     {avg_vm_time*1e6:8.2f} μs")
    print(f"  Total (first run):{(parse_time + compile_time + avg_vm_time)*1e6:8.2f} μs")
    print(f"\nFor comparison:")
    print(f"  Interpreter:      {avg_interp_time*1e6:8.2f} μs")
    print(f"  Speedup:          {avg_interp_time/avg_vm_time:8.1f}x")
    
    # Show bytecode
    print(f"\nGenerated bytecode:")
    print(bytecode.disassemble())
    
    # Benchmark with different input sizes
    print("\n\nScaling Analysis:")
    print("-" * 50)
    print(f"{'List Size':<15} {'Interp (μs)':<15} {'VM (μs)':<15} {'Speedup':<10}")
    print("-" * 50)
    
    for size in [5, 10, 20]:
        # Build nested list
        list_code = "[]"
        for i in range(size):
            list_code = f"(cons {i} {list_code})"
        code = f"(length {list_code})"
        
        interp_time, _ = benchmark_interpreter(code, 100)
        vm_time, _ = benchmark_vm(code, 100)
        
        print(f"{size:<15} {interp_time*1e6:<15.2f} {vm_time*1e6:<15.2f} {interp_time/vm_time:<10.1f}x")


if __name__ == "__main__":
    main()