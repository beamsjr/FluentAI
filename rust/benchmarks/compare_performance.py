#!/usr/bin/env python3
"""Simple performance comparison between Python and Rust implementations"""

import subprocess
import time
import sys
import os

def run_rust_benchmark():
    """Run Rust benchmarks and parse results"""
    print("Running Rust benchmarks...")
    result = subprocess.run(
        ["cargo", "bench", "--bench", "stdlib_benchmarks"],
        capture_output=True,
        text=True,
        cwd=os.path.join(os.path.dirname(__file__), "../fluentai-stdlib")
    )
    
    if result.returncode != 0:
        print("Error running Rust benchmarks:")
        print(result.stderr)
        return {}
    
    # Parse benchmark results
    results = {}
    for line in result.stdout.split('\n'):
        if "time:" in line and "[" in line:
            # Extract benchmark name and time
            parts = line.split()
            if len(parts) >= 3:
                name = parts[0]
                # Find the time value (in format like [1.2345 µs 1.2456 µs 1.2567 µs])
                time_str = line[line.find('['):line.find(']')+1]
                # Extract the middle value
                time_parts = time_str.strip('[]').split()
                if len(time_parts) >= 3:
                    time_value = float(time_parts[1])
                    time_unit = time_parts[2]
                    # Convert to microseconds
                    if time_unit == 'ns':
                        time_value /= 1000
                    elif time_unit == 'ms':
                        time_value *= 1000
                    results[name] = time_value
    
    return results

def simple_python_benchmarks():
    """Run simple Python benchmarks for comparison"""
    print("\nRunning Python equivalent benchmarks...")
    results = {}
    
    # List reverse benchmark
    def bench_reverse():
        lst = list(range(100))
        start = time.perf_counter()
        for _ in range(10000):
            lst.copy()[::-1]
        end = time.perf_counter()
        return (end - start) / 10000 * 1e6  # microseconds per iteration
    
    results['list_reverse_100'] = bench_reverse()
    print(f"list_reverse_100: {results['list_reverse_100']:.2f} µs")
    
    # String concat benchmark
    def bench_concat():
        strings = [f"string{i}" for i in range(10)]
        start = time.perf_counter()
        for _ in range(10000):
            "".join(strings)
        end = time.perf_counter()
        return (end - start) / 10000 * 1e6
    
    results['string_concat_10'] = bench_concat()
    print(f"string_concat_10: {results['string_concat_10']:.2f} µs")
    
    # Math operations
    def bench_sqrt():
        import math
        start = time.perf_counter()
        for _ in range(100000):
            math.sqrt(16384.0)
        end = time.perf_counter()
        return (end - start) / 100000 * 1e6
    
    results['math_sqrt'] = bench_sqrt()
    print(f"math_sqrt: {results['math_sqrt']:.2f} µs")
    
    return results

def compare_results(rust_results, python_results):
    """Compare Rust and Python results"""
    print("\n" + "="*60)
    print("Performance Comparison (lower is better)")
    print("="*60)
    print(f"{'Benchmark':<30} {'Python (µs)':<15} {'Rust (µs)':<15} {'Speedup':<10}")
    print("-"*60)
    
    for bench in python_results:
        if bench in rust_results:
            py_time = python_results[bench]
            rust_time = rust_results[bench]
            speedup = py_time / rust_time
            print(f"{bench:<30} {py_time:<15.2f} {rust_time:<15.2f} {speedup:<10.1f}x")

def main():
    print("ClaudeLang Performance Comparison")
    print("="*50)
    
    # Run benchmarks
    rust_results = run_rust_benchmark()
    python_results = simple_python_benchmarks()
    
    # Compare results
    if rust_results:
        compare_results(rust_results, python_results)
    else:
        print("\nCould not run Rust benchmarks for comparison")
        print("Python results:")
        for name, time in python_results.items():
            print(f"  {name}: {time:.2f} µs")

if __name__ == "__main__":
    main()