#!/usr/bin/env python3
"""
Benchmark to demonstrate caching improvements
"""

import time
from src.parser import parse
from src.interpreter import Interpreter
from src.core.cache import cached_parse, get_cache_stats, clear_cache
from src.stdlib import core, strings, io


def benchmark_without_cache(code: str, iterations: int = 1000):
    """Benchmark without caching"""
    interpreter = Interpreter()
    
    start = time.perf_counter()
    for _ in range(iterations):
        graph = parse(code)
        result = interpreter.interpret(graph)
    end = time.perf_counter()
    
    return (end - start) / iterations


def benchmark_with_cache(code: str, iterations: int = 1000):
    """Benchmark with caching"""
    interpreter = Interpreter()
    clear_cache()  # Start fresh
    
    start = time.perf_counter()
    for _ in range(iterations):
        result = interpreter.eval(code)  # Uses cached_parse internally
    end = time.perf_counter()
    
    return (end - start) / iterations


def main():
    print("ClaudeLang Caching Performance Benchmark")
    print("=" * 50)
    
    test_cases = [
        ("Simple arithmetic", "(+ 1 2)"),
        ("Nested expression", "(* (+ 1 2) (- 5 3))"),
        ("Function call", "(let ((square (lambda (x) (* x x)))) (square 7))"),
        ("List operation", "(length [1 2 3 4 5])"),
        ("String operation", '(string-upcase "hello world")'),
    ]
    
    print(f"\n{'Operation':<25} {'No Cache (μs)':<15} {'Cached (μs)':<15} {'Speedup':<10}")
    print("-" * 70)
    
    total_speedup = 0
    count = 0
    
    for name, code in test_cases:
        # Warm up
        for _ in range(10):
            parse(code)
            cached_parse(code)
        
        # Benchmark
        no_cache_time = benchmark_without_cache(code, 1000) * 1e6  # Convert to μs
        cache_time = benchmark_with_cache(code, 1000) * 1e6
        speedup = no_cache_time / cache_time
        
        print(f"{name:<25} {no_cache_time:<15.2f} {cache_time:<15.2f} {speedup:<10.1f}x")
        
        total_speedup += speedup
        count += 1
    
    print("-" * 70)
    print(f"{'Average speedup:':<25} {'':<15} {'':<15} {total_speedup/count:<10.1f}x")
    
    # Show cache statistics
    stats = get_cache_stats()
    print(f"\nCache Statistics:")
    print(f"  Hits: {stats['hits']}")
    print(f"  Misses: {stats['misses']}")
    print(f"  Hit rate: {stats['hit_rate']:.1%}")
    print(f"  Cache size: {stats['size']}")
    
    # Detailed breakdown for one example
    print("\n\nDetailed Breakdown for '(+ 1 2)':")
    print("-" * 50)
    
    code = "(+ 1 2)"
    interpreter = Interpreter()
    
    # Time individual phases without cache
    parse_times = []
    interpret_times = []
    
    for _ in range(100):
        start = time.perf_counter()
        graph = parse(code)
        parse_time = time.perf_counter() - start
        
        start = time.perf_counter()
        result = interpreter.interpret(graph)
        interpret_time = time.perf_counter() - start
        
        parse_times.append(parse_time)
        interpret_times.append(interpret_time)
    
    avg_parse = sum(parse_times) / len(parse_times) * 1e6
    avg_interpret = sum(interpret_times) / len(interpret_times) * 1e6
    
    # Time with cache
    clear_cache()
    cache_times = []
    
    # First execution (cache miss)
    start = time.perf_counter()
    result = interpreter.eval(code)
    first_time = (time.perf_counter() - start) * 1e6
    
    # Subsequent executions (cache hits)
    for _ in range(100):
        start = time.perf_counter()
        result = interpreter.eval(code)
        cache_times.append((time.perf_counter() - start) * 1e6)
    
    avg_cached = sum(cache_times) / len(cache_times)
    
    print(f"Without cache:")
    print(f"  Parsing:        {avg_parse:.2f} μs ({avg_parse/(avg_parse+avg_interpret)*100:.1f}%)")
    print(f"  Interpretation: {avg_interpret:.2f} μs ({avg_interpret/(avg_parse+avg_interpret)*100:.1f}%)")
    print(f"  Total:          {avg_parse + avg_interpret:.2f} μs")
    
    print(f"\nWith cache:")
    print(f"  First run:      {first_time:.2f} μs (cache miss)")
    print(f"  Cached runs:    {avg_cached:.2f} μs (cache hit)")
    print(f"  Speedup:        {(avg_parse + avg_interpret) / avg_cached:.1f}x")
    
    # Memory usage
    import sys
    graph = parse("(+ 1 2)")
    graph_size = sys.getsizeof(graph.nodes)
    print(f"\nMemory usage:")
    print(f"  AST size: ~{graph_size / 1024:.1f} KB")
    print(f"  Cache overhead: ~{graph_size * stats['size'] / 1024 / 1024:.1f} MB for {stats['size']} entries")


if __name__ == "__main__":
    main()