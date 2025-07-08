#!/usr/bin/env python3
"""Benchmark Python implementation of FluentAI stdlib functions"""

import time
import sys
import os

# Add parent directory to path to import FluentAI modules
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../'))

# Import the core primitives which contains stdlib functions
from src.core.primitives import PRIMITIVES

def init_stdlib():
    # Import all stdlib modules to register functions
    import src.stdlib
    return PRIMITIVES.implementations

def benchmark_function(name, func, args, iterations=1000):
    """Benchmark a single function"""
    start = time.perf_counter()
    for _ in range(iterations):
        result = func(*args)
    end = time.perf_counter()
    
    elapsed = end - start
    per_iter = elapsed / iterations * 1e6  # Convert to microseconds
    print(f"{name}: {per_iter:.2f} μs/iter ({iterations} iterations in {elapsed:.3f}s)")
    return per_iter

def benchmark_list_operations():
    """Benchmark list operations"""
    print("\n=== List Operations ===")
    stdlib = init_stdlib()
    
    # Test append
    small_list = [1, 2, 3]
    benchmark_function("list_append_small", 
                      lambda: stdlib['append'](small_list.copy(), 4), 
                      [], 10000)
    
    # Test reverse
    list_100 = list(range(100))
    benchmark_function("list_reverse_100",
                      lambda: stdlib['reverse'](list_100.copy()),
                      [], 10000)
    
    # Test length
    list_1000 = list(range(1000))
    benchmark_function("list_length_1000",
                      lambda: stdlib['length'](list_1000),
                      [], 10000)
    
    # Test range
    benchmark_function("range_1000",
                      lambda: stdlib['range'](1000),
                      [], 1000)

def benchmark_string_operations():
    """Benchmark string operations"""
    print("\n=== String Operations ===")
    stdlib = init_stdlib()
    
    # Test concat
    strings = [f"string{i}" for i in range(10)]
    benchmark_function("string_concat_10",
                      lambda: stdlib['string-concat'](*strings),
                      [], 10000)
    
    # Test split
    csv = "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z"
    benchmark_function("string_split_csv",
                      lambda: stdlib['string-split'](csv, ","),
                      [], 10000)
    
    # Test upcase
    text = "the quick brown fox jumps over the lazy dog" * 10
    benchmark_function("string_upcase_long",
                      lambda: stdlib['string-upcase'](text),
                      [], 1000)

def benchmark_math_operations():
    """Benchmark math operations"""
    print("\n=== Math Operations ===")
    stdlib = init_stdlib()
    
    # Test addition
    numbers = list(range(100))
    benchmark_function("math_add_100_ints",
                      lambda: stdlib['+'](*numbers),
                      [], 1000)
    
    # Test sqrt
    benchmark_function("math_sqrt_float",
                      lambda: stdlib['sqrt'](16384.0),
                      [], 100000)
    
    # Test trig functions
    angle = 1.5708  # π/2
    def trig_test():
        s = stdlib['sin'](angle)
        c = stdlib['cos'](angle)
        t = stdlib['tan'](angle)
        return s, c, t
    
    benchmark_function("math_trig_sin_cos_tan",
                      trig_test,
                      [], 10000)

def benchmark_collection_operations():
    """Benchmark collection operations"""
    print("\n=== Collection Operations ===")
    stdlib = init_stdlib()
    
    # Test map operations
    def map_test():
        m = {}
        for i in range(100):
            m = stdlib['map-set'](m, f"key{i}", i)
        for i in range(100):
            stdlib['map-get'](m, f"key{i}")
        return m
    
    benchmark_function("map_set_get_100",
                      map_test,
                      [], 100)
    
    # Test flatten
    nested = [[i*2, i*2+1] for i in range(50)]
    benchmark_function("list_flatten_nested",
                      lambda: stdlib['flatten'](nested),
                      [], 1000)

def benchmark_type_checking():
    """Benchmark type checking"""
    print("\n=== Type Checking ===")
    stdlib = init_stdlib()
    
    values = [42, "hello", [], None, 3.14]
    
    def type_check_test():
        for val in values:
            stdlib['int?'](val)
            stdlib['string?'](val)
            stdlib['list?'](val)
            stdlib['nil?'](val)
    
    benchmark_function("type_predicates_mixed",
                      type_check_test,
                      [], 10000)

def main():
    """Run all benchmarks"""
    print("FluentAI Python Standard Library Benchmarks")
    print("=" * 50)
    
    benchmark_list_operations()
    benchmark_string_operations()
    benchmark_math_operations()
    benchmark_collection_operations()
    benchmark_type_checking()
    
    print("\nBenchmarks complete!")

if __name__ == "__main__":
    main()