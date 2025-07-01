#!/usr/bin/env python3
"""
Performance benchmarks comparing ClaudeLang vs Python
"""

import time
import statistics
from src.parser import parse
from src.interpreter import Interpreter
from src.stdlib import core, strings, io


def time_execution(func, *args, iterations=100):
    """Time a function execution over multiple iterations"""
    times = []
    for _ in range(iterations):
        start = time.perf_counter()
        result = func(*args)
        end = time.perf_counter()
        times.append(end - start)
    
    return {
        'mean': statistics.mean(times),
        'median': statistics.median(times),
        'stdev': statistics.stdev(times) if len(times) > 1 else 0,
        'min': min(times),
        'max': max(times),
        'result': result
    }


def format_time(seconds):
    """Format time in appropriate units"""
    if seconds < 1e-6:
        return f"{seconds*1e9:.2f} ns"
    elif seconds < 1e-3:
        return f"{seconds*1e6:.2f} μs"
    elif seconds < 1:
        return f"{seconds*1e3:.2f} ms"
    else:
        return f"{seconds:.2f} s"


class Benchmark:
    def __init__(self):
        self.interpreter = Interpreter()
        self.results = []
    
    def run_claudelang(self, code):
        """Run ClaudeLang code"""
        graph = parse(code)
        return self.interpreter.interpret(graph).data
    
    def add_result(self, name, claudelang_time, python_time):
        """Add benchmark result"""
        speedup = claudelang_time['mean'] / python_time['mean']
        self.results.append({
            'name': name,
            'claudelang': claudelang_time,
            'python': python_time,
            'speedup': speedup
        })
        
        print(f"\n{name}:")
        print(f"  Python:     {format_time(python_time['mean'])} ± {format_time(python_time['stdev'])}")
        print(f"  ClaudeLang: {format_time(claudelang_time['mean'])} ± {format_time(claudelang_time['stdev'])}")
        print(f"  Slowdown:   {speedup:.2f}x slower")
    
    def benchmark_arithmetic(self):
        """Benchmark arithmetic operations"""
        print("\n=== ARITHMETIC OPERATIONS ===")
        
        # Simple addition
        cl_code = "(+ 42 58)"
        py_func = lambda: 42 + 58
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("Simple Addition (42 + 58)", cl_time, py_time)
        
        # Complex arithmetic expression
        cl_code = "(* (+ 10 20) (- 100 50))"
        py_func = lambda: (10 + 20) * (100 - 50)
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("Complex Arithmetic ((10+20)*(100-50))", cl_time, py_time)
        
        # Nested arithmetic
        cl_code = "(+ (* 2 3) (* 4 5) (* 6 7))"
        py_func = lambda: (2 * 3) + (4 * 5) + (6 * 7)
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("Multiple Operations", cl_time, py_time)
    
    def benchmark_list_operations(self):
        """Benchmark list operations"""
        print("\n=== LIST OPERATIONS ===")
        
        # List length
        cl_code = "(length [1 2 3 4 5 6 7 8 9 10])"
        test_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        py_func = lambda: len(test_list)
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("List Length (10 elements)", cl_time, py_time)
        
        # List reverse
        cl_code = "(reverse [1 2 3 4 5 6 7 8 9 10])"
        py_func = lambda: list(reversed(test_list))
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("List Reverse (10 elements)", cl_time, py_time)
        
        # List append
        cl_code = "(append [1 2 3 4 5] [6 7 8 9 10])"
        list1, list2 = [1, 2, 3, 4, 5], [6, 7, 8, 9, 10]
        py_func = lambda: list1 + list2
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("List Append", cl_time, py_time)
    
    def benchmark_string_operations(self):
        """Benchmark string operations"""
        print("\n=== STRING OPERATIONS ===")
        
        # String length
        cl_code = '(string-length "Hello, World!")'
        test_str = "Hello, World!"
        py_func = lambda: len(test_str)
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("String Length", cl_time, py_time)
        
        # String uppercase
        cl_code = '(string-upcase "hello world")'
        test_str = "hello world"
        py_func = lambda: test_str.upper()
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("String Uppercase", cl_time, py_time)
        
        # String split
        cl_code = '(string-split "one,two,three,four,five" ",")'
        test_str = "one,two,three,four,five"
        py_func = lambda: test_str.split(",")
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("String Split", cl_time, py_time)
    
    def benchmark_function_calls(self):
        """Benchmark function definitions and calls"""
        print("\n=== FUNCTION CALLS ===")
        
        # Simple function call
        cl_code = """
        (let ((square (lambda (x) (* x x))))
          (square 7))
        """
        py_func = lambda: (lambda x: x * x)(7)
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_func)
        self.add_result("Simple Function Call", cl_time, py_time)
        
        # Nested function calls
        cl_code = """
        (let ((add1 (lambda (x) (+ x 1)))
              (mul2 (lambda (x) (* x 2))))
          (mul2 (add1 5)))
        """
        def py_nested():
            add1 = lambda x: x + 1
            mul2 = lambda x: x * 2
            return mul2(add1(5))
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code))
        py_time = time_execution(py_nested)
        self.add_result("Nested Function Calls", cl_time, py_time)
    
    def benchmark_recursion(self):
        """Benchmark recursive functions"""
        print("\n=== RECURSION ===")
        
        # Factorial
        cl_code = """
        (let ((fact (lambda (n)
                      (if (<= n 1)
                          1
                          (* n (fact (- n 1)))))))
          (fact 10))
        """
        def py_factorial(n):
            return 1 if n <= 1 else n * py_factorial(n - 1)
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code), iterations=50)
        py_time = time_execution(lambda: py_factorial(10), iterations=50)
        self.add_result("Factorial(10)", cl_time, py_time)
        
        # Fibonacci
        cl_code = """
        (let ((fib (lambda (n)
                     (if (<= n 1)
                         n
                         (+ (fib (- n 1)) (fib (- n 2)))))))
          (fib 15))
        """
        def py_fibonacci(n):
            return n if n <= 1 else py_fibonacci(n - 1) + py_fibonacci(n - 2)
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code), iterations=10)
        py_time = time_execution(lambda: py_fibonacci(15), iterations=10)
        self.add_result("Fibonacci(15)", cl_time, py_time)
    
    def benchmark_higher_order(self):
        """Benchmark higher-order functions"""
        print("\n=== HIGHER-ORDER FUNCTIONS ===")
        
        # Map operation
        cl_code = """
        (let ((map (lambda (f lst)
                     (if (empty? lst)
                         []
                         (cons (f (head lst))
                               (map f (tail lst))))))
              (double (lambda (x) (* x 2))))
          (map double [1 2 3 4 5]))
        """
        def py_map():
            return list(map(lambda x: x * 2, [1, 2, 3, 4, 5]))
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code), iterations=50)
        py_time = time_execution(py_map, iterations=50)
        self.add_result("Map Double [1..5]", cl_time, py_time)
        
        # Filter operation
        cl_code = """
        (let ((filter (lambda (pred lst)
                        (if (empty? lst)
                            []
                            (if (pred (head lst))
                                (cons (head lst) (filter pred (tail lst)))
                                (filter pred (tail lst))))))
              (even? (lambda (x) (== (mod x 2) 0))))
          (filter even? [1 2 3 4 5 6 7 8 9 10]))
        """
        def py_filter():
            return list(filter(lambda x: x % 2 == 0, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
        
        cl_time = time_execution(lambda: self.run_claudelang(cl_code), iterations=50)
        py_time = time_execution(py_filter, iterations=50)
        self.add_result("Filter Even [1..10]", cl_time, py_time)
    
    def print_summary(self):
        """Print summary of all benchmarks"""
        print("\n" + "="*60)
        print("PERFORMANCE SUMMARY")
        print("="*60)
        print(f"{'Operation':<40} {'Slowdown':>10}")
        print("-"*60)
        
        slowdowns = []
        for result in self.results:
            print(f"{result['name']:<40} {result['speedup']:>9.1f}x")
            slowdowns.append(result['speedup'])
        
        print("-"*60)
        print(f"{'Average Slowdown:':<40} {statistics.mean(slowdowns):>9.1f}x")
        print(f"{'Median Slowdown:':<40} {statistics.median(slowdowns):>9.1f}x")
        
        print("\nNOTE: ClaudeLang is interpreted with a tree-walking interpreter")
        print("      written in Python, while Python operations are compiled.")
        print("      A production ClaudeLang implementation would be much faster.")


def main():
    print("ClaudeLang vs Python Performance Benchmark")
    print("==========================================")
    
    benchmark = Benchmark()
    
    # Run all benchmarks
    benchmark.benchmark_arithmetic()
    benchmark.benchmark_list_operations()
    benchmark.benchmark_string_operations()
    benchmark.benchmark_function_calls()
    benchmark.benchmark_recursion()
    benchmark.benchmark_higher_order()
    
    # Print summary
    benchmark.print_summary()


if __name__ == "__main__":
    main()