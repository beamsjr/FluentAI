"""
JIT Compilation Benchmarks for ClaudeLang

Measures the performance impact of JIT compilation on various workloads.
"""

import time
import statistics
from typing import List, Tuple, Dict, Any

from src.vm import VM
from src.vm.compiler import BytecodeCompiler
from src.parser.sexpr_parser import parse
from src.core.ast import Graph
from src.jit import HotspotThreshold


class JITBenchmark:
    """Benchmark suite for JIT compilation"""
    
    def __init__(self):
        self.compiler = BytecodeCompiler()
        # Lower threshold for benchmarking
        HotspotThreshold.FUNCTION_CALLS = 100
    
    def benchmark_fibonacci(self, n: int = 20) -> Tuple[float, float, float]:
        """Benchmark fibonacci with and without JIT"""
        code = f"""
        (let ((fib (lambda (n)
                    (if (<= n 1)
                        n
                        (+ (fib (- n 1))
                           (fib (- n 2)))))))
          (fib {n}))
        """
        
        ast = parse(code)
        graph = Graph()
        graph.add_node(ast)
        chunk = self.compiler.compile_graph(graph)
        
        # Benchmark without JIT
        vm_no_jit = VM(enable_jit=False)
        start = time.perf_counter()
        result_no_jit = vm_no_jit.execute(chunk)
        time_no_jit = time.perf_counter() - start
        
        # Benchmark with JIT (cold start)
        vm_jit = VM(enable_jit=True)
        start = time.perf_counter()
        result_jit_cold = vm_jit.execute(chunk, function_id="fibonacci")
        time_jit_cold = time.perf_counter() - start
        
        # Warm up JIT
        for _ in range(150):
            vm_jit.execute(chunk, function_id="fibonacci")
        
        # Benchmark with JIT (hot)
        times = []
        for _ in range(10):
            start = time.perf_counter()
            result_jit_hot = vm_jit.execute(chunk, function_id="fibonacci")
            times.append(time.perf_counter() - start)
        time_jit_hot = statistics.mean(times)
        
        # Verify results are correct
        assert result_no_jit == result_jit_cold == result_jit_hot
        
        return time_no_jit, time_jit_cold, time_jit_hot
    
    def benchmark_list_operations(self, size: int = 1000) -> Tuple[float, float, float]:
        """Benchmark list operations with and without JIT"""
        code = f"""
        (let ((sum-list (lambda (lst)
                         (if (empty? lst)
                             0
                             (+ (head lst) (sum-list (tail lst)))))))
          (sum-list [1 2 3 4 5 6 7 8 9 10]))
        """
        
        ast = parse(code)
        graph = Graph()
        graph.add_node(ast)
        chunk = self.compiler.compile_graph(graph)
        
        # Similar benchmarking pattern
        vm_no_jit = VM(enable_jit=False)
        start = time.perf_counter()
        vm_no_jit.execute(chunk)
        time_no_jit = time.perf_counter() - start
        
        vm_jit = VM(enable_jit=True)
        start = time.perf_counter()
        vm_jit.execute(chunk, function_id="list_sum")
        time_jit_cold = time.perf_counter() - start
        
        # Warm up
        for _ in range(150):
            vm_jit.execute(chunk, function_id="list_sum")
        
        # Hot benchmark
        times = []
        for _ in range(10):
            start = time.perf_counter()
            vm_jit.execute(chunk, function_id="list_sum")
            times.append(time.perf_counter() - start)
        time_jit_hot = statistics.mean(times)
        
        return time_no_jit, time_jit_cold, time_jit_hot
    
    def benchmark_arithmetic_loop(self, iterations: int = 10000) -> Tuple[float, float, float]:
        """Benchmark arithmetic-heavy loop"""
        code = f"""
        (let ((loop (lambda (n acc)
                     (if (= n 0)
                         acc
                         (loop (- n 1) (+ acc (* n n)))))))
          (loop {iterations} 0))
        """
        
        ast = parse(code)
        graph = Graph()
        graph.add_node(ast)
        chunk = self.compiler.compile_graph(graph)
        
        # Benchmark pattern
        vm_no_jit = VM(enable_jit=False)
        start = time.perf_counter()
        vm_no_jit.execute(chunk)
        time_no_jit = time.perf_counter() - start
        
        vm_jit = VM(enable_jit=True)
        start = time.perf_counter()
        vm_jit.execute(chunk, function_id="arithmetic_loop")
        time_jit_cold = time.perf_counter() - start
        
        # Warm up
        for _ in range(150):
            vm_jit.execute(chunk, function_id="arithmetic_loop")
        
        # Hot benchmark
        times = []
        for _ in range(10):
            start = time.perf_counter()
            vm_jit.execute(chunk, function_id="arithmetic_loop")
            times.append(time.perf_counter() - start)
        time_jit_hot = statistics.mean(times)
        
        return time_no_jit, time_jit_cold, time_jit_hot
    
    def run_all_benchmarks(self) -> Dict[str, Dict[str, float]]:
        """Run all JIT benchmarks and return results"""
        results = {}
        
        print("Running JIT Compilation Benchmarks...")
        print("=" * 60)
        
        # Fibonacci benchmark
        print("\nFibonacci(20):")
        no_jit, jit_cold, jit_hot = self.benchmark_fibonacci(20)
        results["fibonacci"] = {
            "no_jit": no_jit,
            "jit_cold": jit_cold,
            "jit_hot": jit_hot,
            "speedup_cold": no_jit / jit_cold if jit_cold > 0 else 0,
            "speedup_hot": no_jit / jit_hot if jit_hot > 0 else 0
        }
        print(f"  No JIT:    {no_jit:.4f}s")
        print(f"  JIT Cold:  {jit_cold:.4f}s ({results['fibonacci']['speedup_cold']:.2f}x)")
        print(f"  JIT Hot:   {jit_hot:.4f}s ({results['fibonacci']['speedup_hot']:.2f}x)")
        
        # List operations benchmark
        print("\nList Sum (10 elements):")
        no_jit, jit_cold, jit_hot = self.benchmark_list_operations()
        results["list_operations"] = {
            "no_jit": no_jit,
            "jit_cold": jit_cold,
            "jit_hot": jit_hot,
            "speedup_cold": no_jit / jit_cold if jit_cold > 0 else 0,
            "speedup_hot": no_jit / jit_hot if jit_hot > 0 else 0
        }
        print(f"  No JIT:    {no_jit:.4f}s")
        print(f"  JIT Cold:  {jit_cold:.4f}s ({results['list_operations']['speedup_cold']:.2f}x)")
        print(f"  JIT Hot:   {jit_hot:.4f}s ({results['list_operations']['speedup_hot']:.2f}x)")
        
        # Arithmetic loop benchmark
        print("\nArithmetic Loop (1000 iterations):")
        no_jit, jit_cold, jit_hot = self.benchmark_arithmetic_loop(1000)
        results["arithmetic_loop"] = {
            "no_jit": no_jit,
            "jit_cold": jit_cold,
            "jit_hot": jit_hot,
            "speedup_cold": no_jit / jit_cold if jit_cold > 0 else 0,
            "speedup_hot": no_jit / jit_hot if jit_hot > 0 else 0
        }
        print(f"  No JIT:    {no_jit:.4f}s")
        print(f"  JIT Cold:  {jit_cold:.4f}s ({results['arithmetic_loop']['speedup_cold']:.2f}x)")
        print(f"  JIT Hot:   {jit_hot:.4f}s ({results['arithmetic_loop']['speedup_hot']:.2f}x)")
        
        # Summary
        print("\n" + "=" * 60)
        print("Summary:")
        print(f"  Average Cold Speedup: {statistics.mean([r['speedup_cold'] for r in results.values()]):.2f}x")
        print(f"  Average Hot Speedup:  {statistics.mean([r['speedup_hot'] for r in results.values()]):.2f}x")
        
        # Note about current implementation
        print("\nNote: JIT compilation is currently in profile mode only.")
        print("Native code generation will provide actual speedups.")
        
        return results


def main():
    """Run JIT benchmarks"""
    benchmark = JITBenchmark()
    results = benchmark.run_all_benchmarks()
    
    # Create a simple VM with JIT and show compilation stats
    print("\nJIT Compilation Statistics:")
    print("-" * 40)
    vm = VM(enable_jit=True)
    
    # Run a simple function many times to trigger profiling
    code = "(+ 1 2)"
    ast = parse(code)
    graph = Graph()
    graph.add_node(ast)
    chunk = benchmark.compiler.compile_graph(graph)
    
    for i in range(200):
        vm.execute(chunk, function_id="simple_add")
    
    stats = vm.get_jit_stats()
    print(f"Total functions profiled: {stats.get('total_functions', 0)}")
    print(f"Functions compiled: {stats.get('compiled_functions', 0)}")
    print(f"Total executions: {stats.get('total_executions', 0)}")
    
    if stats.get('hot_functions'):
        print("\nHot functions:")
        for func in stats['hot_functions']:
            print(f"  {func['id']}: {func['calls']} calls, "
                  f"avg time: {func['avg_time']:.6f}s, "
                  f"compiled: {func['compiled']}")


if __name__ == "__main__":
    main()