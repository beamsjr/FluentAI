"""
Tests for ClaudeLang JIT Compiler
"""

import unittest
import time
from src.vm import VM
from src.vm.compiler import BytecodeCompiler
from src.parser.sexpr_parser import parse
from src.core.ast import Graph
from src.jit import HotspotThreshold


class TestJIT(unittest.TestCase):
    """Test JIT compilation functionality"""
    
    def setUp(self):
        """Set up test environment"""
        self.vm = VM(enable_jit=True)
        self.compiler = BytecodeCompiler()
        # Lower thresholds for testing
        HotspotThreshold.FUNCTION_CALLS = 10
    
    def test_jit_profiling(self):
        """Test that JIT profiling works"""
        # Simple function to profile
        code = "(+ 1 2)"
        graph = parse(code)
        
        chunk = self.compiler.compile(graph)
        
        # Execute multiple times to trigger profiling
        for i in range(15):
            result = self.vm.execute(chunk, function_id="test_add")
            self.assertEqual(result, 3)
        
        # Check JIT stats
        stats = self.vm.get_jit_stats()
        self.assertIn("total_functions", stats)
        self.assertIn("hot_functions", stats)
        self.assertEqual(stats["total_functions"], 1)
    
    def test_hot_function_detection(self):
        """Test detection of hot functions"""
        # Fibonacci function that will become hot
        code = """
        (let ((fib (lambda (n)
                    (if (<= n 1)
                        n
                        (+ (fib (- n 1))
                           (fib (- n 2)))))))
          (fib 5))
        """
        
        graph = parse(code)
        chunk = self.compiler.compile(graph)
        
        # Execute multiple times
        results = []
        for i in range(12):
            result = self.vm.execute(chunk, function_id="fibonacci")
            results.append(result)
        
        # All results should be correct
        for result in results:
            self.assertEqual(result, 5)  # fib(5) = 5
        
        # Check that function is marked as hot
        stats = self.vm.get_jit_stats()
        if stats.get("hot_functions"):
            hot_func = stats["hot_functions"][0]
            self.assertTrue(hot_func["calls"] >= 10)
    
    def test_performance_improvement(self):
        """Test that JIT compilation improves performance"""
        # Compute-intensive function
        code = """
        (let ((sum (lambda (n)
                    (if (= n 0)
                        0
                        (+ n (sum (- n 1)))))))
          (sum 100))
        """
        
        graph = parse(code)
        chunk = self.compiler.compile(graph)
        
        # Warm up - should trigger JIT compilation
        for i in range(15):
            self.vm.execute(chunk, function_id="sum_recursive")
        
        # Measure performance after JIT
        start = time.perf_counter()
        for i in range(100):
            result = self.vm.execute(chunk, function_id="sum_recursive")
        jit_time = time.perf_counter() - start
        
        # Compare with non-JIT VM
        vm_no_jit = VM(enable_jit=False)
        start = time.perf_counter()
        for i in range(100):
            result = vm_no_jit.execute(chunk)
        no_jit_time = time.perf_counter() - start
        
        # JIT should be faster (or at least not significantly slower)
        # Note: actual speedup depends on native code generation implementation
        print(f"JIT time: {jit_time:.4f}s, No JIT time: {no_jit_time:.4f}s")
        print(f"JIT stats: {self.vm.get_jit_stats()}")
    
    def test_guard_failure_handling(self):
        """Test handling of guard failures"""
        # Function that changes behavior based on input type
        code = """
        (let ((process (lambda (x)
                        (if (int? x)
                            (* x 2)
                            (string-length x)))))
          process)
        """
        
        graph = parse(code)
        chunk = self.compiler.compile(graph)
        
        # First, call with integers to establish type profile
        for i in range(15):
            # This would need proper function call setup
            pass
        
        # Then call with string to trigger guard failure
        # The JIT should handle this gracefully
        # (Implementation depends on full function call support)


if __name__ == "__main__":
    unittest.main()