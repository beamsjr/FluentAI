"""
Tests for JIT Guard-based Type Specialization
"""

import unittest
from src.vm import VM
from src.vm.compiler import BytecodeCompiler
from src.parser.sexpr_parser import parse
from src.jit import HotspotThreshold, GuardType
from src.jit.guards import TypeProfiler, GuardCondition


class TestJITGuards(unittest.TestCase):
    """Test guard-based specialization"""
    
    def setUp(self):
        """Set up test environment"""
        self.vm = VM(enable_jit=True)
        self.compiler = BytecodeCompiler()
        # Lower thresholds for testing
        HotspotThreshold.FUNCTION_CALLS = 10
    
    def test_type_profiling(self):
        """Test that type profiling works correctly"""
        profiler = TypeProfiler()
        
        # Profile integer parameters
        for i in range(100):
            profiler.record_parameter(0, i)
            profiler.record_parameter(1, i * 2)
        
        # Profile some strings
        for i in range(5):
            profiler.record_parameter(0, f"str_{i}")
        
        # Generate guards
        guards = profiler.get_guards(threshold=0.9)
        
        # Should have type guard for parameter 1 (all integers)
        int_guards = [g for g in guards if g.guard_type == GuardType.TYPE_CHECK]
        self.assertTrue(any(g.parameter_index == 1 for g in int_guards))
        
        # Should have range guard for numeric parameters
        range_guards = [g for g in guards if g.guard_type == GuardType.RANGE_CHECK]
        self.assertTrue(len(range_guards) > 0)
    
    def test_constant_specialization(self):
        """Test specialization for constant values"""
        profiler = TypeProfiler()
        
        # Profile mostly constant value
        for i in range(95):
            profiler.record_parameter(0, 42)
        for i in range(5):
            profiler.record_parameter(0, i)
        
        guards = profiler.get_guards(threshold=0.9)
        
        # Should have constant guard for parameter 0
        const_guards = [g for g in guards if g.guard_type == GuardType.CONSTANT]
        self.assertEqual(len(const_guards), 1)
        self.assertEqual(const_guards[0].expected_value, 42)
    
    def test_monomorphic_function(self):
        """Test JIT compilation of monomorphic function"""
        # Function that always receives integers
        code = """
        (define (add x y)
          (+ x y))
        """
        
        graph = parse(code)
        chunk = self.compiler.compile(graph)
        
        # Call with integers many times
        for i in range(15):
            result = self.vm.execute(chunk, function_id="add_int")
        
        # Check that specialization was created
        stats = self.vm.get_jit_stats()
        self.assertGreater(stats["total_specializations"], 0)
        
        # Hot functions should have specialization info
        if stats["hot_functions"]:
            func = stats["hot_functions"][0]
            spec_stats = func["specializations"]
            self.assertGreater(spec_stats["specialization_count"], 0)
    
    def test_polymorphic_function(self):
        """Test function with multiple type profiles"""
        code = """
        (define (identity x)
          x)
        """
        
        graph = parse(code)
        chunk = self.compiler.compile(graph)
        
        # Call with different types
        for i in range(10):
            # Integer calls
            self.vm.execute(chunk, function_id="identity_poly")
            # String calls would need proper string support
        
        # Multiple specializations might be created
        stats = self.vm.get_jit_stats()
        print(f"Polymorphic stats: {stats}")
    
    def test_range_based_optimization(self):
        """Test optimization based on value ranges"""
        code = """
        (define (safe-divide x y)
          (/ x y))
        """
        
        graph = parse(code)
        chunk = self.compiler.compile(graph)
        
        # Call with positive divisors only
        for i in range(15):
            # y is always positive (1-10)
            self.vm.execute(chunk, function_id="safe_divide")
        
        # Should create specialization that skips zero check
        stats = self.vm.get_jit_stats()
        self.assertGreater(stats["total_specializations"], 0)
    
    def test_guard_failure_handling(self):
        """Test handling of guard failures"""
        # Create a simple profiler
        profiler = TypeProfiler()
        
        # Create integer-only guard
        guard = GuardCondition(
            guard_type=GuardType.TYPE_CHECK,
            parameter_index=0,
            expected_value="int"
        )
        
        # Test guard success
        self.assertTrue(guard.check_value(42))
        
        # Test guard failure
        self.assertFalse(guard.check_value("not an int"))
        self.assertFalse(guard.check_value(3.14))
        self.assertFalse(guard.check_value([1, 2, 3]))


if __name__ == "__main__":
    unittest.main()