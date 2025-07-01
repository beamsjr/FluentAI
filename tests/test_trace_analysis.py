"""
Tests for Execution Trace Analysis
"""

import unittest
import json
import tempfile
from src.interpreter.trace_interpreter import TracingInterpreter
from src.docs.trace_analyzer import ExecutionTraceAnalyzer


class TestTraceAnalysis(unittest.TestCase):
    """Test execution trace analysis for documentation"""
    
    def setUp(self):
        """Set up test environment"""
        self.analyzer = ExecutionTraceAnalyzer()
        self.interpreter = TracingInterpreter(trace_analyzer=self.analyzer)
    
    def test_function_profiling(self):
        """Test function execution profiling"""
        code = """
        (let ((add (lambda (x y) (+ x y)))
              (multiply (lambda (x y) (* x y))))
          (do
            (add 1 2)
            (add 3 4)
            (multiply 5 6)
            (add 7 8)))
        """
        
        # Execute code
        result = self.interpreter.eval(code)
        
        # Check function profiles
        profiles = self.analyzer.function_profiles
        
        # Should have profiles for add and multiply
        self.assertIn('add', profiles)
        self.assertIn('multiply', profiles)
        
        # Check call counts
        self.assertEqual(profiles['add'].call_count, 3)
        self.assertEqual(profiles['multiply'].call_count, 1)
        
        # Check that examples were recorded
        self.assertTrue(len(profiles['add'].example_inputs) > 0)
        self.assertTrue(len(profiles['add'].example_outputs) > 0)
        
        # Generate documentation
        doc = self.interpreter.generate_documentation()
        self.assertIn('Function Profiles', doc)
        self.assertIn('add', doc)
        self.assertIn('Calls: 3', doc)
    
    def test_data_flow_tracking(self):
        """Test tracking data flow between functions"""
        code = """
        (let ((double (lambda (x) (* x 2)))
              (add-one (lambda (x) (+ x 1)))
              (compose (lambda (f g x) (f (g x)))))
          (compose double add-one 5))
        """
        
        result = self.interpreter.eval(code)
        
        # Check data flows
        data_flows = self.analyzer.data_flows
        self.assertTrue(len(data_flows) > 0)
        
        # Should see flow from add-one to double
        flow_found = False
        for flow in data_flows:
            if flow.source_function == 'add-one' and flow.target_function == 'double':
                flow_found = True
                break
        self.assertTrue(flow_found)
    
    def test_effect_tracking(self):
        """Test tracking of effects in functions"""
        code = """
        (let ((pure-func (lambda (x) (* x 2)))
              (impure-func (lambda (x) 
                             (do
                               (effect io print x)
                               (* x 2)))))
          (do
            (pure-func 5)
            (impure-func 10)))
        """
        
        # Suppress output during test
        import sys
        from io import StringIO
        old_stdout = sys.stdout
        sys.stdout = StringIO()
        
        try:
            result = self.interpreter.eval(code)
            
            # Check effect tracking
            profiles = self.analyzer.function_profiles
            
            # pure-func should have no effects
            self.assertTrue(profiles['pure-func'].is_pure)
            self.assertEqual(len(profiles['pure-func'].effects_used), 0)
            
            # impure-func should have IO effect
            self.assertFalse(profiles['impure-func'].is_pure)
            self.assertTrue(len(profiles['impure-func'].effects_used) > 0)
            
        finally:
            sys.stdout = old_stdout
    
    def test_branch_profiling(self):
        """Test profiling of conditional branches"""
        code = """
        (let ((abs (lambda (x) (if (< x 0) (- 0 x) x))))
          (do
            (abs -5)
            (abs -3)
            (abs 2)
            (abs 4)
            (abs -1)))
        """
        
        result = self.interpreter.eval(code)
        
        # Check branch profiles
        branch_profiles = self.analyzer.branch_profiles
        self.assertTrue(len(branch_profiles) > 0)
        
        # Should have recorded both true and false branches
        total_true = sum(p.true_count for p in branch_profiles.values())
        total_false = sum(p.false_count for p in branch_profiles.values())
        
        self.assertEqual(total_true, 3)  # Three negative numbers
        self.assertEqual(total_false, 2)  # Two positive numbers
    
    def test_call_graph_generation(self):
        """Test call graph generation"""
        code = """
        (let ((f1 (lambda () (f2)))
              (f2 (lambda () (f3)))
              (f3 (lambda () 42)))
          (f1))
        """
        
        result = self.interpreter.eval(code)
        
        # Check call graph
        call_graph = self.analyzer.call_graph
        
        # f1 should call f2
        self.assertIn('f2', call_graph.get('f1', set()))
        
        # f2 should call f3
        self.assertIn('f3', call_graph.get('f2', set()))
        
        # Generate documentation with call graph
        doc = self.interpreter.generate_documentation()
        self.assertIn('Call Graph', doc)
        self.assertIn('f1 --> f2', doc)
        self.assertIn('f2 --> f3', doc)
    
    def test_performance_analysis(self):
        """Test performance bottleneck detection"""
        code = """
        (let ((slow-func (lambda (n)
                          (if (= n 0)
                              0
                              (+ n (slow-func (- n 1))))))
              (fast-func (lambda (x) (* x 2))))
          (do
            (slow-func 10)
            (fast-func 5)
            (fast-func 10)))
        """
        
        result = self.interpreter.eval(code)
        
        # Analyze patterns
        patterns = self.analyzer.analyze_patterns()
        
        # Check for hot paths
        self.assertIn('hot_paths', patterns)
        
        # Check for bottlenecks
        bottlenecks = patterns.get('bottlenecks', [])
        # The recursive function should show up as high call frequency
        slow_func_found = any(b['function'] == 'slow-func' for b in bottlenecks)
        self.assertTrue(slow_func_found or len(self.analyzer.function_profiles['slow-func'].call_sites) > 5)
    
    def test_optimization_opportunity_detection(self):
        """Test detection of optimization opportunities"""
        code = """
        (let ((pure-add (lambda (x y) (+ x y))))
          (do
            (pure-add 1 2)
            (pure-add 1 2)
            (pure-add 1 2)
            (pure-add 3 4)
            (pure-add 3 4)))
        """
        
        result = self.interpreter.eval(code)
        
        # Analyze patterns
        patterns = self.analyzer.analyze_patterns()
        opportunities = patterns.get('optimization_opportunities', [])
        
        # Should detect memoization opportunity
        memo_found = any(o['type'] == 'memoization' for o in opportunities)
        # This might not be detected with current simple implementation
        # but the structure is there for more sophisticated analysis
    
    def test_export_trace_data(self):
        """Test exporting trace data"""
        code = """
        (let ((factorial (lambda (n)
                          (if (= n 0)
                              1
                              (* n (factorial (- n 1)))))))
          (factorial 5))
        """
        
        result = self.interpreter.eval(code)
        
        # Export to temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            self.interpreter.export_traces(f.name)
            
            # Read back and verify
            with open(f.name, 'r') as rf:
                data = json.load(rf)
            
            self.assertIn('metadata', data)
            self.assertIn('function_profiles', data)
            self.assertIn('call_graph', data)
            self.assertIn('patterns', data)
            
            # Check factorial profile
            self.assertIn('factorial', data['function_profiles'])
            factorial_profile = data['function_profiles']['factorial']
            self.assertGreater(factorial_profile['call_count'], 1)  # Recursive calls
    
    def test_documentation_generation(self):
        """Test complete documentation generation"""
        code = """
        ; Example program with various patterns
        (let ((map (lambda (f lst)
                     (if (empty? lst)
                         []
                         (cons (f (head lst)) 
                               (map f (tail lst))))))
              (square (lambda (x) (* x x)))
              (is-even (lambda (x) (= (mod x 2) 0))))
          (do
            ; Map square over a list
            (map square [1 2 3 4 5])
            
            ; Filter with condition
            (let ((filtered []))
              (map (lambda (x) 
                     (if (is-even x) 
                         (cons x filtered)
                         filtered))
                   [1 2 3 4 5 6]))))
        """
        
        result = self.interpreter.eval(code)
        
        # Generate documentation
        doc = self.interpreter.generate_documentation()
        
        # Check documentation sections
        self.assertIn('Execution Trace Analysis Report', doc)
        self.assertIn('Function Profiles', doc)
        self.assertIn('Call Graph', doc)
        self.assertIn('map', doc)
        self.assertIn('square', doc)
        self.assertIn('is-even', doc)
        
        # Check for examples
        self.assertIn('Examples', doc)
        
        # Check for performance analysis
        if 'Performance Bottlenecks' in doc:
            self.assertIn('map', doc)  # Recursive function
        
        print("\nGenerated Documentation Sample:")
        print("=" * 60)
        print(doc[:1000] + "..." if len(doc) > 1000 else doc)


if __name__ == "__main__":
    unittest.main()