"""
Test type inference system
"""

import unittest
from src.parser import parse
from src.types.type_inference import OptimizationTypeInferencer, InferredType, BasicType


class TestTypeInference(unittest.TestCase):
    """Test type inference functionality"""
    
    def setUp(self):
        self.inferencer = OptimizationTypeInferencer()
    
    def test_literal_types(self):
        """Test type inference for literals"""
        test_cases = [
            ("42", BasicType.INT, True, 42),
            ("3.14", BasicType.FLOAT, True, 3.14),
            ("#t", BasicType.BOOL, True, True),
            ("#f", BasicType.BOOL, True, False),
            ('"hello"', BasicType.STRING, True, "hello"),
            ("[1 2 3]", BasicType.LIST, True, None),
        ]
        
        for code, expected_type, is_constant, value in test_cases:
            with self.subTest(code=code):
                graph = parse(code)
                types = self.inferencer.infer_graph_types(graph)
                
                # Find the literal node
                literal_type = None
                for node_id, node_type in types.items():
                    if node_type.base_type == expected_type:
                        literal_type = node_type
                        break
                
                self.assertIsNotNone(literal_type, f"No type found for {code}")
                self.assertEqual(literal_type.base_type, expected_type)
                self.assertEqual(literal_type.is_constant, is_constant)
                if value is not None:
                    self.assertEqual(literal_type.constant_value, value)
    
    def test_arithmetic_inference(self):
        """Test type inference for arithmetic"""
        test_cases = [
            ("(+ 2 3)", BasicType.INT, True, 5),
            ("(* 4 5)", BasicType.INT, True, 20),
            ("(- 10 3)", BasicType.INT, True, 7),
            ("(/ 20 4)", BasicType.INT, True, 5),
        ]
        
        for code, expected_type, is_constant, value in test_cases:
            with self.subTest(code=code):
                graph = parse(code)
                types = self.inferencer.infer_graph_types(graph)
                
                # Find application node
                app_type = None
                for node_id, node in graph.nodes.items():
                    if hasattr(node, 'function_id'):  # Application node
                        app_type = types.get(node_id)
                        break
                
                self.assertIsNotNone(app_type, f"No application type for {code}")
                self.assertEqual(app_type.base_type, expected_type)
                self.assertEqual(app_type.is_constant, is_constant)
                if value is not None:
                    self.assertEqual(app_type.constant_value, value)
    
    def test_comparison_inference(self):
        """Test type inference for comparisons"""
        test_cases = [
            ("(< 3 5)", BasicType.BOOL, True, True),
            ("(> 10 5)", BasicType.BOOL, True, True),
            ("(== 42 42)", BasicType.BOOL, True, True),
            ("(!= 10 20)", BasicType.BOOL, True, True),
        ]
        
        for code, expected_type, is_constant, value in test_cases:
            with self.subTest(code=code):
                graph = parse(code)
                types = self.inferencer.infer_graph_types(graph)
                
                # Find application node
                app_type = None
                for node_id, node in graph.nodes.items():
                    if hasattr(node, 'function_id'):  # Application node
                        app_type = types.get(node_id)
                        break
                
                self.assertIsNotNone(app_type)
                self.assertEqual(app_type.base_type, expected_type)
                self.assertEqual(app_type.is_constant, is_constant)
                self.assertEqual(app_type.constant_value, value)
    
    def test_conditional_inference(self):
        """Test type inference for conditionals"""
        # Constant condition
        graph = parse("(if (> 10 5) 100 200)")
        types = self.inferencer.infer_graph_types(graph)
        
        # The if node should have inferred the taken branch
        if_type = None
        for node_id, node in graph.nodes.items():
            if hasattr(node, 'condition_id'):  # If node
                if_type = types.get(node_id)
                break
        
        self.assertIsNotNone(if_type)
        self.assertEqual(if_type.base_type, BasicType.INT)
        self.assertEqual(if_type.constant_value, 100)
    
    def test_optimization_opportunities(self):
        """Test identification of optimization opportunities"""
        from src.types.type_inference import analyze_optimization_opportunities
        
        code = "(+ (* 2 3) (* 4 5))"
        graph = parse(code)
        opportunities = analyze_optimization_opportunities(graph)
        
        # Should identify constants
        self.assertGreater(len(opportunities['constant_nodes']), 0)
        
        # Should identify numeric operations
        self.assertGreater(len(opportunities['numeric_operations']), 0)
        
        # Should identify all constants are integers
        for const in opportunities['constant_nodes']:
            if const['value'] in [2, 3, 4, 5, 6, 20, 26]:
                self.assertIn('INT', const['type'])


class TestTypeSpecialization(unittest.TestCase):
    """Test type-based specialization"""
    
    def test_numeric_specialization(self):
        """Test that numeric operations get specialized"""
        from src.types.type_inference import OptimizationTypeInferencer
        
        inferencer = OptimizationTypeInferencer()
        
        # Integer operation
        graph = parse("(+ 10 20)")
        types = inferencer.infer_graph_types(graph)
        
        # Check all values are typed as INT
        int_count = sum(1 for t in types.values() if t.base_type == BasicType.INT)
        self.assertGreater(int_count, 2)  # At least the literals and result
    
    def test_mixed_type_inference(self):
        """Test inference with mixed types"""
        from src.types.type_inference import OptimizationTypeInferencer
        
        inferencer = OptimizationTypeInferencer()
        
        # String operation
        graph = parse('(string-length "hello")')
        types = inferencer.infer_graph_types(graph)
        
        # Result should be INT
        app_type = None
        for node_id, node in graph.nodes.items():
            if hasattr(node, 'function_id'):
                app_type = types.get(node_id)
                break
        
        self.assertIsNotNone(app_type)
        self.assertEqual(app_type.base_type, BasicType.INT)


if __name__ == "__main__":
    unittest.main()