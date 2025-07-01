"""
Tests for ClaudeLang Contract Specifications
"""

import unittest
from src.parser.sexpr_parser import parse
from src.core.ast import Contract, NodeType


class TestContracts(unittest.TestCase):
    """Test contract parsing and handling"""
    
    def test_parse_simple_contract(self):
        """Test parsing a simple contract"""
        code = """
        (spec:contract add
          :requires [(>= x 0) (>= y 0)]
          :ensures [(>= result 0)]
          :complexity "O(1)")
        """
        
        graph = parse(code)
        
        # Find the contract node
        contract_node = None
        for node in graph.nodes.values():
            if node.node_type == NodeType.CONTRACT:
                contract_node = node
                break
        
        self.assertIsNotNone(contract_node)
        self.assertIsInstance(contract_node, Contract)
        self.assertEqual(contract_node.function_name, "add")
        self.assertEqual(len(contract_node.preconditions), 2)
        self.assertEqual(len(contract_node.postconditions), 1)
        self.assertEqual(contract_node.complexity, "O(1)")
        self.assertTrue(contract_node.pure)
    
    def test_parse_contract_with_invariants(self):
        """Test parsing contract with invariants"""
        code = """
        (spec:contract binary-search
          :requires [(sorted? arr)]
          :ensures [(or (= result -1) (= (nth arr result) target))]
          :invariant [(>= high low)]
          :complexity "O(log n)")
        """
        
        graph = parse(code)
        
        contract_node = None
        for node in graph.nodes.values():
            if node.node_type == NodeType.CONTRACT:
                contract_node = node
                break
        
        self.assertIsNotNone(contract_node)
        self.assertEqual(contract_node.function_name, "binary-search")
        self.assertEqual(len(contract_node.invariants), 1)
        self.assertEqual(contract_node.complexity, "O(log n)")
    
    def test_parse_impure_contract(self):
        """Test parsing contract for impure function"""
        code = """
        (spec:contract read-file
          :requires [(file-exists? path)]
          :ensures [(string? result)]
          :pure false)
        """
        
        graph = parse(code)
        
        contract_node = None
        for node in graph.nodes.values():
            if node.node_type == NodeType.CONTRACT:
                contract_node = node
                break
        
        self.assertIsNotNone(contract_node)
        self.assertEqual(contract_node.function_name, "read-file")
        self.assertFalse(contract_node.pure)
    
    def test_parse_contract_with_pre_post_keywords(self):
        """Test alternative keywords :pre and :post"""
        code = """
        (spec:contract divide
          :pre [(not= y 0)]
          :post [(= result (/ x y))])
        """
        
        graph = parse(code)
        
        contract_node = None
        for node in graph.nodes.values():
            if node.node_type == NodeType.CONTRACT:
                contract_node = node
                break
        
        self.assertIsNotNone(contract_node)
        self.assertEqual(len(contract_node.preconditions), 1)
        self.assertEqual(len(contract_node.postconditions), 1)
    
    def test_parse_empty_contract(self):
        """Test parsing contract with no conditions"""
        code = """
        (spec:contract identity)
        """
        
        graph = parse(code)
        
        contract_node = None
        for node in graph.nodes.values():
            if node.node_type == NodeType.CONTRACT:
                contract_node = node
                break
        
        self.assertIsNotNone(contract_node)
        self.assertEqual(contract_node.function_name, "identity")
        self.assertEqual(len(contract_node.preconditions), 0)
        self.assertEqual(len(contract_node.postconditions), 0)
        self.assertEqual(len(contract_node.invariants), 0)
        self.assertIsNone(contract_node.complexity)
    
    def test_contract_with_complex_conditions(self):
        """Test contract with complex nested conditions"""
        code = """
        (spec:contract matrix-multiply
          :requires [
            (and (matrix? A) (matrix? B))
            (= (cols A) (rows B))
          ]
          :ensures [
            (matrix? result)
            (= (rows result) (rows A))
            (= (cols result) (cols B))
          ]
          :complexity "O(n^3)")
        """
        
        graph = parse(code)
        
        contract_node = None
        for node in graph.nodes.values():
            if node.node_type == NodeType.CONTRACT:
                contract_node = node
                break
        
        self.assertIsNotNone(contract_node)
        self.assertEqual(len(contract_node.preconditions), 2)
        self.assertEqual(len(contract_node.postconditions), 3)
        self.assertEqual(contract_node.complexity, "O(n^3)")


if __name__ == "__main__":
    unittest.main()