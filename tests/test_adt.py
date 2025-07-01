"""
Tests for Algebraic Data Types (ADTs) in ClaudeLang
"""

import unittest
from src.parser.sexpr_parser import parse
from src.interpreter.interpreter import Interpreter
from src.core.ast import DataDeclaration, NodeType


class TestADTParsing(unittest.TestCase):
    
    def test_parse_simple_adt(self):
        """Test parsing a simple ADT declaration"""
        source = "(data Option a (None) (Some a))"
        graph = parse(source)
        
        # Should have one root node - the data declaration
        self.assertEqual(len(graph.nodes), 1)
        
        # Get the root node
        node_id = graph.root_id
        node = graph.get_node(node_id)
        
        self.assertIsInstance(node, DataDeclaration)
        self.assertEqual(node.type_name, "Option")
        self.assertEqual(node.type_params, ["a"])
        self.assertEqual(len(node.constructors), 2)
        
        # Check None constructor
        none_cons = node.constructors[0]
        self.assertEqual(none_cons["name"], "None")
        self.assertEqual(len(none_cons["fields"]), 0)
        
        # Check Some constructor
        some_cons = node.constructors[1]
        self.assertEqual(some_cons["name"], "Some")
        self.assertEqual(len(some_cons["fields"]), 1)
        self.assertEqual(some_cons["fields"][0].name, "a")
    
    def test_parse_recursive_adt(self):
        """Test parsing a recursive ADT"""
        source = "(data List a (Nil) (Cons a (List a)))"
        graph = parse(source)
        
        # Get the root node
        node_id = graph.root_id
        node = graph.get_node(node_id)
        
        self.assertIsInstance(node, DataDeclaration)
        self.assertEqual(node.type_name, "List")
        self.assertEqual(node.type_params, ["a"])
        
        # Check Cons constructor has two fields
        cons = node.constructors[1]
        self.assertEqual(cons["name"], "Cons")
        self.assertEqual(len(cons["fields"]), 2)
        self.assertEqual(cons["fields"][0].name, "a")
        self.assertEqual(cons["fields"][1].name, "List")
        self.assertEqual(cons["fields"][1].parameters[0].name, "a")
    
    def test_parse_multi_field_adt(self):
        """Test parsing ADT with multiple fields"""
        source = "(data Point (Point2D Int Int) (Point3D Int Int Int))"
        graph = parse(source)
        
        # Get the root node
        node_id = graph.root_id
        node = graph.get_node(node_id)
        
        self.assertEqual(node.type_name, "Point")
        self.assertEqual(len(node.type_params), 0)
        
        # Check Point2D
        p2d = node.constructors[0]
        self.assertEqual(p2d["name"], "Point2D")
        self.assertEqual(len(p2d["fields"]), 2)
        self.assertEqual(p2d["fields"][0].name, "Int")
        self.assertEqual(p2d["fields"][1].name, "Int")
        
        # Check Point3D
        p3d = node.constructors[1]
        self.assertEqual(p3d["name"], "Point3D")
        self.assertEqual(len(p3d["fields"]), 3)


class TestADTEvaluation(unittest.TestCase):
    
    def setUp(self):
        self.interpreter = Interpreter()
    
    def test_eval_simple_adt(self):
        """Test evaluating a simple ADT creates constructor functions"""
        source = "(data Option a (None) (Some a))"
        result = self.interpreter.eval(source)
        
        # Check that constructors are bound in environment
        none_val = self.interpreter.global_env.lookup("None")
        self.assertIsNotNone(none_val)
        self.assertTrue(callable(none_val.data))
        
        some_val = self.interpreter.global_env.lookup("Some")
        self.assertIsNotNone(some_val)
        self.assertTrue(callable(some_val.data))
        
        # Test using constructors
        none_result = none_val.data()
        self.assertEqual(none_result, ("None",))
        
        some_result = some_val.data(42)
        self.assertEqual(some_result, ("Some", 42))
    
    def test_adt_with_pattern_matching(self):
        """Test using ADT with pattern matching"""
        source = """
        (do
          (data Option a (None) (Some a))
          (let ((opt (Some 42)))
            (match opt
              ((None) 0)
              ((Some x) x))))
        """
        result = self.interpreter.eval(source)
        
        self.assertEqual(result.data, 42)
    
    def test_recursive_adt(self):
        """Test recursive ADT with list example"""
        source = """
        (do
          (data List a (Nil) (Cons a (List a)))
          (let ((lst (Cons 1 (Cons 2 (Cons 3 (Nil))))))
            (match lst
              ((Nil) 0)
              ((Cons head tail) head))))
        """
        result = self.interpreter.eval(source)
        
        self.assertEqual(result.data, 1)
    
    def test_nested_pattern_matching(self):
        """Test nested pattern matching on ADTs"""
        source = """
        (do
          (data List a (Nil) (Cons a (List a)))
          (let ((lst (Cons 1 (Cons 2 (Nil)))))
            (match lst
              ((Nil) 0)
              ((Cons x (Nil)) x)
              ((Cons x (Cons y _)) (+ x y)))))
        """
        result = self.interpreter.eval(source)
        
        self.assertEqual(result.data, 3)  # 1 + 2
    
    def test_adt_constructor_arity_check(self):
        """Test that constructors check arity"""
        source = """
        (do
          (data Option a (None) (Some a))
          (Some))  ; Should fail - Some expects 1 argument
        """
        with self.assertRaises(TypeError) as cm:
            self.interpreter.eval(source)
        
        self.assertIn("expects 1 arguments, got 0", str(cm.exception))


if __name__ == '__main__':
    unittest.main()