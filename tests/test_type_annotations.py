"""
Tests for type annotation syntax in ClaudeLang
"""
import unittest
from src.parser.sexpr_parser import parse
from src.interpreter.interpreter import Interpreter
from src.core.ast import NodeType, TypeAscription, TypeAnnotation


class TestTypeAnnotationParsing(unittest.TestCase):
    """Test parsing of type annotations"""
    
    def test_parse_type_ascription(self):
        """Test parsing type ascription"""
        graph = parse("(: 42 Int)")
        
        # Check root is type ascription
        root = graph.get_node(graph.root_id)
        self.assertIsInstance(root, TypeAscription)
        self.assertEqual(root.node_type, NodeType.TYPE_ASCRIPTION)
        
        # Check ascribed type
        self.assertIsNotNone(root.ascribed_type)
        self.assertEqual(root.ascribed_type.name, "Int")
        self.assertEqual(len(root.ascribed_type.parameters), 0)
        
        # Check expression
        expr = graph.get_node(root.expr_id)
        self.assertEqual(expr.node_type, NodeType.LITERAL)
        self.assertEqual(expr.value, 42)
    
    def test_parse_function_type_ascription(self):
        """Test parsing function type ascription"""
        graph = parse("(: (lambda (x) x) (Function Int Int))")
        
        root = graph.get_node(graph.root_id)
        self.assertIsInstance(root, TypeAscription)
        
        # Check function type
        self.assertEqual(root.ascribed_type.name, "Function")
        self.assertEqual(len(root.ascribed_type.parameters), 2)
        self.assertEqual(root.ascribed_type.parameters[0].name, "Int")
        self.assertEqual(root.ascribed_type.parameters[1].name, "Int")
    
    def test_parse_nested_type_annotation(self):
        """Test parsing nested type annotations"""
        graph = parse("(: [] (List Int))")
        
        root = graph.get_node(graph.root_id)
        self.assertIsInstance(root, TypeAscription)
        
        # Check List type
        self.assertEqual(root.ascribed_type.name, "List")
        self.assertEqual(len(root.ascribed_type.parameters), 1)
        self.assertEqual(root.ascribed_type.parameters[0].name, "Int")
    
    def test_parse_complex_type(self):
        """Test parsing complex type annotations"""
        graph = parse("(: (lambda (x) x) (Function (List a) (List a)))")
        
        root = graph.get_node(graph.root_id)
        func_type = root.ascribed_type
        
        self.assertEqual(func_type.name, "Function")
        self.assertEqual(len(func_type.parameters), 2)
        
        # Check parameter types
        param_type = func_type.parameters[0]
        self.assertEqual(param_type.name, "List")
        self.assertEqual(param_type.parameters[0].name, "a")
        
        return_type = func_type.parameters[1]
        self.assertEqual(return_type.name, "List")
        self.assertEqual(return_type.parameters[0].name, "a")


class TestTypeAnnotationEvaluation(unittest.TestCase):
    """Test evaluation of type annotations"""
    
    def setUp(self):
        self.interpreter = Interpreter()
    
    def test_eval_type_ascription(self):
        """Test evaluating type ascription"""
        result = self.interpreter.eval("(: 42 Int)")
        
        # Value should be unchanged
        self.assertEqual(result.data, 42)
        
        # Type annotation should be set
        self.assertIsNotNone(result.type_info)
        self.assertEqual(result.type_info.name, "Int")
    
    def test_eval_function_type_ascription(self):
        """Test evaluating function with type ascription"""
        result = self.interpreter.eval("(: (lambda (x) x) (Function Int Int))")
        
        # Should get a closure (dict with 'type', 'params', etc.)
        self.assertIsInstance(result.data, dict)
        self.assertEqual(result.data['type'], 'closure')
        self.assertIn('params', result.data)
        self.assertIn('body_id', result.data)
        self.assertIn('env', result.data)
        
        # Type annotation should be the ascribed type
        self.assertIsNotNone(result.type_info)
        self.assertEqual(result.type_info.name, "Function")
        self.assertEqual(len(result.type_info.parameters), 2)
    
    def test_type_ascription_preserves_behavior(self):
        """Test that type ascription doesn't affect behavior"""
        # Without type ascription
        result1 = self.interpreter.eval("((lambda (x) (+ x 1)) 5)")
        
        # With type ascription
        result2 = self.interpreter.eval("((: (lambda (x) (+ x 1)) (Function Int Int)) 5)")
        
        # Both should give same result
        self.assertEqual(result1.data, result2.data)
        self.assertEqual(result1.data, 6)
    
    def test_nested_type_ascriptions(self):
        """Test nested type ascriptions"""
        result = self.interpreter.eval("(: (: 42 Number) Int)")
        
        # Value should be unchanged
        self.assertEqual(result.data, 42)
        
        # Outer type should win
        self.assertEqual(result.type_info.name, "Int")


class TestLetBindingTypeAnnotations(unittest.TestCase):
    """Test type annotations in let bindings"""
    
    def setUp(self):
        self.interpreter = Interpreter()
    
    def test_simple_let_type_annotation(self):
        """Test simple type annotation in let binding"""
        # For now, we'll use type ascription in let bindings
        result = self.interpreter.eval("""
            (let ((x (: 42 Int)))
              x)
        """)
        
        self.assertEqual(result.data, 42)
    
    def test_multiple_let_annotations(self):
        """Test multiple type annotations in let"""
        result = self.interpreter.eval("""
            (let ((x (: 42 Int))
                  (y (: 3.14 Float)))
              [x y])
        """)
        
        self.assertEqual(result.data, [42, 3.14])


class TestLambdaParameterAnnotations(unittest.TestCase):
    """Test type annotations on lambda parameters"""
    
    def setUp(self):
        self.interpreter = Interpreter()
    
    def test_annotated_lambda(self):
        """Test lambda with annotated parameters using type ascription"""
        # Since we don't have special syntax for parameter annotations yet,
        # we can annotate the whole lambda
        result = self.interpreter.eval("""
            (let ((add (: (lambda (x y) (+ x y)) 
                         (Function Int Int Int))))
              (add 5 7))
        """)
        
        self.assertEqual(result.data, 12)


if __name__ == '__main__':
    unittest.main()