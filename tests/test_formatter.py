"""
Tests for ClaudeLang Code Formatter
"""

import unittest
from textwrap import dedent

from src.formatter import ClaudeLangFormatter, format_code, FormatOptions


class TestFormatter(unittest.TestCase):
    """Test code formatting functionality"""
    
    def setUp(self):
        self.formatter = ClaudeLangFormatter()
    
    def test_format_literal(self):
        """Test formatting of literals"""
        # Numbers
        self.assertEqual(format_code('42'), '42\n')
        self.assertEqual(format_code('3.14'), '3.14\n')
        
        # Strings
        self.assertEqual(format_code('"hello"'), '"hello"\n')
        self.assertEqual(format_code('"hello\\"world"'), '"hello\\"world"\n')
        
        # Booleans
        self.assertEqual(format_code('#t'), '#t\n')
        self.assertEqual(format_code('#f'), '#f\n')
        
        # Lists
        self.assertEqual(format_code('[]'), '[]\n')
        self.assertEqual(format_code('[1 2 3]'), '[1 2 3]\n')
    
    def test_format_simple_application(self):
        """Test formatting of simple function applications"""
        self.assertEqual(format_code('(+ 1 2)'), '(+ 1 2)\n')
        self.assertEqual(format_code('(  +   1    2  )'), '(+ 1 2)\n')
        self.assertEqual(format_code('(* (+ 1 2) 3)'), '(* (+ 1 2) 3)\n')
    
    def test_format_lambda(self):
        """Test formatting of lambda expressions"""
        # Simple lambda
        self.assertEqual(
            format_code('(lambda (x) x)'),
            '(lambda (x) x)\n'
        )
        
        # Lambda with multiple params
        self.assertEqual(
            format_code('(lambda (x y) (+ x y))'),
            '(lambda (x y) (+ x y))\n'
        )
        
        # Lambda with complex body
        result = format_code('(lambda (x) (if (> x 0) x (- x)))')
        expected = dedent("""\
            (lambda (x)
              (if (> x 0) x (- x)))
            """)
        self.assertEqual(result, expected)
    
    def test_format_let(self):
        """Test formatting of let expressions"""
        # Simple let
        self.assertEqual(
            format_code('(let ((x 5)) x)'),
            '(let ((x 5)) x)\n'
        )
        
        # Let with multiple bindings
        result = format_code('(let ((x 5) (y 10)) (+ x y))')
        expected = dedent("""\
            (let ((x 5)
                  (y 10))
              (+ x y))
            """)
        self.assertEqual(result, expected)
        
        # Let with aligned bindings
        result = format_code('(let ((x 5) (long-name 10)) (+ x long-name))')
        expected = dedent("""\
            (let ((x         5)
                  (long-name 10))
              (+ x long-name))
            """)
        self.assertEqual(result, expected)
    
    def test_format_if(self):
        """Test formatting of if expressions"""
        # Simple if
        self.assertEqual(
            format_code('(if #t 1 2)'),
            '(if #t 1 2)\n'
        )
        
        # Complex if
        result = format_code('(if (> x 0) (print "positive") (print "negative"))')
        expected = dedent("""\
            (if (> x 0)
              (print "positive")
              (print "negative"))
            """)
        self.assertEqual(result, expected)
    
    def test_format_match(self):
        """Test formatting of match expressions"""
        code = '(match x ((None) 0) ((Some y) y))'
        result = format_code(code)
        expected = dedent("""\
            (match x
              ((None) 0)
              ((Some y) y))
            """)
        self.assertEqual(result, expected)
        
        # Match with complex branches
        code = '(match lst ([] 0) ([x, ... xs] (+ 1 (length xs))))'
        result = format_code(code)
        expected = dedent("""\
            (match lst
              ([] 0)
              ([x, ... xs]
                (+ 1 (length xs))))
            """)
        self.assertEqual(result, expected)
    
    def test_format_data_declaration(self):
        """Test formatting of ADT declarations"""
        code = '(data Option a (None) (Some a))'
        result = format_code(code)
        expected = dedent("""\
            (data Option a
              (None)
              (Some a))
            """)
        self.assertEqual(result, expected)
        
        # More complex ADT
        code = '(data Tree a (Leaf a) (Node (Tree a) a (Tree a)))'
        result = format_code(code)
        expected = dedent("""\
            (data Tree a
              (Leaf a)
              (Node (Tree a) a (Tree a)))
            """)
        self.assertEqual(result, expected)
    
    def test_format_type_ascription(self):
        """Test formatting of type ascriptions"""
        self.assertEqual(
            format_code('(: 42 Int)'),
            '(: 42 Int)\n'
        )
        
        self.assertEqual(
            format_code('(: (lambda (x) x) (Function a a))'),
            '(: (lambda (x) x) (Function a a))\n'
        )
    
    def test_format_contract(self):
        """Test formatting of contracts"""
        code = '(spec:contract add :requires [(number? x) (number? y)] :ensures [(number? result)])'
        result = format_code(code)
        expected = dedent("""\
            (spec:contract add
              :requires [(number? x) (number? y)]
              :ensures [(number? result)])
            """)
        self.assertEqual(result, expected)
    
    def test_format_sequence(self):
        """Test formatting of sequences"""
        # Simple sequence
        self.assertEqual(
            format_code('(do (print 1) (print 2))'),
            '(do (print 1) (print 2))\n'
        )
        
        # Complex sequence
        code = '(do (print "start") (let ((x 5)) (print x)) (print "end"))'
        result = format_code(code)
        expected = dedent("""\
            (do
              (print "start")
              (let ((x 5)) (print x))
              (print "end"))
            """)
        self.assertEqual(result, expected)
    
    def test_format_effect(self):
        """Test formatting of effects"""
        self.assertEqual(
            format_code('(effect io:print "hello")'),
            '(effect io:print "hello")\n'
        )
        
        self.assertEqual(
            format_code('(effect state:set "key" 42)'),
            '(effect state:set "key" 42)\n'
        )
    
    def test_preserve_semantics(self):
        """Test that formatting preserves program semantics"""
        # Complex nested expression
        original = '((lambda (x y) (if (> x y) x y)) 5 3)'
        formatted = format_code(original)
        
        # Parse both to ensure they're equivalent
        from src.parser.sexpr_parser import parse
        original_ast = parse(original)
        formatted_ast = parse(formatted)
        
        # Both should have the same root node type
        self.assertEqual(
            type(original_ast.nodes[original_ast.root_id]),
            type(formatted_ast.nodes[formatted_ast.root_id])
        )
    
    def test_custom_options(self):
        """Test formatting with custom options"""
        options = FormatOptions(
            indent_width=4,
            align_let_bindings=False
        )
        
        code = '(let ((x 5) (y 10)) (+ x y))'
        result = format_code(code, options)
        expected = dedent("""\
            (let ((x 5) (y 10))
                (+ x y))
            """)
        self.assertEqual(result, expected)
    
    def test_long_line_breaking(self):
        """Test breaking of long lines"""
        options = FormatOptions(max_line_length=40)
        
        code = '(function-with-long-name arg1 arg2 arg3 arg4 arg5)'
        result = format_code(code, options)
        # Should break into multiple lines
        self.assertIn('\n  ', result)  # Indented continuation
    
    def test_empty_input(self):
        """Test formatting empty input"""
        self.assertEqual(format_code(''), '')
        self.assertEqual(format_code('   \n  \n'), '')
    
    def test_malformed_input(self):
        """Test handling of malformed input"""
        # Should return original on parse error
        malformed = '(+ 1'
        self.assertEqual(format_code(malformed), malformed)
        
        malformed = '(let ((x 5)'
        self.assertEqual(format_code(malformed), malformed)


class TestFormatterIntegration(unittest.TestCase):
    """Integration tests for formatter"""
    
    def test_format_complex_program(self):
        """Test formatting a complete program"""
        program = dedent("""\
            (data Option a (None) (Some a))
            (define map-option (lambda (f opt) (match opt ((None) (None)) ((Some x) (Some (f x))))))
            (spec:contract map-option :requires [(function? f)] :ensures [(option? result)])
            (let ((x (Some 5))) (map-option (lambda (n) (* n 2)) x))
            """)
        
        result = format_code(program)
        
        # Should have proper structure
        self.assertIn('(data Option a', result)
        self.assertIn('  (None)', result)
        self.assertIn('  (Some a)', result)
        self.assertIn('(match opt', result)
        self.assertIn('(spec:contract map-option', result)
        
        # Should end with newline
        self.assertTrue(result.endswith('\n'))
        
        # Should be parseable
        from src.parser.sexpr_parser import parse
        try:
            parse(result)
        except Exception:
            self.fail("Formatted output is not parseable")
    
    def test_format_preserves_evaluation(self):
        """Test that formatting doesn't change evaluation results"""
        from src.interpreter.interpreter import Interpreter
        
        programs = [
            '(+ 1 2)',
            '(let ((x 5)) (* x x))',
            '((lambda (x y) (+ x y)) 3 4)',
            '(if (> 5 3) "yes" "no")',
        ]
        
        interpreter = Interpreter()
        
        for program in programs:
            # Evaluate original
            original_result = interpreter.eval(program)
            
            # Format and evaluate
            formatted = format_code(program)
            formatted_result = interpreter.eval(formatted)
            
            # Results should be identical
            self.assertEqual(original_result.data, formatted_result.data)


if __name__ == '__main__':
    unittest.main()