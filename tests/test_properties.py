"""
Property-based tests for ClaudeLang using Hypothesis

This module uses property-based testing to verify invariants and properties
of the ClaudeLang implementation that should hold for all valid inputs.
"""
import unittest
from hypothesis import given, strategies as st, assume, settings, example
from hypothesis.strategies import composite
import string

from src.parser.sexpr_parser import parse, Lexer
from src.interpreter.interpreter import Interpreter
from src.core.ast import NodeType, Literal, Application, Lambda, Let
from src.errors.exceptions import ClaudeLangError


# Custom strategies for generating ClaudeLang expressions
@composite
def literals(draw):
    """Generate literal values"""
    return draw(st.one_of(
        st.integers(min_value=-1000, max_value=1000),
        st.floats(min_value=-1000, max_value=1000, allow_nan=False, allow_infinity=False),
        st.text(alphabet=string.printable, min_size=0, max_size=20),
        st.booleans(),
        st.lists(st.integers(min_value=-100, max_value=100), min_size=0, max_size=5)
    ))


@composite
def identifiers(draw):
    """Generate valid identifiers"""
    # Start with a letter or allowed special char
    first = draw(st.sampled_from(string.ascii_letters + "_+-*/<>=!?"))
    # Rest can include digits
    rest = draw(st.text(alphabet=string.ascii_letters + string.digits + "_+-*/<>=!?", 
                       min_size=0, max_size=10))
    return first + rest


@composite
def simple_expressions(draw):
    """Generate simple S-expressions"""
    expr_type = draw(st.sampled_from(['literal', 'variable', 'arithmetic', 'list']))
    
    if expr_type == 'literal':
        lit = draw(literals())
        if isinstance(lit, str):
            return f'"{lit}"'
        elif isinstance(lit, bool):
            return '#t' if lit else '#f'
        elif isinstance(lit, list):
            return f"[{' '.join(str(x) for x in lit)}]"
        else:
            return str(lit)
    
    elif expr_type == 'variable':
        return draw(identifiers())
    
    elif expr_type == 'arithmetic':
        op = draw(st.sampled_from(['+', '-', '*', '/']))
        left = draw(st.integers(min_value=-100, max_value=100))
        right = draw(st.integers(min_value=1, max_value=100))  # Avoid division by zero
        return f"({op} {left} {right})"
    
    else:  # list
        elements = draw(st.lists(st.integers(min_value=-100, max_value=100), 
                                min_size=0, max_size=5))
        return f"[{' '.join(str(x) for x in elements)}]"


@composite 
def lambda_expressions(draw):
    """Generate lambda expressions"""
    params = draw(st.lists(identifiers(), min_size=1, max_size=3, unique=True))
    body = draw(simple_expressions())
    param_list = ' '.join(params)
    return f"(lambda ({param_list}) {body})"


@composite
def let_expressions(draw):
    """Generate let expressions"""
    num_bindings = draw(st.integers(min_value=1, max_value=3))
    bindings = []
    
    for _ in range(num_bindings):
        var = draw(identifiers())
        val = draw(simple_expressions())
        bindings.append(f"({var} {val})")
    
    body = draw(simple_expressions())
    bindings_str = ' '.join(bindings)
    return f"(let ({bindings_str}) {body})"


class TestParserProperties(unittest.TestCase):
    """Property-based tests for the parser"""
    
    @given(simple_expressions())
    def test_parse_does_not_crash(self, expr):
        """Parser should not crash on any valid expression"""
        try:
            graph = parse(expr)
            # Basic sanity checks
            self.assertIsNotNone(graph)
            self.assertIsNotNone(graph.root_id)
            self.assertIn(graph.root_id, graph.nodes)
        except SyntaxError:
            # Syntax errors are expected for malformed input
            pass
        except Exception as e:
            # Any other exception is a bug
            self.fail(f"Parser crashed on {repr(expr)}: {type(e).__name__}: {e}")
    
    @given(literals())
    def test_literal_roundtrip(self, value):
        """Parsing and evaluating a literal should return the original value"""
        interpreter = Interpreter()
        
        # Format the literal for parsing
        if isinstance(value, str):
            # Properly escape the string
            escaped = value.replace('\\', '\\\\').replace('"', '\\"')
            expr = f'"{escaped}"'
        elif isinstance(value, bool):
            expr = '#t' if value else '#f'
        elif isinstance(value, list):
            expr = f"[{' '.join(str(x) for x in value)}]"
        else:
            expr = str(value)
        
        try:
            result = interpreter.eval(expr)
            
            # Handle floating point comparison
            if isinstance(value, float) and isinstance(result.data, float):
                self.assertAlmostEqual(value, result.data, places=5)
            else:
                self.assertEqual(value, result.data)
        except ClaudeLangError as e:
            # If parsing failed, that's a bug we should know about
            self.fail(f"Failed to parse/eval literal {repr(value)}: {e}")
    
    @given(st.text(alphabet=string.printable, min_size=0, max_size=100))
    def test_string_escaping(self, text):
        """String literals should handle escaping correctly"""
        interpreter = Interpreter()
        
        # Instead of skipping, properly escape the string
        escaped = text.replace('\\', '\\\\').replace('"', '\\"')
        expr = f'"{escaped}"'
        
        try:
            result = interpreter.eval(expr)
            # The result should be a string
            self.assertIsInstance(result.data, str)
            # And it should match the original text
            self.assertEqual(result.data, text)
        except Exception as e:
            # Any failure is a bug in our string handling
            self.fail(f"Failed to handle string {repr(text)}: {type(e).__name__}: {e}")


class TestInterpreterProperties(unittest.TestCase):
    """Property-based tests for the interpreter"""
    
    def setUp(self):
        self.interpreter = Interpreter()
    
    @given(st.integers(), st.integers())
    def test_addition_commutative(self, a, b):
        """Addition should be commutative"""
        result1 = self.interpreter.eval(f"(+ {a} {b})")
        result2 = self.interpreter.eval(f"(+ {b} {a})")
        self.assertEqual(result1.data, result2.data)
    
    @given(st.integers(), st.integers(), st.integers())
    def test_addition_associative(self, a, b, c):
        """Addition should be associative"""
        result1 = self.interpreter.eval(f"(+ (+ {a} {b}) {c})")
        result2 = self.interpreter.eval(f"(+ {a} (+ {b} {c}))")
        self.assertEqual(result1.data, result2.data)
    
    @given(st.integers())
    def test_identity_function(self, x):
        """Identity function should return its input"""
        result = self.interpreter.eval(f"((lambda (x) x) {x})")
        self.assertEqual(result.data, x)
    
    @given(st.integers(min_value=-100, max_value=100))
    def test_if_expression_exhaustive(self, x):
        """If expressions should handle both branches"""
        result = self.interpreter.eval(
            f'(if (> {x} 0) "positive" (if (< {x} 0) "negative" "zero"))'
        )
        
        if x > 0:
            self.assertEqual(result.data, "positive")
        elif x < 0:
            self.assertEqual(result.data, "negative")
        else:
            self.assertEqual(result.data, "zero")
    
    @given(st.lists(st.integers(), min_size=0, max_size=10))
    def test_list_construction(self, lst):
        """List construction should preserve elements"""
        list_expr = f"[{' '.join(str(x) for x in lst)}]"
        result = self.interpreter.eval(list_expr)
        self.assertEqual(result.data, lst)
    
    @given(st.lists(st.integers(), min_size=1, max_size=10))
    def test_head_tail_properties(self, lst):
        """head should return first element, tail should return rest"""
        list_expr = f"[{' '.join(str(x) for x in lst)}]"
        
        # Test head
        head = self.interpreter.eval(f"(head {list_expr})").data
        self.assertEqual(head, lst[0])
        
        # Test tail
        tail = self.interpreter.eval(f"(tail {list_expr})").data
        self.assertEqual(tail, lst[1:])
    
    def test_head_tail_empty_list_errors(self):
        """head and tail should handle empty lists appropriately"""
        # Test head on empty list
        head_result = self.interpreter.eval("(head [])")
        self.assertIsInstance(head_result.data, dict)
        self.assertIn('error', head_result.data)
        
        # Test tail on empty list
        tail_result = self.interpreter.eval("(tail [])")
        self.assertIsInstance(tail_result.data, dict)
        self.assertIn('error', tail_result.data)


class TestEffectProperties(unittest.TestCase):
    """Property-based tests for effects system"""
    
    def setUp(self):
        self.interpreter = Interpreter()
    
    @given(st.integers())
    def test_pure_functions_deterministic(self, x):
        """Pure functions should always return the same result"""
        expr = f"(* {x} 2)"
        
        result1 = self.interpreter.eval(expr)
        result2 = self.interpreter.eval(expr)
        
        self.assertEqual(result1.data, result2.data)
    
    @given(st.integers())
    def test_state_effects_are_isolated(self, value):
        """State effects should be isolated between evaluations"""
        # Set state in first evaluation
        self.interpreter.eval(f"""
            (effect state:set "test-key" {value})
        """)
        
        # Create a new interpreter for isolation
        new_interpreter = Interpreter()
        
        # The new interpreter should not see the state
        result = new_interpreter.eval("""
            (effect state:get "test-key")
        """)
        
        # Should return None or error, not the value we set
        self.assertNotEqual(result.data, value)


class TestPatternMatchingProperties(unittest.TestCase):
    """Property-based tests for pattern matching"""
    
    def setUp(self):
        self.interpreter = Interpreter()
    
    @given(st.integers())
    def test_pattern_variable_binding(self, value):
        """Pattern variables should bind correctly"""
        result = self.interpreter.eval(f"""
            (match {value}
              (x x))
        """)
        self.assertEqual(result.data, value)
    
    @given(st.lists(st.integers(), min_size=0, max_size=5))
    def test_list_pattern_matching(self, lst):
        """List patterns should destructure correctly"""
        list_expr = f"[{' '.join(str(x) for x in lst)}]"
        
        if lst:
            # Non-empty list should match the first pattern
            result = self.interpreter.eval(f"""
                (match {list_expr}
                  ([x, ... xs] x)
                  ([] "empty"))
            """)
            self.assertEqual(result.data, lst[0])
        else:
            # Empty list should match the empty pattern
            result = self.interpreter.eval(f"""
                (match {list_expr}
                  ([x, ... xs] x)
                  ([] "empty"))
            """)
            self.assertEqual(result.data, "empty")
    
    @given(st.one_of(st.integers(), st.text(), st.booleans()))
    def test_wildcard_pattern_matches_all(self, value):
        """Wildcard pattern should match any value including empty strings"""
        # Format the value
        if isinstance(value, str):
            escaped = value.replace('\\', '\\\\').replace('"', '\\"')
            expr = f'"{escaped}"'
        elif isinstance(value, bool):
            expr = '#t' if value else '#f'
        else:
            expr = str(value)
        
        result = self.interpreter.eval(f"""
            (match {expr}
              (_ "matched"))
        """)
        
        self.assertEqual(result.data, "matched")


class TestADTProperties(unittest.TestCase):
    """Property-based tests for algebraic data types"""
    
    def setUp(self):
        self.interpreter = Interpreter()
        # Define Option type
        self.interpreter.eval("(data Option a (None) (Some a))")
    
    @given(st.integers())
    def test_adt_constructor_preserves_value(self, value):
        """ADT constructors should preserve their arguments"""
        result = self.interpreter.eval(f"(Some {value})")
        
        # Result should be a tagged tuple
        self.assertEqual(result.data[0], "Some")
        self.assertEqual(result.data[1], value)
    
    @given(st.integers())
    def test_adt_pattern_matching_extraction(self, value):
        """Pattern matching on ADTs should extract values correctly"""
        result = self.interpreter.eval(f"""
            (match (Some {value})
              ((None) 0)
              ((Some x) x))
        """)
        
        self.assertEqual(result.data, value)
    
    def test_adt_exhaustive_matching(self):
        """ADT pattern matching can be exhaustive"""
        # Test with None
        result1 = self.interpreter.eval("""
            (match (None)
              ((None) "none")
              ((Some x) "some"))
        """)
        self.assertEqual(result1.data, "none")
        
        # Test with Some
        result2 = self.interpreter.eval("""
            (match (Some 42)
              ((None) "none")  
              ((Some x) "some"))
        """)
        self.assertEqual(result2.data, "some")


class TestTypeAnnotationProperties(unittest.TestCase):
    """Property-based tests for type annotations"""
    
    def setUp(self):
        self.interpreter = Interpreter()
    
    @given(st.integers())
    def test_type_ascription_preserves_value(self, value):
        """Type ascription should not change the value"""
        result_untyped = self.interpreter.eval(str(value))
        result_typed = self.interpreter.eval(f"(: {value} Int)")
        
        self.assertEqual(result_untyped.data, result_typed.data)
    
    @given(st.one_of(st.integers(), st.text(min_size=1, max_size=10), st.booleans()))
    def test_type_ascription_does_not_crash(self, value):
        """Type ascription should handle any value"""
        # Format the value properly
        if isinstance(value, str):
            escaped = value.replace('\\', '\\\\').replace('"', '\\"')
            expr = f'"{escaped}"'
        elif isinstance(value, bool):
            expr = '#t' if value else '#f'
        else:
            expr = str(value)
        
        # Type ascription should work for any value
        result = self.interpreter.eval(f"(: {expr} Any)")
        
        # The value should be unchanged
        self.assertEqual(result.data, value)


# Configure hypothesis settings for CI
settings.register_profile("ci", max_examples=50, deadline=5000)
settings.register_profile("dev", max_examples=10, deadline=2000)
settings.register_profile("debug", max_examples=5, deadline=None)


if __name__ == '__main__':
    # Use CI profile by default
    settings.load_profile("ci")
    unittest.main()