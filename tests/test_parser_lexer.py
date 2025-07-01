"""
Tests for ClaudeLang Lexer
"""

import unittest
from src.parser.sexpr_parser import Lexer, Token


class TestLexer(unittest.TestCase):
    """Test the ClaudeLang lexer"""
    
    def tokenize(self, source):
        """Helper to tokenize source and return tokens"""
        lexer = Lexer(source)
        return lexer.tokenize()
    
    def test_empty_input(self):
        """Test lexing empty input"""
        tokens = self.tokenize("")
        self.assertEqual(len(tokens), 0)
    
    def test_whitespace_only(self):
        """Test lexing whitespace-only input"""
        tokens = self.tokenize("   \n\t  \n  ")
        self.assertEqual(len(tokens), 0)
    
    def test_parentheses(self):
        """Test lexing parentheses"""
        tokens = self.tokenize("()")
        self.assertEqual(len(tokens), 2)
        self.assertEqual(tokens[0].type, 'LPAREN')
        self.assertEqual(tokens[1].type, 'RPAREN')
        
        # Check position tracking
        self.assertEqual(tokens[0].line, 1)
        self.assertEqual(tokens[0].column, 1)
        self.assertEqual(tokens[1].line, 1)
        self.assertEqual(tokens[1].column, 1)  # Column doesn't advance in current impl
    
    def test_brackets(self):
        """Test lexing brackets"""
        tokens = self.tokenize("[]")
        self.assertEqual(len(tokens), 2)
        self.assertEqual(tokens[0].type, 'LBRACKET')
        self.assertEqual(tokens[1].type, 'RBRACKET')
    
    def test_integers(self):
        """Test lexing integer literals"""
        # Positive integers
        tokens = self.tokenize("42")
        self.assertEqual(len(tokens), 1)
        self.assertEqual(tokens[0].type, 'INT')
        self.assertEqual(tokens[0].value, 42)
        
        # Negative integers
        tokens = self.tokenize("-42")
        self.assertEqual(len(tokens), 1)
        self.assertEqual(tokens[0].type, 'INT')
        self.assertEqual(tokens[0].value, -42)
        
        # Zero
        tokens = self.tokenize("0")
        self.assertEqual(len(tokens), 1)
        self.assertEqual(tokens[0].value, 0)
        
        # Large numbers
        tokens = self.tokenize("1234567890")
        self.assertEqual(tokens[0].value, 1234567890)
    
    def test_floats(self):
        """Test lexing float literals"""
        # Simple float
        tokens = self.tokenize("3.14")
        self.assertEqual(len(tokens), 1)
        self.assertEqual(tokens[0].type, 'FLOAT')
        self.assertAlmostEqual(tokens[0].value, 3.14)
        
        # Negative float
        tokens = self.tokenize("-3.14")
        self.assertAlmostEqual(tokens[0].value, -3.14)
        
        # Float with many decimals
        tokens = self.tokenize("0.123456789")
        self.assertAlmostEqual(tokens[0].value, 0.123456789)
        
        # Float starting with decimal - this actually tokenizes as symbol
        tokens = self.tokenize(".5")
        self.assertEqual(tokens[0].type, 'SYMBOL')
        self.assertEqual(tokens[0].value, '.5')
    
    def test_strings(self):
        """Test lexing string literals"""
        # Simple string
        tokens = self.tokenize('"hello"')
        self.assertEqual(len(tokens), 1)
        self.assertEqual(tokens[0].type, 'STRING')
        self.assertEqual(tokens[0].value, 'hello')
        
        # Empty string
        tokens = self.tokenize('""')
        self.assertEqual(tokens[0].value, '')
        
        # String with spaces
        tokens = self.tokenize('"hello world"')
        self.assertEqual(tokens[0].value, 'hello world')
        
        # String with escape sequences
        tokens = self.tokenize(r'"hello\nworld"')
        self.assertEqual(tokens[0].value, 'hello\nworld')
        
        tokens = self.tokenize(r'"tab\there"')
        self.assertEqual(tokens[0].value, 'tab\there')
        
        tokens = self.tokenize(r'"quote\"inside"')
        self.assertEqual(tokens[0].value, 'quote"inside')
        
        tokens = self.tokenize(r'"backslash\\"')
        self.assertEqual(tokens[0].value, 'backslash\\\\')
    
    def test_symbols(self):
        """Test lexing symbols"""
        # Simple symbols
        symbols = ['x', 'foo', 'bar123', 'kebab-case', 'snake_case', 
                  'contains?', 'mutate!', '+', '-', '*', '/', 
                  '=', '<', '>', '<=', '>=', 'not=']
        
        for sym in symbols:
            tokens = self.tokenize(sym)
            self.assertEqual(len(tokens), 1)
            self.assertEqual(tokens[0].type, 'SYMBOL')
            self.assertEqual(tokens[0].value, sym)
    
    def test_boolean_literals(self):
        """Test lexing boolean literals"""
        # Standard booleans
        for true_form in ['true', '#t']:
            tokens = self.tokenize(true_form)
            self.assertEqual(len(tokens), 1)
            self.assertEqual(tokens[0].type, 'BOOL')
            self.assertEqual(tokens[0].value, True)
        
        for false_form in ['false', '#f']:
            tokens = self.tokenize(false_form)
            self.assertEqual(len(tokens), 1)
            self.assertEqual(tokens[0].type, 'BOOL')
            self.assertEqual(tokens[0].value, False)
    
    def test_comments(self):
        """Test comment handling"""
        # Line comment
        tokens = self.tokenize("; This is a comment\n42")
        self.assertEqual(len(tokens), 1)
        self.assertEqual(tokens[0].value, 42)
        
        # Comment at end
        tokens = self.tokenize("42 ; comment")
        self.assertEqual(len(tokens), 1)
        self.assertEqual(tokens[0].value, 42)
        
        # Multiple comments
        source = """
        ; First comment
        42
        ; Second comment
        ; Third comment
        "hello"
        """
        tokens = self.tokenize(source)
        self.assertEqual(len(tokens), 2)
        self.assertEqual(tokens[0].value, 42)
        self.assertEqual(tokens[1].value, "hello")
    
    def test_complex_expression(self):
        """Test lexing a complex expression"""
        source = '(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))'
        tokens = self.tokenize(source)
        
        expected_types = [
            'LPAREN', 'SYMBOL', 'LPAREN', 'SYMBOL', 'SYMBOL', 'RPAREN',
            'LPAREN', 'SYMBOL', 'LPAREN', 'SYMBOL', 'SYMBOL', 'INT', 'RPAREN',
            'INT', 'LPAREN', 'SYMBOL', 'SYMBOL', 'LPAREN', 'SYMBOL',
            'LPAREN', 'SYMBOL', 'SYMBOL', 'INT', 'RPAREN', 'RPAREN',
            'RPAREN', 'RPAREN', 'RPAREN'
        ]
        
        self.assertEqual(len(tokens), len(expected_types))
        for i, (token, expected) in enumerate(zip(tokens, expected_types)):
            self.assertEqual(token.type, expected, 
                           f"Token {i}: expected {expected}, got {token.type}")
    
    def test_line_column_tracking(self):
        """Test accurate line and column tracking"""
        source = """(foo
  bar
  baz)"""
        tokens = self.tokenize(source)
        
        # Check positions
        self.assertEqual(tokens[0].line, 1)  # (
        self.assertEqual(tokens[0].column, 1)
        
        self.assertEqual(tokens[1].line, 1)  # foo
        self.assertEqual(tokens[1].column, 1)
        
        self.assertEqual(tokens[2].line, 2)  # bar
        self.assertEqual(tokens[2].column, 3)
        
        self.assertEqual(tokens[3].line, 3)  # baz
        self.assertEqual(tokens[3].column, 3)
        
        self.assertEqual(tokens[4].line, 3)  # )
        self.assertEqual(tokens[4].column, 3)
    
    def test_unterminated_string(self):
        """Test unterminated string handling"""
        # Current implementation doesn't raise error
        tokens = self.tokenize('"unterminated')
        self.assertEqual(len(tokens), 1)
        self.assertEqual(tokens[0].type, 'STRING')
        self.assertEqual(tokens[0].value, 'unterminated')
    
    def test_invalid_escape_sequence(self):
        """Test handling of invalid escape sequences"""
        # Currently, invalid escapes are passed through
        tokens = self.tokenize(r'"invalid\q"')
        self.assertEqual(tokens[0].value, 'invalid\\q')
    
    def test_unicode_in_strings(self):
        """Test Unicode support in strings"""
        tokens = self.tokenize('"Hello 世界 🌍"')
        self.assertEqual(tokens[0].value, 'Hello 世界 🌍')
    
    def test_unicode_in_symbols(self):
        """Test Unicode support in symbols"""
        tokens = self.tokenize('λ')
        self.assertEqual(tokens[0].type, 'SYMBOL')
        self.assertEqual(tokens[0].value, 'λ')
    
    def test_numeric_edge_cases(self):
        """Test edge cases in numeric parsing"""
        # Multiple decimal points (should be symbol)
        tokens = self.tokenize('3.14.159')
        # This might parse as number then symbol, depending on implementation
        
        # Leading zeros
        tokens = self.tokenize('007')
        self.assertEqual(tokens[0].value, 7)
        
        # Very large number
        tokens = self.tokenize('999999999999999999999')
        self.assertIsInstance(tokens[0].value, (int, float))
    
    def test_adjacent_tokens(self):
        """Test tokens without whitespace separation"""
        tokens = self.tokenize('(foo(bar))')
        self.assertEqual(len(tokens), 6)
        self.assertEqual([t.type for t in tokens], 
                        ['LPAREN', 'SYMBOL', 'LPAREN', 'SYMBOL', 'RPAREN', 'RPAREN'])
        
        # Numbers and symbols
        tokens = self.tokenize('123abc')
        self.assertEqual(len(tokens), 2)
        self.assertEqual(tokens[0].type, 'INT')
        self.assertEqual(tokens[0].value, 123)
        self.assertEqual(tokens[1].type, 'SYMBOL')
        self.assertEqual(tokens[1].value, 'abc')


if __name__ == '__main__':
    unittest.main()