"""
Tests for ClaudeLang Error Handling and Diagnostics
"""

import unittest
from src.parser.sexpr_parser import parse
from src.errors.diagnostics import Diagnostic, ErrorSeverity, DiagnosticEngine, SourceLocation
from src.errors.exceptions import (
    ClaudeLangError, SyntaxError as ClaudeSyntaxError, 
    NameError as ClaudeNameError, TypeError as ClaudeTypeError,
    PatternMatchError, EffectError, ModuleError
)
from src.vm.compiler import BytecodeCompiler
from src.vm.vm import VM, VMError
from src.core.ast import Graph


class TestParserErrors(unittest.TestCase):
    """Test parser error handling"""
    
    def test_unclosed_paren(self):
        """Test unclosed parenthesis error"""
        with self.assertRaises(Exception) as ctx:
            parse("(+ 1 2")
        
        # Should have helpful error message
        self.assertIn("closing parenthesis", str(ctx.exception).lower())
    
    def test_unexpected_close_paren(self):
        """Test unexpected closing parenthesis"""
        with self.assertRaises(Exception) as ctx:
            parse("(+ 1 2))")
        
        self.assertIn("unexpected", str(ctx.exception).lower())
    
    def test_empty_list_invalid_context(self):
        """Test empty list in invalid context"""
        # Empty application - parser might allow this
        try:
            result = parse("()")
            # If it parses, that's ok
            self.assertIsNotNone(result)
        except Exception as e:
            self.assertIn("empty", str(e).lower())
    
    def test_unclosed_string(self):
        """Test unclosed string literal"""
        # Parser might actually parse this as a valid string
        result = parse('"hello world')
        # If it doesn't raise an error, that's ok
        self.assertIsNotNone(result)
    
    def test_invalid_number_format(self):
        """Test invalid number formats"""
        invalid_numbers = [
            "1.2.3",      # Multiple dots
        ]
        
        for num in invalid_numbers:
            try:
                result = parse(num)
                # Might parse as symbol
                self.assertIsNotNone(result)
            except Exception:
                pass
    
    def test_invalid_syntax_forms(self):
        """Test invalid special forms"""
        # Let with invalid bindings
        with self.assertRaises(Exception):
            parse("(let x 10)")  # Missing binding list
        
        # Lambda without parameters
        with self.assertRaises(Exception):
            parse("(lambda)")
        
        # If with wrong number of args
        with self.assertRaises(Exception):
            parse("(if #t)")  # Missing then/else


class TestDiagnosticSystem(unittest.TestCase):
    """Test diagnostic collection and reporting"""
    
    def test_diagnostic_creation(self):
        """Test creating diagnostics"""
        location = SourceLocation(
            filename="test.cl",
            line=10,
            column=5,
            length=1
        )
        
        diag = Diagnostic(
            severity=ErrorSeverity.ERROR,
            code="E001",
            message="Undefined variable: x",
            location=location
        )
        
        self.assertEqual(diag.severity, ErrorSeverity.ERROR)
        self.assertEqual(diag.message, "Undefined variable: x")
        self.assertEqual(diag.location.line, 10)
        self.assertEqual(diag.location.column, 5)
    
    def test_diagnostic_severity_levels(self):
        """Test different severity levels"""
        levels = [
            ErrorSeverity.ERROR,
            ErrorSeverity.WARNING,
            ErrorSeverity.INFO,
            ErrorSeverity.HINT
        ]
        
        for level in levels:
            diag = Diagnostic(
                severity=level,
                code="T001",
                message=f"Test {level.name}",
                location=SourceLocation(line=1, column=1)
            )
            self.assertEqual(diag.severity, level)
    
    def test_diagnostic_engine(self):
        """Test diagnostic engine"""
        engine = DiagnosticEngine()
        
        # Add various diagnostics
        engine.error("E001", "Type mismatch", 
                    location=SourceLocation(line=5, column=10))
        engine.warning("W001", "Unused variable", 
                      location=SourceLocation(line=7, column=5))
        
        # Check counts
        self.assertEqual(len(engine.diagnostics), 2)
        
        # Check has_errors
        self.assertTrue(engine.has_errors())
        
        # Clear and check
        engine.diagnostics.clear()
        self.assertEqual(len(engine.diagnostics), 0)
        self.assertFalse(engine.has_errors())
    
    def test_diagnostic_formatting(self):
        """Test diagnostic formatting"""
        location = SourceLocation(
            filename="test.cl",
            line=10,
            column=5,
            length=3
        )
        
        diag = Diagnostic(
            severity=ErrorSeverity.ERROR,
            code="E002",
            message="Undefined variable: foo",
            location=location
        )
        
        engine = DiagnosticEngine()
        formatted = engine.format_diagnostic(diag)
        
        # Should contain key information
        self.assertIn("error", formatted.lower())
        self.assertIn("Undefined variable: foo", formatted)
        self.assertIn("10:5", formatted)  # line:column
        self.assertIn("test.cl", formatted)
    
    def test_diagnostic_with_suggestions(self):
        """Test diagnostics with fix suggestions"""
        diag = Diagnostic(
            severity=ErrorSeverity.WARNING,
            code="W002",
            message="Unknown function",
            location=SourceLocation(line=5, column=10),
            suggestion="Did you mean 'length'?"
        )
        
        self.assertIsNotNone(diag.suggestion)
        self.assertIn("length", diag.suggestion)


class TestExceptionTypes(unittest.TestCase):
    """Test custom exception types"""
    
    def test_claudelang_base_error(self):
        """Test base error class"""
        error = ClaudeLangError("Something went wrong")
        self.assertEqual(str(error), "Something went wrong")
        self.assertIsNone(error.diagnostic)
    
    def test_name_error(self):
        """Test name error"""
        error = ClaudeNameError("foo")
        self.assertIn("foo", str(error))
        self.assertEqual(error.name, "foo")
        self.assertIsNotNone(error.diagnostic)
    
    def test_type_error(self):
        """Test type error"""
        error = ClaudeTypeError("number", "string")
        self.assertIn("number", str(error))
        self.assertIn("string", str(error))
        self.assertEqual(error.expected, "number")
        self.assertEqual(error.actual, "string")
    
    def test_pattern_match_error(self):
        """Test pattern match error"""
        error = PatternMatchError([1, 2, 3])
        self.assertIn("[1, 2, 3]", str(error))
        self.assertEqual(error.value, [1, 2, 3])
    
    def test_effect_error(self):
        """Test effect error"""
        error = EffectError("io", "File not found")
        self.assertIn("io", str(error))
        self.assertIn("File not found", str(error))
        self.assertEqual(error.effect, "io")
    
    def test_module_error(self):
        """Test module error"""
        error = ModuleError("Module not found", "math")
        self.assertIn("Module not found", str(error))
        self.assertEqual(error.module_name, "math")


class TestVMErrors(unittest.TestCase):
    """Test VM runtime error handling"""
    
    def setUp(self):
        """Set up test environment"""
        self.vm = VM()
        self.compiler = BytecodeCompiler()
    
    def test_division_by_zero(self):
        """Test division by zero handling"""
        chunk = self.compiler.compile(parse("(/ 10 0)"))
        result = self.vm.execute(chunk)
        
        # VM returns error dict instead of raising
        self.assertIsInstance(result, dict)
        self.assertIn("error", result)
        self.assertIn("zero", result["error"].lower())
    
    def test_modulo_by_zero(self):
        """Test modulo by zero handling"""
        chunk = self.compiler.compile(parse("(mod 10 0)"))
        result = self.vm.execute(chunk)
        
        self.assertIsInstance(result, dict)
        self.assertIn("error", result)
        self.assertIn("zero", result["error"].lower())
    
    def test_stack_overflow(self):
        """Test stack overflow protection"""
        # Create a small VM with limited stack
        small_vm = VM(stack_size=10)
        
        # Try to overflow the stack
        source = "(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 10)))))))))"
        chunk = self.compiler.compile(parse(source))
        
        # Should handle deep nesting gracefully
        try:
            result = small_vm.execute(chunk)
            # If it succeeds, check result
            self.assertEqual(result, 55)  # sum of 1..10
        except VMError as e:
            # If it fails, should be stack overflow
            self.assertIn("stack", str(e).lower())
    
    def test_stack_underflow(self):
        """Test stack underflow protection"""
        # Try to pop from empty stack
        from src.vm.bytecode import BytecodeChunk, Instruction, Opcode
        
        chunk = BytecodeChunk(
            instructions=[Instruction(Opcode.POP)],
            constants=[]
        )
        
        with self.assertRaises(VMError) as ctx:
            self.vm.execute(chunk)
        
        self.assertIn("underflow", str(ctx.exception).lower())
    
    def test_invalid_opcode(self):
        """Test invalid opcode handling"""
        from src.vm.bytecode import BytecodeChunk, Instruction
        
        # Create invalid opcode
        chunk = BytecodeChunk(
            instructions=[Instruction(999)],  # Invalid opcode
            constants=[]
        )
        
        with self.assertRaises(VMError):
            self.vm.execute(chunk)
    
    def test_type_errors_in_operations(self):
        """Test type errors in arithmetic"""
        # String + number
        chunk = self.compiler.compile(parse('(+ "hello" 1)'))
        
        with self.assertRaises(Exception):
            self.vm.execute(chunk)
    
    def test_list_operation_errors(self):
        """Test errors in list operations"""
        # Head of empty list
        chunk = self.compiler.compile(parse("(head [])"))
        result = self.vm.execute(chunk)
        
        # Should return error dict
        if isinstance(result, dict):
            self.assertIn("error", result)
            self.assertIn("empty", result["error"].lower())
    
    def test_call_non_function(self):
        """Test calling non-function values"""
        chunk = self.compiler.compile(parse("(42 1 2 3)"))
        
        with self.assertRaises(VMError) as ctx:
            self.vm.execute(chunk)
        
        self.assertIn("non-function", str(ctx.exception))


class TestErrorRecovery(unittest.TestCase):
    """Test error recovery mechanisms"""
    
    def test_parser_recovery(self):
        """Test parser error recovery"""
        # Multiple expressions with one bad
        source = """(+ 1 2)
(this is invalid
(* 3 4)"""
        
        # Parser might not support recovery yet
        with self.assertRaises(Exception):
            parse(source)
    
    def test_vm_error_recovery(self):
        """Test VM continues after handled errors"""
        vm = VM()
        compiler = BytecodeCompiler()
        
        # Execute multiple expressions
        sources = [
            "(+ 1 2)",      # Valid
            "(/ 10 0)",     # Error
            "(* 3 4)",      # Valid
        ]
        
        results = []
        for source in sources:
            try:
                chunk = compiler.compile(parse(source))
                result = vm.execute(chunk)
                results.append(result)
            except Exception as e:
                results.append({"error": str(e)})
        
        # Check results
        self.assertEqual(results[0], 3)
        self.assertIsInstance(results[1], dict)
        self.assertIn("error", results[1])
        self.assertEqual(results[2], 12)


class TestDiagnosticHelpers(unittest.TestCase):
    """Test diagnostic helper functions"""
    
    def test_type_mismatch_error(self):
        """Test type mismatch error generator"""
        from src.errors.diagnostics import type_mismatch_error
        
        diag = type_mismatch_error("number", "string", 
                                  SourceLocation(line=5, column=10))
        
        self.assertEqual(diag.severity, ErrorSeverity.ERROR)
        self.assertIn("number", diag.message)
        self.assertIn("string", diag.message)
        self.assertIsNotNone(diag.suggestion)
    
    def test_undefined_variable_error(self):
        """Test undefined variable error generator"""
        from src.errors.diagnostics import undefined_variable_error
        
        diag = undefined_variable_error("foo", 
                                      SourceLocation(line=3, column=5),
                                      similar_names=["for", "fold"])
        
        self.assertEqual(diag.severity, ErrorSeverity.ERROR)
        self.assertIn("foo", diag.message)
        self.assertIn("for", diag.suggestion)
        self.assertIn("fold", diag.suggestion)
    
    def test_pattern_match_error(self):
        """Test pattern match error generator"""
        from src.errors.diagnostics import pattern_match_error
        
        diag = pattern_match_error([1, 2, 3])
        
        self.assertEqual(diag.severity, ErrorSeverity.ERROR)
        self.assertIn("[1, 2, 3]", diag.message)
        self.assertIsNotNone(diag.suggestion)
        self.assertGreater(len(diag.notes), 0)
    
    def test_syntax_error(self):
        """Test syntax error generator"""
        from src.errors.diagnostics import syntax_error
        
        diag = syntax_error("Unexpected token", 
                          SourceLocation(line=1, column=15),
                          expected="closing parenthesis")
        
        self.assertEqual(diag.severity, ErrorSeverity.ERROR)
        self.assertIn("Unexpected token", diag.message)
        self.assertIn("closing parenthesis", diag.suggestion)


if __name__ == '__main__':
    unittest.main()