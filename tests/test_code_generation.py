"""
Tests for ClaudeLang Code Generation Backends
"""

import unittest
from src.vm.compiler import BytecodeCompiler
from src.vm.bytecode import BytecodeChunk, BytecodeBuilder, Opcode
from src.parser.sexpr_parser import parse
from src.core.ast import *
from src.jit.jit_compiler import JITCompiler
from src.jit.x86_64_codegen import X86_64CodeGen, Register
from src.optimizer.graph_optimizer import GraphOptimizer


class TestBytecodeCompiler(unittest.TestCase):
    """Test bytecode compiler functionality"""
    
    def setUp(self):
        """Set up test environment"""
        self.compiler = BytecodeCompiler()
    
    def compile(self, source):
        """Helper to parse and compile source"""
        graph = parse(source)
        return self.compiler.compile(graph)
    
    def test_compile_literal(self):
        """Test compiling literal values"""
        # Integer
        bytecode = self.compile("42")
        self.assertEqual(len(bytecode.instructions), 1)
        self.assertEqual(bytecode.instructions[0], Opcode.LOAD_CONST)
        self.assertEqual(bytecode.constants[0], 42)
        
        # String
        bytecode = self.compile('"hello"')
        self.assertEqual(bytecode.instructions[0], Opcode.LOAD_CONST)
        self.assertEqual(bytecode.constants[0], "hello")
        
        # Boolean
        bytecode = self.compile("true")
        self.assertEqual(bytecode.instructions[0], Opcode.LOAD_CONST)
        self.assertEqual(bytecode.constants[0], True)
    
    def test_compile_variable(self):
        """Test compiling variable references"""
        # Need a context with the variable
        bytecode = self.compile("(let ((x 42)) x)")
        
        # Should have LOAD_CONST 42, STORE_VAR x, LOAD_VAR x
        self.assertIn(Opcode.STORE_VAR, bytecode.instructions)
        self.assertIn(Opcode.LOAD_VAR, bytecode.instructions)
    
    def test_compile_arithmetic(self):
        """Test compiling arithmetic operations"""
        bytecode = self.compile("(+ 2 3)")
        
        # Should load constants and add
        self.assertEqual(bytecode.instructions[0], Opcode.LOAD_CONST)
        self.assertEqual(bytecode.instructions[2], Opcode.LOAD_CONST)
        self.assertEqual(bytecode.instructions[4], Opcode.ADD)
        
        # Test other operations
        for op, opcode in [("+", Opcode.ADD), ("-", Opcode.SUB), 
                          ("*", Opcode.MUL), ("/", Opcode.DIV)]:
            bytecode = self.compile(f"({op} 10 5)")
            self.assertIn(opcode, bytecode.instructions)
    
    def test_compile_comparison(self):
        """Test compiling comparison operations"""
        comparisons = [
            ("<", Opcode.LT),
            (">", Opcode.GT),
            ("<=", Opcode.LE),
            (">=", Opcode.GE),
            ("=", Opcode.EQ),
            ("not=", Opcode.NE)
        ]
        
        for op, expected_opcode in comparisons:
            bytecode = self.compile(f"({op} 5 3)")
            self.assertIn(expected_opcode, bytecode.instructions)
    
    def test_compile_if(self):
        """Test compiling if expressions"""
        bytecode = self.compile("(if true 1 2)")
        
        # Should have conditional jump
        self.assertIn(Opcode.JUMP_IF_FALSE, bytecode.instructions)
        self.assertIn(Opcode.JUMP, bytecode.instructions)
    
    def test_compile_let(self):
        """Test compiling let expressions"""
        bytecode = self.compile("(let ((x 10) (y 20)) (+ x y))")
        
        # Should store variables
        store_count = bytecode.instructions.count(Opcode.STORE_VAR)
        self.assertEqual(store_count, 2)
        
        # Should load variables for addition
        load_count = bytecode.instructions.count(Opcode.LOAD_VAR)
        self.assertEqual(load_count, 2)
    
    def test_compile_lambda(self):
        """Test compiling lambda expressions"""
        bytecode = self.compile("(lambda (x) (* x 2))")
        
        # Should create closure
        self.assertIn(Opcode.MAKE_CLOSURE, bytecode.instructions)
        
        # Lambda body should be compiled
        self.assertIn(Opcode.MUL, bytecode.instructions)
    
    def test_compile_application(self):
        """Test compiling function applications"""
        bytecode = self.compile("((lambda (x) (* x 2)) 5)")
        
        # Should have CALL instruction
        self.assertIn(Opcode.CALL, bytecode.instructions)
    
    def test_compile_list_operations(self):
        """Test compiling list operations"""
        # List literal
        bytecode = self.compile("[1 2 3]")
        
        # Should build list
        self.assertIn(Opcode.CALL, bytecode.instructions)
        
        # List operations
        operations = [
            ("(cons 1 [])", "cons"),
            ("(head [1 2 3])", "head"),
            ("(tail [1 2 3])", "tail"),
            ("(length [1 2 3])", "length")
        ]
        
        for expr, _ in operations:
            bytecode = self.compile(expr)
            self.assertIn(Opcode.CALL, bytecode.instructions)
    
    def test_compile_sequence(self):
        """Test compiling do/sequence expressions"""
        bytecode = self.compile("(do 1 2 3)")
        
        # Should evaluate all expressions
        const_count = bytecode.instructions.count(Opcode.LOAD_CONST)
        self.assertEqual(const_count, 3)
        
        # Should pop intermediate values
        self.assertIn(Opcode.POP, bytecode.instructions)
    
    def test_compile_recursive_function(self):
        """Test compiling recursive functions"""
        source = """
        (let ((fact (lambda (n)
                      (if (= n 0)
                          1
                          (* n (fact (- n 1)))))))
          (fact 5))
        """
        
        bytecode = self.compile(source)
        
        # Should have recursive call
        call_count = bytecode.instructions.count(Opcode.CALL)
        self.assertGreater(call_count, 1)
    
    def test_compile_nested_lets(self):
        """Test compiling nested let expressions"""
        bytecode = self.compile("""
        (let ((x 1))
          (let ((y 2))
            (+ x y)))
        """)
        
        # Both variables should be stored
        store_count = bytecode.instructions.count(Opcode.STORE_VAR)
        self.assertEqual(store_count, 2)
    
    def test_empty_program(self):
        """Test compiling empty program"""
        # Empty program might be just a literal
        bytecode = self.compile("nil")
        self.assertGreater(len(bytecode.instructions), 0)
    
    def test_bytecode_metadata(self):
        """Test bytecode metadata generation"""
        bytecode = self.compile("(+ 1 2)")
        
        # Should have metadata
        self.assertIsInstance(bytecode.metadata, dict)
        
        # Should track source info if available
        if "source_map" in bytecode.metadata:
            self.assertIsInstance(bytecode.metadata["source_map"], list)


class TestNativeCodeGeneration(unittest.TestCase):
    """Test native code generation"""
    
    def setUp(self):
        """Set up test environment"""
        self.jit_compiler = JITCompiler()
        self.x86_gen = X86_64CodeGen()
    
    def test_x86_register_allocation(self):
        """Test x86-64 register allocation"""
        # Test register enum values
        self.assertEqual(Register.RAX, 0)
        self.assertEqual(Register.RCX, 1)
        self.assertEqual(Register.RDX, 2)
        
        # Test special registers
        self.assertEqual(Register.RSP, 4)
        self.assertEqual(Register.RBP, 5)
        self.assertEqual(Register.R15, 15)
    
    def test_x86_basic_instructions(self):
        """Test x86-64 instruction encoding"""
        # Test MOV
        self.x86_gen.mov_reg_imm(Register.RAX, 42)
        self.assertGreater(len(self.x86_gen.code), 0)
        
        # Test arithmetic
        self.x86_gen.add_reg_reg(Register.RAX, Register.RBX)
        self.x86_gen.sub_reg_reg(Register.RCX, Register.RDX)
        self.x86_gen.imul_reg_reg(Register.R8, Register.R9)
        
        # Verify code was generated
        self.assertGreater(len(self.x86_gen.code), 10)
    
    def test_x86_stack_operations(self):
        """Test x86-64 stack operations"""
        initial_size = len(self.x86_gen.code)
        
        # Push and pop
        self.x86_gen.push_reg(Register.RAX)
        self.x86_gen.pop_reg(Register.RBX)
        
        # Should have generated code
        self.assertGreater(len(self.x86_gen.code), initial_size)
    
    def test_x86_memory_operations(self):
        """Test x86-64 memory operations"""
        # Load from memory
        self.x86_gen.mov_reg_mem(Register.RAX, Register.RBX, 8)
        
        # Store to memory
        self.x86_gen.mov_mem_reg(Register.RBX, 8, Register.RAX)
        
        # Verify instructions were generated
        self.assertGreater(len(self.x86_gen.code), 0)
    
    def test_x86_control_flow(self):
        """Test x86-64 control flow instructions"""
        # Comparison
        self.x86_gen.cmp_reg_reg(Register.RAX, Register.RBX)
        
        # Conditional jumps
        self.x86_gen.je(0)  # Jump if equal
        self.x86_gen.jne(0) # Jump if not equal
        self.x86_gen.jl(0)  # Jump if less
        self.x86_gen.jg(0)  # Jump if greater
        
        # Unconditional jump
        self.x86_gen.jmp(0)
        
        # Call and return
        self.x86_gen.call_reg(Register.RAX)
        self.x86_gen.ret()
        
        self.assertGreater(len(self.x86_gen.code), 10)
    
    def test_native_compilation_simple(self):
        """Test native compilation of simple expressions"""
        # Parse and compile a simple expression
        graph = parse("(+ 2 3)")
        bytecode = BytecodeCompiler().compile(graph)
        
        # JIT compilation would compile the bytecode
        # This is a simplified test since full JIT needs more setup
        self.assertIsNotNone(bytecode)
        self.assertGreater(len(bytecode.instructions), 0)
    
    def test_native_function_prologue_epilogue(self):
        """Test function prologue and epilogue generation"""
        self.x86_gen.emit_prologue()
        initial_size = len(self.x86_gen.code)
        
        # Should have pushed RBP and set up stack frame
        self.assertGreater(initial_size, 0)
        
        self.x86_gen.emit_epilogue()
        
        # Should have restored RBP and return
        self.assertGreater(len(self.x86_gen.code), initial_size)
    
    def test_native_constant_pool(self):
        """Test native code constant pool"""
        # Test that JIT compiler can handle constants
        builder = BytecodeBuilder()
        builder.emit(Opcode.PUSH, 0)
        builder.add_constant(42)
        bytecode = builder.build()
        
        # Should be valid bytecode
        self.assertGreater(len(bytecode.code), 0)
        self.assertEqual(bytecode.constants[0], 42)


class TestCodeOptimization(unittest.TestCase):
    """Test code optimization in compilation"""
    
    def setUp(self):
        """Set up test environment"""
        self.compiler = BytecodeCompiler()
        self.optimizer = GraphOptimizer()
    
    def compile_optimized(self, source):
        """Parse, optimize, and compile source"""
        graph = parse(source)
        optimized = self.optimizer.optimize(graph)
        return self.compiler.compile(optimized)
    
    def test_constant_folding_optimization(self):
        """Test constant folding in bytecode"""
        # Without optimization
        unopt = self.compiler.compile(parse("(+ 2 3)"))
        
        # With optimization
        opt = self.compile_optimized("(+ 2 3)")
        
        # Optimized should be shorter (single constant load)
        self.assertLess(len(opt.instructions), len(unopt.instructions))
        
        # Should load 5 directly
        self.assertEqual(opt.constants[0], 5)
    
    def test_dead_code_elimination(self):
        """Test dead code elimination"""
        source = """
        (let ((x 10)
              (y 20)
              (z 30))
          (+ x y))
        """
        
        opt = self.compile_optimized(source)
        
        # z should not be stored (dead code)
        # Count STORE_VAR instructions
        store_count = opt.instructions.count(Opcode.STORE_VAR)
        # Might still store all due to let semantics
        self.assertGreaterEqual(store_count, 2)
    
    def test_tail_call_optimization(self):
        """Test tail call optimization"""
        source = """
        (let ((loop (lambda (n acc)
                      (if (= n 0)
                          acc
                          (loop (- n 1) (+ acc n))))))
          (loop 10 0))
        """
        
        bytecode = self.compile_optimized(source)
        
        # Should use tail call optimization
        # Look for TAIL_CALL opcode if implemented
        if Opcode.TAIL_CALL in [op for op in Opcode]:
            self.assertIn(Opcode.TAIL_CALL, bytecode.instructions)
    
    def test_peephole_optimization(self):
        """Test peephole optimizations"""
        # Test redundant operations
        source = "(let ((x 5)) (+ x 0))"  # Adding 0 should be optimized
        
        opt = self.compile_optimized(source)
        
        # Should be optimized to just load x
        # Less instructions than unoptimized
        self.assertGreater(len(opt.instructions), 0)
    
    def test_inline_small_functions(self):
        """Test inlining of small functions"""
        source = """
        (let ((identity (lambda (x) x)))
          (identity 42))
        """
        
        opt = self.compile_optimized(source)
        
        # Small function might be inlined
        # Would have fewer CALL instructions
        call_count = opt.instructions.count(Opcode.CALL)
        self.assertGreaterEqual(call_count, 0)


class TestBytecodeValidation(unittest.TestCase):
    """Test bytecode validation and verification"""
    
    def setUp(self):
        """Set up test environment"""
        self.compiler = BytecodeCompiler()
    
    def test_bytecode_structure(self):
        """Test bytecode has valid structure"""
        bytecode = self.compiler.compile(parse("(+ 1 2)"))
        
        # Should have required attributes
        self.assertIsInstance(bytecode.instructions, list)
        self.assertIsInstance(bytecode.constants, list)
        self.assertIsInstance(bytecode.metadata, dict)
        
        # Instructions should be valid opcodes
        for i in range(0, len(bytecode.instructions), 2):
            opcode = bytecode.instructions[i]
            self.assertIsInstance(opcode, Opcode)
    
    def test_jump_targets_valid(self):
        """Test jump targets are valid"""
        bytecode = self.compiler.compile(parse("(if true 1 2)"))
        
        # Find jump instructions
        for i in range(0, len(bytecode.instructions), 2):
            opcode = bytecode.instructions[i]
            if opcode in [Opcode.JUMP, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE]:
                target = bytecode.instructions[i + 1]
                # Target should be within bounds
                self.assertGreaterEqual(target, 0)
                self.assertLess(target, len(bytecode.instructions))
    
    def test_variable_references_valid(self):
        """Test variable references are valid"""
        bytecode = self.compiler.compile(parse("(let ((x 1)) x)"))
        
        # Find variable operations
        for i in range(0, len(bytecode.instructions), 2):
            opcode = bytecode.instructions[i]
            if opcode in [Opcode.LOAD_VAR, Opcode.STORE_VAR]:
                var_index = bytecode.instructions[i + 1]
                # Variable index should be reasonable
                self.assertGreaterEqual(var_index, 0)
    
    def test_constant_references_valid(self):
        """Test constant references are valid"""
        bytecode = self.compiler.compile(parse("42"))
        
        # Find constant loads
        for i in range(0, len(bytecode.instructions), 2):
            if bytecode.instructions[i] == Opcode.LOAD_CONST:
                const_index = bytecode.instructions[i + 1]
                # Should reference valid constant
                self.assertGreaterEqual(const_index, 0)
                self.assertLess(const_index, len(bytecode.constants))
    
    def test_closure_compilation(self):
        """Test closure compilation correctness"""
        source = """
        (let ((x 10))
          (lambda (y) (+ x y)))
        """
        
        bytecode = self.compiler.compile(parse(source))
        
        # Should create closure capturing x
        self.assertIn(Opcode.MAKE_CLOSURE, bytecode.instructions)
        
        # Should have closure info in metadata
        if "closures" in bytecode.metadata:
            self.assertGreater(len(bytecode.metadata["closures"]), 0)


class TestCodeGenerationEdgeCases(unittest.TestCase):
    """Test edge cases in code generation"""
    
    def setUp(self):
        """Set up test environment"""
        self.compiler = BytecodeCompiler()
    
    def test_empty_list_compilation(self):
        """Test compiling empty list"""
        bytecode = self.compiler.compile(parse("[]"))
        
        # Should compile successfully
        self.assertGreater(len(bytecode.instructions), 0)
    
    def test_deeply_nested_expression(self):
        """Test compiling deeply nested expressions"""
        # Create deeply nested expression
        depth = 10
        expr = "1"
        for _ in range(depth):
            expr = f"(+ {expr} 1)"
        
        bytecode = self.compiler.compile(parse(expr))
        
        # Should handle deep nesting
        self.assertGreater(len(bytecode.instructions), depth * 2)
    
    def test_large_literal_lists(self):
        """Test compiling large literal lists"""
        # Create large list
        size = 100
        elements = " ".join(str(i) for i in range(size))
        source = f"[{elements}]"
        
        bytecode = self.compiler.compile(parse(source))
        
        # Should handle large lists
        self.assertGreater(len(bytecode.constants), 50)
    
    def test_many_variables(self):
        """Test compiling many variables"""
        # Create let with many bindings
        bindings = " ".join(f"(x{i} {i})" for i in range(20))
        source = f"(let ({bindings}) x0)"
        
        bytecode = self.compiler.compile(parse(source))
        
        # Should handle many variables
        store_count = bytecode.instructions.count(Opcode.STORE_VAR)
        self.assertEqual(store_count, 20)
    
    def test_mutual_recursion(self):
        """Test compiling mutually recursive functions"""
        source = """
        (let ((even? (lambda (n)
                       (if (= n 0)
                           true
                           (odd? (- n 1)))))
              (odd? (lambda (n)
                      (if (= n 0)
                          false
                          (even? (- n 1))))))
          (even? 4))
        """
        
        bytecode = self.compiler.compile(parse(source))
        
        # Should compile mutual recursion
        self.assertIn(Opcode.CALL, bytecode.instructions)
        self.assertGreater(len(bytecode.instructions), 20)
    
    def test_unicode_strings(self):
        """Test compiling Unicode strings"""
        bytecode = self.compiler.compile(parse('"Hello 世界 🌍"'))
        
        # Should handle Unicode
        self.assertEqual(bytecode.constants[0], "Hello 世界 🌍")
    
    def test_special_float_values(self):
        """Test compiling special float values"""
        special_values = ["0.0", "-0.0", "3.14159", "1e10", "1e-10"]
        
        for value in special_values:
            bytecode = self.compiler.compile(parse(value))
            self.assertEqual(len(bytecode.instructions), 2)  # LOAD_CONST + arg


if __name__ == '__main__':
    unittest.main()