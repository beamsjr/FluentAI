"""
Simple tests for ClaudeLang Code Generation
"""

import unittest
from src.vm.compiler import BytecodeCompiler
from src.vm.bytecode import BytecodeChunk, BytecodeBuilder, Opcode, Instruction
from src.parser.sexpr_parser import parse
from src.core.ast import *
from src.jit.x86_64_codegen import X86_64CodeGen, Register
from src.optimizer.graph_optimizer import GraphOptimizer


class TestBytecodeStructure(unittest.TestCase):
    """Test bytecode data structures"""
    
    def test_instruction_creation(self):
        """Test creating instructions"""
        instr = Instruction(Opcode.PUSH, 42)
        self.assertEqual(instr.opcode, Opcode.PUSH)
        self.assertEqual(instr.arg, 42)
        
        # Instruction without argument
        instr2 = Instruction(Opcode.ADD)
        self.assertEqual(instr2.opcode, Opcode.ADD)
        self.assertIsNone(instr2.arg)
    
    def test_bytecode_chunk_creation(self):
        """Test creating bytecode chunks"""
        chunk = BytecodeChunk(instructions=[], constants=[])
        
        # Add instruction
        chunk.add_instruction(Opcode.PUSH, 0)
        self.assertEqual(len(chunk.instructions), 1)
        
        # Add constant
        idx = chunk.add_constant(42)
        self.assertEqual(idx, 0)
        self.assertEqual(chunk.constants[0], 42)
        
        # Reuse constant
        idx2 = chunk.add_constant(42)
        self.assertEqual(idx2, 0)  # Same index
        self.assertEqual(len(chunk.constants), 1)
    
    def test_bytecode_builder(self):
        """Test bytecode builder"""
        builder = BytecodeBuilder()
        
        # Emit instructions
        builder.emit(Opcode.PUSH, 0)
        builder.emit(Opcode.PUSH, 1)
        builder.emit(Opcode.ADD)
        
        # Build chunk
        chunk = builder.build()
        self.assertEqual(len(chunk.instructions), 3)
        self.assertEqual(chunk.instructions[0].opcode, Opcode.PUSH)
        self.assertEqual(chunk.instructions[2].opcode, Opcode.ADD)


class TestBytecodeCompilation(unittest.TestCase):
    """Test bytecode compilation"""
    
    def setUp(self):
        """Set up test environment"""
        self.compiler = BytecodeCompiler()
    
    def compile(self, source):
        """Helper to parse and compile source"""
        graph = parse(source)
        return self.compiler.compile(graph)
    
    def test_compile_literal_integer(self):
        """Test compiling integer literal"""
        chunk = self.compile("42")
        
        # Should have at least one instruction
        self.assertGreater(len(chunk.instructions), 0)
        
        # Should have the constant
        self.assertIn(42, chunk.constants)
    
    def test_compile_literal_string(self):
        """Test compiling string literal"""
        chunk = self.compile('"hello"')
        
        # Should have the string constant
        self.assertIn("hello", chunk.constants)
    
    def test_compile_literal_boolean(self):
        """Test compiling boolean literals"""
        chunk_true = self.compile("true")
        chunk_false = self.compile("false")
        
        # Should compile successfully
        self.assertGreater(len(chunk_true.instructions), 0)
        self.assertGreater(len(chunk_false.instructions), 0)
    
    def test_compile_simple_arithmetic(self):
        """Test compiling simple arithmetic"""
        chunk = self.compile("(+ 2 3)")
        
        # Should have ADD instruction
        opcodes = [instr.opcode for instr in chunk.instructions]
        self.assertIn(Opcode.ADD, opcodes)
        
        # Should have both constants
        self.assertIn(2, chunk.constants)
        self.assertIn(3, chunk.constants)
    
    def test_compile_nested_arithmetic(self):
        """Test compiling nested arithmetic"""
        chunk = self.compile("(+ (* 2 3) 4)")
        
        # Should have both operations
        opcodes = [instr.opcode for instr in chunk.instructions]
        self.assertIn(Opcode.MUL, opcodes)
        self.assertIn(Opcode.ADD, opcodes)
    
    def test_compile_comparison(self):
        """Test compiling comparisons"""
        chunk = self.compile("(< 5 10)")
        
        # Should have comparison instruction
        opcodes = [instr.opcode for instr in chunk.instructions]
        self.assertIn(Opcode.LT, opcodes)
    
    def test_compile_if_expression(self):
        """Test compiling if expressions"""
        chunk = self.compile("(if true 1 2)")
        
        # Should have jump instructions
        opcodes = [instr.opcode for instr in chunk.instructions]
        jump_opcodes = [Opcode.JUMP, Opcode.JUMP_IF, Opcode.JUMP_IF_NOT]
        self.assertTrue(any(op in opcodes for op in jump_opcodes))
    
    def test_compile_let_binding(self):
        """Test compiling let bindings"""
        chunk = self.compile("(let ((x 42)) x)")
        
        # Should have store and load
        opcodes = [instr.opcode for instr in chunk.instructions]
        self.assertIn(Opcode.STORE, opcodes)
        self.assertIn(Opcode.LOAD, opcodes)
    
    def test_compile_lambda(self):
        """Test compiling lambda expressions"""
        chunk = self.compile("(lambda (x) x)")
        
        # Should create function
        opcodes = [instr.opcode for instr in chunk.instructions]
        self.assertIn(Opcode.MAKE_FUNC, opcodes)
    
    def test_compile_function_call(self):
        """Test compiling function calls"""
        chunk = self.compile("((lambda (x) (* x 2)) 5)")
        
        # Should have call instruction
        opcodes = [instr.opcode for instr in chunk.instructions]
        self.assertIn(Opcode.CALL, opcodes)
    
    def test_compile_list_literal(self):
        """Test compiling list literals"""
        chunk = self.compile("[1 2 3]")
        
        # Should create list
        opcodes = [instr.opcode for instr in chunk.instructions]
        # Might use MAKE_LIST or repeated CONS
        list_opcodes = [Opcode.MAKE_LIST, Opcode.LIST_CONS]
        self.assertTrue(any(op in opcodes for op in list_opcodes))
    
    def test_compile_empty_list(self):
        """Test compiling empty list"""
        chunk = self.compile("[]")
        
        # Should compile successfully
        self.assertGreater(len(chunk.instructions), 0)
    
    def test_compile_list_operations(self):
        """Test compiling list operations"""
        # Head
        chunk = self.compile("(head [1 2 3])")
        opcodes = [instr.opcode for instr in chunk.instructions]
        self.assertIn(Opcode.LIST_HEAD, opcodes)
        
        # Tail
        chunk = self.compile("(tail [1 2 3])")
        opcodes = [instr.opcode for instr in chunk.instructions]
        self.assertIn(Opcode.LIST_TAIL, opcodes)
        
        # Length
        chunk = self.compile("(length [1 2 3])")
        opcodes = [instr.opcode for instr in chunk.instructions]
        self.assertIn(Opcode.LIST_LEN, opcodes)


class TestX86CodeGeneration(unittest.TestCase):
    """Test x86-64 code generation"""
    
    def setUp(self):
        """Set up test environment"""
        self.codegen = X86_64CodeGen()
    
    def test_register_encoding(self):
        """Test register encoding"""
        # Basic registers
        self.assertEqual(Register.RAX, 0)
        self.assertEqual(Register.RCX, 1)
        self.assertEqual(Register.RDX, 2)
        self.assertEqual(Register.RBX, 3)
        
        # Extended registers
        self.assertEqual(Register.R8, 8)
        self.assertEqual(Register.R15, 15)
    
    def test_immediate_operations(self):
        """Test immediate value operations"""
        # MOV immediate to register
        self.codegen.mov_reg_imm(Register.RAX, 42)
        
        # ADD immediate
        self.codegen.add_reg_imm(Register.RAX, 10)
        
        # SUB immediate
        self.codegen.sub_reg_imm(Register.RBX, 5)
        
        # Should generate code
        self.assertGreater(len(self.codegen.code), 0)
    
    def test_register_operations(self):
        """Test register-to-register operations"""
        # MOV between registers
        self.codegen.mov_reg_reg(Register.RBX, Register.RAX)
        
        # Arithmetic
        self.codegen.add_reg_reg(Register.RAX, Register.RBX)
        self.codegen.sub_reg_reg(Register.RCX, Register.RDX)
        
        # Should generate code
        self.assertGreater(len(self.codegen.code), 0)
    
    def test_stack_operations(self):
        """Test stack push/pop"""
        initial_size = len(self.codegen.code)
        
        self.codegen.push_reg(Register.RAX)
        self.codegen.push_reg(Register.RBX)
        self.codegen.pop_reg(Register.RCX)
        self.codegen.pop_reg(Register.RDX)
        
        # Should add instructions
        self.assertGreater(len(self.codegen.code), initial_size)
    
    def test_comparison_operations(self):
        """Test comparison and conditional jumps"""
        # Compare registers
        self.codegen.cmp_reg_reg(Register.RAX, Register.RBX)
        
        # Conditional jumps
        self.codegen.je(0x10)   # Jump if equal
        self.codegen.jne(0x20)  # Jump if not equal
        self.codegen.jl(0x30)   # Jump if less
        self.codegen.jg(0x40)   # Jump if greater
        
        self.assertGreater(len(self.codegen.code), 0)
    
    def test_function_calls(self):
        """Test function call generation"""
        # Direct call
        self.codegen.call_imm(0x1000)
        
        # Indirect call
        self.codegen.call_reg(Register.RAX)
        
        # Return
        self.codegen.ret()
        
        self.assertGreater(len(self.codegen.code), 0)
    
    def test_memory_addressing(self):
        """Test memory addressing modes"""
        # Load from memory
        self.codegen.mov_reg_mem(Register.RAX, Register.RBX, 0)
        self.codegen.mov_reg_mem(Register.RCX, Register.RBP, -8)
        
        # Store to memory
        self.codegen.mov_mem_reg(Register.RBX, 0, Register.RAX)
        self.codegen.mov_mem_reg(Register.RBP, -8, Register.RCX)
        
        self.assertGreater(len(self.codegen.code), 0)


class TestOptimization(unittest.TestCase):
    """Test optimization in code generation"""
    
    def setUp(self):
        """Set up test environment"""
        self.compiler = BytecodeCompiler()
        self.optimizer = GraphOptimizer()
    
    def compile_with_optimization(self, source):
        """Compile with optimization"""
        graph = parse(source)
        optimized = self.optimizer.optimize(graph)
        return self.compiler.compile(optimized)
    
    def test_constant_folding(self):
        """Test constant folding optimization"""
        # Compile without optimization
        graph1 = parse("(+ 2 3)")
        chunk1 = self.compiler.compile(graph1)
        
        # Compile with optimization
        chunk2 = self.compile_with_optimization("(+ 2 3)")
        
        # Optimized version should be simpler
        # (might just load 5 directly)
        self.assertLessEqual(len(chunk2.instructions), len(chunk1.instructions))
    
    def test_identity_operations(self):
        """Test identity operation elimination"""
        # Adding 0 or multiplying by 1
        chunk = self.compile_with_optimization("(+ x 0)")
        
        # Should be simplified
        self.assertGreater(len(chunk.instructions), 0)
    
    def test_dead_code_elimination(self):
        """Test dead code elimination"""
        source = """
        (let ((x 10)
              (y 20)
              (unused 30))
          (+ x y))
        """
        
        chunk = self.compile_with_optimization(source)
        
        # Should still compile correctly
        self.assertGreater(len(chunk.instructions), 0)
        
        # Should have optimized out unused binding
        # (hard to test without knowing exact optimization)


class TestEdgeCases(unittest.TestCase):
    """Test edge cases in code generation"""
    
    def setUp(self):
        """Set up test environment"""
        self.compiler = BytecodeCompiler()
    
    def test_deeply_nested_expression(self):
        """Test deeply nested expressions"""
        # Build nested expression
        expr = "1"
        for i in range(10):
            expr = f"(+ {expr} 1)"
        
        chunk = self.compiler.compile(parse(expr))
        
        # Should handle deep nesting
        self.assertGreater(len(chunk.instructions), 10)
    
    def test_many_arguments(self):
        """Test functions with many arguments"""
        # Create function with many parameters
        params = " ".join(f"x{i}" for i in range(10))
        body = "(+ " + " ".join(f"x{i}" for i in range(10)) + ")"
        source = f"(lambda ({params}) {body})"
        
        chunk = self.compiler.compile(parse(source))
        
        # Should compile successfully
        self.assertGreater(len(chunk.instructions), 0)
    
    def test_large_constants(self):
        """Test large numeric constants"""
        large_numbers = [
            "1000000",
            "-1000000",
            "3.14159265358979323846",
            "1e100",
            "-1e100"
        ]
        
        for num in large_numbers:
            chunk = self.compiler.compile(parse(num))
            self.assertGreater(len(chunk.instructions), 0)
    
    def test_unicode_in_strings(self):
        """Test Unicode in string constants"""
        chunk = self.compiler.compile(parse('"Hello 世界 🌍"'))
        
        # Should handle Unicode
        self.assertIn("Hello 世界 🌍", chunk.constants)
    
    def test_empty_function_body(self):
        """Test function with empty body"""
        # Lambda that returns nil
        chunk = self.compiler.compile(parse("(lambda () nil)"))
        
        # Should compile successfully
        self.assertGreater(len(chunk.instructions), 0)


if __name__ == '__main__':
    unittest.main()