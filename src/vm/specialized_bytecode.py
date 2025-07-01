"""
Type-Specialized Bytecode Instructions for ClaudeLang VM

These specialized instructions avoid boxing/unboxing overhead and enable
more efficient execution for common cases.
"""

from enum import IntEnum, auto
from typing import List, Union, Dict, Any
from .bytecode import Opcode, BytecodeBuilder, BytecodeChunk


class SpecializedOpcode(IntEnum):
    """Extended opcodes with type-specialized versions"""
    
    # Original opcodes
    PUSH = Opcode.PUSH
    POP = Opcode.POP
    DUP = Opcode.DUP
    SWAP = Opcode.SWAP
    
    # Specialized push for common constants
    PUSH_INT_0 = auto()
    PUSH_INT_1 = auto()
    PUSH_INT_2 = auto()
    PUSH_INT_SMALL = auto()  # For small integers (-128 to 127)
    PUSH_TRUE = auto()
    PUSH_FALSE = auto()
    PUSH_NIL = auto()
    
    # Type-specialized arithmetic (integer)
    ADD_INT = auto()
    SUB_INT = auto()
    MUL_INT = auto()
    DIV_INT = auto()
    MOD_INT = auto()
    NEG_INT = auto()
    
    # Type-specialized arithmetic (float)
    ADD_FLOAT = auto()
    SUB_FLOAT = auto()
    MUL_FLOAT = auto()
    DIV_FLOAT = auto()
    NEG_FLOAT = auto()
    
    # Type-specialized comparison (integer)
    LT_INT = auto()
    LE_INT = auto()
    GT_INT = auto()
    GE_INT = auto()
    EQ_INT = auto()
    NE_INT = auto()
    
    # Type-specialized comparison (float)
    LT_FLOAT = auto()
    LE_FLOAT = auto()
    GT_FLOAT = auto()
    GE_FLOAT = auto()
    EQ_FLOAT = auto()
    NE_FLOAT = auto()
    
    # Fast local variable access
    LOAD_LOCAL_0 = auto()
    LOAD_LOCAL_1 = auto()
    LOAD_LOCAL_2 = auto()
    LOAD_LOCAL_3 = auto()
    STORE_LOCAL_0 = auto()
    STORE_LOCAL_1 = auto()
    STORE_LOCAL_2 = auto()
    STORE_LOCAL_3 = auto()
    
    # Type conversions
    INT_TO_FLOAT = auto()
    FLOAT_TO_INT = auto()
    
    # Optimized control flow
    JUMP_IF_TRUE_INT = auto()  # Jump if TOS is non-zero integer
    JUMP_IF_FALSE_INT = auto()  # Jump if TOS is zero integer
    
    # List operations
    LIST_LENGTH_FAST = auto()  # For native lists
    LIST_GET = auto()  # Get element by index
    LIST_SET = auto()  # Set element by index
    
    # Generic fallbacks
    ADD = Opcode.ADD
    SUB = Opcode.SUB
    MUL = Opcode.MUL
    DIV = Opcode.DIV
    MOD = Opcode.MOD
    LT = Opcode.LT
    LE = Opcode.LE
    GT = Opcode.GT
    GE = Opcode.GE
    EQ = Opcode.EQ
    NE = Opcode.NE
    
    # Other original opcodes
    AND = Opcode.AND
    OR = Opcode.OR
    NOT = Opcode.NOT
    JUMP = Opcode.JUMP
    JUMP_IF_NOT = Opcode.JUMP_IF_NOT
    CALL = Opcode.CALL
    RETURN = Opcode.RETURN
    MAKE_FUNC = Opcode.MAKE_FUNC
    LOAD = Opcode.LOAD
    STORE = Opcode.STORE
    LIST_CONS = Opcode.LIST_CONS
    LIST_HEAD = Opcode.LIST_HEAD
    LIST_TAIL = Opcode.LIST_TAIL
    LIST_LEN = Opcode.LIST_LEN
    LIST_EMPTY = Opcode.LIST_EMPTY
    STR_LEN = Opcode.STR_LEN
    STR_CONCAT = Opcode.STR_CONCAT
    STR_UPPER = Opcode.STR_UPPER
    STR_LOWER = Opcode.STR_LOWER
    HALT = Opcode.HALT


class SpecializedBytecodeBuilder(BytecodeBuilder):
    """Extended bytecode builder with type specialization support"""
    
    def __init__(self):
        super().__init__()
        self.type_hints: Dict[int, str] = {}  # Instruction index -> type hint
        # Ensure we have the necessary attributes
        if not hasattr(self, 'instructions'):
            self.instructions = []
        if not hasattr(self, 'constants'):
            self.constants = []
    
    def emit_specialized(self, opcode: SpecializedOpcode, operand: int = 0):
        """Emit a specialized opcode"""
        self.chunk.instructions.append(int(opcode))
        if operand != 0:
            self.chunk.instructions.append(operand)
        return len(self.chunk.instructions) - 1
    
    def emit_int_constant(self, value: int):
        """Emit integer constant with optimization"""
        if value == 0:
            self.emit_specialized(SpecializedOpcode.PUSH_INT_0)
        elif value == 1:
            self.emit_specialized(SpecializedOpcode.PUSH_INT_1)
        elif value == 2:
            self.emit_specialized(SpecializedOpcode.PUSH_INT_2)
        elif -128 <= value <= 127:
            # Use specialized small int push
            self.emit_specialized(SpecializedOpcode.PUSH_INT_SMALL, value & 0xFF)
        else:
            # Fall back to regular constant
            self.emit_constant(value)
    
    def emit_bool_constant(self, value: bool):
        """Emit boolean constant"""
        if value:
            self.emit_specialized(SpecializedOpcode.PUSH_TRUE)
        else:
            self.emit_specialized(SpecializedOpcode.PUSH_FALSE)
    
    def emit_int_arithmetic(self, op: str):
        """Emit type-specialized integer arithmetic"""
        op_map = {
            '+': SpecializedOpcode.ADD_INT,
            '-': SpecializedOpcode.SUB_INT,
            '*': SpecializedOpcode.MUL_INT,
            '/': SpecializedOpcode.DIV_INT,
            'mod': SpecializedOpcode.MOD_INT,
        }
        
        if op in op_map:
            self.emit_specialized(op_map[op])
        else:
            # Fall back to generic
            self.emit(Opcode.ADD)  # Default
    
    def emit_float_arithmetic(self, op: str):
        """Emit type-specialized float arithmetic"""
        op_map = {
            '+.': SpecializedOpcode.ADD_FLOAT,
            '-.': SpecializedOpcode.SUB_FLOAT,
            '*.': SpecializedOpcode.MUL_FLOAT,
            '/.': SpecializedOpcode.DIV_FLOAT,
        }
        
        if op in op_map:
            self.emit_specialized(op_map[op])
        else:
            # Fall back to generic
            self.emit(Opcode.ADD)  # Default
    
    def emit_int_comparison(self, op: str):
        """Emit type-specialized integer comparison"""
        op_map = {
            '<': SpecializedOpcode.LT_INT,
            '<=': SpecializedOpcode.LE_INT,
            '>': SpecializedOpcode.GT_INT,
            '>=': SpecializedOpcode.GE_INT,
            '==': SpecializedOpcode.EQ_INT,
            '!=': SpecializedOpcode.NE_INT,
        }
        
        if op in op_map:
            self.emit_specialized(op_map[op])
        else:
            # Fall back to generic
            self.emit(Opcode.LT)  # Default
    
    def emit_local_load(self, index: int):
        """Emit optimized local variable load"""
        if index == 0:
            self.emit_specialized(SpecializedOpcode.LOAD_LOCAL_0)
        elif index == 1:
            self.emit_specialized(SpecializedOpcode.LOAD_LOCAL_1)
        elif index == 2:
            self.emit_specialized(SpecializedOpcode.LOAD_LOCAL_2)
        elif index == 3:
            self.emit_specialized(SpecializedOpcode.LOAD_LOCAL_3)
        else:
            self.emit(Opcode.LOAD, index)
    
    def emit_local_store(self, index: int):
        """Emit optimized local variable store"""
        if index == 0:
            self.emit_specialized(SpecializedOpcode.STORE_LOCAL_0)
        elif index == 1:
            self.emit_specialized(SpecializedOpcode.STORE_LOCAL_1)
        elif index == 2:
            self.emit_specialized(SpecializedOpcode.STORE_LOCAL_2)
        elif index == 3:
            self.emit_specialized(SpecializedOpcode.STORE_LOCAL_3)
        else:
            self.emit(Opcode.STORE, index)
    
    def get_chunk(self) -> BytecodeChunk:
        """Get the bytecode chunk"""
        return self.chunk
    
    def emit_jump(self, opcode: Union[Opcode, SpecializedOpcode]) -> int:
        """Emit a jump instruction and return position for backpatching"""
        pos = len(self.chunk.instructions)
        self.chunk.instructions.append(int(opcode))
        self.chunk.instructions.append(0)  # Placeholder
        return pos
    
    def patch_jump(self, pos: int, target: int):
        """Patch a jump instruction with target"""
        self.chunk.instructions[pos + 1] = target
    
    def current_position(self) -> int:
        """Get current instruction position"""
        return len(self.chunk.instructions)


class SpecializedVM:
    """VM with support for type-specialized instructions"""
    
    def __init__(self):
        self.stack: List[Any] = []
        self.frames: List[Dict[str, Any]] = []
        self.constants: List[Any] = []
    
    def execute(self, chunk: BytecodeChunk) -> Any:
        """Execute bytecode with specialized instructions"""
        self.constants = chunk.constants
        pc = 0
        locals_dict: Dict[int, Any] = {}
        
        while pc < len(chunk.instructions):
            opcode = SpecializedOpcode(chunk.instructions[pc])
            pc += 1
            
            # Specialized push instructions
            if opcode == SpecializedOpcode.PUSH_INT_0:
                self.stack.append(0)
            
            elif opcode == SpecializedOpcode.PUSH_INT_1:
                self.stack.append(1)
            
            elif opcode == SpecializedOpcode.PUSH_INT_2:
                self.stack.append(2)
            
            elif opcode == SpecializedOpcode.PUSH_INT_SMALL:
                # Sign-extend from 8-bit
                value = chunk.instructions[pc]
                if value > 127:
                    value = value - 256
                self.stack.append(value)
                pc += 1
            
            elif opcode == SpecializedOpcode.PUSH_TRUE:
                self.stack.append(True)
            
            elif opcode == SpecializedOpcode.PUSH_FALSE:
                self.stack.append(False)
            
            elif opcode == SpecializedOpcode.PUSH_NIL:
                self.stack.append(None)
            
            # Type-specialized integer arithmetic
            elif opcode == SpecializedOpcode.ADD_INT:
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.append(a + b)  # No type checking needed
            
            elif opcode == SpecializedOpcode.SUB_INT:
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.append(a - b)
            
            elif opcode == SpecializedOpcode.MUL_INT:
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.append(a * b)
            
            elif opcode == SpecializedOpcode.DIV_INT:
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.append(a // b)  # Integer division
            
            elif opcode == SpecializedOpcode.MOD_INT:
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.append(a % b)
            
            # Type-specialized integer comparison
            elif opcode == SpecializedOpcode.LT_INT:
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.append(a < b)
            
            elif opcode == SpecializedOpcode.GT_INT:
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.append(a > b)
            
            elif opcode == SpecializedOpcode.EQ_INT:
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.append(a == b)
            
            # Fast local variable access
            elif opcode == SpecializedOpcode.LOAD_LOCAL_0:
                self.stack.append(locals_dict.get(0))
            
            elif opcode == SpecializedOpcode.LOAD_LOCAL_1:
                self.stack.append(locals_dict.get(1))
            
            elif opcode == SpecializedOpcode.LOAD_LOCAL_2:
                self.stack.append(locals_dict.get(2))
            
            elif opcode == SpecializedOpcode.LOAD_LOCAL_3:
                self.stack.append(locals_dict.get(3))
            
            elif opcode == SpecializedOpcode.STORE_LOCAL_0:
                locals_dict[0] = self.stack.pop()
            
            elif opcode == SpecializedOpcode.STORE_LOCAL_1:
                locals_dict[1] = self.stack.pop()
            
            elif opcode == SpecializedOpcode.STORE_LOCAL_2:
                locals_dict[2] = self.stack.pop()
            
            elif opcode == SpecializedOpcode.STORE_LOCAL_3:
                locals_dict[3] = self.stack.pop()
            
            # Optimized control flow
            elif opcode == SpecializedOpcode.JUMP_IF_TRUE_INT:
                offset = chunk.instructions[pc]
                pc += 1
                value = self.stack.pop()
                if value != 0:  # Non-zero is true
                    pc = offset
            
            elif opcode == SpecializedOpcode.JUMP_IF_FALSE_INT:
                offset = chunk.instructions[pc]
                pc += 1
                value = self.stack.pop()
                if value == 0:  # Zero is false
                    pc = offset
            
            # Fall back to original opcodes for unhandled cases
            elif opcode == SpecializedOpcode.PUSH:
                const_idx = chunk.instructions[pc]
                pc += 1
                self.stack.append(self.constants[const_idx])
            
            elif opcode == SpecializedOpcode.HALT:
                break
            
            # Handle other opcodes...
            else:
                # For demonstration, just continue
                pass
        
        return self.stack[-1] if self.stack else None


def demonstrate_specialized_bytecode():
    """Demonstrate specialized bytecode generation"""
    print("Type-Specialized Bytecode Demo")
    print("=" * 60)
    
    # Example 1: Integer arithmetic
    print("\nExample 1: Integer arithmetic")
    print("-" * 40)
    
    builder = SpecializedBytecodeBuilder()
    
    # (+ 2 3)
    print("Expression: (+ 2 3)")
    print("\nUnoptimized:")
    print("  PUSH 0  ; constant 2")
    print("  PUSH 1  ; constant 3")
    print("  ADD")
    
    print("\nOptimized:")
    builder.emit_int_constant(2)
    builder.emit_int_constant(3)
    builder.emit_int_arithmetic('+')
    builder.emit_specialized(SpecializedOpcode.HALT)
    
    chunk = builder.get_chunk()
    for i in range(0, len(chunk.instructions), 2):
        op = SpecializedOpcode(chunk.instructions[i])
        if i + 1 < len(chunk.instructions) and op == SpecializedOpcode.PUSH_INT_SMALL:
            print(f"  {op.name} {chunk.instructions[i+1]}")
        else:
            print(f"  {op.name}")
    
    # Example 2: Local variables
    print("\n\nExample 2: Local variable optimization")
    print("-" * 40)
    print("Expression: (let ((x 10) (y 20)) (+ x y))")
    
    builder2 = SpecializedBytecodeBuilder()
    
    print("\nUnoptimized:")
    print("  PUSH 0  ; constant 10")
    print("  STORE 0 ; x")
    print("  PUSH 1  ; constant 20")
    print("  STORE 1 ; y")
    print("  LOAD 0  ; x")
    print("  LOAD 1  ; y")
    print("  ADD")
    
    print("\nOptimized:")
    builder2.emit_int_constant(10)
    builder2.emit_local_store(0)
    builder2.emit_int_constant(20)
    builder2.emit_local_store(1)
    builder2.emit_local_load(0)
    builder2.emit_local_load(1)
    builder2.emit_int_arithmetic('+')
    
    chunk2 = builder2.get_chunk()
    for i in range(0, len(chunk2.instructions)):
        op = SpecializedOpcode(chunk2.instructions[i])
        print(f"  {op.name}")
    
    # Show size comparison
    print("\n\nSize comparison:")
    print(f"  Unoptimized: ~14 bytes")
    print(f"  Optimized:   {len(chunk2.instructions)} bytes")
    print(f"  Reduction:   {int((1 - len(chunk2.instructions)/14) * 100)}%")


if __name__ == "__main__":
    demonstrate_specialized_bytecode()