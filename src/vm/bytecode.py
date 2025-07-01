"""
ClaudeLang Bytecode Definitions

Stack-based bytecode for efficient execution.
"""

from enum import IntEnum, auto
from dataclasses import dataclass
from typing import Any, List, Optional


class Opcode(IntEnum):
    """Bytecode opcodes for ClaudeLang VM"""
    
    # Stack manipulation
    PUSH = auto()       # Push constant onto stack
    POP = auto()        # Pop and discard top
    DUP = auto()        # Duplicate top
    SWAP = auto()       # Swap top two elements
    
    # Arithmetic
    ADD = auto()        # Pop 2, push sum
    SUB = auto()        # Pop 2, push difference
    MUL = auto()        # Pop 2, push product
    DIV = auto()        # Pop 2, push quotient
    MOD = auto()        # Pop 2, push remainder
    NEG = auto()        # Pop 1, push negation
    
    # Comparison
    EQ = auto()         # Pop 2, push equality
    NE = auto()         # Pop 2, push inequality
    LT = auto()         # Pop 2, push less than
    LE = auto()         # Pop 2, push less or equal
    GT = auto()         # Pop 2, push greater than
    GE = auto()         # Pop 2, push greater or equal
    
    # Boolean
    AND = auto()        # Pop 2, push logical and
    OR = auto()         # Pop 2, push logical or
    NOT = auto()        # Pop 1, push logical not
    
    # Control flow
    JUMP = auto()       # Unconditional jump
    JUMP_IF = auto()    # Pop 1, jump if true
    JUMP_IF_NOT = auto()# Pop 1, jump if false
    CALL = auto()       # Function call
    RETURN = auto()     # Return from function
    
    # Variables
    LOAD = auto()       # Load variable
    STORE = auto()      # Store variable
    LOAD_GLOBAL = auto()# Load global
    STORE_GLOBAL = auto()# Store global
    
    # Functions
    MAKE_FUNC = auto()  # Create closure
    MAKE_ENV = auto()   # Create new environment
    POP_ENV = auto()    # Restore previous environment
    
    # Lists
    MAKE_LIST = auto()  # Create list from N stack items
    LIST_HEAD = auto()  # Get first element
    LIST_TAIL = auto()  # Get rest of list
    LIST_CONS = auto()  # Cons element to list
    LIST_LEN = auto()   # Get list length
    LIST_EMPTY = auto() # Check if list empty
    
    # Strings
    STR_LEN = auto()    # String length
    STR_CONCAT = auto() # Concatenate strings
    STR_UPPER = auto()  # Uppercase string
    STR_LOWER = auto()  # Lowercase string
    
    # Effects
    EFFECT = auto()     # Trigger effect
    
    # Special
    HALT = auto()       # Stop execution
    NOP = auto()        # No operation


@dataclass
class Instruction:
    """Single bytecode instruction"""
    opcode: Opcode
    arg: Optional[Any] = None
    
    def __repr__(self):
        if self.arg is not None:
            return f"{self.opcode.name} {self.arg}"
        return self.opcode.name


@dataclass
class BytecodeChunk:
    """A chunk of bytecode with constants"""
    instructions: List[Instruction]
    constants: List[Any]
    
    def add_instruction(self, opcode: Opcode, arg: Optional[Any] = None):
        """Add an instruction"""
        self.instructions.append(Instruction(opcode, arg))
    
    def add_constant(self, value: Any) -> int:
        """Add a constant and return its index"""
        # Reuse existing constants
        if value in self.constants:
            return self.constants.index(value)
        
        self.constants.append(value)
        return len(self.constants) - 1
    
    def disassemble(self) -> str:
        """Disassemble bytecode to readable format"""
        lines = ["=== Bytecode ==="]
        
        # Constants
        if self.constants:
            lines.append("\nConstants:")
            for i, const in enumerate(self.constants):
                lines.append(f"  [{i}] {repr(const)}")
        
        # Instructions
        lines.append("\nInstructions:")
        for i, instr in enumerate(self.instructions):
            lines.append(f"  {i:04d}: {instr}")
        
        return "\n".join(lines)


class BytecodeBuilder:
    """Helper for building bytecode"""
    
    def __init__(self):
        self.chunk = BytecodeChunk([], [])
    
    def emit(self, opcode: Opcode, arg: Optional[Any] = None):
        """Emit an instruction"""
        self.chunk.add_instruction(opcode, arg)
    
    def emit_constant(self, value: Any):
        """Emit a constant push"""
        idx = self.chunk.add_constant(value)
        self.emit(Opcode.PUSH, idx)
    
    def emit_jump(self, opcode: Opcode) -> int:
        """Emit a jump instruction and return position for backpatching"""
        pos = len(self.chunk.instructions)
        self.emit(opcode, 0)  # Placeholder
        return pos
    
    def patch_jump(self, pos: int, target: int):
        """Patch a jump instruction with target"""
        self.chunk.instructions[pos].arg = target
    
    def current_position(self) -> int:
        """Get current instruction position"""
        return len(self.chunk.instructions)
    
    def get_chunk(self) -> BytecodeChunk:
        """Get the completed chunk"""
        return self.chunk