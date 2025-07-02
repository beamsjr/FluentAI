"""
ClaudeLang Virtual Machine

Stack-based VM for executing bytecode with JIT compilation support.
"""

from typing import List, Any, Dict, Optional, Tuple
from dataclasses import dataclass
import time
from .bytecode import BytecodeChunk, Opcode, Instruction
from ..jit import JITCompiler


@dataclass
class CallFrame:
    """Call frame for function calls"""
    return_address: int
    base_pointer: int
    function: Optional[Any] = None


class VMError(Exception):
    """VM execution error"""
    pass


class VM:
    """Stack-based virtual machine with JIT compilation"""
    
    def __init__(self, stack_size: int = 10000, enable_jit: bool = True):
        self.stack: List[Any] = []
        self.call_stack: List[CallFrame] = []
        self.globals: Dict[str, Any] = {}
        self.ip = 0  # Instruction pointer
        self.chunk: Optional[BytecodeChunk] = None
        self.max_stack_size = stack_size
        self.enable_jit = enable_jit
        self.jit_compiler = JITCompiler() if enable_jit else None
        self.current_function_id: Optional[str] = None
        self.function_entry_time: Optional[float] = None
        self.tracer: Optional[Any] = None  # ExecutionTracer
    
    def execute(self, chunk: BytecodeChunk, function_id: Optional[str] = None) -> Any:
        """Execute bytecode chunk with optional JIT compilation"""
        self.chunk = chunk
        self.ip = 0
        self.stack.clear()
        self.call_stack.clear()
        
        # Track function execution for JIT profiling
        if self.enable_jit and function_id:
            self.current_function_id = function_id
            self.function_entry_time = time.perf_counter()
        
        try:
            result = self._run_bytecode()
            
            # Profile execution if JIT is enabled
            if self.enable_jit and function_id and self.function_entry_time:
                execution_time = time.perf_counter() - self.function_entry_time
                self.jit_compiler.profile_execution(
                    function_id, 
                    execution_time, 
                    tuple(self.stack[:5]),  # Sample args from stack
                    result  # Return value
                )
                
                # Check if we should compile this function
                if self.jit_compiler.should_compile(function_id):
                    # For now, we'll need the AST graph to compile
                    # This would be passed in from the caller
                    pass
            
            return result
            
        except Exception as e:
            raise VMError(f"VM error at instruction {self.ip}: {e}") from e
    
    def _run_bytecode(self) -> Any:
        """Run bytecode instructions"""
        while self.ip < len(self.chunk.instructions):
            instruction = self.chunk.instructions[self.ip]
            self._execute_instruction(instruction)
            self.ip += 1
        
        # Return top of stack if any
        return self.stack[-1] if self.stack else None
    
    def _execute_instruction(self, instruction: Instruction):
        """Execute a single instruction"""
        opcode = instruction.opcode
        arg = instruction.arg
        
        # Stack manipulation
        if opcode == Opcode.PUSH:
            self._push(self.chunk.constants[arg])
        
        elif opcode == Opcode.POP:
            self._pop()
        
        elif opcode == Opcode.DUP:
            self._push(self._peek())
        
        elif opcode == Opcode.SWAP:
            self.stack[-1], self.stack[-2] = self.stack[-2], self.stack[-1]
        
        # Arithmetic
        elif opcode == Opcode.ADD:
            b = self._pop()
            a = self._pop()
            self._push(a + b)
        
        elif opcode == Opcode.SUB:
            b = self._pop()
            a = self._pop()
            self._push(a - b)
        
        elif opcode == Opcode.MUL:
            b = self._pop()
            a = self._pop()
            self._push(a * b)
        
        elif opcode == Opcode.DIV:
            b = self._pop()
            a = self._pop()
            if b == 0:
                self._push({"error": "Division by zero"})
            else:
                self._push(a / b)
        
        elif opcode == Opcode.MOD:
            b = self._pop()
            a = self._pop()
            if b == 0:
                self._push({"error": "Modulo by zero"})
            else:
                self._push(a % b)
        
        elif opcode == Opcode.NEG:
            self._push(-self._pop())
        
        # Comparison
        elif opcode == Opcode.EQ:
            b = self._pop()
            a = self._pop()
            self._push(a == b)
        
        elif opcode == Opcode.NE:
            b = self._pop()
            a = self._pop()
            self._push(a != b)
        
        elif opcode == Opcode.LT:
            b = self._pop()
            a = self._pop()
            self._push(a < b)
        
        elif opcode == Opcode.LE:
            b = self._pop()
            a = self._pop()
            self._push(a <= b)
        
        elif opcode == Opcode.GT:
            b = self._pop()
            a = self._pop()
            self._push(a > b)
        
        elif opcode == Opcode.GE:
            b = self._pop()
            a = self._pop()
            self._push(a >= b)
        
        # Boolean
        elif opcode == Opcode.AND:
            b = self._pop()
            a = self._pop()
            self._push(a and b)
        
        elif opcode == Opcode.OR:
            b = self._pop()
            a = self._pop()
            self._push(a or b)
        
        elif opcode == Opcode.NOT:
            self._push(not self._pop())
        
        # Control flow
        elif opcode == Opcode.JUMP:
            self.ip = arg - 1  # -1 because ip will be incremented
        
        elif opcode == Opcode.JUMP_IF:
            if self._pop():
                self.ip = arg - 1
        
        elif opcode == Opcode.JUMP_IF_NOT:
            if not self._pop():
                self.ip = arg - 1
        
        elif opcode == Opcode.CALL:
            # Simple function call (lambdas not fully implemented yet)
            arity = arg
            func = self.stack[-(arity + 1)]
            args = [self.stack[-i] for i in range(arity, 0, -1)]
            
            # Pop function and args
            for _ in range(arity + 1):
                self._pop()
            
            # For now, just handle built-in functions
            if callable(func):
                result = func(*args)
                self._push(result)
            else:
                raise VMError(f"Cannot call non-function: {func}")
        
        elif opcode == Opcode.RETURN:
            # Return from function
            if self.call_stack:
                frame = self.call_stack.pop()
                self.ip = frame.return_address
        
        # Variables
        elif opcode == Opcode.LOAD:
            # Load from stack offset
            self._push(self.stack[arg])
        
        elif opcode == Opcode.STORE:
            # Store at stack offset
            value = self._peek()
            if arg >= len(self.stack):
                self.stack.extend([None] * (arg - len(self.stack) + 1))
            self.stack[arg] = value
        
        # Lists - optimized for native Python lists
        elif opcode == Opcode.LIST_CONS:
            lst = self._pop()
            elem = self._pop()
            if isinstance(lst, list):
                # Efficient: create new list without copying all elements
                result = [elem]
                result.extend(lst)
                self._push(result)
            else:
                self._push([elem])
        
        elif opcode == Opcode.LIST_HEAD:
            lst = self._pop()
            if isinstance(lst, list) and lst:
                self._push(lst[0])
            else:
                self._push({"error": "Empty list"})
        
        elif opcode == Opcode.LIST_TAIL:
            lst = self._pop()
            if isinstance(lst, list) and lst:
                # Note: This creates a copy, but it's still O(n)
                self._push(lst[1:])
            else:
                self._push({"error": "Empty list"})
        
        elif opcode == Opcode.LIST_LEN:
            lst = self._pop()
            # O(1) for Python lists!
            self._push(len(lst) if isinstance(lst, list) else 0)
        
        elif opcode == Opcode.LIST_EMPTY:
            lst = self._pop()
            # O(1) check
            self._push(len(lst) == 0 if isinstance(lst, list) else True)
        
        elif opcode == Opcode.MAKE_LIST:
            # Make list from N stack items
            n = arg
            items = []
            for _ in range(n):
                items.append(self._pop())
            items.reverse()
            self._push(items)
        
        # Strings
        elif opcode == Opcode.STR_LEN:
            s = self._pop()
            self._push(len(s) if isinstance(s, str) else 0)
        
        elif opcode == Opcode.STR_CONCAT:
            b = self._pop()
            a = self._pop()
            self._push(str(a) + str(b))
        
        elif opcode == Opcode.STR_UPPER:
            s = self._pop()
            self._push(s.upper() if isinstance(s, str) else "")
        
        elif opcode == Opcode.STR_LOWER:
            s = self._pop()
            self._push(s.lower() if isinstance(s, str) else "")
        
        # Special
        elif opcode == Opcode.HALT:
            self.ip = len(self.chunk.instructions)  # Stop execution
        
        elif opcode == Opcode.NOP:
            pass  # No operation
        
        else:
            raise VMError(f"Unknown opcode: {opcode}")
    
    def _push(self, value: Any):
        """Push value onto stack"""
        if len(self.stack) >= self.max_stack_size:
            raise VMError("Stack overflow")
        self.stack.append(value)
    
    def _pop(self) -> Any:
        """Pop value from stack"""
        if not self.stack:
            raise VMError("Stack underflow")
        return self.stack.pop()
    
    def set_tracer(self, tracer: Any):
        """Set execution tracer"""
        self.tracer = tracer
    
    def reset(self):
        """Reset VM state"""
        self.stack.clear()
        self.call_stack.clear()
        self.ip = 0
        self.chunk = None
    
    def load_bytecode(self, chunk: BytecodeChunk):
        """Load bytecode chunk"""
        self.chunk = chunk
        self.ip = 0
    
    def push(self, value: Any):
        """Push value onto stack (public API)"""
        self._push(value)
    
    def run(self) -> Any:
        """Run loaded bytecode"""
        if not self.chunk:
            raise VMError("No bytecode loaded")
        
        while self.ip < len(self.chunk.instructions):
            self._execute_instruction()
            self.ip += 1
        
        # Return top of stack if anything
        return self.stack[-1] if self.stack else None
    
    def _peek(self, offset: int = 0) -> Any:
        """Peek at stack value"""
        idx = -(offset + 1)
        if abs(idx) > len(self.stack):
            raise VMError("Stack underflow")
        return self.stack[idx]
    
    def get_jit_stats(self) -> Dict[str, Any]:
        """Get JIT compilation statistics"""
        if self.jit_compiler:
            return self.jit_compiler.get_compilation_stats()
        return {"jit_enabled": False}