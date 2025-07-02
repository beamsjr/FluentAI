"""
VM with improved generational garbage collection

This module provides a garbage-collected version of the ClaudeLang VM
that uses the improved generational GC for better performance.
"""

from typing import List, Any, Dict, Optional, Tuple
from dataclasses import dataclass
import time

from .vm import VM, CallFrame, VMError
from .bytecode import BytecodeChunk, Opcode, Instruction
from .gc_integration import (
    VMGarbageCollector, VMValue, VMEnvironment, VMClosure, VMList
)


class GenerationalGCVM(VM):
    """Virtual machine with generational garbage collection"""
    
    def __init__(self, stack_size: int = 10000, enable_jit: bool = True,
                 young_size: int = 1000, young_threshold: int = 100,
                 old_threshold: int = 10, promotion_age: int = 3):
        super().__init__(stack_size, enable_jit)
        
        # Initialize generational GC
        self.gc = VMGarbageCollector(
            young_size=young_size,
            young_threshold=young_threshold,
            old_threshold=old_threshold,
            promotion_age=promotion_age
        )
        
        # Use GC-managed global environment
        self.global_env = self.gc.allocate_environment()
        self.gc.add_root(self.global_env)
        
        # Stack now holds VMValue objects
        self.stack: List[VMValue] = []
        self.current_env: Optional[VMEnvironment] = self.global_env
    
    def enable_gc(self, enabled: bool = True) -> None:
        """Enable or disable garbage collection"""
        self.gc.gc.write_barrier_enabled = enabled
    
    def gc_collect(self) -> Dict[str, Any]:
        """Force garbage collection and return stats"""
        self.gc.collect()
        return self.gc.get_statistics()
    
    def gc_collect_full(self) -> Dict[str, Any]:
        """Force full garbage collection and return stats"""
        self.gc.collect_full()
        return self.gc.get_statistics()
    
    def _push(self, value: Any) -> None:
        """Push value onto stack with GC management"""
        # Convert to VMValue if needed
        if isinstance(value, VMValue):
            vm_value = value
        else:
            vm_value = self.gc.allocate_value(value)
        
        vm_value.incref()  # Stack holds a reference
        self.stack.append(vm_value)
        
        if len(self.stack) > self.max_stack_size:
            raise VMError("Stack overflow")
    
    def _pop(self) -> Any:
        """Pop value from stack with GC management"""
        if not self.stack:
            raise VMError("Stack underflow")
        
        vm_value = self.stack.pop()
        data = vm_value.data
        vm_value.decref()  # Stack no longer holds reference
        
        return data
    
    def _execute_instruction(self, instruction: Instruction):
        """Execute instruction with GC support"""
        opcode = instruction.opcode
        arg = instruction.arg
        
        # Stack manipulation
        if opcode == Opcode.PUSH:
            self._push(self.chunk.constants[arg])
        
        elif opcode == Opcode.POP:
            self._pop()
        
        elif opcode == Opcode.DUP:
            if self.stack:
                top = self.stack[-1]
                self._push(top.data)
        
        # Arithmetic operations
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
                raise VMError("Division by zero")
            self._push(a / b)
        
        # List operations with GC
        elif opcode == Opcode.MAKE_LIST:
            count = arg
            elements = []
            for _ in range(count):
                elem = self._pop()
                vm_elem = self.gc.allocate_value(elem)
                elements.insert(0, vm_elem)
            
            vm_list = self.gc.allocate_list(elements)
            self._push(vm_list)
        
        elif opcode == Opcode.LIST_HEAD:  # Get first element
            lst = self._pop()
            if isinstance(lst, VMList):
                if lst.elements:
                    self._push(lst.elements[0].data)
                else:
                    raise VMError("Cannot get head of empty list")
            else:
                raise VMError(f"Expected list, got {type(lst)}")
        
        # Environment operations with GC
        elif opcode == Opcode.LOAD_GLOBAL:
            name = self.chunk.constants[arg]
            value = self.global_env.lookup(name)
            if value:
                self._push(value.data)
            else:
                raise VMError(f"Undefined global: {name}")
        
        elif opcode == Opcode.STORE_GLOBAL:
            name = self.chunk.constants[arg]
            value = self._pop()
            vm_value = self.gc.allocate_value(value)
            self.global_env.bind(name, vm_value, self.gc.gc)
        
        elif opcode == Opcode.LOAD:
            name = self.chunk.constants[arg]
            if self.current_env:
                value = self.current_env.lookup(name)
                if value:
                    self._push(value.data)
                else:
                    raise VMError(f"Undefined local: {name}")
            else:
                raise VMError("No current environment")
        
        elif opcode == Opcode.STORE:
            name = self.chunk.constants[arg]
            value = self._pop()
            if self.current_env:
                vm_value = self.gc.allocate_value(value)
                self.current_env.bind(name, vm_value, self.gc.gc)
        
        # Function operations with GC
        elif opcode == Opcode.MAKE_FUNC:
            # arg points to function info in constants
            func_info = self.chunk.constants[arg]
            
            # Create GC closure
            closure = self.gc.allocate_closure(
                params=func_info.get('params', []),
                body_id=func_info.get('body_id', ''),
                env=self.current_env or self.global_env,
                name=func_info.get('name', '<anonymous>')
            )
            self._push(closure)
        
        elif opcode == Opcode.CALL:
            num_args = arg
            
            # Get function and arguments
            args = []
            for _ in range(num_args):
                args.insert(0, self._pop())
            
            func = self._pop()
            
            if isinstance(func, VMClosure):
                # Create new environment for call
                call_env = self.gc.allocate_environment(parent=func.captured_env)
                
                # Bind parameters
                for i, param in enumerate(func.params):
                    if i < len(args):
                        vm_value = self.gc.allocate_value(args[i])
                        call_env.bind(param, vm_value, self.gc.gc)
                
                # Save current environment
                old_env = self.current_env
                self.current_env = call_env
                
                # Execute function body (simplified - in real impl would jump to body)
                # For now, just restore environment
                self.current_env = old_env
                
                # Push dummy result
                self._push(None)
            else:
                raise VMError(f"Cannot call non-function: {type(func)}")
        
        # Control flow
        elif opcode == Opcode.JUMP:
            self.ip = arg - 1  # -1 because ip will be incremented
        
        elif opcode == Opcode.JUMP_IF_NOT:
            condition = self._pop()
            if not condition:
                self.ip = arg - 1
        
        elif opcode == Opcode.RETURN:
            # Return from function
            pass
        
        # Comparisons
        elif opcode == Opcode.EQ:
            b = self._pop()
            a = self._pop()
            self._push(a == b)
        
        elif opcode == Opcode.LT:
            b = self._pop()
            a = self._pop()
            self._push(a < b)
        
        elif opcode == Opcode.GT:
            b = self._pop()
            a = self._pop()
            self._push(a > b)
        
        else:
            # Let parent handle other opcodes
            super()._execute_instruction(instruction)
    
    def execute(self, chunk: BytecodeChunk, function_id: Optional[str] = None) -> Any:
        """Execute bytecode with GC support"""
        try:
            # Clear stack references before execution
            for vm_value in self.stack:
                vm_value.decref()
            self.stack.clear()
            
            # Execute bytecode
            self.chunk = chunk
            self.ip = 0
            
            while self.ip < len(chunk.instructions):
                instruction = chunk.instructions[self.ip]
                self._execute_instruction(instruction)
                self.ip += 1
                
                # Check if return was executed
                if instruction.opcode == Opcode.RETURN:
                    break
            
            # Get result from stack if any
            result = None
            if self.stack:
                result = self._pop()
            
            return result
            
        except Exception as e:
            # Clean up stack on error
            for vm_value in self.stack:
                vm_value.decref()
            self.stack.clear()
            raise
    
    def __del__(self):
        """Clean up when VM is destroyed"""
        # Remove global environment from roots
        if hasattr(self, 'global_env'):
            self.gc.remove_root(self.global_env)
        
        # Clear stack references
        if hasattr(self, 'stack'):
            for vm_value in self.stack:
                vm_value.decref()
            self.stack.clear()
        
        # Shutdown GC
        if hasattr(self, 'gc'):
            self.gc.shutdown()


def create_generational_gc_vm(**kwargs) -> GenerationalGCVM:
    """Factory function to create a generational GC-enabled VM"""
    return GenerationalGCVM(**kwargs)