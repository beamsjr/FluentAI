"""
VM with integrated garbage collection

This module provides a garbage-collected version of the ClaudeLang VM
that automatically manages memory for all allocated objects.
"""

from typing import List, Any, Dict, Optional, Tuple
from dataclasses import dataclass
import time

from .vm import VM, CallFrame, VMError
from .bytecode import BytecodeChunk, Opcode, Instruction
from .gc import (
    GCValue, GCEnvironment, GCClosure, GCList, GCObject,
    gc_allocate, gc_add_root, gc_remove_root, gc_collect, gc_stats
)


class GCVM(VM):
    """Virtual machine with integrated garbage collection"""
    
    def __init__(self, stack_size: int = 10000, enable_jit: bool = True, gc_threshold: int = 100):
        super().__init__(stack_size, enable_jit)
        self.gc_threshold = gc_threshold
        self.gc_enabled = True
        
        # Use GC-managed global environment
        self.global_env = GCEnvironment()
        gc_allocate(self.global_env)
        gc_add_root(self.global_env)
        
        # Stack now holds GCValue objects
        self.stack: List[GCValue] = []
        self.current_env: Optional[GCEnvironment] = self.global_env
    
    def enable_gc(self, enabled: bool = True) -> None:
        """Enable or disable garbage collection"""
        self.gc_enabled = enabled
    
    def gc_collect(self) -> Dict[str, int]:
        """Force garbage collection and return stats"""
        gc_collect()
        return gc_stats()
    
    def _wrap_value(self, value: Any) -> GCValue:
        """Wrap a Python value in a GCValue"""
        # Check if already wrapped
        if isinstance(value, GCValue):
            return value
        
        # Check if it's a GC object that needs wrapping
        if isinstance(value, GCObject):
            gc_value = GCValue(data=value)
            gc_allocate(gc_value)
            return gc_value
        
        # Wrap primitive values
        gc_value = GCValue(data=value)
        gc_allocate(gc_value)
        return gc_value
    
    def _unwrap_value(self, gc_value: GCValue) -> Any:
        """Unwrap a GCValue to get the Python value"""
        return gc_value.data
    
    def _push(self, value: Any) -> None:
        """Push value onto stack with GC management"""
        gc_value = self._wrap_value(value)
        gc_value.incref()  # Stack holds a reference
        self.stack.append(gc_value)
        
        if len(self.stack) > self.max_stack_size:
            raise VMError("Stack overflow")
    
    def _pop(self) -> Any:
        """Pop value from stack with GC management"""
        if not self.stack:
            raise VMError("Stack underflow")
        
        gc_value = self.stack.pop()
        value = self._unwrap_value(gc_value)
        gc_value.decref()  # Stack no longer holds reference
        
        # If refcount is 0, it will be collected
        if gc_value.ref_count == 0 and self.gc_enabled:
            # The main GC will handle this
            pass
        
        return value
    
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
                self._push(self._unwrap_value(top))
        
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
        elif opcode == Opcode.BUILD_LIST:
            count = arg
            elements = []
            for _ in range(count):
                elem = self._pop()
                gc_elem = self._wrap_value(elem)
                elements.insert(0, gc_elem)
            
            gc_list = GCList(elements=elements)
            gc_allocate(gc_list)
            self._push(gc_list)
        
        elif opcode == Opcode.LIST_GET:
            index = self._pop()
            lst = self._pop()
            if isinstance(lst, GCList):
                if 0 <= index < len(lst.elements):
                    self._push(self._unwrap_value(lst.elements[index]))
                else:
                    raise VMError(f"List index out of range: {index}")
            else:
                raise VMError(f"Expected list, got {type(lst)}")
        
        # Environment operations with GC
        elif opcode == Opcode.LOAD_GLOBAL:
            name = self.chunk.constants[arg]
            if name in self.global_env.bindings:
                value = self.global_env.bindings[name]
                self._push(self._unwrap_value(value))
            else:
                raise VMError(f"Undefined global: {name}")
        
        elif opcode == Opcode.STORE_GLOBAL:
            name = self.chunk.constants[arg]
            value = self._pop()
            gc_value = self._wrap_value(value)
            self.global_env.bind(name, gc_value)
        
        elif opcode == Opcode.LOAD_LOCAL:
            name = self.chunk.constants[arg]
            if self.current_env and name in self.current_env.bindings:
                value = self.current_env.bindings[name]
                self._push(self._unwrap_value(value))
            else:
                raise VMError(f"Undefined local: {name}")
        
        elif opcode == Opcode.STORE_LOCAL:
            name = self.chunk.constants[arg]
            value = self._pop()
            if self.current_env:
                gc_value = self._wrap_value(value)
                self.current_env.bind(name, gc_value)
        
        # Function operations with GC
        elif opcode == Opcode.MAKE_CLOSURE:
            # arg points to function info in constants
            func_info = self.chunk.constants[arg]
            
            # Create GC closure
            closure = GCClosure(
                params=func_info.get('params', []),
                body_id=func_info.get('body_id', ''),
                captured_env=self.current_env or self.global_env,
                name=func_info.get('name', '<anonymous>')
            )
            gc_allocate(closure)
            self._push(closure)
        
        elif opcode == Opcode.CALL:
            num_args = arg
            
            # Get function and arguments
            args = []
            for _ in range(num_args):
                args.insert(0, self._pop())
            
            func = self._pop()
            
            if isinstance(func, GCClosure):
                # Create new environment for call
                call_env = func.captured_env.extend()
                gc_allocate(call_env)
                
                # Bind parameters
                for i, param in enumerate(func.params):
                    if i < len(args):
                        gc_value = self._wrap_value(args[i])
                        call_env.bind(param, gc_value)
                
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
        
        elif opcode == Opcode.JUMP_IF_FALSE:
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
            for gc_value in self.stack:
                gc_value.decref()
            self.stack.clear()
            
            # Execute bytecode
            result = super().execute(chunk, function_id)
            
            # Wrap result if needed
            if result is not None and not isinstance(result, GCValue):
                result = self._wrap_value(result)
            
            # Run GC after execution if enabled
            if self.gc_enabled:
                stats = gc_stats()
                if stats['live_objects'] > self.gc_threshold:
                    gc_collect()
            
            return result
            
        except Exception as e:
            # Clean up stack on error
            for gc_value in self.stack:
                gc_value.decref()
            self.stack.clear()
            raise
    
    def __del__(self):
        """Clean up when VM is destroyed"""
        # Remove global environment from roots
        if hasattr(self, 'global_env'):
            gc_remove_root(self.global_env)
        
        # Clear stack references
        for gc_value in self.stack:
            gc_value.decref()
        self.stack.clear()


def create_gc_vm(**kwargs) -> GCVM:
    """Factory function to create a GC-enabled VM"""
    return GCVM(**kwargs)