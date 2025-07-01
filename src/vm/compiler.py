"""
ClaudeLang Bytecode Compiler

Compiles AST to bytecode for efficient execution.
"""

from typing import Dict, List, Optional
from ..core.ast import *
from .bytecode import BytecodeBuilder, BytecodeChunk, Opcode


class CompilerContext:
    """Compilation context for tracking variables"""
    
    def __init__(self, parent: Optional['CompilerContext'] = None):
        self.parent = parent
        self.locals: Dict[str, int] = {}
        self.next_local = 0
    
    def define_local(self, name: str) -> int:
        """Define a local variable and return its index"""
        if name in self.locals:
            return self.locals[name]
        
        idx = self.next_local
        self.locals[name] = idx
        self.next_local += 1
        return idx
    
    def lookup(self, name: str) -> Optional[int]:
        """Look up a variable"""
        if name in self.locals:
            return self.locals[name]
        elif self.parent:
            return self.parent.lookup(name)
        return None
    
    def extend(self) -> 'CompilerContext':
        """Create a child context"""
        return CompilerContext(self)


class BytecodeCompiler:
    """Compiles ClaudeLang AST to bytecode"""
    
    def __init__(self):
        self.builder = BytecodeBuilder()
        self.context = CompilerContext()
        self.function_cache: Dict[str, int] = {}
    
    def compile(self, graph: Graph) -> BytecodeChunk:
        """Compile a graph to bytecode"""
        if not graph.root_id:
            raise ValueError("Graph has no root node")
        
        # Reset state
        self.builder = BytecodeBuilder()
        self.context = CompilerContext()
        
        # Compile from root
        self._compile_node(graph.root_id, graph)
        
        # Add halt instruction
        self.builder.emit(Opcode.HALT)
        
        return self.builder.get_chunk()
    
    def _compile_node(self, node_id: str, graph: Graph):
        """Compile a single node"""
        node = graph.get_node(node_id)
        if not node:
            raise ValueError(f"Node {node_id} not found")
        
        # Dispatch based on node type
        if isinstance(node, Literal):
            self._compile_literal(node)
        elif isinstance(node, Variable):
            self._compile_variable(node)
        elif isinstance(node, Function):
            self._compile_function(node)
        elif isinstance(node, Application):
            self._compile_application(node, graph)
        elif isinstance(node, Lambda):
            self._compile_lambda(node, graph)
        elif isinstance(node, Let):
            self._compile_let(node, graph)
        elif isinstance(node, If):
            self._compile_if(node, graph)
        elif isinstance(node, Sequence):
            self._compile_sequence(node, graph)
        else:
            raise NotImplementedError(f"Compilation for {type(node).__name__} not implemented")
    
    def _compile_literal(self, node: Literal):
        """Compile a literal"""
        self.builder.emit_constant(node.value)
    
    def _compile_variable(self, node: Variable):
        """Compile a variable reference"""
        # Try local lookup
        idx = self.context.lookup(node.name)
        if idx is not None:
            self.builder.emit(Opcode.LOAD, idx)
        else:
            # Try built-in functions
            if node.name in self._get_builtin_opcodes():
                # Will be handled by application
                self.builder.emit_constant(node.name)
            else:
                raise NameError(f"Unbound variable: {node.name}")
    
    def _compile_function(self, node: Function):
        """Compile a function reference"""
        # Built-in functions are handled specially
        self.builder.emit_constant(node.name)
    
    def _compile_application(self, node: Application, graph: Graph):
        """Compile function application"""
        # Get function node
        func_node = graph.get_node(node.function_id)
        
        # Special handling for built-in functions
        if isinstance(func_node, Function) and func_node.name in self._get_builtin_opcodes():
            # Compile arguments first
            for arg_id in node.argument_ids:
                self._compile_node(arg_id, graph)
            
            # Emit opcode for built-in
            opcode = self._get_builtin_opcodes()[func_node.name]
            self.builder.emit(opcode)
        
        elif isinstance(func_node, Variable) and func_node.name in self._get_builtin_opcodes():
            # Variable reference to built-in
            for arg_id in node.argument_ids:
                self._compile_node(arg_id, graph)
            
            opcode = self._get_builtin_opcodes()[func_node.name]
            self.builder.emit(opcode)
        
        else:
            # User-defined function
            # Compile function
            self._compile_node(node.function_id, graph)
            
            # Compile arguments
            for arg_id in node.argument_ids:
                self._compile_node(arg_id, graph)
            
            # Call with arity
            self.builder.emit(Opcode.CALL, len(node.argument_ids))
    
    def _compile_lambda(self, node: Lambda, graph: Graph):
        """Compile lambda to closure"""
        # Save current position
        jump_pos = self.builder.emit_jump(Opcode.JUMP)
        func_start = self.builder.current_position()
        
        # Create new context for lambda
        old_context = self.context
        self.context = self.context.extend()
        
        # Bind parameters
        for param in node.parameter_names:
            self.context.define_local(param)
        
        # Compile body
        self._compile_node(node.body_id, graph)
        self.builder.emit(Opcode.RETURN)
        
        # Restore context
        self.context = old_context
        
        # Patch jump
        self.builder.patch_jump(jump_pos, self.builder.current_position())
        
        # Create closure
        self.builder.emit(Opcode.MAKE_FUNC, func_start)
    
    def _compile_let(self, node: Let, graph: Graph):
        """Compile let binding"""
        # Create new context
        old_context = self.context
        self.context = self.context.extend()
        
        # First pass: define all names (for recursion)
        for binding in node.bindings:
            self.context.define_local(binding['name'])
        
        # Second pass: compile values
        for i, binding in enumerate(node.bindings):
            # Compile value
            self._compile_node(binding['value_id'], graph)
            
            # Store in local
            idx = self.context.locals[binding['name']]
            self.builder.emit(Opcode.STORE, idx)
        
        # Compile body
        self._compile_node(node.body_id, graph)
        
        # Restore context
        self.context = old_context
    
    def _compile_if(self, node: If, graph: Graph):
        """Compile conditional"""
        # Compile condition
        self._compile_node(node.condition_id, graph)
        
        # Jump if false
        false_jump = self.builder.emit_jump(Opcode.JUMP_IF_NOT)
        
        # Compile then branch
        self._compile_node(node.then_id, graph)
        
        # Jump over else
        end_jump = self.builder.emit_jump(Opcode.JUMP)
        
        # Patch false jump to else branch
        self.builder.patch_jump(false_jump, self.builder.current_position())
        
        # Compile else branch
        self._compile_node(node.else_id, graph)
        
        # Patch end jump
        self.builder.patch_jump(end_jump, self.builder.current_position())
    
    def _compile_sequence(self, node: Sequence, graph: Graph):
        """Compile sequence"""
        for i, step_id in enumerate(node.step_ids):
            self._compile_node(step_id, graph)
            
            # Pop intermediate results except last
            if i < len(node.step_ids) - 1:
                self.builder.emit(Opcode.POP)
    
    def _get_builtin_opcodes(self) -> Dict[str, Opcode]:
        """Map built-in functions to opcodes"""
        return {
            # Arithmetic
            '+': Opcode.ADD,
            '-': Opcode.SUB,
            '*': Opcode.MUL,
            '/': Opcode.DIV,
            'mod': Opcode.MOD,
            
            # Comparison
            '==': Opcode.EQ,
            '!=': Opcode.NE,
            '<': Opcode.LT,
            '<=': Opcode.LE,
            '>': Opcode.GT,
            '>=': Opcode.GE,
            
            # Boolean
            'and': Opcode.AND,
            'or': Opcode.OR,
            'not': Opcode.NOT,
            
            # Lists
            'cons': Opcode.LIST_CONS,
            'head': Opcode.LIST_HEAD,
            'tail': Opcode.LIST_TAIL,
            'length': Opcode.LIST_LEN,
            'empty?': Opcode.LIST_EMPTY,
            
            # Strings
            'string-length': Opcode.STR_LEN,
            'concat': Opcode.STR_CONCAT,
            'string-upcase': Opcode.STR_UPPER,
            'string-downcase': Opcode.STR_LOWER,
        }