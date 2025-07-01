"""
Optimizing Bytecode Compiler for ClaudeLang

Uses type inference to generate specialized bytecode instructions.
"""

from typing import Dict, Optional, Set
from ..core.ast import *
from ..types.type_inference import OptimizationTypeInferencer, InferredType, BasicType
from .specialized_bytecode import SpecializedBytecodeBuilder, SpecializedOpcode
from .bytecode import BytecodeChunk
from .compiler import CompilerContext


class OptimizingCompiler:
    """Bytecode compiler with type-based optimizations"""
    
    def __init__(self):
        self.builder = SpecializedBytecodeBuilder()
        self.context = CompilerContext()
        self.type_info: Dict[str, InferredType] = {}
        self.inferencer = OptimizationTypeInferencer()
    
    def compile(self, graph: Graph) -> BytecodeChunk:
        """Compile graph to optimized bytecode"""
        if not graph.root_id:
            raise ValueError("Graph has no root node")
        
        # Reset state
        self.builder = SpecializedBytecodeBuilder()
        self.context = CompilerContext()
        
        # Run type inference
        self.type_info = self.inferencer.infer_graph_types(graph)
        
        # Compile from root
        self._compile_node(graph.root_id, graph)
        
        # Add halt
        self.builder.emit_specialized(SpecializedOpcode.HALT)
        
        return self.builder.get_chunk()
    
    def _get_node_type(self, node_id: str) -> InferredType:
        """Get inferred type for a node"""
        return self.type_info.get(node_id, InferredType(BasicType.ANY))
    
    def _compile_node(self, node_id: str, graph: Graph):
        """Compile a node with type information"""
        node = graph.get_node(node_id)
        if not node:
            raise ValueError(f"Node {node_id} not found")
        
        node_type = self._get_node_type(node_id)
        
        # Dispatch based on node type
        if isinstance(node, Literal):
            self._compile_literal(node, node_type)
        elif isinstance(node, Variable):
            self._compile_variable(node, node_type)
        elif isinstance(node, Function):
            self._compile_function(node, node_type)
        elif isinstance(node, Application):
            self._compile_application(node, graph, node_type)
        elif isinstance(node, Lambda):
            self._compile_lambda(node, graph, node_type)
        elif isinstance(node, Let):
            self._compile_let(node, graph, node_type)
        elif isinstance(node, If):
            self._compile_if(node, graph, node_type)
        elif isinstance(node, Sequence):
            self._compile_sequence(node, graph)
        else:
            raise NotImplementedError(f"Compilation for {type(node).__name__} not implemented")
    
    def _compile_literal(self, node: Literal, node_type: InferredType):
        """Compile literal with type-specific instructions"""
        if node_type.base_type == BasicType.INT and isinstance(node.value, int):
            self.builder.emit_int_constant(node.value)
        elif node_type.base_type == BasicType.BOOL and isinstance(node.value, bool):
            self.builder.emit_bool_constant(node.value)
        else:
            # Fall back to generic constant
            self.builder.emit_constant(node.value)
    
    def _compile_variable(self, node: Variable, node_type: InferredType):
        """Compile variable reference"""
        # Try local lookup
        idx = self.context.lookup(node.name)
        if idx is not None:
            # Use specialized local load if possible
            self.builder.emit_local_load(idx)
        else:
            # Try built-in functions
            if node.name in self._get_builtin_functions():
                self.builder.emit_constant(node.name)
            else:
                raise NameError(f"Unbound variable: {node.name}")
    
    def _compile_function(self, node: Function, node_type: InferredType):
        """Compile function reference"""
        self.builder.emit_constant(node.name)
    
    def _compile_application(self, node: Application, graph: Graph, node_type: InferredType):
        """Compile function application with type specialization"""
        func_node = graph.get_node(node.function_id)
        
        # Compile arguments first
        arg_types = []
        for arg_id in node.argument_ids:
            arg_type = self._get_node_type(arg_id)
            arg_types.append(arg_type)
            self._compile_node(arg_id, graph)
        
        # Check if we can use specialized instructions
        if isinstance(func_node, (Function, Variable)):
            func_name = func_node.name
            
            # Integer arithmetic
            if (func_name in ['+', '-', '*', '/', 'mod'] and 
                all(t.base_type == BasicType.INT for t in arg_types) and
                node_type.base_type == BasicType.INT):
                self.builder.emit_int_arithmetic(func_name)
                return
            
            # Float arithmetic
            if (func_name in ['+.', '-.', '*.', '/.'] and
                all(t.base_type == BasicType.FLOAT for t in arg_types) and
                node_type.base_type == BasicType.FLOAT):
                self.builder.emit_float_arithmetic(func_name)
                return
            
            # Integer comparison
            if (func_name in ['<', '>', '<=', '>=', '==', '!='] and
                len(arg_types) >= 2 and
                arg_types[0].base_type == BasicType.INT and
                arg_types[1].base_type == BasicType.INT):
                self.builder.emit_int_comparison(func_name)
                return
            
            # Built-in functions
            if func_name in self._get_builtin_opcodes():
                opcode = self._get_builtin_opcodes()[func_name]
                self.builder.emit(opcode)
                return
        
        # Fall back to generic function call
        self._compile_node(node.function_id, graph)
        self.builder.emit(SpecializedOpcode.CALL, len(node.argument_ids))
    
    def _compile_let(self, node: Let, graph: Graph, node_type: InferredType):
        """Compile let with optimized local variable access"""
        old_context = self.context
        self.context = self.context.extend()
        
        # Define all names first (for recursion)
        for binding in node.bindings:
            self.context.define_local(binding['name'])
        
        # Compile values and store
        for i, binding in enumerate(node.bindings):
            self._compile_node(binding['value_id'], graph)
            idx = self.context.locals[binding['name']]
            self.builder.emit_local_store(idx)
        
        # Compile body
        self._compile_node(node.body_id, graph)
        
        self.context = old_context
    
    def _compile_if(self, node: If, graph: Graph, node_type: InferredType):
        """Compile conditional with type-aware jumps"""
        # Get condition type
        cond_type = self._get_node_type(node.condition_id)
        
        # Compile condition
        self._compile_node(node.condition_id, graph)
        
        # Use specialized jump if condition is integer
        if cond_type.base_type == BasicType.INT:
            false_jump = self.builder.emit_jump(SpecializedOpcode.JUMP_IF_FALSE_INT)
        else:
            false_jump = self.builder.emit_jump(SpecializedOpcode.JUMP_IF_NOT)
        
        # Compile then branch
        self._compile_node(node.then_id, graph)
        
        # Jump over else
        end_jump = self.builder.emit_jump(SpecializedOpcode.JUMP)
        
        # Patch false jump
        self.builder.patch_jump(false_jump, self.builder.current_position())
        
        # Compile else branch
        self._compile_node(node.else_id, graph)
        
        # Patch end jump
        self.builder.patch_jump(end_jump, self.builder.current_position())
    
    def _compile_lambda(self, node: Lambda, graph: Graph, node_type: InferredType):
        """Compile lambda"""
        # For now, use the same approach as the base compiler
        jump_pos = self.builder.emit_jump(SpecializedOpcode.JUMP)
        func_start = self.builder.current_position()
        
        old_context = self.context
        self.context = self.context.extend()
        
        # Bind parameters
        for param in node.parameter_names:
            self.context.define_local(param)
        
        # Compile body
        self._compile_node(node.body_id, graph)
        self.builder.emit(SpecializedOpcode.RETURN)
        
        self.context = old_context
        
        # Patch jump
        self.builder.patch_jump(jump_pos, self.builder.current_position())
        
        # Create closure
        self.builder.emit(SpecializedOpcode.MAKE_FUNC, func_start)
    
    def _compile_sequence(self, node: Sequence, graph: Graph):
        """Compile sequence"""
        for i, step_id in enumerate(node.step_ids):
            self._compile_node(step_id, graph)
            
            # Pop intermediate results except last
            if i < len(node.step_ids) - 1:
                self.builder.emit(SpecializedOpcode.POP)
    
    def _get_builtin_functions(self) -> Set[str]:
        """Get set of built-in function names"""
        return set(self._get_builtin_opcodes().keys())
    
    def _get_builtin_opcodes(self) -> Dict[str, SpecializedOpcode]:
        """Map built-in functions to opcodes"""
        # Return only non-specialized opcodes here
        # Specialized ones are handled in _compile_application
        return {
            'and': SpecializedOpcode.AND,
            'or': SpecializedOpcode.OR,
            'not': SpecializedOpcode.NOT,
            'cons': SpecializedOpcode.LIST_CONS,
            'head': SpecializedOpcode.LIST_HEAD,
            'tail': SpecializedOpcode.LIST_TAIL,
            'length': SpecializedOpcode.LIST_LEN,
            'empty?': SpecializedOpcode.LIST_EMPTY,
            'string-length': SpecializedOpcode.STR_LEN,
            'concat': SpecializedOpcode.STR_CONCAT,
            'string-upcase': SpecializedOpcode.STR_UPPER,
            'string-downcase': SpecializedOpcode.STR_LOWER,
        }


def demonstrate_optimizing_compiler():
    """Demonstrate the optimizing compiler"""
    from ..parser import parse
    from ..vm import VM
    from .compiler import BytecodeCompiler
    
    print("Optimizing Compiler Demo")
    print("=" * 60)
    
    examples = [
        ("Integer arithmetic", "(+ 2 3)"),
        ("Mixed arithmetic", "(+ (* 2 3) (/ 10 2))"),
        ("Conditional", "(if (> 10 5) 100 200)"),
        ("Let binding", "(let ((x 10) (y 20)) (+ x y))"),
        ("Type mixing", "(+ 2 (* 3 4))"),
    ]
    
    opt_compiler = OptimizingCompiler()
    base_compiler = BytecodeCompiler()
    
    for name, code in examples:
        print(f"\n{name}: {code}")
        print("-" * 40)
        
        graph = parse(code)
        
        # Show type inference results
        types = opt_compiler.inferencer.infer_graph_types(graph)
        print("\nInferred types:")
        for node_id, node in graph.nodes.items():
            if node_id in types:
                node_type = types[node_id]
                if isinstance(node, Application):
                    print(f"  Application: {node_type}")
                elif isinstance(node, Literal):
                    print(f"  Literal {node.value}: {node_type}")
        
        # Compile both ways
        opt_bytecode = opt_compiler.compile(graph)
        base_bytecode = base_compiler.compile(graph)
        
        print(f"\nBytecode comparison:")
        print(f"  Base compiler:      {len(base_bytecode.instructions)} instructions")
        print(f"  Optimizing compiler: {len(opt_bytecode.instructions)} instructions")
        
        # Show the optimized bytecode
        print("\nOptimized bytecode:")
        disasm = opt_bytecode.disassemble()
        for line in disasm.split('\n'):
            if line.strip() and not line.startswith('===') and not line.startswith('Constants'):
                print(f"  {line}")


if __name__ == "__main__":
    demonstrate_optimizing_compiler()