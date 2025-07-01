"""
LLVM Code Generation Proof of Concept for ClaudeLang

This demonstrates how we could compile ClaudeLang to native code using LLVM.
Note: This requires llvmlite to be installed: pip install llvmlite
"""

try:
    import llvmlite.ir as ir
    import llvmlite.binding as llvm
    LLVM_AVAILABLE = True
except ImportError:
    LLVM_AVAILABLE = False
    print("Warning: llvmlite not installed. Native code generation unavailable.")

from typing import Dict, Any, Optional, Tuple
from dataclasses import dataclass
from ..core.ast import *
from ..optimizer.advanced_optimizer import AdvancedGraphOptimizer


@dataclass
class LLVMContext:
    """Context for LLVM code generation"""
    module: Any  # ir.Module
    builder: Any  # ir.IRBuilder
    func: Any    # ir.Function
    variables: Dict[str, Any]  # Variable name to LLVM value
    
    
class LLVMCodeGenerator:
    """Generates LLVM IR from ClaudeLang AST"""
    
    def __init__(self):
        if not LLVM_AVAILABLE:
            raise RuntimeError("llvmlite not installed")
            
        # Initialize LLVM
        llvm.initialize()
        llvm.initialize_native_target()
        llvm.initialize_native_asmprinter()
        
        self.target = llvm.Target.from_default_triple()
        
    def compile_to_native(self, graph: Graph, function_name: str = "main") -> bytes:
        """Compile a ClaudeLang graph to native machine code"""
        # First optimize the graph
        optimizer = AdvancedGraphOptimizer()
        optimized = optimizer.optimize(graph)
        
        # Generate LLVM IR
        llvm_ir = self._generate_llvm_ir(optimized, function_name)
        
        # Compile to machine code
        return self._compile_ir_to_machine_code(llvm_ir)
    
    def _generate_llvm_ir(self, graph: Graph, function_name: str) -> str:
        """Generate LLVM IR from optimized AST"""
        # Create module
        module = ir.Module(name="claudelang_module")
        
        # Define function type (for now, simple: () -> i64)
        func_type = ir.FunctionType(ir.IntType(64), [])
        func = ir.Function(module, func_type, name=function_name)
        
        # Create entry block
        block = func.append_basic_block(name="entry")
        builder = ir.IRBuilder(block)
        
        # Create context
        ctx = LLVMContext(
            module=module,
            builder=builder,
            func=func,
            variables={}
        )
        
        # Generate code for root node
        if graph.root_id:
            result = self._gen_node(graph.root_id, graph, ctx)
            builder.ret(result)
        else:
            builder.ret(ir.Constant(ir.IntType(64), 0))
        
        return str(module)
    
    def _gen_node(self, node_id: str, graph: Graph, ctx: LLVMContext) -> Any:
        """Generate LLVM IR for a node"""
        node = graph.get_node(node_id)
        if not node:
            raise ValueError(f"Node {node_id} not found")
        
        if isinstance(node, Literal):
            return self._gen_literal(node, ctx)
        elif isinstance(node, Application):
            return self._gen_application(node, graph, ctx)
        elif isinstance(node, If):
            return self._gen_if(node, graph, ctx)
        elif isinstance(node, Let):
            return self._gen_let(node, graph, ctx)
        else:
            # For now, just return 0 for unsupported nodes
            return ir.Constant(ir.IntType(64), 0)
    
    def _gen_literal(self, node: Literal, ctx: LLVMContext) -> Any:
        """Generate LLVM IR for a literal"""
        if isinstance(node.value, int):
            return ir.Constant(ir.IntType(64), node.value)
        elif isinstance(node.value, float):
            return ir.Constant(ir.DoubleType(), node.value)
        elif isinstance(node.value, bool):
            return ir.Constant(ir.IntType(1), 1 if node.value else 0)
        else:
            # For now, unsupported literals return 0
            return ir.Constant(ir.IntType(64), 0)
    
    def _gen_application(self, node: Application, graph: Graph, ctx: LLVMContext) -> Any:
        """Generate LLVM IR for function application"""
        func_node = graph.get_node(node.function_id)
        
        # Handle built-in arithmetic operations
        if isinstance(func_node, Function):
            if func_node.name == '+':
                # Generate addition
                if len(node.argument_ids) == 2:
                    left = self._gen_node(node.argument_ids[0], graph, ctx)
                    right = self._gen_node(node.argument_ids[1], graph, ctx)
                    return ctx.builder.add(left, right, name="add_result")
            
            elif func_node.name == '-':
                # Generate subtraction
                if len(node.argument_ids) == 2:
                    left = self._gen_node(node.argument_ids[0], graph, ctx)
                    right = self._gen_node(node.argument_ids[1], graph, ctx)
                    return ctx.builder.sub(left, right, name="sub_result")
            
            elif func_node.name == '*':
                # Generate multiplication
                if len(node.argument_ids) == 2:
                    left = self._gen_node(node.argument_ids[0], graph, ctx)
                    right = self._gen_node(node.argument_ids[1], graph, ctx)
                    return ctx.builder.mul(left, right, name="mul_result")
            
            elif func_node.name in ['<', '>', '<=', '>=', '==', '!=']:
                # Generate comparison
                if len(node.argument_ids) == 2:
                    left = self._gen_node(node.argument_ids[0], graph, ctx)
                    right = self._gen_node(node.argument_ids[1], graph, ctx)
                    
                    cmp_map = {
                        '<': 'slt',
                        '>': 'sgt',
                        '<=': 'sle',
                        '>=': 'sge',
                        '==': 'eq',
                        '!=': 'ne'
                    }
                    
                    cmp_op = cmp_map.get(func_node.name, 'eq')
                    cmp_result = ctx.builder.icmp_signed(cmp_op, left, right)
                    # Convert bool to i64
                    return ctx.builder.zext(cmp_result, ir.IntType(64))
        
        # Default: return 0
        return ir.Constant(ir.IntType(64), 0)
    
    def _gen_if(self, node: If, graph: Graph, ctx: LLVMContext) -> Any:
        """Generate LLVM IR for conditional"""
        # Evaluate condition
        cond_val = self._gen_node(node.condition_id, graph, ctx)
        
        # Convert to i1 (bool)
        cond_bool = ctx.builder.icmp_signed('ne', cond_val, ir.Constant(ir.IntType(64), 0))
        
        # Create blocks
        then_block = ctx.func.append_basic_block(name="then")
        else_block = ctx.func.append_basic_block(name="else")
        merge_block = ctx.func.append_basic_block(name="merge")
        
        # Branch
        ctx.builder.cbranch(cond_bool, then_block, else_block)
        
        # Then block
        ctx.builder.position_at_end(then_block)
        then_val = self._gen_node(node.then_id, graph, ctx)
        ctx.builder.branch(merge_block)
        then_block = ctx.builder.block
        
        # Else block
        ctx.builder.position_at_end(else_block)
        else_val = self._gen_node(node.else_id, graph, ctx)
        ctx.builder.branch(merge_block)
        else_block = ctx.builder.block
        
        # Merge block
        ctx.builder.position_at_end(merge_block)
        phi = ctx.builder.phi(ir.IntType(64), name="if_result")
        phi.add_incoming(then_val, then_block)
        phi.add_incoming(else_val, else_block)
        
        return phi
    
    def _gen_let(self, node: Let, graph: Graph, ctx: LLVMContext) -> Any:
        """Generate LLVM IR for let binding"""
        # Save old variables
        old_vars = ctx.variables.copy()
        
        # Bind variables
        for binding in node.bindings:
            value = self._gen_node(binding['value_id'], graph, ctx)
            ctx.variables[binding['name']] = value
        
        # Generate body
        result = self._gen_node(node.body_id, graph, ctx)
        
        # Restore variables
        ctx.variables = old_vars
        
        return result
    
    def _compile_ir_to_machine_code(self, llvm_ir: str) -> bytes:
        """Compile LLVM IR to machine code"""
        # Parse IR
        mod = llvm.parse_assembly(llvm_ir)
        mod.verify()
        
        # Create execution engine
        target_machine = self.target.create_target_machine()
        
        # Compile to machine code
        return target_machine.emit_object(mod)


def demonstrate_llvm_compilation():
    """Demonstrate LLVM compilation with a simple example"""
    if not LLVM_AVAILABLE:
        print("Skipping LLVM demonstration - llvmlite not installed")
        return
    
    from ..parser import parse
    
    # Simple arithmetic expression
    code = "(+ (* 2 3) (* 4 5))"
    print(f"Compiling: {code}")
    
    # Parse
    graph = parse(code)
    
    # Create code generator
    codegen = LLVMCodeGenerator()
    
    # Generate LLVM IR
    llvm_ir = codegen._generate_llvm_ir(graph, "calculate")
    print("\nGenerated LLVM IR:")
    print(llvm_ir)
    
    # For execution, we'd need to link with a runtime and create an executable
    # This is just a proof of concept
    
    # More complex example with conditionals
    code2 = "(if (> 10 5) (* 2 50) (+ 1000 1000))"
    print(f"\n\nCompiling: {code2}")
    
    graph2 = parse(code2)
    llvm_ir2 = codegen._generate_llvm_ir(graph2, "conditional_calc")
    print("\nGenerated LLVM IR:")
    print(llvm_ir2)


if __name__ == "__main__":
    demonstrate_llvm_compilation()