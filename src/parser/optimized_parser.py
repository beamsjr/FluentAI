"""
Optimized parser that creates native lists directly
"""

from typing import List, Dict, Any, Optional
from ..core.ast import *
from ..core.primitives import PRIMITIVES
from .sexpr_parser import Lexer, Parser as BaseParser


class OptimizedParser(BaseParser):
    """Parser that generates optimized AST with native lists"""
    
    def _parse_list_literal(self) -> str:
        """Parse list literal: [1 2 3] - optimized version"""
        self._advance()  # Skip LBRACKET
        
        elements = []
        element_ids = []
        
        while self._current() and self._current().type != 'RBRACKET':
            elem_id = self._parse_expr()
            if elem_id:
                element_ids.append(elem_id)
                # Check if it's a literal we can evaluate
                elem_node = self.graph.get_node(elem_id)
                if isinstance(elem_node, Literal):
                    elements.append(elem_node.value)
                else:
                    # Can't optimize - fall back to cons construction
                    return self._build_cons_list(element_ids)
        
        if not self._current() or self._current().type != 'RBRACKET':
            raise SyntaxError("Expected closing bracket for list")
        
        self._advance()  # Skip RBRACKET
        
        # All elements are literals - create native list directly
        list_node = Literal(value=elements, literal_type='list')
        return self.graph.add_node(list_node)
    
    def _build_cons_list(self, element_ids: List[str]) -> str:
        """Build list using cons operations (fallback)"""
        # Start with empty list
        empty_list = Literal(value=[], literal_type='list')
        result_id = self.graph.add_node(empty_list)
        
        # Build list from right to left
        for elem_id in reversed(element_ids):
            cons_func = Function(name="cons", arity=2, effects={EffectType.PURE})
            cons_id = self.graph.add_node(cons_func)
            
            app = Application(
                function_id=cons_id,
                argument_ids=[elem_id, result_id]
            )
            result_id = self.graph.add_node(app)
        
        return result_id


class ListOptimizer:
    """Optimize list operations in the AST"""
    
    def optimize(self, graph: Graph) -> Graph:
        """Optimize list operations in a graph"""
        # Create a mapping of optimizations
        optimized = Graph()
        node_mapping = {}
        
        # Copy and optimize nodes
        for node_id in graph.topological_sort():
            node = graph.get_node(node_id)
            new_node = self._optimize_node(node, graph, node_mapping)
            
            if new_node:
                new_id = optimized.add_node(new_node)
                node_mapping[node_id] = new_id
        
        # Update root
        if graph.root_id in node_mapping:
            optimized.root_id = node_mapping[graph.root_id]
        
        return optimized
    
    def _optimize_node(self, node: ASTNode, graph: Graph, mapping: Dict[str, str]) -> Optional[ASTNode]:
        """Optimize a single node"""
        if isinstance(node, Application):
            return self._optimize_application(node, graph, mapping)
        
        # For other nodes, create a copy with updated references
        return self._copy_node_with_mapping(node, mapping)
    
    def _optimize_application(self, app: Application, graph: Graph, mapping: Dict[str, str]) -> Optional[ASTNode]:
        """Optimize function applications"""
        func_node = graph.get_node(app.function_id)
        
        # Optimize list operations
        if isinstance(func_node, Function):
            if func_node.name == "length":
                # Check if argument is a literal list
                if len(app.argument_ids) == 1:
                    arg_node = graph.get_node(app.argument_ids[0])
                    if isinstance(arg_node, Literal) and isinstance(arg_node.value, list):
                        # Constant fold length
                        return Literal(value=len(arg_node.value), literal_type='int')
            
            elif func_node.name == "cons":
                # Check if we're consing to a literal list
                if len(app.argument_ids) == 2:
                    elem_node = graph.get_node(app.argument_ids[0])
                    list_node = graph.get_node(app.argument_ids[1])
                    
                    if (isinstance(elem_node, Literal) and 
                        isinstance(list_node, Literal) and 
                        isinstance(list_node.value, list)):
                        # Constant fold cons
                        new_list = [elem_node.value] + list_node.value
                        return Literal(value=new_list, literal_type='list')
        
        # Default: copy with mapped arguments
        return self._copy_node_with_mapping(app, mapping)
    
    def _copy_node_with_mapping(self, node: ASTNode, mapping: Dict[str, str]) -> ASTNode:
        """Create a copy of node with updated references"""
        if isinstance(node, Literal):
            return Literal(value=node.value, literal_type=node.literal_type)
        
        elif isinstance(node, Variable):
            new_node = Variable(name=node.name)
            if node.binding_id and node.binding_id in mapping:
                new_node.binding_id = mapping[node.binding_id]
            return new_node
        
        elif isinstance(node, Function):
            return Function(
                name=node.name,
                arity=node.arity,
                effects=node.effects.copy(),
                implementation=node.implementation
            )
        
        elif isinstance(node, Application):
            new_func_id = mapping.get(node.function_id, node.function_id)
            new_arg_ids = [mapping.get(arg_id, arg_id) for arg_id in node.argument_ids]
            return Application(
                function_id=new_func_id,
                argument_ids=new_arg_ids,
                is_parallel=node.is_parallel
            )
        
        elif isinstance(node, Lambda):
            new_body_id = mapping.get(node.body_id, node.body_id)
            new_captured = {
                name: mapping.get(binding_id, binding_id)
                for name, binding_id in node.captured_variables.items()
            }
            return Lambda(
                parameter_names=node.parameter_names.copy(),
                parameter_types=node.parameter_types.copy(),
                body_id=new_body_id,
                captured_variables=new_captured
            )
        
        elif isinstance(node, Let):
            new_bindings = []
            for binding in node.bindings:
                new_value_id = mapping.get(binding['value_id'], binding['value_id'])
                new_bindings.append({
                    'name': binding['name'],
                    'value_id': new_value_id
                })
            new_body_id = mapping.get(node.body_id, node.body_id)
            return Let(
                bindings=new_bindings,
                body_id=new_body_id,
                is_recursive=node.is_recursive
            )
        
        elif isinstance(node, If):
            return If(
                condition_id=mapping.get(node.condition_id, node.condition_id),
                then_id=mapping.get(node.then_id, node.then_id),
                else_id=mapping.get(node.else_id, node.else_id)
            )
        
        elif isinstance(node, Sequence):
            new_step_ids = [mapping.get(step_id, step_id) for step_id in node.step_ids]
            return Sequence(step_ids=new_step_ids)
        
        elif isinstance(node, Parallel):
            new_branch_ids = [mapping.get(branch_id, branch_id) for branch_id in node.branch_ids]
            return Parallel(branch_ids=new_branch_ids, merge_strategy=node.merge_strategy)
        
        # Default: return as-is
        return node


def optimized_parse(source: str) -> Graph:
    """Parse with optimizations"""
    lexer = Lexer(source)
    tokens = lexer.tokenize()
    parser = OptimizedParser(tokens)
    graph = parser.parse()
    
    # Apply optimizations
    optimizer = ListOptimizer()
    optimized_graph = optimizer.optimize(graph)
    
    return optimized_graph