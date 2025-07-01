"""
ClaudeLang Graph Optimizer

Performs compile-time optimization on AST graphs.
"""

from typing import Dict, Set, Optional, Any, List
from dataclasses import dataclass
from ..core.ast import *
from ..core.primitives import PRIMITIVES


@dataclass
class OptimizationStats:
    """Statistics about optimizations performed"""
    constant_folded: int = 0
    dead_code_eliminated: int = 0
    pure_expressions_evaluated: int = 0
    nodes_before: int = 0
    nodes_after: int = 0


class GraphOptimizer:
    """Optimizes ClaudeLang AST graphs"""
    
    def __init__(self):
        self.stats = OptimizationStats()
    
    def optimize(self, graph: Graph) -> Graph:
        """Optimize a graph with multiple passes"""
        self.stats = OptimizationStats()
        self.stats.nodes_before = len(graph.nodes)
        
        # Apply optimization passes
        optimized = graph
        
        # Pass 1: Constant folding
        optimized = self._constant_folding_pass(optimized)
        
        # Pass 2: Dead code elimination
        optimized = self._dead_code_elimination_pass(optimized)
        
        # Pass 3: Pure expression evaluation
        optimized = self._pure_evaluation_pass(optimized)
        
        self.stats.nodes_after = len(optimized.nodes)
        return optimized
    
    def _constant_folding_pass(self, graph: Graph) -> Graph:
        """Fold constant expressions"""
        optimized = Graph()
        node_mapping = {}
        
        for node_id in graph.topological_sort():
            node = graph.get_node(node_id)
            
            # Try to fold constants
            if isinstance(node, Application):
                folded = self._try_fold_application(node, graph, node_mapping)
                if folded:
                    new_id = optimized.add_node(folded)
                    node_mapping[node_id] = new_id
                    self.stats.constant_folded += 1
                    continue
            
            # Copy node with updated references
            new_node = self._copy_with_mapping(node, node_mapping)
            if new_node:
                new_id = optimized.add_node(new_node)
                node_mapping[node_id] = new_id
        
        # Update root
        if graph.root_id in node_mapping:
            optimized.root_id = node_mapping[graph.root_id]
        
        return optimized
    
    def _try_fold_application(self, app: Application, graph: Graph, mapping: Dict[str, str]) -> Optional[Literal]:
        """Try to fold a function application"""
        func_node = graph.get_node(app.function_id)
        
        # Check if it's a foldable function
        if not isinstance(func_node, Function):
            return None
        
        # Only fold pure functions
        if EffectType.PURE not in func_node.effects or len(func_node.effects) > 1:
            return None
        
        # Get the implementation
        impl = PRIMITIVES.get_implementation(func_node.name)
        if not impl:
            return None
        
        # Check if all arguments are literals
        arg_values = []
        for arg_id in app.argument_ids:
            # Map the argument ID if needed
            actual_arg_id = mapping.get(arg_id, arg_id)
            arg_node = graph.get_node(actual_arg_id)
            
            if not isinstance(arg_node, Literal):
                return None
            
            arg_values.append(arg_node.value)
        
        # Try to evaluate
        try:
            result = impl(*arg_values)
            
            # Determine result type
            if isinstance(result, bool):
                literal_type = 'bool'
            elif isinstance(result, int):
                literal_type = 'int'
            elif isinstance(result, float):
                literal_type = 'float'
            elif isinstance(result, str):
                literal_type = 'string'
            elif isinstance(result, list):
                literal_type = 'list'
            else:
                return None
            
            return Literal(value=result, literal_type=literal_type)
            
        except Exception:
            # Can't fold if evaluation fails
            return None
    
    def _dead_code_elimination_pass(self, graph: Graph) -> Graph:
        """Eliminate unreachable code"""
        # Find all reachable nodes from root
        reachable = set()
        
        def mark_reachable(node_id: str):
            if node_id in reachable:
                return
            reachable.add(node_id)
            
            node = graph.get_node(node_id)
            if node:
                for dep_id in node.get_dependencies():
                    mark_reachable(dep_id)
        
        if graph.root_id:
            mark_reachable(graph.root_id)
        
        # Create new graph with only reachable nodes
        optimized = Graph()
        for node_id in reachable:
            node = graph.get_node(node_id)
            if node:
                optimized.nodes[node_id] = node
        
        optimized.root_id = graph.root_id
        
        self.stats.dead_code_eliminated = len(graph.nodes) - len(optimized.nodes)
        
        return optimized
    
    def _pure_evaluation_pass(self, graph: Graph) -> Graph:
        """Evaluate pure subgraphs at compile time"""
        optimized = Graph()
        node_mapping = {}
        
        # Simple interpreter for pure expressions
        def evaluate_pure(node_id: str) -> Optional[Any]:
            node = graph.get_node(node_id)
            if not node:
                return None
            
            # Check if node is pure
            if node.get_effects() != {EffectType.PURE}:
                return None
            
            if isinstance(node, Literal):
                return node.value
            
            elif isinstance(node, Application):
                func_node = graph.get_node(node.function_id)
                if isinstance(func_node, Function):
                    impl = PRIMITIVES.get_implementation(func_node.name)
                    if impl:
                        # Evaluate arguments
                        args = []
                        for arg_id in node.argument_ids:
                            arg_val = evaluate_pure(arg_id)
                            if arg_val is None:
                                return None
                            args.append(arg_val)
                        
                        try:
                            return impl(*args)
                        except:
                            return None
            
            return None
        
        # Try to evaluate each node
        for node_id in graph.topological_sort():
            node = graph.get_node(node_id)
            
            # Skip if already processed
            if node_id in node_mapping:
                continue
            
            # Try pure evaluation for complex expressions
            if isinstance(node, Application) or isinstance(node, If):
                result = evaluate_pure(node_id)
                if result is not None:
                    # Determine literal type
                    if isinstance(result, bool):
                        literal_type = 'bool'
                    elif isinstance(result, int):
                        literal_type = 'int'
                    elif isinstance(result, float):
                        literal_type = 'float'
                    elif isinstance(result, str):
                        literal_type = 'string'
                    elif isinstance(result, list):
                        literal_type = 'list'
                    else:
                        literal_type = 'unknown'
                    
                    literal = Literal(value=result, literal_type=literal_type)
                    new_id = optimized.add_node(literal)
                    node_mapping[node_id] = new_id
                    self.stats.pure_expressions_evaluated += 1
                    continue
            
            # Copy node with mapping
            new_node = self._copy_with_mapping(node, node_mapping)
            if new_node:
                new_id = optimized.add_node(new_node)
                node_mapping[node_id] = new_id
        
        # Update root
        if graph.root_id in node_mapping:
            optimized.root_id = node_mapping[graph.root_id]
        
        return optimized
    
    def _copy_with_mapping(self, node: ASTNode, mapping: Dict[str, str]) -> Optional[ASTNode]:
        """Copy a node with updated references"""
        if isinstance(node, Literal):
            return Literal(value=node.value, literal_type=node.literal_type)
        
        elif isinstance(node, Variable):
            return Variable(name=node.name, binding_id=mapping.get(node.binding_id, node.binding_id))
        
        elif isinstance(node, Function):
            return Function(
                name=node.name,
                arity=node.arity,
                effects=node.effects.copy(),
                implementation=node.implementation
            )
        
        elif isinstance(node, Application):
            return Application(
                function_id=mapping.get(node.function_id, node.function_id),
                argument_ids=[mapping.get(arg_id, arg_id) for arg_id in node.argument_ids],
                is_parallel=node.is_parallel
            )
        
        elif isinstance(node, Lambda):
            return Lambda(
                parameter_names=node.parameter_names.copy(),
                parameter_types=node.parameter_types.copy(),
                body_id=mapping.get(node.body_id, node.body_id),
                captured_variables={
                    name: mapping.get(binding_id, binding_id)
                    for name, binding_id in node.captured_variables.items()
                }
            )
        
        elif isinstance(node, Let):
            return Let(
                bindings=[{
                    'name': binding['name'],
                    'value_id': mapping.get(binding['value_id'], binding['value_id'])
                } for binding in node.bindings],
                body_id=mapping.get(node.body_id, node.body_id),
                is_recursive=node.is_recursive
            )
        
        elif isinstance(node, If):
            # Special handling: if condition is constant, eliminate branch
            cond_id = mapping.get(node.condition_id, node.condition_id)
            cond_node = self._get_mapped_node(cond_id, mapping)
            
            if isinstance(cond_node, Literal):
                # Constant condition - return the appropriate branch
                if cond_node.value:
                    return self._get_mapped_node(mapping.get(node.then_id, node.then_id), mapping)
                else:
                    return self._get_mapped_node(mapping.get(node.else_id, node.else_id), mapping)
            
            return If(
                condition_id=cond_id,
                then_id=mapping.get(node.then_id, node.then_id),
                else_id=mapping.get(node.else_id, node.else_id)
            )
        
        elif isinstance(node, Sequence):
            return Sequence(step_ids=[mapping.get(step_id, step_id) for step_id in node.step_ids])
        
        elif isinstance(node, Parallel):
            return Parallel(
                branch_ids=[mapping.get(branch_id, branch_id) for branch_id in node.branch_ids],
                merge_strategy=node.merge_strategy
            )
        
        elif isinstance(node, Effect):
            return Effect(
                effect_type=node.effect_type,
                operation=node.operation,
                argument_ids=[mapping.get(arg_id, arg_id) for arg_id in node.argument_ids],
                handler_id=mapping.get(node.handler_id, node.handler_id) if node.handler_id else None
            )
        
        elif isinstance(node, Uncertainty):
            return Uncertainty(
                choices=[{
                    'node_id': mapping.get(choice['node_id'], choice['node_id']),
                    'probability': choice['probability']
                } for choice in node.choices],
                distribution_type=node.distribution_type
            )
        
        return None
    
    def _get_mapped_node(self, node_id: str, mapping: Dict[str, str]) -> Optional[ASTNode]:
        """Helper to get a node through mapping (for optimization context)"""
        # This would need access to the graph being built
        # For now, return None to indicate we can't optimize further
        return None