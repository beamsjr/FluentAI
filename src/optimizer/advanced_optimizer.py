"""
Advanced ClaudeLang Optimizer with more aggressive optimizations
"""

from typing import Dict, Set, Optional, Any, List, Tuple
from dataclasses import dataclass
from ..core.ast import *
from ..core.primitives import PRIMITIVES


@dataclass
class OptimizationStats:
    """Statistics about optimizations performed"""
    constant_folded: int = 0
    dead_code_eliminated: int = 0
    pure_expressions_evaluated: int = 0
    branches_eliminated: int = 0
    inlined_expressions: int = 0
    nodes_before: int = 0
    nodes_after: int = 0


class AdvancedGraphOptimizer:
    """Advanced optimizer with aggressive optimizations"""
    
    def __init__(self):
        self.stats = OptimizationStats()
        self.graph: Optional[Graph] = None
        self.optimized: Optional[Graph] = None
        self.value_cache: Dict[str, Any] = {}
    
    def optimize(self, graph: Graph) -> Graph:
        """Optimize with multiple aggressive passes"""
        self.stats = OptimizationStats()
        self.stats.nodes_before = len(graph.nodes)
        self.graph = graph
        
        # Pass 1: Evaluate all pure expressions
        self.optimized = Graph()
        self.value_cache = {}
        
        # Build optimized graph
        if graph.root_id:
            new_root = self._optimize_node(graph.root_id)
            if new_root:
                self.optimized.root_id = new_root
        
        # Pass 2: Dead code elimination
        self._eliminate_dead_code()
        
        self.stats.nodes_after = len(self.optimized.nodes)
        return self.optimized
    
    def _optimize_node(self, node_id: str) -> Optional[str]:
        """Optimize a node and return its ID in the optimized graph"""
        if not self.graph:
            return None
            
        node = self.graph.get_node(node_id)
        if not node:
            return None
        
        # Try to evaluate node completely
        value = self._try_evaluate(node_id)
        if value is not None:
            # Create literal node for the computed value
            literal = self._create_literal(value)
            new_id = self.optimized.add_node(literal)
            self.value_cache[node_id] = value
            self.stats.pure_expressions_evaluated += 1
            return new_id
        
        # Otherwise, copy node with optimized children
        return self._copy_node_optimized(node)
    
    def _try_evaluate(self, node_id: str) -> Optional[Any]:
        """Try to evaluate a node to a constant value"""
        if node_id in self.value_cache:
            return self.value_cache[node_id]
        
        if not self.graph:
            return None
            
        node = self.graph.get_node(node_id)
        if not node:
            return None
        
        # Check if node is pure
        if node.get_effects() != {EffectType.PURE}:
            return None
        
        if isinstance(node, Literal):
            return node.value
        
        elif isinstance(node, Application):
            return self._evaluate_application(node)
        
        elif isinstance(node, If):
            return self._evaluate_if(node)
        
        elif isinstance(node, Let):
            return self._evaluate_let(node)
        
        elif isinstance(node, Sequence):
            # Sequence can be evaluated if all steps are pure
            results = []
            for step_id in node.step_ids:
                val = self._try_evaluate(step_id)
                if val is None:
                    return None
                results.append(val)
            return results[-1] if results else None
        
        return None
    
    def _evaluate_application(self, app: Application) -> Optional[Any]:
        """Evaluate a function application"""
        if not self.graph:
            return None
            
        func_node = self.graph.get_node(app.function_id)
        
        # Handle Function nodes
        if isinstance(func_node, Function):
            impl = PRIMITIVES.get_implementation(func_node.name)
            if not impl:
                return None
        # Handle Variable nodes (built-in functions)
        elif isinstance(func_node, Variable):
            # Check if it's a built-in
            impl = PRIMITIVES.get_implementation(func_node.name)
            if not impl:
                return None
        else:
            return None
        
        # Evaluate all arguments
        args = []
        for arg_id in app.argument_ids:
            val = self._try_evaluate(arg_id)
            if val is None:
                return None
            args.append(val)
        
        # Apply function
        try:
            result = impl(*args)
            return result
        except Exception:
            return None
    
    def _evaluate_if(self, if_node: If) -> Optional[Any]:
        """Evaluate conditional"""
        # Evaluate condition
        cond_val = self._try_evaluate(if_node.condition_id)
        if cond_val is None:
            return None
        
        # Branch based on condition
        if cond_val:
            self.stats.branches_eliminated += 1
            return self._try_evaluate(if_node.then_id)
        else:
            self.stats.branches_eliminated += 1
            return self._try_evaluate(if_node.else_id)
    
    def _evaluate_let(self, let_node: Let) -> Optional[Any]:
        """Evaluate let binding"""
        # Save current cache state
        old_cache = self.value_cache.copy()
        
        try:
            # Evaluate bindings
            for binding in let_node.bindings:
                val = self._try_evaluate(binding['value_id'])
                if val is None:
                    return None
                # Cache binding value
                self.value_cache[binding['value_id']] = val
            
            # Evaluate body
            return self._try_evaluate(let_node.body_id)
        finally:
            # Restore cache
            self.value_cache = old_cache
    
    def _copy_node_optimized(self, node: ASTNode) -> Optional[str]:
        """Copy a node with optimized children"""
        if isinstance(node, Literal):
            new_id = self.optimized.add_node(node)
            return new_id
        
        elif isinstance(node, Variable):
            new_id = self.optimized.add_node(node)
            return new_id
        
        elif isinstance(node, Function):
            new_id = self.optimized.add_node(node)
            return new_id
        
        elif isinstance(node, Application):
            # Optimize function and arguments
            new_func_id = self._optimize_node(node.function_id)
            if not new_func_id:
                return None
            
            new_arg_ids = []
            for arg_id in node.argument_ids:
                new_arg_id = self._optimize_node(arg_id)
                if not new_arg_id:
                    return None
                new_arg_ids.append(new_arg_id)
            
            new_app = Application(
                function_id=new_func_id,
                argument_ids=new_arg_ids,
                is_parallel=node.is_parallel
            )
            return self.optimized.add_node(new_app)
        
        elif isinstance(node, Lambda):
            # Optimize body
            new_body_id = self._optimize_node(node.body_id)
            if not new_body_id:
                return None
            
            new_lambda = Lambda(
                parameter_names=node.parameter_names.copy(),
                parameter_types=node.parameter_types.copy(),
                body_id=new_body_id,
                captured_variables=node.captured_variables.copy()
            )
            return self.optimized.add_node(new_lambda)
        
        elif isinstance(node, Let):
            # Optimize bindings and body
            new_bindings = []
            for binding in node.bindings:
                new_value_id = self._optimize_node(binding['value_id'])
                if not new_value_id:
                    return None
                new_bindings.append({
                    'name': binding['name'],
                    'value_id': new_value_id
                })
            
            new_body_id = self._optimize_node(node.body_id)
            if not new_body_id:
                return None
            
            new_let = Let(
                bindings=new_bindings,
                body_id=new_body_id,
                is_recursive=node.is_recursive
            )
            return self.optimized.add_node(new_let)
        
        elif isinstance(node, If):
            # Check if condition is constant
            cond_val = self._try_evaluate(node.condition_id)
            if cond_val is not None:
                # Constant condition - only include the taken branch
                if cond_val:
                    self.stats.branches_eliminated += 1
                    return self._optimize_node(node.then_id)
                else:
                    self.stats.branches_eliminated += 1
                    return self._optimize_node(node.else_id)
            
            # Otherwise optimize all branches
            new_cond_id = self._optimize_node(node.condition_id)
            new_then_id = self._optimize_node(node.then_id)
            new_else_id = self._optimize_node(node.else_id)
            
            if not all([new_cond_id, new_then_id, new_else_id]):
                return None
            
            new_if = If(
                condition_id=new_cond_id,
                then_id=new_then_id,
                else_id=new_else_id
            )
            return self.optimized.add_node(new_if)
        
        elif isinstance(node, Sequence):
            new_step_ids = []
            for step_id in node.step_ids:
                new_step_id = self._optimize_node(step_id)
                if not new_step_id:
                    return None
                new_step_ids.append(new_step_id)
            
            new_seq = Sequence(step_ids=new_step_ids)
            return self.optimized.add_node(new_seq)
        
        # For other node types, just copy
        return self.optimized.add_node(node)
    
    def _create_literal(self, value: Any) -> Literal:
        """Create a literal node from a value"""
        if isinstance(value, bool):
            return Literal(value=value, literal_type='bool')
        elif isinstance(value, int):
            return Literal(value=value, literal_type='int')
        elif isinstance(value, float):
            return Literal(value=value, literal_type='float')
        elif isinstance(value, str):
            return Literal(value=value, literal_type='string')
        elif isinstance(value, list):
            return Literal(value=value, literal_type='list')
        else:
            return Literal(value=value, literal_type='unknown')
    
    def _eliminate_dead_code(self):
        """Remove unreachable nodes"""
        if not self.optimized or not self.optimized.root_id:
            return
        
        # Find reachable nodes
        reachable = set()
        
        def mark_reachable(node_id: str):
            if node_id in reachable:
                return
            reachable.add(node_id)
            
            node = self.optimized.get_node(node_id)
            if node:
                for dep_id in node.get_dependencies():
                    mark_reachable(dep_id)
        
        mark_reachable(self.optimized.root_id)
        
        # Remove unreachable nodes
        unreachable = set(self.optimized.nodes.keys()) - reachable
        for node_id in unreachable:
            del self.optimized.nodes[node_id]
            self.stats.dead_code_eliminated += 1