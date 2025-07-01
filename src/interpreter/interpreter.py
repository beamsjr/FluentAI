"""
ClaudeLang Interpreter

This module implements an interpreter for executing ClaudeLang AST graphs.
The interpreter handles effects explicitly and maintains provenance information.
"""

from typing import Dict, Any, List, Optional, Set, Tuple, Union
from dataclasses import dataclass, field
from collections import defaultdict
import time
from ..core.ast import *
from ..core.primitives import PRIMITIVES
from ..core.cache import cached_parse
from ..types.type_system import TypeChecker, TypeEnvironment
from ..effects.handlers import EffectContext, create_default_handler
from ..effects.primitives import set_effect_context
from ..contracts.verification import ContractVerifier, ContractViolation, register_contract_predicates


@dataclass
class Value:
    """Runtime value representation"""
    data: Any
    type_info: Optional[TypeAnnotation] = None
    provenance: List[str] = field(default_factory=list)  # Node IDs that produced this value
    timestamp: float = field(default_factory=time.time)
    effects_triggered: List[Tuple[EffectType, Any]] = field(default_factory=list)


@dataclass
class Environment:
    """Runtime environment for variable bindings"""
    bindings: Dict[str, Value] = field(default_factory=dict)
    parent: Optional['Environment'] = None
    
    def bind(self, name: str, value: Value):
        """Bind a variable to a value"""
        self.bindings[name] = value
    
    def define(self, name: str, value: Any):
        """Define a variable (for contract predicates)"""
        # Wrap raw values in Value objects
        if not isinstance(value, Value):
            value = Value(data=value)
        self.bindings[name] = value
    
    def lookup(self, name: str) -> Optional[Value]:
        """Look up a variable"""
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent:
            return self.parent.lookup(name)
        return None
    
    def extend(self) -> 'Environment':
        """Create a child environment"""
        return Environment(parent=self)


@dataclass
class EffectHandler:
    """Handler for managing effects"""
    name: str
    handler: callable
    
    def handle(self, effect_type: EffectType, operation: str, args: List[Value]) -> Value:
        """Handle an effect"""
        return self.handler(effect_type, operation, args)


class Interpreter:
    """ClaudeLang interpreter"""
    
    def __init__(self, effect_handler=None, enable_contracts=True):
        self.global_env = Environment()
        self.execution_trace: List[Dict[str, Any]] = []
        self.type_checker = TypeChecker()
        self.graph = None  # Will be set when evaluating
        
        # Use new effect system
        self.effect_handler = effect_handler or create_default_handler()
        self.effect_context = EffectContext(self.effect_handler)
        
        # Set global effect context for primitives
        set_effect_context(self.effect_context)
        
        # Keep legacy handlers for compatibility
        self.effect_handlers: Dict[EffectType, EffectHandler] = {}
        self._setup_default_handlers()
        
        # Contract verification
        self.contract_verifier = ContractVerifier(self)
        self.contract_verifier.enabled = enable_contracts
        
        # Initialize standard library
        self._init_stdlib()
        
        # Register contract predicates
        register_contract_predicates(self.global_env)
        
        # Register contract control functions
        self._register_contract_functions()
    
    def _setup_default_handlers(self):
        """Set up default effect handlers"""
        
        # IO handler
        self.register_handler(
            EffectType.IO,
            EffectHandler(
                "io",
                lambda et, op, args: self._handle_io(op, args)
            )
        )
        
        # Error handler
        self.register_handler(
            EffectType.ERROR,
            EffectHandler(
                "error",
                lambda et, op, args: self._handle_error(op, args)
            )
        )
        
        # State handler
        self.register_handler(
            EffectType.STATE,
            EffectHandler(
                "state",
                lambda et, op, args: self._handle_state(op, args)
            )
        )
    
    def register_handler(self, effect_type: EffectType, handler: EffectHandler):
        """Register an effect handler"""
        self.effect_handlers[effect_type] = handler
    
    def interpret(self, graph: Graph, env: Optional[Environment] = None) -> Value:
        """Interpret a graph starting from its root"""
        if not graph.root_id:
            raise ValueError("Graph has no root node")
        
        # Store graph for contract verification
        self.graph = graph
        
        # Register any contracts in the graph
        self._register_contracts(graph)
        
        # Type check first (disabled for now)
        # valid, errors = self.type_checker.type_check(graph)
        # if not valid:
        #     raise TypeError(f"Type errors: {errors}")
        
        env = env or self.global_env
        return self.eval_node(graph.root_id, graph, env)
    
    def _register_contracts(self, graph: Graph):
        """Register all contracts found in the graph"""
        for node in graph.nodes.values():
            if node.node_type == NodeType.CONTRACT:
                self.contract_verifier.register_contract(node)
    
    def eval(self, source: str, env: Optional[Environment] = None) -> Value:
        """Parse and evaluate ClaudeLang source code with caching"""
        graph = cached_parse(source)
        return self.interpret(graph, env)
    
    def evaluate_node(self, node_id: str, env: Environment) -> Any:
        """Evaluate a node (used by contract verifier)"""
        if not self.graph:
            raise ValueError("No graph set for evaluation")
        result = self.eval_node(node_id, self.graph, env)
        return result.data
    
    def eval_node(self, node_id: str, graph: Graph, env: Environment) -> Value:
        """Evaluate a single node"""
        node = graph.get_node(node_id)
        if not node:
            raise ValueError(f"Node {node_id} not found")
        
        # Record execution
        start_time = time.time()
        self.execution_trace.append({
            "node_id": node_id,
            "node_type": node.node_type.name,
            "timestamp": start_time
        })
        
        # Dispatch based on node type
        result = self._eval_dispatch(node, graph, env)
        
        # Add provenance
        result.provenance.append(node_id)
        
        # Record completion
        self.execution_trace[-1]["duration"] = time.time() - start_time
        self.execution_trace[-1]["result_type"] = str(result.type_info) if result.type_info else "unknown"
        
        return result
    
    def _eval_dispatch(self, node: ASTNode, graph: Graph, env: Environment) -> Value:
        """Dispatch evaluation based on node type"""
        
        if isinstance(node, Literal):
            return self._eval_literal(node)
        
        elif isinstance(node, Variable):
            return self._eval_variable(node, env)
        
        elif isinstance(node, Function):
            return self._eval_function(node)
        
        elif isinstance(node, Application):
            return self._eval_application(node, graph, env)
        
        elif isinstance(node, Lambda):
            return self._eval_lambda(node, graph, env)
        
        elif isinstance(node, Let):
            return self._eval_let(node, graph, env)
        
        elif isinstance(node, If):
            return self._eval_if(node, graph, env)
        
        elif isinstance(node, Effect):
            return self._eval_effect(node, graph, env)
        
        elif isinstance(node, Sequence):
            return self._eval_sequence(node, graph, env)
        
        elif isinstance(node, Parallel):
            return self._eval_parallel(node, graph, env)
        
        elif isinstance(node, Uncertainty):
            return self._eval_uncertainty(node, graph, env)
        
        elif hasattr(node, 'node_type'):
            # Handle module system nodes
            if node.node_type == NodeType.MODULE:
                return self._eval_module(node, graph, env)
            elif node.node_type == NodeType.IMPORT:
                return self._eval_import(node, graph, env)
            elif node.node_type == NodeType.EXPORT:
                return self._eval_export(node, graph, env)
            elif node.node_type == NodeType.QUALIFIED_VAR:
                return self._eval_qualified_var(node, env)
            elif node.node_type == NodeType.MATCH:
                return self._eval_match(node, graph, env)
        
        else:
            raise ValueError(f"Unknown node type: {node.node_type}")
    
    def _eval_literal(self, node: Literal) -> Value:
        """Evaluate a literal"""
        return Value(
            data=node.value,
            type_info=node.type_annotation
        )
    
    def _eval_variable(self, node: Variable, env: Environment) -> Value:
        """Evaluate a variable"""
        value = env.lookup(node.name)
        if value is None:
            # Check if it's a built-in function
            func = PRIMITIVES.get_function(node.name)
            if func:
                return Value(
                    data=func,
                    type_info=func.type_annotation
                )
            from ..errors.exceptions import NameError as ClaudeLangNameError
            raise ClaudeLangNameError(node.name)
        return value
    
    def _eval_function(self, node: Function) -> Value:
        """Evaluate a function reference"""
        return Value(
            data=node,
            type_info=node.type_annotation
        )
    
    def _eval_application(self, node: Application, graph: Graph, env: Environment) -> Value:
        """Evaluate function application"""
        # Evaluate function
        func_val = self.eval_node(node.function_id, graph, env)
        
        # Evaluate arguments
        arg_vals = []
        for arg_id in node.argument_ids:
            arg_vals.append(self.eval_node(arg_id, graph, env))
        
        # Apply function
        if isinstance(func_val.data, Function):
            # Built-in function
            impl = PRIMITIVES.get_implementation(func_val.data.name)
            if impl:
                try:
                    args = [arg.data for arg in arg_vals]
                    # Handle variadic functions
                    if func_val.data.arity == -1:
                        result = impl(*args)
                    else:
                        result = impl(*args)
                    return Value(
                        data=result,
                        type_info=func_val.data.type_annotation.parameters[-1] if func_val.data.type_annotation and func_val.data.type_annotation.parameters else None
                    )
                except Exception as e:
                    # Handle errors as effect
                    return self._handle_error("application_error", [Value(data=str(e))])
            else:
                raise ValueError(f"No implementation for function: {func_val.data.name}")
        
        elif isinstance(func_val.data, dict) and 'type' in func_val.data and func_val.data['type'] == 'closure':
            # User-defined function (closure)
            return self._apply_closure(func_val.data, arg_vals, graph)
        
        else:
            raise TypeError(f"Cannot apply non-function: {func_val.data}")
    
    def _eval_lambda(self, node: Lambda, graph: Graph, env: Environment) -> Value:
        """Evaluate lambda to create closure"""
        closure = {
            'type': 'closure',
            'params': node.parameter_names,
            'body_id': node.body_id,
            'env': env,
            'captured': node.captured_variables,
            'name': getattr(node, 'function_name', '<anonymous>')  # Track function name
        }
        
        return Value(
            data=closure,
            type_info=node.type_annotation
        )
    
    def _apply_closure(self, closure: Dict, args: List[Value], graph: Graph) -> Value:
        """Apply a closure with contract verification"""
        if len(args) != len(closure['params']):
            raise ValueError(f"Arity mismatch: expected {len(closure['params'])}, got {len(args)}")
        
        # Check if this closure has a contract
        function_name = closure.get('name', '<anonymous>')
        contract_context = None
        
        if self.contract_verifier.enabled and function_name in self.contract_verifier.contracts:
            # Verify preconditions
            arg_values = [arg.data for arg in args]
            # Create environment with parameter bindings for contract checking
            contract_env = closure['env'].extend()
            for param, arg in zip(closure['params'], args):
                contract_env.bind(param, arg)
            contract_context = self.contract_verifier.verify_function_call(
                function_name, arg_values, closure['params'], contract_env
            )
        
        # Create new environment
        new_env = closure['env'].extend()
        
        # Bind parameters
        for param, arg in zip(closure['params'], args):
            new_env.bind(param, arg)
        
        # Track effects if contract checking is enabled
        effects_before = set()  # TODO: Implement proper effect tracking
        
        try:
            # Evaluate body
            result = self.eval_node(closure['body_id'], graph, new_env)
            
            # Verify postconditions if contract exists
            if contract_context:
                effects_after = set()  # TODO: Implement proper effect tracking
                effects_used = effects_after - effects_before
                
                self.contract_verifier.verify_function_return(
                    contract_context, result.data, new_env, effects_used
                )
            
            return result
            
        except Exception as e:
            # Clean up contract context on exception
            if contract_context and self.contract_verifier.contract_stack:
                if self.contract_verifier.contract_stack[-1] == contract_context:
                    self.contract_verifier.contract_stack.pop()
            raise
    
    def _eval_let(self, node: Let, graph: Graph, env: Environment) -> Value:
        """Evaluate let binding"""
        new_env = env.extend()
        
        # Evaluate bindings
        for binding in node.bindings:
            value = self.eval_node(binding['value_id'], graph, new_env)
            
            # If binding a closure, associate the name with it
            if (isinstance(value.data, dict) and 
                value.data.get('type') == 'closure' and 
                value.data.get('name') == '<anonymous>'):
                value.data['name'] = binding['name']
            
            new_env.bind(binding['name'], value)
        
        # Evaluate body
        return self.eval_node(node.body_id, graph, new_env)
    
    def _eval_if(self, node: If, graph: Graph, env: Environment) -> Value:
        """Evaluate conditional"""
        cond_val = self.eval_node(node.condition_id, graph, env)
        
        if cond_val.data:
            return self.eval_node(node.then_id, graph, env)
        else:
            return self.eval_node(node.else_id, graph, env)
    
    def _eval_effect(self, node: Effect, graph: Graph, env: Environment) -> Value:
        """Evaluate effect operation"""
        # Evaluate arguments
        arg_vals = []
        for arg_id in node.argument_ids:
            val = self.eval_node(arg_id, graph, env)
            arg_vals.append(val.data if hasattr(val, 'data') else val)
        
        # Use new effect context
        try:
            result_data = self.effect_context.perform(node.effect_type, node.operation, *arg_vals)
            return Value(
                data=result_data,
                effects_triggered=[(node.effect_type, node.operation)]
            )
        except Exception as e:
            # Fallback to old handlers if new system fails
            handler = self.effect_handlers.get(node.effect_type)
            if handler:
                result = handler.handle(node.effect_type, node.operation, 
                                      [Value(data=v) for v in arg_vals])
                result.effects_triggered.append((node.effect_type, node.operation))
                return result
            else:
                raise ValueError(f"No handler for effect: {node.effect_type}") from e
    
    def _eval_sequence(self, node: Sequence, graph: Graph, env: Environment) -> Value:
        """Evaluate sequence of expressions"""
        result = None
        for step_id in node.step_ids:
            result = self.eval_node(step_id, graph, env)
        
        return result or Value(data=None)
    
    def _eval_parallel(self, node: Parallel, graph: Graph, env: Environment) -> Value:
        """Evaluate parallel branches"""
        # In a real implementation, this would use actual parallelism
        # For now, we evaluate sequentially and collect results
        results = []
        
        for branch_id in node.branch_ids:
            results.append(self.eval_node(branch_id, graph, env))
        
        # Merge results based on strategy
        if node.merge_strategy == "tuple":
            return Value(
                data=tuple(r.data for r in results),
                type_info=TypeAnnotation("Tuple")
            )
        else:
            return Value(data=results)
    
    def _eval_uncertainty(self, node: Uncertainty, graph: Graph, env: Environment) -> Value:
        """Evaluate probabilistic choice"""
        import random
        
        # Simple implementation: weighted random choice
        r = random.random()
        cumulative = 0.0
        
        for choice in node.choices:
            cumulative += choice['probability']
            if r <= cumulative:
                result = self.eval_node(choice['node_id'], graph, env)
                result.type_info = UncertainType(
                    base_type=result.type_info,
                    confidence=choice['probability']
                ) if result.type_info else None
                return result
        
        # Fallback to last choice
        last_choice = node.choices[-1]
        return self.eval_node(last_choice['node_id'], graph, env)
    
    def _handle_io(self, operation: str, args: List[Value]) -> Value:
        """Handle IO effects"""
        if operation == "print":
            for arg in args:
                print(arg.data)
            return Value(data=None)
        elif operation == "read":
            data = input()
            return Value(data=data)
        else:
            raise ValueError(f"Unknown IO operation: {operation}")
    
    def _handle_error(self, operation: str, args: List[Value]) -> Value:
        """Handle error effects"""
        error_msg = args[0].data if args else "Unknown error"
        return Value(
            data={"error": error_msg},
            type_info=TypeAnnotation("Error")
        )
    
    def _handle_state(self, operation: str, args: List[Value]) -> Value:
        """Handle state effects"""
        # Simple state handling - would be more sophisticated in real implementation
        if operation == "get":
            key = args[0].data
            return Value(data=self.global_env.lookup(key))
        elif operation == "set":
            key = args[0].data
            value = args[1]
            self.global_env.bind(key, value)
            return value
        else:
            raise ValueError(f"Unknown state operation: {operation}")
    
    def _init_stdlib(self):
        """Initialize standard library functions"""
        from ..stdlib.strings import register_string_functions
        from ..stdlib.core import register_core_functions
        from ..stdlib.math import register_math_functions
        from ..stdlib.io import register_io_functions
        from ..stdlib.data import register_data_functions
        from ..stdlib.functional import register_functional_functions
        from ..stdlib.datetime import register_datetime_functions
        
        # Register all stdlib modules
        register_string_functions()
        register_core_functions()
        register_math_functions()
        register_io_functions()
        register_data_functions()
        register_functional_functions()
        register_datetime_functions()
    
    def _register_contract_functions(self):
        """Register contract control functions"""
        # Enable contract checking function
        def enable_contracts():
            self.contract_verifier.enable()
            return True  # Return True instead of None
        
        # Disable contract checking function
        def disable_contracts():
            self.contract_verifier.disable()
            return True  # Return True instead of None
        
        # Register as primitives
        from ..core.primitives import PRIMITIVES
        from ..core.ast import Function
        
        PRIMITIVES.register(
            "enable-contract-checking!",
            Function(
                name="enable-contract-checking!",
                arity=0,
                effects={EffectType.STATE}
            ),
            enable_contracts
        )
        
        PRIMITIVES.register(
            "disable-contract-checking!",
            Function(
                name="disable-contract-checking!",
                arity=0,
                effects={EffectType.STATE}
            ),
            disable_contracts
        )
    
    def _eval_module(self, node: 'Module', graph: Graph, env: Environment) -> Value:
        """Evaluate a module - just evaluate its body"""
        from ..core.ast import Module
        if isinstance(node, Module):
            return self.eval_node(node.body_id, graph, env)
        else:
            # Generic module node
            body_id = node.children[0] if node.children else None
            if body_id:
                return self.eval_node(body_id, graph, env)
            return Value(None)
    
    def _eval_import(self, node: 'Import', graph: Graph, env: Environment) -> Value:
        """Import statements are handled at a higher level"""
        return Value(None)
    
    def _eval_export(self, node: 'Export', graph: Graph, env: Environment) -> Value:
        """Export statements are handled at module level"""
        return Value(None)
    
    def _eval_qualified_var(self, node: 'QualifiedVariable', env: Environment) -> Value:
        """Look up a qualified variable - should be resolved by module system"""
        from ..core.ast import QualifiedVariable
        if isinstance(node, QualifiedVariable):
            raise RuntimeError(f"Unresolved qualified variable: {node.module_name}:{node.variable_name}")
        else:
            # Generic qualified var
            module_name = node.attributes.get("module", "unknown")
            var_name = node.attributes.get("name", "unknown")
            raise RuntimeError(f"Unresolved qualified variable: {module_name}:{var_name}")
    
    def _eval_match(self, node: 'Match', graph: Graph, env: Environment) -> Value:
        """Evaluate pattern matching expression"""
        from ..core.ast import Match
        if not isinstance(node, Match):
            # Handle generic match node
            expr_id = node.attributes.get("expr_id")
            branches = node.attributes.get("branches", [])
        else:
            expr_id = node.expr_id
            branches = node.branches
        
        # Evaluate the expression to match against
        expr_val = self.eval_node(expr_id, graph, env)
        
        # Try each pattern branch
        for branch in branches:
            pattern_id = branch["pattern_id"]
            body_id = branch["body_id"]
            
            # Try to match the pattern
            bindings = self._match_pattern(pattern_id, expr_val.data, graph)
            
            if bindings is not None:
                # Pattern matched - evaluate body with bindings
                match_env = env.extend()
                for name, value in bindings.items():
                    match_env.bind(name, Value(value))
                
                return self.eval_node(body_id, graph, match_env)
        
        # No patterns matched
        from ..errors.exceptions import PatternMatchError
        raise PatternMatchError(expr_val.data)
    
    def _match_pattern(self, pattern_id: str, value: Any, graph: Graph) -> Optional[Dict[str, Any]]:
        """Try to match a pattern against a value. Returns bindings or None."""
        pattern = graph.nodes[pattern_id]
        
        from ..core.ast import (PatternLiteral, PatternVar, PatternWildcard, 
                                PatternConstructor, PatternList)
        
        if isinstance(pattern, PatternLiteral):
            # Literal pattern - exact match
            if pattern.value == value:
                return {}
            return None
        
        elif isinstance(pattern, PatternVar):
            # Variable pattern - always matches, creates binding
            return {pattern.name: value}
        
        elif isinstance(pattern, PatternWildcard):
            # Wildcard - always matches, no bindings
            return {}
        
        elif isinstance(pattern, PatternList):
            # List pattern
            if not isinstance(value, list):
                return None
            
            bindings = {}
            
            # Match fixed elements
            if len(value) < len(pattern.elements):
                return None
            
            for i, elem_pattern_id in enumerate(pattern.elements):
                elem_bindings = self._match_pattern(elem_pattern_id, value[i], graph)
                if elem_bindings is None:
                    return None
                bindings.update(elem_bindings)
            
            # Match rest pattern if present
            if pattern.rest_pattern:
                rest_value = value[len(pattern.elements):]
                rest_bindings = self._match_pattern(pattern.rest_pattern, rest_value, graph)
                if rest_bindings is None:
                    return None
                bindings.update(rest_bindings)
            elif len(value) != len(pattern.elements):
                # No rest pattern but lengths don't match
                return None
            
            return bindings
        
        elif isinstance(pattern, PatternConstructor):
            # Constructor pattern - check if value is a tuple with constructor tag
            if not isinstance(value, tuple) or len(value) < 1:
                return None
            
            # First element should be the constructor name
            if value[0] != pattern.constructor:
                return None
            
            # Match sub-patterns against remaining elements
            if len(value) - 1 != len(pattern.sub_patterns):
                return None
            
            bindings = {}
            for i, sub_pattern_id in enumerate(pattern.sub_patterns):
                sub_bindings = self._match_pattern(sub_pattern_id, value[i + 1], graph)
                if sub_bindings is None:
                    return None
                bindings.update(sub_bindings)
            
            return bindings
        
        else:
            # Handle generic pattern nodes
            if hasattr(pattern, 'node_type'):
                if pattern.node_type == NodeType.PATTERN_LITERAL:
                    value_attr = pattern.attributes.get("value")
                    if value_attr == value:
                        return {}
                    return None
                elif pattern.node_type == NodeType.PATTERN_VAR:
                    name = pattern.attributes.get("name", "_")
                    return {name: value}
                elif pattern.node_type == NodeType.PATTERN_WILDCARD:
                    return {}
            
            raise ValueError(f"Unknown pattern type: {pattern}")