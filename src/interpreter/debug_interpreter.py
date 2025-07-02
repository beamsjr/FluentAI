"""
Debug-enabled interpreter for ClaudeLang

Extends the base interpreter with debugging capabilities.
"""

from typing import Optional, Any, List, Dict
import time

from .interpreter import Interpreter, Value, Environment
from ..core.ast import ASTNode, Function, Lambda, Application
from ..core.graph import Graph
from ..debugger.debugger import Debugger, DebuggerState


class DebugInterpreter(Interpreter):
    """Interpreter with debugging support"""
    
    def __init__(self, debugger: Optional[Debugger] = None, **kwargs):
        super().__init__(**kwargs)
        self.debugger = debugger
        self._call_depth = 0
        self._in_debug_mode = False
    
    def eval_node(self, node_id: str, graph: Graph, env: Environment) -> Value:
        """Evaluate a single node with debug hooks"""
        node = graph.get_node(node_id)
        if not node:
            raise ValueError(f"Node {node_id} not found")
        
        # Call debugger hook if attached and not in debug mode
        # (avoid recursion when debugger evaluates expressions)
        if self.debugger and not self._in_debug_mode:
            self.debugger.on_node_enter(node, graph, env)
        
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
        
        # Call debugger exit hook
        if self.debugger and not self._in_debug_mode:
            self.debugger.on_node_exit(node, result.data)
        
        return result
    
    def _eval_application(self, node: Application, graph: Graph, env: Environment) -> Value:
        """Evaluate function application with debug hooks"""
        # Get function name for debugging
        func_name = None
        if node.function_id:
            func_node = graph.get_node(node.function_id)
            if isinstance(func_node, Function):
                func_name = func_node.name
            elif isinstance(func_node, Variable):
                func_name = func_node.name
        
        # Evaluate arguments
        args = []
        for arg_id in node.argument_ids:
            arg_value = self.eval_node(arg_id, graph, env)
            args.append(arg_value.data)
        
        # Call function enter hook
        if self.debugger and func_name and not self._in_debug_mode:
            self.debugger.on_function_enter(func_name, args)
            self._call_depth += 1
        
        try:
            # Call parent implementation
            result = super()._eval_application(node, graph, env)
            
            # Call function exit hook
            if self.debugger and func_name and not self._in_debug_mode:
                self._call_depth -= 1
                self.debugger.on_function_exit(func_name, result.data)
            
            return result
            
        except Exception as e:
            # Call function exit hook on error
            if self.debugger and func_name and not self._in_debug_mode:
                self._call_depth -= 1
                self.debugger.on_function_exit(func_name, None)
            
            # Call exception hook if it's a ClaudeLang error
            if self.debugger and not self._in_debug_mode:
                from ..errors.base import ClaudeLangError
                if isinstance(e, ClaudeLangError):
                    self.debugger.on_exception(e)
            
            raise
    
    def debug_evaluate(self, expression: str, frame_env: Optional[Environment] = None) -> Any:
        """Evaluate an expression in debug context (avoids recursion)"""
        self._in_debug_mode = True
        try:
            # Parse the expression
            from ..core.cache import cached_parse
            graph = cached_parse(expression)
            
            # Use provided environment or current global
            env = frame_env or self.global_env
            
            # Evaluate
            result = self.eval_node(graph.root_id, graph, env)
            return result.data
            
        finally:
            self._in_debug_mode = False
    
    def get_variable_value(self, name: str, env: Environment) -> Optional[Any]:
        """Get a variable value from environment (for debugger)"""
        value = env.lookup(name)
        if value:
            return value.data
        return None
    
    def get_all_variables(self, env: Environment) -> Dict[str, Any]:
        """Get all variables in environment (for debugger)"""
        variables = {}
        
        # Get local variables
        for name, value in env.bindings.items():
            variables[name] = value.data
        
        # Get parent variables if requested
        if env.parent:
            parent_vars = self.get_all_variables(env.parent)
            # Local variables shadow parent ones
            for name, value in parent_vars.items():
                if name not in variables:
                    variables[f"parent.{name}"] = value
        
        return variables