"""
Tracing Interpreter for ClaudeLang

This interpreter extends the base interpreter to record detailed execution
traces for documentation and analysis purposes.
"""

from typing import Any, Optional, List, Tuple, Dict
import time

from .interpreter import Interpreter, Value, Environment
from ..core.ast import Graph, ASTNode, NodeType, EffectType, Application, Effect
from ..docs.trace_analyzer import ExecutionTraceAnalyzer, TraceEvent, TraceEventType


class TracingInterpreter(Interpreter):
    """Interpreter that records execution traces"""
    
    def __init__(self, trace_analyzer: Optional[ExecutionTraceAnalyzer] = None, **kwargs):
        super().__init__(**kwargs)
        self.trace_analyzer = trace_analyzer or ExecutionTraceAnalyzer()
        self.trace_enabled = True
        self._call_depth = 0
        self._current_function_name = None
    
    def eval_node(self, node_id: str, graph: Graph, env: Environment) -> Value:
        """Evaluate a node with tracing"""
        # Record node execution start
        start_time = time.time()
        node = graph.get_node(node_id)
        
        # Call parent implementation
        result = super().eval_node(node_id, graph, env)
        
        # Record execution time
        duration = time.time() - start_time
        
        # Trace specific node types
        if self.trace_enabled and node:
            if node.node_type == NodeType.IF:
                # Record branch taken
                self._trace_branch(node_id, node, result)
            
        return result
    
    def _eval_application(self, node: Application, graph: Graph, env: Environment) -> Value:
        """Evaluate function application with tracing"""
        # Get function name if available
        func_node = graph.get_node(node.function_id)
        func_name = self._get_function_name(func_node, env)
        
        # Evaluate arguments for tracing
        arg_vals = []
        arg_names = []
        for arg_id in node.argument_ids:
            arg_val = self.eval_node(arg_id, graph, env)
            arg_vals.append(arg_val)
            # Try to get parameter name
            arg_node = graph.get_node(arg_id)
            if arg_node and hasattr(arg_node, 'name'):
                arg_names.append(arg_node.name)
            else:
                arg_names.append(f"arg{len(arg_names)}")
        
        # Record function call
        if self.trace_enabled:
            params = list(zip(arg_names, [v.data for v in arg_vals]))
            self._trace_function_call(func_name, params, node_id)
        
        # Store current function name
        prev_function = self._current_function_name
        self._current_function_name = func_name
        self._call_depth += 1
        
        try:
            # Call parent implementation
            result = super()._eval_application(node, graph, env)
            
            # Record function return
            if self.trace_enabled:
                self._trace_function_return(func_name, result, node_id)
            
            return result
            
        finally:
            self._call_depth -= 1
            self._current_function_name = prev_function
    
    def _eval_effect(self, node: Effect, graph: Graph, env: Environment) -> Value:
        """Evaluate effect with tracing"""
        # Record effect before execution
        if self.trace_enabled:
            self._trace_effect(node.effect_type, node.operation, node_id=str(node))
        
        # Call parent implementation
        result = super()._eval_effect(node, graph, env)
        
        return result
    
    def _apply_closure(self, closure: Dict, args: List[Value], graph: Graph) -> Value:
        """Apply closure with enhanced tracing"""
        # Extract function name from closure
        func_name = closure.get('name', '<anonymous>')
        param_names = closure.get('params', [])
        
        # Build parameter list
        params = list(zip(param_names, [arg.data for arg in args]))
        
        # Record function call
        if self.trace_enabled:
            self._trace_function_call(func_name, params, closure.get('body_id', 'unknown'))
        
        # Store current function
        prev_function = self._current_function_name
        self._current_function_name = func_name
        self._call_depth += 1
        
        start_time = time.time()
        effects_before = set(self.effect_context.effects_performed) if hasattr(self, 'effect_context') else set()
        
        try:
            # Call parent implementation
            result = super()._apply_closure(closure, args, graph)
            
            # Calculate effects used
            effects_after = set(self.effect_context.effects_performed) if hasattr(self, 'effect_context') else set()
            effects_used = effects_after - effects_before
            
            # Record function return with timing
            if self.trace_enabled:
                duration = time.time() - start_time
                self._trace_function_return(func_name, result, closure.get('body_id', 'unknown'), 
                                          duration=duration, effects=effects_used)
            
            return result
            
        except Exception as e:
            # Record exception
            if self.trace_enabled:
                self._trace_exception(func_name, e)
            raise
            
        finally:
            self._call_depth -= 1
            self._current_function_name = prev_function
    
    def _trace_function_call(self, func_name: str, params: List[Tuple[str, Any]], 
                            call_site: str):
        """Record function call event"""
        event = TraceEvent(
            event_type=TraceEventType.FUNCTION_CALL,
            timestamp=time.time(),
            node_id=call_site,
            node_type=NodeType.APPLICATION,
            data={
                'function_name': func_name,
                'parameters': params,
                'call_site': call_site,
                'call_depth': self._call_depth
            }
        )
        self.trace_analyzer.record_event(event)
    
    def _trace_function_return(self, func_name: str, result: Value, call_site: str,
                              duration: Optional[float] = None, 
                              effects: Optional[set] = None):
        """Record function return event"""
        event = TraceEvent(
            event_type=TraceEventType.FUNCTION_RETURN,
            timestamp=time.time(),
            node_id=call_site,
            node_type=NodeType.APPLICATION,
            data={
                'function_name': func_name,
                'return_value': result.data if result else None,
                'call_site': call_site,
                'duration': duration,
                'effects': list(effects) if effects else []
            }
        )
        self.trace_analyzer.record_event(event)
    
    def _trace_branch(self, node_id: str, node: ASTNode, result: Value):
        """Record branch taken event"""
        # Determine which branch was taken based on the result
        # This is simplified - would need to track which branch was actually executed
        branch_taken = bool(result.data) if hasattr(result, 'data') else True
        
        event = TraceEvent(
            event_type=TraceEventType.BRANCH_TAKEN,
            timestamp=time.time(),
            node_id=node_id,
            node_type=NodeType.IF,
            data={
                'branch': branch_taken,
                'condition_value': result.data if hasattr(result, 'data') else None
            }
        )
        self.trace_analyzer.record_event(event)
    
    def _trace_effect(self, effect_type: EffectType, operation: str, node_id: str):
        """Record effect triggered event"""
        event = TraceEvent(
            event_type=TraceEventType.EFFECT_TRIGGERED,
            timestamp=time.time(),
            node_id=node_id,
            node_type=NodeType.EFFECT,
            data={
                'effect_type': effect_type,
                'operation': operation,
                'function': self._current_function_name
            }
        )
        self.trace_analyzer.record_event(event)
    
    def _trace_exception(self, func_name: str, exception: Exception):
        """Record exception event"""
        event = TraceEvent(
            event_type=TraceEventType.EXCEPTION_RAISED,
            timestamp=time.time(),
            node_id='exception',
            node_type=NodeType.APPLICATION,
            data={
                'function_name': func_name,
                'exception_type': type(exception).__name__,
                'exception_message': str(exception)
            }
        )
        self.trace_analyzer.record_event(event)
    
    def _get_function_name(self, func_node: Optional[ASTNode], env: Environment) -> str:
        """Extract function name from node or environment"""
        if func_node:
            if hasattr(func_node, 'name'):
                return func_node.name
            elif func_node.node_type == NodeType.VARIABLE:
                # Look up variable name
                return func_node.name
        return '<anonymous>'
    
    def enable_tracing(self):
        """Enable execution tracing"""
        self.trace_enabled = True
    
    def disable_tracing(self):
        """Disable execution tracing"""
        self.trace_enabled = False
    
    def get_trace_analyzer(self) -> ExecutionTraceAnalyzer:
        """Get the trace analyzer"""
        return self.trace_analyzer
    
    def generate_documentation(self) -> str:
        """Generate documentation from collected traces"""
        return self.trace_analyzer.generate_documentation()
    
    def export_traces(self, filename: str):
        """Export trace data to file"""
        self.trace_analyzer.export_trace_data(filename)