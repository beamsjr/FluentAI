"""
Core debugger implementation for ClaudeLang

Provides breakpoint management, step execution, and debugging hooks.
"""

import sys
from typing import Dict, List, Optional, Any, Callable, Set, Tuple
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
import threading
import queue

from ..core.ast import ASTNode
from ..core.graph import Graph
from ..interpreter.interpreter import Environment
from ..errors.exceptions import ClaudeLangError


class DebuggerState(Enum):
    """Debugger execution state"""
    IDLE = auto()
    RUNNING = auto()
    PAUSED = auto()
    STEPPING = auto()
    STEPPING_IN = auto()
    STEPPING_OUT = auto()
    STEPPING_OVER = auto()


@dataclass
class Breakpoint:
    """Represents a breakpoint"""
    id: int
    file: str
    line: int
    condition: Optional[str] = None
    hit_count: int = 0
    enabled: bool = True
    log_message: Optional[str] = None
    
    def should_break(self, context: Dict[str, Any]) -> bool:
        """Check if breakpoint should trigger"""
        if not self.enabled:
            return False
            
        if self.condition:
            # Evaluate condition in context
            try:
                # TODO: Evaluate condition expression
                return True
            except:
                return False
        
        return True


@dataclass
class StackFrame:
    """Represents a stack frame in the call stack"""
    id: int
    name: str
    file: str
    line: int
    column: int
    locals: Dict[str, Any] = field(default_factory=dict)
    node_id: Optional[str] = None


@dataclass
class DebugEvent:
    """Event emitted by the debugger"""
    type: str  # 'breakpoint', 'step', 'exception', 'output'
    data: Dict[str, Any]


class Debugger:
    """ClaudeLang debugger implementation"""
    
    def __init__(self):
        self.state = DebuggerState.IDLE
        self.breakpoints: Dict[int, Breakpoint] = {}
        self.breakpoint_counter = 0
        self.call_stack: List[StackFrame] = []
        self.frame_counter = 0
        
        # Current execution context
        self.current_node: Optional[ASTNode] = None
        self.current_graph: Optional[Graph] = None
        self.current_env: Optional[Environment] = None
        
        # Step execution tracking
        self.step_depth = 0
        self.step_target_depth = 0
        
        # Event handling
        self.event_handlers: Dict[str, List[Callable]] = {}
        self.event_queue = queue.Queue()
        
        # Breakpoint lookup by location
        self._breakpoint_lookup: Dict[Tuple[str, int], List[Breakpoint]] = {}
        
        # Thread safety
        self._lock = threading.Lock()
        
        # Watch expressions
        self.watches: Dict[str, str] = {}  # name -> expression
        
        # Exception breakpoints
        self.break_on_exception = False
        self.exception_filters: Set[str] = set()
    
    def add_breakpoint(
        self, 
        file: str, 
        line: int,
        condition: Optional[str] = None,
        log_message: Optional[str] = None
    ) -> Breakpoint:
        """Add a breakpoint"""
        with self._lock:
            self.breakpoint_counter += 1
            bp = Breakpoint(
                id=self.breakpoint_counter,
                file=file,
                line=line,
                condition=condition,
                log_message=log_message
            )
            
            self.breakpoints[bp.id] = bp
            
            # Add to lookup
            key = (file, line)
            if key not in self._breakpoint_lookup:
                self._breakpoint_lookup[key] = []
            self._breakpoint_lookup[key].append(bp)
            
            self._emit_event('breakpointAdded', {'breakpoint': self._bp_to_dict(bp)})
            return bp
    
    def remove_breakpoint(self, bp_id: int) -> bool:
        """Remove a breakpoint"""
        with self._lock:
            if bp_id not in self.breakpoints:
                return False
                
            bp = self.breakpoints.pop(bp_id)
            
            # Remove from lookup
            key = (bp.file, bp.line)
            if key in self._breakpoint_lookup:
                self._breakpoint_lookup[key].remove(bp)
                if not self._breakpoint_lookup[key]:
                    del self._breakpoint_lookup[key]
            
            self._emit_event('breakpointRemoved', {'id': bp_id})
            return True
    
    def toggle_breakpoint(self, bp_id: int) -> bool:
        """Toggle breakpoint enabled state"""
        with self._lock:
            if bp_id not in self.breakpoints:
                return False
                
            bp = self.breakpoints[bp_id]
            bp.enabled = not bp.enabled
            
            self._emit_event('breakpointChanged', {'breakpoint': self._bp_to_dict(bp)})
            return True
    
    def clear_breakpoints(self, file: Optional[str] = None) -> int:
        """Clear all breakpoints or breakpoints in a file"""
        with self._lock:
            if file:
                # Clear breakpoints in specific file
                to_remove = [bp for bp in self.breakpoints.values() if bp.file == file]
                for bp in to_remove:
                    self.remove_breakpoint(bp.id)
                return len(to_remove)
            else:
                # Clear all breakpoints
                count = len(self.breakpoints)
                self.breakpoints.clear()
                self._breakpoint_lookup.clear()
                self._emit_event('breakpointsCleared', {})
                return count
    
    def continue_execution(self) -> None:
        """Continue execution until next breakpoint"""
        with self._lock:
            if self.state == DebuggerState.PAUSED:
                self.state = DebuggerState.RUNNING
                self._emit_event('continued', {})
    
    def pause_execution(self) -> None:
        """Pause execution at next opportunity"""
        with self._lock:
            if self.state == DebuggerState.RUNNING:
                self.state = DebuggerState.PAUSED
                self._emit_event('paused', {'reason': 'pause'})
    
    def step_over(self) -> None:
        """Step over to next statement at same level"""
        with self._lock:
            if self.state == DebuggerState.PAUSED:
                self.state = DebuggerState.STEPPING_OVER
                self.step_target_depth = self.step_depth
                self._emit_event('stepped', {'type': 'stepOver'})
    
    def step_into(self) -> None:
        """Step into function calls"""
        with self._lock:
            if self.state == DebuggerState.PAUSED:
                self.state = DebuggerState.STEPPING_IN
                self._emit_event('stepped', {'type': 'stepIn'})
    
    def step_out(self) -> None:
        """Step out of current function"""
        with self._lock:
            if self.state == DebuggerState.PAUSED:
                self.state = DebuggerState.STEPPING_OUT
                self.step_target_depth = max(0, self.step_depth - 1)
                self._emit_event('stepped', {'type': 'stepOut'})
    
    def on_node_enter(self, node: ASTNode, graph: Graph, env: Environment) -> None:
        """Called when entering an AST node during evaluation"""
        with self._lock:
            self.current_node = node
            self.current_graph = graph
            self.current_env = env
            
            # Check if we should pause
            should_pause = False
            pause_reason = None
            
            # Check breakpoints
            if hasattr(node, 'source_location') and node.source_location:
                loc = node.source_location
                key = (loc.get('file', ''), loc.get('line', 0))
                
                if key in self._breakpoint_lookup:
                    for bp in self._breakpoint_lookup[key]:
                        if bp.should_break(self._get_context()):
                            bp.hit_count += 1
                            should_pause = True
                            pause_reason = 'breakpoint'
                            
                            if bp.log_message:
                                # Log message instead of breaking
                                self._emit_event('output', {
                                    'category': 'console',
                                    'output': self._format_log_message(bp.log_message)
                                })
                                should_pause = False
            
            # Check step conditions
            if self.state == DebuggerState.STEPPING_IN:
                should_pause = True
                pause_reason = 'step'
            elif self.state == DebuggerState.STEPPING_OVER:
                if self.step_depth <= self.step_target_depth:
                    should_pause = True
                    pause_reason = 'step'
            elif self.state == DebuggerState.STEPPING_OUT:
                if self.step_depth < self.step_target_depth:
                    should_pause = True
                    pause_reason = 'step'
            
            if should_pause:
                self.state = DebuggerState.PAUSED
                self._emit_event('paused', {
                    'reason': pause_reason,
                    'frame': self._current_frame_to_dict()
                })
                
                # Wait for continue signal
                self._wait_for_continue()
    
    def on_node_exit(self, node: ASTNode, result: Any) -> None:
        """Called when exiting an AST node after evaluation"""
        pass
    
    def on_function_enter(self, name: str, args: List[Any]) -> None:
        """Called when entering a function"""
        with self._lock:
            self.step_depth += 1
            self.frame_counter += 1
            
            frame = StackFrame(
                id=self.frame_counter,
                name=name,
                file=self.current_node.source_location.get('file', '') if self.current_node and hasattr(self.current_node, 'source_location') else '',
                line=self.current_node.source_location.get('line', 0) if self.current_node and hasattr(self.current_node, 'source_location') else 0,
                column=self.current_node.source_location.get('column', 0) if self.current_node and hasattr(self.current_node, 'source_location') else 0,
                locals={'arguments': args}
            )
            
            self.call_stack.append(frame)
            self._emit_event('stackChanged', {'stack': self._stack_to_dict()})
    
    def on_function_exit(self, name: str, result: Any) -> None:
        """Called when exiting a function"""
        with self._lock:
            self.step_depth -= 1
            if self.call_stack and self.call_stack[-1].name == name:
                self.call_stack.pop()
                self._emit_event('stackChanged', {'stack': self._stack_to_dict()})
    
    def on_exception(self, error: ClaudeLangError) -> None:
        """Called when an exception occurs"""
        with self._lock:
            if self.break_on_exception:
                # Check exception filters
                error_type = type(error).__name__
                if not self.exception_filters or error_type in self.exception_filters:
                    self.state = DebuggerState.PAUSED
                    self._emit_event('paused', {
                        'reason': 'exception',
                        'exception': {
                            'type': error_type,
                            'message': str(error),
                            'stackTrace': self._stack_to_dict()
                        }
                    })
                    self._wait_for_continue()
    
    def evaluate_expression(self, expression: str, frame_id: Optional[int] = None) -> Any:
        """Evaluate an expression in the current or specified frame context"""
        # Get the appropriate environment
        env = None
        if frame_id is not None:
            # Find specific frame
            for frame in self.call_stack:
                if frame.id == frame_id:
                    # Get environment from frame locals
                    # This would need to be set during function enter
                    env = frame.locals.get('__env__')
                    break
        else:
            env = self.current_env
        
        # Use the interpreter to evaluate if available
        if hasattr(self, '_interpreter') and self._interpreter:
            from ..interpreter.debug_interpreter import DebugInterpreter
            if isinstance(self._interpreter, DebugInterpreter):
                return self._interpreter.debug_evaluate(expression, env)
        
        # Fallback
        return f"<evaluated: {expression}>"
    
    def get_variables(self, frame_id: Optional[int] = None) -> Dict[str, Any]:
        """Get variables for current or specified frame"""
        if frame_id is not None:
            # Find specific frame
            for frame in self.call_stack:
                if frame.id == frame_id:
                    return frame.locals
        elif self.current_env:
            # Current frame
            return self._env_to_dict(self.current_env)
        
        return {}
    
    def add_watch(self, name: str, expression: str) -> None:
        """Add a watch expression"""
        self.watches[name] = expression
        self._emit_event('watchAdded', {'name': name, 'expression': expression})
    
    def remove_watch(self, name: str) -> bool:
        """Remove a watch expression"""
        if name in self.watches:
            del self.watches[name]
            self._emit_event('watchRemoved', {'name': name})
            return True
        return False
    
    def evaluate_watches(self) -> Dict[str, Any]:
        """Evaluate all watch expressions"""
        results = {}
        for name, expr in self.watches.items():
            try:
                results[name] = self.evaluate_expression(expr)
            except Exception as e:
                results[name] = f"<error: {e}>"
        return results
    
    def register_event_handler(self, event_type: str, handler: Callable) -> None:
        """Register an event handler"""
        if event_type not in self.event_handlers:
            self.event_handlers[event_type] = []
        self.event_handlers[event_type].append(handler)
    
    def _emit_event(self, event_type: str, data: Dict[str, Any]) -> None:
        """Emit a debug event"""
        event = DebugEvent(type=event_type, data=data)
        self.event_queue.put(event)
        
        # Call registered handlers
        if event_type in self.event_handlers:
            for handler in self.event_handlers[event_type]:
                try:
                    handler(event)
                except Exception as e:
                    print(f"Error in event handler: {e}", file=sys.stderr)
    
    def _wait_for_continue(self) -> None:
        """Wait for debugger to continue"""
        # In a real implementation, this would block until continue signal
        # For now, we'll use a simple state check
        import time
        while self.state == DebuggerState.PAUSED:
            time.sleep(0.1)
    
    def _get_context(self) -> Dict[str, Any]:
        """Get current execution context"""
        return {
            'node': self.current_node,
            'env': self.current_env,
            'stack': self.call_stack
        }
    
    def _bp_to_dict(self, bp: Breakpoint) -> Dict[str, Any]:
        """Convert breakpoint to dictionary"""
        return {
            'id': bp.id,
            'file': bp.file,
            'line': bp.line,
            'condition': bp.condition,
            'hitCount': bp.hit_count,
            'enabled': bp.enabled,
            'logMessage': bp.log_message
        }
    
    def _frame_to_dict(self, frame: StackFrame) -> Dict[str, Any]:
        """Convert stack frame to dictionary"""
        return {
            'id': frame.id,
            'name': frame.name,
            'file': frame.file,
            'line': frame.line,
            'column': frame.column
        }
    
    def _current_frame_to_dict(self) -> Dict[str, Any]:
        """Get current frame as dictionary"""
        if self.call_stack:
            return self._frame_to_dict(self.call_stack[-1])
        else:
            # Create synthetic frame for top level
            return {
                'id': 0,
                'name': '<main>',
                'file': self.current_node.source_location.get('file', '') if self.current_node and hasattr(self.current_node, 'source_location') else '',
                'line': self.current_node.source_location.get('line', 0) if self.current_node and hasattr(self.current_node, 'source_location') else 0,
                'column': 0
            }
    
    def _stack_to_dict(self) -> List[Dict[str, Any]]:
        """Convert call stack to dictionary list"""
        return [self._frame_to_dict(frame) for frame in reversed(self.call_stack)]
    
    def _env_to_dict(self, env: Environment) -> Dict[str, Any]:
        """Convert environment to dictionary of variables"""
        if hasattr(self, '_interpreter') and self._interpreter:
            from ..interpreter.debug_interpreter import DebugInterpreter
            if isinstance(self._interpreter, DebugInterpreter):
                return self._interpreter.get_all_variables(env)
        
        # Fallback: extract directly
        variables = {}
        if hasattr(env, 'bindings'):
            for name, value in env.bindings.items():
                if hasattr(value, 'data'):
                    variables[name] = value.data
                else:
                    variables[name] = value
        return variables
    
    def _format_log_message(self, message: str) -> str:
        """Format log message with variable substitution"""
        # TODO: Implement variable substitution in log messages
        return message