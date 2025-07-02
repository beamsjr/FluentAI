"""
ClaudeLang Effect Handlers

This module implements a comprehensive effect system with composable handlers.
Effects are first-class values that can be handled, transformed, and composed.
"""

from typing import Dict, Any, List, Optional, Callable, Union, Tuple
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
from contextlib import contextmanager
import sys
import time
import random
import json
from pathlib import Path

from ..core.ast import EffectType, TypeAnnotation


@dataclass
class EffectRequest:
    """A request to perform an effect"""
    effect_type: EffectType
    operation: str
    arguments: List[Any]
    metadata: Dict[str, Any] = field(default_factory=dict)
    continuation: Optional[Callable] = None


@dataclass
class EffectResult:
    """Result of handling an effect"""
    value: Any
    state_changes: Dict[str, Any] = field(default_factory=dict)
    secondary_effects: List[EffectRequest] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


class EffectHandler(ABC):
    """Abstract base class for effect handlers"""
    
    def __init__(self):
        self.parent_handler: Optional[EffectHandler] = None
        self.state: Dict[str, Any] = {}
    
    @abstractmethod
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        """Check if this handler can handle the given effect"""
        pass
    
    @abstractmethod
    def handle(self, request: EffectRequest) -> EffectResult:
        """Handle an effect request"""
        pass
    
    def compose(self, other: 'EffectHandler') -> 'ComposedHandler':
        """Compose this handler with another"""
        return ComposedHandler([self, other])
    
    def with_parent(self, parent: 'EffectHandler') -> 'EffectHandler':
        """Set parent handler for delegation"""
        self.parent_handler = parent
        return self


class ComposedHandler(EffectHandler):
    """Handler that composes multiple handlers"""
    
    def __init__(self, handlers: List[EffectHandler]):
        super().__init__()
        self.handlers = handlers
        # Link handlers in chain
        for i in range(len(handlers) - 1):
            handlers[i].parent_handler = handlers[i + 1]
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return any(h.can_handle(effect_type, operation) for h in self.handlers)
    
    def handle(self, request: EffectRequest) -> EffectResult:
        for handler in self.handlers:
            if handler.can_handle(request.effect_type, request.operation):
                return handler.handle(request)
        
        if self.parent_handler:
            return self.parent_handler.handle(request)
        
        raise ValueError(f"No handler for {request.effect_type}:{request.operation}")


class IOHandler(EffectHandler):
    """Handler for IO effects"""
    
    def __init__(self, stdin=None, stdout=None, stderr=None):
        super().__init__()
        self.stdin = stdin or sys.stdin
        self.stdout = stdout or sys.stdout
        self.stderr = stderr or sys.stderr
        self.open_files: Dict[str, Any] = {}
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return effect_type == EffectType.IO
    
    def handle(self, request: EffectRequest) -> EffectResult:
        op = request.operation
        args = request.arguments
        
        if op == "print":
            # Print to stdout
            output = " ".join(str(arg) for arg in args)
            print(output, file=self.stdout)
            return EffectResult(value=None)
        
        elif op == "print-err":
            # Print to stderr
            output = " ".join(str(arg) for arg in args)
            print(output, file=self.stderr)
            return EffectResult(value=None)
        
        elif op == "read-line":
            # Read line from stdin
            line = self.stdin.readline().rstrip('\n')
            return EffectResult(value=line)
        
        elif op == "open-file":
            # Open file
            path, mode = args[0], args[1] if len(args) > 1 else 'r'
            try:
                handle = open(path, mode)
                file_id = str(id(handle))
                self.open_files[file_id] = handle
                return EffectResult(value=file_id)
            except IOError as e:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "io-error", [str(e)])
                    ]
                )
        
        elif op == "close-file":
            # Close file
            file_id = args[0]
            if file_id in self.open_files:
                self.open_files[file_id].close()
                del self.open_files[file_id]
            return EffectResult(value=None)
        
        elif op == "read-file":
            # Read from file
            file_id = args[0]
            if file_id in self.open_files:
                content = self.open_files[file_id].read()
                return EffectResult(value=content)
            else:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "invalid-file", ["File not open"])
                    ]
                )
        
        elif op == "write-file":
            # Write to file
            file_id, content = args[0], args[1]
            if file_id in self.open_files:
                self.open_files[file_id].write(content)
                return EffectResult(value=None)
            else:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "invalid-file", ["File not open"])
                    ]
                )
        
        else:
            raise ValueError(f"Unknown IO operation: {op}")


class StateHandler(EffectHandler):
    """Handler for mutable state effects"""
    
    def __init__(self, initial_state: Optional[Dict[str, Any]] = None):
        super().__init__()
        self.state = initial_state or {}
        self.transaction_stack: List[Dict[str, Any]] = []
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return effect_type == EffectType.STATE
    
    def handle(self, request: EffectRequest) -> EffectResult:
        op = request.operation
        args = request.arguments
        
        if op == "get":
            # Get state value
            key = args[0]
            value = self.state.get(key)
            return EffectResult(value=value)
        
        elif op == "set":
            # Set state value
            key, value = args[0], args[1]
            old_value = self.state.get(key)
            self.state[key] = value
            return EffectResult(
                value=old_value,
                state_changes={key: (old_value, value)}
            )
        
        elif op == "update":
            # Update state with function
            key, update_fn = args[0], args[1]
            old_value = self.state.get(key)
            new_value = update_fn(old_value)
            self.state[key] = new_value
            return EffectResult(
                value=new_value,
                state_changes={key: (old_value, new_value)}
            )
        
        elif op == "delete":
            # Delete state value
            key = args[0]
            old_value = self.state.pop(key, None)
            return EffectResult(
                value=old_value,
                state_changes={key: (old_value, None)}
            )
        
        elif op == "begin-transaction":
            # Start transaction
            self.transaction_stack.append(self.state.copy())
            return EffectResult(value=None)
        
        elif op == "commit-transaction":
            # Commit transaction
            if self.transaction_stack:
                self.transaction_stack.pop()
            return EffectResult(value=None)
        
        elif op == "rollback-transaction":
            # Rollback transaction
            if self.transaction_stack:
                self.state = self.transaction_stack.pop()
            return EffectResult(value=None)
        
        elif op == "gc-collect":
            # Trigger garbage collection
            # Import here to avoid circular imports
            from ..vm.gc import gc_collect
            gc_collect()
            return EffectResult(value=None)
        
        elif op == "gc-stats":
            # Get garbage collection statistics
            from ..vm.gc import gc_stats
            stats = gc_stats()
            return EffectResult(value=stats)
        
        elif op == "gc-set-threshold":
            # Set garbage collection threshold
            threshold = args[0]
            # This would need to be implemented in the GC module
            # For now, just store it in state
            self.state['gc_threshold'] = threshold
            return EffectResult(value=None)
        
        elif op == "gc-enable":
            # Enable garbage collection
            self.state['gc_enabled'] = True
            return EffectResult(value=None)
        
        elif op == "gc-disable":
            # Disable garbage collection
            self.state['gc_enabled'] = False
            return EffectResult(value=None)
        
        elif op == "gc-is-enabled":
            # Check if garbage collection is enabled
            return EffectResult(value=self.state.get('gc_enabled', True))
        
        elif op == "gc-get-threshold":
            # Get garbage collection threshold
            return EffectResult(value=self.state.get('gc_threshold', 100))
        
        else:
            raise ValueError(f"Unknown state operation: {op}")


class ErrorHandler(EffectHandler):
    """Handler for error effects with recovery"""
    
    def __init__(self, error_handlers: Optional[Dict[str, Callable]] = None):
        super().__init__()
        self.error_handlers = error_handlers or {}
        self.error_stack: List[Dict[str, Any]] = []
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return effect_type == EffectType.ERROR
    
    def handle(self, request: EffectRequest) -> EffectResult:
        op = request.operation
        args = request.arguments
        
        if op == "raise":
            # Raise an error
            error_type = args[0] if args else "generic"
            error_msg = args[1] if len(args) > 1 else "Unknown error"
            error_data = args[2] if len(args) > 2 else None
            
            error = {
                "type": error_type,
                "message": error_msg,
                "data": error_data
            }
            
            # Check for error handler
            if error_type in self.error_handlers:
                handler_result = self.error_handlers[error_type](error)
                return EffectResult(value=handler_result)
            
            # Store in error stack
            self.error_stack.append(error)
            
            return EffectResult(
                value={"error": error},
                metadata={"unhandled": True}
            )
        
        elif op == "catch":
            # Set up error handler
            error_type, handler = args[0], args[1]
            self.error_handlers[error_type] = handler
            return EffectResult(value=None)
        
        elif op == "get-errors":
            # Get error stack
            return EffectResult(value=self.error_stack.copy())
        
        elif op == "clear-errors":
            # Clear error stack
            self.error_stack.clear()
            return EffectResult(value=None)
        
        else:
            raise ValueError(f"Unknown error operation: {op}")


class TimeHandler(EffectHandler):
    """Handler for time-based effects"""
    
    def __init__(self, mock_time: Optional[float] = None):
        super().__init__()
        self.mock_time = mock_time
        self.start_time = time.time()
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return effect_type == EffectType.TIME
    
    def handle(self, request: EffectRequest) -> EffectResult:
        op = request.operation
        args = request.arguments
        
        if op == "now":
            # Get current time
            current = self.mock_time if self.mock_time is not None else time.time()
            return EffectResult(value=current)
        
        elif op == "sleep":
            # Sleep for duration
            duration = args[0]
            if self.mock_time is None:
                time.sleep(duration)
            else:
                self.mock_time += duration
            return EffectResult(value=None)
        
        elif op == "elapsed":
            # Get elapsed time since start
            current = self.mock_time if self.mock_time is not None else time.time()
            elapsed = current - self.start_time
            return EffectResult(value=elapsed)
        
        elif op == "set-mock":
            # Set mock time for testing
            self.mock_time = args[0]
            return EffectResult(value=None)
        
        else:
            raise ValueError(f"Unknown time operation: {op}")


class RandomHandler(EffectHandler):
    """Handler for random number generation"""
    
    def __init__(self, seed: Optional[int] = None):
        super().__init__()
        self.rng = random.Random(seed)
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return effect_type == EffectType.RANDOM
    
    def handle(self, request: EffectRequest) -> EffectResult:
        op = request.operation
        args = request.arguments
        
        if op == "random":
            # Random float [0, 1)
            return EffectResult(value=self.rng.random())
        
        elif op == "randint":
            # Random integer in range
            a, b = args[0], args[1]
            return EffectResult(value=self.rng.randint(a, b))
        
        elif op == "choice":
            # Random choice from list
            choices = args[0]
            if not choices:
                return EffectResult(
                    value=None,
                    secondary_effects=[
                        EffectRequest(EffectType.ERROR, "raise", ["empty-choice", "Cannot choose from empty list"])
                    ]
                )
            return EffectResult(value=self.rng.choice(choices))
        
        elif op == "shuffle":
            # Shuffle list in-place
            lst = args[0].copy()  # Make a copy
            self.rng.shuffle(lst)
            return EffectResult(value=lst)
        
        elif op == "seed":
            # Set random seed
            seed = args[0]
            self.rng.seed(seed)
            return EffectResult(value=None)
        
        else:
            raise ValueError(f"Unknown random operation: {op}")


# Import the enhanced network handler
from .network_handler import NetworkHandler as EnhancedNetworkHandler

# Use a simple wrapper for backwards compatibility
class NetworkHandler(EffectHandler):
    """Handler for network effects - wraps the enhanced handler"""
    
    def __init__(self):
        super().__init__()
        self.enhanced_handler = EnhancedNetworkHandler(mock_mode=True)
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return self.enhanced_handler.can_handle(effect_type, operation)
    
    def handle(self, request: EffectRequest) -> EffectResult:
        # Map old operations to new ones for compatibility
        if request.operation == "fetch":
            # Map to http-get
            return self.enhanced_handler.handle(
                EffectRequest(request.effect_type, "http-get", request.arguments)
            )
        elif request.operation == "mock":
            # Map to mock-response
            url, response = request.arguments[0], request.arguments[1]
            return self.enhanced_handler.handle(
                EffectRequest(request.effect_type, "mock-response", 
                            ["GET", url, {"body": response, "status": 200}])
            )
        else:
            # Pass through to enhanced handler
            return self.enhanced_handler.handle(request)
    
    def cleanup(self):
        """Clean up resources"""
        if hasattr(self.enhanced_handler, 'cleanup'):
            self.enhanced_handler.cleanup()


class EffectContext:
    """Context for managing effect handlers"""
    
    def __init__(self, handler: EffectHandler):
        self.handler = handler
        self.effect_log: List[Tuple[EffectRequest, EffectResult]] = []
        self.suspended_effects: List[EffectRequest] = []
    
    def perform(self, effect_type: EffectType, operation: str, *arguments) -> Any:
        """Perform an effect"""
        request = EffectRequest(effect_type, operation, list(arguments))
        
        # Check if effect is suspended
        if self._is_suspended(effect_type):
            self.suspended_effects.append(request)
            return None
        
        # Handle the effect
        result = self.handler.handle(request)
        
        # Log the effect
        self.effect_log.append((request, result))
        
        # Handle secondary effects
        for secondary in result.secondary_effects:
            self.perform(secondary.effect_type, secondary.operation, *secondary.arguments)
        
        return result.value
    
    def _is_suspended(self, effect_type: EffectType) -> bool:
        """Check if an effect type is suspended"""
        # Could implement effect suspension for testing/debugging
        return False
    
    @contextmanager
    def transaction(self):
        """Run effects in a transaction"""
        # Begin transaction for stateful handlers
        self.perform(EffectType.STATE, "begin-transaction")
        
        try:
            yield self
            # Commit on success
            self.perform(EffectType.STATE, "commit-transaction")
        except Exception:
            # Rollback on failure
            self.perform(EffectType.STATE, "rollback-transaction")
            raise
    
    def get_log(self) -> List[Dict[str, Any]]:
        """Get effect log for debugging"""
        return [
            {
                "effect": req.effect_type.name,
                "operation": req.operation,
                "arguments": req.arguments,
                "result": result.value,
                "state_changes": result.state_changes
            }
            for req, result in self.effect_log
        ]


def create_default_handler() -> EffectHandler:
    """Create a default effect handler with all standard handlers"""
    from .async_handler import AsyncHandler
    return ComposedHandler([
        IOHandler(),
        StateHandler(),
        ErrorHandler(),
        TimeHandler(),
        RandomHandler(),
        NetworkHandler(),
        AsyncHandler()
    ])


def create_test_handler(**kwargs) -> EffectHandler:
    """Create a handler for testing with mocked components"""
    handlers = []
    
    if "io" in kwargs:
        handlers.append(IOHandler(**kwargs["io"]))
    else:
        handlers.append(IOHandler())
    
    if "state" in kwargs:
        handlers.append(StateHandler(kwargs["state"]))
    else:
        handlers.append(StateHandler())
    
    from .async_handler import AsyncHandler
    handlers.extend([
        ErrorHandler(),
        TimeHandler(mock_time=kwargs.get("mock_time")),
        RandomHandler(seed=kwargs.get("seed")),
        NetworkHandler(),
        AsyncHandler()
    ])
    
    return ComposedHandler(handlers)