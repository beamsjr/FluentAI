"""
Reactive State System for ClaudeLang UI

This module provides reactive primitives that automatically trigger
UI updates when state changes, enabling efficient and declarative UIs.
"""

from typing import Dict, Any, List, Optional, Callable, Set, Tuple
from dataclasses import dataclass, field
import weakref
from collections import defaultdict
import threading

from ..core.ast import EffectType
from .handlers import EffectHandler, EffectRequest, EffectResult


@dataclass
class Dependency:
    """Represents a dependency relationship"""
    source: str  # State key
    target: str  # Computed or effect ID
    transform: Optional[Callable] = None


@dataclass
class ComputedValue:
    """Represents a computed value that depends on state"""
    id: str
    compute_fn: Callable
    dependencies: Set[str] = field(default_factory=set)
    cached_value: Any = None
    is_dirty: bool = True


@dataclass
class Watcher:
    """Watches state changes and triggers effects"""
    id: str
    watch_fn: Callable
    dependencies: Set[str] = field(default_factory=set)
    immediate: bool = False
    deep: bool = False


class ReactiveHandler(EffectHandler):
    """Handler for reactive state operations"""
    
    def __init__(self):
        super().__init__()
        # Reactive state storage
        self.state: Dict[str, Any] = {}
        
        # Dependency tracking
        self.dependencies: Dict[str, Set[str]] = defaultdict(set)  # state -> dependents
        self.reverse_deps: Dict[str, Set[str]] = defaultdict(set)  # dependent -> states
        
        # Computed values
        self.computed: Dict[str, ComputedValue] = {}
        
        # Watchers
        self.watchers: Dict[str, Watcher] = {}
        
        # Current tracking context
        self.tracking_stack: List[str] = []
        
        # Update batching
        self.pending_updates: Set[str] = set()
        self.is_batching = False
        self.update_lock = threading.Lock()
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        # Handle both STATE and DOM effects for reactive operations
        return (effect_type == EffectType.STATE and operation.startswith("reactive:")) or \
               (effect_type == EffectType.DOM and operation.startswith("reactive:"))
    
    def handle(self, request: EffectRequest) -> EffectResult:
        op = request.operation
        args = request.arguments
        
        # Remove reactive: prefix
        if op.startswith("reactive:"):
            op = op[9:]
        
        if op == "ref":
            # Create reactive reference
            initial_value = args[0] if args else None
            ref_id = f"ref_{len(self.state)}"
            self.state[ref_id] = initial_value
            return EffectResult(value=ref_id)
        
        elif op == "get":
            # Get reactive value with dependency tracking
            ref_id = args[0]
            
            # Track dependency if we're in a tracking context
            if self.tracking_stack and ref_id in self.state:
                tracker_id = self.tracking_stack[-1]
                self.dependencies[ref_id].add(tracker_id)
                self.reverse_deps[tracker_id].add(ref_id)
            
            return EffectResult(value=self.state.get(ref_id))
        
        elif op == "set":
            # Set reactive value and trigger updates
            ref_id = args[0]
            new_value = args[1]
            
            old_value = self.state.get(ref_id)
            if old_value != new_value:
                self.state[ref_id] = new_value
                self._trigger_updates(ref_id)
            
            return EffectResult(value=old_value)
        
        elif op == "update":
            # Update reactive value with function
            ref_id = args[0]
            update_fn = args[1]
            
            old_value = self.state.get(ref_id)
            new_value = update_fn(old_value) if callable(update_fn) else update_fn
            
            if old_value != new_value:
                self.state[ref_id] = new_value
                self._trigger_updates(ref_id)
            
            return EffectResult(value=new_value)
        
        elif op == "computed":
            # Create computed value
            compute_fn = args[0]
            computed_id = f"computed_{len(self.computed)}"
            
            computed_val = ComputedValue(
                id=computed_id,
                compute_fn=compute_fn
            )
            self.computed[computed_id] = computed_val
            
            # Compute initial value with dependency tracking
            self._compute_value(computed_id)
            
            return EffectResult(value=computed_id)
        
        elif op == "get-computed":
            # Get computed value
            computed_id = args[0]
            
            if computed_id in self.computed:
                computed_val = self.computed[computed_id]
                
                # Recompute if dirty
                if computed_val.is_dirty:
                    self._compute_value(computed_id)
                
                # Track dependency
                if self.tracking_stack:
                    tracker_id = self.tracking_stack[-1]
                    self.dependencies[computed_id].add(tracker_id)
                    self.reverse_deps[tracker_id].add(computed_id)
                
                return EffectResult(value=computed_val.cached_value)
            
            return EffectResult(value=None)
        
        elif op == "watch":
            # Create watcher
            watch_fn = args[0]
            deps = args[1] if len(args) > 1 else []
            options = args[2] if len(args) > 2 else {}
            
            watcher_id = f"watcher_{len(self.watchers)}"
            watcher = Watcher(
                id=watcher_id,
                watch_fn=watch_fn,
                immediate=options.get('immediate', False),
                deep=options.get('deep', False)
            )
            
            # Track dependencies
            if deps:
                for dep in deps:
                    watcher.dependencies.add(dep)
                    self.dependencies[dep].add(watcher_id)
                    self.reverse_deps[watcher_id].add(dep)
            
            self.watchers[watcher_id] = watcher
            
            # Run immediately if requested
            if watcher.immediate:
                self._run_watcher(watcher_id)
            
            return EffectResult(value=watcher_id)
        
        elif op == "unwatch":
            # Remove watcher
            watcher_id = args[0]
            
            if watcher_id in self.watchers:
                watcher = self.watchers[watcher_id]
                
                # Clean up dependencies
                for dep in watcher.dependencies:
                    self.dependencies[dep].discard(watcher_id)
                for state_key in self.reverse_deps[watcher_id]:
                    self.dependencies[state_key].discard(watcher_id)
                del self.reverse_deps[watcher_id]
                
                del self.watchers[watcher_id]
            
            return EffectResult(value=None)
        
        elif op == "batch":
            # Batch updates
            update_fn = args[0]
            
            was_batching = self.is_batching
            self.is_batching = True
            
            try:
                result = update_fn() if callable(update_fn) else None
                self._flush_updates()
                return EffectResult(value=result)
            finally:
                self.is_batching = was_batching
        
        elif op == "track":
            # Start dependency tracking
            tracker_id = args[0]
            self.tracking_stack.append(tracker_id)
            return EffectResult(value=None)
        
        elif op == "untrack":
            # Stop dependency tracking
            if self.tracking_stack:
                self.tracking_stack.pop()
            return EffectResult(value=None)
        
        elif op == "cleanup":
            # Clean up all reactive state
            self.state.clear()
            self.dependencies.clear()
            self.reverse_deps.clear()
            self.computed.clear()
            self.watchers.clear()
            self.tracking_stack.clear()
            self.pending_updates.clear()
            return EffectResult(value=None)
        
        else:
            raise ValueError(f"Unknown reactive operation: {op}")
    
    def _trigger_updates(self, state_key: str):
        """Trigger updates for all dependents of a state key"""
        if self.is_batching:
            self.pending_updates.add(state_key)
        else:
            self._process_updates({state_key})
    
    def _process_updates(self, changed_keys: Set[str]):
        """Process updates for changed keys"""
        # Mark computed values as dirty
        for key in changed_keys:
            for dependent_id in self.dependencies.get(key, []):
                if dependent_id.startswith("computed_"):
                    if dependent_id in self.computed:
                        self.computed[dependent_id].is_dirty = True
                elif dependent_id.startswith("watcher_"):
                    if dependent_id in self.watchers:
                        self._run_watcher(dependent_id)
    
    def _flush_updates(self):
        """Flush all pending updates"""
        if self.pending_updates:
            updates = self.pending_updates.copy()
            self.pending_updates.clear()
            self._process_updates(updates)
    
    def _compute_value(self, computed_id: str):
        """Compute and cache a computed value"""
        if computed_id not in self.computed:
            return
        
        computed_val = self.computed[computed_id]
        
        # Clear old dependencies
        for dep in self.reverse_deps.get(computed_id, []):
            self.dependencies[dep].discard(computed_id)
        computed_val.dependencies.clear()
        
        # Track dependencies during computation
        self.tracking_stack.append(computed_id)
        try:
            computed_val.cached_value = computed_val.compute_fn()
            computed_val.is_dirty = False
        finally:
            self.tracking_stack.pop()
    
    def _run_watcher(self, watcher_id: str):
        """Run a watcher function"""
        if watcher_id in self.watchers:
            watcher = self.watchers[watcher_id]
            
            # Collect current values
            values = {}
            for dep in watcher.dependencies:
                if dep in self.state:
                    values[dep] = self.state[dep]
                elif dep in self.computed:
                    computed_val = self.computed[dep]
                    if computed_val.is_dirty:
                        self._compute_value(dep)
                    values[dep] = computed_val.cached_value
            
            # Run watcher
            watcher.watch_fn(values)
    
    def get_dependency_graph(self) -> Dict[str, List[str]]:
        """Get the dependency graph for debugging"""
        graph = {}
        for source, targets in self.dependencies.items():
            graph[source] = list(targets)
        return graph