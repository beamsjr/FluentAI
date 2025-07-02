"""
Execution Trace for Performance Analysis

This module provides execution tracing capabilities for analyzing
program performance and collecting data for ML optimization.
"""

from typing import Dict, List, Optional, Any, Set
from dataclasses import dataclass, field
import time


@dataclass
class NodeTrace:
    """Trace information for a single node"""
    node_id: str
    execution_count: int = 0
    total_time: float = 0.0
    min_time: float = float('inf')
    max_time: float = 0.0
    memory_allocated: int = 0
    cache_misses: int = 0
    
    @property
    def average_time(self) -> float:
        """Average execution time"""
        return self.total_time / max(self.execution_count, 1)
    
    def is_hot_spot(self, threshold: float = 0.1) -> bool:
        """Check if this node is a performance hot spot"""
        return self.total_time > threshold
    
    def record_execution(self, duration: float, memory: int = 0, cache_misses: int = 0):
        """Record a single execution"""
        self.execution_count += 1
        self.total_time += duration
        self.min_time = min(self.min_time, duration)
        self.max_time = max(self.max_time, duration)
        self.memory_allocated += memory
        self.cache_misses += cache_misses


@dataclass
class FunctionTrace:
    """Trace information for a function"""
    function_name: str
    call_count: int = 0
    total_time: float = 0.0
    self_time: float = 0.0  # Time excluding sub-calls
    max_recursion_depth: int = 0
    arg_patterns: List[Dict[str, Any]] = field(default_factory=list)


@dataclass
class ExecutionTrace:
    """Complete execution trace for a program"""
    program_id: str
    start_time: float = field(default_factory=time.time)
    end_time: Optional[float] = None
    node_traces: Dict[str, NodeTrace] = field(default_factory=dict)
    function_traces: Dict[str, FunctionTrace] = field(default_factory=dict)
    peak_memory: int = 0
    total_allocations: int = 0
    total_cache_misses: int = 0
    hot_paths: List[List[str]] = field(default_factory=list)  # Sequences of hot nodes
    
    def get_node_trace(self, node_id: str) -> Optional[NodeTrace]:
        """Get trace for a specific node"""
        return self.node_traces.get(node_id)
    
    def record_node_execution(self, node_id: str, duration: float, 
                            memory: int = 0, cache_misses: int = 0):
        """Record execution of a node"""
        if node_id not in self.node_traces:
            self.node_traces[node_id] = NodeTrace(node_id)
        
        self.node_traces[node_id].record_execution(duration, memory, cache_misses)
        
        # Update global stats
        self.total_allocations += memory
        self.peak_memory = max(self.peak_memory, memory)
        self.total_cache_misses += cache_misses
    
    def record_function_call(self, function_name: str, duration: float, 
                           self_time: float, recursion_depth: int = 0):
        """Record a function call"""
        if function_name not in self.function_traces:
            self.function_traces[function_name] = FunctionTrace(function_name)
        
        trace = self.function_traces[function_name]
        trace.call_count += 1
        trace.total_time += duration
        trace.self_time += self_time
        trace.max_recursion_depth = max(trace.max_recursion_depth, recursion_depth)
    
    def identify_hot_paths(self, threshold: float = 0.1) -> List[List[str]]:
        """Identify sequences of nodes that are performance hot spots"""
        hot_nodes = [
            node_id for node_id, trace in self.node_traces.items()
            if trace.is_hot_spot(threshold)
        ]
        
        # Simple hot path detection - would be more sophisticated in practice
        if len(hot_nodes) > 1:
            self.hot_paths = [hot_nodes]
        
        return self.hot_paths
    
    def get_optimization_candidates(self) -> Dict[str, Dict[str, Any]]:
        """Get nodes that are good candidates for optimization"""
        candidates = {}
        
        for node_id, trace in self.node_traces.items():
            if trace.execution_count > 10 or trace.is_hot_spot():
                candidates[node_id] = {
                    "execution_count": trace.execution_count,
                    "total_time": trace.total_time,
                    "average_time": trace.average_time,
                    "is_hot": trace.is_hot_spot(),
                    "optimization_priority": trace.total_time * trace.execution_count
                }
        
        return candidates
    
    def finalize(self):
        """Finalize the trace"""
        self.end_time = time.time()
        self.identify_hot_paths()
    
    @property
    def total_duration(self) -> float:
        """Total execution duration"""
        if self.end_time:
            return self.end_time - self.start_time
        return time.time() - self.start_time


class ExecutionTracer:
    """Tracer for collecting execution traces"""
    
    def __init__(self):
        self.current_trace: Optional[ExecutionTrace] = None
        self.node_stack: List[Tuple[str, float]] = []  # (node_id, start_time)
        self.function_stack: List[Tuple[str, float]] = []  # (func_name, start_time)
        
    def start_trace(self, program_id: str = "unknown"):
        """Start a new trace"""
        self.current_trace = ExecutionTrace(program_id)
        self.node_stack.clear()
        self.function_stack.clear()
        
    def end_trace(self) -> Optional[ExecutionTrace]:
        """End current trace and return it"""
        if self.current_trace:
            self.current_trace.finalize()
            trace = self.current_trace
            self.current_trace = None
            return trace
        return None
    
    def enter_node(self, node_id: str):
        """Record entering a node"""
        if self.current_trace:
            self.node_stack.append((node_id, time.time()))
    
    def exit_node(self, node_id: str, memory_used: int = 0, cache_misses: int = 0):
        """Record exiting a node"""
        if self.current_trace and self.node_stack:
            # Find matching entry (should be last, but be safe)
            for i in range(len(self.node_stack) - 1, -1, -1):
                if self.node_stack[i][0] == node_id:
                    _, start_time = self.node_stack.pop(i)
                    duration = time.time() - start_time
                    self.current_trace.record_node_execution(
                        node_id, duration, memory_used, cache_misses
                    )
                    break
    
    def enter_function(self, function_name: str):
        """Record entering a function"""
        if self.current_trace:
            self.function_stack.append((function_name, time.time()))
    
    def exit_function(self, function_name: str):
        """Record exiting a function"""
        if self.current_trace and self.function_stack:
            for i in range(len(self.function_stack) - 1, -1, -1):
                if self.function_stack[i][0] == function_name:
                    _, start_time = self.function_stack.pop(i)
                    duration = time.time() - start_time
                    
                    # Calculate self time (approximate)
                    self_time = duration
                    if i < len(self.function_stack) - 1:
                        # Subtract time spent in nested calls
                        self_time *= 0.8  # Simple approximation
                    
                    self.current_trace.record_function_call(
                        function_name, duration, self_time, 
                        recursion_depth=len(self.function_stack)
                    )
                    break
    
    def record_memory_allocation(self, size: int):
        """Record memory allocation"""
        if self.current_trace:
            self.current_trace.total_allocations += size
            self.current_trace.peak_memory = max(
                self.current_trace.peak_memory,
                self.current_trace.total_allocations
            )
    
    def record_cache_miss(self):
        """Record a cache miss"""
        if self.current_trace:
            self.current_trace.total_cache_misses += 1


def create_tracer() -> ExecutionTracer:
    """Create a new execution tracer"""
    return ExecutionTracer()