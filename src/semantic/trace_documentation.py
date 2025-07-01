"""
ClaudeLang Execution Trace Documentation Generator

This module generates comprehensive documentation from program execution traces,
providing insights into runtime behavior and performance characteristics.
"""

import json
import time
from typing import Dict, List, Optional, Any, Tuple, Set
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict, Counter

from ..core.ast import Graph, ASTNode, NodeType, EffectType
from ..interpreter.interpreter import Value


class TraceEventType(Enum):
    """Types of trace events"""
    ENTER_FUNCTION = "enter_function"
    EXIT_FUNCTION = "exit_function"
    EVALUATE_NODE = "evaluate_node"
    BIND_VARIABLE = "bind_variable"
    TRIGGER_EFFECT = "trigger_effect"
    BRANCH_TAKEN = "branch_taken"
    PATTERN_MATCH = "pattern_match"
    EXCEPTION = "exception"
    OPTIMIZATION = "optimization"


@dataclass
class TraceEvent:
    """A single event in an execution trace"""
    timestamp: float
    event_type: TraceEventType
    node_id: Optional[str]
    data: Dict[str, Any]
    stack_depth: int
    memory_usage: Optional[int] = None
    
    def duration_to(self, other: 'TraceEvent') -> float:
        """Calculate duration to another event"""
        return other.timestamp - self.timestamp


@dataclass
class ExecutionTrace:
    """Complete execution trace of a program"""
    program_id: str
    start_time: float
    end_time: float
    events: List[TraceEvent]
    input_values: Dict[str, Any]
    output_value: Any
    graph: Optional[Graph] = None
    
    def total_duration(self) -> float:
        """Total execution duration"""
        return self.end_time - self.start_time
    
    def event_count(self) -> Dict[TraceEventType, int]:
        """Count events by type"""
        counts = Counter(event.event_type for event in self.events)
        return dict(counts)


@dataclass
class FunctionProfile:
    """Performance profile of a function"""
    name: str
    call_count: int = 0
    total_time: float = 0.0
    self_time: float = 0.0
    min_time: float = float('inf')
    max_time: float = 0.0
    avg_time: float = 0.0
    children: Dict[str, float] = field(default_factory=dict)
    input_patterns: List[Tuple[Any, float]] = field(default_factory=list)
    effect_counts: Dict[EffectType, int] = field(default_factory=lambda: defaultdict(int))


@dataclass
class BehaviorPattern:
    """Observed behavior pattern"""
    pattern_type: str
    frequency: int
    examples: List[Dict[str, Any]]
    performance_impact: float
    description: str


class TraceAnalyzer:
    """Analyzes execution traces to extract insights"""
    
    def __init__(self):
        self.traces: List[ExecutionTrace] = []
        self.function_profiles: Dict[str, FunctionProfile] = {}
        self.behavior_patterns: List[BehaviorPattern] = []
        self.optimization_opportunities: List[Dict[str, Any]] = []
    
    def add_trace(self, trace: ExecutionTrace):
        """Add a trace for analysis"""
        self.traces.append(trace)
        self._update_profiles(trace)
        self._extract_patterns(trace)
    
    def _update_profiles(self, trace: ExecutionTrace):
        """Update function profiles from trace"""
        call_stack: List[Tuple[str, float]] = []
        
        for event in trace.events:
            if event.event_type == TraceEventType.ENTER_FUNCTION:
                func_name = event.data.get("function_name", "unknown")
                call_stack.append((func_name, event.timestamp))
                
                if func_name not in self.function_profiles:
                    self.function_profiles[func_name] = FunctionProfile(func_name)
                
                self.function_profiles[func_name].call_count += 1
            
            elif event.event_type == TraceEventType.EXIT_FUNCTION:
                if call_stack:
                    func_name, enter_time = call_stack.pop()
                    duration = event.timestamp - enter_time
                    
                    profile = self.function_profiles[func_name]
                    profile.total_time += duration
                    profile.min_time = min(profile.min_time, duration)
                    profile.max_time = max(profile.max_time, duration)
                    
                    # Track input patterns
                    if "return_value" in event.data:
                        profile.input_patterns.append(
                            (event.data.get("args", ()), duration)
                        )
            
            elif event.event_type == TraceEventType.TRIGGER_EFFECT:
                if call_stack:
                    func_name = call_stack[-1][0]
                    effect_type = event.data.get("effect_type")
                    if effect_type:
                        self.function_profiles[func_name].effect_counts[effect_type] += 1
    
    def _extract_patterns(self, trace: ExecutionTrace):
        """Extract behavior patterns from trace"""
        # Look for common patterns
        self._find_recursion_patterns(trace)
        self._find_loop_patterns(trace)
        self._find_branch_patterns(trace)
        self._find_effect_patterns(trace)
    
    def _find_recursion_patterns(self, trace: ExecutionTrace):
        """Find recursive call patterns"""
        call_stack = []
        recursion_depths = defaultdict(list)
        
        for event in trace.events:
            if event.event_type == TraceEventType.ENTER_FUNCTION:
                func_name = event.data.get("function_name", "unknown")
                
                # Check if already in call stack (recursion)
                depth = call_stack.count(func_name)
                if depth > 0:
                    recursion_depths[func_name].append(depth + 1)
                
                call_stack.append(func_name)
            
            elif event.event_type == TraceEventType.EXIT_FUNCTION:
                if call_stack:
                    call_stack.pop()
        
        # Create patterns for recursive functions
        for func_name, depths in recursion_depths.items():
            if depths:
                pattern = BehaviorPattern(
                    pattern_type="recursion",
                    frequency=len(depths),
                    examples=[{"function": func_name, "max_depth": max(depths)}],
                    performance_impact=max(depths) * 0.1,  # Estimate
                    description=f"{func_name} recurses up to {max(depths)} levels"
                )
                self.behavior_patterns.append(pattern)
    
    def _find_loop_patterns(self, trace: ExecutionTrace):
        """Find loop iteration patterns"""
        # Track repeated node evaluations
        node_visits = defaultdict(list)
        
        for i, event in enumerate(trace.events):
            if event.event_type == TraceEventType.EVALUATE_NODE:
                node_id = event.node_id
                if node_id:
                    node_visits[node_id].append(i)
        
        # Find nodes visited multiple times in sequence
        for node_id, visits in node_visits.items():
            if len(visits) > 10:  # Likely a loop
                pattern = BehaviorPattern(
                    pattern_type="loop",
                    frequency=len(visits),
                    examples=[{"node_id": node_id, "iterations": len(visits)}],
                    performance_impact=len(visits) * 0.01,
                    description=f"Node {node_id} evaluated {len(visits)} times"
                )
                self.behavior_patterns.append(pattern)
    
    def _find_branch_patterns(self, trace: ExecutionTrace):
        """Find branching patterns"""
        branch_counts = defaultdict(lambda: {"true": 0, "false": 0})
        
        for event in trace.events:
            if event.event_type == TraceEventType.BRANCH_TAKEN:
                condition_id = event.data.get("condition_id")
                branch_taken = event.data.get("branch", "true")
                
                if condition_id:
                    branch_counts[condition_id][branch_taken] += 1
        
        # Find biased branches
        for condition_id, counts in branch_counts.items():
            total = counts["true"] + counts["false"]
            if total > 0:
                true_ratio = counts["true"] / total
                
                if true_ratio > 0.9 or true_ratio < 0.1:
                    pattern = BehaviorPattern(
                        pattern_type="biased_branch",
                        frequency=total,
                        examples=[{
                            "condition_id": condition_id,
                            "true_ratio": true_ratio
                        }],
                        performance_impact=0.05,  # Branch prediction benefit
                        description=f"Branch {condition_id} is {true_ratio:.0%} true"
                    )
                    self.behavior_patterns.append(pattern)
    
    def _find_effect_patterns(self, trace: ExecutionTrace):
        """Find effect usage patterns"""
        effect_sequences = []
        current_sequence = []
        
        for event in trace.events:
            if event.event_type == TraceEventType.TRIGGER_EFFECT:
                effect_type = event.data.get("effect_type")
                if effect_type:
                    current_sequence.append(effect_type)
            elif event.event_type == TraceEventType.EXIT_FUNCTION:
                if len(current_sequence) > 1:
                    effect_sequences.append(tuple(current_sequence))
                current_sequence = []
        
        # Find common effect sequences
        sequence_counts = Counter(effect_sequences)
        
        for sequence, count in sequence_counts.most_common(5):
            if count > 1:
                pattern = BehaviorPattern(
                    pattern_type="effect_sequence",
                    frequency=count,
                    examples=[{"sequence": [e.name for e in sequence]}],
                    performance_impact=len(sequence) * 0.02,
                    description=f"Effect sequence {[e.name for e in sequence]}"
                )
                self.behavior_patterns.append(pattern)
    
    def generate_documentation(self) -> str:
        """Generate comprehensive documentation from traces"""
        doc = ["# Execution Trace Documentation", ""]
        
        # Overview
        doc.append("## Overview")
        doc.append(f"Analyzed {len(self.traces)} execution traces")
        doc.append("")
        
        # Performance Summary
        doc.append("## Performance Summary")
        doc.append("")
        doc.append("### Function Profiles")
        doc.append("")
        
        # Sort functions by total time
        sorted_profiles = sorted(
            self.function_profiles.values(),
            key=lambda p: p.total_time,
            reverse=True
        )
        
        doc.append("| Function | Calls | Total Time | Avg Time | Effects |")
        doc.append("|----------|-------|------------|----------|---------|")
        
        for profile in sorted_profiles[:20]:  # Top 20
            avg_time = profile.total_time / max(profile.call_count, 1)
            effects = ", ".join(f"{e.name}({c})" 
                              for e, c in profile.effect_counts.items())
            
            doc.append(f"| {profile.name} | {profile.call_count} | "
                      f"{profile.total_time:.3f}s | {avg_time:.3f}s | {effects} |")
        
        doc.append("")
        
        # Behavior Patterns
        doc.append("## Behavior Patterns")
        doc.append("")
        
        pattern_groups = defaultdict(list)
        for pattern in self.behavior_patterns:
            pattern_groups[pattern.pattern_type].append(pattern)
        
        for pattern_type, patterns in pattern_groups.items():
            doc.append(f"### {pattern_type.title()} Patterns")
            doc.append("")
            
            for pattern in patterns[:5]:  # Top 5 per type
                doc.append(f"- {pattern.description}")
                doc.append(f"  - Frequency: {pattern.frequency}")
                doc.append(f"  - Performance Impact: {pattern.performance_impact:.2%}")
            
            doc.append("")
        
        # Call Graphs
        doc.append("## Call Relationships")
        doc.append("")
        doc.append("```mermaid")
        doc.append("graph TD")
        
        # Generate call graph
        call_edges = set()
        for trace in self.traces:
            call_stack = []
            
            for event in trace.events:
                if event.event_type == TraceEventType.ENTER_FUNCTION:
                    func_name = event.data.get("function_name", "unknown")
                    
                    if call_stack:
                        caller = call_stack[-1]
                        call_edges.add((caller, func_name))
                    
                    call_stack.append(func_name)
                
                elif event.event_type == TraceEventType.EXIT_FUNCTION:
                    if call_stack:
                        call_stack.pop()
        
        for caller, callee in sorted(call_edges):
            doc.append(f"    {caller} --> {callee}")
        
        doc.append("```")
        doc.append("")
        
        # Optimization Opportunities
        doc.append("## Optimization Opportunities")
        doc.append("")
        
        opportunities = self._identify_optimizations()
        for opp in opportunities:
            doc.append(f"### {opp['type']}")
            doc.append(f"- Target: {opp['target']}")
            doc.append(f"- Potential Speedup: {opp['speedup']:.1f}x")
            doc.append(f"- Rationale: {opp['rationale']}")
            doc.append("")
        
        # Input/Output Examples
        doc.append("## Input/Output Examples")
        doc.append("")
        
        for i, trace in enumerate(self.traces[:5]):  # First 5 traces
            doc.append(f"### Trace {i+1}")
            doc.append(f"- Input: `{trace.input_values}`")
            doc.append(f"- Output: `{trace.output_value}`")
            doc.append(f"- Duration: {trace.total_duration():.3f}s")
            doc.append("")
        
        return "\n".join(doc)
    
    def _identify_optimizations(self) -> List[Dict[str, Any]]:
        """Identify optimization opportunities from traces"""
        opportunities = []
        
        # Look for functions that could be memoized
        for name, profile in self.function_profiles.items():
            if (profile.call_count > 10 and 
                len(profile.effect_counts) == 0 and  # Pure function
                profile.avg_time > 0.001):  # Non-trivial computation
                
                # Check for repeated inputs
                input_counts = Counter(args for args, _ in profile.input_patterns)
                repeat_ratio = len(input_counts) / max(len(profile.input_patterns), 1)
                
                if repeat_ratio < 0.5:  # Many repeated inputs
                    opportunities.append({
                        "type": "Memoization",
                        "target": name,
                        "speedup": 1.0 / repeat_ratio,
                        "rationale": f"Function called {profile.call_count} times "
                                   f"with {len(input_counts)} unique inputs"
                    })
        
        # Look for recursive functions that could use dynamic programming
        for pattern in self.behavior_patterns:
            if pattern.pattern_type == "recursion" and pattern.frequency > 10:
                opportunities.append({
                    "type": "Dynamic Programming",
                    "target": pattern.examples[0]["function"],
                    "speedup": pattern.examples[0]["max_depth"] / 2.0,
                    "rationale": f"Recursive function with max depth "
                               f"{pattern.examples[0]['max_depth']}"
                })
        
        # Look for biased branches
        for pattern in self.behavior_patterns:
            if pattern.pattern_type == "biased_branch":
                opportunities.append({
                    "type": "Branch Elimination",
                    "target": pattern.examples[0]["condition_id"],
                    "speedup": 1.1,
                    "rationale": f"Branch is {pattern.examples[0]['true_ratio']:.0%} "
                               f"predictable"
                })
        
        return opportunities
    
    def export_traces(self, filename: str):
        """Export traces to JSON format"""
        data = {
            "traces": [],
            "profiles": {},
            "patterns": []
        }
        
        # Export traces
        for trace in self.traces:
            trace_data = {
                "program_id": trace.program_id,
                "duration": trace.total_duration(),
                "event_count": len(trace.events),
                "input": trace.input_values,
                "output": trace.output_value
            }
            data["traces"].append(trace_data)
        
        # Export profiles
        for name, profile in self.function_profiles.items():
            data["profiles"][name] = {
                "calls": profile.call_count,
                "total_time": profile.total_time,
                "avg_time": profile.total_time / max(profile.call_count, 1)
            }
        
        # Export patterns
        for pattern in self.behavior_patterns:
            data["patterns"].append({
                "type": pattern.pattern_type,
                "frequency": pattern.frequency,
                "description": pattern.description
            })
        
        with open(filename, 'w') as f:
            json.dump(data, f, indent=2)


class TracingInterpreter:
    """Interpreter that generates execution traces"""
    
    def __init__(self, base_interpreter):
        self.base_interpreter = base_interpreter
        self.current_trace: Optional[ExecutionTrace] = None
        self.event_stack: List[TraceEvent] = []
    
    def interpret_with_trace(self, graph: Graph, inputs: Dict[str, Any]) -> Tuple[Any, ExecutionTrace]:
        """Interpret program and generate trace"""
        self.current_trace = ExecutionTrace(
            program_id=graph.metadata.get("name", "unknown"),
            start_time=time.time(),
            end_time=0,
            events=[],
            input_values=inputs,
            output_value=None,
            graph=graph
        )
        
        # Hook into interpreter events
        self._install_hooks()
        
        try:
            # Execute program
            result = self.base_interpreter.interpret(graph)
            self.current_trace.output_value = result
        finally:
            self.current_trace.end_time = time.time()
            self._uninstall_hooks()
        
        return result, self.current_trace
    
    def _install_hooks(self):
        """Install tracing hooks in interpreter"""
        # Would hook into interpreter methods
        pass
    
    def _uninstall_hooks(self):
        """Remove tracing hooks"""
        pass
    
    def _record_event(self, event_type: TraceEventType, node_id: Optional[str] = None,
                     data: Optional[Dict[str, Any]] = None):
        """Record a trace event"""
        if self.current_trace:
            event = TraceEvent(
                timestamp=time.time(),
                event_type=event_type,
                node_id=node_id,
                data=data or {},
                stack_depth=len(self.event_stack)
            )
            self.current_trace.events.append(event)