"""
Execution Trace Analysis for Documentation Generation

This module analyzes runtime execution traces to automatically generate
documentation about function behavior, usage patterns, and data flow.
"""

from typing import Dict, List, Set, Tuple, Any, Optional
from dataclasses import dataclass, field
from collections import defaultdict, Counter
from enum import Enum, auto
import json
import time

from ..core.ast import Graph, ASTNode, NodeType, EffectType
from ..interpreter.interpreter import Value, Environment


class TraceEventType(Enum):
    """Types of events in execution traces"""
    FUNCTION_CALL = auto()
    FUNCTION_RETURN = auto()
    VARIABLE_BIND = auto()
    EFFECT_TRIGGERED = auto()
    EXCEPTION_RAISED = auto()
    BRANCH_TAKEN = auto()
    LOOP_ITERATION = auto()


@dataclass
class TraceEvent:
    """A single event in an execution trace"""
    event_type: TraceEventType
    timestamp: float
    node_id: str
    node_type: NodeType
    data: Dict[str, Any] = field(default_factory=dict)
    
    @property
    def duration(self) -> Optional[float]:
        """Duration if this is a timed event"""
        return self.data.get('duration')


@dataclass
class FunctionProfile:
    """Profile of a function's execution characteristics"""
    name: str
    call_count: int = 0
    total_time: float = 0.0
    min_time: float = float('inf')
    max_time: float = 0.0
    parameter_types: Dict[str, Counter] = field(default_factory=lambda: defaultdict(Counter))
    return_types: Counter = field(default_factory=Counter)
    effects_used: Set[EffectType] = field(default_factory=set)
    exceptions_raised: Counter = field(default_factory=Counter)
    call_sites: Set[str] = field(default_factory=set)
    example_inputs: List[Tuple] = field(default_factory=list)
    example_outputs: List[Any] = field(default_factory=list)
    
    @property
    def average_time(self) -> float:
        """Average execution time"""
        return self.total_time / self.call_count if self.call_count > 0 else 0.0
    
    @property
    def is_pure(self) -> bool:
        """Check if function appears to be pure"""
        return len(self.effects_used) == 0
    
    def add_call(self, duration: float, params: List[Tuple[str, Any]], 
                 return_value: Any, effects: Set[EffectType], call_site: str):
        """Record a function call"""
        self.call_count += 1
        self.total_time += duration
        self.min_time = min(self.min_time, duration)
        self.max_time = max(self.max_time, duration)
        
        # Record parameter types
        for param_name, param_value in params:
            param_type = type(param_value).__name__
            self.parameter_types[param_name][param_type] += 1
        
        # Record return type
        return_type = type(return_value).__name__
        self.return_types[return_type] += 1
        
        # Record effects
        self.effects_used.update(effects)
        
        # Record call site
        self.call_sites.add(call_site)
        
        # Store examples (limit to 5)
        if len(self.example_inputs) < 5:
            self.example_inputs.append(tuple(v for _, v in params))
            self.example_outputs.append(return_value)


@dataclass
class DataFlowEdge:
    """Represents data flow between functions"""
    source_function: str
    target_function: str
    data_type: str
    frequency: int = 1
    examples: List[Any] = field(default_factory=list)


@dataclass
class BranchProfile:
    """Profile of conditional branches"""
    condition_id: str
    true_count: int = 0
    false_count: int = 0
    
    @property
    def total_count(self) -> int:
        return self.true_count + self.false_count
    
    @property
    def true_ratio(self) -> float:
        return self.true_count / self.total_count if self.total_count > 0 else 0.0


class ExecutionTraceAnalyzer:
    """Analyzes execution traces to generate documentation"""
    
    def __init__(self):
        self.traces: List[TraceEvent] = []
        self.function_profiles: Dict[str, FunctionProfile] = {}
        self.data_flows: List[DataFlowEdge] = []
        self.branch_profiles: Dict[str, BranchProfile] = defaultdict(BranchProfile)
        self.call_graph: Dict[str, Set[str]] = defaultdict(set)
        self.current_call_stack: List[Tuple[str, float, List[Tuple[str, Any]]]] = []
    
    def record_event(self, event: TraceEvent):
        """Record a trace event"""
        self.traces.append(event)
        
        # Process event based on type
        if event.event_type == TraceEventType.FUNCTION_CALL:
            self._process_function_call(event)
        elif event.event_type == TraceEventType.FUNCTION_RETURN:
            self._process_function_return(event)
        elif event.event_type == TraceEventType.BRANCH_TAKEN:
            self._process_branch(event)
        elif event.event_type == TraceEventType.EFFECT_TRIGGERED:
            self._process_effect(event)
    
    def _process_function_call(self, event: TraceEvent):
        """Process function call event"""
        func_name = event.data.get('function_name', 'anonymous')
        params = event.data.get('parameters', [])
        call_site = event.data.get('call_site', 'unknown')
        
        # Initialize profile if needed
        if func_name not in self.function_profiles:
            self.function_profiles[func_name] = FunctionProfile(name=func_name)
        
        # Record call stack
        self.current_call_stack.append((func_name, event.timestamp, params))
        
        # Update call graph
        if len(self.current_call_stack) > 1:
            caller = self.current_call_stack[-2][0]
            self.call_graph[caller].add(func_name)
    
    def _process_function_return(self, event: TraceEvent):
        """Process function return event"""
        if not self.current_call_stack:
            return
        
        func_name, start_time, params = self.current_call_stack.pop()
        return_value = event.data.get('return_value')
        duration = event.timestamp - start_time
        effects = set(event.data.get('effects', []))
        call_site = event.data.get('call_site', 'unknown')
        
        # Update profile
        profile = self.function_profiles.get(func_name)
        if profile:
            profile.add_call(duration, params, return_value, effects, call_site)
        
        # Track data flow
        if self.current_call_stack and return_value is not None:
            caller = self.current_call_stack[-1][0]
            self._record_data_flow(func_name, caller, return_value)
    
    def _process_branch(self, event: TraceEvent):
        """Process branch taken event"""
        condition_id = event.node_id
        branch_taken = event.data.get('branch', True)
        
        profile = self.branch_profiles[condition_id]
        profile.condition_id = condition_id
        
        if branch_taken:
            profile.true_count += 1
        else:
            profile.false_count += 1
    
    def _process_effect(self, event: TraceEvent):
        """Process effect triggered event"""
        if self.current_call_stack:
            func_name = self.current_call_stack[-1][0]
            effect_type = event.data.get('effect_type')
            
            if func_name in self.function_profiles and effect_type:
                self.function_profiles[func_name].effects_used.add(effect_type)
    
    def _record_data_flow(self, source: str, target: str, data: Any):
        """Record data flow between functions"""
        data_type = type(data).__name__
        
        # Find existing edge
        for edge in self.data_flows:
            if (edge.source_function == source and 
                edge.target_function == target and
                edge.data_type == data_type):
                edge.frequency += 1
                if len(edge.examples) < 3:
                    edge.examples.append(data)
                return
        
        # Create new edge
        self.data_flows.append(DataFlowEdge(
            source_function=source,
            target_function=target,
            data_type=data_type,
            examples=[data]
        ))
    
    def analyze_patterns(self) -> Dict[str, Any]:
        """Analyze execution patterns"""
        patterns = {
            'hot_paths': self._find_hot_paths(),
            'bottlenecks': self._find_bottlenecks(),
            'error_patterns': self._find_error_patterns(),
            'usage_patterns': self._find_usage_patterns(),
            'optimization_opportunities': self._find_optimization_opportunities()
        }
        return patterns
    
    def _find_hot_paths(self) -> List[List[str]]:
        """Find frequently executed paths"""
        # Analyze call sequences
        paths = []
        path_counts = defaultdict(int)
        
        # Simple path extraction from traces
        current_path = []
        for event in self.traces:
            if event.event_type == TraceEventType.FUNCTION_CALL:
                current_path.append(event.data.get('function_name', 'anonymous'))
                if len(current_path) > 5:  # Limit path length
                    current_path.pop(0)
                path_counts[tuple(current_path)] += 1
        
        # Return top paths
        top_paths = sorted(path_counts.items(), key=lambda x: x[1], reverse=True)[:5]
        return [list(path) for path, _ in top_paths]
    
    def _find_bottlenecks(self) -> List[Dict[str, Any]]:
        """Find performance bottlenecks"""
        bottlenecks = []
        
        for name, profile in self.function_profiles.items():
            if profile.call_count > 0:
                # High total time
                if profile.total_time > 1.0:  # More than 1 second total
                    bottlenecks.append({
                        'function': name,
                        'reason': 'high_total_time',
                        'total_time': profile.total_time,
                        'call_count': profile.call_count,
                        'average_time': profile.average_time
                    })
                
                # High average time
                elif profile.average_time > 0.1:  # More than 100ms per call
                    bottlenecks.append({
                        'function': name,
                        'reason': 'high_average_time',
                        'average_time': profile.average_time,
                        'call_count': profile.call_count
                    })
                
                # Very frequent calls
                elif profile.call_count > 1000:
                    bottlenecks.append({
                        'function': name,
                        'reason': 'high_call_frequency',
                        'call_count': profile.call_count,
                        'total_time': profile.total_time
                    })
        
        return sorted(bottlenecks, key=lambda x: x.get('total_time', 0), reverse=True)
    
    def _find_error_patterns(self) -> List[Dict[str, Any]]:
        """Find common error patterns"""
        error_patterns = []
        
        # Analyze exceptions by function
        for name, profile in self.function_profiles.items():
            if profile.exceptions_raised:
                error_patterns.append({
                    'function': name,
                    'exceptions': dict(profile.exceptions_raised),
                    'error_rate': sum(profile.exceptions_raised.values()) / profile.call_count
                })
        
        return sorted(error_patterns, key=lambda x: x['error_rate'], reverse=True)
    
    def _find_usage_patterns(self) -> Dict[str, Any]:
        """Find common usage patterns"""
        patterns = {
            'common_parameters': {},
            'common_sequences': [],
            'type_patterns': {}
        }
        
        # Analyze parameter patterns
        for name, profile in self.function_profiles.items():
            param_patterns = {}
            for param_name, type_counts in profile.parameter_types.items():
                most_common = type_counts.most_common(1)
                if most_common:
                    param_patterns[param_name] = {
                        'common_type': most_common[0][0],
                        'frequency': most_common[0][1] / profile.call_count
                    }
            if param_patterns:
                patterns['common_parameters'][name] = param_patterns
        
        # Find common call sequences
        sequence_counts = defaultdict(int)
        for caller, callees in self.call_graph.items():
            for callee in callees:
                sequence_counts[(caller, callee)] += 1
        
        patterns['common_sequences'] = [
            {'caller': seq[0], 'callee': seq[1], 'count': count}
            for seq, count in sorted(sequence_counts.items(), 
                                    key=lambda x: x[1], reverse=True)[:10]
        ]
        
        return patterns
    
    def _find_optimization_opportunities(self) -> List[Dict[str, Any]]:
        """Find potential optimization opportunities"""
        opportunities = []
        
        # Pure functions called with same arguments
        for name, profile in self.function_profiles.items():
            if profile.is_pure and profile.call_count > 10:
                # Check for repeated inputs
                input_counts = Counter(profile.example_inputs)
                repeated = [inp for inp, count in input_counts.items() if count > 1]
                if repeated:
                    opportunities.append({
                        'type': 'memoization',
                        'function': name,
                        'reason': 'pure function with repeated inputs',
                        'repeated_inputs': len(repeated)
                    })
        
        # Unbalanced branches
        for condition_id, branch in self.branch_profiles.items():
            if branch.total_count > 100:
                ratio = branch.true_ratio
                if ratio > 0.95 or ratio < 0.05:
                    opportunities.append({
                        'type': 'branch_optimization',
                        'condition': condition_id,
                        'reason': 'heavily biased branch',
                        'true_ratio': ratio,
                        'total_evaluations': branch.total_count
                    })
        
        return opportunities
    
    def generate_documentation(self) -> str:
        """Generate documentation from trace analysis"""
        doc_lines = []
        doc_lines.append("# Execution Trace Analysis Report")
        doc_lines.append(f"\nGenerated at: {time.strftime('%Y-%m-%d %H:%M:%S')}")
        doc_lines.append(f"Total events analyzed: {len(self.traces)}")
        
        # Function profiles
        doc_lines.append("\n## Function Profiles\n")
        for name, profile in sorted(self.function_profiles.items(), 
                                   key=lambda x: x[1].call_count, reverse=True):
            doc_lines.append(f"### `{name}`")
            doc_lines.append(f"- **Calls**: {profile.call_count}")
            doc_lines.append(f"- **Average time**: {profile.average_time:.4f}s")
            doc_lines.append(f"- **Total time**: {profile.total_time:.4f}s")
            doc_lines.append(f"- **Pure**: {'Yes' if profile.is_pure else 'No'}")
            
            if profile.parameter_types:
                doc_lines.append("- **Parameter types**:")
                for param, types in profile.parameter_types.items():
                    common_type = types.most_common(1)[0]
                    doc_lines.append(f"  - `{param}`: {common_type[0]} ({common_type[1]}/{profile.call_count} calls)")
            
            if profile.return_types:
                common_return = profile.return_types.most_common(1)[0]
                doc_lines.append(f"- **Return type**: {common_return[0]} ({common_return[1]}/{profile.call_count} calls)")
            
            if profile.effects_used:
                doc_lines.append(f"- **Effects**: {', '.join(str(e) for e in profile.effects_used)}")
            
            if profile.example_inputs and profile.example_outputs:
                doc_lines.append("- **Examples**:")
                for inp, out in zip(profile.example_inputs[:3], profile.example_outputs[:3]):
                    doc_lines.append(f"  - `{name}{inp}` → `{out}`")
            
            doc_lines.append("")
        
        # Call graph
        doc_lines.append("\n## Call Graph\n")
        doc_lines.append("```mermaid")
        doc_lines.append("graph TD")
        for caller, callees in self.call_graph.items():
            for callee in callees:
                doc_lines.append(f"    {caller} --> {callee}")
        doc_lines.append("```")
        
        # Data flows
        if self.data_flows:
            doc_lines.append("\n## Data Flows\n")
            for flow in sorted(self.data_flows, key=lambda x: x.frequency, reverse=True)[:10]:
                doc_lines.append(f"- `{flow.source_function}` → `{flow.target_function}`: "
                               f"{flow.data_type} (×{flow.frequency})")
        
        # Performance analysis
        patterns = self.analyze_patterns()
        
        bottlenecks = patterns.get('bottlenecks', [])
        if bottlenecks:
            doc_lines.append("\n## Performance Bottlenecks\n")
            for bottleneck in bottlenecks[:5]:
                doc_lines.append(f"- **{bottleneck['function']}**: "
                               f"{bottleneck['reason'].replace('_', ' ').title()}")
                if 'total_time' in bottleneck:
                    doc_lines.append(f"  - Total time: {bottleneck['total_time']:.4f}s")
                if 'average_time' in bottleneck:
                    doc_lines.append(f"  - Average time: {bottleneck['average_time']:.4f}s")
                if 'call_count' in bottleneck:
                    doc_lines.append(f"  - Call count: {bottleneck['call_count']}")
        
        # Optimization opportunities
        opportunities = patterns.get('optimization_opportunities', [])
        if opportunities:
            doc_lines.append("\n## Optimization Opportunities\n")
            for opp in opportunities:
                doc_lines.append(f"- **{opp['type'].replace('_', ' ').title()}**: {opp.get('function', opp.get('condition'))}")
                doc_lines.append(f"  - Reason: {opp['reason']}")
        
        return "\n".join(doc_lines)
    
    def export_trace_data(self, filename: str):
        """Export trace data for external analysis"""
        data = {
            'metadata': {
                'total_events': len(self.traces),
                'functions_profiled': len(self.function_profiles),
                'export_time': time.time()
            },
            'function_profiles': {
                name: {
                    'call_count': profile.call_count,
                    'total_time': profile.total_time,
                    'average_time': profile.average_time,
                    'min_time': profile.min_time,
                    'max_time': profile.max_time,
                    'is_pure': profile.is_pure,
                    'parameter_types': {k: dict(v) for k, v in profile.parameter_types.items()},
                    'return_types': dict(profile.return_types),
                    'effects': list(str(e) for e in profile.effects_used)
                }
                for name, profile in self.function_profiles.items()
            },
            'call_graph': {k: list(v) for k, v in self.call_graph.items()},
            'branch_profiles': {
                cid: {
                    'true_count': prof.true_count,
                    'false_count': prof.false_count,
                    'true_ratio': prof.true_ratio
                }
                for cid, prof in self.branch_profiles.items()
            },
            'patterns': self.analyze_patterns()
        }
        
        with open(filename, 'w') as f:
            json.dump(data, f, indent=2, default=str)