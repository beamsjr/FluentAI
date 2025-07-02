"""
ClaudeLang Machine-Learnable Optimization Hints

This module implements a system for learning and applying optimization patterns
based on program behavior and performance characteristics.
"""

import json
from typing import Dict, List, Tuple, Optional, Any, Set
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict

# Try to import numpy
try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False
    # Basic list-based fallback
    class FakeNumpy:
        @staticmethod
        def array(lst):
            return lst
        @staticmethod
        def dot(a, b):
            return sum(x * y for x, y in zip(a, b))
        @staticmethod
        def tanh(x):
            import math
            return math.tanh(x)
    np = FakeNumpy()

from ..core.ast import Graph, ASTNode, NodeType
from ..optimizer.graph_optimizer import GraphOptimizer


class OptimizationHintType(Enum):
    """Types of optimization hints"""
    INLINE = "inline"                    # Inline this function
    UNROLL = "unroll"                    # Unroll this loop
    VECTORIZE = "vectorize"              # Vectorize this operation
    PARALLELIZE = "parallelize"          # Parallelize this computation
    MEMOIZE = "memoize"                  # Cache results
    SPECIALIZE = "specialize"            # Type-specialize this function
    FUSION = "fusion"                    # Fuse these operations
    PREFETCH = "prefetch"                # Prefetch this data
    LAYOUT = "layout"                    # Change data layout
    STREAMING = "streaming"              # Use streaming computation


@dataclass
class ProgramFeatures:
    """Features extracted from a program for ML"""
    
    # Structural features
    node_count: int
    depth: int
    branching_factor: float
    cycle_count: int
    
    # Operation features
    arithmetic_ops: int
    memory_ops: int
    control_flow_ops: int
    function_calls: int
    
    # Data flow features
    data_dependencies: int
    live_variables: int
    register_pressure: float
    
    # Pattern features
    has_recursion: bool
    has_loops: bool
    has_map_pattern: bool
    has_reduce_pattern: bool
    
    # Type features
    uses_integers: bool
    uses_floats: bool
    uses_lists: bool
    uses_higher_order: bool
    
    # Performance hints
    estimated_iterations: Optional[int] = None
    data_size_hint: Optional[int] = None
    hotness_score: float = 0.0
    
    def to_vector(self):
        """Convert features to ML feature vector"""
        return np.array([
            self.node_count,
            self.depth,
            self.branching_factor,
            self.cycle_count,
            self.arithmetic_ops,
            self.memory_ops,
            self.control_flow_ops,
            self.function_calls,
            self.data_dependencies,
            self.live_variables,
            self.register_pressure,
            float(self.has_recursion),
            float(self.has_loops),
            float(self.has_map_pattern),
            float(self.has_reduce_pattern),
            float(self.uses_integers),
            float(self.uses_floats),
            float(self.uses_lists),
            float(self.uses_higher_order),
            self.estimated_iterations or 0,
            self.data_size_hint or 0,
            self.hotness_score
        ])


@dataclass
class OptimizationHint:
    """An optimization hint for a code region"""
    hint_type: OptimizationHintType
    node_id: str
    confidence: float  # 0.0 to 1.0
    parameters: Dict[str, Any] = field(default_factory=dict)
    learned_from: List[str] = field(default_factory=list)  # Program IDs
    
    def apply_threshold(self) -> bool:
        """Should this hint be applied based on confidence?"""
        thresholds = {
            OptimizationHintType.INLINE: 0.7,
            OptimizationHintType.UNROLL: 0.8,
            OptimizationHintType.VECTORIZE: 0.85,
            OptimizationHintType.PARALLELIZE: 0.9,
            OptimizationHintType.MEMOIZE: 0.6,
            OptimizationHintType.SPECIALIZE: 0.75,
            OptimizationHintType.FUSION: 0.8,
            OptimizationHintType.PREFETCH: 0.7,
            OptimizationHintType.LAYOUT: 0.85,
            OptimizationHintType.STREAMING: 0.8
        }
        return self.confidence >= thresholds.get(self.hint_type, 0.8)


@dataclass
class PerformanceProfile:
    """Performance characteristics of a program"""
    execution_time: float
    memory_usage: int
    cache_misses: int
    branch_mispredicts: int
    optimizations_applied: List[str]
    speedup: float = 1.0


class OptimizationLearner:
    """Learns optimization patterns from program execution"""
    
    def __init__(self):
        self.feature_extractor = FeatureExtractor()
        self.hint_database: Dict[str, List[OptimizationHint]] = {}
        self.performance_history: Dict[str, PerformanceProfile] = {}
        self.pattern_models: Dict[OptimizationHintType, 'PatternModel'] = {}
        self._initialize_models()
    
    def _initialize_models(self):
        """Initialize pattern recognition models"""
        for hint_type in OptimizationHintType:
            self.pattern_models[hint_type] = PatternModel(hint_type)
    
    def extract_hints(self, graph: Graph, program_id: str) -> List[OptimizationHint]:
        """Extract optimization hints from a program"""
        features = self.feature_extractor.extract(graph)
        hints = []
        
        # Check each node for optimization opportunities
        for node_id, node in graph.nodes.items():
            node_features = self.feature_extractor.extract_node_features(
                graph, node_id
            )
            
            # Ask each model for predictions
            for hint_type, model in self.pattern_models.items():
                confidence = model.predict(features, node_features)
                
                if confidence > 0.5:  # Base threshold
                    hint = self._create_hint(
                        hint_type, node_id, confidence, 
                        features, node_features
                    )
                    hints.append(hint)
        
        # Store for learning
        self.hint_database[program_id] = hints
        return hints
    
    def _create_hint(self, hint_type: OptimizationHintType, node_id: str,
                    confidence: float, program_features: ProgramFeatures,
                    node_features: Dict[str, Any]) -> OptimizationHint:
        """Create an optimization hint with parameters"""
        parameters = {}
        
        if hint_type == OptimizationHintType.UNROLL:
            # Estimate unroll factor
            parameters["factor"] = self._estimate_unroll_factor(
                node_features.get("iteration_count", 0)
            )
        
        elif hint_type == OptimizationHintType.INLINE:
            # Check inline conditions
            parameters["always_inline"] = node_features.get("call_count", 0) > 10
            parameters["max_size"] = 50
        
        elif hint_type == OptimizationHintType.VECTORIZE:
            # Determine vector width
            parameters["vector_width"] = self._determine_vector_width(
                node_features.get("data_type", "unknown")
            )
        
        elif hint_type == OptimizationHintType.MEMOIZE:
            # Estimate cache size
            parameters["cache_size"] = self._estimate_cache_size(
                node_features.get("input_space_size", 0)
            )
        
        return OptimizationHint(
            hint_type=hint_type,
            node_id=node_id,
            confidence=confidence,
            parameters=parameters
        )
    
    def learn_from_execution(self, program_id: str, profile: PerformanceProfile):
        """Learn from execution results"""
        self.performance_history[program_id] = profile
        
        # Update models based on performance
        if program_id in self.hint_database:
            hints = self.hint_database[program_id]
            
            for hint in hints:
                if hint.hint_type.value in profile.optimizations_applied:
                    # This optimization was beneficial
                    self._positive_feedback(hint, profile.speedup)
                else:
                    # This optimization wasn't applied or wasn't beneficial
                    self._negative_feedback(hint)
    
    def _positive_feedback(self, hint: OptimizationHint, speedup: float):
        """Reinforce successful optimization patterns"""
        model = self.pattern_models[hint.hint_type]
        model.positive_examples.append({
            "hint": hint,
            "speedup": speedup,
            "weight": speedup  # Weight by effectiveness
        })
        model.update()
    
    def _negative_feedback(self, hint: OptimizationHint):
        """Learn from unsuccessful optimizations"""
        model = self.pattern_models[hint.hint_type]
        model.negative_examples.append({
            "hint": hint,
            "weight": 1.0
        })
        model.update()
    
    def _estimate_unroll_factor(self, iteration_count: int) -> int:
        """Estimate optimal unroll factor"""
        if iteration_count <= 0:
            return 1
        elif iteration_count <= 4:
            return iteration_count
        elif iteration_count <= 16:
            return 4
        else:
            return 8
    
    def _determine_vector_width(self, data_type: str) -> int:
        """Determine vector width based on data type"""
        return {
            "int": 8,    # 8 integers in AVX2
            "float": 8,  # 8 floats in AVX
            "double": 4, # 4 doubles in AVX
            "bool": 32   # 32 bools packed
        }.get(data_type, 4)
    
    def _estimate_cache_size(self, input_space: int) -> int:
        """Estimate appropriate cache size"""
        if input_space <= 100:
            return input_space
        elif input_space <= 1000:
            return 256
        else:
            return 1024
    
    def save_learned_patterns(self, path: str):
        """Save learned optimization patterns"""
        data = {
            "patterns": {},
            "performance_history": {}
        }
        
        for hint_type, model in self.pattern_models.items():
            data["patterns"][hint_type.value] = model.to_dict()
        
        for prog_id, profile in self.performance_history.items():
            data["performance_history"][prog_id] = {
                "execution_time": profile.execution_time,
                "speedup": profile.speedup,
                "optimizations": profile.optimizations_applied
            }
        
        with open(path, 'w') as f:
            json.dump(data, f, indent=2)
    
    def load_learned_patterns(self, path: str):
        """Load previously learned patterns"""
        with open(path, 'r') as f:
            data = json.load(f)
        
        for hint_type_str, model_data in data.get("patterns", {}).items():
            hint_type = OptimizationHintType(hint_type_str)
            self.pattern_models[hint_type].from_dict(model_data)


class FeatureExtractor:
    """Extracts features from programs for ML"""
    
    def extract(self, graph: Graph) -> ProgramFeatures:
        """Extract features from a program graph"""
        return ProgramFeatures(
            node_count=len(graph.nodes),
            depth=self._compute_depth(graph),
            branching_factor=self._compute_branching_factor(graph),
            cycle_count=self._count_cycles(graph),
            arithmetic_ops=self._count_ops(graph, {'+', '-', '*', '/', 'mod'}),
            memory_ops=self._count_ops(graph, {'load', 'store', 'alloc'}),
            control_flow_ops=self._count_nodes(graph, {NodeType.IF, NodeType.MATCH}),
            function_calls=self._count_nodes(graph, {NodeType.APPLICATION}),
            data_dependencies=self._count_dependencies(graph),
            live_variables=self._estimate_live_variables(graph),
            register_pressure=self._estimate_register_pressure(graph),
            has_recursion=self._has_recursion(graph),
            has_loops=self._has_loops(graph),
            has_map_pattern=self._has_pattern(graph, "map"),
            has_reduce_pattern=self._has_pattern(graph, "reduce"),
            uses_integers=self._uses_type(graph, "int"),
            uses_floats=self._uses_type(graph, "float"),
            uses_lists=self._uses_type(graph, "list"),
            uses_higher_order=self._uses_higher_order(graph)
        )
    
    def extract_node_features(self, graph: Graph, node_id: str) -> Dict[str, Any]:
        """Extract features for a specific node"""
        node = graph.nodes[node_id]
        features = {
            "node_type": node.node_type.value,
            "child_count": len(node.get_dependencies()),
            "depth": self._node_depth(graph, node_id)
        }
        
        # Type-specific features
        if node.node_type == NodeType.APPLICATION:
            features["call_count"] = self._estimate_call_count(graph, node_id)
        
        return features
    
    # Feature computation methods
    def _compute_depth(self, graph: Graph) -> int:
        """Compute maximum depth of the graph"""
        if not graph.root_id:
            return 0
        
        depths = {}
        
        def compute_depth(node_id: str) -> int:
            if node_id in depths:
                return depths[node_id]
            
            node = graph.nodes.get(node_id)
            if not node:
                return 0
            
            deps = node.get_dependencies()
            if not deps:
                depths[node_id] = 1
            else:
                depths[node_id] = 1 + max(compute_depth(d) for d in deps)
            
            return depths[node_id]
        
        return compute_depth(graph.root_id)
    
    def _compute_branching_factor(self, graph: Graph) -> float:
        """Compute average branching factor"""
        total_branches = 0
        branch_nodes = 0
        
        for node in graph.nodes.values():
            deps = node.get_dependencies()
            if len(deps) > 1:
                total_branches += len(deps)
                branch_nodes += 1
        
        return total_branches / max(branch_nodes, 1)
    
    def _count_cycles(self, graph: Graph) -> int:
        """Count cycles in the graph"""
        # Simplified - would use proper cycle detection
        return 0
    
    def _count_ops(self, graph: Graph, ops: Set[str]) -> int:
        """Count specific operations"""
        count = 0
        for node in graph.nodes.values():
            if hasattr(node, 'name') and node.name in ops:
                count += 1
        return count
    
    def _count_nodes(self, graph: Graph, types: Set[NodeType]) -> int:
        """Count nodes of specific types"""
        return sum(1 for n in graph.nodes.values() if n.node_type in types)
    
    def _count_dependencies(self, graph: Graph) -> int:
        """Count total dependencies"""
        return sum(len(n.get_dependencies()) for n in graph.nodes.values())
    
    def _estimate_live_variables(self, graph: Graph) -> int:
        """Estimate maximum live variables"""
        # Simplified - would do proper liveness analysis
        return len(graph.nodes) // 3
    
    def _estimate_register_pressure(self, graph: Graph) -> float:
        """Estimate register pressure"""
        return self._estimate_live_variables(graph) / 16.0  # Assume 16 registers
    
    def _has_recursion(self, graph: Graph) -> bool:
        """Check if graph has recursive calls"""
        # Would check for self-references
        return False
    
    def _has_loops(self, graph: Graph) -> bool:
        """Check if graph has loops"""
        # Would check for loop constructs or recursive patterns
        return False
    
    def _has_pattern(self, graph: Graph, pattern: str) -> bool:
        """Check if graph has a specific pattern"""
        for node in graph.nodes.values():
            if hasattr(node, 'name') and node.name == pattern:
                return True
        return False
    
    def _uses_type(self, graph: Graph, type_name: str) -> bool:
        """Check if graph uses a specific type"""
        for node in graph.nodes.values():
            # Check type annotations
            if node.type_annotation and type_name in str(node.type_annotation):
                return True
            # Check literal types
            if hasattr(node, 'literal_type'):
                if type_name.lower() == "int" and node.literal_type == "int":
                    return True
                elif type_name.lower() == "float" and node.literal_type == "float":
                    return True
                elif type_name.lower() == "string" and node.literal_type == "string":
                    return True
                elif type_name.lower() == "list" and node.literal_type == "list":
                    return True
        return False
    
    def _uses_higher_order(self, graph: Graph) -> bool:
        """Check if graph uses higher-order functions"""
        for node in graph.nodes.values():
            if node.node_type == NodeType.LAMBDA:
                return True
        return False
    
    def _node_depth(self, graph: Graph, node_id: str) -> int:
        """Compute depth of a specific node"""
        # Would compute distance from root
        return 1
    
    def _estimate_call_count(self, graph: Graph, node_id: str) -> int:
        """Estimate how many times a function is called"""
        # Would analyze control flow
        return 1


class PatternModel:
    """Simple pattern recognition model for optimization hints"""
    
    def __init__(self, hint_type: OptimizationHintType):
        self.hint_type = hint_type
        self.positive_examples: List[Dict[str, Any]] = []
        self.negative_examples: List[Dict[str, Any]] = []
        self.weights: Optional[np.ndarray] = None
    
    def predict(self, program_features: ProgramFeatures, 
                node_features: Dict[str, Any]) -> float:
        """Predict confidence for applying this optimization"""
        # Simple heuristic-based prediction
        confidence = 0.5  # Base confidence
        
        if self.hint_type == OptimizationHintType.INLINE:
            # Inline small, frequently called functions
            if node_features.get("call_count", 0) > 5:
                confidence += 0.2
            if node_features.get("child_count", 0) < 10:
                confidence += 0.2
        
        elif self.hint_type == OptimizationHintType.VECTORIZE:
            # Vectorize arithmetic-heavy code
            if program_features.arithmetic_ops > 10:
                confidence += 0.3
            if program_features.has_map_pattern:
                confidence += 0.2
        
        elif self.hint_type == OptimizationHintType.MEMOIZE:
            # Memoize pure recursive functions
            if program_features.has_recursion:
                confidence += 0.4
        
        # Adjust based on learned examples
        if self.weights is not None:
            features = program_features.to_vector()
            adjustment = np.dot(self.weights, features)
            confidence += np.tanh(adjustment) * 0.3
        
        return min(max(confidence, 0.0), 1.0)
    
    def update(self):
        """Update model based on examples"""
        # Simple weight update - would use proper ML in practice
        if len(self.positive_examples) > 10:
            # Extract features and compute weights
            pass
    
    def to_dict(self) -> Dict[str, Any]:
        """Serialize model"""
        return {
            "hint_type": self.hint_type.value,
            "positive_count": len(self.positive_examples),
            "negative_count": len(self.negative_examples),
            "weights": self.weights.tolist() if self.weights is not None else None
        }
    
    def from_dict(self, data: Dict[str, Any]):
        """Deserialize model"""
        if data.get("weights"):
            self.weights = np.array(data["weights"])