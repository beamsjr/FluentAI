"""
ClaudeLang Semantic Versioning System

This module implements behavior-based versioning where version numbers
are derived from the semantic behavior of code, not its syntax.
"""

import hashlib
import json
from typing import Dict, List, Set, Tuple, Any, Optional
from dataclasses import dataclass, field
from enum import Enum

from ..core.ast import Graph, ASTNode, EffectType
from ..types.type_system import TypeAnnotation


class BehaviorChange(Enum):
    """Types of behavioral changes"""
    NONE = "none"                    # No change
    COMPATIBLE = "compatible"        # Backward compatible
    INCOMPATIBLE = "incompatible"    # Breaking change
    EFFECT = "effect"               # Effect signature changed
    PERFORMANCE = "performance"      # Performance characteristics changed


@dataclass
class BehaviorSignature:
    """Captures the semantic behavior of a function or module"""
    
    # Input/output types
    input_types: List[TypeAnnotation]
    output_type: TypeAnnotation
    
    # Effects
    effects: Set[EffectType]
    effect_ordering: List[Tuple[EffectType, str]]  # Ordered effect operations
    
    # Semantic properties
    is_pure: bool
    is_total: bool  # Defined for all inputs
    is_deterministic: bool
    is_idempotent: bool
    
    # Performance characteristics
    time_complexity: str  # e.g., "O(n)", "O(n log n)"
    space_complexity: str  # e.g., "O(1)", "O(n)"
    
    # Behavioral constraints
    preconditions: List[str]  # Logical preconditions
    postconditions: List[str]  # Logical postconditions
    invariants: List[str]  # Maintained invariants
    
    # Example behaviors (input -> output mappings)
    example_behaviors: List[Tuple[Any, Any]] = field(default_factory=list)
    
    def to_hash(self) -> str:
        """Generate a stable hash of the behavior"""
        behavior_dict = {
            "inputs": [str(t) for t in self.input_types],
            "output": str(self.output_type),
            "effects": sorted([e.name for e in self.effects]),
            "effect_ordering": [(e.name, op) for e, op in self.effect_ordering],
            "properties": {
                "pure": self.is_pure,
                "total": self.is_total,
                "deterministic": self.is_deterministic,
                "idempotent": self.is_idempotent
            },
            "complexity": {
                "time": self.time_complexity,
                "space": self.space_complexity
            },
            "constraints": {
                "pre": sorted(self.preconditions),
                "post": sorted(self.postconditions),
                "invariants": sorted(self.invariants)
            }
        }
        
        # Stable JSON serialization
        json_str = json.dumps(behavior_dict, sort_keys=True)
        return hashlib.sha256(json_str.encode()).hexdigest()[:16]


@dataclass
class SemanticVersion:
    """Semantic version based on behavior"""
    major: int  # Incompatible behavior changes
    minor: int  # Compatible new behaviors
    patch: int  # Bug fixes (same behavior, different implementation)
    behavior_hash: str  # Hash of behavior signature
    
    def __str__(self):
        return f"{self.major}.{self.minor}.{self.patch}+{self.behavior_hash}"


class BehaviorAnalyzer:
    """Analyzes code to extract behavioral signatures"""
    
    def __init__(self):
        self.signatures: Dict[str, BehaviorSignature] = {}
        self.version_history: Dict[str, List[SemanticVersion]] = {}
    
    def analyze_function(self, name: str, graph: Graph, node_id: str) -> BehaviorSignature:
        """Extract behavior signature from a function"""
        node = graph.nodes[node_id]
        
        # Extract type information
        type_info = self._infer_types(graph, node_id)
        
        # Analyze effects
        effects = self._analyze_effects(graph, node_id)
        effect_ordering = self._trace_effect_ordering(graph, node_id)
        
        # Analyze properties
        properties = self._analyze_properties(graph, node_id)
        
        # Extract complexity
        complexity = self._analyze_complexity(graph, node_id)
        
        # Extract constraints
        constraints = self._extract_constraints(graph, node_id)
        
        # Generate example behaviors through symbolic execution
        examples = self._generate_examples(graph, node_id)
        
        signature = BehaviorSignature(
            input_types=type_info["inputs"],
            output_type=type_info["output"],
            effects=effects,
            effect_ordering=effect_ordering,
            is_pure=properties["pure"],
            is_total=properties["total"],
            is_deterministic=properties["deterministic"],
            is_idempotent=properties["idempotent"],
            time_complexity=complexity["time"],
            space_complexity=complexity["space"],
            preconditions=constraints["pre"],
            postconditions=constraints["post"],
            invariants=constraints["invariants"],
            example_behaviors=examples
        )
        
        self.signatures[name] = signature
        return signature
    
    def _infer_types(self, graph: Graph, node_id: str) -> Dict[str, Any]:
        """Infer input/output types"""
        # This would use the type inference system
        from ..types.type_system import TypeChecker
        checker = TypeChecker()
        
        # Simplified for now
        return {
            "inputs": [],
            "output": TypeAnnotation("Any")
        }
    
    def _analyze_effects(self, graph: Graph, node_id: str) -> Set[EffectType]:
        """Analyze effects triggered by the function"""
        effects = set()
        
        def collect_effects(nid: str):
            node = graph.nodes.get(nid)
            if node:
                effects.update(node.effects)
                for child_id in node.get_dependencies():
                    collect_effects(child_id)
        
        collect_effects(node_id)
        return effects
    
    def _trace_effect_ordering(self, graph: Graph, node_id: str) -> List[Tuple[EffectType, str]]:
        """Trace the ordering of effect operations"""
        # This would perform a topological sort of effect operations
        return []
    
    def _analyze_properties(self, graph: Graph, node_id: str) -> Dict[str, bool]:
        """Analyze semantic properties"""
        effects = self._analyze_effects(graph, node_id)
        
        return {
            "pure": len(effects) == 0 or effects == {EffectType.PURE},
            "total": self._check_totality(graph, node_id),
            "deterministic": EffectType.RANDOM not in effects,
            "idempotent": self._check_idempotence(graph, node_id)
        }
    
    def _check_totality(self, graph: Graph, node_id: str) -> bool:
        """Check if function is total (defined for all inputs)"""
        # Look for error cases, exceptions, or undefined behavior
        return True  # Simplified
    
    def _check_idempotence(self, graph: Graph, node_id: str) -> bool:
        """Check if f(f(x)) = f(x)"""
        # This would require symbolic execution or testing
        return False  # Conservative default
    
    def _analyze_complexity(self, graph: Graph, node_id: str) -> Dict[str, str]:
        """Analyze time and space complexity"""
        # This would analyze loops, recursion, data structure usage
        return {
            "time": "O(?)",
            "space": "O(?)"
        }
    
    def _extract_constraints(self, graph: Graph, node_id: str) -> Dict[str, List[str]]:
        """Extract logical constraints"""
        # This would parse assertions, contracts, or infer from code
        return {
            "pre": [],
            "post": [],
            "invariants": []
        }
    
    def _generate_examples(self, graph: Graph, node_id: str) -> List[Tuple[Any, Any]]:
        """Generate example input/output pairs"""
        # This would use symbolic execution or property-based testing
        return []
    
    def compute_version_change(self, old_sig: BehaviorSignature, 
                             new_sig: BehaviorSignature) -> BehaviorChange:
        """Determine the type of change between signatures"""
        
        # Breaking changes
        if old_sig.input_types != new_sig.input_types:
            return BehaviorChange.INCOMPATIBLE
        
        if not self._is_subtype(new_sig.output_type, old_sig.output_type):
            return BehaviorChange.INCOMPATIBLE
        
        if not new_sig.effects.issubset(old_sig.effects):
            return BehaviorChange.EFFECT
        
        # Property changes
        if old_sig.is_pure and not new_sig.is_pure:
            return BehaviorChange.INCOMPATIBLE
        
        if old_sig.is_deterministic and not new_sig.is_deterministic:
            return BehaviorChange.INCOMPATIBLE
        
        # Performance changes
        if (old_sig.time_complexity != new_sig.time_complexity or
            old_sig.space_complexity != new_sig.space_complexity):
            return BehaviorChange.PERFORMANCE
        
        # Compatible changes
        if new_sig != old_sig:
            return BehaviorChange.COMPATIBLE
        
        return BehaviorChange.NONE
    
    def _is_subtype(self, sub: TypeAnnotation, super: TypeAnnotation) -> bool:
        """Check if sub is a subtype of super"""
        # Simplified - would use full type system
        return str(sub) == str(super)
    
    def version_function(self, name: str, signature: BehaviorSignature) -> SemanticVersion:
        """Generate a semantic version for a function"""
        behavior_hash = signature.to_hash()
        
        if name not in self.version_history:
            # First version
            version = SemanticVersion(1, 0, 0, behavior_hash)
            self.version_history[name] = [version]
            return version
        
        # Compare with previous version
        history = self.version_history[name]
        last_version = history[-1]
        last_sig = self.signatures.get(f"{name}_v{last_version}")
        
        if not last_sig:
            # Can't compare, bump minor
            version = SemanticVersion(
                last_version.major,
                last_version.minor + 1,
                0,
                behavior_hash
            )
        else:
            change = self.compute_version_change(last_sig, signature)
            
            if change == BehaviorChange.INCOMPATIBLE:
                version = SemanticVersion(last_version.major + 1, 0, 0, behavior_hash)
            elif change == BehaviorChange.COMPATIBLE:
                version = SemanticVersion(
                    last_version.major,
                    last_version.minor + 1,
                    0,
                    behavior_hash
                )
            elif change == BehaviorChange.NONE:
                version = SemanticVersion(
                    last_version.major,
                    last_version.minor,
                    last_version.patch + 1,
                    behavior_hash
                )
            else:
                # Effect or performance changes - minor bump with metadata
                version = SemanticVersion(
                    last_version.major,
                    last_version.minor + 1,
                    0,
                    behavior_hash
                )
        
        history.append(version)
        self.signatures[f"{name}_v{version}"] = signature
        return version


def generate_behavior_documentation(signature: BehaviorSignature) -> str:
    """Generate documentation from behavior signature"""
    doc = []
    
    # Type signature
    input_str = ", ".join(str(t) for t in signature.input_types)
    doc.append(f"Type: ({input_str}) -> {signature.output_type}")
    
    # Effects
    if signature.effects:
        effect_str = ", ".join(e.name for e in sorted(signature.effects))
        doc.append(f"Effects: {effect_str}")
    
    # Properties
    props = []
    if signature.is_pure:
        props.append("pure")
    if signature.is_total:
        props.append("total")
    if signature.is_deterministic:
        props.append("deterministic")
    if signature.is_idempotent:
        props.append("idempotent")
    
    if props:
        doc.append(f"Properties: {', '.join(props)}")
    
    # Complexity
    doc.append(f"Time: {signature.time_complexity}, Space: {signature.space_complexity}")
    
    # Constraints
    if signature.preconditions:
        doc.append("Preconditions:")
        for pre in signature.preconditions:
            doc.append(f"  - {pre}")
    
    if signature.postconditions:
        doc.append("Postconditions:")
        for post in signature.postconditions:
            doc.append(f"  - {post}")
    
    # Examples
    if signature.example_behaviors:
        doc.append("Examples:")
        for inp, out in signature.example_behaviors[:3]:
            doc.append(f"  {inp} => {out}")
    
    return "\n".join(doc)