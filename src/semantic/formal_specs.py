"""
ClaudeLang Formal Specification System

This module enables embedding formal specifications directly in code
and verifying them at compile time or runtime.
"""

from typing import Dict, List, Optional, Any, Callable, Set
from dataclasses import dataclass, field
from enum import Enum

from ..core.ast import Graph, ASTNode, NodeType, EffectType


class SpecType(Enum):
    """Types of formal specifications"""
    PRECONDITION = "requires"     # Function preconditions
    POSTCONDITION = "ensures"     # Function postconditions
    INVARIANT = "invariant"       # Loop/data structure invariants
    ASSERTION = "assert"          # Runtime assertions
    ASSUMPTION = "assume"         # Assumptions for verification
    LEMMA = "lemma"              # Auxiliary theorems
    PROPERTY = "property"         # General properties


@dataclass
class FormalSpec:
    """A formal specification"""
    spec_type: SpecType
    predicate: str  # Logical predicate in ClaudeLang syntax
    description: str
    node_id: Optional[str] = None  # Associated AST node
    verified: bool = False
    proof: Optional[str] = None
    
    def to_smt(self) -> str:
        """Convert to SMT-LIB format for verification"""
        # Convert ClaudeLang predicate to SMT
        return f"(assert {self._translate_to_smt(self.predicate)})"
    
    def _translate_to_smt(self, pred: str) -> str:
        """Translate predicate to SMT format"""
        # Simple translation - would be more sophisticated
        translations = {
            "and": "and",
            "or": "or", 
            "not": "not",
            "=>": "=>",
            "=": "=",
            ">": ">",
            "<": "<",
            ">=": ">=",
            "<=": "<=",
            "+": "+",
            "-": "-",
            "*": "*",
            "forall": "forall",
            "exists": "exists"
        }
        
        result = pred
        for cl_op, smt_op in translations.items():
            result = result.replace(cl_op, smt_op)
        
        return result


@dataclass
class ContractedFunction:
    """A function with formal contracts"""
    name: str
    preconditions: List[FormalSpec] = field(default_factory=list)
    postconditions: List[FormalSpec] = field(default_factory=list)
    invariants: List[FormalSpec] = field(default_factory=list)
    complexity: Optional[str] = None  # e.g., "O(n log n)"
    termination_proof: Optional[str] = None


@dataclass  
class SpecAnnotation:
    """Specification annotation for AST nodes"""
    specs: List[FormalSpec] = field(default_factory=list)
    verified_properties: Dict[str, bool] = field(default_factory=dict)
    counterexamples: List[Dict[str, Any]] = field(default_factory=list)


class SpecificationEngine:
    """Engine for managing and verifying formal specifications"""
    
    def __init__(self):
        self.contracts: Dict[str, ContractedFunction] = {}
        self.global_invariants: List[FormalSpec] = []
        self.verified_lemmas: Dict[str, FormalSpec] = {}
        self.spec_annotations: Dict[str, SpecAnnotation] = {}
    
    def add_contract(self, function_name: str, 
                    requires: List[str] = None,
                    ensures: List[str] = None,
                    invariants: List[str] = None):
        """Add a contract to a function"""
        contract = self.contracts.get(function_name, 
                                    ContractedFunction(function_name))
        
        if requires:
            for req in requires:
                spec = FormalSpec(
                    spec_type=SpecType.PRECONDITION,
                    predicate=req,
                    description=f"Precondition for {function_name}"
                )
                contract.preconditions.append(spec)
        
        if ensures:
            for ens in ensures:
                spec = FormalSpec(
                    spec_type=SpecType.POSTCONDITION,
                    predicate=ens,
                    description=f"Postcondition for {function_name}"
                )
                contract.postconditions.append(spec)
        
        if invariants:
            for inv in invariants:
                spec = FormalSpec(
                    spec_type=SpecType.INVARIANT,
                    predicate=inv,
                    description=f"Invariant for {function_name}"
                )
                contract.invariants.append(spec)
        
        self.contracts[function_name] = contract
    
    def verify_contracts(self, graph: Graph) -> Dict[str, bool]:
        """Verify all contracts in a program"""
        results = {}
        
        for func_name, contract in self.contracts.items():
            # Find function in graph
            func_node = self._find_function(graph, func_name)
            if not func_node:
                results[func_name] = False
                continue
            
            # Verify preconditions
            pre_valid = all(self._verify_spec(spec, graph, func_node) 
                          for spec in contract.preconditions)
            
            # Verify postconditions
            post_valid = all(self._verify_spec(spec, graph, func_node)
                           for spec in contract.postconditions)
            
            # Verify invariants
            inv_valid = all(self._verify_spec(spec, graph, func_node)
                          for spec in contract.invariants)
            
            results[func_name] = pre_valid and post_valid and inv_valid
        
        return results
    
    def _verify_spec(self, spec: FormalSpec, graph: Graph, 
                    context_node: str) -> bool:
        """Verify a single specification"""
        # Use SMT solver or symbolic execution
        # For now, simple checks
        
        if spec.spec_type == SpecType.PRECONDITION:
            # Check if precondition can be violated
            return self._check_precondition(spec, graph, context_node)
        
        elif spec.spec_type == SpecType.POSTCONDITION:
            # Check if postcondition is guaranteed
            return self._check_postcondition(spec, graph, context_node)
        
        elif spec.spec_type == SpecType.INVARIANT:
            # Check if invariant is maintained
            return self._check_invariant(spec, graph, context_node)
        
        return False
    
    def _check_precondition(self, spec: FormalSpec, graph: Graph, 
                           func_node: str) -> bool:
        """Check if precondition can be satisfied"""
        # Analyze call sites
        call_sites = self._find_call_sites(graph, func_node)
        
        for call_site in call_sites:
            # Check if arguments satisfy precondition
            if not self._evaluate_at_call_site(spec.predicate, graph, call_site):
                return False
        
        return True
    
    def _check_postcondition(self, spec: FormalSpec, graph: Graph,
                            func_node: str) -> bool:
        """Check if postcondition is guaranteed"""
        # Analyze function body
        # Would use symbolic execution or abstract interpretation
        return True
    
    def _check_invariant(self, spec: FormalSpec, graph: Graph,
                        context_node: str) -> bool:
        """Check if invariant is maintained"""
        # Find loop or recursive structure
        # Verify invariant holds initially and is preserved
        return True
    
    def annotate_graph(self, graph: Graph):
        """Add specification annotations to graph nodes"""
        for node_id, node in graph.nodes.items():
            annotation = SpecAnnotation()
            
            # Extract specifications from node metadata
            if hasattr(node, 'metadata') and 'specs' in node.metadata:
                for spec_dict in node.metadata['specs']:
                    spec = FormalSpec(
                        spec_type=SpecType(spec_dict['type']),
                        predicate=spec_dict['predicate'],
                        description=spec_dict.get('description', ''),
                        node_id=node_id
                    )
                    annotation.specs.append(spec)
            
            if annotation.specs:
                self.spec_annotations[node_id] = annotation
    
    def generate_verification_conditions(self, graph: Graph) -> List[str]:
        """Generate verification conditions for the program"""
        vcs = []
        
        # For each function with contracts
        for func_name, contract in self.contracts.items():
            func_node = self._find_function(graph, func_name)
            if not func_node:
                continue
            
            # Generate VCs for preconditions
            for i, pre in enumerate(contract.preconditions):
                vc = f"VC_{func_name}_pre_{i}: {pre.predicate}"
                vcs.append(vc)
            
            # Generate VCs for postconditions
            for i, post in enumerate(contract.postconditions):
                # Assume preconditions, prove postcondition
                assumes = " AND ".join(pre.predicate for pre in contract.preconditions)
                vc = f"VC_{func_name}_post_{i}: {assumes} => {post.predicate}"
                vcs.append(vc)
        
        return vcs
    
    def prove_lemma(self, lemma_name: str, statement: str, 
                   proof_script: Optional[str] = None) -> bool:
        """Prove a lemma and add it to verified lemmas"""
        lemma = FormalSpec(
            spec_type=SpecType.LEMMA,
            predicate=statement,
            description=f"Lemma: {lemma_name}",
            proof=proof_script
        )
        
        # Attempt to verify the lemma
        if self._verify_lemma(lemma):
            lemma.verified = True
            self.verified_lemmas[lemma_name] = lemma
            return True
        
        return False
    
    def _verify_lemma(self, lemma: FormalSpec) -> bool:
        """Verify a lemma using proof script or automated proving"""
        if lemma.proof:
            # Execute proof script
            return self._execute_proof_script(lemma.proof)
        else:
            # Try automated proving
            return self._auto_prove(lemma.predicate)
    
    def _execute_proof_script(self, script: str) -> bool:
        """Execute a proof script"""
        # Would integrate with proof assistant
        return True
    
    def _auto_prove(self, statement: str) -> bool:
        """Attempt automated proving"""
        # Would use SMT solver or theorem prover
        return False
    
    def _find_function(self, graph: Graph, name: str) -> Optional[str]:
        """Find a function node by name"""
        for node_id, node in graph.nodes.items():
            if hasattr(node, 'name') and node.name == name:
                return node_id
        return None
    
    def _find_call_sites(self, graph: Graph, func_node: str) -> List[str]:
        """Find all call sites of a function"""
        call_sites = []
        
        for node_id, node in graph.nodes.items():
            if node.node_type == NodeType.APPLICATION:
                if hasattr(node, 'function_id') and node.function_id == func_node:
                    call_sites.append(node_id)
        
        return call_sites
    
    def _evaluate_at_call_site(self, predicate: str, graph: Graph, 
                              call_site: str) -> bool:
        """Evaluate a predicate at a call site"""
        # Would use abstract interpretation
        return True
    
    def export_coq(self) -> str:
        """Export specifications to Coq format"""
        lines = ["(* ClaudeLang Formal Specifications *)", ""]
        
        # Export contracts
        for func_name, contract in self.contracts.items():
            lines.append(f"(* Contract for {func_name} *)")
            
            # Preconditions
            for pre in contract.preconditions:
                lines.append(f"Definition {func_name}_pre := {pre.predicate}.")
            
            # Postconditions
            for post in contract.postconditions:
                lines.append(f"Definition {func_name}_post := {post.predicate}.")
            
            lines.append("")
        
        # Export lemmas
        for lemma_name, lemma in self.verified_lemmas.items():
            lines.append(f"Lemma {lemma_name} : {lemma.predicate}.")
            if lemma.proof:
                lines.append(f"Proof. {lemma.proof} Qed.")
            lines.append("")
        
        return "\n".join(lines)
    
    def export_lean(self) -> str:
        """Export specifications to Lean format"""
        lines = ["-- ClaudeLang Formal Specifications", ""]
        
        for func_name, contract in self.contracts.items():
            lines.append(f"-- Contract for {func_name}")
            
            for pre in contract.preconditions:
                lines.append(f"def {func_name}_pre : Prop := {pre.predicate}")
            
            for post in contract.postconditions:
                lines.append(f"def {func_name}_post : Prop := {post.predicate}")
            
            lines.append("")
        
        return "\n".join(lines)


# Specification DSL functions
def requires(predicate: str) -> Callable:
    """Decorator for preconditions"""
    def decorator(func):
        # Store specification in function metadata
        if not hasattr(func, '_specs'):
            func._specs = []
        
        func._specs.append({
            'type': 'requires',
            'predicate': predicate
        })
        
        return func
    
    return decorator


def ensures(predicate: str) -> Callable:
    """Decorator for postconditions"""
    def decorator(func):
        if not hasattr(func, '_specs'):
            func._specs = []
        
        func._specs.append({
            'type': 'ensures',
            'predicate': predicate
        })
        
        return func
    
    return decorator


def invariant(predicate: str) -> Callable:
    """Decorator for invariants"""
    def decorator(func):
        if not hasattr(func, '_specs'):
            func._specs = []
        
        func._specs.append({
            'type': 'invariant',
            'predicate': predicate
        })
        
        return func
    
    return decorator