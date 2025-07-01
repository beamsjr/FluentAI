"""
ClaudeLang Type System

This module implements the type system for ClaudeLang, including:
- Algebraic data types
- Effect types
- Probabilistic types
- Type inference
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Set, Union, Tuple
from enum import Enum, auto
from ..core.ast import TypeAnnotation, EffectType, ASTNode, Graph


class TypeKind(Enum):
    """Kinds of types in the type system"""
    PRIMITIVE = auto()
    FUNCTION = auto()
    TUPLE = auto()
    LIST = auto()
    RECORD = auto()
    VARIANT = auto()
    EFFECT = auto()
    UNCERTAIN = auto()
    TEMPORAL = auto()
    TYPE_VARIABLE = auto()


@dataclass
class Type:
    """Base class for all types"""
    kind: TypeKind
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)


@dataclass
class PrimitiveType(Type):
    """Primitive types like Int, Float, String, Bool"""
    name: str = ""
    kind: TypeKind = TypeKind.PRIMITIVE
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.PRIMITIVE
    
    def __str__(self):
        return self.name


@dataclass
class FunctionType(Type):
    """Function types with explicit effects"""
    params: List[Type] = field(default_factory=list)
    result: Type = None
    is_pure: bool = True
    kind: TypeKind = TypeKind.FUNCTION
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.FUNCTION
        # Collect effects from parameters and result
        for param in self.params:
            self.effects.update(param.effects)
        self.effects.update(self.result.effects)
        
        if not self.is_pure:
            self.effects.add(EffectType.STATE)
    
    def __str__(self):
        params_str = " -> ".join(str(p) for p in self.params)
        effects_str = f" ~{{{', '.join(e.name for e in self.effects)}}}" if self.effects else ""
        return f"({params_str} -> {self.result}{effects_str})"


@dataclass
class TupleType(Type):
    """Product types (tuples)"""
    elements: List[Type] = field(default_factory=list)
    kind: TypeKind = TypeKind.TUPLE
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.TUPLE
        # Collect effects from all elements
        for elem in self.elements:
            self.effects.update(elem.effects)
    
    def __str__(self):
        elements_str = ", ".join(str(e) for e in self.elements)
        return f"({elements_str})"


@dataclass
class ListType(Type):
    """Homogeneous list type"""
    element_type: Type = None
    kind: TypeKind = TypeKind.LIST
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.LIST
        self.effects.update(self.element_type.effects)
    
    def __str__(self):
        return f"[{self.element_type}]"


@dataclass
class RecordType(Type):
    """Record types with named fields"""
    fields: Dict[str, Type] = field(default_factory=dict)
    kind: TypeKind = TypeKind.RECORD
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.RECORD
        # Collect effects from all fields
        for field_type in self.fields.values():
            self.effects.update(field_type.effects)
    
    def __str__(self):
        fields_str = ", ".join(f"{k}: {v}" for k, v in self.fields.items())
        return f"{{{fields_str}}}"


@dataclass
class VariantType(Type):
    """Sum types (variants/enums)"""
    variants: Dict[str, Optional[Type]] = field(default_factory=dict)  # Tag -> optional payload type
    kind: TypeKind = TypeKind.VARIANT
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.VARIANT
        # Collect effects from all variant payloads
        for variant_type in self.variants.values():
            if variant_type:
                self.effects.update(variant_type.effects)
    
    def __str__(self):
        variants_str = " | ".join(
            f"{tag}({payload})" if payload else tag
            for tag, payload in self.variants.items()
        )
        return f"<{variants_str}>"


@dataclass
class EffectType_(Type):
    """Effect types for tracking side effects"""
    effect_kind: EffectType = EffectType.IO
    payload_type: Optional[Type] = None
    kind: TypeKind = TypeKind.EFFECT
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.EFFECT
        self.effects.add(self.effect_kind)
        if self.payload_type:
            self.effects.update(self.payload_type.effects)
    
    def __str__(self):
        payload_str = f"[{self.payload_type}]" if self.payload_type else ""
        return f"Effect<{self.effect_kind.name}{payload_str}>"


@dataclass
class UncertainType(Type):
    """Probabilistic types with confidence"""
    base_type: Type = None
    confidence: float = 1.0  # 0.0 to 1.0
    distribution: str = "uniform"  # Distribution type
    kind: TypeKind = TypeKind.UNCERTAIN
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.UNCERTAIN
        self.effects.add(EffectType.RANDOM)
        self.effects.update(self.base_type.effects)
    
    def __str__(self):
        return f"Uncertain<{self.base_type}, {self.confidence:.2f}>"


@dataclass
class TemporalType(Type):
    """Temporal types with time constraints"""
    base_type: Type = None
    constraint: str = ""  # e.g., "within 5s", "before T", "eventually"
    kind: TypeKind = TypeKind.TEMPORAL
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.TEMPORAL
        self.effects.add(EffectType.TIME)
        self.effects.update(self.base_type.effects)
    
    def __str__(self):
        return f"Temporal<{self.base_type}, {self.constraint}>"


@dataclass
class TypeVariable(Type):
    """Type variables for polymorphism"""
    name: str = ""
    constraints: List[Type] = field(default_factory=list)
    kind: TypeKind = TypeKind.TYPE_VARIABLE
    effects: Set[EffectType] = field(default_factory=set)
    metadata: Dict[str, any] = field(default_factory=dict)
    
    def __post_init__(self):
        self.kind = TypeKind.TYPE_VARIABLE
    
    def __str__(self):
        if self.constraints:
            constraints_str = f" where {self.name}: {' + '.join(str(c) for c in self.constraints)}"
            return f"{self.name}{constraints_str}"
        return self.name


class TypeEnvironment:
    """Type environment for type checking"""
    
    def __init__(self):
        self.bindings: Dict[str, Type] = {}
        self.type_variables: Dict[str, TypeVariable] = {}
        self._init_primitives()
    
    def _init_primitives(self):
        """Initialize primitive types"""
        self.bindings["Int"] = PrimitiveType("Int")
        self.bindings["Float"] = PrimitiveType("Float")
        self.bindings["String"] = PrimitiveType("String")
        self.bindings["Bool"] = PrimitiveType("Bool")
        self.bindings["Unit"] = PrimitiveType("Unit")
    
    def bind(self, name: str, type_: Type):
        """Bind a name to a type"""
        self.bindings[name] = type_
    
    def lookup(self, name: str) -> Optional[Type]:
        """Look up a type by name"""
        return self.bindings.get(name)
    
    def fresh_type_var(self, prefix: str = "T") -> TypeVariable:
        """Generate a fresh type variable"""
        i = 0
        while f"{prefix}{i}" in self.type_variables:
            i += 1
        var = TypeVariable(f"{prefix}{i}")
        self.type_variables[var.name] = var
        return var


class TypeChecker:
    """Type checker for ClaudeLang"""
    
    def __init__(self):
        self.env = TypeEnvironment()
        self.errors: List[str] = []
    
    def infer_type(self, node: ASTNode, graph: Graph) -> Type:
        """Infer the type of an AST node"""
        if hasattr(node, 'type_annotation') and node.type_annotation:
            return self._annotation_to_type(node.type_annotation)
        
        # Type inference based on node type
        from ..core.ast import NodeType, Literal, Variable, Application, Lambda, Let, If
        
        if node.node_type == NodeType.LITERAL:
            literal = node
            return self._infer_literal_type(literal)
        
        elif node.node_type == NodeType.VARIABLE:
            var = node
            return self.env.lookup(var.name) or self.env.fresh_type_var()
        
        elif node.node_type == NodeType.APPLICATION:
            app = node
            func_type = self.infer_type(graph.get_node(app.function_id), graph)
            
            # Handle case where function type inference returned a type variable
            if hasattr(func_type, 'kind') and func_type.kind != TypeKind.FUNCTION:
                # Try to infer function type for closures
                return self.env.fresh_type_var()
            
            # Check argument types
            if hasattr(func_type, 'params') and hasattr(func_type, 'result'):
                for i, arg_id in enumerate(app.argument_ids):
                    if i < len(func_type.params):
                        arg_type = self.infer_type(graph.get_node(arg_id), graph)
                        if not self.unify(arg_type, func_type.params[i]):
                            self.errors.append(
                                f"Type mismatch in argument {i}: expected {func_type.params[i]}, got {arg_type}"
                            )
                
                return func_type.result
            
            return self.env.fresh_type_var()
        
        elif node.node_type == NodeType.LAMBDA:
            lambda_node = node
            # Create function type for lambda
            param_types = []
            for _ in lambda_node.parameter_names:
                param_types.append(self.env.fresh_type_var())
            
            # Bind parameters in environment
            new_env = TypeEnvironment()
            new_env.bindings = self.env.bindings.copy()
            for name, ptype in zip(lambda_node.parameter_names, param_types):
                new_env.bind(name, ptype)
            
            # Infer body type with parameters bound
            old_env = self.env
            self.env = new_env
            body_type = self.infer_type(graph.get_node(lambda_node.body_id), graph)
            self.env = old_env
            
            return FunctionType(
                params=param_types,
                result=body_type
            )
        
        # Add more cases for other node types...
        
        return self.env.fresh_type_var()
    
    def _annotation_to_type(self, ann: TypeAnnotation) -> Type:
        """Convert a type annotation to a Type object"""
        if ann.name in ["Int", "Float", "String", "Bool", "Unit"]:
            return PrimitiveType(name=ann.name)
        
        if ann.name == "Function" and ann.parameters:
            params = ann.parameters[:-1]
            result = ann.parameters[-1]
            return FunctionType(
                params=[self._annotation_to_type(p) for p in params],
                result=self._annotation_to_type(result),
                effects=ann.effects
            )
        
        if ann.name == "List" and ann.parameters:
            return ListType(element_type=self._annotation_to_type(ann.parameters[0]))
        
        if ann.name == "Tuple" and ann.parameters:
            return TupleType(elements=[self._annotation_to_type(p) for p in ann.parameters])
        
        if ann.name == "Uncertain" and ann.parameters:
            return UncertainType(
                base_type=self._annotation_to_type(ann.parameters[0]),
                confidence=ann.confidence or 1.0
            )
        
        # Default to type variable for unknown types
        return self.env.fresh_type_var(ann.name)
    
    def _infer_literal_type(self, literal: 'Literal') -> Type:
        """Infer type of a literal"""
        type_map = {
            "int": PrimitiveType("Int"),
            "float": PrimitiveType("Float"),
            "string": PrimitiveType("String"),
            "bool": PrimitiveType("Bool"),
        }
        return type_map.get(literal.literal_type, self.env.fresh_type_var())
    
    def unify(self, t1: Type, t2: Type) -> bool:
        """Unify two types"""
        # Simple unification for now
        if t1.kind == TypeKind.TYPE_VARIABLE:
            return True
        if t2.kind == TypeKind.TYPE_VARIABLE:
            return True
        
        if t1.kind != t2.kind:
            return False
        
        if t1.kind == TypeKind.PRIMITIVE:
            return t1.name == t2.name
        
        if t1.kind == TypeKind.FUNCTION:
            if len(t1.params) != len(t2.params):
                return False
            for p1, p2 in zip(t1.params, t2.params):
                if not self.unify(p1, p2):
                    return False
            return self.unify(t1.result, t2.result)
        
        # Add more unification cases...
        
        return True
    
    def check_effects(self, graph: Graph, allowed_effects: Set[EffectType]) -> List[str]:
        """Check that a graph only uses allowed effects"""
        actual_effects = graph.get_effects()
        forbidden = actual_effects - allowed_effects
        
        if forbidden:
            return [f"Forbidden effects used: {', '.join(e.name for e in forbidden)}"]
        return []
    
    def type_check(self, graph: Graph) -> Tuple[bool, List[str]]:
        """Type check an entire graph"""
        self.errors = []
        
        # Check each node in topological order
        for node_id in graph.topological_sort():
            node = graph.get_node(node_id)
            if node:
                self.infer_type(node, graph)
        
        # Validate graph structure
        self.errors.extend(graph.validate())
        
        return len(self.errors) == 0, self.errors