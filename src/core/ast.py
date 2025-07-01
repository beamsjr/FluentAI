"""
ClaudeLang Abstract Syntax Tree (AST) Definitions

This module defines the core AST nodes for ClaudeLang. Each node represents
a computation with explicit inputs, outputs, and effects.
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional, Set, Any, Union
from enum import Enum, auto
import uuid


class NodeType(Enum):
    """Types of AST nodes in ClaudeLang"""
    LITERAL = auto()
    VARIABLE = auto()
    FUNCTION = auto()
    APPLICATION = auto()
    LAMBDA = auto()
    LET = auto()
    IF = auto()
    EFFECT = auto()
    SEQUENCE = auto()
    PARALLEL = auto()
    UNCERTAINTY = auto()
    MODULE = auto()
    IMPORT = auto()
    EXPORT = auto()
    QUALIFIED_VAR = auto()
    MATCH = auto()
    PATTERN_LITERAL = auto()
    PATTERN_VAR = auto()
    PATTERN_CONSTRUCTOR = auto()
    PATTERN_LIST = auto()
    PATTERN_WILDCARD = auto()
    CONTRACT = auto()
    DATA_DECLARATION = auto()
    TYPE_ASCRIPTION = auto()


class EffectType(Enum):
    """Types of effects that operations can have"""
    PURE = auto()
    IO = auto()
    STATE = auto()
    ERROR = auto()
    TIME = auto()
    NETWORK = auto()
    RANDOM = auto()


@dataclass
class TypeAnnotation:
    """Type annotation for AST nodes"""
    name: str
    parameters: List['TypeAnnotation'] = field(default_factory=list)
    effects: Set[EffectType] = field(default_factory=set)
    confidence: Optional[float] = None  # For probabilistic types

    def __str__(self):
        if self.parameters:
            params = ", ".join(str(p) for p in self.parameters)
            return f"{self.name}[{params}]"
        return self.name


@dataclass
class ASTNode:
    """Base class for all AST nodes"""
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    node_type: NodeType = NodeType.LITERAL
    type_annotation: Optional[TypeAnnotation] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    source_location: Optional[Dict[str, int]] = None
    
    def get_effects(self) -> Set[EffectType]:
        """Get all effects this node can produce"""
        if self.type_annotation:
            return self.type_annotation.effects
        return {EffectType.PURE}
    
    def get_dependencies(self) -> List[str]:
        """Get IDs of nodes this node depends on"""
        return []


@dataclass
class Literal(ASTNode):
    """Literal value node"""
    value: Any = None
    literal_type: str = ""  # "int", "float", "string", "bool", etc.
    
    def __post_init__(self):
        self.node_type = NodeType.LITERAL
        if not self.type_annotation:
            self.type_annotation = TypeAnnotation(
                name=self.literal_type,
                effects={EffectType.PURE}
            )


@dataclass
class Variable(ASTNode):
    """Variable reference node"""
    name: str = ""
    binding_id: Optional[str] = None  # ID of the node that binds this variable
    
    def __post_init__(self):
        self.node_type = NodeType.VARIABLE
    
    def get_dependencies(self) -> List[str]:
        return [self.binding_id] if self.binding_id else []


@dataclass
class Function(ASTNode):
    """Built-in function node"""
    name: str = ""
    arity: int = 0
    effects: Set[EffectType] = field(default_factory=lambda: {EffectType.PURE})
    implementation: Optional[str] = None  # Reference to native implementation
    
    def __post_init__(self):
        self.node_type = NodeType.FUNCTION
        if self.type_annotation:
            self.type_annotation.effects = self.effects


@dataclass
class Application(ASTNode):
    """Function application node"""
    function_id: str = ""
    argument_ids: List[str] = field(default_factory=list)
    is_parallel: bool = False  # Can arguments be evaluated in parallel?
    
    def __post_init__(self):
        self.node_type = NodeType.APPLICATION
    
    def get_dependencies(self) -> List[str]:
        return [self.function_id] + self.argument_ids


@dataclass
class Lambda(ASTNode):
    """Lambda abstraction node"""
    parameter_names: List[str] = field(default_factory=list)
    parameter_types: List[TypeAnnotation] = field(default_factory=list)
    body_id: str = ""
    captured_variables: Dict[str, str] = field(default_factory=dict)  # name -> binding_id
    
    def __post_init__(self):
        self.node_type = NodeType.LAMBDA
    
    def get_dependencies(self) -> List[str]:
        deps = [self.body_id]
        deps.extend(self.captured_variables.values())
        return deps


@dataclass
class Let(ASTNode):
    """Let binding node"""
    bindings: List[Dict[str, str]] = field(default_factory=list)  # [{"name": str, "value_id": str}, ...]
    body_id: str = ""
    is_recursive: bool = False
    
    def __post_init__(self):
        self.node_type = NodeType.LET
    
    def get_dependencies(self) -> List[str]:
        deps = [binding["value_id"] for binding in self.bindings]
        deps.append(self.body_id)
        return deps


@dataclass
class If(ASTNode):
    """Conditional node"""
    condition_id: str = ""
    then_id: str = ""
    else_id: str = ""
    
    def __post_init__(self):
        self.node_type = NodeType.IF
    
    def get_dependencies(self) -> List[str]:
        return [self.condition_id, self.then_id, self.else_id]


@dataclass
class Effect(ASTNode):
    """Effect operation node"""
    effect_type: EffectType = EffectType.IO
    operation: str = ""
    argument_ids: List[str] = field(default_factory=list)
    handler_id: Optional[str] = None  # Optional custom handler
    
    def __post_init__(self):
        self.node_type = NodeType.EFFECT
        if not self.type_annotation:
            self.type_annotation = TypeAnnotation(
                name="Effect",
                parameters=[],
                effects={self.effect_type}
            )
    
    def get_dependencies(self) -> List[str]:
        deps = self.argument_ids.copy()
        if self.handler_id:
            deps.append(self.handler_id)
        return deps


@dataclass
class Sequence(ASTNode):
    """Sequential composition node"""
    step_ids: List[str] = field(default_factory=list)
    
    def __post_init__(self):
        self.node_type = NodeType.SEQUENCE
    
    def get_dependencies(self) -> List[str]:
        return self.step_ids


@dataclass
class Parallel(ASTNode):
    """Parallel composition node"""
    branch_ids: List[str] = field(default_factory=list)
    merge_strategy: str = "tuple"  # How to combine results
    
    def __post_init__(self):
        self.node_type = NodeType.PARALLEL
    
    def get_dependencies(self) -> List[str]:
        return self.branch_ids


@dataclass
class Uncertainty(ASTNode):
    """Probabilistic choice node"""
    choices: List[Dict[str, Any]] = field(default_factory=list)  # [{"node_id": str, "probability": float}, ...]
    distribution_type: str = "discrete"
    
    def __post_init__(self):
        self.node_type = NodeType.UNCERTAINTY
        if not self.type_annotation:
            self.type_annotation = TypeAnnotation(
                name="Uncertain",
                effects={EffectType.RANDOM}
            )
    
    def get_dependencies(self) -> List[str]:
        return [choice["node_id"] for choice in self.choices]


@dataclass
class Contract(ASTNode):
    """Contract specification node for functions"""
    function_name: str = ""
    preconditions: List[str] = field(default_factory=list)  # Condition node IDs
    postconditions: List[str] = field(default_factory=list)  # Condition node IDs
    invariants: List[str] = field(default_factory=list)  # Condition node IDs
    complexity: Optional[str] = None  # Big-O complexity
    pure: bool = True  # Whether function is pure
    
    def __post_init__(self):
        self.node_type = NodeType.CONTRACT
        if not self.type_annotation:
            self.type_annotation = TypeAnnotation(
                name="Contract",
                effects=set()
            )
    
    def get_dependencies(self) -> List[str]:
        return self.preconditions + self.postconditions + self.invariants


@dataclass
class Graph:
    """Complete AST graph representation"""
    nodes: Dict[str, ASTNode] = field(default_factory=dict)
    root_id: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def add_node(self, node: ASTNode) -> str:
        """Add a node to the graph"""
        self.nodes[node.id] = node
        return node.id
    
    def get_node(self, node_id: str) -> Optional[ASTNode]:
        """Get a node by ID"""
        return self.nodes.get(node_id)
    
    def topological_sort(self) -> List[str]:
        """Return nodes in topological order"""
        visited = set()
        result = []
        
        def visit(node_id: str):
            if node_id in visited:
                return
            visited.add(node_id)
            
            node = self.nodes.get(node_id)
            if node:
                for dep_id in node.get_dependencies():
                    visit(dep_id)
                result.append(node_id)
        
        if self.root_id:
            visit(self.root_id)
        else:
            for node_id in self.nodes:
                visit(node_id)
        
        return result
    
    def get_effects(self) -> Set[EffectType]:
        """Get all effects in the graph"""
        effects = set()
        for node in self.nodes.values():
            effects.update(node.get_effects())
        return effects
    
    def validate(self) -> List[str]:
        """Validate the graph structure"""
        errors = []
        
        # Check all dependencies exist
        for node_id, node in self.nodes.items():
            for dep_id in node.get_dependencies():
                if dep_id not in self.nodes:
                    errors.append(f"Node {node_id} depends on non-existent node {dep_id}")
        
        # Check for cycles
        try:
            self.topological_sort()
        except RecursionError:
            errors.append("Graph contains cycles")
        
        # Check root exists
        if self.root_id and self.root_id not in self.nodes:
            errors.append(f"Root node {self.root_id} does not exist")
        
        return errors


@dataclass
class Module(ASTNode):
    """Module definition node"""
    name: str = ""
    exports: List[str] = field(default_factory=list)  # List of exported names
    body_id: str = ""  # Root node of module body
    
    def __post_init__(self):
        self.node_type = NodeType.MODULE
        if not self.type_annotation:
            self.type_annotation = TypeAnnotation(
                name="Module",
                effects={EffectType.PURE}
            )
    
    def get_dependencies(self) -> List[str]:
        return [self.body_id]


@dataclass
class Import(ASTNode):
    """Import statement node"""
    module_path: str = ""  # Path to module file or module name
    import_list: List[Dict[str, str]] = field(default_factory=list)  # [{"name": "foo", "as": "bar"}, ...]
    import_all: bool = False  # import * behavior
    
    def __post_init__(self):
        self.node_type = NodeType.IMPORT
        if not self.type_annotation:
            self.type_annotation = TypeAnnotation(
                name="Import",
                effects={EffectType.IO}  # Loading modules is an IO effect
            )
    
    def get_dependencies(self) -> List[str]:
        return []


@dataclass  
class Export(ASTNode):
    """Export statement node"""
    export_list: List[Dict[str, str]] = field(default_factory=list)  # [{"name": "foo", "as": "bar"}, ...]
    
    def __post_init__(self):
        self.node_type = NodeType.EXPORT
        if not self.type_annotation:
            self.type_annotation = TypeAnnotation(
                name="Export",
                effects={EffectType.PURE}
            )
    
    def get_dependencies(self) -> List[str]:
        return []


@dataclass
class QualifiedVariable(ASTNode):
    """Qualified variable reference (module.name)"""
    module_name: str = ""
    variable_name: str = ""
    
    def __post_init__(self):
        self.node_type = NodeType.QUALIFIED_VAR
    
    def get_dependencies(self) -> List[str]:
        return []


@dataclass
class Match(ASTNode):
    """Pattern matching expression"""
    expr_id: str = ""  # Expression to match against
    branches: List[Dict[str, str]] = field(default_factory=list)  # [{"pattern_id": str, "body_id": str}, ...]
    
    def __post_init__(self):
        self.node_type = NodeType.MATCH
        if not self.type_annotation:
            self.type_annotation = TypeAnnotation(
                name="Match",
                effects={EffectType.PURE}
            )
    
    def get_dependencies(self) -> List[str]:
        deps = [self.expr_id]
        for branch in self.branches:
            deps.extend([branch["pattern_id"], branch["body_id"]])
        return deps


@dataclass
class PatternLiteral(ASTNode):
    """Literal pattern in pattern matching"""
    value: Any = None
    
    def __post_init__(self):
        self.node_type = NodeType.PATTERN_LITERAL
    
    def get_dependencies(self) -> List[str]:
        return []


@dataclass
class PatternVar(ASTNode):
    """Variable binding pattern"""
    name: str = ""
    
    def __post_init__(self):
        self.node_type = NodeType.PATTERN_VAR
    
    def get_dependencies(self) -> List[str]:
        return []


@dataclass  
class PatternConstructor(ASTNode):
    """Constructor pattern (e.g., Cons x xs)"""
    constructor: str = ""
    sub_patterns: List[str] = field(default_factory=list)  # Pattern node IDs
    
    def __post_init__(self):
        self.node_type = NodeType.PATTERN_CONSTRUCTOR
    
    def get_dependencies(self) -> List[str]:
        return self.sub_patterns


@dataclass
class PatternList(ASTNode):
    """List pattern [x, y, ...rest]"""
    elements: List[str] = field(default_factory=list)  # Pattern node IDs
    rest_pattern: Optional[str] = None  # For ...rest patterns
    
    def __post_init__(self):
        self.node_type = NodeType.PATTERN_LIST
    
    def get_dependencies(self) -> List[str]:
        deps = self.elements.copy()
        if self.rest_pattern:
            deps.append(self.rest_pattern)
        return deps


@dataclass
class PatternWildcard(ASTNode):
    """Wildcard pattern (_)"""
    
    def __post_init__(self):
        self.node_type = NodeType.PATTERN_WILDCARD
    
    def get_dependencies(self) -> List[str]:
        return []


@dataclass
class DataDeclaration(ASTNode):
    """Algebraic Data Type declaration
    
    Example: (data Option a (None) (Some a))
    Example: (data List a (Nil) (Cons a (List a)))
    """
    type_name: str = ""
    type_params: List[str] = field(default_factory=list)  # Type variables
    constructors: List[Dict[str, Any]] = field(default_factory=list)
    # Each constructor is a dict with:
    # - "name": str (constructor name)
    # - "fields": List[TypeAnnotation] (field types)
    
    def __post_init__(self):
        self.node_type = NodeType.DATA_DECLARATION
        self.type_annotation = TypeAnnotation(
            name="Type",
            effects={EffectType.PURE}
        )
    
    def get_dependencies(self) -> List[str]:
        return []


@dataclass
class TypeAscription(ASTNode):
    """Type ascription - annotates an expression with a type
    
    Example: (: 42 Int)
    Example: (: (lambda (x) x) (Function a a))
    """
    expr_id: str = ""
    ascribed_type: Optional[TypeAnnotation] = None
    
    def __post_init__(self):
        self.node_type = NodeType.TYPE_ASCRIPTION
        # The type annotation is the ascribed type
        self.type_annotation = self.ascribed_type
    
    def get_dependencies(self) -> List[str]:
        return [self.expr_id] if self.expr_id else []