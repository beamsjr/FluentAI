"""
Test utilities for ClaudeLang tests
"""

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
from src.core.ast import ASTNode, NodeType


@dataclass
class TestNode(ASTNode):
    """Flexible test node that accepts arbitrary attributes"""
    
    # Common attributes for testing
    value: Any = None
    name: Optional[str] = None
    children: List[str] = field(default_factory=list)
    operator: Optional[str] = None
    function_id: Optional[str] = None
    argument_ids: List[str] = field(default_factory=list)
    parameter_names: List[str] = field(default_factory=list)
    body_id: Optional[str] = None
    bindings: List[Dict[str, str]] = field(default_factory=list)
    
    def __post_init__(self):
        # Node type is already set by base class default or constructor
        pass
    
    @classmethod
    def literal(cls, value: Any, **kwargs):
        """Create a literal node"""
        node = cls(node_type=NodeType.LITERAL, value=value, **kwargs)
        return node
    
    @classmethod
    def variable(cls, name: str, **kwargs):
        """Create a variable node"""
        node = cls(node_type=NodeType.VARIABLE, name=name, **kwargs)
        return node
    
    @classmethod
    def binary_op(cls, operator: str, left_id: str, right_id: str, **kwargs):
        """Create a binary operation node"""
        node = cls(
            node_type=NodeType.BINARY_OP,
            operator=operator,
            children=[left_id, right_id],
            **kwargs
        )
        return node
    
    @classmethod
    def unary_op(cls, operator: str, operand_id: str, **kwargs):
        """Create a unary operation node"""
        node = cls(
            node_type=NodeType.UNARY_OP,
            operator=operator,
            children=[operand_id],
            **kwargs
        )
        return node
    
    @classmethod
    def if_node(cls, cond_id: str, then_id: str, else_id: str, **kwargs):
        """Create an if node"""
        node = cls(
            node_type=NodeType.IF,
            children=[cond_id, then_id, else_id],
            **kwargs
        )
        return node
    
    @classmethod
    def application(cls, func_id: str, arg_ids: List[str], **kwargs):
        """Create an application node"""
        node = cls(
            node_type=NodeType.APPLICATION,
            function_id=func_id,
            argument_ids=arg_ids,
            **kwargs
        )
        return node
    
    @classmethod
    def lambda_node(cls, params: List[str], body_id: str = None, **kwargs):
        """Create a lambda node"""
        node = cls(
            node_type=NodeType.LAMBDA,
            parameter_names=params,
            body_id=body_id or "",
            **kwargs
        )
        return node
    
    @classmethod
    def let_node(cls, bindings: List[Dict[str, str]], body_id: str, **kwargs):
        """Create a let node"""
        node = cls(
            node_type=NodeType.LET,
            bindings=bindings,
            body_id=body_id,
            **kwargs
        )
        return node