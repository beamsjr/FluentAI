"""
ClaudeLang Metaprogramming System
"""

from .graph_query import (
    GraphQuery,
    GraphTransformation,
    QueryResult,
    QueryOp,
    register_graph_query_primitives
)

__all__ = [
    'GraphQuery',
    'GraphTransformation', 
    'QueryResult',
    'QueryOp',
    'register_graph_query_primitives'
]