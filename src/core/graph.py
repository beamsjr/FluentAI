"""
Graph-based AST representation for ClaudeLang

This module provides a graph-based representation of the Abstract Syntax Tree,
enabling powerful optimizations and transformations.
"""

from typing import Dict, List, Set, Optional, Any, Tuple
from dataclasses import dataclass, field
import uuid
from collections import defaultdict, deque

from .ast import ASTNode, NodeType


@dataclass
class Edge:
    """Represents an edge in the AST graph"""
    source: str
    target: str
    edge_type: str  # 'child', 'reference', 'dataflow', 'control'
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class Graph:
    """Graph representation of ClaudeLang AST"""
    nodes: Dict[str, ASTNode] = field(default_factory=dict)
    edges: List[Edge] = field(default_factory=list)
    root_id: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    # Caches for graph traversal
    _adjacency_list: Optional[Dict[str, List[str]]] = field(default=None, init=False)
    _reverse_adjacency: Optional[Dict[str, List[str]]] = field(default=None, init=False)
    _node_types: Optional[Dict[NodeType, Set[str]]] = field(default=None, init=False)
    
    def add_node(self, node: ASTNode) -> str:
        """Add a node to the graph and return its ID"""
        node_id = str(uuid.uuid4())
        self.nodes[node_id] = node
        self._invalidate_caches()
        return node_id
    
    def add_edge(self, source: str, target: str, edge_type: str = 'child', 
                 metadata: Optional[Dict[str, Any]] = None):
        """Add an edge between nodes"""
        edge = Edge(source, target, edge_type, metadata or {})
        self.edges.append(edge)
        self._invalidate_caches()
    
    def get_node(self, node_id: str) -> Optional[ASTNode]:
        """Get a node by ID"""
        return self.nodes.get(node_id)
    
    def get_children(self, node_id: str, edge_type: str = 'child') -> List[str]:
        """Get children of a node"""
        if self._adjacency_list is None:
            self._build_adjacency_lists()
        
        children = []
        for edge in self.edges:
            if edge.source == node_id and edge.edge_type == edge_type:
                children.append(edge.target)
        return children
    
    def get_parents(self, node_id: str, edge_type: str = 'child') -> List[str]:
        """Get parents of a node"""
        if self._reverse_adjacency is None:
            self._build_adjacency_lists()
        
        parents = []
        for edge in self.edges:
            if edge.target == node_id and edge.edge_type == edge_type:
                parents.append(edge.source)
        return parents
    
    def get_nodes_by_type(self, node_type: NodeType) -> Set[str]:
        """Get all nodes of a specific type"""
        if self._node_types is None:
            self._build_node_type_index()
        
        return self._node_types.get(node_type, set())
    
    def traverse_bfs(self, start_id: Optional[str] = None) -> List[str]:
        """Breadth-first traversal of the graph"""
        start = start_id or self.root_id
        if not start or start not in self.nodes:
            return []
        
        visited = set()
        queue = deque([start])
        result = []
        
        while queue:
            node_id = queue.popleft()
            if node_id not in visited:
                visited.add(node_id)
                result.append(node_id)
                
                # Add children to queue
                for child in self.get_children(node_id):
                    if child not in visited:
                        queue.append(child)
        
        return result
    
    def traverse_dfs(self, start_id: Optional[str] = None) -> List[str]:
        """Depth-first traversal of the graph"""
        start = start_id or self.root_id
        if not start or start not in self.nodes:
            return []
        
        visited = set()
        result = []
        
        def dfs(node_id: str):
            if node_id in visited:
                return
            
            visited.add(node_id)
            result.append(node_id)
            
            for child in self.get_children(node_id):
                dfs(child)
        
        dfs(start)
        return result
    
    def topological_sort(self) -> List[str]:
        """Perform topological sort on the graph"""
        # Calculate in-degrees
        in_degree = defaultdict(int)
        for edge in self.edges:
            in_degree[edge.target] += 1
        
        # Find nodes with no incoming edges
        queue = deque()
        for node_id in self.nodes:
            if in_degree[node_id] == 0:
                queue.append(node_id)
        
        result = []
        while queue:
            node_id = queue.popleft()
            result.append(node_id)
            
            # Update in-degrees of children
            for child in self.get_children(node_id):
                in_degree[child] -= 1
                if in_degree[child] == 0:
                    queue.append(child)
        
        # Check for cycles
        if len(result) != len(self.nodes):
            raise ValueError("Graph contains cycles")
        
        return result
    
    def find_dependencies(self, node_id: str) -> Set[str]:
        """Find all nodes that a given node depends on"""
        dependencies = set()
        
        def find_deps(current: str):
            for parent in self.get_parents(current):
                if parent not in dependencies:
                    dependencies.add(parent)
                    find_deps(parent)
        
        find_deps(node_id)
        return dependencies
    
    def find_dependents(self, node_id: str) -> Set[str]:
        """Find all nodes that depend on a given node"""
        dependents = set()
        
        def find_deps(current: str):
            for child in self.get_children(current):
                if child not in dependents:
                    dependents.add(child)
                    find_deps(child)
        
        find_deps(node_id)
        return dependents
    
    def clone(self) -> 'Graph':
        """Create a deep copy of the graph"""
        new_graph = Graph()
        
        # Clone nodes
        old_to_new = {}
        for old_id, node in self.nodes.items():
            new_id = new_graph.add_node(node)
            old_to_new[old_id] = new_id
        
        # Clone edges with updated IDs
        for edge in self.edges:
            new_graph.add_edge(
                old_to_new[edge.source],
                old_to_new[edge.target],
                edge.edge_type,
                edge.metadata.copy()
            )
        
        # Update root
        if self.root_id:
            new_graph.root_id = old_to_new[self.root_id]
        
        # Clone metadata
        new_graph.metadata = self.metadata.copy()
        
        return new_graph
    
    def subgraph(self, node_ids: Set[str]) -> 'Graph':
        """Extract a subgraph containing only specified nodes"""
        sub = Graph()
        
        # Add nodes
        old_to_new = {}
        for node_id in node_ids:
            if node_id in self.nodes:
                new_id = sub.add_node(self.nodes[node_id])
                old_to_new[node_id] = new_id
        
        # Add edges between included nodes
        for edge in self.edges:
            if edge.source in node_ids and edge.target in node_ids:
                sub.add_edge(
                    old_to_new[edge.source],
                    old_to_new[edge.target],
                    edge.edge_type,
                    edge.metadata.copy()
                )
        
        return sub
    
    def remove_node(self, node_id: str):
        """Remove a node and all connected edges"""
        if node_id in self.nodes:
            del self.nodes[node_id]
            
            # Remove edges
            self.edges = [
                edge for edge in self.edges
                if edge.source != node_id and edge.target != node_id
            ]
            
            # Update root if necessary
            if self.root_id == node_id:
                self.root_id = None
            
            self._invalidate_caches()
    
    def replace_node(self, old_id: str, new_node: ASTNode) -> str:
        """Replace a node with a new one, preserving edges"""
        if old_id not in self.nodes:
            raise KeyError(f"Node {old_id} not found")
        
        # Add new node
        new_id = self.add_node(new_node)
        
        # Update edges
        new_edges = []
        for edge in self.edges:
            if edge.source == old_id:
                new_edges.append(Edge(new_id, edge.target, edge.edge_type, edge.metadata))
            elif edge.target == old_id:
                new_edges.append(Edge(edge.source, new_id, edge.edge_type, edge.metadata))
            else:
                new_edges.append(edge)
        
        self.edges = new_edges
        
        # Remove old node
        del self.nodes[old_id]
        
        # Update root if necessary
        if self.root_id == old_id:
            self.root_id = new_id
        
        self._invalidate_caches()
        return new_id
    
    def _build_adjacency_lists(self):
        """Build adjacency list representations"""
        self._adjacency_list = defaultdict(list)
        self._reverse_adjacency = defaultdict(list)
        
        for edge in self.edges:
            self._adjacency_list[edge.source].append(edge.target)
            self._reverse_adjacency[edge.target].append(edge.source)
    
    def _build_node_type_index(self):
        """Build index of nodes by type"""
        self._node_types = defaultdict(set)
        
        for node_id, node in self.nodes.items():
            # Determine node type
            node_type = None
            for nt in NodeType:
                if type(node).__name__.upper() == nt.name:
                    node_type = nt
                    break
            
            if node_type:
                self._node_types[node_type].add(node_id)
    
    def _invalidate_caches(self):
        """Invalidate cached data structures"""
        self._adjacency_list = None
        self._reverse_adjacency = None
        self._node_types = None
    
    def to_dot(self) -> str:
        """Convert graph to Graphviz DOT format for visualization"""
        lines = ["digraph AST {"]
        lines.append("  rankdir=TB;")
        lines.append("  node [shape=box];")
        
        # Add nodes
        for node_id, node in self.nodes.items():
            label = type(node).__name__
            if hasattr(node, 'name'):
                label += f"\\n{node.name}"
            elif hasattr(node, 'value'):
                label += f"\\n{node.value}"
            
            lines.append(f'  "{node_id}" [label="{label}"];')
        
        # Add edges
        for edge in self.edges:
            style = ""
            if edge.edge_type != 'child':
                style = f' [style=dashed, label="{edge.edge_type}"]'
            lines.append(f'  "{edge.source}" -> "{edge.target}"{style};')
        
        lines.append("}")
        return "\n".join(lines)


def build_graph_from_ast(root_node: ASTNode) -> Graph:
    """Build a graph from an AST node tree"""
    graph = Graph()
    
    def add_node_recursive(node: ASTNode, parent_id: Optional[str] = None) -> str:
        """Recursively add nodes to the graph"""
        node_id = graph.add_node(node)
        
        if parent_id:
            graph.add_edge(parent_id, node_id, 'child')
        
        # Handle child nodes based on node type
        if hasattr(node, 'body_id') and node.body_id:
            # For nodes with body_id references
            child_node = getattr(node, '_body_node', None)
            if child_node:
                child_id = add_node_recursive(child_node, node_id)
                node.body_id = child_id
        
        if hasattr(node, 'argument_ids'):
            # For nodes with argument lists
            for i, arg_node in enumerate(getattr(node, '_argument_nodes', [])):
                arg_id = add_node_recursive(arg_node, node_id)
                if i < len(node.argument_ids):
                    node.argument_ids[i] = arg_id
        
        return node_id
    
    root_id = add_node_recursive(root_node)
    graph.root_id = root_id
    
    return graph