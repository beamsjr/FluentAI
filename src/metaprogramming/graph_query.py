"""
ClaudeLang Runtime Graph Query System

This module provides a powerful query language for introspecting and
transforming program graphs at runtime.
"""

from typing import Dict, List, Optional, Any, Callable, Set, Union
from dataclasses import dataclass, field
from enum import Enum

from ..core.ast import Graph, ASTNode, NodeType, EffectType
from ..core.primitives import PRIMITIVES


class QueryOp(Enum):
    """Graph query operations"""
    # Selection
    SELECT = "select"          # Select nodes matching criteria
    WHERE = "where"            # Filter nodes by predicate
    
    # Navigation
    CHILDREN = "children"      # Get child nodes
    PARENTS = "parents"        # Get parent nodes
    DESCENDANTS = "descendants"  # Get all descendants
    ANCESTORS = "ancestors"    # Get all ancestors
    SIBLINGS = "siblings"      # Get sibling nodes
    
    # Analysis
    PATH = "path"             # Find paths between nodes
    REACHABLE = "reachable"   # Find reachable nodes
    DOMINATES = "dominates"   # Find dominating nodes
    
    # Transformation
    TRANSFORM = "transform"    # Apply transformation
    REPLACE = "replace"       # Replace nodes
    INSERT = "insert"         # Insert nodes
    DELETE = "delete"         # Delete nodes
    
    # Aggregation
    COUNT = "count"           # Count matching nodes
    COLLECT = "collect"       # Collect values
    FOLD = "fold"            # Fold over nodes


@dataclass
class QueryResult:
    """Result of a graph query"""
    nodes: List[str]  # Node IDs
    values: List[Any] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_graph(self, original: Graph) -> Graph:
        """Create a subgraph from query results"""
        subgraph = Graph()
        
        # Copy matching nodes
        for node_id in self.nodes:
            if node_id in original.nodes:
                subgraph.nodes[node_id] = original.nodes[node_id]
        
        # Set root to first node if not specified
        if self.nodes and not subgraph.root_id:
            subgraph.root_id = self.nodes[0]
        
        return subgraph


class GraphQuery:
    """Fluent API for querying graphs"""
    
    def __init__(self, graph: Graph):
        self.graph = graph
        self.operations: List[Tuple[QueryOp, Any]] = []
        self._result_cache: Optional[QueryResult] = None
    
    def select(self, node_type: Optional[NodeType] = None, 
               **attributes) -> 'GraphQuery':
        """Select nodes by type and attributes"""
        self.operations.append((QueryOp.SELECT, {
            "node_type": node_type,
            "attributes": attributes
        }))
        self._result_cache = None
        return self
    
    def where(self, predicate: Callable[[ASTNode], bool]) -> 'GraphQuery':
        """Filter nodes by predicate"""
        self.operations.append((QueryOp.WHERE, predicate))
        self._result_cache = None
        return self
    
    def children(self) -> 'GraphQuery':
        """Get children of selected nodes"""
        self.operations.append((QueryOp.CHILDREN, None))
        self._result_cache = None
        return self
    
    def parents(self) -> 'GraphQuery':
        """Get parents of selected nodes"""
        self.operations.append((QueryOp.PARENTS, None))
        self._result_cache = None
        return self
    
    def descendants(self) -> 'GraphQuery':
        """Get all descendants of selected nodes"""
        self.operations.append((QueryOp.DESCENDANTS, None))
        self._result_cache = None
        return self
    
    def ancestors(self) -> 'GraphQuery':
        """Get all ancestors of selected nodes"""
        self.operations.append((QueryOp.ANCESTORS, None))
        self._result_cache = None
        return self
    
    def path_to(self, target_id: str) -> 'GraphQuery':
        """Find path to target node"""
        self.operations.append((QueryOp.PATH, target_id))
        self._result_cache = None
        return self
    
    def reachable_from(self, source_id: str) -> 'GraphQuery':
        """Find nodes reachable from source"""
        self.operations.append((QueryOp.REACHABLE, source_id))
        self._result_cache = None
        return self
    
    def transform(self, transformer: Callable[[ASTNode], ASTNode]) -> 'GraphQuery':
        """Transform matching nodes"""
        self.operations.append((QueryOp.TRANSFORM, transformer))
        self._result_cache = None
        return self
    
    def replace_with(self, replacement: Union[ASTNode, Callable[[ASTNode], ASTNode]]) -> 'GraphQuery':
        """Replace matching nodes"""
        self.operations.append((QueryOp.REPLACE, replacement))
        self._result_cache = None
        return self
    
    def count(self) -> int:
        """Count matching nodes"""
        result = self.execute()
        return len(result.nodes)
    
    def collect(self, extractor: Callable[[ASTNode], Any]) -> List[Any]:
        """Collect values from matching nodes"""
        result = self.execute()
        values = []
        for node_id in result.nodes:
            node = self.graph.nodes.get(node_id)
            if node:
                values.append(extractor(node))
        return values
    
    def execute(self) -> QueryResult:
        """Execute the query"""
        if self._result_cache is not None:
            return self._result_cache
        
        # Start with all nodes
        current_nodes = set(self.graph.nodes.keys())
        
        # Apply operations in sequence
        for op, arg in self.operations:
            current_nodes = self._apply_operation(op, arg, current_nodes)
        
        self._result_cache = QueryResult(nodes=list(current_nodes))
        return self._result_cache
    
    def _apply_operation(self, op: QueryOp, arg: Any, nodes: Set[str]) -> Set[str]:
        """Apply a query operation"""
        if op == QueryOp.SELECT:
            return self._select_nodes(arg["node_type"], arg["attributes"], nodes)
        
        elif op == QueryOp.WHERE:
            return self._filter_nodes(arg, nodes)
        
        elif op == QueryOp.CHILDREN:
            return self._get_children(nodes)
        
        elif op == QueryOp.PARENTS:
            return self._get_parents(nodes)
        
        elif op == QueryOp.DESCENDANTS:
            return self._get_descendants(nodes)
        
        elif op == QueryOp.ANCESTORS:
            return self._get_ancestors(nodes)
        
        elif op == QueryOp.PATH:
            return self._find_paths(nodes, arg)
        
        elif op == QueryOp.REACHABLE:
            return self._find_reachable(arg, nodes)
        
        elif op == QueryOp.TRANSFORM:
            self._transform_nodes(arg, nodes)
            return nodes
        
        elif op == QueryOp.REPLACE:
            return self._replace_nodes(arg, nodes)
        
        else:
            return nodes
    
    def _select_nodes(self, node_type: Optional[NodeType], 
                     attributes: Dict[str, Any], 
                     candidates: Set[str]) -> Set[str]:
        """Select nodes by type and attributes"""
        selected = set()
        
        for node_id in candidates:
            node = self.graph.nodes.get(node_id)
            if not node:
                continue
            
            # Check node type
            if node_type and node.node_type != node_type:
                continue
            
            # Check attributes
            match = True
            for attr, value in attributes.items():
                if not hasattr(node, attr) or getattr(node, attr) != value:
                    match = False
                    break
            
            if match:
                selected.add(node_id)
        
        return selected
    
    def _filter_nodes(self, predicate: Callable[[ASTNode], bool], 
                     nodes: Set[str]) -> Set[str]:
        """Filter nodes by predicate"""
        filtered = set()
        
        for node_id in nodes:
            node = self.graph.nodes.get(node_id)
            if node and predicate(node):
                filtered.add(node_id)
        
        return filtered
    
    def _get_children(self, nodes: Set[str]) -> Set[str]:
        """Get children of nodes"""
        children = set()
        
        for node_id in nodes:
            node = self.graph.nodes.get(node_id)
            if node:
                children.update(node.get_dependencies())
        
        return children
    
    def _get_parents(self, nodes: Set[str]) -> Set[str]:
        """Get parents of nodes"""
        parents = set()
        
        # Build parent map
        parent_map = self._build_parent_map()
        
        for node_id in nodes:
            if node_id in parent_map:
                parents.update(parent_map[node_id])
        
        return parents
    
    def _get_descendants(self, nodes: Set[str]) -> Set[str]:
        """Get all descendants of nodes"""
        descendants = set()
        to_visit = list(nodes)
        
        while to_visit:
            node_id = to_visit.pop()
            if node_id in descendants:
                continue
            
            descendants.add(node_id)
            node = self.graph.nodes.get(node_id)
            if node:
                to_visit.extend(node.get_dependencies())
        
        return descendants - nodes  # Exclude starting nodes
    
    def _get_ancestors(self, nodes: Set[str]) -> Set[str]:
        """Get all ancestors of nodes"""
        ancestors = set()
        parent_map = self._build_parent_map()
        to_visit = list(nodes)
        
        while to_visit:
            node_id = to_visit.pop()
            if node_id in ancestors:
                continue
            
            ancestors.add(node_id)
            if node_id in parent_map:
                to_visit.extend(parent_map[node_id])
        
        return ancestors - nodes  # Exclude starting nodes
    
    def _find_paths(self, sources: Set[str], target: str) -> Set[str]:
        """Find nodes on paths from sources to target"""
        paths_nodes = set()
        
        for source in sources:
            path = self._find_path(source, target)
            if path:
                paths_nodes.update(path)
        
        return paths_nodes
    
    def _find_path(self, source: str, target: str) -> Optional[List[str]]:
        """Find a path between two nodes using BFS"""
        if source == target:
            return [source]
        
        queue = [(source, [source])]
        visited = {source}
        
        while queue:
            node_id, path = queue.pop(0)
            node = self.graph.nodes.get(node_id)
            
            if not node:
                continue
            
            for child_id in node.get_dependencies():
                if child_id in visited:
                    continue
                
                new_path = path + [child_id]
                
                if child_id == target:
                    return new_path
                
                queue.append((child_id, new_path))
                visited.add(child_id)
        
        return None
    
    def _find_reachable(self, source: str, nodes: Set[str]) -> Set[str]:
        """Find nodes reachable from source"""
        reachable = set()
        to_visit = [source]
        
        while to_visit:
            node_id = to_visit.pop()
            if node_id in reachable:
                continue
            
            reachable.add(node_id)
            node = self.graph.nodes.get(node_id)
            
            if node:
                to_visit.extend(node.get_dependencies())
        
        return reachable.intersection(nodes)
    
    def _transform_nodes(self, transformer: Callable[[ASTNode], ASTNode], 
                        nodes: Set[str]):
        """Transform nodes in place"""
        for node_id in nodes:
            if node_id in self.graph.nodes:
                old_node = self.graph.nodes[node_id]
                new_node = transformer(old_node)
                self.graph.nodes[node_id] = new_node
    
    def _replace_nodes(self, replacement: Union[ASTNode, Callable], 
                      nodes: Set[str]) -> Set[str]:
        """Replace nodes and return new node IDs"""
        new_nodes = set()
        
        for node_id in nodes:
            if node_id in self.graph.nodes:
                old_node = self.graph.nodes[node_id]
                
                if callable(replacement):
                    new_node = replacement(old_node)
                else:
                    new_node = replacement
                
                # Add new node
                new_id = self.graph.add_node(new_node)
                new_nodes.add(new_id)
                
                # Update references
                self._update_references(node_id, new_id)
                
                # Remove old node
                del self.graph.nodes[node_id]
        
        return new_nodes
    
    def _update_references(self, old_id: str, new_id: str):
        """Update references from old node to new node"""
        for node in self.graph.nodes.values():
            # Update children references
            if hasattr(node, 'children'):
                node.children = [new_id if c == old_id else c for c in node.children]
            
            # Update other reference fields
            for attr in ['function_id', 'argument_id', 'condition_id', 
                        'then_id', 'else_id', 'body_id', 'value_id']:
                if hasattr(node, attr) and getattr(node, attr) == old_id:
                    setattr(node, attr, new_id)
    
    def _build_parent_map(self) -> Dict[str, Set[str]]:
        """Build a map from nodes to their parents"""
        parent_map = {}
        
        for parent_id, parent_node in self.graph.nodes.items():
            for child_id in parent_node.get_dependencies():
                if child_id not in parent_map:
                    parent_map[child_id] = set()
                parent_map[child_id].add(parent_id)
        
        return parent_map


class GraphTransformation:
    """High-level graph transformation patterns"""
    
    @staticmethod
    def inline_function(graph: Graph, call_node_id: str) -> bool:
        """Inline a function call"""
        query = GraphQuery(graph)
        
        # Get the call node
        call_nodes = query.select().where(
            lambda n: n.node_id == call_node_id
        ).execute()
        
        if not call_nodes.nodes:
            return False
        
        # Implementation would inline the function
        return True
    
    @staticmethod
    def fuse_operations(graph: Graph, op1_id: str, op2_id: str) -> Optional[str]:
        """Fuse two operations into one"""
        # Check if operations can be fused
        query = GraphQuery(graph)
        
        # Check if op2 depends on op1
        path = query.select().where(
            lambda n: n.node_id == op1_id
        ).path_to(op2_id).execute()
        
        if path.nodes:
            # Create fused operation
            # Implementation would create new fused node
            return "fused_node_id"
        
        return None
    
    @staticmethod
    def eliminate_dead_code(graph: Graph) -> int:
        """Remove unreachable code"""
        query = GraphQuery(graph)
        
        # Find all nodes reachable from root
        if not graph.root_id:
            return 0
        
        reachable = query.reachable_from(graph.root_id).execute()
        reachable_set = set(reachable.nodes)
        
        # Delete unreachable nodes
        deleted = 0
        for node_id in list(graph.nodes.keys()):
            if node_id not in reachable_set:
                del graph.nodes[node_id]
                deleted += 1
        
        return deleted
    
    @staticmethod
    def extract_common_subexpressions(graph: Graph) -> int:
        """Extract common subexpressions"""
        # Find duplicate subgraphs
        extracted = 0
        
        # Implementation would identify and extract common patterns
        
        return extracted


# Register graph query functions as primitives
def register_graph_query_primitives():
    """Register graph query functions in the standard library"""
    
    PRIMITIVES.register(
        "graph:query",
        lambda g: GraphQuery(g),
        arity=1,
        effects={EffectType.PURE},
        doc="Create a graph query object"
    )
    
    PRIMITIVES.register(
        "graph:select",
        lambda q, node_type: q.select(node_type),
        arity=2,
        effects={EffectType.PURE},
        doc="Select nodes by type"
    )
    
    PRIMITIVES.register(
        "graph:transform",
        lambda g, transformer: GraphTransformation.transform(g, transformer),
        arity=2,
        effects={EffectType.STATE},
        doc="Transform a graph"
    )
    
    PRIMITIVES.register(
        "graph:optimize",
        lambda g: GraphTransformation.eliminate_dead_code(g),
        arity=1,
        effects={EffectType.STATE},
        doc="Optimize a graph"
    )