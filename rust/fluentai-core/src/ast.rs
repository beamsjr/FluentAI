//! AST representation using an efficient graph structure

use crate::documentation::{
    Documentation, DocumentationCategory, DocumentationVisibility, DocumentedNode,
};
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::num::NonZeroU32;

/// Type alias for the HashMap implementation used in the AST
/// This allows easy switching between different hash map implementations
pub type AstHashMap<K, V> = FxHashMap<K, V>;

/// Type alias for the HashSet implementation used in the AST
/// This allows easy switching between different hash set implementations
pub type AstHashSet<T> = FxHashSet<T>;

/// Node identifier in the AST graph
///
/// Uses NonZeroU32 internally to enable null pointer optimization for Option<NodeId>.
/// NodeId(0) is reserved as an invalid/null node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NodeId(pub NonZeroU32);

impl NodeId {
    /// Creates a new NodeId from a u32.
    /// Returns None if the value is 0.
    pub fn new(value: u32) -> Option<Self> {
        NonZeroU32::new(value).map(NodeId)
    }

    /// Gets the inner u32 value
    pub fn get(&self) -> u32 {
        self.0.get()
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "n{}", self.0)
    }
}

/// Node metadata for analysis and optimization
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct NodeMetadata {
    /// Source location information
    pub span: Option<(usize, usize)>,
    /// Inferred type information
    pub type_info: Option<String>,
    /// Purity flag for optimization
    pub is_pure: Option<bool>,
    /// Custom annotations
    pub annotations: Vec<String>,
    /// Link to documentation
    pub documentation_id: Option<String>,
    /// Context memory for machine understanding
    pub context_memory: Option<ContextMemory>,
}

/// Context memory for machine-readable semantic information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextMemory {
    /// Semantic embedding vector
    pub embedding_id: Option<EmbeddingId>,
    /// Usage statistics from runtime
    pub usage_stats: UsageStatistics,
    /// Design rationale - why this code exists
    pub rationale: Option<String>,
    /// Performance characteristics
    pub performance_hints: Vec<PerformanceHint>,
    /// Semantic tags for categorization
    pub semantic_tags: Vec<String>,
    /// Last modification timestamp
    pub last_modified: Option<u64>,
}

/// Identifier for external embedding storage
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EmbeddingId(pub u64);

/// Runtime usage statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct UsageStatistics {
    /// Number of times this node was executed
    pub execution_count: u64,
    /// Average execution time in nanoseconds
    pub avg_execution_time_ns: u64,
    /// Number of times this node caused an error
    pub error_count: u64,
    /// Hot path indicator (frequently executed)
    pub is_hot_path: bool,
}

/// Performance hint for optimization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceHint {
    /// Type of hint
    pub hint_type: PerformanceHintType,
    /// Confidence level (0.0 - 1.0)
    pub confidence: f32,
    /// Additional context
    pub context: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PerformanceHintType {
    /// This function should be inlined
    ShouldInline,
    /// This loop should be unrolled
    ShouldUnroll {
        /// Optional unroll factor (None means complete unrolling)
        factor: Option<usize>,
    },
    /// This operation can be vectorized
    CanVectorize {
        /// SIMD width hint (e.g., 128, 256, 512 bits)
        simd_width: Option<u32>,
    },
    /// This computation can be parallelized
    CanParallelize {
        /// Suggested parallelism strategy
        strategy: ParallelismStrategy,
    },
    /// Results should be memoized
    ShouldMemoize {
        /// Maximum cache size hint
        max_cache_size: Option<usize>,
    },
    /// Memory access pattern hint
    MemoryAccessPattern(MemoryPattern),
    /// Custom hint
    Custom(String),
}

/// Memory access patterns for optimization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MemoryPattern {
    /// Sequential access pattern
    Sequential,
    /// Strided access with given stride
    Strided(usize),
    /// Random access pattern
    Random,
    /// Streaming (no reuse)
    Streaming,
    /// Spatial locality (nearby elements accessed together)
    SpatialLocality,
    /// Temporal locality (same elements reused)
    TemporalLocality,
}

/// Parallelism strategies
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ParallelismStrategy {
    /// Data parallelism (same operation on different data)
    DataParallel,
    /// Task parallelism (different operations in parallel)
    TaskParallel,
    /// Pipeline parallelism (stages of computation)
    Pipeline,
    /// Map-reduce pattern
    MapReduce,
    /// Fork-join pattern
    ForkJoin,
}

/// AST graph representation
///
/// # Invariants
/// - `next_id` monotonically increases and is never reused
/// - NodeIds are unique within a graph
/// - All NodeId references in nodes must point to valid nodes in the graph
/// - The root_id, if present, must point to a valid node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Graph {
    pub nodes: AstHashMap<NodeId, Node>,
    pub root_id: Option<NodeId>,
    /// Next ID to assign. Starts at 1 and monotonically increases.
    /// IDs are never reused, even after node deletion.
    next_id: u32,
    /// Optional metadata for nodes
    pub metadata: AstHashMap<NodeId, NodeMetadata>,
}

impl Default for Graph {
    fn default() -> Self {
        Self::new()
    }
}

impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: AstHashMap::default(),
            root_id: None,
            next_id: 1, // Start at 1 since 0 is reserved for null
            metadata: AstHashMap::default(),
        }
    }

    pub fn add_node(&mut self, node: Node) -> crate::error::Result<NodeId> {
        // Check for overflow before incrementing
        if self.next_id == u32::MAX {
            return Err(crate::error::Error::GraphNodeIdOverflow);
        }

        // SAFETY: next_id starts at 1 and only increments, so it's always non-zero
        let id = NodeId(NonZeroU32::new(self.next_id).unwrap());
        self.next_id += 1;
        self.nodes.insert(id, node);
        Ok(id)
    }

    pub fn get_node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(&id)
    }

    /// Gets mutable reference to a node
    pub fn get_node_mut(&mut self, id: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(&id)
    }

    /// Gets metadata for a node
    pub fn get_metadata(&self, id: NodeId) -> Option<&NodeMetadata> {
        self.metadata.get(&id)
    }

    /// Gets or creates metadata for a node
    pub fn metadata_mut(&mut self, id: NodeId) -> &mut NodeMetadata {
        self.metadata.entry(id).or_default()
    }

    /// Sets metadata for a node
    pub fn set_metadata(&mut self, id: NodeId, metadata: NodeMetadata) {
        self.metadata.insert(id, metadata);
    }

    /// Sets documentation ID for a node
    pub fn set_documentation(&mut self, id: NodeId, doc_id: String) {
        self.metadata_mut(id).documentation_id = Some(doc_id);
    }

    /// Gets documentation ID for a node
    pub fn get_documentation(&self, id: NodeId) -> Option<&String> {
        self.metadata.get(&id)?.documentation_id.as_ref()
    }

    /// Sets or updates context memory for a node
    pub fn set_context_memory(&mut self, id: NodeId, context: ContextMemory) {
        self.metadata_mut(id).context_memory = Some(context);
    }

    /// Gets context memory for a node
    pub fn get_context_memory(&self, id: NodeId) -> Option<&ContextMemory> {
        self.metadata.get(&id)?.context_memory.as_ref()
    }

    /// Updates usage statistics for a node
    pub fn update_usage_stats<F>(&mut self, id: NodeId, updater: F)
    where
        F: FnOnce(&mut UsageStatistics),
    {
        let metadata = self.metadata_mut(id);
        if metadata.context_memory.is_none() {
            metadata.context_memory = Some(ContextMemory {
                embedding_id: None,
                usage_stats: UsageStatistics::default(),
                rationale: None,
                performance_hints: Vec::new(),
                semantic_tags: Vec::new(),
                last_modified: None,
            });
        }
        if let Some(context) = &mut metadata.context_memory {
            updater(&mut context.usage_stats);
        }
    }

    /// Returns an iterator over all node IDs in the graph
    pub fn node_ids(&self) -> impl Iterator<Item = NodeId> + '_ {
        self.nodes.keys().copied()
    }

    /// Returns an iterator over all nodes in the graph
    pub fn nodes(&self) -> impl Iterator<Item = (&NodeId, &Node)> + '_ {
        self.nodes.iter()
    }

    /// Performs a depth-first traversal starting from the given node
    /// Uses iterative implementation to avoid stack overflow on deep graphs
    pub fn dfs_from(&self, start: NodeId, mut visitor: impl FnMut(NodeId, &Node)) {
        self.dfs_iterative(start, &mut visitor);
    }

    /// Performs an iterative depth-first traversal to avoid stack overflow
    pub fn dfs_iterative(&self, start: NodeId, visitor: &mut impl FnMut(NodeId, &Node)) {
        let mut visited = AstHashSet::default();
        let mut stack = vec![start];

        while let Some(node_id) = stack.pop() {
            if !visited.insert(node_id) {
                continue; // Already visited
            }

            if let Some(node) = self.get_node(node_id) {
                visitor(node_id, node);

                // Push children onto stack in reverse order for correct DFS order
                match node {
                    Node::Lambda { body, .. } => {
                        stack.push(*body);
                    }
                    Node::Application { function, args } => {
                        // Push in reverse order so they're processed in correct order
                        for arg in args.iter().rev() {
                            stack.push(*arg);
                        }
                        stack.push(*function);
                    }
                    Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                        stack.push(*body);
                        for (_, value) in bindings.iter().rev() {
                            stack.push(*value);
                        }
                    }
                    Node::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
                        stack.push(*else_branch);
                        stack.push(*then_branch);
                        stack.push(*condition);
                    }
                    Node::Match { expr, branches } => {
                        for (_, branch) in branches.iter().rev() {
                            stack.push(*branch);
                        }
                        stack.push(*expr);
                    }
                    Node::List(items) => {
                        for item in items.iter().rev() {
                            stack.push(*item);
                        }
                    }
                    Node::Effect { args, .. } => {
                        for arg in args.iter().rev() {
                            stack.push(*arg);
                        }
                    }
                    Node::Handler { handlers, body } => {
                        stack.push(*body);
                        // Push handler functions in reverse order
                        for (_, _, handler_fn) in handlers.iter().rev() {
                            stack.push(*handler_fn);
                        }
                    }
                    Node::Send { channel, value } => {
                        stack.push(*value);
                        stack.push(*channel);
                    }
                    Node::Receive { channel } => {
                        stack.push(*channel);
                    }
                    Node::TrySend { channel, value } => {
                        stack.push(*value);
                        stack.push(*channel);
                    }
                    Node::TryReceive { channel } => {
                        stack.push(*channel);
                    }
                    Node::Select { branches, default } => {
                        for (channel_op, handler) in branches.iter().rev() {
                            stack.push(*handler);
                            stack.push(*channel_op);
                        }
                        if let Some(def) = default {
                            stack.push(*def);
                        }
                    }
                    Node::Actor { initial_state, handler } => {
                        stack.push(*handler);
                        stack.push(*initial_state);
                    }
                    Node::ActorSend { actor, message } => {
                        stack.push(*message);
                        stack.push(*actor);
                    }
                    Node::ActorReceive { patterns, timeout } => {
                        for (_, handler) in patterns.iter().rev() {
                            stack.push(*handler);
                        }
                        if let Some((duration, handler)) = timeout {
                            stack.push(*handler);
                            stack.push(*duration);
                        }
                    }
                    Node::Become { new_state } => {
                        stack.push(*new_state);
                    }
                    Node::Try { body, catch_branches, finally } => {
                        stack.push(*body);
                        for (_, handler) in catch_branches.iter().rev() {
                            stack.push(*handler);
                        }
                        if let Some(finally_block) = finally {
                            stack.push(*finally_block);
                        }
                    }
                    Node::Throw { error } => {
                        stack.push(*error);
                    }
                    Node::Promise { body } => {
                        stack.push(*body);
                    }
                    Node::PromiseAll { promises } | Node::PromiseRace { promises } => {
                        for promise in promises.iter().rev() {
                            stack.push(*promise);
                        }
                    }
                    Node::Timeout { duration, promise, default } => {
                        stack.push(*duration);
                        stack.push(*promise);
                        if let Some(def) = default {
                            stack.push(*def);
                        }
                    }
                    Node::Channel { capacity } => {
                        if let Some(cap) = capacity {
                            stack.push(*cap);
                        }
                    }
                    Node::Async { body } => {
                        stack.push(*body);
                    }
                    Node::Await { expr } | Node::Spawn { expr } => {
                        stack.push(*expr);
                    }
                    Node::Module { body, .. } => {
                        stack.push(*body);
                    }
                    Node::Begin { exprs } => {
                        // Push in reverse order so they're visited in correct order
                        for expr in exprs.iter().rev() {
                            stack.push(*expr);
                        }
                    }
                    Node::Assignment { target, value } => {
                        stack.push(*target);
                        stack.push(*value);
                    }
                    _ => {} // Leaf nodes
                }
            }
        }
    }

    /// Performs a recursive depth-first traversal (legacy method, may overflow on deep graphs)
    /// Consider using dfs_from() or dfs_iterative() instead
    pub fn dfs_recursive(&self, start: NodeId, mut visitor: impl FnMut(NodeId, &Node)) {
        let mut visited = AstHashSet::default();
        self.dfs_helper(start, &mut visited, &mut visitor);
    }

    fn dfs_helper(
        &self,
        node_id: NodeId,
        visited: &mut AstHashSet<NodeId>,
        visitor: &mut impl FnMut(NodeId, &Node),
    ) {
        if !visited.insert(node_id) {
            return; // Already visited
        }

        if let Some(node) = self.get_node(node_id) {
            visitor(node_id, node);

            // Visit child nodes
            match node {
                Node::Lambda { body, .. } => {
                    self.dfs_helper(*body, visited, visitor);
                }
                Node::Application { function, args } => {
                    self.dfs_helper(*function, visited, visitor);
                    for arg in args {
                        self.dfs_helper(*arg, visited, visitor);
                    }
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    for (_, value) in bindings {
                        self.dfs_helper(*value, visited, visitor);
                    }
                    self.dfs_helper(*body, visited, visitor);
                }
                Node::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    self.dfs_helper(*condition, visited, visitor);
                    self.dfs_helper(*then_branch, visited, visitor);
                    self.dfs_helper(*else_branch, visited, visitor);
                }
                Node::Match { expr, branches } => {
                    self.dfs_helper(*expr, visited, visitor);
                    for (_, branch) in branches {
                        self.dfs_helper(*branch, visited, visitor);
                    }
                }
                Node::List(items) => {
                    for item in items {
                        self.dfs_helper(*item, visited, visitor);
                    }
                }
                Node::Effect { args, .. } => {
                    for arg in args {
                        self.dfs_helper(*arg, visited, visitor);
                    }
                }
                Node::Handler { handlers, body } => {
                    self.dfs_helper(*body, visited, visitor);
                    for (_, _, handler_fn) in handlers {
                        self.dfs_helper(*handler_fn, visited, visitor);
                    }
                }
                Node::Begin { exprs } => {
                    for expr in exprs {
                        self.dfs_helper(*expr, visited, visitor);
                    }
                }
                Node::Assignment { target, value } => {
                    self.dfs_helper(*target, visited, visitor);
                    self.dfs_helper(*value, visited, visitor);
                }
                _ => {} // Leaf nodes
            }
        }
    }

    /// Collects all child node IDs of a given node
    pub fn children(&self, node_id: NodeId) -> Vec<NodeId> {
        let mut children = Vec::new();
        if let Some(node) = self.get_node(node_id) {
            match node {
                Node::Lambda { body, .. } => {
                    children.push(*body);
                }
                Node::Application { function, args } => {
                    children.push(*function);
                    children.extend(args);
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    children.extend(bindings.iter().map(|(_, v)| v));
                    children.push(*body);
                }
                Node::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    children.push(*condition);
                    children.push(*then_branch);
                    children.push(*else_branch);
                }
                Node::Match { expr, branches } => {
                    children.push(*expr);
                    children.extend(branches.iter().map(|(_, b)| b));
                }
                Node::List(items) => {
                    children.extend(items);
                }
                Node::Effect { args, .. } => {
                    children.extend(args);
                }
                Node::Handler { handlers, body } => {
                    children.push(*body);
                    children.extend(handlers.iter().map(|(_, _, handler_fn)| handler_fn));
                }
                Node::Send { channel, value } => {
                    children.push(*channel);
                    children.push(*value);
                }
                Node::Receive { channel } => {
                    children.push(*channel);
                }
                Node::TrySend { channel, value } => {
                    children.push(*channel);
                    children.push(*value);
                }
                Node::TryReceive { channel } => {
                    children.push(*channel);
                }
                Node::Select { branches, default } => {
                    for (channel_op, handler) in branches {
                        children.push(*channel_op);
                        children.push(*handler);
                    }
                    if let Some(def) = default {
                        children.push(*def);
                    }
                }
                Node::Actor { initial_state, handler } => {
                    children.push(*initial_state);
                    children.push(*handler);
                }
                Node::ActorSend { actor, message } => {
                    children.push(*actor);
                    children.push(*message);
                }
                Node::ActorReceive { patterns, timeout } => {
                    for (_, handler) in patterns {
                        children.push(*handler);
                    }
                    if let Some((duration, handler)) = timeout {
                        children.push(*duration);
                        children.push(*handler);
                    }
                }
                Node::Become { new_state } => {
                    children.push(*new_state);
                }
                Node::Try { body, catch_branches, finally } => {
                    children.push(*body);
                    for (_, handler) in catch_branches {
                        children.push(*handler);
                    }
                    if let Some(finally_block) = finally {
                        children.push(*finally_block);
                    }
                }
                Node::Throw { error } => {
                    children.push(*error);
                }
                Node::Promise { body } => {
                    children.push(*body);
                }
                Node::PromiseAll { promises } | Node::PromiseRace { promises } => {
                    for promise in promises {
                        children.push(*promise);
                    }
                }
                Node::Timeout { duration, promise, default } => {
                    children.push(*duration);
                    children.push(*promise);
                    if let Some(def) = default {
                        children.push(*def);
                    }
                }
                Node::Async { body } => {
                    children.push(*body);
                }
                Node::Await { expr } | Node::Spawn { expr } => {
                    children.push(*expr);
                }
                Node::Module { body, .. } => {
                    children.push(*body);
                }
                Node::Begin { exprs } => {
                    children.extend(exprs);
                }
                Node::Assignment { target, value } => {
                    children.push(*target);
                    children.push(*value);
                }
                _ => {} // Leaf nodes have no children
            }
        }
        children
    }

    /// Validates the graph structure and checks all invariants
    ///
    /// # Invariants checked:
    /// - All NodeId references point to existing nodes
    /// - The root_id (if present) points to a valid node
    /// - No orphaned nodes (all nodes reachable from root or metadata)
    /// - No cycles in non-recursive contexts
    /// - Node IDs are within expected range
    pub fn validate(&self) -> crate::error::Result<()> {
        // Check if root exists when specified
        if let Some(root) = self.root_id {
            if !self.nodes.contains_key(&root) {
                return Err(crate::error::Error::Other(anyhow::anyhow!(
                    "Root node {} does not exist in graph",
                    root
                )));
            }
        }

        // Collect all referenced node IDs
        let mut referenced_nodes = AstHashSet::default();

        // Check all nodes and their references
        for &node_id in self.nodes.keys() {
            // Check node ID is within valid range
            if node_id.get() == 0 || node_id.get() >= self.next_id {
                return Err(crate::error::Error::Other(anyhow::anyhow!(
                    "Invalid node ID {}: outside valid range 1..{}",
                    node_id,
                    self.next_id
                )));
            }

            // Get all child references from this node
            let children = self.children(node_id);
            for child_id in children {
                // Check that referenced nodes exist
                if !self.nodes.contains_key(&child_id) {
                    return Err(crate::error::Error::Other(anyhow::anyhow!(
                        "Node {} references non-existent node {}",
                        node_id,
                        child_id
                    )));
                }
                referenced_nodes.insert(child_id);
            }
        }

        // Add root to referenced nodes if it exists
        if let Some(root) = self.root_id {
            referenced_nodes.insert(root);
        }

        // Check for orphaned nodes (optional - may be too strict for some use cases)
        let all_nodes: AstHashSet<_> = self.nodes.keys().copied().collect();
        let orphaned: Vec<_> = all_nodes.difference(&referenced_nodes).collect();

        // Allow orphaned nodes if they have metadata (they might be referenced externally)
        for &orphan in &orphaned {
            if !self.metadata.contains_key(orphan) && Some(*orphan) != self.root_id {
                // This is a warning, not an error - some use cases may have intentionally orphaned nodes
                // You could make this stricter if needed
                eprintln!(
                    "Warning: Node {orphan} is orphaned (not reachable from root)"
                );
            }
        }

        // Validate metadata references
        for meta_id in self.metadata.keys() {
            if !self.nodes.contains_key(meta_id) {
                return Err(crate::error::Error::Other(anyhow::anyhow!(
                    "Metadata exists for non-existent node {}",
                    meta_id
                )));
            }
        }

        // Check for cycles (simplified check - full cycle detection would be more complex)
        if let Some(root) = self.root_id {
            let mut visited = AstHashSet::default();
            let mut stack = AstHashSet::default();
            if self.has_cycle_from(root, &mut visited, &mut stack) {
                return Err(crate::error::Error::Other(anyhow::anyhow!(
                    "Graph contains a cycle"
                )));
            }
        }

        Ok(())
    }

    /// Helper method to detect cycles using DFS
    fn has_cycle_from(
        &self,
        node_id: NodeId,
        visited: &mut AstHashSet<NodeId>,
        stack: &mut AstHashSet<NodeId>,
    ) -> bool {
        if stack.contains(&node_id) {
            return true; // Found a cycle
        }

        if visited.contains(&node_id) {
            return false; // Already processed this node
        }

        visited.insert(node_id);
        stack.insert(node_id);

        // Check children
        for child in self.children(node_id) {
            if self.has_cycle_from(child, visited, stack) {
                return true;
            }
        }

        stack.remove(&node_id);
        false
    }

    /// Creates a new match builder for constructing pattern matching expressions
    pub fn build_match(&mut self) -> MatchBuilder {
        MatchBuilder::new(self)
    }

    /// Helper for list pattern matching
    pub fn match_list<F>(
        &mut self,
        list: NodeId,
        on_empty: NodeId,
        on_cons: F,
    ) -> crate::error::Result<NodeId>
    where
        F: FnOnce(&mut Self, NodeId, NodeId) -> crate::error::Result<NodeId>,
    {
        let head_var = self.add_node(Node::Variable {
            name: "head".to_string(),
        })?;
        let tail_var = self.add_node(Node::Variable {
            name: "tail".to_string(),
        })?;
        let cons_result = on_cons(self, head_var, tail_var)?;

        self.add_node(Node::Match {
            expr: list,
            branches: vec![
                (Pattern::nil(), on_empty),
                (
                    Pattern::cons(Pattern::var("head"), Pattern::var("tail")),
                    cons_result,
                ),
            ],
        })
    }

    /// Helper for simple value matching with literal patterns
    pub fn match_value(
        &mut self,
        value: NodeId,
        cases: Vec<(Literal, NodeId)>,
        default: NodeId,
    ) -> crate::error::Result<NodeId> {
        let mut branches: Vec<(Pattern, NodeId)> = cases
            .into_iter()
            .map(|(lit, result)| (Pattern::lit(lit), result))
            .collect();
        branches.push((Pattern::wildcard(), default));

        self.add_node(Node::Match {
            expr: value,
            branches,
        })
    }
}

/// AST node types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Node {
    // Literals
    Literal(Literal),

    // Variables and bindings
    Variable {
        name: String,
    },
    Lambda {
        params: Vec<String>,
        body: NodeId,
    },
    Let {
        bindings: Vec<(String, NodeId)>,
        body: NodeId,
    },
    Letrec {
        bindings: Vec<(String, NodeId)>,
        body: NodeId,
    },

    // Control flow
    If {
        condition: NodeId,
        then_branch: NodeId,
        else_branch: NodeId,
    },

    // Function application
    Application {
        function: NodeId,
        args: Vec<NodeId>,
    },

    // Effects
    Effect {
        effect_type: EffectType,
        operation: String,
        args: Vec<NodeId>,
    },

    // Effect handlers
    Handler {
        /// List of handlers: (effect_type, optional operation filter, handler function)
        handlers: Vec<(EffectType, Option<String>, NodeId)>,
        body: NodeId,
    },

    // Data structures
    List(Vec<NodeId>),

    // Pattern matching
    Match {
        expr: NodeId,
        branches: Vec<(Pattern, NodeId)>,
    },

    // Module system
    Module {
        name: String,
        exports: Vec<String>,
        body: NodeId,
    },
    Import {
        module_path: String,
        import_list: Vec<ImportItem>,
        import_all: bool,
    },
    Export {
        export_list: Vec<ExportItem>,
    },
    QualifiedVariable {
        module_name: String,
        variable_name: String,
    },

    // Top-level definitions
    Define {
        name: String,
        value: NodeId,
    },

    // Assignment statement
    Assignment {
        target: NodeId,  // The lvalue to assign to
        value: NodeId,   // The rvalue expression
    },

    // Sequencing
    Begin {
        exprs: Vec<NodeId>,
    },

    // Async/concurrent constructs
    Async {
        body: NodeId,
    },
    Await {
        expr: NodeId,
    },
    Spawn {
        expr: NodeId,
    },
    Channel {
        capacity: Option<NodeId>, // Optional capacity expression
    },
    Send {
        channel: NodeId,
        value: NodeId,
    },
    Receive {
        channel: NodeId,
    },
    TrySend {
        channel: NodeId,
        value: NodeId,
    },
    TryReceive {
        channel: NodeId,
    },
    Select {
        branches: Vec<(NodeId, NodeId)>, // (channel_op, handler)
        default: Option<NodeId>,         // optional default branch
    },
    
    // Actor model primitives
    Actor {
        initial_state: NodeId,
        handler: NodeId, // Lambda that handles messages
    },
    ActorSend {
        actor: NodeId,
        message: NodeId,
    },
    ActorReceive {
        patterns: Vec<(Pattern, NodeId)>, // Pattern matching for messages
        timeout: Option<(NodeId, NodeId)>, // Optional timeout (duration, handler)
    },
    Become {
        new_state: NodeId, // New state for the actor
    },
    
    // Error handling
    Try {
        body: NodeId,
        catch_branches: Vec<(Pattern, NodeId)>, // Pattern match on error types
        finally: Option<NodeId>, // Optional finally block
    },
    Throw {
        error: NodeId, // Error value to throw
    },
    Promise {
        body: NodeId, // Body that returns a promise
    },
    PromiseAll {
        promises: Vec<NodeId>, // List of promises to wait for
    },
    PromiseRace {
        promises: Vec<NodeId>, // List of promises to race
    },
    Timeout {
        duration: NodeId, // Timeout duration in milliseconds
        promise: NodeId,  // Promise to timeout
        default: Option<NodeId>, // Optional default value on timeout
    },

    // Contract specification
    Contract {
        function_name: String,
        preconditions: Vec<NodeId>,
        postconditions: Vec<NodeId>,
        invariants: Vec<NodeId>,
        complexity: Option<String>,
        pure: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Symbol(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Float(fl) => write!(f, "{fl}"),
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Symbol(s) => write!(f, "{s}"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RangePattern {
    pub start: Literal,
    pub end: Literal,
    pub inclusive: bool, // true for ..=, false for ..
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pattern {
    Variable(String),
    Literal(Literal),
    Constructor {
        name: String,
        patterns: Vec<Pattern>,
    },
    Wildcard,

    // Complex pattern extensions
    /// Pattern with a guard condition: pattern when condition
    Guard {
        pattern: Box<Pattern>,
        condition: NodeId,
    },
    /// As-pattern: binds the whole match while also destructuring
    As {
        binding: String,
        pattern: Box<Pattern>,
    },
    /// Or-pattern: multiple patterns that lead to the same branch
    Or(Vec<Pattern>),
    /// Range pattern: matches values within a range
    Range(RangePattern),
    /// View pattern: applies a function before matching
    View {
        function: NodeId,
        pattern: Box<Pattern>,
    },
}

impl Pattern {
    /// Creates a variable pattern that binds the matched value to a name
    pub fn var(name: &str) -> Self {
        Pattern::Variable(name.to_string())
    }

    /// Creates a literal pattern that matches an exact value
    pub fn lit(literal: Literal) -> Self {
        Pattern::Literal(literal)
    }

    /// Creates an integer literal pattern
    pub fn int(value: i64) -> Self {
        Pattern::Literal(Literal::Integer(value))
    }

    /// Creates a string literal pattern
    pub fn string(value: &str) -> Self {
        Pattern::Literal(Literal::String(value.to_string()))
    }

    /// Creates a boolean literal pattern
    pub fn bool(value: bool) -> Self {
        Pattern::Literal(Literal::Boolean(value))
    }

    /// Creates a nil literal pattern
    pub fn nil_lit() -> Self {
        Pattern::Literal(Literal::Nil)
    }

    /// Creates a cons pattern for matching non-empty lists
    pub fn cons(head: Pattern, tail: Pattern) -> Self {
        Pattern::Constructor {
            name: "cons".to_string(),
            patterns: vec![head, tail],
        }
    }

    /// Creates a nil pattern for matching empty lists
    pub fn nil() -> Self {
        Pattern::Constructor {
            name: "nil".to_string(),
            patterns: vec![],
        }
    }

    /// Creates a custom constructor pattern
    pub fn constructor(name: &str, patterns: Vec<Pattern>) -> Self {
        Pattern::Constructor {
            name: name.to_string(),
            patterns,
        }
    }

    /// Creates a wildcard pattern that matches anything
    pub fn wildcard() -> Self {
        Pattern::Wildcard
    }

    /// Alias for wildcard pattern using underscore convention
    pub fn underscore() -> Self {
        Pattern::Wildcard
    }

    // Complex pattern builders

    /// Creates a guard pattern: pattern when condition
    pub fn guard(pattern: Pattern, condition: NodeId) -> Self {
        Pattern::Guard {
            pattern: Box::new(pattern),
            condition,
        }
    }

    /// Creates an as-pattern: binding @ pattern
    pub fn as_pattern(binding: &str, pattern: Pattern) -> Self {
        Pattern::As {
            binding: binding.to_string(),
            pattern: Box::new(pattern),
        }
    }

    /// Creates an or-pattern from multiple patterns
    pub fn or(patterns: Vec<Pattern>) -> Self {
        Pattern::Or(patterns)
    }

    /// Creates an inclusive range pattern (start..=end)
    pub fn range_inclusive(start: Literal, end: Literal) -> Self {
        Pattern::Range(RangePattern {
            start,
            end,
            inclusive: true,
        })
    }

    /// Creates an exclusive range pattern (start..end)
    pub fn range_exclusive(start: Literal, end: Literal) -> Self {
        Pattern::Range(RangePattern {
            start,
            end,
            inclusive: false,
        })
    }

    /// Creates an integer range pattern (inclusive)
    pub fn int_range(start: i64, end: i64) -> Self {
        Pattern::Range(RangePattern {
            start: Literal::Integer(start),
            end: Literal::Integer(end),
            inclusive: true,
        })
    }

    /// Creates a view pattern: applies function before matching
    pub fn view(function: NodeId, pattern: Pattern) -> Self {
        Pattern::View {
            function,
            pattern: Box::new(pattern),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EffectType {
    Pure,
    IO,
    State,
    Error,
    Time,
    Network,
    Random,
    Dom,
    Async,
    Concurrent,
}

impl fmt::Display for EffectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EffectType::Pure => write!(f, "Pure"),
            EffectType::IO => write!(f, "IO"),
            EffectType::State => write!(f, "State"),
            EffectType::Error => write!(f, "Error"),
            EffectType::Time => write!(f, "Time"),
            EffectType::Network => write!(f, "Network"),
            EffectType::Random => write!(f, "Random"),
            EffectType::Dom => write!(f, "Dom"),
            EffectType::Async => write!(f, "Async"),
            EffectType::Concurrent => write!(f, "Concurrent"),
        }
    }
}

impl DocumentedNode for Literal {
    fn name() -> &'static str {
        "Literal"
    }

    fn syntax() -> &'static str {
        "<literal>"
    }

    fn description() -> &'static str {
        "Literal values in FluentAi"
    }

    fn examples() -> &'static [&'static str] {
        &["42", "3.14", "\"hello\"", "true", "nil"]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Literal
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExportItem {
    pub name: String,
    pub alias: Option<String>,
}

/// Documentation System for AST Nodes
///
/// FluentAI uses a compile-time enforced documentation system to ensure all language
/// constructs are properly documented. The system works as follows:
///
/// 1. The `DocumentedNode` trait defines the interface for documentation
/// 2. The `Node` enum implements this trait generically
/// 3. The `get_node_docs()` method provides specific documentation for each variant
/// 4. The match statement in `get_node_docs()` is exhaustive and enforced by the compiler
///
/// When adding a new Node variant:
/// - You MUST update the match statement in `get_node_docs()`
/// - If you forget, the code will fail to compile with a non-exhaustive pattern error
/// - This ensures documentation is never out of sync with the AST
///
/// The documentation includes:
/// - name: The construct's name
/// - syntax: How to write it in FluentAI code
/// - description: What it does
/// - examples: Usage examples
/// - category: What kind of construct it is
/// - see_also: Related constructs
/// - visibility: Whether it's public API or internal
impl DocumentedNode for Node {
    fn name() -> &'static str {
        "AST Node"
    }

    fn syntax() -> &'static str {
        "Various - see specific node types"
    }

    fn description() -> &'static str {
        "Abstract Syntax Tree node representing a FluentAi construct"
    }

    fn examples() -> &'static [&'static str] {
        &[]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::DataStructure
    }

    fn visibility() -> DocumentationVisibility {
        DocumentationVisibility::Internal
    }

    fn get_docs() -> Documentation {
        // For the enum itself, we return generic documentation
        // The real documentation comes from get_node_docs() below
        Documentation {
            name: Self::name().to_string(),
            syntax: Self::syntax().to_string(),
            description: Self::description().to_string(),
            examples: Self::examples().iter().map(|s| s.to_string()).collect(),
            category: Self::category(),
            see_also: vec![],
            visibility: Self::visibility(),
        }
    }
}

impl Node {
    /// Get documentation specific to this node variant
    ///
    /// This method uses an exhaustive match to ensure all Node variants have documentation.
    /// If a new Node variant is added without updating this method, the code will fail to compile.
    /// Rust's compiler enforces exhaustive pattern matching by default.
    pub fn get_node_docs(&self) -> Documentation {
        match self {
            Node::Literal(lit) => match lit {
                Literal::Integer(_) => Documentation {
                    name: "Integer".to_string(),
                    syntax: "<integer>".to_string(),
                    description: "Integer literals represent whole numbers. FluentAi supports 64-bit signed integers.".to_string(),
                    examples: vec!["42".to_string(), "-17".to_string(), "0".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
                Literal::Float(_) => Documentation {
                    name: "Float".to_string(),
                    syntax: "<float>".to_string(),
                    description: "Floating-point literals represent decimal numbers. FluentAi uses 64-bit double precision.".to_string(),
                    examples: vec!["3.14".to_string(), "-2.5".to_string(), "1.23e-4".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
                Literal::String(_) => Documentation {
                    name: "String".to_string(),
                    syntax: "\"<text>\"".to_string(),
                    description: "String literals represent text data. Strings are enclosed in double quotes and support escape sequences.".to_string(),
                    examples: vec!["\"Hello, World!\"".to_string(), "\"Line 1\\nLine 2\"".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
                Literal::Boolean(_) => Documentation {
                    name: "Boolean".to_string(),
                    syntax: "true | false".to_string(),
                    description: "Boolean literals represent truth values. Only 'true' and 'false' are valid boolean values.".to_string(),
                    examples: vec!["true".to_string(), "false".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
                Literal::Nil => Documentation {
                    name: "Nil".to_string(),
                    syntax: "nil".to_string(),
                    description: "Nil represents the absence of a value. It is the only value of its type.".to_string(),
                    examples: vec!["nil".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
                Literal::Symbol(_) => Documentation {
                    name: "Symbol".to_string(),
                    syntax: "'<symbol>".to_string(),
                    description: "Symbol literals represent unique identifiers. Symbols are interned strings used for efficient comparison.".to_string(),
                    examples: vec!["'foo".to_string(), "'bar".to_string(), "'my-symbol".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
            },
            Node::Variable { name: _ } => Documentation {
                name: "Variable".to_string(),
                syntax: "<identifier>".to_string(),
                description: "Variables reference values bound in the current scope. Variable names must start with a letter or underscore.".to_string(),
                examples: vec!["x".to_string(), "count".to_string(), "my_variable".to_string()],
                category: DocumentationCategory::Variable,
                see_also: vec!["Let".to_string(), "Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Lambda { .. } => Documentation {
                name: "Lambda".to_string(),
                syntax: "(lambda (<params>) <body>)".to_string(),
                description: "Lambda creates anonymous functions. Parameters are bound in the function body scope.".to_string(),
                examples: vec!["(lambda (x) (+ x 1))".to_string(), "(lambda (x y) (* x y))".to_string()],
                category: DocumentationCategory::Function,
                see_also: vec!["Application".to_string(), "Let".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Let { .. } => Documentation {
                name: "Let".to_string(),
                syntax: "(let ((<var> <expr>) ...) <body>)".to_string(),
                description: "Let creates local variable bindings. Bindings are evaluated sequentially and are available in the body.".to_string(),
                examples: vec!["(let ((x 5)) (+ x 1))".to_string(), "(let ((x 10) (y 20)) (+ x y))".to_string()],
                category: DocumentationCategory::Function,
                see_also: vec!["Letrec".to_string(), "Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Letrec { .. } => Documentation {
                name: "Letrec".to_string(),
                syntax: "(letrec ((<var> <expr>) ...) <body>)".to_string(),
                description: "Letrec creates recursive local bindings. All bindings are in scope for all binding expressions, enabling mutual recursion.".to_string(),
                examples: vec!["(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))".to_string()],
                category: DocumentationCategory::Function,
                see_also: vec!["Let".to_string(), "Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::If { .. } => Documentation {
                name: "If".to_string(),
                syntax: "(if <condition> <then> <else>)".to_string(),
                description: "Conditional expression. Evaluates condition, then evaluates and returns either the then or else branch.".to_string(),
                examples: vec!["(if true \"yes\" \"no\")".to_string(), "(if (> x 0) \"positive\" \"non-positive\")".to_string()],
                category: DocumentationCategory::ControlFlow,
                see_also: vec!["Match".to_string(), "Boolean".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Application { .. } => Documentation {
                name: "Application".to_string(),
                syntax: "(<function> <arg1> <arg2> ...)".to_string(),
                description: "Function application. Applies a function to zero or more arguments.".to_string(),
                examples: vec!["(+ 1 2)".to_string(), "(print \"Hello\")".to_string(), "(map square [1 2 3])".to_string()],
                category: DocumentationCategory::Function,
                see_also: vec!["Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Effect { .. } => Documentation {
                name: "Effect".to_string(),
                syntax: "(effect <type> <operation> <args>...) | (<type>:<operation> <args>...)".to_string(),
                description: "Performs an effectful operation. Effects are tracked by the type system and handled by effect handlers. Can be called using explicit effect syntax or shorthand notation with colon.".to_string(),
                examples: vec!["(effect IO print \"Hello\")".to_string(), "(io:print \"Hello\")".to_string()],
                category: DocumentationCategory::Effect,
                see_also: vec!["Handler".to_string(), "Async".to_string(), "IO".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Handler { .. } => Documentation {
                name: "Handler".to_string(),
                syntax: "(handler ((<effect-type> <handler-fn>) ...) <body>)".to_string(),
                description: "Installs effect handlers for the dynamic scope of the body expression. Handlers intercept matching effects and can provide custom implementations.".to_string(),
                examples: vec![
                    "(handler ((error (lambda (err) \"default\"))) (risky-op))".to_string(),
                    "(handler ((io (lambda (op . args) (log op args)))) (io:print \"test\"))".to_string(),
                ],
                category: DocumentationCategory::Effect,
                see_also: vec!["Effect".to_string(), "Error".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::List(_) => Documentation {
                name: "List".to_string(),
                syntax: "[<expr1> <expr2> ...]".to_string(),
                description: "List literal. Creates a list containing the evaluated expressions.".to_string(),
                examples: vec!["[1 2 3]".to_string(), "[\"a\" \"b\" \"c\"]".to_string(), "[]".to_string()],
                category: DocumentationCategory::DataStructure,
                see_also: vec!["cons".to_string(), "car".to_string(), "cdr".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Match { .. } => Documentation {
                name: "Match".to_string(),
                syntax: "(match <expr> (<pattern> <body>) ...)".to_string(),
                description: "Pattern matching expression. Matches the expression against patterns and evaluates the corresponding body.".to_string(),
                examples: vec!["(match x (0 \"zero\") (1 \"one\") (_ \"other\"))".to_string()],
                category: DocumentationCategory::PatternMatching,
                see_also: vec!["If".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Module { .. } => Documentation {
                name: "Module".to_string(),
                syntax: "(module <name> <exports> <body>)".to_string(),
                description: "Defines a module with a name, list of exports, and body. Modules provide namespace isolation.".to_string(),
                examples: vec!["(module math [add subtract] (let ((add +) (subtract -)) ...))".to_string()],
                category: DocumentationCategory::Module,
                see_also: vec!["Import".to_string(), "Export".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Import { .. } => Documentation {
                name: "Import".to_string(),
                syntax: "(import <module-path> [<items>...] | *)".to_string(),
                description: "Imports items from a module. Can import specific items or all exports with *.".to_string(),
                examples: vec!["(import \"std/math\" [sin cos])".to_string(), "(import \"utils\" *)".to_string()],
                category: DocumentationCategory::Module,
                see_also: vec!["Module".to_string(), "Export".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Export { .. } => Documentation {
                name: "Export".to_string(),
                syntax: "(export <name1> <name2> ... | <name> as <alias> ...)".to_string(),
                description: "Exports items from the current module. Can optionally rename exports with aliases.".to_string(),
                examples: vec!["(export add subtract multiply)".to_string()],
                category: DocumentationCategory::Module,
                see_also: vec!["Module".to_string(), "Import".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::QualifiedVariable { .. } => Documentation {
                name: "QualifiedVariable".to_string(),
                syntax: "<module>.<variable>".to_string(),
                description: "References a variable from a specific module namespace.".to_string(),
                examples: vec!["math.pi".to_string(), "std.print".to_string()],
                category: DocumentationCategory::Variable,
                see_also: vec!["Variable".to_string(), "Import".to_string(), "Module".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Define { .. } => Documentation {
                name: "Define".to_string(),
                syntax: "(define <name> <value>) | (define (<name> <params>...) <body>)".to_string(),
                description: "Defines a top-level binding. Can define variables or functions using nested syntax.".to_string(),
                examples: vec![
                    "(define x 42)".to_string(),
                    "(define add (lambda (x y) (+ x y)))".to_string(),
                    "(define (square x) (* x x))".to_string()
                ],
                category: DocumentationCategory::Variable,
                see_also: vec!["Let".to_string(), "Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Begin { .. } => Documentation {
                name: "Begin".to_string(),
                syntax: "(begin <expr1> <expr2> ... <exprN>)".to_string(),
                description: "Evaluates multiple expressions in sequence, returning the value of the last expression.".to_string(),
                examples: vec![
                    "(begin (print \"Hello\") (print \"World\") 42)".to_string(),
                    "(begin (set! x 10) (set! y 20) (+ x y))".to_string()
                ],
                category: DocumentationCategory::ControlFlow,
                see_also: vec!["Let".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Async { .. } => Documentation {
                name: "Async".to_string(),
                syntax: "(async <body>)".to_string(),
                description: "Creates an asynchronous computation that can be awaited.".to_string(),
                examples: vec!["(async (http-get \"https://api.example.com\"))".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Await".to_string(), "Spawn".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Await { .. } => Documentation {
                name: "Await".to_string(),
                syntax: "(await <async-expr>)".to_string(),
                description: "Waits for an asynchronous computation to complete and returns its result.".to_string(),
                examples: vec!["(await (async (+ 1 2)))".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Async".to_string(), "Spawn".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Spawn { .. } => Documentation {
                name: "Spawn".to_string(),
                syntax: "(spawn <expr>)".to_string(),
                description: "Spawns a new concurrent task. Returns immediately without waiting for completion.".to_string(),
                examples: vec!["(spawn (process-data data))".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Async".to_string(), "Channel".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Channel { .. } => Documentation {
                name: "Channel".to_string(),
                syntax: "(chan) or (chan capacity)".to_string(),
                description: "Creates a new communication channel for sending values between concurrent tasks. Optional capacity parameter specifies buffer size (default is 1).".to_string(),
                examples: vec!["(let ((ch (chan))) ...)".to_string(), "(let ((ch (chan 10))) ...)".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Send".to_string(), "Receive".to_string(), "Spawn".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Send { .. } => Documentation {
                name: "Send".to_string(),
                syntax: "(send! <channel> <value>)".to_string(),
                description: "Sends a value through a channel. May block if the channel is full.".to_string(),
                examples: vec!["(send! ch 42)".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Channel".to_string(), "Receive".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Receive { .. } => Documentation {
                name: "Receive".to_string(),
                syntax: "(recv! <channel>)".to_string(),
                description: "Receives a value from a channel. Blocks until a value is available.".to_string(),
                examples: vec!["(recv! ch)".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Channel".to_string(), "Send".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::TrySend { .. } => Documentation {
                name: "TrySend".to_string(),
                syntax: "(try-send! <channel> <value>)".to_string(),
                description: "Attempts to send a value through a channel. Returns a result indicating success or failure.".to_string(),
                examples: vec!["(try-send! ch 42)".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Channel".to_string(), "Send".to_string(), "TryReceive".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::TryReceive { .. } => Documentation {
                name: "TryReceive".to_string(),
                syntax: "(try-recv! <channel>)".to_string(),
                description: "Attempts to receive a value from a channel. Returns a result indicating success or failure.".to_string(),
                examples: vec!["(try-recv! ch)".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Channel".to_string(), "Receive".to_string(), "TrySend".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Select { .. } => Documentation {
                name: "Select".to_string(),
                syntax: "(select ((recv! ch1) handler1) ((recv! ch2) handler2) ...)".to_string(),
                description: "Waits on multiple channel operations and executes the handler for the first one that completes.".to_string(),
                examples: vec![
                    "(select ((recv! ch1) (handle-ch1)) ((recv! ch2) (handle-ch2)))".to_string(),
                    "(select ((recv! ch) (process it)) (default (timeout-handler)))".to_string()
                ],
                category: DocumentationCategory::Async,
                see_also: vec!["Channel".to_string(), "Receive".to_string(), "Send".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Actor { .. } => Documentation {
                name: "Actor".to_string(),
                syntax: "(actor <initial-state> <handler-fn>)".to_string(),
                description: "Creates an actor with an initial state and a message handler function.".to_string(),
                examples: vec![
                    "(actor 0 (lambda (state msg) ...))".to_string()
                ],
                category: DocumentationCategory::Async,
                see_also: vec!["ActorSend".to_string(), "ActorReceive".to_string(), "Become".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::ActorSend { .. } => Documentation {
                name: "ActorSend".to_string(),
                syntax: "(! <actor> <message>)".to_string(),
                description: "Sends a message to an actor asynchronously.".to_string(),
                examples: vec![
                    "(! counter-actor (increment 5))".to_string()
                ],
                category: DocumentationCategory::Async,
                see_also: vec!["Actor".to_string(), "ActorReceive".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::ActorReceive { .. } => Documentation {
                name: "ActorReceive".to_string(),
                syntax: "(receive ((pattern1) handler1) ((pattern2) handler2) ...)".to_string(),
                description: "Receives messages in an actor, pattern matching on message types.".to_string(),
                examples: vec![
                    "(receive ((increment n) (+ state n)) ((get reply-to) (send! reply-to state)))".to_string()
                ],
                category: DocumentationCategory::Async,
                see_also: vec!["Actor".to_string(), "ActorSend".to_string(), "Become".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Become { .. } => Documentation {
                name: "Become".to_string(),
                syntax: "(become <new-state>)".to_string(),
                description: "Changes the state of an actor to a new value.".to_string(),
                examples: vec![
                    "(become (+ state 1))".to_string()
                ],
                category: DocumentationCategory::Async,
                see_also: vec!["Actor".to_string(), "ActorReceive".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Try { .. } => Documentation {
                name: "Try".to_string(),
                syntax: "(try <body> (catch <pattern> <handler>) ... [(finally <cleanup>)])".to_string(),
                description: "Executes body and catches errors matching patterns.".to_string(),
                examples: vec![
                    "(try (risky-operation) (catch (error msg) (log msg)))".to_string()
                ],
                category: DocumentationCategory::ControlFlow,
                see_also: vec!["Throw".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Throw { .. } => Documentation {
                name: "Throw".to_string(),
                syntax: "(throw <error>)".to_string(),
                description: "Throws an error that can be caught by try-catch.".to_string(),
                examples: vec![
                    "(throw (error \"Invalid input\"))".to_string()
                ],
                category: DocumentationCategory::ControlFlow,
                see_also: vec!["Try".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Promise { .. } => Documentation {
                name: "Promise".to_string(),
                syntax: "(promise <body>)".to_string(),
                description: "Creates a promise that resolves to the result of body.".to_string(),
                examples: vec![
                    "(promise (compute-async))".to_string()
                ],
                category: DocumentationCategory::Async,
                see_also: vec!["PromiseAll".to_string(), "PromiseRace".to_string(), "Await".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::PromiseAll { .. } => Documentation {
                name: "PromiseAll".to_string(),
                syntax: "(promise-all <promise1> <promise2> ...)".to_string(),
                description: "Waits for all promises to resolve and returns their results.".to_string(),
                examples: vec![
                    "(promise-all (fetch-user) (fetch-posts))".to_string()
                ],
                category: DocumentationCategory::Async,
                see_also: vec!["Promise".to_string(), "PromiseRace".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::PromiseRace { .. } => Documentation {
                name: "PromiseRace".to_string(),
                syntax: "(promise-race <promise1> <promise2> ...)".to_string(),
                description: "Returns the result of the first promise to resolve.".to_string(),
                examples: vec![
                    "(promise-race (fetch-primary) (fetch-backup))".to_string()
                ],
                category: DocumentationCategory::Async,
                see_also: vec!["Promise".to_string(), "PromiseAll".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Timeout { .. } => Documentation {
                name: "Timeout".to_string(),
                syntax: "(timeout <duration-ms> <promise> [<default>])".to_string(),
                description: "Waits for a promise with timeout, returning default on timeout.".to_string(),
                examples: vec![
                    "(timeout 5000 (fetch-data) \"timeout\")".to_string()
                ],
                category: DocumentationCategory::Async,
                see_also: vec!["Promise".to_string(), "Await".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Contract { .. } => Documentation {
                name: "Contract".to_string(),
                syntax: "(spec:contract <function-name> :requires [...] :ensures [...] :invariant [...] :complexity \"...\" :pure <bool>)".to_string(),
                description: "Specifies formal contracts for functions including preconditions, postconditions, invariants, complexity bounds, and purity constraints.".to_string(),
                examples: vec![
                    "(spec:contract factorial :requires [(>= n 0)] :ensures [(>= result 1)] :complexity \"O(n)\" :pure true)".to_string(),
                    "(spec:contract sort :requires [(list? input)] :ensures [(sorted? result) (= (length result) (length input))] :pure true)".to_string()
                ],
                category: DocumentationCategory::Verification,
                see_also: vec!["Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Assignment { .. } => Documentation {
                name: "Assignment".to_string(),
                syntax: "<target> = <value>".to_string(),
                description: "Assigns a value to a variable. The target must be a variable name. Returns nil.".to_string(),
                examples: vec![
                    "x = 42".to_string(),
                    "count = count + 1".to_string(),
                    "result = process(data)".to_string()
                ],
                category: DocumentationCategory::ControlFlow,
                see_also: vec!["Let".to_string(), "Define".to_string()],
                visibility: DocumentationVisibility::Public,
            },
        }
    }
}

/// Builder for constructing pattern matching expressions
pub struct MatchBuilder<'a> {
    graph: &'a mut Graph,
    expr: Option<NodeId>,
    branches: Vec<(Pattern, NodeId)>,
}

impl<'a> MatchBuilder<'a> {
    /// Creates a new match builder
    pub fn new(graph: &'a mut Graph) -> Self {
        MatchBuilder {
            graph,
            expr: None,
            branches: Vec::new(),
        }
    }

    /// Sets the expression to match against
    pub fn expr(mut self, node_id: NodeId) -> Self {
        self.expr = Some(node_id);
        self
    }

    /// Adds a branch with a pattern and result expression
    pub fn branch(mut self, pattern: Pattern, result: NodeId) -> Self {
        self.branches.push((pattern, result));
        self
    }

    /// Adds a branch with an integer literal pattern
    pub fn int_case(mut self, value: i64, result: NodeId) -> Self {
        self.branches.push((Pattern::int(value), result));
        self
    }

    /// Adds a branch with a string literal pattern
    pub fn string_case(mut self, value: &str, result: NodeId) -> Self {
        self.branches.push((Pattern::string(value), result));
        self
    }

    /// Adds a branch with a boolean literal pattern
    pub fn bool_case(mut self, value: bool, result: NodeId) -> Self {
        self.branches.push((Pattern::bool(value), result));
        self
    }

    /// Adds a branch with a variable pattern
    pub fn var_case(mut self, name: &str, result: NodeId) -> Self {
        self.branches.push((Pattern::var(name), result));
        self
    }

    /// Adds a wildcard/default branch
    pub fn default(mut self, result: NodeId) -> Self {
        self.branches.push((Pattern::wildcard(), result));
        self
    }

    /// Builds the match expression and returns its NodeId
    pub fn build(self) -> crate::error::Result<NodeId> {
        let expr = self.expr.ok_or_else(|| {
            crate::error::Error::Other(anyhow::anyhow!("Match expression required"))
        })?;

        if self.branches.is_empty() {
            return Err(crate::error::Error::Other(anyhow::anyhow!(
                "Match expression must have at least one branch"
            )));
        }

        self.graph.add_node(Node::Match {
            expr,
            branches: self.branches,
        })
    }
}

#[cfg(test)]
#[path = "ast_tests.rs"]
mod tests;
