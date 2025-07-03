//! Machine learning-based optimization hints

use claudelang_core::ast::{Graph, Node, NodeId};
use rustc_hash::FxHashMap;
use serde::{Serialize, Deserialize};

/// Types of optimization hints
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OptimizationHint {
    /// Inline this function
    Inline,
    /// Unroll this loop
    Unroll,
    /// Vectorize this operation
    Vectorize,
    /// Parallelize this computation
    Parallelize,
    /// Cache results
    Memoize,
    /// Type-specialize this function
    Specialize,
    /// Fuse these operations
    Fusion,
    /// Prefetch this data
    Prefetch,
    /// Change data layout
    Layout,
    /// Use streaming computation
    Streaming,
}

/// Features extracted from a program for ML
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramFeatures {
    /// Structural features
    pub node_count: usize,
    pub depth: usize,
    pub branching_factor: f32,
    pub cycle_count: usize,
    
    /// Operation features
    pub arithmetic_ops: usize,
    pub memory_ops: usize,
    pub control_flow_ops: usize,
    pub function_calls: usize,
    
    /// Data flow features
    pub data_dependencies: usize,
    pub live_variables: usize,
    pub register_pressure: f32,
    
    /// Pattern features
    pub has_recursion: bool,
    pub has_loops: bool,
    pub has_map_pattern: bool,
    pub has_reduce_pattern: bool,
    
    /// Type features
    pub uses_integers: bool,
    pub uses_floats: bool,
    pub uses_lists: bool,
    pub uses_higher_order: bool,
    
    /// Performance hints
    pub estimated_iterations: Option<usize>,
    pub data_size_hint: Option<usize>,
    pub hotness_score: f32,
}

impl ProgramFeatures {
    /// Convert features to vector for ML
    pub fn to_vector(&self) -> Vec<f32> {
        vec![
            self.node_count as f32,
            self.depth as f32,
            self.branching_factor,
            self.cycle_count as f32,
            self.arithmetic_ops as f32,
            self.memory_ops as f32,
            self.control_flow_ops as f32,
            self.function_calls as f32,
            self.data_dependencies as f32,
            self.live_variables as f32,
            self.register_pressure,
            if self.has_recursion { 1.0 } else { 0.0 },
            if self.has_loops { 1.0 } else { 0.0 },
            if self.has_map_pattern { 1.0 } else { 0.0 },
            if self.has_reduce_pattern { 1.0 } else { 0.0 },
            if self.uses_integers { 1.0 } else { 0.0 },
            if self.uses_floats { 1.0 } else { 0.0 },
            if self.uses_lists { 1.0 } else { 0.0 },
            if self.uses_higher_order { 1.0 } else { 0.0 },
            self.estimated_iterations.unwrap_or(0) as f32,
            self.data_size_hint.unwrap_or(0) as f32,
            self.hotness_score,
        ]
    }
}

/// ML-based optimization hint generator
pub struct MLOptimizationHints {
    /// Feature weights (simple linear model)
    weights: Vec<f32>,
    /// Hint thresholds
    thresholds: FxHashMap<OptimizationHint, f32>,
}

impl MLOptimizationHints {
    /// Create new ML hint generator
    pub fn new() -> Self {
        // Initialize with some heuristic weights
        let weights = vec![
            0.1,  // node_count
            0.2,  // depth
            0.15, // branching_factor
            0.3,  // cycle_count
            0.1,  // arithmetic_ops
            0.05, // memory_ops
            0.2,  // control_flow_ops
            0.25, // function_calls
            0.1,  // data_dependencies
            0.15, // live_variables
            0.2,  // register_pressure
            0.5,  // has_recursion
            0.4,  // has_loops
            0.3,  // has_map_pattern
            0.3,  // has_reduce_pattern
            0.1,  // uses_integers
            0.1,  // uses_floats
            0.2,  // uses_lists
            0.3,  // uses_higher_order
            0.4,  // estimated_iterations
            0.3,  // data_size_hint
            0.5,  // hotness_score
        ];
        
        let mut thresholds = FxHashMap::default();
        thresholds.insert(OptimizationHint::Inline, 0.6);
        thresholds.insert(OptimizationHint::Unroll, 0.7);
        thresholds.insert(OptimizationHint::Vectorize, 0.65);
        thresholds.insert(OptimizationHint::Parallelize, 0.75);
        thresholds.insert(OptimizationHint::Memoize, 0.6);
        thresholds.insert(OptimizationHint::Specialize, 0.7);
        
        Self { weights, thresholds }
    }

    /// Extract features from a graph
    pub fn extract_features(&self, graph: &Graph) -> ProgramFeatures {
        let mut features = ProgramFeatures {
            node_count: graph.nodes.len(),
            depth: self.compute_depth(graph),
            branching_factor: self.compute_branching_factor(graph),
            cycle_count: 0, // TODO: Detect cycles
            arithmetic_ops: 0,
            memory_ops: 0,
            control_flow_ops: 0,
            function_calls: 0,
            data_dependencies: 0,
            live_variables: 0,
            register_pressure: 0.0,
            has_recursion: false,
            has_loops: false,
            has_map_pattern: false,
            has_reduce_pattern: false,
            uses_integers: false,
            uses_floats: false,
            uses_lists: false,
            uses_higher_order: false,
            estimated_iterations: None,
            data_size_hint: None,
            hotness_score: 0.0,
        };

        // Analyze nodes
        for node in graph.nodes.values() {
            self.analyze_node(node, &mut features);
        }

        features
    }

    /// Analyze a single node
    fn analyze_node(&self, node: &Node, features: &mut ProgramFeatures) {
        match node {
            Node::Application { .. } => {
                features.function_calls += 1;
                if let Node::Variable { name } = node {
                    if is_arithmetic_op(name) {
                        features.arithmetic_ops += 1;
                    }
                }
            }
            Node::If { .. } => {
                features.control_flow_ops += 1;
            }
            Node::Lambda { .. } => {
                features.uses_higher_order = true;
            }
            Node::List(_) => {
                features.uses_lists = true;
            }
            Node::Literal(lit) => {
                use claudelang_core::ast::Literal;
                match lit {
                    Literal::Integer(_) => features.uses_integers = true,
                    Literal::Float(_) => features.uses_floats = true,
                    _ => {}
                }
            }
            _ => {}
        }
    }

    /// Compute graph depth
    fn compute_depth(&self, graph: &Graph) -> usize {
        if let Some(root) = graph.root_id {
            self.node_depth(graph, root)
        } else {
            0
        }
    }

    /// Compute node depth recursively
    fn node_depth(&self, graph: &Graph, node_id: NodeId) -> usize {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Application { function, args } => {
                    let func_depth = self.node_depth(graph, *function);
                    let arg_depth = args.iter()
                        .map(|id| self.node_depth(graph, *id))
                        .max()
                        .unwrap_or(0);
                    1 + func_depth.max(arg_depth)
                }
                Node::Lambda { body, .. } => 1 + self.node_depth(graph, *body),
                Node::Let { bindings, body } => {
                    let bind_depth = bindings.iter()
                        .map(|(_, id)| self.node_depth(graph, *id))
                        .max()
                        .unwrap_or(0);
                    let body_depth = self.node_depth(graph, *body);
                    1 + bind_depth.max(body_depth)
                }
                Node::If { condition, then_branch, else_branch } => {
                    let cond_depth = self.node_depth(graph, *condition);
                    let then_depth = self.node_depth(graph, *then_branch);
                    let else_depth = self.node_depth(graph, *else_branch);
                    1 + cond_depth.max(then_depth).max(else_depth)
                }
                _ => 1,
            }
        } else {
            0
        }
    }

    /// Compute average branching factor
    fn compute_branching_factor(&self, graph: &Graph) -> f32 {
        let mut total_children = 0;
        let mut parent_nodes = 0;

        for node in graph.nodes.values() {
            let children = match node {
                Node::Application { args, .. } => args.len() + 1,
                Node::Let { bindings, .. } => bindings.len() + 1,
                Node::If { .. } => 3,
                Node::Lambda { .. } => 1,
                _ => 0,
            };

            if children > 0 {
                total_children += children;
                parent_nodes += 1;
            }
        }

        if parent_nodes > 0 {
            total_children as f32 / parent_nodes as f32
        } else {
            0.0
        }
    }

    /// Generate optimization hints for a graph
    pub fn generate_hints(&self, graph: &Graph) -> Vec<(NodeId, OptimizationHint)> {
        let features = self.extract_features(graph);
        let feature_vec = features.to_vector();
        
        // Simple dot product scoring
        let score: f32 = feature_vec.iter()
            .zip(self.weights.iter())
            .map(|(f, w)| f * w)
            .sum();

        let mut hints = Vec::new();

        // Generate hints based on score and patterns
        for (node_id, node) in &graph.nodes {
            match node {
                Node::Lambda { .. } if score > self.thresholds[&OptimizationHint::Inline] => {
                    hints.push((*node_id, OptimizationHint::Inline));
                }
                Node::Application { .. } if features.has_loops && score > self.thresholds[&OptimizationHint::Unroll] => {
                    hints.push((*node_id, OptimizationHint::Unroll));
                }
                _ => {}
            }
        }

        hints
    }
}

impl Default for MLOptimizationHints {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if a function name is an arithmetic operation
fn is_arithmetic_op(name: &str) -> bool {
    matches!(name, "+" | "-" | "*" | "/" | "mod")
}