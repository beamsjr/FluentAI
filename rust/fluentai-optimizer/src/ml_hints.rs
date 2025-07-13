//! Machine learning-based optimization hints

use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

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
    /// Maximum depth of the computation graph
    pub depth: usize,
    /// Average branching factor
    pub branching_factor: f32,
    /// Number of cycles in the graph
    pub cycle_count: usize,

    /// Operation features
    pub arithmetic_ops: usize,
    /// Number of memory operations
    pub memory_ops: usize,
    /// Number of control flow operations
    pub control_flow_ops: usize,
    /// Number of function calls
    pub function_calls: usize,

    /// Data flow features
    pub data_dependencies: usize,
    /// Number of live variables
    pub live_variables: usize,
    /// Estimated register pressure
    pub register_pressure: f32,

    /// Pattern features
    pub has_recursion: bool,
    /// Whether the program contains loops
    pub has_loops: bool,
    /// Whether the program contains map patterns
    pub has_map_pattern: bool,
    /// Whether the program contains reduce patterns
    pub has_reduce_pattern: bool,

    /// Type features
    pub uses_integers: bool,
    /// Whether the program uses floating point operations
    pub uses_floats: bool,
    /// Whether the program uses list operations
    pub uses_lists: bool,
    /// Whether the program uses higher-order functions
    pub uses_higher_order: bool,

    /// Performance hints
    pub estimated_iterations: Option<usize>,
    /// Hint about expected data size
    pub data_size_hint: Option<usize>,
    /// Hotness score for optimization priority
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

        Self {
            weights,
            thresholds,
        }
    }

    /// Extract features from a graph
    pub fn extract_features(&self, graph: &Graph) -> ProgramFeatures {
        let mut features = ProgramFeatures {
            node_count: graph.nodes.len(),
            depth: self.compute_depth(graph),
            branching_factor: self.compute_branching_factor(graph),
            cycle_count: self.detect_cycles(graph),
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
        for (node_id, node) in &graph.nodes {
            self.analyze_node(graph, *node_id, node, &mut features);
        }

        // Estimate data flow complexity
        self.estimate_data_flow(&mut features, graph);

        features
    }

    /// Analyze a single node
    fn analyze_node(
        &self,
        graph: &Graph,
        _node_id: NodeId,
        node: &Node,
        features: &mut ProgramFeatures,
    ) {
        match node {
            Node::Application { function, args } => {
                features.function_calls += 1;

                // Check function type
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    if is_arithmetic_op(name) {
                        features.arithmetic_ops += 1;
                    } else if is_memory_op(name) {
                        features.memory_ops += 1;
                    } else if name == "map" {
                        features.has_map_pattern = true;
                    } else if name == "reduce" || name == "fold" {
                        features.has_reduce_pattern = true;
                    }
                }

                // Count data dependencies
                features.data_dependencies += args.len();
            }
            Node::If { .. } => {
                features.control_flow_ops += 1;
            }
            Node::Match { branches, .. } => {
                features.control_flow_ops += 1;
                features.branching_factor = features.branching_factor.max(branches.len() as f32);
            }
            Node::Lambda { .. } => {
                features.uses_higher_order = true;
            }
            Node::List(_) => {
                features.uses_lists = true;
            }
            Node::Literal(lit) => {
                use fluentai_core::ast::Literal;
                match lit {
                    Literal::Integer(_) => features.uses_integers = true,
                    Literal::Float(_) => features.uses_floats = true,
                    _ => {}
                }
            }
            Node::Letrec { bindings, .. } => {
                // Check for recursion
                for (name, func_id) in bindings {
                    if self.is_recursive_binding(graph, name, *func_id) {
                        features.has_recursion = true;
                        features.has_loops = true; // Recursion is a form of loop
                    }
                }
            }
            _ => {}
        }
    }

    /// Check if a binding is recursive
    fn is_recursive_binding(&self, graph: &Graph, name: &str, func_id: NodeId) -> bool {
        use rustc_hash::FxHashSet;
        let mut visited = FxHashSet::default();
        self.contains_reference(graph, func_id, name, &mut visited)
    }

    /// Check if a node contains a reference to a name
    fn contains_reference(
        &self,
        graph: &Graph,
        node_id: NodeId,
        target_name: &str,
        visited: &mut rustc_hash::FxHashSet<NodeId>,
    ) -> bool {
        if !visited.insert(node_id) {
            return false;
        }

        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Variable { name } => name == target_name,
                Node::Application { function, args } => {
                    self.contains_reference(graph, *function, target_name, visited)
                        || args
                            .iter()
                            .any(|arg| self.contains_reference(graph, *arg, target_name, visited))
                }
                Node::Lambda { body, .. } => {
                    self.contains_reference(graph, *body, target_name, visited)
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    bindings
                        .iter()
                        .any(|(_, val)| self.contains_reference(graph, *val, target_name, visited))
                        || self.contains_reference(graph, *body, target_name, visited)
                }
                Node::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    self.contains_reference(graph, *condition, target_name, visited)
                        || self.contains_reference(graph, *then_branch, target_name, visited)
                        || self.contains_reference(graph, *else_branch, target_name, visited)
                }
                _ => false,
            }
        } else {
            false
        }
    }

    /// Detect cycles in the graph
    fn detect_cycles(&self, graph: &Graph) -> usize {
        // Simple cycle detection - count back edges
        use rustc_hash::FxHashSet;
        let mut visited = FxHashSet::default();
        let mut rec_stack = FxHashSet::default();
        let mut cycle_count = 0;

        for node_id in graph.nodes.keys() {
            if !visited.contains(node_id) {
                cycle_count += self.dfs_cycle_detect(graph, *node_id, &mut visited, &mut rec_stack);
            }
        }

        cycle_count
    }

    /// DFS-based cycle detection
    fn dfs_cycle_detect(
        &self,
        graph: &Graph,
        node_id: NodeId,
        visited: &mut rustc_hash::FxHashSet<NodeId>,
        rec_stack: &mut rustc_hash::FxHashSet<NodeId>,
    ) -> usize {
        visited.insert(node_id);
        rec_stack.insert(node_id);
        let mut cycles = 0;

        if let Some(node) = graph.get_node(node_id) {
            let children = self.get_node_children(node);
            for child in children {
                if !visited.contains(&child) {
                    cycles += self.dfs_cycle_detect(graph, child, visited, rec_stack);
                } else if rec_stack.contains(&child) {
                    cycles += 1;
                }
            }
        }

        rec_stack.remove(&node_id);
        cycles
    }

    /// Get children of a node
    fn get_node_children(&self, node: &Node) -> Vec<NodeId> {
        match node {
            Node::Application { function, args } => {
                let mut children = vec![*function];
                children.extend(args);
                children
            }
            Node::Lambda { body, .. } => vec![*body],
            Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                let mut children: Vec<_> = bindings.iter().map(|(_, v)| *v).collect();
                children.push(*body);
                children
            }
            Node::If {
                condition,
                then_branch,
                else_branch,
            } => {
                vec![*condition, *then_branch, *else_branch]
            }
            Node::Match { expr, branches } => {
                let mut children = vec![*expr];
                children.extend(branches.iter().map(|(_, b)| *b));
                children
            }
            _ => vec![],
        }
    }

    /// Estimate data flow complexity
    fn estimate_data_flow(&self, features: &mut ProgramFeatures, _graph: &Graph) {
        // Simple estimation based on node connectivity
        let avg_connections = features.data_dependencies as f32 / features.node_count.max(1) as f32;
        features.register_pressure = avg_connections * features.branching_factor;

        // Estimate live variables (simplified)
        features.live_variables = (features.node_count as f32 * 0.3) as usize;
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
                    let arg_depth = args
                        .iter()
                        .map(|id| self.node_depth(graph, *id))
                        .max()
                        .unwrap_or(0);
                    1 + func_depth.max(arg_depth)
                }
                Node::Lambda { body, .. } => 1 + self.node_depth(graph, *body),
                Node::Let { bindings, body } => {
                    let bind_depth = bindings
                        .iter()
                        .map(|(_, id)| self.node_depth(graph, *id))
                        .max()
                        .unwrap_or(0);
                    let body_depth = self.node_depth(graph, *body);
                    1 + bind_depth.max(body_depth)
                }
                Node::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
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
        let score: f32 = feature_vec
            .iter()
            .zip(self.weights.iter())
            .map(|(f, w)| f * w)
            .sum();

        let mut hints = Vec::new();

        // Generate hints based on score and patterns
        for (node_id, node) in &graph.nodes {
            match node {
                Node::Lambda { .. }
                    if score
                        > self
                            .thresholds
                            .get(&OptimizationHint::Inline)
                            .copied()
                            .unwrap_or(0.6) =>
                {
                    hints.push((*node_id, OptimizationHint::Inline));
                }
                Node::Application { .. }
                    if features.has_loops
                        && score
                            > self
                                .thresholds
                                .get(&OptimizationHint::Unroll)
                                .copied()
                                .unwrap_or(0.7) =>
                {
                    hints.push((*node_id, OptimizationHint::Unroll));
                }
                Node::Application { function, .. } if features.arithmetic_ops > 10 => {
                    // Check if it's an arithmetic operation
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        if is_arithmetic_op(name) && score > 0.5 {
                            hints.push((*node_id, OptimizationHint::Vectorize));
                        }
                    }
                }
                _ => {}
            }
        }

        hints
    }

    /// Apply optimization hints to a graph (placeholder for now)
    pub fn apply_hints(&self, _graph: &Graph, _hints: &[(NodeId, OptimizationHint)]) {
        // This would integrate with the optimization pipeline
        // For now, it's just a placeholder for testing
    }
}

impl Default for MLOptimizationHints {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ProgramFeatures {
    fn default() -> Self {
        Self {
            node_count: 0,
            depth: 0,
            branching_factor: 0.0,
            cycle_count: 0,
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
        }
    }
}

/// Simple prediction model for testing
pub struct SimplePredictionModel;

impl SimplePredictionModel {
    /// Create a new simple prediction model
    pub fn new() -> Self {
        Self
    }

    /// Predict optimization hints based on features
    pub fn predict(&self, features: &ProgramFeatures) -> Vec<(OptimizationHint, f32)> {
        let mut hints = Vec::new();

        // Simple heuristic rules
        if features.has_loops && features.arithmetic_ops > 10 {
            hints.push((OptimizationHint::Unroll, 0.8));
        }

        if features.arithmetic_ops > 20 && features.data_dependencies < 10 {
            hints.push((OptimizationHint::Vectorize, 0.7));
        }

        if features.function_calls > 5 && features.node_count < 50 {
            hints.push((OptimizationHint::Inline, 0.6));
        }

        if features.has_recursion && features.estimated_iterations.unwrap_or(0) > 100 {
            hints.push((OptimizationHint::Memoize, 0.7));
        }

        hints
    }
}

/// Check if a function name is an arithmetic operation
fn is_arithmetic_op(name: &str) -> bool {
    matches!(
        name,
        "+" | "-"
            | "*"
            | "/"
            | "mod"
            | "expt"
            | "sqrt"
            | "sin"
            | "cos"
            | "tan"
            | "log"
            | "exp"
            | "abs"
    )
}

/// Check if a function name is a memory operation
fn is_memory_op(name: &str) -> bool {
    matches!(
        name,
        "car"
            | "cdr"
            | "cons"
            | "list"
            | "append"
            | "ref"
            | "deref"
            | "set!"
            | "vector-ref"
            | "vector-set!"
    )
}

/// Check if a function name is a control flow operation
#[cfg(test)]
fn is_control_flow_op(name: &str) -> bool {
    matches!(
        name,
        "if" | "cond"
            | "case"
            | "match"
            | "call/cc"
            | "call-with-current-continuation"
            | "begin"
            | "do"
    )
}

#[cfg(test)]
mod tests {
    use crate::ml_hints::*;
    use fluentai_core::ast::{Graph, Literal, Node};

    #[test]
    fn test_optimization_hint_traits() {
        // Test that OptimizationHint implements expected traits
        let hint = OptimizationHint::Inline;

        // Test Debug
        assert_eq!(format!("{:?}", hint), "Inline");

        // Test Clone
        let cloned = hint.clone();
        assert_eq!(hint, cloned);

        // Test PartialEq
        assert_eq!(OptimizationHint::Inline, OptimizationHint::Inline);
        assert_ne!(OptimizationHint::Inline, OptimizationHint::Unroll);

        // Test Hash
        use std::collections::HashSet;
        let mut set = HashSet::new();
        set.insert(OptimizationHint::Inline);
        assert!(set.contains(&OptimizationHint::Inline));
    }

    #[test]
    fn test_program_features_default() {
        let features = ProgramFeatures::default();

        assert_eq!(features.node_count, 0);
        assert_eq!(features.depth, 0);
        assert_eq!(features.branching_factor, 0.0);
        assert_eq!(features.cycle_count, 0);
        assert_eq!(features.arithmetic_ops, 0);
        assert_eq!(features.memory_ops, 0);
        assert_eq!(features.control_flow_ops, 0);
        assert_eq!(features.function_calls, 0);
        assert_eq!(features.data_dependencies, 0);
        assert_eq!(features.live_variables, 0);
        assert_eq!(features.register_pressure, 0.0);
        assert!(!features.has_loops);
        assert!(!features.has_map_pattern);
        assert!(!features.has_reduce_pattern);
        assert!(!features.uses_floats);
        assert!(!features.uses_lists);
        assert!(!features.uses_higher_order);
        assert_eq!(features.data_size_hint, None);
        assert_eq!(features.hotness_score, 0.0);
    }

    #[test]
    fn test_feature_extractor_basic() {
        let extractor = MLOptimizationHints::new();
        let mut graph = Graph::new();

        // Create a simple expression: (+ 1 2)
        let one = graph
            .add_node(Node::Literal(Literal::Integer(1)))
            .expect("Failed to add node");
        let two = graph
            .add_node(Node::Literal(Literal::Integer(2)))
            .expect("Failed to add node");
        let plus = graph
            .add_node(Node::Variable {
                name: "+".to_string(),
            })
            .expect("Failed to add node");
        let app = graph
            .add_node(Node::Application {
                function: plus,
                args: vec![one, two],
            })
            .expect("Failed to add node");
        graph.root_id = Some(app);

        let features = extractor.extract_features(&graph);

        assert_eq!(features.node_count, 4);
        assert_eq!(features.arithmetic_ops, 1);
        assert_eq!(features.function_calls, 1);
        assert!(features.depth > 0);
    }

    #[test]
    fn test_feature_extractor_control_flow() {
        let extractor = MLOptimizationHints::new();
        let mut graph = Graph::new();

        // Create: (if #t 1 2)
        let cond = graph
            .add_node(Node::Literal(Literal::Boolean(true)))
            .expect("Failed to add node");
        let then_val = graph
            .add_node(Node::Literal(Literal::Integer(1)))
            .expect("Failed to add node");
        let else_val = graph
            .add_node(Node::Literal(Literal::Integer(2)))
            .expect("Failed to add node");
        let if_node = graph
            .add_node(Node::If {
                condition: cond,
                then_branch: then_val,
                else_branch: else_val,
            })
            .expect("Failed to add node");
        graph.root_id = Some(if_node);

        let features = extractor.extract_features(&graph);

        assert_eq!(features.control_flow_ops, 1);
        assert!(features.branching_factor > 0.0);
    }

    #[test]
    fn test_feature_extractor_higher_order() {
        let extractor = MLOptimizationHints::new();
        let mut graph = Graph::new();

        // Create: (lambda (x) x)
        let x_ref = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .expect("Failed to add node");
        let lambda = graph
            .add_node(Node::Lambda {
                params: vec!["x".to_string()],
                body: x_ref,
            })
            .expect("Failed to add node");
        graph.root_id = Some(lambda);

        let features = extractor.extract_features(&graph);

        assert!(features.uses_higher_order);
    }

    #[test]
    fn test_feature_extractor_lists() {
        let extractor = MLOptimizationHints::new();
        let mut graph = Graph::new();

        // Create: (cons 1 '())
        let one = graph
            .add_node(Node::Literal(Literal::Integer(1)))
            .expect("Failed to add node");
        let nil = graph
            .add_node(Node::List(vec![]))
            .expect("Failed to add node");
        let cons = graph
            .add_node(Node::Variable {
                name: "cons".to_string(),
            })
            .expect("Failed to add node");
        let app = graph
            .add_node(Node::Application {
                function: cons,
                args: vec![one, nil],
            })
            .expect("Failed to add node");
        graph.root_id = Some(app);

        let features = extractor.extract_features(&graph);

        assert!(features.uses_lists);
        assert_eq!(features.memory_ops, 1);
    }

    #[test]
    fn test_feature_extractor_map_pattern() {
        let extractor = MLOptimizationHints::new();
        let mut graph = Graph::new();

        // Create: (map f list)
        let f = graph
            .add_node(Node::Variable {
                name: "f".to_string(),
            })
            .expect("Failed to add node");
        let list = graph
            .add_node(Node::Variable {
                name: "list".to_string(),
            })
            .expect("Failed to add node");
        let map_fn = graph
            .add_node(Node::Variable {
                name: "map".to_string(),
            })
            .expect("Failed to add node");
        let app = graph
            .add_node(Node::Application {
                function: map_fn,
                args: vec![f, list],
            })
            .expect("Failed to add node");
        graph.root_id = Some(app);

        let features = extractor.extract_features(&graph);

        assert!(features.has_map_pattern);
    }

    #[test]
    fn test_hint_generator() {
        let mut hints = MLOptimizationHints::new();
        let mut graph = Graph::new();

        // Create a function with enough operations to trigger inlining hint
        let mut nodes = vec![];
        for i in 0..5 {
            nodes.push(
                graph
                    .add_node(Node::Literal(Literal::Integer(i)))
                    .expect("Failed to add node"),
            );
        }

        // Create lambda body with multiple operations
        let plus = graph
            .add_node(Node::Variable {
                name: "+".to_string(),
            })
            .expect("Failed to add node");
        let mut sum = nodes[0];
        for i in 1..5 {
            sum = graph
                .add_node(Node::Application {
                    function: plus,
                    args: vec![sum, nodes[i]],
                })
                .expect("Failed to add node");
        }

        let lambda = graph
            .add_node(Node::Lambda {
                params: vec!["x".to_string()],
                body: sum,
            })
            .expect("Failed to add node");

        // Apply the lambda
        let x = graph
            .add_node(Node::Literal(Literal::Integer(10)))
            .expect("Failed to add node");
        let app = graph
            .add_node(Node::Application {
                function: lambda,
                args: vec![x],
            })
            .expect("Failed to add node");
        graph.root_id = Some(app);

        // Generate hints
        let generated_hints = hints.generate_hints(&graph);

        // Should suggest inlining for small functions
        assert!(!generated_hints.is_empty());
    }

    #[test]
    fn test_prediction_model_simple() {
        let model = SimplePredictionModel::new();

        let mut features = ProgramFeatures::default();
        features.node_count = 50;
        features.arithmetic_ops = 20;
        features.has_loops = true;

        let hints = model.predict(&features);

        // Should suggest unrolling for loops with arithmetic
        assert!(hints
            .iter()
            .any(|(h, _)| matches!(h, OptimizationHint::Unroll)));
    }

    #[test]
    fn test_prediction_model_vectorization() {
        let model = SimplePredictionModel::new();

        let mut features = ProgramFeatures::default();
        features.arithmetic_ops = 30;
        features.memory_ops = 10;
        features.data_dependencies = 5;

        let hints = model.predict(&features);

        // Should suggest vectorization for arithmetic-heavy code
        assert!(hints
            .iter()
            .any(|(h, _)| matches!(h, OptimizationHint::Vectorize)));
    }

    #[test]
    fn test_ml_optimization_hints_integration() {
        let mut ml_hints = MLOptimizationHints::new();
        let mut graph = Graph::new();

        // Create a complex expression to get various hints
        let mut values = vec![];
        for i in 0..10 {
            values.push(
                graph
                    .add_node(Node::Literal(Literal::Integer(i)))
                    .expect("Failed to add node"),
            );
        }

        // Create arithmetic operations
        let plus = graph
            .add_node(Node::Variable {
                name: "+".to_string(),
            })
            .expect("Failed to add node");
        let mult = graph
            .add_node(Node::Variable {
                name: "*".to_string(),
            })
            .expect("Failed to add node");

        let mut result = values[0];
        for i in 1..10 {
            let prod = graph
                .add_node(Node::Application {
                    function: mult,
                    args: vec![values[i], values[i]],
                })
                .expect("Failed to add node");
            result = graph
                .add_node(Node::Application {
                    function: plus,
                    args: vec![result, prod],
                })
                .expect("Failed to add node");
        }

        graph.root_id = Some(result);

        // Get optimization hints
        let features = ml_hints.extract_features(&graph);
        let hints = ml_hints.generate_hints(&graph);

        // Verify features were extracted
        assert!(features.node_count > 20);
        assert!(features.arithmetic_ops > 15);

        // Verify hints were generated
        assert!(!hints.is_empty());

        // Apply hints (this tests the apply_hints method)
        ml_hints.apply_hints(&graph, &hints);
    }

    #[test]
    fn test_arithmetic_op_detection() {
        assert!(is_arithmetic_op("+"));
        assert!(is_arithmetic_op("-"));
        assert!(is_arithmetic_op("*"));
        assert!(is_arithmetic_op("/"));
        assert!(is_arithmetic_op("mod"));
        assert!(is_arithmetic_op("expt"));
        assert!(is_arithmetic_op("sqrt"));
        assert!(is_arithmetic_op("sin"));
        assert!(is_arithmetic_op("cos"));
        assert!(!is_arithmetic_op("cons"));
        assert!(!is_arithmetic_op("map"));
    }

    #[test]
    fn test_control_flow_detection() {
        assert!(is_control_flow_op("if"));
        assert!(is_control_flow_op("cond"));
        assert!(is_control_flow_op("case"));
        assert!(is_control_flow_op("match"));
        assert!(is_control_flow_op("call/cc"));
        assert!(!is_control_flow_op("map"));
        assert!(!is_control_flow_op("+"));
    }

    #[test]
    fn test_memory_op_detection() {
        assert!(is_memory_op("car"));
        assert!(is_memory_op("cdr"));
        assert!(is_memory_op("cons"));
        assert!(is_memory_op("list"));
        assert!(is_memory_op("append"));
        assert!(is_memory_op("ref"));
        assert!(is_memory_op("deref"));
        assert!(is_memory_op("set!"));
        assert!(is_memory_op("vector-ref"));
        assert!(is_memory_op("vector-set!"));
        assert!(!is_memory_op("+"));
        assert!(!is_memory_op("if"));
    }
}
