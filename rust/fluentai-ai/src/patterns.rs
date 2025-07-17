//! Pattern detection for AST graphs

use fluentai_core::ast::{Graph, Node, NodeId};
use std::collections::{HashMap, HashSet};
use tracing::warn;

/// Pattern detector for AST graphs
pub struct PatternDetector {
    /// Pattern library
    patterns: Vec<PatternTemplate>,
    /// Detection configuration
    config: DetectionConfig,
}

/// Configuration for pattern detection
#[derive(Debug, Clone)]
pub struct DetectionConfig {
    /// Minimum confidence threshold
    pub min_confidence: f32,
    /// Enable fuzzy matching
    pub fuzzy_matching: bool,
    /// Maximum pattern size
    pub max_pattern_size: usize,
}

impl Default for DetectionConfig {
    fn default() -> Self {
        Self {
            min_confidence: 0.7,
            fuzzy_matching: true,
            max_pattern_size: 100,
        }
    }
}

/// Template for matching patterns
#[derive(Debug, Clone)]
pub struct PatternTemplate {
    /// Pattern name
    pub name: String,
    /// Pattern type
    pub pattern_type: PatternType,
    /// Root matcher
    pub root: NodeMatcher,
    /// Subpattern matchers
    pub subpatterns: Vec<NodeMatcher>,
    /// Minimum nodes required
    pub min_nodes: usize,
    /// Description
    pub description: String,
}

/// Types of patterns
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PatternType {
    /// Map-reduce pattern
    MapReduce,
    /// Filter-map chain
    FilterMap,
    /// Fold/reduce pattern
    Fold,
    /// Recursive pattern
    Recursion,
    /// Tail recursion
    TailRecursion,
    /// Higher-order function
    HigherOrder,
    /// Monad operations
    Monad,
    /// Actor pattern
    Actor,
    /// Effect handling
    Effect,
    /// Pipeline pattern
    Pipeline,
}

/// Node matcher for pattern templates
#[derive(Debug, Clone)]
pub enum NodeMatcher {
    /// Match any node
    Any,
    /// Match specific node type
    NodeType(String),
    /// Match variable with pattern
    Variable(String),
    /// Match application
    Application {
        function: Box<NodeMatcher>,
        args: Vec<NodeMatcher>,
    },
    /// Match lambda
    Lambda {
        params: Vec<String>,
        body: Box<NodeMatcher>,
    },
    /// Match literal
    Literal,
    /// Match one of several patterns
    Or(Vec<NodeMatcher>),
    /// Match sequence
    Sequence(Vec<NodeMatcher>),
}

/// Detected pattern instance
#[derive(Debug, Clone)]
pub struct DetectedPattern {
    /// Pattern template that matched
    pub template: PatternTemplate,
    /// Root node of the pattern
    pub root_node: NodeId,
    /// All nodes involved
    pub nodes: Vec<NodeId>,
    /// Confidence score
    pub confidence: f32,
    /// Variable bindings
    pub bindings: HashMap<String, NodeId>,
}

impl PatternDetector {
    /// Create a new pattern detector
    pub fn new(config: DetectionConfig) -> Self {
        let patterns = Self::create_default_patterns();
        Self { patterns, config }
    }
    
    /// Add a custom pattern
    pub fn add_pattern(&mut self, pattern: PatternTemplate) {
        self.patterns.push(pattern);
    }
    
    /// Detect all patterns in a graph
    pub fn detect_patterns(&self, graph: &Graph) -> Vec<DetectedPattern> {
        let mut detected = Vec::new();
        
        for node_id in graph.node_ids() {
            for pattern in &self.patterns {
                if let Some(result) = self.try_match_pattern(graph, node_id, pattern) {
                    if result.confidence >= self.config.min_confidence {
                        detected.push(result);
                    }
                }
            }
        }
        
        // Remove overlapping patterns (keep highest confidence)
        self.filter_overlapping_patterns(detected)
    }
    
    /// Try to match a pattern at a specific node
    fn try_match_pattern(
        &self,
        graph: &Graph,
        node_id: NodeId,
        pattern: &PatternTemplate,
    ) -> Option<DetectedPattern> {
        let mut bindings = HashMap::new();
        let mut matched_nodes = Vec::new();
        
        if self.match_node(graph, node_id, &pattern.root, &mut bindings, &mut matched_nodes) {
            // Check subpatterns
            let mut confidence = 1.0;
            
            for subpattern in &pattern.subpatterns {
                let mut found = false;
                for &matched_node in &matched_nodes {
                    if self.match_node(graph, matched_node, subpattern, &mut bindings, &mut Vec::new()) {
                        found = true;
                        break;
                    }
                }
                if !found {
                    confidence *= 0.8; // Reduce confidence for missing subpatterns
                }
            }
            
            if matched_nodes.len() >= pattern.min_nodes {
                Some(DetectedPattern {
                    template: pattern.clone(),
                    root_node: node_id,
                    nodes: matched_nodes,
                    confidence,
                    bindings,
                })
            } else {
                None
            }
        } else {
            None
        }
    }
    
    /// Match a node against a pattern
    fn match_node(
        &self,
        graph: &Graph,
        node_id: NodeId,
        matcher: &NodeMatcher,
        bindings: &mut HashMap<String, NodeId>,
        matched_nodes: &mut Vec<NodeId>,
    ) -> bool {
        if !matched_nodes.contains(&node_id) {
            matched_nodes.push(node_id);
        }
        
        match matcher {
            NodeMatcher::Any => true,
            
            NodeMatcher::NodeType(expected_type) => {
                if let Some(node) = graph.get_node(node_id) {
                    self.node_type_name(node) == expected_type
                } else {
                    false
                }
            }
            
            NodeMatcher::Variable(pattern) => {
                if let Some(node) = graph.get_node(node_id) {
                    if let Node::Variable { name } = node {
                        if pattern == "*" || name.contains(pattern) {
                            bindings.insert(pattern.clone(), node_id);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            
            NodeMatcher::Application { function, args } => {
                if let Some(Node::Application { function: f_id, args: a_ids }) = graph.get_node(node_id) {
                    if self.match_node(graph, *f_id, function, bindings, matched_nodes) {
                        if args.is_empty() || a_ids.len() == args.len() {
                            for (i, arg_matcher) in args.iter().enumerate() {
                                if i < a_ids.len() {
                                    if !self.match_node(graph, a_ids[i], arg_matcher, bindings, matched_nodes) {
                                        return false;
                                    }
                                }
                            }
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            
            NodeMatcher::Lambda { params, body } => {
                if let Some(Node::Lambda { params: p, body: b, .. }) = graph.get_node(node_id) {
                    if params.is_empty() || p.len() == params.len() {
                        self.match_node(graph, *b, body, bindings, matched_nodes)
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            
            NodeMatcher::Literal => {
                matches!(graph.get_node(node_id), Some(Node::Literal(_)))
            }
            
            NodeMatcher::Or(matchers) => {
                for m in matchers {
                    if self.match_node(graph, node_id, m, bindings, matched_nodes) {
                        return true;
                    }
                }
                false
            }
            
            NodeMatcher::Sequence(matchers) => {
                // For sequences, try to match children in order
                let children = graph.children(node_id);
                if children.len() >= matchers.len() {
                    for (i, m) in matchers.iter().enumerate() {
                        if !self.match_node(graph, children[i], m, bindings, matched_nodes) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
        }
    }
    
    /// Get the type name of a node
    fn node_type_name(&self, node: &Node) -> &'static str {
        match node {
            Node::Variable { .. } => "Variable",
            Node::Lambda { .. } => "Lambda",
            Node::Application { .. } => "Application",
            Node::Literal(_) => "Literal",
            Node::Let { .. } => "Let",
            Node::Letrec { .. } => "Letrec",
            Node::If { .. } => "If",
            Node::Match { .. } => "Match",
            Node::List(_) => "List",
            Node::Map(_) => "Map",
            Node::Effect { .. } => "Effect",
            Node::Handler { .. } => "Handler",
            Node::Module { .. } => "Module",
            Node::Import { .. } => "Import",
            Node::Export { .. } => "Export",
            Node::QualifiedVariable { .. } => "QualifiedVariable",
            Node::Define { .. } => "Define",
            Node::Assignment { .. } => "Assignment",
            Node::Begin { .. } => "Begin",
            Node::Async { .. } => "Async",
            Node::Await { .. } => "Await",
            Node::Spawn { .. } => "Spawn",
            Node::Channel { .. } => "Channel",
            Node::Send { .. } => "Send",
            Node::Receive { .. } => "Receive",
            Node::Select { .. } => "Select",
            Node::Try { .. } => "Try",
            Node::Throw { .. } => "Throw",
            Node::Actor { .. } => "Actor",
            Node::ActorSend { .. } => "ActorSend",
            Node::ActorReceive { .. } => "ActorReceive",
            Node::Become { .. } => "Become",
            _ => {
                tracing::warn!("Unknown node type encountered in pattern detection: {:?}", std::mem::discriminant(node));
                "Unknown"
            }
        }
    }
    
    /// Filter overlapping patterns
    fn filter_overlapping_patterns(&self, patterns: Vec<DetectedPattern>) -> Vec<DetectedPattern> {
        let mut filtered = Vec::new();
        let mut used_nodes = HashSet::new();
        
        // Sort by confidence (descending)
        let mut sorted = patterns;
        sorted.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());
        
        for pattern in sorted {
            let nodes_set: HashSet<_> = pattern.nodes.iter().cloned().collect();
            let overlap = nodes_set.intersection(&used_nodes).count();
            
            // Allow some overlap but not complete overlap
            if overlap < nodes_set.len() / 2 {
                used_nodes.extend(&nodes_set);
                filtered.push(pattern);
            }
        }
        
        filtered
    }
    
    /// Create default pattern library
    fn create_default_patterns() -> Vec<PatternTemplate> {
        vec![
            // Map-Reduce pattern
            PatternTemplate {
                name: "map_reduce".to_string(),
                pattern_type: PatternType::MapReduce,
                root: NodeMatcher::Application {
                    function: Box::new(NodeMatcher::Variable("reduce".to_string())),
                    args: vec![
                        NodeMatcher::Application {
                            function: Box::new(NodeMatcher::Variable("map".to_string())),
                            args: vec![NodeMatcher::Any],
                        },
                        NodeMatcher::Any,
                    ],
                },
                subpatterns: vec![],
                min_nodes: 3,
                description: "Map followed by reduce".to_string(),
            },
            
            // Filter-Map pattern
            PatternTemplate {
                name: "filter_map".to_string(),
                pattern_type: PatternType::FilterMap,
                root: NodeMatcher::Application {
                    function: Box::new(NodeMatcher::Variable("map".to_string())),
                    args: vec![
                        NodeMatcher::Application {
                            function: Box::new(NodeMatcher::Variable("filter".to_string())),
                            args: vec![NodeMatcher::Any],
                        },
                    ],
                },
                subpatterns: vec![],
                min_nodes: 3,
                description: "Filter followed by map".to_string(),
            },
            
            // Tail recursion pattern
            PatternTemplate {
                name: "tail_recursion".to_string(),
                pattern_type: PatternType::TailRecursion,
                root: NodeMatcher::Lambda {
                    params: vec![],
                    body: Box::new(NodeMatcher::if_matcher(
                        Box::new(NodeMatcher::Any),
                        Box::new(NodeMatcher::Any),
                        Box::new(NodeMatcher::Application {
                            function: Box::new(NodeMatcher::Variable("*".to_string())),
                            args: vec![],
                        }),
                    )),
                },
                subpatterns: vec![],
                min_nodes: 4,
                description: "Tail recursive function".to_string(),
            },
            
            // Pipeline pattern
            PatternTemplate {
                name: "pipeline".to_string(),
                pattern_type: PatternType::Pipeline,
                root: NodeMatcher::Application {
                    function: Box::new(NodeMatcher::Variable("|>".to_string())),
                    args: vec![NodeMatcher::Any, NodeMatcher::Any],
                },
                subpatterns: vec![],
                min_nodes: 2,
                description: "Pipeline operator chain".to_string(),
            },
        ]
    }
}

impl NodeMatcher {
    /// Create an If matcher
    pub fn if_matcher(
        condition: Box<NodeMatcher>,
        then_branch: Box<NodeMatcher>,
        else_branch: Box<NodeMatcher>,
    ) -> Self {
        NodeMatcher::Sequence(vec![
            NodeMatcher::NodeType("If".to_string()),
            *condition,
            *then_branch,
            *else_branch,
        ])
    }
}

/// Pattern analysis results
#[derive(Debug, Clone)]
pub struct PatternAnalysis {
    /// Detected patterns grouped by type
    pub patterns_by_type: HashMap<PatternType, Vec<DetectedPattern>>,
    /// Pattern frequency
    pub pattern_frequency: HashMap<PatternType, usize>,
    /// Suggested refactorings
    pub refactorings: Vec<RefactoringSuggestion>,
}

/// Refactoring suggestion based on patterns
#[derive(Debug, Clone)]
pub struct RefactoringSuggestion {
    /// Pattern that triggered the suggestion
    pub pattern: DetectedPattern,
    /// Refactoring type
    pub refactoring_type: RefactoringType,
    /// Description
    pub description: String,
    /// Estimated improvement
    pub improvement: f32,
}

/// Types of refactorings
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefactoringType {
    /// Convert to tail recursion
    TailRecursionConversion,
    /// Fuse map operations
    MapFusion,
    /// Parallelize independent operations
    Parallelization,
    /// Convert to actor pattern
    ActorConversion,
    /// Optimize pipeline
    PipelineOptimization,
}

impl PatternDetector {
    /// Analyze patterns and suggest refactorings
    pub fn analyze_patterns(&self, graph: &Graph) -> PatternAnalysis {
        let detected = self.detect_patterns(graph);
        
        // Group by type
        let mut patterns_by_type: HashMap<PatternType, Vec<DetectedPattern>> = HashMap::new();
        let mut pattern_frequency: HashMap<PatternType, usize> = HashMap::new();
        
        for pattern in detected {
            let pattern_type = pattern.template.pattern_type;
            patterns_by_type.entry(pattern_type).or_default().push(pattern.clone());
            *pattern_frequency.entry(pattern_type).or_default() += 1;
        }
        
        // Generate refactoring suggestions
        let refactorings = self.suggest_refactorings(&patterns_by_type);
        
        PatternAnalysis {
            patterns_by_type,
            pattern_frequency,
            refactorings,
        }
    }
    
    /// Suggest refactorings based on detected patterns
    fn suggest_refactorings(
        &self,
        patterns_by_type: &HashMap<PatternType, Vec<DetectedPattern>>,
    ) -> Vec<RefactoringSuggestion> {
        let mut suggestions = Vec::new();
        
        // Check for map fusion opportunities
        if let Some(maps) = patterns_by_type.get(&PatternType::FilterMap) {
            for pattern in maps {
                suggestions.push(RefactoringSuggestion {
                    pattern: pattern.clone(),
                    refactoring_type: RefactoringType::MapFusion,
                    description: "Fuse filter and map into a single operation".to_string(),
                    improvement: 0.3,
                });
            }
        }
        
        // Check for tail recursion conversion
        if let Some(recursions) = patterns_by_type.get(&PatternType::Recursion) {
            for pattern in recursions {
                if pattern.template.pattern_type != PatternType::TailRecursion {
                    suggestions.push(RefactoringSuggestion {
                        pattern: pattern.clone(),
                        refactoring_type: RefactoringType::TailRecursionConversion,
                        description: "Convert to tail recursion for better performance".to_string(),
                        improvement: 0.4,
                    });
                }
            }
        }
        
        suggestions
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{Node, Literal};
    
    #[test]
    fn test_pattern_detection() {
        let mut graph = Graph::new();
        
        // Create a simple map-reduce pattern
        let map_var = graph.add_node(Node::Variable { name: "map".to_string() }).unwrap();
        let reduce_var = graph.add_node(Node::Variable { name: "reduce".to_string() }).unwrap();
        let data = graph.add_node(Node::List(vec![])).unwrap();
        
        let map_app = graph.add_node(Node::Application {
            function: map_var,
            args: vec![data],
        }).unwrap();
        
        let initial = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
        let _reduce_app = graph.add_node(Node::Application {
            function: reduce_var,
            args: vec![map_app, initial],
        }).unwrap();
        
        let detector = PatternDetector::new(DetectionConfig::default());
        let patterns = detector.detect_patterns(&graph);
        
        // Should detect at least one pattern
        assert!(!patterns.is_empty());
    }
    
    #[test]
    fn test_pattern_analysis() {
        let mut graph = Graph::new();
        
        // Create multiple patterns
        for i in 0..3 {
            let filter = graph.add_node(Node::Variable { name: "filter".to_string() }).unwrap();
            let map = graph.add_node(Node::Variable { name: "map".to_string() }).unwrap();
            let data = graph.add_node(Node::Literal(Literal::Integer(i))).unwrap();
            
            let filter_app = graph.add_node(Node::Application {
                function: filter,
                args: vec![data],
            }).unwrap();
            
            let _map_app = graph.add_node(Node::Application {
                function: map,
                args: vec![filter_app],
            }).unwrap();
        }
        
        let detector = PatternDetector::new(DetectionConfig::default());
        let analysis = detector.analyze_patterns(&graph);
        
        // Should have detected filter-map patterns
        assert!(analysis.pattern_frequency.contains_key(&PatternType::FilterMap));
        
        // Should suggest refactorings
        assert!(!analysis.refactorings.is_empty());
    }
}