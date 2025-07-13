//! AST transformation and rewriting

use crate::error::{MetaprogrammingError, Result};
use crate::patterns::{Pattern, PatternMatcher};
use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

/// A transformation rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransformRule {
    /// Name of the rule
    pub name: String,
    /// Pattern to match
    pub pattern: Pattern,
    /// Transformation to apply
    pub transform: Transform,
    /// Whether to apply recursively
    pub recursive: bool,
}

/// Types of transformations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Transform {
    /// Replace with a template
    Replace(String),
    /// Apply a function
    Function(String),
    /// Compose multiple transforms
    Compose(Vec<Transform>),
    /// Conditional transform
    Conditional {
        condition: String,
        then_transform: Box<Transform>,
        else_transform: Option<Box<Transform>>,
    },
    /// Rename variables
    RenameVars(FxHashMap<String, String>),
    /// Inline a binding
    Inline(String),
    /// Extract to a new binding
    Extract { name: String, binding_point: String },
}

/// AST transformer
pub struct Transformer {
    /// Registered transformation rules
    rules: Vec<TransformRule>,
    /// Custom transformation functions
    functions: FxHashMap<String, Box<dyn Fn(&Graph, NodeId) -> Result<Graph>>>,
}

impl Transformer {
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
            functions: FxHashMap::default(),
        }
    }

    /// Register a transformation rule
    pub fn register_rule(&mut self, rule: TransformRule) {
        self.rules.push(rule);
    }

    /// Register a custom transformation function
    pub fn register_function<F>(&mut self, name: impl Into<String>, f: F)
    where
        F: Fn(&Graph, NodeId) -> Result<Graph> + 'static,
    {
        self.functions.insert(name.into(), Box::new(f));
    }

    /// Apply all matching rules to a graph
    pub fn transform_graph(&self, graph: &mut Graph) -> Result<()> {
        let mut changed = true;
        let mut iterations = 0;
        const MAX_ITERATIONS: usize = 100;

        while changed && iterations < MAX_ITERATIONS {
            changed = false;
            iterations += 1;

            for rule in &self.rules {
                if self.apply_rule(graph, rule)? {
                    changed = true;
                }
            }
        }

        if iterations >= MAX_ITERATIONS {
            return Err(MetaprogrammingError::TransformError(
                "Maximum transformation iterations exceeded".to_string(),
            ));
        }

        Ok(())
    }

    /// Apply a single rule to the graph
    fn apply_rule(&self, graph: &mut Graph, rule: &TransformRule) -> Result<bool> {
        let mut matcher = PatternMatcher::new(graph);
        let mut transformations = Vec::new();

        // Find all matches
        for &node_id in graph.nodes.keys() {
            if let Some(result) = matcher.match_pattern(&rule.pattern, node_id) {
                transformations.push((node_id, result));
            }
        }

        // Apply transformations
        let mut changed = false;
        for (node_id, match_result) in transformations {
            let transformed =
                self.apply_transform(graph, node_id, &rule.transform, &match_result)?;
            if let Some(new_graph) = transformed {
                self.integrate_transformation(graph, node_id, new_graph)?;
                changed = true;

                if !rule.recursive {
                    break;
                }
            }
        }

        Ok(changed)
    }

    /// Apply a transformation
    fn apply_transform(
        &self,
        graph: &Graph,
        node_id: NodeId,
        transform: &Transform,
        match_result: &crate::patterns::MatchResult,
    ) -> Result<Option<Graph>> {
        match transform {
            Transform::Replace(template) => {
                let expanded = self.expand_template(template, graph, match_result)?;
                Ok(Some(expanded))
            }

            Transform::Function(name) => {
                if let Some(f) = self.functions.get(name) {
                    Ok(Some(f(graph, node_id)?))
                } else {
                    Err(MetaprogrammingError::TransformError(format!(
                        "Unknown transform function: {}",
                        name
                    )))
                }
            }

            Transform::Compose(transforms) => {
                let mut current = None;
                let mut working_graph = graph.clone();

                for t in transforms {
                    if let Some(g) = current {
                        working_graph = g;
                    }

                    current = self.apply_transform(&working_graph, node_id, t, match_result)?;
                    if current.is_none() {
                        return Ok(None);
                    }
                }

                Ok(current)
            }

            Transform::Conditional {
                condition,
                then_transform,
                else_transform,
            } => {
                if self.evaluate_condition(condition, graph, match_result)? {
                    self.apply_transform(graph, node_id, then_transform, match_result)
                } else if let Some(else_t) = else_transform {
                    self.apply_transform(graph, node_id, else_t, match_result)
                } else {
                    Ok(None)
                }
            }

            Transform::RenameVars(mapping) => {
                let renamed = self.rename_variables(graph, node_id, mapping)?;
                Ok(Some(renamed))
            }

            Transform::Inline(var_name) => {
                let inlined = self.inline_variable(graph, node_id, var_name)?;
                Ok(Some(inlined))
            }

            Transform::Extract {
                name,
                binding_point,
            } => {
                let extracted = self.extract_expression(graph, node_id, name, binding_point)?;
                Ok(Some(extracted))
            }
        }
    }

    /// Expand a template with substitutions
    fn expand_template(
        &self,
        template: &str,
        graph: &Graph,
        match_result: &crate::patterns::MatchResult,
    ) -> Result<Graph> {
        let mut expanded = template.to_string();

        // Substitute matched bindings
        for (name, &node_id) in &match_result.bindings {
            let node_str = self.node_to_string(graph, node_id)?;
            expanded = expanded.replace(&format!("${}", name), &node_str);
        }

        fluentai_parser::parse_flc(&expanded)
            .map_err(|e| MetaprogrammingError::ParseError(e.to_string()))
    }

    /// Convert node to string
    fn node_to_string(&self, graph: &Graph, node_id: NodeId) -> Result<String> {
        // Simplified serialization
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Variable { name } => Ok(name.clone()),
                Node::Literal(lit) => Ok(format!("{}", lit)),
                Node::List(items) => {
                    let item_strs: Result<Vec<_>> = items
                        .iter()
                        .map(|&id| self.node_to_string(graph, id))
                        .collect();
                    Ok(format!("({})", item_strs?.join(" ")))
                }
                _ => Ok(format!("<node:{:?}>", node_id)),
            }
        } else {
            Err(MetaprogrammingError::NodeNotFound(node_id))
        }
    }

    /// Evaluate a condition
    fn evaluate_condition(
        &self,
        condition: &str,
        _graph: &Graph,
        _match_result: &crate::patterns::MatchResult,
    ) -> Result<bool> {
        // Simplified condition evaluation
        match condition {
            "always" => Ok(true),
            "never" => Ok(false),
            _ => Ok(false),
        }
    }

    /// Rename variables in a subgraph
    fn rename_variables(
        &self,
        graph: &Graph,
        root_id: NodeId,
        mapping: &FxHashMap<String, String>,
    ) -> Result<Graph> {
        let mut new_graph = Graph::new();
        let mut id_mapping = FxHashMap::default();

        // Clone nodes with renaming
        self.clone_with_renaming(graph, root_id, &mut new_graph, &mut id_mapping, mapping)?;

        new_graph.root_id = id_mapping.get(&root_id).copied();
        Ok(new_graph)
    }

    fn clone_with_renaming(
        &self,
        source: &Graph,
        node_id: NodeId,
        target: &mut Graph,
        id_mapping: &mut FxHashMap<NodeId, NodeId>,
        var_mapping: &FxHashMap<String, String>,
    ) -> Result<NodeId> {
        if let Some(&new_id) = id_mapping.get(&node_id) {
            return Ok(new_id);
        }

        let node = source
            .get_node(node_id)
            .ok_or_else(|| MetaprogrammingError::NodeNotFound(node_id))?;

        let new_node = match node {
            Node::Variable { name } => Node::Variable {
                name: var_mapping
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| name.clone()),
            },
            Node::Lambda { params, body } => {
                let new_params = params
                    .iter()
                    .map(|p| var_mapping.get(p).cloned().unwrap_or_else(|| p.clone()))
                    .collect();
                let new_body =
                    self.clone_with_renaming(source, *body, target, id_mapping, var_mapping)?;
                Node::Lambda {
                    params: new_params,
                    body: new_body,
                }
            }
            _ => node.clone(), // Simplified - would need to handle all node types
        };

        let new_id = target.add_node(new_node)?;
        id_mapping.insert(node_id, new_id);
        Ok(new_id)
    }

    /// Inline a variable
    fn inline_variable(&self, _graph: &Graph, _node_id: NodeId, _var_name: &str) -> Result<Graph> {
        // Placeholder implementation
        Err(MetaprogrammingError::TransformError(
            "Variable inlining not yet implemented".to_string(),
        ))
    }

    /// Extract an expression to a binding
    fn extract_expression(
        &self,
        _graph: &Graph,
        _node_id: NodeId,
        _name: &str,
        _binding_point: &str,
    ) -> Result<Graph> {
        // Placeholder implementation
        Err(MetaprogrammingError::TransformError(
            "Expression extraction not yet implemented".to_string(),
        ))
    }

    /// Integrate a transformation back into the graph
    fn integrate_transformation(
        &self,
        graph: &mut Graph,
        old_root: NodeId,
        new_graph: Graph,
    ) -> Result<()> {
        // Add new nodes
        let mut id_mapping = FxHashMap::default();
        for (old_id, node) in new_graph.nodes {
            let new_id = graph.add_node(node)?;
            id_mapping.insert(old_id, new_id);
        }

        // Create a proper mapping for update_node_ids
        let node_id_mapping: FxHashMap<NodeId, NodeId> =
            id_mapping.iter().map(|(k, v)| (*k, *v)).collect();

        // Update references in new nodes
        for &new_id in id_mapping.values() {
            if let Some(node) = graph.nodes.get_mut(&new_id) {
                self.update_node_ids(node, &node_id_mapping);
            }
        }

        // Replace references to old root
        if let Some(&new_root) = new_graph.root_id.and_then(|id| id_mapping.get(&id)) {
            for node in graph.nodes.values_mut() {
                self.replace_node_id(node, old_root, new_root);
            }

            if graph.root_id == Some(old_root) {
                graph.root_id = Some(new_root);
            }
        }

        // Remove old root
        graph.nodes.remove(&old_root);

        Ok(())
    }

    fn update_node_ids(&self, node: &mut Node, mapping: &FxHashMap<NodeId, NodeId>) {
        match node {
            Node::Lambda { body, .. } => {
                if let Some(&new_id) = mapping.get(body) {
                    *body = new_id;
                }
            }
            Node::Application { function, args } => {
                if let Some(&new_id) = mapping.get(function) {
                    *function = new_id;
                }
                for arg in args {
                    if let Some(&new_id) = mapping.get(arg) {
                        *arg = new_id;
                    }
                }
            }
            // Handle other node types...
            _ => {}
        }
    }

    fn replace_node_id(&self, node: &mut Node, old_id: NodeId, new_id: NodeId) {
        match node {
            Node::Lambda { body, .. } => {
                if *body == old_id {
                    *body = new_id;
                }
            }
            Node::Application { function, args } => {
                if *function == old_id {
                    *function = new_id;
                }
                for arg in args {
                    if *arg == old_id {
                        *arg = new_id;
                    }
                }
            }
            // Handle other node types...
            _ => {}
        }
    }
}
