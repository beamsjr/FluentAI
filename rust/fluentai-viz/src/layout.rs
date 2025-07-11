//! Graph layout algorithms for AST visualization

use fluentai_core::ast::{Graph, Node, NodeId};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Position of a node in 2D space
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Position {
    pub x: f64,
    pub y: f64,
}

/// Layout information for a node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeLayout {
    pub id: u32,
    pub position: Position,
    pub label: String,
    pub node_type: String,
    pub children: Vec<u32>,
}

/// Edge in the graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Edge {
    pub source: u32,
    pub target: u32,
    pub label: Option<String>,
}

/// Complete graph layout
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphLayout {
    pub nodes: Vec<NodeLayout>,
    pub edges: Vec<Edge>,
    pub width: f64,
    pub height: f64,
}

/// Layout algorithm for AST graphs
pub struct ASTLayouter {
    node_width: f64,
    node_height: f64,
    horizontal_spacing: f64,
    vertical_spacing: f64,
}

impl Default for ASTLayouter {
    fn default() -> Self {
        Self {
            node_width: 120.0,
            node_height: 60.0,
            horizontal_spacing: 30.0,
            vertical_spacing: 80.0,
        }
    }
}

impl ASTLayouter {
    /// Layout the AST graph
    pub fn layout(&self, graph: &Graph) -> GraphLayout {
        let mut nodes = Vec::new();
        let mut edges = Vec::new();

        if let Some(root_id) = graph.root_id {
            let mut positions = HashMap::new();
            let mut visited = HashSet::new();

            // Perform tree layout starting from root
            let (width, height) =
                self.layout_subtree(graph, root_id, 0.0, 0.0, &mut positions, &mut visited);

            // Convert to node layouts
            for (node_id, position) in positions {
                if let Some(node) = graph.get_node(node_id) {
                    let children = self.get_children(node);
                    nodes.push(NodeLayout {
                        id: node_id.get(),
                        position,
                        label: self.get_node_label(node),
                        node_type: self.get_node_type(node),
                        children: children.iter().map(|id| id.get()).collect(),
                    });

                    // Create edges
                    for child_id in children {
                        edges.push(Edge {
                            source: node_id.get(),
                            target: child_id.get(),
                            label: None,
                        });
                    }
                }
            }

            GraphLayout {
                nodes,
                edges,
                width: width + self.node_width,
                height: height + self.node_height,
            }
        } else {
            GraphLayout {
                nodes,
                edges,
                width: 0.0,
                height: 0.0,
            }
        }
    }

    /// Layout a subtree recursively
    fn layout_subtree(
        &self,
        graph: &Graph,
        node_id: NodeId,
        x: f64,
        y: f64,
        positions: &mut HashMap<NodeId, Position>,
        visited: &mut HashSet<NodeId>,
    ) -> (f64, f64) {
        if visited.contains(&node_id) {
            return (0.0, 0.0);
        }
        visited.insert(node_id);

        positions.insert(node_id, Position { x, y });

        if let Some(node) = graph.get_node(node_id) {
            let children = self.get_children(node);

            if children.is_empty() {
                (self.node_width, self.node_height)
            } else {
                let mut child_x = x;
                let child_y = y + self.node_height + self.vertical_spacing;
                let mut max_height: f64 = 0.0;
                let mut total_width = 0.0;

                for (i, &child_id) in children.iter().enumerate() {
                    if i > 0 {
                        child_x += self.horizontal_spacing;
                    }

                    let (child_width, child_height) =
                        self.layout_subtree(graph, child_id, child_x, child_y, positions, visited);

                    child_x += child_width;
                    max_height = max_height.max(child_height);
                    total_width += child_width;
                    if i > 0 {
                        total_width += self.horizontal_spacing;
                    }
                }

                // Center parent node over children
                let center_x = x + total_width / 2.0 - self.node_width / 2.0;
                positions.insert(node_id, Position { x: center_x, y });

                (
                    total_width,
                    self.node_height + self.vertical_spacing + max_height,
                )
            }
        } else {
            (self.node_width, self.node_height)
        }
    }

    /// Get children node IDs for a node
    fn get_children(&self, node: &Node) -> Vec<NodeId> {
        match node {
            Node::Lambda { body, .. } => vec![*body],
            Node::Let { bindings, body } => {
                let mut children: Vec<NodeId> = bindings.iter().map(|(_, id)| *id).collect();
                children.push(*body);
                children
            }
            Node::Letrec { bindings, body } => {
                let mut children: Vec<NodeId> = bindings.iter().map(|(_, id)| *id).collect();
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
            Node::Application { function, args } => {
                let mut children = vec![*function];
                children.extend(args);
                children
            }
            Node::List(items) => items.clone(),
            Node::Effect { args, .. } => args.clone(),
            Node::Match { expr, branches } => {
                let mut children = vec![*expr];
                children.extend(branches.iter().map(|(_, body)| *body));
                children
            }
            Node::Module { body, .. } => vec![*body],
            Node::Async { body } => vec![*body],
            Node::Await { expr } => vec![*expr],
            Node::Spawn { expr } => vec![*expr],
            Node::Send { channel, value } => vec![*channel, *value],
            Node::Receive { channel } => vec![*channel],
            Node::TrySend { channel, value } => vec![*channel, *value],
            Node::TryReceive { channel } => vec![*channel],
            Node::Contract {
                preconditions,
                postconditions,
                invariants,
                ..
            } => {
                let mut children = Vec::new();
                children.extend(preconditions);
                children.extend(postconditions);
                children.extend(invariants);
                children
            }
            _ => vec![],
        }
    }

    /// Get a display label for a node
    fn get_node_label(&self, node: &Node) -> String {
        match node {
            Node::Literal(lit) => format!("{:?}", lit),
            Node::Variable { name } => name.clone(),
            Node::Lambda { params, .. } => format!("λ {}", params.join(" ")),
            Node::Let { .. } => "let".to_string(),
            Node::Letrec { .. } => "letrec".to_string(),
            Node::If { .. } => "if".to_string(),
            Node::Application { .. } => "apply".to_string(),
            Node::Effect {
                effect_type,
                operation,
                ..
            } => {
                format!("{}:{}", effect_type, operation)
            }
            Node::List(_) => "list".to_string(),
            Node::Match { .. } => "match".to_string(),
            Node::Module { name, .. } => format!("module {}", name),
            Node::Import { module_path, .. } => format!("import {}", module_path),
            Node::Export { .. } => "export".to_string(),
            Node::QualifiedVariable {
                module_name,
                variable_name,
            } => {
                format!("{}.{}", module_name, variable_name)
            }
            Node::Async { .. } => "async".to_string(),
            Node::Await { .. } => "await".to_string(),
            Node::Spawn { .. } => "spawn".to_string(),
            Node::Channel { .. } => "channel".to_string(),
            Node::Send { .. } => "send!".to_string(),
            Node::Receive { .. } => "recv!".to_string(),
            Node::TrySend { .. } => "try-send!".to_string(),
            Node::TryReceive { .. } => "try-recv!".to_string(),
            Node::Contract { function_name, .. } => format!("contract {}", function_name),
            Node::Handler { handlers, .. } => {
                let handler_count = handlers.len();
                format!("handler ({})", handler_count)
            }
            Node::Define { name, .. } => format!("define {}", name),
            Node::Begin { .. } => "begin".to_string(),
            Node::Select { .. } => "select".to_string(),
            Node::Actor { .. } => "actor".to_string(),
            Node::ActorSend { .. } => "!".to_string(),
            Node::ActorReceive { .. } => "receive".to_string(),
            Node::Become { .. } => "become".to_string(),
            Node::Try { .. } => "try".to_string(),
            Node::Throw { .. } => "throw".to_string(),
            Node::Promise { .. } => "promise".to_string(),
            Node::PromiseAll { .. } => "promise-all".to_string(),
            Node::PromiseRace { .. } => "promise-race".to_string(),
            Node::Timeout { .. } => "with-timeout".to_string(),
            Node::Assignment { .. } => "=".to_string(),
        }
    }

    /// Get the node type as a string
    fn get_node_type(&self, node: &Node) -> String {
        match node {
            Node::Literal(_) => "literal",
            Node::Variable { .. } => "variable",
            Node::Lambda { .. } => "lambda",
            Node::Let { .. } => "let",
            Node::Letrec { .. } => "letrec",
            Node::If { .. } => "if",
            Node::Application { .. } => "application",
            Node::Effect { .. } => "effect",
            Node::List(_) => "list",
            Node::Match { .. } => "match",
            Node::Module { .. } => "module",
            Node::Import { .. } => "import",
            Node::Export { .. } => "export",
            Node::QualifiedVariable { .. } => "qualified",
            Node::Async { .. } => "async",
            Node::Await { .. } => "await",
            Node::Spawn { .. } => "spawn",
            Node::Channel { .. } => "channel",
            Node::Send { .. } => "send",
            Node::Receive { .. } => "receive",
            Node::TrySend { .. } => "try-send",
            Node::TryReceive { .. } => "try-receive",
            Node::Contract { .. } => "contract",
            Node::Handler { .. } => "handler",
            Node::Define { .. } => "define",
            Node::Begin { .. } => "begin",
            Node::Select { .. } => "select",
            Node::Actor { .. } => "actor",
            Node::ActorSend { .. } => "actor-send",
            Node::ActorReceive { .. } => "actor-receive",
            Node::Become { .. } => "become",
            Node::Try { .. } => "try",
            Node::Throw { .. } => "throw",
            Node::Promise { .. } => "promise",
            Node::PromiseAll { .. } => "promise-all",
            Node::PromiseRace { .. } => "promise-race",
            Node::Timeout { .. } => "timeout",
            Node::Assignment { .. } => "assignment",
        }
        .to_string()
    }
}
