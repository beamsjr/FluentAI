//! Scene graph management

use crate::primitives::Renderable;
use std::collections::HashMap;

/// Unique identifier for scene nodes
pub type NodeId = usize;

/// A node in the scene graph
#[derive(Debug, Clone)]
pub struct SceneNode {
    /// Unique identifier
    pub id: NodeId,
    /// The renderable content
    pub renderable: Renderable,
    /// Child node IDs
    pub children: Vec<NodeId>,
    /// Whether this node is visible
    pub visible: bool,
}

/// Scene graph for managing renderable elements
#[derive(Debug, Default)]
pub struct Scene {
    /// All nodes in the scene
    nodes: HashMap<NodeId, SceneNode>,
    /// Root node IDs (nodes without parents)
    roots: Vec<NodeId>,
    /// Counter for generating unique IDs
    next_id: NodeId,
}

impl Scene {
    /// Create a new empty scene
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a renderable to the scene
    pub fn add(&mut self, renderable: Renderable) -> NodeId {
        let id = self.next_id;
        self.next_id += 1;

        let node = SceneNode {
            id,
            renderable,
            children: Vec::new(),
            visible: true,
        };

        self.nodes.insert(id, node);
        self.roots.push(id);
        
        id
    }

    /// Add a child node to a parent
    pub fn add_child(&mut self, parent_id: NodeId, child: Renderable) -> Option<NodeId> {
        if !self.nodes.contains_key(&parent_id) {
            return None;
        }

        let child_id = self.next_id;
        self.next_id += 1;

        let node = SceneNode {
            id: child_id,
            renderable: child,
            children: Vec::new(),
            visible: true,
        };

        self.nodes.insert(child_id, node);
        
        // Add to parent's children
        if let Some(parent) = self.nodes.get_mut(&parent_id) {
            parent.children.push(child_id);
        }

        // Remove from roots if it was there
        self.roots.retain(|&id| id != child_id);

        Some(child_id)
    }

    /// Remove a node and all its children
    pub fn remove(&mut self, node_id: NodeId) {
        if let Some(node) = self.nodes.remove(&node_id) {
            // Recursively remove children
            for child_id in node.children {
                self.remove(child_id);
            }
        }

        // Remove from roots
        self.roots.retain(|&id| id != node_id);

        // Remove from any parent's children list
        for node in self.nodes.values_mut() {
            node.children.retain(|&id| id != node_id);
        }
    }

    /// Get a node by ID
    pub fn get(&self, node_id: NodeId) -> Option<&SceneNode> {
        self.nodes.get(&node_id)
    }

    /// Get a mutable node by ID
    pub fn get_mut(&mut self, node_id: NodeId) -> Option<&mut SceneNode> {
        self.nodes.get_mut(&node_id)
    }

    /// Set visibility of a node
    pub fn set_visible(&mut self, node_id: NodeId, visible: bool) {
        if let Some(node) = self.nodes.get_mut(&node_id) {
            node.visible = visible;
        }
    }

    /// Clear the entire scene
    pub fn clear(&mut self) {
        self.nodes.clear();
        self.roots.clear();
        self.next_id = 0;
    }

    /// Get all renderables in draw order (depth-first traversal)
    pub fn get_renderables(&self) -> Vec<&Renderable> {
        let mut renderables = Vec::new();
        
        for &root_id in &self.roots {
            self.collect_renderables(root_id, &mut renderables);
        }
        
        renderables
    }

    /// Recursive helper to collect renderables
    fn collect_renderables<'a>(&'a self, node_id: NodeId, renderables: &mut Vec<&'a Renderable>) {
        if let Some(node) = self.nodes.get(&node_id) {
            if node.visible {
                renderables.push(&node.renderable);
                
                for &child_id in &node.children {
                    self.collect_renderables(child_id, renderables);
                }
            }
        }
    }

    /// Get root nodes
    pub fn roots(&self) -> &[NodeId] {
        &self.roots
    }

    /// Get total number of nodes
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
}