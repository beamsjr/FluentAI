//! Continuum UI lowering pass - transforms declarative UI syntax into imperative code

use anyhow::Result;
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use rustc_hash::FxHashMap;
use crate::passes::OptimizationPass;

/// Lowers Continuum UI constructs into regular FluentAI code
pub struct ContinuumLowering {
    /// Track state fields and their types
    state_fields: FxHashMap<String, StateFieldInfo>,
    /// Generated initialization code
    init_code: Vec<NodeId>,
    /// Statistics
    surfaces_lowered: usize,
    elements_lowered: usize,
    state_fields_lowered: usize,
}

#[derive(Clone)]
struct StateFieldInfo {
    name: String,
    field_type: Option<String>,
    initial_value: Option<NodeId>,
}

impl ContinuumLowering {
    pub fn new() -> Self {
        Self {
            state_fields: FxHashMap::default(),
            init_code: Vec::new(),
            surfaces_lowered: 0,
            elements_lowered: 0,
            state_fields_lowered: 0,
        }
    }

    pub fn lower(&mut self, graph: &Graph) -> Result<Graph> {
        let mut new_graph = Graph::new();
        
        // First pass: collect all state fields from original graph
        self.collect_state_fields(graph)?;
        
        // Generate state management code
        self.generate_state_management(&mut new_graph)?;
        
        // Second pass: transform UI nodes
        if let Some(root) = graph.root_id {
            let new_root = self.transform_node(graph, &mut new_graph, root)?;
            
            // Combine initialization code with transformed code
            if !self.init_code.is_empty() {
                let mut all_exprs = self.init_code.clone();
                all_exprs.push(new_root);
                let begin_node = new_graph.add_node(Node::Begin { exprs: all_exprs })?;
                new_graph.root_id = Some(begin_node);
            } else {
                new_graph.root_id = Some(new_root);
            }
        }
        
        Ok(new_graph)
    }

    fn collect_state_fields(&mut self, graph: &Graph) -> Result<()> {
        let mut stack = vec![];
        if let Some(root) = graph.root_id {
            stack.push(root);
        }
        
        while let Some(node_id) = stack.pop() {
            if let Some(node) = graph.get_node(node_id) {
                match node {
                    Node::StateField { name, field_type, initial } => {
                        let initial_value = *initial;
                        
                        self.state_fields.insert(name.clone(), StateFieldInfo {
                            name: name.clone(),
                            field_type: field_type.clone(),
                            initial_value,
                        });
                    }
                    _ => {
                        self.collect_child_nodes(node, &mut stack);
                    }
                }
            }
        }
        
        Ok(())
    }

    fn generate_state_management(&mut self, new_graph: &mut Graph) -> Result<()> {
        for (_name, info) in &self.state_fields {
            self.state_fields_lowered += 1;
            
            // For now, create simple variable bindings
            let initial = if let Some(init_id) = info.initial_value {
                // TODO: Copy the initial value node from old graph
                new_graph.add_node(Node::Literal(Literal::Integer(0)))?
            } else {
                // Default value based on type
                match info.field_type.as_deref() {
                    Some("int") => new_graph.add_node(Node::Literal(Literal::Integer(0)))?,
                    Some("string") => new_graph.add_node(Node::Literal(Literal::String(String::new())))?,
                    Some("bool") => new_graph.add_node(Node::Literal(Literal::Boolean(false)))?,
                    _ => new_graph.add_node(Node::Literal(Literal::Nil))?,
                }
            };
            let binding = (info.name.clone(), initial);
            let nil = new_graph.add_node(Node::Literal(Literal::Nil))?;
            let let_node = new_graph.add_node(Node::Let {
                bindings: vec![binding],
                body: nil,
            })?;
            
            self.init_code.push(let_node);
        }
        
        Ok(())
    }

    fn transform_node(&mut self, old_graph: &Graph, new_graph: &mut Graph, node_id: NodeId) -> Result<NodeId> {
        let node = old_graph.get_node(node_id).ok_or_else(|| anyhow::anyhow!("Invalid node"))?.clone();
        
        match node {
            Node::Surface { name, properties, children } => {
                self.transform_surface(old_graph, new_graph, &name, &properties, &children)
            }
            Node::Element { element_type, properties, handlers, .. } => {
                self.transform_element(old_graph, new_graph, element_type.as_deref(), &properties, &handlers)
            }
            Node::StateField { .. } => {
                // State fields are handled in generate_state_management
                Ok(new_graph.add_node(Node::Literal(Literal::Nil))?)
            }
            Node::When { condition, properties } => {
                self.transform_when(old_graph, new_graph, condition, &properties)
            }
            Node::Disturb { field, value } => {
                self.transform_disturb(old_graph, new_graph, &field, value)
            }
            _ => {
                // Non-Continuum nodes pass through with transformed children
                self.copy_node_with_transformed_children(old_graph, new_graph, node_id)
            }
        }
    }

    fn transform_surface(&mut self, old_graph: &Graph, new_graph: &mut Graph, name: &str, properties: &[(String, NodeId)], children: &[NodeId]) -> Result<NodeId> {
        self.surfaces_lowered += 1;
        
        // Transform children
        let mut child_nodes = vec![];
        for child_id in children {
            let transformed = self.transform_node(old_graph, new_graph, *child_id)?;
            child_nodes.push(transformed);
        }
        
        // Create a begin node with all children
        if child_nodes.is_empty() {
            Ok(new_graph.add_node(Node::Literal(Literal::Nil))?)
        } else {
            Ok(new_graph.add_node(Node::Begin { exprs: child_nodes })?)
        }
    }

    fn transform_element(&mut self, old_graph: &Graph, new_graph: &mut Graph, element_type: Option<&str>, properties: &[(String, NodeId)], handlers: &[(String, NodeId)]) -> Result<NodeId> {
        self.elements_lowered += 1;
        
        let elem_type = element_type.unwrap_or("div");
        
        // Create element type string
        let type_node = new_graph.add_node(Node::Literal(Literal::String(elem_type.to_string())))?;
        
        // Build properties list
        let mut prop_list = vec![];
        for (key, value_id) in properties {
            let key_str = new_graph.add_node(Node::Literal(Literal::String(key.clone())))?;
            let value = self.transform_node(old_graph, new_graph, *value_id)?;
            prop_list.push(key_str);
            prop_list.push(value);
        }
        
        // Create property list node
        let props_node = new_graph.add_node(Node::List(prop_list))?;
        
        // Create Dom.create_element effect
        let dom_var = new_graph.add_node(Node::Variable { name: "Dom".to_string() })?;
        let create_elem_var = new_graph.add_node(Node::Variable { name: "create_element".to_string() })?;
        
        // For now, create a mock element since Effect expects EffectType enum
        // In a full implementation, we'd need to handle Dom effects properly
        let effect_result = new_graph.add_node(Node::Application {
            function: create_elem_var,
            args: vec![type_node, props_node],
        })?;
        
        Ok(effect_result)
    }

    fn transform_when(&mut self, old_graph: &Graph, new_graph: &mut Graph, condition: NodeId, properties: &[(String, NodeId)]) -> Result<NodeId> {
        let cond = self.transform_node(old_graph, new_graph, condition)?;
        let nil = new_graph.add_node(Node::Literal(Literal::Nil))?;
        
        Ok(new_graph.add_node(Node::If {
            condition: cond,
            then_branch: nil,
            else_branch: nil,
        })?)
    }

    fn transform_disturb(&mut self, old_graph: &Graph, new_graph: &mut Graph, field: &str, value: Option<NodeId>) -> Result<NodeId> {
        // For now, just return nil
        Ok(new_graph.add_node(Node::Literal(Literal::Nil))?)
    }

    fn copy_node_with_transformed_children(&mut self, old_graph: &Graph, new_graph: &mut Graph, node_id: NodeId) -> Result<NodeId> {
        let node = old_graph.get_node(node_id).ok_or_else(|| anyhow::anyhow!("Invalid node"))?.clone();
        
        let new_node = match node {
            Node::Let { bindings, body } => {
                let mut new_bindings = vec![];
                for (name, value_id) in bindings {
                    let new_value = self.transform_node(old_graph, new_graph, value_id)?;
                    new_bindings.push((name, new_value));
                }
                let new_body = self.transform_node(old_graph, new_graph, body)?;
                Node::Let { bindings: new_bindings, body: new_body }
            }
            Node::Application { function, args } => {
                let new_fn = self.transform_node(old_graph, new_graph, function)?;
                let mut new_args = vec![];
                for arg in args {
                    new_args.push(self.transform_node(old_graph, new_graph, arg)?);
                }
                Node::Application { function: new_fn, args: new_args }
            }
            Node::Lambda { params, body } => {
                let new_body = self.transform_node(old_graph, new_graph, body)?;
                Node::Lambda { params, body: new_body }
            }
            Node::If { condition, then_branch, else_branch } => {
                let new_cond = self.transform_node(old_graph, new_graph, condition)?;
                let new_then = self.transform_node(old_graph, new_graph, then_branch)?;
                let new_else = self.transform_node(old_graph, new_graph, else_branch)?;
                Node::If { condition: new_cond, then_branch: new_then, else_branch: new_else }
            }
            Node::Begin { exprs } => {
                let mut new_exprs = vec![];
                for expr in exprs {
                    new_exprs.push(self.transform_node(old_graph, new_graph, expr)?);
                }
                Node::Begin { exprs: new_exprs }
            }
            Node::List(items) => {
                let mut new_items = vec![];
                for item in items {
                    new_items.push(self.transform_node(old_graph, new_graph, item)?);
                }
                Node::List(new_items)
            }
            // Literals and variables pass through unchanged
            Node::Literal(_) | Node::Variable { .. } => node,
            // For other nodes, just copy as-is for now
            _ => node,
        };
        
        Ok(new_graph.add_node(new_node)?)
    }

    fn collect_child_nodes(&self, node: &Node, stack: &mut Vec<NodeId>) {
        match node {
            Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                for (_, value_id) in bindings {
                    stack.push(*value_id);
                }
                stack.push(*body);
            }
            Node::Application { function, args } => {
                stack.push(*function);
                stack.extend(args);
            }
            Node::Lambda { body, .. } => stack.push(*body),
            Node::If { condition, then_branch, else_branch } => {
                stack.push(*condition);
                stack.push(*then_branch);
                stack.push(*else_branch);
            }
            Node::Begin { exprs } => stack.extend(exprs),
            Node::List(items) => stack.extend(items),
            _ => {} // Other nodes have no children to traverse
        }
    }
}

impl OptimizationPass for ContinuumLowering {
    fn name(&self) -> &str {
        "continuum-lowering"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.lower(graph)
    }

    fn stats(&self) -> String {
        format!(
            "Continuum lowering: {} surfaces, {} elements, {} state fields transformed",
            self.surfaces_lowered,
            self.elements_lowered,
            self.state_fields_lowered
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_lowering() {
        // TODO: Add tests
    }
}