//! Macro system for compile-time code generation

use crate::error::{MetaprogrammingError, Result};
use crate::patterns::{Pattern, PatternMatcher};
use fluentai_core::ast::{Graph, Node, NodeId};
use fluentai_parser::parse;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

/// A macro definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MacroDefinition {
    /// Name of the macro
    pub name: String,
    /// Parameters
    pub params: Vec<String>,
    /// Pattern to match
    pub pattern: Option<Pattern>,
    /// Body template
    pub body: String,
    /// Whether this is a hygenic macro
    pub hygenic: bool,
}

/// Macro expansion context
#[derive(Debug)]
pub struct ExpansionContext {
    /// Bound variables from pattern matching
    pub bindings: FxHashMap<String, NodeId>,
    /// Arguments passed to macro
    pub arguments: FxHashMap<String, NodeId>,
    /// Generated symbols for hygiene
    pub gensym_counter: usize,
}

impl ExpansionContext {
    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
            arguments: FxHashMap::default(),
            gensym_counter: 0,
        }
    }
    
    /// Generate a unique symbol
    pub fn gensym(&mut self, prefix: &str) -> String {
        let sym = format!("{}#{}", prefix, self.gensym_counter);
        self.gensym_counter += 1;
        sym
    }
}

/// Macro expander
pub struct MacroExpander {
    /// Registered macros
    macros: FxHashMap<String, MacroDefinition>,
    /// Expansion depth limit to prevent infinite recursion
    max_depth: usize,
}

impl MacroExpander {
    pub fn new() -> Self {
        Self {
            macros: FxHashMap::default(),
            max_depth: 100,
        }
    }
    
    /// Register a macro
    pub fn register_macro(&mut self, macro_def: MacroDefinition) {
        self.macros.insert(macro_def.name.clone(), macro_def);
    }
    
    /// Register built-in macros
    pub fn register_builtins(&mut self) {
        // when macro: (when condition body) -> (if condition body nil)
        self.register_macro(MacroDefinition {
            name: "when".to_string(),
            params: vec!["condition".to_string(), "body".to_string()],
            pattern: None,
            body: "(if $condition $body nil)".to_string(),
            hygenic: true,
        });
        
        // unless macro: (unless condition body) -> (if condition nil body)
        self.register_macro(MacroDefinition {
            name: "unless".to_string(),
            params: vec!["condition".to_string(), "body".to_string()],
            pattern: None,
            body: "(if $condition nil $body)".to_string(),
            hygenic: true,
        });
        
        // cond macro: (cond (test1 body1) (test2 body2) ...) -> nested ifs
        self.register_macro(MacroDefinition {
            name: "cond".to_string(),
            params: vec!["clauses".to_string()],
            pattern: None,
            body: "cond-expansion".to_string(), // Special handling
            hygenic: true,
        });
        
        // let* macro: sequential let bindings
        self.register_macro(MacroDefinition {
            name: "let*".to_string(),
            params: vec!["bindings".to_string(), "body".to_string()],
            pattern: None,
            body: "let*-expansion".to_string(), // Special handling
            hygenic: true,
        });
    }
    
    /// Expand macros in a graph
    pub fn expand_graph(&self, graph: &mut Graph) -> Result<()> {
        self.expand_graph_recursive(graph, 0)
    }
    
    fn expand_graph_recursive(&self, graph: &mut Graph, depth: usize) -> Result<()> {
        if depth > self.max_depth {
            return Err(MetaprogrammingError::MacroExpansionError(
                "Maximum expansion depth exceeded".to_string()
            ));
        }
        
        // Collect macro calls
        let macro_calls = self.find_macro_calls(graph)?;
        
        // Expand each macro call
        for (node_id, macro_name, args) in macro_calls {
            let expanded = self.expand_macro_call(graph, &macro_name, args)?;
            
            // Replace the macro call with expanded code
            self.replace_node(graph, node_id, expanded)?;
        }
        
        // Check if we need another pass
        if self.has_macro_calls(graph)? {
            self.expand_graph_recursive(graph, depth + 1)?;
        }
        
        Ok(())
    }
    
    /// Find macro calls in the graph
    fn find_macro_calls(&self, graph: &Graph) -> Result<Vec<(NodeId, String, Vec<NodeId>)>> {
        let mut calls = Vec::new();
        
        for (&node_id, node) in &graph.nodes {
            if let Node::Application { function, args } = node {
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    if self.macros.contains_key(name) {
                        calls.push((node_id, name.clone(), args.clone()));
                    }
                }
            }
        }
        
        Ok(calls)
    }
    
    /// Check if graph has any macro calls
    fn has_macro_calls(&self, graph: &Graph) -> Result<bool> {
        for node in graph.nodes.values() {
            if let Node::Application { function, .. } = node {
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    if self.macros.contains_key(name) {
                        return Ok(true);
                    }
                }
            }
        }
        Ok(false)
    }
    
    /// Expand a single macro call
    fn expand_macro_call(&self, graph: &Graph, macro_name: &str, args: Vec<NodeId>) -> Result<Graph> {
        let macro_def = self.macros.get(macro_name)
            .ok_or_else(|| MetaprogrammingError::UndefinedMacro(macro_name.to_string()))?;
        
        // Check argument count
        if args.len() != macro_def.params.len() {
            return Err(MetaprogrammingError::MacroExpansionError(
                format!("Macro {} expects {} arguments, got {}", 
                    macro_name, macro_def.params.len(), args.len())
            ));
        }
        
        // Create expansion context
        let mut context = ExpansionContext::new();
        
        // Bind arguments
        for (param, &arg_id) in macro_def.params.iter().zip(args.iter()) {
            context.arguments.insert(param.clone(), arg_id);
        }
        
        // Pattern matching if specified
        if let Some(pattern) = &macro_def.pattern {
            let mut matcher = PatternMatcher::new(graph);
            for &arg_id in &args {
                if let Some(result) = matcher.match_pattern(pattern, arg_id) {
                    context.bindings.extend(result.bindings);
                }
            }
        }
        
        // Special handling for built-in macros
        match macro_def.body.as_str() {
            "cond-expansion" => self.expand_cond(&context, graph, &args),
            "let*-expansion" => self.expand_let_star(&context, graph, &args),
            _ => {
                // Template-based expansion
                self.expand_template(&macro_def.body, &context, graph)
            }
        }
    }
    
    /// Expand template with substitutions
    fn expand_template(&self, template: &str, context: &ExpansionContext, graph: &Graph) -> Result<Graph> {
        // Parse template
        let mut expanded = template.to_string();
        
        // Substitute arguments
        for (param, &node_id) in &context.arguments {
            let node_str = self.node_to_string(graph, node_id)?;
            expanded = expanded.replace(&format!("${}", param), &node_str);
        }
        
        // Substitute pattern bindings
        for (binding, &node_id) in &context.bindings {
            let node_str = self.node_to_string(graph, node_id)?;
            expanded = expanded.replace(&format!("${{{}}}", binding), &node_str);
        }
        
        // Parse expanded code
        parse(&expanded).map_err(|e| MetaprogrammingError::ParseError(e.to_string()))
    }
    
    /// Convert node to string representation
    fn node_to_string(&self, graph: &Graph, node_id: NodeId) -> Result<String> {
        // Simplified - would need proper serialization
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Variable { name } => Ok(name.clone()),
                Node::Literal(lit) => Ok(format!("{}", lit)),
                _ => Ok(format!("<node:{:?}>", node_id)),
            }
        } else {
            Err(MetaprogrammingError::NodeNotFound(node_id))
        }
    }
    
    /// Expand cond macro
    fn expand_cond(&self, _context: &ExpansionContext, graph: &Graph, args: &[NodeId]) -> Result<Graph> {
        if args.is_empty() {
            return parse("nil").map_err(|e| MetaprogrammingError::ParseError(e.to_string()));
        }
        
        // Build nested if expressions
        let mut result = "nil".to_string();
        
        // Process clauses in reverse order
        for &clause_id in args.iter().rev() {
            if let Some(Node::List(items)) = graph.get_node(clause_id) {
                if items.len() >= 2 {
                    let test = self.node_to_string(graph, items[0])?;
                    let body = self.node_to_string(graph, items[1])?;
                    result = format!("(if {} {} {})", test, body, result);
                }
            }
        }
        
        parse(&result).map_err(|e| MetaprogrammingError::ParseError(e.to_string()))
    }
    
    /// Expand let* macro
    fn expand_let_star(&self, _context: &ExpansionContext, graph: &Graph, args: &[NodeId]) -> Result<Graph> {
        if args.len() != 2 {
            return Err(MetaprogrammingError::MacroExpansionError(
                "let* requires exactly 2 arguments".to_string()
            ));
        }
        
        let bindings_id = args[0];
        let body_id = args[1];
        
        // Get bindings
        let bindings = if let Some(Node::List(items)) = graph.get_node(bindings_id) {
            items
        } else {
            return Err(MetaprogrammingError::MacroExpansionError(
                "let* bindings must be a list".to_string()
            ));
        };
        
        // Build nested let expressions
        let body = self.node_to_string(graph, body_id)?;
        let mut result = body;
        
        // Process bindings in reverse order
        for &binding_id in bindings.iter().rev() {
            if let Some(Node::List(pair)) = graph.get_node(binding_id) {
                if pair.len() == 2 {
                    let var = self.node_to_string(graph, pair[0])?;
                    let val = self.node_to_string(graph, pair[1])?;
                    result = format!("(let (({} {})) {})", var, val, result);
                }
            }
        }
        
        parse(&result).map_err(|e| MetaprogrammingError::ParseError(e.to_string()))
    }
    
    /// Replace a node in the graph
    fn replace_node(&self, graph: &mut Graph, old_id: NodeId, new_graph: Graph) -> Result<()> {
        // Merge new graph into existing graph
        for (new_id, node) in new_graph.nodes {
            graph.nodes.insert(new_id, node);
        }
        
        // Update references to old node
        if let Some(new_root) = new_graph.root_id {
            // Replace all references to old_id with new_root
            for node in graph.nodes.values_mut() {
                self.update_node_references(node, old_id, new_root);
            }
            
            // Update root if necessary
            if graph.root_id == Some(old_id) {
                graph.root_id = Some(new_root);
            }
        }
        
        // Remove old node
        graph.nodes.remove(&old_id);
        
        Ok(())
    }
    
    /// Update references in a node
    fn update_node_references(&self, node: &mut Node, old_id: NodeId, new_id: NodeId) {
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
            Node::Let { bindings, body } => {
                for (_, value) in bindings {
                    if *value == old_id {
                        *value = new_id;
                    }
                }
                if *body == old_id {
                    *body = new_id;
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                if *condition == old_id {
                    *condition = new_id;
                }
                if *then_branch == old_id {
                    *then_branch = new_id;
                }
                if *else_branch == old_id {
                    *else_branch = new_id;
                }
            }
            Node::List(items) => {
                for item in items {
                    if *item == old_id {
                        *item = new_id;
                    }
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
#[path = "macros_tests.rs"]
mod macros_tests;