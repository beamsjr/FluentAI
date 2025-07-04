//! Documentation service that bridges DocumentationRegistry with LSP
//!
//! This service provides a centralized way to access documentation
//! from the DocumentationRegistry for use in hover, completion, etc.

use fluentai_core::documentation::{Documentation, DocumentationRegistry};
use fluentai_core::ast::{Graph, Node, NodeId};
use std::sync::{Arc, RwLock};
use once_cell::sync::Lazy;

/// Global documentation registry instance
static DOCUMENTATION_REGISTRY: Lazy<Arc<RwLock<DocumentationRegistry>>> = Lazy::new(|| {
    let mut registry = DocumentationRegistry::new();
    registry.register_all();
    Arc::new(RwLock::new(registry))
});

/// Documentation service for LSP features
pub struct DocumentationService {
    registry: Arc<RwLock<DocumentationRegistry>>,
}

impl DocumentationService {
    /// Create a new documentation service
    pub fn new() -> Self {
        Self {
            registry: Arc::clone(&DOCUMENTATION_REGISTRY),
        }
    }
    
    /// Get documentation for a keyword or built-in
    pub fn get_documentation(&self, name: &str) -> Option<Documentation> {
        let registry = self.registry.read().ok()?;
        
        // Try general lookup first
        if let Some(doc) = registry.get(name) {
            return Some(doc.clone());
        }
        
        // Try operators
        for op in registry.get_operators() {
            if op.symbol == name {
                return Some(op.to_documentation());
            }
        }
        
        // Try keywords
        for kw in registry.get_keywords() {
            if kw.keyword == name {
                return Some(kw.to_documentation());
            }
        }
        
        // Try built-ins
        for builtin in registry.get_builtins() {
            if builtin.name == name {
                return Some(builtin.to_documentation());
            }
        }
        
        None
    }
    
    /// Get documentation for an AST node
    pub fn get_node_documentation(&self, graph: &Graph, node_id: NodeId) -> Option<Documentation> {
        // First check if the node has a documentation ID
        if let Some(doc_id) = graph.get_documentation(node_id) {
            let registry = self.registry.read().ok()?;
            // Look up by documentation ID (would need to add this method to registry)
            // For now, try to match by name
            return self.get_documentation(doc_id);
        }
        
        // Otherwise, try to determine documentation from node type
        let node = graph.get_node(node_id)?;
        match node {
            Node::Variable(name) => {
                // Check if it's a built-in function
                self.get_documentation(name)
            }
            Node::Define { name, .. } => {
                // For user-defined functions, generate documentation from context memory
                if let Some(context) = graph.get_context_memory(node_id) {
                    Some(self.generate_documentation_from_context(name, context))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    
    /// Generate documentation from context memory
    fn generate_documentation_from_context(
        &self,
        name: &str,
        context: &fluentai_core::ast::ContextMemory,
    ) -> Documentation {
        use fluentai_core::documentation::{DocumentationCategory, DocumentationVisibility};
        
        let mut description = String::from("User-defined function");
        if let Some(rationale) = &context.rationale {
            description.push_str(&format!("\n\n{}", rationale));
        }
        
        // Add usage statistics if available
        if context.usage_stats.execution_count > 0 {
            description.push_str(&format!(
                "\n\n**Usage**: Called {} times, avg time: {}ns", 
                context.usage_stats.execution_count,
                context.usage_stats.avg_execution_time_ns
            ));
        }
        
        // Add performance hints
        if !context.performance_hints.is_empty() {
            description.push_str("\n\n**Performance hints**:");
            for hint in &context.performance_hints {
                description.push_str(&format!("\n- {:?}", hint.hint_type));
            }
        }
        
        Documentation {
            name: name.to_string(),
            syntax: format!("({} ...)", name),
            description,
            examples: vec![],
            category: DocumentationCategory::Function,
            see_also: vec![],
            visibility: DocumentationVisibility::Public,
        }
    }
    
    /// Format documentation for hover display
    pub fn format_hover(&self, doc: &Documentation) -> String {
        let mut result = format!("**{}**\n\n", doc.name);
        
        if !doc.syntax.is_empty() {
            result.push_str(&format!("`{}`\n\n", doc.syntax));
        }
        
        result.push_str(&doc.description);
        
        if !doc.examples.is_empty() {
            result.push_str("\n\n**Examples:**\n");
            for example in &doc.examples {
                result.push_str(&format!("```lisp\n{}\n```\n", example));
            }
        }
        
        if !doc.see_also.is_empty() {
            result.push_str(&format!("\n\n**See also:** {}", doc.see_also.join(", ")));
        }
        
        result
    }
    
    /// Get all available completions
    pub fn get_all_completions(&self) -> Vec<(String, Documentation)> {
        let registry = match self.registry.read() {
            Ok(r) => r,
            Err(_) => return vec![],
        };
        
        let mut completions = Vec::new();
        
        // Add operators
        for op in registry.get_operators() {
            completions.push((op.symbol.to_string(), op.to_documentation()));
        }
        
        // Add keywords
        for kw in registry.get_keywords() {
            completions.push((kw.keyword.to_string(), kw.to_documentation()));
        }
        
        // Add built-ins
        for builtin in registry.get_builtins() {
            completions.push((builtin.name.to_string(), builtin.to_documentation()));
        }
        
        completions
    }
}

impl Default for DocumentationService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_documentation_service() {
        let service = DocumentationService::new();
        
        // Test getting operator documentation
        let doc = service.get_documentation("+").unwrap();
        assert_eq!(doc.name, "+");
        assert!(doc.description.contains("Addition"));
        
        // Test getting keyword documentation
        let doc = service.get_documentation("define").unwrap();
        assert_eq!(doc.name, "define");
        
        // Test hover formatting
        let formatted = service.format_hover(&doc);
        assert!(formatted.contains("**define**"));
        assert!(formatted.contains(&doc.syntax));
    }
}