//! Module registry for managing loaded modules and their exports

use fluentai_core::ast::{ExportItem, Graph};
use fluentai_core::value::Value;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use anyhow::{anyhow, Result};
use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};

/// Information about a loaded module
#[derive(Debug, Clone)]
pub struct ModuleInfo {
    /// The module's name
    pub name: String,
    /// The module's file path (if loaded from file)
    pub path: Option<String>,
    /// The compiled bytecode for the module
    pub bytecode: Arc<fluentai_bytecode::Bytecode>,
    /// Exported items from the module
    pub exports: Vec<ExportItem>,
    /// Cached export values (populated lazily)
    pub export_values: FxHashMap<String, Value>,
}

/// Registry for managing loaded modules
#[derive(Debug, Default)]
pub struct ModuleRegistry {
    /// Map from module name to module info
    modules: FxHashMap<String, ModuleInfo>,
    /// Map from file path to module name (for deduplication)
    path_to_module: FxHashMap<String, String>,
}

impl ModuleRegistry {
    /// Create a new empty module registry
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Register a module from a parsed graph
    pub fn register_module(
        &mut self,
        graph: &Graph,
        bytecode: Arc<fluentai_bytecode::Bytecode>,
        path: Option<String>,
    ) -> Result<String> {
        // Extract module name from graph metadata
        let module_name = graph.graph_metadata
            .get("module_name")
            .cloned()
            .or_else(|| {
                // If no explicit module name, derive from path
                path.as_ref().and_then(|p| {
                    std::path::Path::new(p)
                        .file_stem()
                        .and_then(|stem| stem.to_str())
                        .map(|s| s.to_string())
                })
            })
            .ok_or_else(|| anyhow!("Module must have a name"))?;
        
        // Extract exports from graph metadata
        let exports = if let Some(exports_json) = graph.graph_metadata.get("exports") {
            serde_json::from_str(exports_json)
                .unwrap_or_else(|_| Vec::new())
        } else {
            Vec::new()
        };
        
        // Check if module already exists
        if self.modules.contains_key(&module_name) {
            return Err(anyhow!("Module '{}' is already loaded", module_name));
        }
        
        // Check if path is already loaded
        if let Some(ref p) = path {
            if let Some(existing_module) = self.path_to_module.get(p) {
                return Err(anyhow!(
                    "Path '{}' is already loaded as module '{}'",
                    p,
                    existing_module
                ));
            }
        }
        
        // Create module info
        let module_info = ModuleInfo {
            name: module_name.clone(),
            path: path.clone(),
            bytecode,
            exports,
            export_values: FxHashMap::default(),
        };
        
        // Register the module
        self.modules.insert(module_name.clone(), module_info);
        
        // Register path mapping if applicable
        if let Some(p) = path {
            self.path_to_module.insert(p, module_name.clone());
        }
        
        Ok(module_name)
    }
    
    /// Get a module by name
    pub fn get_module(&self, name: &str) -> Option<&ModuleInfo> {
        self.modules.get(name)
    }
    
    /// Get a mutable reference to a module
    pub fn get_module_mut(&mut self, name: &str) -> Option<&mut ModuleInfo> {
        self.modules.get_mut(name)
    }
    
    /// Get an exported value from a module
    pub fn get_export(&self, module_name: &str, export_name: &str) -> Result<Value> {
        let module = self.modules
            .get(module_name)
            .ok_or_else(|| anyhow!("Module '{}' not found", module_name))?;
        
        // Check if export exists
        let export_exists = module.exports
            .iter()
            .any(|e| e.name == export_name);
        
        if !export_exists {
            return Err(anyhow!(
                "Module '{}' does not export '{}'",
                module_name,
                export_name
            ));
        }
        
        // Check cached value
        if let Some(value) = module.export_values.get(export_name) {
            return Ok(value.clone());
        }
        
        // TODO: Value needs to be computed by running the module
        // For now, return a placeholder
        Err(anyhow!(
            "Export '{}' from module '{}' has not been computed yet",
            export_name,
            module_name
        ))
    }
    
    /// Set an exported value for a module (called by VM after execution)
    pub fn set_export_value(
        &mut self,
        module_name: &str,
        export_name: &str,
        value: Value,
    ) -> Result<()> {
        let module = self.modules
            .get_mut(module_name)
            .ok_or_else(|| anyhow!("Module '{}' not found", module_name))?;
        
        module.export_values.insert(export_name.to_string(), value);
        Ok(())
    }
    
    /// List all loaded modules
    pub fn list_modules(&self) -> Vec<&str> {
        self.modules.keys().map(|s| s.as_str()).collect()
    }
    
    /// Check if a module is loaded
    pub fn has_module(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }
    
    /// Clear all loaded modules
    pub fn clear(&mut self) {
        self.modules.clear();
        self.path_to_module.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::Graph;
    use fluentai_bytecode::{Bytecode, Instruction};
    
    #[test]
    fn test_register_module() {
        let mut registry = ModuleRegistry::new();
        let mut graph = Graph::new();
        
        // Set module metadata
        graph.graph_metadata.insert("module_name".to_string(), "TestModule".to_string());
        graph.graph_metadata.insert("exports".to_string(), r#"[{"name":"test_func","alias":null}]"#.to_string());
        
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("test".to_string()));
        chunk.add_instruction(Instruction::new(Opcode::Return));
        bytecode.main_chunk = bytecode.add_chunk(chunk);
        let bytecode = Arc::new(bytecode);
        
        let result = registry.register_module(&graph, bytecode, None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "TestModule");
        
        // Check module is registered
        assert!(registry.has_module("TestModule"));
        
        let module = registry.get_module("TestModule").unwrap();
        assert_eq!(module.name, "TestModule");
        assert_eq!(module.exports.len(), 1);
        assert_eq!(module.exports[0].name, "test_func");
    }
    
    #[test]
    fn test_duplicate_module_error() {
        let mut registry = ModuleRegistry::new();
        let mut graph = Graph::new();
        graph.graph_metadata.insert("module_name".to_string(), "DupModule".to_string());
        
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("test".to_string()));
        chunk.add_instruction(Instruction::new(Opcode::Return));
        bytecode.main_chunk = bytecode.add_chunk(chunk);
        let bytecode = Arc::new(bytecode);
        
        // First registration should succeed
        assert!(registry.register_module(&graph, bytecode.clone(), None).is_ok());
        
        // Second registration should fail
        let result = registry.register_module(&graph, bytecode, None);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("already loaded"));
    }
    
    #[test]
    fn test_module_from_path() {
        let mut registry = ModuleRegistry::new();
        let graph = Graph::new();
        // No module name in metadata, should derive from path
        
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("test".to_string()));
        chunk.add_instruction(Instruction::new(Opcode::Return));
        bytecode.main_chunk = bytecode.add_chunk(chunk);
        let bytecode = Arc::new(bytecode);
        
        let result = registry.register_module(&graph, bytecode, Some("/path/to/utils.flc".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "utils");
    }
}