//! Module import/export resolution

use crate::{ModuleInfo, ModuleEnvironment, ModuleLoader, ModuleError, Result};
use fluentai_core::ast::{Node, ImportItem};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::Arc;
use tracing::debug;

/// Resolves imports and exports for modules
pub struct ModuleResolver {
    loader: ModuleLoader,
}

impl ModuleResolver {
    /// Create a new module resolver
    pub fn new(loader: ModuleLoader) -> Self {
        Self { loader }
    }
    
    /// Resolve all imports for a module and create its environment
    pub fn resolve_module(&mut self, module: Arc<ModuleInfo>) -> Result<ModuleEnvironment> {
        let mut env = ModuleEnvironment::for_module(module.clone());
        
        // Process all imports in the module
        self.process_imports(&module, &mut env)?;
        
        debug!("Resolved module: {} with {} imports", module.name, env.imports().len());
        
        Ok(env)
    }
    
    /// Process all import nodes in a module
    fn process_imports(&mut self, module: &ModuleInfo, env: &mut ModuleEnvironment) -> Result<()> {
        // Find all import nodes
        for (_, node) in &module.graph.nodes {
            if let Node::Import { module_path, import_list, import_all } = node {
                self.process_import(module_path, import_list, *import_all, env)?;
            }
        }
        
        Ok(())
    }
    
    /// Process a single import statement
    fn process_import(
        &mut self,
        module_path: &str,
        import_list: &[ImportItem],
        import_all: bool,
        env: &mut ModuleEnvironment,
    ) -> Result<()> {
        // Load the imported module
        let imported_module = self.loader.load_module(module_path)?;
        
        if import_all {
            // Import all exports
            for export in &imported_module.exports {
                env.define(
                    export.clone(),
                    crate::environment::ModuleValue::ModuleRef {
                        module_id: imported_module.id.clone(),
                        export_name: export.clone(),
                    },
                );
            }
            
            // Also add the module itself for qualified access
            let module_name = imported_module.name.clone();
            env.add_import(module_name, imported_module);
        } else {
            // Import specific items
            for item in import_list {
                // Check if the item is exported
                if !imported_module.exports.contains(&item.name) {
                    return Err(ModuleError::ExportNotFound {
                        name: item.name.clone(),
                        module: imported_module.name.clone(),
                    });
                }
                
                // Use alias if provided, otherwise use the original name
                let local_name = item.alias.as_ref().unwrap_or(&item.name);
                
                env.define(
                    local_name.clone(),
                    crate::environment::ModuleValue::ModuleRef {
                        module_id: imported_module.id.clone(),
                        export_name: item.name.clone(),
                    },
                );
            }
            
            // Add the module for qualified access
            let module_name = imported_module.name.clone();
            env.add_import(module_name, imported_module);
        }
        
        Ok(())
    }
    
    /// Build a dependency graph for a module and its transitive dependencies
    pub fn build_dependency_graph(&mut self, root_module: &ModuleInfo) -> Result<DependencyGraph> {
        let mut graph = DependencyGraph::new();
        let mut visited = FxHashSet::default();
        
        self.build_graph_recursive(root_module, &mut graph, &mut visited)?;
        
        Ok(graph)
    }
    
    /// Recursively build the dependency graph
    fn build_graph_recursive(
        &mut self,
        module: &ModuleInfo,
        graph: &mut DependencyGraph,
        visited: &mut FxHashSet<String>,
    ) -> Result<()> {
        if visited.contains(&module.id) {
            return Ok(());
        }
        
        visited.insert(module.id.clone());
        graph.add_module(module.clone());
        
        // Load and process dependencies
        for dep_path in &module.dependencies {
            let dep_module = self.loader.load_module(dep_path)?;
            graph.add_dependency(module.id.clone(), dep_module.id.clone());
            
            // Recurse
            self.build_graph_recursive(&dep_module, graph, visited)?;
        }
        
        Ok(())
    }
    
    /// Check for circular dependencies in the module graph
    pub fn check_circular_dependencies(&self, graph: &DependencyGraph) -> Result<()> {
        let mut visited = FxHashSet::default();
        let mut rec_stack = FxHashSet::default();
        
        for module_id in graph.modules.keys() {
            if !visited.contains(module_id) {
                if self.has_cycle(module_id, graph, &mut visited, &mut rec_stack)? {
                    return Err(ModuleError::CircularDependency {
                        cycle: rec_stack.iter().cloned().collect::<Vec<_>>().join(" -> "),
                    });
                }
            }
        }
        
        Ok(())
    }
    
    /// Check if there's a cycle starting from the given module
    fn has_cycle(
        &self,
        module_id: &str,
        graph: &DependencyGraph,
        visited: &mut FxHashSet<String>,
        rec_stack: &mut FxHashSet<String>,
    ) -> Result<bool> {
        visited.insert(module_id.to_string());
        rec_stack.insert(module_id.to_string());
        
        if let Some(deps) = graph.dependencies.get(module_id) {
            for dep in deps {
                if !visited.contains(dep) {
                    if self.has_cycle(dep, graph, visited, rec_stack)? {
                        return Ok(true);
                    }
                } else if rec_stack.contains(dep) {
                    return Ok(true);
                }
            }
        }
        
        rec_stack.remove(module_id);
        Ok(false)
    }
}

/// Represents the dependency graph of modules
#[derive(Debug)]
pub struct DependencyGraph {
    /// All modules in the graph
    pub modules: FxHashMap<String, ModuleInfo>,
    
    /// Dependencies: module_id -> list of dependency module_ids
    pub dependencies: FxHashMap<String, Vec<String>>,
    
    /// Reverse dependencies: module_id -> list of modules that depend on it
    pub dependents: FxHashMap<String, Vec<String>>,
}

impl DependencyGraph {
    /// Create a new empty dependency graph
    pub fn new() -> Self {
        Self {
            modules: FxHashMap::default(),
            dependencies: FxHashMap::default(),
            dependents: FxHashMap::default(),
        }
    }
    
    /// Add a module to the graph
    pub fn add_module(&mut self, module: ModuleInfo) {
        self.modules.insert(module.id.clone(), module);
    }
    
    /// Add a dependency relationship
    pub fn add_dependency(&mut self, module_id: String, dependency_id: String) {
        self.dependencies
            .entry(module_id.clone())
            .or_default()
            .push(dependency_id.clone());
            
        self.dependents
            .entry(dependency_id)
            .or_default()
            .push(module_id);
    }
    
    /// Get all modules that depend on the given module
    pub fn get_dependents(&self, module_id: &str) -> Option<&Vec<String>> {
        self.dependents.get(module_id)
    }
    
    /// Get all dependencies of the given module
    pub fn get_dependencies(&self, module_id: &str) -> Option<&Vec<String>> {
        self.dependencies.get(module_id)
    }
    
    /// Perform a topological sort of the modules
    pub fn topological_sort(&self) -> Result<Vec<String>> {
        let mut sorted = Vec::new();
        let mut visited = FxHashSet::default();
        let mut rec_stack = FxHashSet::default();
        
        // Sort module IDs to ensure consistent iteration order
        let mut module_ids: Vec<_> = self.modules.keys().cloned().collect();
        module_ids.sort();
        
        for module_id in module_ids {
            if !visited.contains(&module_id) {
                self.topological_sort_util(
                    &module_id,
                    &mut visited,
                    &mut rec_stack,
                    &mut sorted,
                )?;
            }
        }
        
        // Don't reverse - DFS post-order already gives us the correct topological order
        Ok(sorted)
    }
    
    fn topological_sort_util(
        &self,
        module_id: &str,
        visited: &mut FxHashSet<String>,
        rec_stack: &mut FxHashSet<String>,
        sorted: &mut Vec<String>,
    ) -> Result<()> {
        visited.insert(module_id.to_string());
        rec_stack.insert(module_id.to_string());
        
        if let Some(deps) = self.dependencies.get(module_id) {
            for dep in deps {
                if !visited.contains(dep) {
                    self.topological_sort_util(dep, visited, rec_stack, sorted)?;
                } else if rec_stack.contains(dep) {
                    return Err(ModuleError::CircularDependency {
                        cycle: format!("{} -> {}", module_id, dep),
                    });
                }
            }
        }
        
        rec_stack.remove(module_id);
        sorted.push(module_id.to_string());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{Graph, NodeId};
    use std::path::PathBuf;
    
    fn create_test_module(id: &str, name: &str, deps: Vec<String>) -> ModuleInfo {
        ModuleInfo {
            id: id.to_string(),
            name: name.to_string(),
            path: PathBuf::from(format!("{}.cl", id)),
            graph: Graph::new(),
            root: NodeId::new(1).unwrap(),
            exports: vec![],
            dependencies: deps,
            metadata: FxHashMap::default(),
        }
    }
    
    #[test]
    fn test_dependency_graph() {
        let mut graph = DependencyGraph::new();
        
        graph.add_module(create_test_module("a", "a", vec!["b".to_string()]));
        graph.add_module(create_test_module("b", "b", vec!["c".to_string()]));
        graph.add_module(create_test_module("c", "c", vec![]));
        
        graph.add_dependency("a".to_string(), "b".to_string());
        graph.add_dependency("b".to_string(), "c".to_string());
        
        let sorted = graph.topological_sort().unwrap();
        assert_eq!(sorted.len(), 3);
        
        // The correct order should be [c, b, a] because:
        // - c has no dependencies
        // - b depends on c
        // - a depends on b
        assert_eq!(sorted, vec!["c", "b", "a"]);
    }
    
    #[test]
    fn test_circular_dependency_detection() {
        let mut graph = DependencyGraph::new();
        
        graph.add_module(create_test_module("a", "a", vec!["b".to_string()]));
        graph.add_module(create_test_module("b", "b", vec!["a".to_string()]));
        
        graph.add_dependency("a".to_string(), "b".to_string());
        graph.add_dependency("b".to_string(), "a".to_string());
        
        let result = graph.topological_sort();
        assert!(matches!(result, Err(ModuleError::CircularDependency { .. })));
    }
}