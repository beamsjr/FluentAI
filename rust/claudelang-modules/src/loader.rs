//! Module loading functionality

use crate::{ModuleInfo, ModuleConfig, ModuleCache, ModuleError, Result};
use claudelang_core::ast::{Graph, Node, NodeId, Literal};
use claudelang_parser::parse;
use rustc_hash::FxHashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{info, trace};

/// Module loader responsible for finding and loading modules
pub struct ModuleLoader {
    config: ModuleConfig,
    cache: ModuleCache,
    loading: FxHashSet<String>,
}

impl ModuleLoader {
    /// Create a new module loader with the given configuration
    pub fn new(config: ModuleConfig) -> Self {
        let cache = ModuleCache::new(config.max_cache_size);
        Self {
            config,
            cache,
            loading: FxHashSet::default(),
        }
    }
    
    /// Load a module by path or name
    pub fn load_module(&mut self, module_ref: &str) -> Result<Arc<ModuleInfo>> {
        // Resolve module path
        let path = self.resolve_module_path(module_ref)?;
        let module_id = self.module_id_from_path(&path);
        
        // Check cache first using the module ID
        if self.config.enable_cache {
            if let Some(module) = self.cache.get(&module_id) {
                return Ok(module);
            }
        }
        
        // Check for circular dependencies
        if self.loading.contains(&module_id) {
            if !self.config.allow_circular {
                return Err(ModuleError::CircularDependency {
                    cycle: self.loading.iter().cloned().collect::<Vec<_>>().join(" -> "),
                });
            }
        }
        
        // Mark as loading
        self.loading.insert(module_id.clone());
        
        // Load and parse the module
        let result = self.load_module_from_path(&path);
        
        // Remove from loading set
        self.loading.remove(&module_id);
        
        match result {
            Ok(module) => {
                // Cache the module and return the cached Arc
                if self.config.enable_cache {
                    self.cache.insert(module)?;
                    // Get the Arc from the cache to ensure we return the same instance
                    self.cache.get(&module_id).ok_or_else(|| {
                        ModuleError::CacheError {
                            message: "Failed to retrieve module from cache after insertion".to_string(),
                        }
                    })
                } else {
                    Ok(Arc::new(module))
                }
            }
            Err(e) => Err(e),
        }
    }
    
    /// Resolve a module reference to a file path
    fn resolve_module_path(&self, module_ref: &str) -> Result<PathBuf> {
        // If it's already a path with .cl extension, use it directly
        if module_ref.ends_with(".cl") {
            let path = Path::new(module_ref);
            if path.exists() {
                return Ok(path.to_path_buf());
            }
        }
        
        // Try each search path
        for search_path in &self.config.search_paths {
            // Try as a file
            let file_path = search_path.join(format!("{}.cl", module_ref));
            if file_path.exists() {
                trace!("Found module at: {:?}", file_path);
                return Ok(file_path);
            }
            
            // Try as a directory with module.cl
            let dir_path = search_path.join(module_ref).join("module.cl");
            if dir_path.exists() {
                trace!("Found module at: {:?}", dir_path);
                return Ok(dir_path);
            }
        }
        
        Err(ModuleError::ModuleNotFound {
            path: module_ref.to_string(),
        })
    }
    
    /// Generate a module ID from a path
    fn module_id_from_path(&self, path: &Path) -> String {
        // Convert to absolute path and use as ID
        path.canonicalize()
            .unwrap_or_else(|_| path.to_path_buf())
            .to_string_lossy()
            .to_string()
    }
    
    /// Load a module from a specific file path
    fn load_module_from_path(&mut self, path: &Path) -> Result<ModuleInfo> {
        // Read file contents
        let contents = fs::read_to_string(path).map_err(|e| ModuleError::IoError {
            path: path.to_path_buf(),
            error: e,
        })?;
        
        // Parse the module
        let mut graph = parse(&contents).map_err(|e| ModuleError::ParseError {
            path: path.to_path_buf(),
            error: e,
        })?;
        
        // Extract module information
        let (name, exports, dependencies) = self.extract_module_info(&graph)?;
        
        let module_id = self.module_id_from_path(path);
        
        info!("Loaded module: {} ({})", name, module_id);
        
        let root = graph.root_id.unwrap_or_else(|| {
            // Create a dummy node if there's no root
            let dummy = Node::Literal(Literal::Nil);
            graph.add_node(dummy)
        });
        
        Ok(ModuleInfo {
            id: module_id,
            name,
            path: path.to_path_buf(),
            graph,
            root,
            exports,
            dependencies,
            metadata: Default::default(),
        })
    }
    
    /// Extract module name, exports, and dependencies from AST
    fn extract_module_info(&self, graph: &Graph) -> Result<(String, Vec<String>, Vec<String>)> {
        let mut name = String::new();
        let mut exports = Vec::new();
        let mut dependencies = Vec::new();
        
        // Walk through all nodes to find module declarations, exports, and imports
        for (_node_id, node) in &graph.nodes {
            match node {
                Node::Module { 
                    name: module_name, 
                    exports: module_exports, 
                    body: _ 
                } => {
                    name = module_name.clone();
                    exports.extend(module_exports.clone());
                }
                
                Node::Export { export_list } => {
                    for item in export_list {
                        exports.push(item.name.clone());
                    }
                }
                
                Node::Import { module_path, .. } => {
                    dependencies.push(module_path.clone());
                }
                
                _ => {}
            }
        }
        
        // If no module name found, derive from file name
        if name.is_empty() {
            name = "anonymous".to_string();
        }
        
        Ok((name, exports, dependencies))
    }
    
    /// Load all dependencies of a module
    pub fn load_dependencies(&mut self, module: &ModuleInfo) -> Result<Vec<Arc<ModuleInfo>>> {
        let mut loaded = Vec::new();
        
        for dep_path in &module.dependencies {
            let dep_module = self.load_module(dep_path)?;
            loaded.push(dep_module);
        }
        
        Ok(loaded)
    }
    
    /// Get the module cache
    pub fn cache(&self) -> &ModuleCache {
        &self.cache
    }
    
    /// Clear the module cache
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;
    
    fn create_test_module_file(dir: &Path, name: &str, content: &str) -> PathBuf {
        let path = dir.join(format!("{}.cl", name));
        fs::write(&path, content).unwrap();
        path
    }
    
    #[test]
    fn test_load_simple_module() {
        let temp_dir = TempDir::new().unwrap();
        let module_content = r#"
            (module test-module (export foo)
                (define foo 42))
        "#;
        
        create_test_module_file(temp_dir.path(), "test", module_content);
        
        let config = ModuleConfig {
            search_paths: vec![temp_dir.path().to_path_buf()],
            ..Default::default()
        };
        
        let mut loader = ModuleLoader::new(config);
        let module = loader.load_module("test").unwrap();
        
        assert_eq!(module.name, "test-module");
        assert_eq!(module.exports, vec!["foo"]);
    }
    
    #[test]
    fn test_module_not_found() {
        let config = ModuleConfig::default();
        let mut loader = ModuleLoader::new(config);
        
        let result = loader.load_module("nonexistent");
        assert!(matches!(result, Err(ModuleError::ModuleNotFound { .. })));
    }
    
    #[test]
    fn test_cache_behavior() {
        let temp_dir = TempDir::new().unwrap();
        let module_content = r#"(module cached-test (export x) x)"#;
        
        create_test_module_file(temp_dir.path(), "cached", module_content);
        
        let config = ModuleConfig {
            search_paths: vec![temp_dir.path().to_path_buf()],
            enable_cache: true,
            ..Default::default()
        };
        
        let mut loader = ModuleLoader::new(config);
        
        // First load
        let module1 = loader.load_module("cached").unwrap();
        
        // Second load should come from cache
        let module2 = loader.load_module("cached").unwrap();
        
        // Should be the same Arc
        assert!(Arc::ptr_eq(&module1, &module2));
    }
}