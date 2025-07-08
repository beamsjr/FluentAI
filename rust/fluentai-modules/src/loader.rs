//! Module loading functionality

use crate::{ModuleInfo, ModuleConfig, ModuleCache, ModuleError, Result};
use fluentai_core::ast::{Graph, Node, Literal};
use fluentai_parser::parse;
use rustc_hash::FxHashSet;
use std::fs;
use std::path::{Path, PathBuf, Component};
use std::sync::Arc;
use tracing::{info, trace, warn};

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
        // Validate module reference
        self.validate_module_ref(module_ref)?;
        
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
    
    /// Validate a module reference to prevent directory traversal attacks
    fn validate_module_ref(&self, module_ref: &str) -> Result<()> {
        // Check for empty reference
        if module_ref.is_empty() {
            return Err(ModuleError::InvalidPath {
                path: PathBuf::from("<empty>"),
            });
        }
        
        // Check for dangerous patterns
        if module_ref.contains("..") || module_ref.contains("./") || module_ref.contains("/.") {
            warn!("Rejected module reference with directory traversal: {}", module_ref);
            return Err(ModuleError::InvalidPath {
                path: PathBuf::from(module_ref),
            });
        }
        
        // Check for absolute paths (security risk)
        let path = Path::new(module_ref);
        if path.is_absolute() {
            warn!("Rejected absolute path module reference: {}", module_ref);
            return Err(ModuleError::InvalidPath {
                path: path.to_path_buf(),
            });
        }
        
        // Check for null bytes
        if module_ref.contains('\0') {
            warn!("Rejected module reference with null byte: {:?}", module_ref);
            return Err(ModuleError::InvalidPath {
                path: PathBuf::from(module_ref),
            });
        }
        
        Ok(())
    }
    
    /// Resolve a module reference to a file path
    fn resolve_module_path(&self, module_ref: &str) -> Result<PathBuf> {
        // If it's already a path with .cl extension, validate and use it
        if module_ref.ends_with(".cl") {
            let path = Path::new(module_ref);
            if path.is_absolute() {
                return Err(ModuleError::InvalidPath {
                    path: path.to_path_buf(),
                });
            }
            if path.exists() {
                return self.validate_resolved_path(path.to_path_buf());
            }
        }
        
        // Try each search path
        for search_path in &self.config.search_paths {
            // Try as a file
            let file_path = search_path.join(format!("{}.cl", module_ref));
            if file_path.exists() {
                trace!("Found module at: {:?}", file_path);
                return self.validate_resolved_path(file_path);
            }
            
            // Try as a directory with module.cl
            let dir_path = search_path.join(module_ref).join("module.cl");
            if dir_path.exists() {
                trace!("Found module at: {:?}", dir_path);
                return self.validate_resolved_path(dir_path);
            }
        }
        
        Err(ModuleError::ModuleNotFound {
            path: module_ref.to_string(),
        })
    }
    
    /// Validate that a resolved path is within allowed search paths
    fn validate_resolved_path(&self, path: PathBuf) -> Result<PathBuf> {
        // Canonicalize the path to resolve symlinks and relative components
        let canonical_path = path.canonicalize().map_err(|e| ModuleError::IoError {
            path: path.clone(),
            error: e,
        })?;
        
        // Check if the canonical path is within any of our search paths
        let mut is_within_search_path = false;
        for search_path in &self.config.search_paths {
            if let Ok(canonical_search) = search_path.canonicalize() {
                if canonical_path.starts_with(&canonical_search) {
                    is_within_search_path = true;
                    break;
                }
            }
        }
        
        if !is_within_search_path {
            warn!("Module path {:?} is outside allowed search paths", canonical_path);
            return Err(ModuleError::InvalidPath { path: canonical_path });
        }
        
        // Additional safety check: ensure no directory traversal in components
        for component in canonical_path.components() {
            match component {
                Component::ParentDir => {
                    warn!("Path contains parent directory component: {:?}", canonical_path);
                    return Err(ModuleError::InvalidPath { path: canonical_path });
                }
                Component::RootDir => {
                    // This is ok if it's part of the canonical path
                }
                Component::CurDir => {
                    // Current directory is ok
                }
                Component::Prefix(_) | Component::Normal(_) => {
                    // Normal components are ok
                }
            }
        }
        
        Ok(canonical_path)
    }
    
    /// Generate a module ID from a path
    fn module_id_from_path(&self, path: &Path) -> String {
        // Use canonical path as ID (already validated)
        path.to_string_lossy().to_string()
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
            graph.add_node(dummy).expect("Failed to create dummy root node")
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
    
    #[test]
    fn test_reject_directory_traversal() {
        let temp_dir = TempDir::new().unwrap();
        let config = ModuleConfig {
            search_paths: vec![temp_dir.path().to_path_buf()],
            ..Default::default()
        };
        
        let mut loader = ModuleLoader::new(config);
        
        // Test various directory traversal attempts
        let dangerous_refs = vec![
            "../etc/passwd",
            "../../sensitive",
            "./../../escape",
            "normal/../../../escape",
            "..\\windows\\path",
        ];
        
        for module_ref in dangerous_refs {
            let result = loader.load_module(module_ref);
            assert!(matches!(result, Err(ModuleError::InvalidPath { .. })),
                    "Should reject module ref: {}", module_ref);
        }
    }
    
    #[test]
    fn test_reject_absolute_paths() {
        let config = ModuleConfig::default();
        let mut loader = ModuleLoader::new(config);
        
        // Test absolute path attempts
        #[cfg(unix)]
        let absolute_refs = vec![
            "/etc/passwd",
            "/usr/bin/evil",
            "/absolute/path/module.cl",
        ];
        
        #[cfg(windows)]
        let absolute_refs = vec![
            "C:\\Windows\\System32\\cmd.exe",
            "\\\\server\\share\\file",
            "D:\\absolute\\path\\module.cl",
        ];
        
        for module_ref in absolute_refs {
            let result = loader.load_module(module_ref);
            assert!(matches!(result, Err(ModuleError::InvalidPath { .. })),
                    "Should reject absolute path: {}", module_ref);
        }
    }
    
    #[test]
    fn test_reject_null_bytes() {
        let config = ModuleConfig::default();
        let mut loader = ModuleLoader::new(config);
        
        let result = loader.load_module("evil\0module");
        assert!(matches!(result, Err(ModuleError::InvalidPath { .. })));
    }
    
    #[test]
    fn test_reject_empty_module_ref() {
        let config = ModuleConfig::default();
        let mut loader = ModuleLoader::new(config);
        
        let result = loader.load_module("");
        assert!(matches!(result, Err(ModuleError::InvalidPath { .. })));
    }
    
    #[test]
    fn test_module_outside_search_path() {
        let temp_dir = TempDir::new().unwrap();
        let safe_dir = temp_dir.path().join("safe");
        fs::create_dir(&safe_dir).unwrap();
        
        // Create a module outside the safe directory
        let module_content = r#"(module outside-test (export x) x)"#;
        create_test_module_file(temp_dir.path(), "outside", module_content);
        
        let config = ModuleConfig {
            search_paths: vec![safe_dir], // Only allow modules from safe/
            ..Default::default()
        };
        
        let mut loader = ModuleLoader::new(config);
        
        // Try to load the module outside the search path
        let result = loader.load_module("../outside");
        assert!(matches!(result, Err(ModuleError::InvalidPath { .. })));
    }
}