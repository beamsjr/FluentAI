//! Module loading and compilation

use dashmap::DashMap;
use fluentai_optimizer::GraphOptimizer;
use fluentai_parser::Parser;
use fluentai_vm::compiler::Compiler;
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use std::time::SystemTime;

use crate::config::RuntimeConfig;
use crate::error::{Result, RuntimeError};
use crate::module::{
    load_module_source, parse_module_metadata, resolve_module_path, CacheEntry, CompiledModule,
    ModuleMetadata, SourceModule,
};

/// Module loader
pub struct ModuleLoader {
    /// Runtime configuration
    config: Arc<RuntimeConfig>,
    /// Module cache
    cache: DashMap<String, CacheEntry>,
    /// Loaded modules
    modules: DashMap<String, Arc<CompiledModule>>,
}

impl ModuleLoader {
    /// Create a new module loader
    pub fn new(config: Arc<RuntimeConfig>) -> Self {
        Self {
            config,
            cache: DashMap::new(),
            modules: DashMap::new(),
        }
    }

    /// Load a module by name
    pub fn load(&self, name: &str) -> Result<Arc<CompiledModule>> {
        // Check if already loaded
        if let Some(module) = self.modules.get(name) {
            return Ok(module.clone());
        }

        // Check cache
        if self.config.modules.enable_cache {
            if let Some(cached) = self.get_cached(name)? {
                self.modules.insert(name.to_string(), cached.clone());
                return Ok(cached);
            }
        }

        // Resolve module path
        let path = resolve_module_path(name, &self.config.modules.search_paths)
            .ok_or_else(|| RuntimeError::module(format!("Module '{}' not found", name)))?;

        // Load and compile
        let module = self.load_from_path(&path)?;

        // Cache if enabled
        if self.config.modules.enable_cache {
            self.cache_module(name, &module, &path)?;
        }

        // Store in loaded modules
        let module = Arc::new(module);
        self.modules.insert(name.to_string(), module.clone());

        Ok(module)
    }

    /// Load a module from a file path
    pub fn load_from_path(&self, path: &Path) -> Result<CompiledModule> {
        let source = load_module_source(path)?;
        let metadata = parse_module_metadata(&source)?;

        let source_module = SourceModule {
            metadata,
            source,
            path: Some(path.to_path_buf()),
        };

        self.compile_module(source_module)
    }

    /// Load a module from source
    pub fn load_from_source(&self, name: &str, source: &str) -> Result<Arc<CompiledModule>> {
        let metadata = ModuleMetadata {
            name: name.to_string(),
            version: None,
            description: None,
            dependencies: Vec::new(),
            exports: Vec::new(),
        };

        let source_module = SourceModule {
            metadata,
            source: source.to_string(),
            path: None,
        };

        let module = Arc::new(self.compile_module(source_module)?);
        self.modules.insert(name.to_string(), module.clone());
        Ok(module)
    }

    /// Compile a source module
    fn compile_module(&self, source: SourceModule) -> Result<CompiledModule> {
        // Load dependencies first
        for dep in &source.metadata.dependencies {
            self.load(dep)?;
        }

        // Parse source
        let mut parser = Parser::new(&source.source);
        let ast = parser
            .parse()
            .map_err(|e| RuntimeError::ParseError(format!("{:?}", e)))?;

        // Optimize if not in debug mode
        let ast = if self.config.debug.enabled {
            ast
        } else {
            let mut optimizer = GraphOptimizer::new();
            optimizer
                .optimize(&ast)
                .map_err(|e| RuntimeError::other(format!("Optimization error: {}", e)))?
        };

        // Dump AST if requested
        if self.config.debug.dump_ast {
            eprintln!("=== Optimized AST for {} ===", source.metadata.name);
            eprintln!("{:#?}", ast);
        }

        // Compile to bytecode
        let compiler = Compiler::new();
        let bytecode = compiler
            .compile(&ast)
            .map_err(|e| RuntimeError::other(format!("Compilation error: {}", e)))?;

        // Dump bytecode if requested
        if self.config.debug.dump_bytecode {
            eprintln!("=== Bytecode for {} ===", source.metadata.name);
            eprintln!("{:#?}", bytecode);
        }

        // Extract exports (simplified - in real implementation would evaluate module)
        let exports = HashMap::new();

        Ok(CompiledModule {
            metadata: source.metadata,
            bytecode: Arc::new(bytecode),
            ast: Some(Arc::new(ast)),
            source_map: None, // TODO: Generate source map
            exports,
        })
    }

    /// Get a cached module
    fn get_cached(&self, name: &str) -> Result<Option<Arc<CompiledModule>>> {
        if let Some(entry) = self.cache.get(name) {
            // Check if still valid (simplified - just check if exists)
            if let Some(path) = resolve_module_path(name, &self.config.modules.search_paths) {
                if let Ok(metadata) = path.metadata() {
                    if let Ok(modified) = metadata.modified() {
                        if modified <= entry.last_modified {
                            return Ok(Some(entry.module.clone()));
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    /// Cache a module
    fn cache_module(&self, name: &str, module: &CompiledModule, path: &Path) -> Result<()> {
        let last_modified = path
            .metadata()
            .and_then(|m| m.modified())
            .unwrap_or_else(|_| SystemTime::now());

        let hash = 0; // TODO: Calculate module hash

        let entry = CacheEntry {
            module: Arc::new(module.clone()),
            last_modified,
            hash,
        };

        self.cache.insert(name.to_string(), entry);
        Ok(())
    }

    /// Clear the module cache
    pub fn clear_cache(&self) {
        self.cache.clear();
    }

    /// Clear loaded modules
    pub fn clear_modules(&self) {
        self.modules.clear();
    }

    /// Get a loaded module
    pub fn get_module(&self, name: &str) -> Option<Arc<CompiledModule>> {
        self.modules.get(name).map(|m| m.clone())
    }

    /// Get all loaded modules
    pub fn loaded_modules(&self) -> Vec<String> {
        self.modules.iter().map(|e| e.key().clone()).collect()
    }

    /// Reload a module
    pub fn reload(&self, name: &str) -> Result<Arc<CompiledModule>> {
        // Remove from cache and loaded modules
        self.cache.remove(name);
        self.modules.remove(name);

        // Load again
        self.load(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_module_loader() {
        let temp_dir = TempDir::new().unwrap();
        let module_path = temp_dir.path().join("test.fl");

        fs::write(
            &module_path,
            r#"
;; @module test
;; @version 1.0.0

(define x 42)
"#,
        )
        .unwrap();

        let mut config = RuntimeConfig::default();
        config
            .modules
            .search_paths
            .push(temp_dir.path().to_path_buf());

        let loader = ModuleLoader::new(Arc::new(config));
        let module = loader.load("test").unwrap();

        assert_eq!(module.metadata.name, "test");
        assert_eq!(module.metadata.version.as_deref(), Some("1.0.0"));
    }

    #[test]
    fn test_load_from_source() {
        let config = RuntimeConfig::default();
        let loader = ModuleLoader::new(Arc::new(config));

        let module = loader.load_from_source("inline", "(define x 42)").unwrap();
        assert_eq!(module.metadata.name, "inline");
    }
}
