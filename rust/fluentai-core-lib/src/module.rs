//! Module representation and management

use fluentai_core::ast::Graph as AstGraph;
use fluentai_core::value::Value;
use fluentai_vm::bytecode::Bytecode;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::error::{Result, RuntimeError};

/// Module metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleMetadata {
    /// Module name
    pub name: String,
    /// Module version
    pub version: Option<String>,
    /// Module description
    pub description: Option<String>,
    /// Module dependencies
    pub dependencies: Vec<String>,
    /// Export list
    pub exports: Vec<String>,
}

/// Source module (uncompiled)
#[derive(Debug, Clone)]
pub struct SourceModule {
    /// Module metadata
    pub metadata: ModuleMetadata,
    /// Source code
    pub source: String,
    /// Source path (if loaded from file)
    pub path: Option<PathBuf>,
}

/// Compiled module
#[derive(Debug, Clone)]
pub struct CompiledModule {
    /// Module metadata
    pub metadata: ModuleMetadata,
    /// Compiled bytecode
    pub bytecode: Arc<Bytecode>,
    /// AST graph (for debugging/analysis)
    pub ast: Option<Arc<AstGraph>>,
    /// Source map (for debugging)
    pub source_map: Option<SourceMap>,
    /// Export table (name -> value)
    pub exports: HashMap<String, Value>,
}

/// Source map for debugging
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceMap {
    /// Source file path
    pub source_path: Option<PathBuf>,
    /// Bytecode offset to source location mapping
    pub mappings: Vec<SourceMapping>,
}

/// Single source mapping
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceMapping {
    /// Bytecode offset
    pub bytecode_offset: usize,
    /// Source line
    pub line: usize,
    /// Source column
    pub column: usize,
}

/// Module interface
pub trait Module: Send + Sync {
    /// Get module metadata
    fn metadata(&self) -> &ModuleMetadata;

    /// Get exported value by name
    fn get_export(&self, name: &str) -> Option<&Value>;

    /// Get all exports
    fn exports(&self) -> &HashMap<String, Value>;

    /// Check if module exports a name
    fn has_export(&self, name: &str) -> bool {
        self.get_export(name).is_some()
    }
}

impl Module for CompiledModule {
    fn metadata(&self) -> &ModuleMetadata {
        &self.metadata
    }

    fn get_export(&self, name: &str) -> Option<&Value> {
        self.exports.get(name)
    }

    fn exports(&self) -> &HashMap<String, Value> {
        &self.exports
    }
}

/// Module cache entry
#[derive(Clone)]
pub struct CacheEntry {
    /// Compiled module
    pub module: Arc<CompiledModule>,
    /// Last modified time
    pub last_modified: std::time::SystemTime,
    /// Module hash
    pub hash: u64,
}

/// Module builder
pub struct ModuleBuilder {
    metadata: ModuleMetadata,
    source: Option<String>,
    path: Option<PathBuf>,
}

impl ModuleBuilder {
    /// Create a new module builder
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            metadata: ModuleMetadata {
                name: name.into(),
                version: None,
                description: None,
                dependencies: Vec::new(),
                exports: Vec::new(),
            },
            source: None,
            path: None,
        }
    }

    /// Set module version
    pub fn version(mut self, version: impl Into<String>) -> Self {
        self.metadata.version = Some(version.into());
        self
    }

    /// Set module description
    pub fn description(mut self, desc: impl Into<String>) -> Self {
        self.metadata.description = Some(desc.into());
        self
    }

    /// Add a dependency
    pub fn dependency(mut self, dep: impl Into<String>) -> Self {
        self.metadata.dependencies.push(dep.into());
        self
    }

    /// Add multiple dependencies
    pub fn dependencies(mut self, deps: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.metadata
            .dependencies
            .extend(deps.into_iter().map(|d| d.into()));
        self
    }

    /// Set source code
    pub fn source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    /// Set source path
    pub fn path(mut self, path: impl Into<PathBuf>) -> Self {
        self.path = Some(path.into());
        self
    }

    /// Build source module
    pub fn build_source(self) -> Result<SourceModule> {
        let source = self
            .source
            .ok_or_else(|| RuntimeError::module("Module source is required"))?;

        Ok(SourceModule {
            metadata: self.metadata,
            source,
            path: self.path,
        })
    }
}

/// Module resolution
pub fn resolve_module_path(name: &str, search_paths: &[PathBuf]) -> Option<PathBuf> {
    // Try direct path first
    let path = Path::new(name);
    if path.exists() && path.is_file() {
        return Some(path.to_path_buf());
    }

    // Convert module name to path (e.g., "foo/bar" -> "foo/bar.fl")
    let module_path = format!("{}.fl", name.replace('/', std::path::MAIN_SEPARATOR_STR));

    // Search in search paths
    for search_path in search_paths {
        let full_path = search_path.join(&module_path);
        if full_path.exists() && full_path.is_file() {
            return Some(full_path);
        }

        // Also try .ai extension
        let ai_path = search_path.join(format!(
            "{}.ai",
            name.replace('/', std::path::MAIN_SEPARATOR_STR)
        ));
        if ai_path.exists() && ai_path.is_file() {
            return Some(ai_path);
        }
    }

    None
}

/// Load module source from file
pub fn load_module_source(path: &Path) -> Result<String> {
    std::fs::read_to_string(path)
        .map_err(|e| RuntimeError::module(format!("Failed to read module: {}", e)))
}

/// Parse module metadata from source
pub fn parse_module_metadata(source: &str) -> Result<ModuleMetadata> {
    // This is a simplified version - in a real implementation,
    // we'd parse module declarations from the source

    // Look for module declaration comments
    let mut name = None;
    let mut version = None;
    let mut description = None;
    let mut dependencies = Vec::new();
    let mut exports = Vec::new();

    for line in source.lines() {
        let line = line.trim();
        if line.starts_with(";; @module") {
            if let Some(module_name) = line.strip_prefix(";; @module").map(|s| s.trim()) {
                name = Some(module_name.to_string());
            }
        } else if line.starts_with(";; @version") {
            if let Some(ver) = line.strip_prefix(";; @version").map(|s| s.trim()) {
                version = Some(ver.to_string());
            }
        } else if line.starts_with(";; @description") {
            if let Some(desc) = line.strip_prefix(";; @description").map(|s| s.trim()) {
                description = Some(desc.to_string());
            }
        } else if line.starts_with(";; @depends") {
            if let Some(dep) = line.strip_prefix(";; @depends").map(|s| s.trim()) {
                dependencies.push(dep.to_string());
            }
        } else if line.starts_with(";; @export") {
            if let Some(export) = line.strip_prefix(";; @export").map(|s| s.trim()) {
                exports.push(export.to_string());
            }
        }
    }

    let name = name.ok_or_else(|| RuntimeError::module("Module name not found"))?;

    Ok(ModuleMetadata {
        name,
        version,
        description,
        dependencies,
        exports,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_builder() {
        let module = ModuleBuilder::new("test")
            .version("1.0.0")
            .description("Test module")
            .dependency("core")
            .source("(define x 42)")
            .build_source()
            .unwrap();

        assert_eq!(module.metadata.name, "test");
        assert_eq!(module.metadata.version.as_deref(), Some("1.0.0"));
        assert_eq!(module.metadata.dependencies, vec!["core"]);
        assert_eq!(module.source, "(define x 42)");
    }

    #[test]
    fn test_parse_module_metadata() {
        let source = r#"
;; @module math
;; @version 1.0.0
;; @description Math utilities
;; @depends core
;; @export add
;; @export multiply

(define add (lambda (a b) (+ a b)))
(define multiply (lambda (a b) (* a b)))
"#;

        let metadata = parse_module_metadata(source).unwrap();
        assert_eq!(metadata.name, "math");
        assert_eq!(metadata.version.as_deref(), Some("1.0.0"));
        assert_eq!(metadata.description.as_deref(), Some("Math utilities"));
        assert_eq!(metadata.dependencies, vec!["core"]);
        assert_eq!(metadata.exports, vec!["add", "multiply"]);
    }
}
