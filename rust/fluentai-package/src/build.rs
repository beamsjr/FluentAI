//! Build system for FluentAI packages
//!
//! This module provides compilation, bundling, and build orchestration
//! for FluentAI projects.

use crate::{Manifest, PackageConfig, PackageError, Result};
use fluentai_core::ast::{ExportItem, Graph, ImportItem};
use fluentai_parser::parse_flc;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tracing::{debug, info};

/// Build configuration
#[derive(Debug, Clone)]
pub struct BuildConfig {
    /// Source directory
    pub src_dir: PathBuf,
    
    /// Output directory
    pub out_dir: PathBuf,
    
    /// Whether to include source maps
    pub source_maps: bool,
    
    /// Optimization level (0-3)
    pub opt_level: u8,
    
    /// Whether to bundle dependencies
    pub bundle_deps: bool,
    
    /// Additional include paths
    pub include_paths: Vec<PathBuf>,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            src_dir: PathBuf::from("src"),
            out_dir: PathBuf::from("dist"),
            source_maps: true,
            opt_level: 0,
            bundle_deps: false,
            include_paths: vec![],
        }
    }
}

/// Build system for FluentAI packages
pub struct Builder {
    /// Package manifest
    manifest: Manifest,
    
    /// Build configuration
    config: BuildConfig,
    
    /// Package configuration
    package_config: PackageConfig,
    
    /// Module cache to avoid recompiling
    module_cache: HashMap<PathBuf, CompiledModule>,
}

/// A compiled module
#[derive(Debug, Clone)]
struct CompiledModule {
    /// Module name
    name: String,
    
    /// Parsed AST graph
    graph: Graph,
    
    /// Source file path
    source_path: PathBuf,
    
    /// Dependencies (module names)
    dependencies: Vec<String>,
    
    /// Exported items
    exports: Vec<String>,
}

impl Builder {
    /// Create a new builder
    pub fn new(manifest: Manifest, config: BuildConfig, package_config: PackageConfig) -> Self {
        Self {
            manifest,
            config,
            package_config,
            module_cache: HashMap::new(),
        }
    }
    
    /// Build the project
    pub fn build(&mut self) -> Result<()> {
        info!("Building package {}", self.manifest.name);
        
        // Ensure output directory exists
        fs::create_dir_all(&self.config.out_dir)?;
        
        // Find entry point
        let entry_point = self.find_entry_point()?;
        info!("Entry point: {}", entry_point.display());
        
        // Build dependency graph
        let module_graph = self.build_module_graph(&entry_point)?;
        
        // Compile modules in dependency order
        let compiled_modules = self.compile_modules(module_graph)?;
        
        // Bundle if requested
        if self.config.bundle_deps {
            self.bundle_modules(compiled_modules)?;
        } else {
            self.output_modules(compiled_modules)?;
        }
        
        // Copy assets
        self.copy_assets()?;
        
        // Generate package metadata
        self.generate_metadata()?;
        
        info!("Build complete");
        Ok(())
    }
    
    /// Find the entry point file
    fn find_entry_point(&self) -> Result<PathBuf> {
        // Check manifest for explicit main
        if let Some(main) = &self.manifest.main {
            let path = self.config.src_dir.join(main);
            if path.exists() {
                return Ok(path);
            }
        }
        
        // Check common entry points
        let common_mains = ["main.flc", "index.flc", "lib.flc"];
        for main in &common_mains {
            let path = self.config.src_dir.join(main);
            if path.exists() {
                return Ok(path);
            }
        }
        
        Err(PackageError::BuildError {
            message: "No entry point found. Specify 'main' in fluentai.json".to_string(),
        })
    }
    
    /// Build a dependency graph of all modules
    fn build_module_graph(&mut self, entry: &Path) -> Result<Vec<PathBuf>> {
        let mut graph = Vec::new();
        let mut visited = std::collections::HashSet::new();
        
        self.trace_dependencies(entry, &mut graph, &mut visited)?;
        
        // Reverse for dependency order (dependencies first)
        graph.reverse();
        Ok(graph)
    }
    
    /// Trace dependencies recursively
    fn trace_dependencies(
        &mut self,
        file: &Path,
        graph: &mut Vec<PathBuf>,
        visited: &mut std::collections::HashSet<PathBuf>,
    ) -> Result<()> {
        let canonical = file.canonicalize()?;
        if visited.contains(&canonical) {
            return Ok(());
        }
        visited.insert(canonical.clone());
        
        // Parse file to find imports
        let content = fs::read_to_string(file)?;
        let parsed = parse_flc(&content).map_err(|e| PackageError::BuildError {
            message: format!("Failed to parse {}: {}", file.display(), e),
        })?;
        
        // Extract imports from metadata
        if let Some(imports_json) = parsed.graph_metadata.get("imports") {
            if let Ok(imports) = serde_json::from_str::<Vec<ImportItem>>(imports_json) {
                for import in imports {
                    // Resolve import path - using the name as the module path
                    if let Some(import_path) = self.resolve_import(&import.name, file)? {
                        self.trace_dependencies(&import_path, graph, visited)?;
                    }
                }
            }
        }
        
        graph.push(canonical);
        Ok(())
    }
    
    /// Resolve an import path to a file
    fn resolve_import(&self, module_path: &str, from_file: &Path) -> Result<Option<PathBuf>> {
        // Handle relative imports
        if module_path.starts_with("./") || module_path.starts_with("../") {
            let base = from_file.parent().unwrap();
            let mut path = base.join(module_path);
            
            // Try with .flc extension
            if !path.exists() {
                path.set_extension("flc");
            }
            
            if path.exists() {
                return Ok(Some(path));
            }
        }
        
        // Handle absolute imports from src/
        let src_path = self.config.src_dir.join(module_path).with_extension("flc");
        if src_path.exists() {
            return Ok(Some(src_path));
        }
        
        // Handle stdlib imports (no file needed)
        if module_path.starts_with("std::") {
            return Ok(None);
        }
        
        // Handle installed packages
        let modules_dir = PathBuf::from("fluentai_modules");
        if modules_dir.exists() {
            // Try to find in installed packages
            for entry in fs::read_dir(&modules_dir)? {
                let entry = entry?;
                if entry.file_type()?.is_dir() {
                    let package_main = entry.path().join("src/main.flc");
                    if package_main.exists() {
                        // Check if this package exports the module
                        // TODO: Properly check package exports
                        return Ok(Some(package_main));
                    }
                }
            }
        }
        
        debug!("Could not resolve import: {}", module_path);
        Ok(None)
    }
    
    /// Compile all modules
    fn compile_modules(&mut self, module_paths: Vec<PathBuf>) -> Result<Vec<CompiledModule>> {
        let mut compiled = Vec::new();
        
        for path in module_paths {
            let module = self.compile_module(&path)?;
            compiled.push(module);
        }
        
        Ok(compiled)
    }
    
    /// Compile a single module
    fn compile_module(&mut self, path: &Path) -> Result<CompiledModule> {
        // Check cache
        if let Some(cached) = self.module_cache.get(path) {
            return Ok(cached.clone());
        }
        
        info!("Compiling {}", path.display());
        
        let content = fs::read_to_string(path)?;
        let mut graph = parse_flc(&content).map_err(|e| PackageError::BuildError {
            message: format!("Failed to parse {}: {}", path.display(), e),
        })?;
        
        // Apply optimizations based on opt_level
        if self.config.opt_level > 0 {
            self.optimize_graph(&mut graph)?;
        }
        
        // Extract module info
        let module_name = self.extract_module_name(&graph, path);
        let dependencies = self.extract_dependencies(&graph);
        let exports = self.extract_exports(&graph);
        
        let module = CompiledModule {
            name: module_name,
            graph,
            source_path: path.to_path_buf(),
            dependencies,
            exports,
        };
        
        // Cache the result
        self.module_cache.insert(path.to_path_buf(), module.clone());
        
        Ok(module)
    }
    
    /// Extract module name from graph or path
    fn extract_module_name(&self, graph: &Graph, path: &Path) -> String {
        // Check for explicit module declaration
        if let Some(name) = graph.graph_metadata.get("module_name") {
            return name.clone();
        }
        
        // Derive from file path
        let relative = path.strip_prefix(&self.config.src_dir).unwrap_or(path);
        relative
            .with_extension("")
            .to_string_lossy()
            .replace('/', "::")
    }
    
    /// Extract dependencies from graph
    fn extract_dependencies(&self, graph: &Graph) -> Vec<String> {
        let mut deps = Vec::new();
        
        if let Some(imports_json) = graph.graph_metadata.get("imports") {
            if let Ok(imports) = serde_json::from_str::<Vec<ImportItem>>(imports_json) {
                for import in imports {
                    deps.push(import.name);
                }
            }
        }
        
        deps
    }
    
    /// Extract exports from graph
    fn extract_exports(&self, graph: &Graph) -> Vec<String> {
        let mut exports = Vec::new();
        
        if let Some(exports_json) = graph.graph_metadata.get("exports") {
            if let Ok(export_items) = serde_json::from_str::<Vec<ExportItem>>(exports_json) {
                for item in export_items {
                    exports.push(item.name);
                }
            }
        }
        
        exports
    }
    
    /// Apply optimizations to the graph
    fn optimize_graph(&self, _graph: &mut Graph) -> Result<()> {
        // TODO: Implement actual optimizations
        // For now, just a placeholder
        debug!("Applying optimizations (level {})", self.config.opt_level);
        Ok(())
    }
    
    /// Bundle modules into a single file
    fn bundle_modules(&self, modules: Vec<CompiledModule>) -> Result<()> {
        info!("Bundling {} modules", modules.len());
        
        let bundle_path = self.config.out_dir.join(format!("{}.bundle.flc", self.manifest.name));
        
        // TODO: Implement proper bundling
        // For now, just concatenate modules with module boundaries
        let mut bundle_content = String::new();
        
        bundle_content.push_str("// FluentAI Bundle\n");
        bundle_content.push_str(&format!("// Package: {} v{}\n", self.manifest.name, self.manifest.version));
        bundle_content.push_str("// Generated by FluentAI Build System\n\n");
        
        for module in modules {
            bundle_content.push_str(&format!("// Module: {}\n", module.name));
            bundle_content.push_str(&format!("// Source: {}\n", module.source_path.display()));
            
            // Serialize the graph
            let module_json = serde_json::to_string_pretty(&module.graph)?;
            bundle_content.push_str(&format!("// BEGIN MODULE {}\n", module.name));
            bundle_content.push_str(&module_json);
            bundle_content.push_str(&format!("\n// END MODULE {}\n\n", module.name));
        }
        
        fs::write(&bundle_path, bundle_content)?;
        info!("Bundle written to {}", bundle_path.display());
        
        Ok(())
    }
    
    /// Output individual compiled modules
    fn output_modules(&self, modules: Vec<CompiledModule>) -> Result<()> {
        for module in modules {
            let relative_path = module.source_path
                .strip_prefix(&self.config.src_dir)
                .unwrap_or(&module.source_path);
            
            let output_path = self.config.out_dir.join(relative_path);
            
            // Ensure directory exists
            if let Some(parent) = output_path.parent() {
                fs::create_dir_all(parent)?;
            }
            
            // Serialize the compiled graph
            let json = serde_json::to_string_pretty(&module.graph)?;
            fs::write(&output_path.with_extension("json"), json)?;
            
            // Copy original source if source maps enabled
            if self.config.source_maps {
                fs::copy(&module.source_path, &output_path)?;
            }
        }
        
        Ok(())
    }
    
    /// Copy non-source assets
    fn copy_assets(&self) -> Result<()> {
        // Copy files listed in manifest
        for file_pattern in &self.manifest.files {
            // TODO: Implement glob pattern matching
            let path = PathBuf::from(file_pattern);
            if path.exists() && !path.starts_with(&self.config.src_dir) {
                let dest = self.config.out_dir.join(&path);
                if let Some(parent) = dest.parent() {
                    fs::create_dir_all(parent)?;
                }
                fs::copy(&path, dest)?;
            }
        }
        
        Ok(())
    }
    
    /// Generate package metadata
    fn generate_metadata(&self) -> Result<()> {
        let metadata = serde_json::json!({
            "name": self.manifest.name,
            "version": self.manifest.version,
            "description": self.manifest.description,
            "main": self.manifest.main,
            "build_time": chrono::Utc::now().to_rfc3339(),
            "fluentai_version": env!("CARGO_PKG_VERSION"),
        });
        
        let metadata_path = self.config.out_dir.join("package.json");
        fs::write(metadata_path, serde_json::to_string_pretty(&metadata)?)?;
        
        Ok(())
    }
}

/// Run a build script defined in the manifest
pub fn run_build_script(manifest: &Manifest, script_name: &str) -> Result<()> {
    if let Some(script) = manifest.scripts.get(script_name) {
        match script {
            crate::manifest::Script::Command(cmd) => {
                info!("Running build script: {}", cmd);
                let status = std::process::Command::new("sh")
                    .arg("-c")
                    .arg(cmd)
                    .status()?;
                
                if !status.success() {
                    return Err(PackageError::BuildError {
                        message: format!("Build script '{}' failed", script_name),
                    });
                }
            }
            crate::manifest::Script::Detailed { cmd, env, cwd } => {
                let mut command = std::process::Command::new("sh");
                command.arg("-c").arg(cmd);
                
                for (key, value) in env {
                    command.env(key, value);
                }
                
                if let Some(cwd) = cwd {
                    command.current_dir(cwd);
                }
                
                let status = command.status()?;
                
                if !status.success() {
                    return Err(PackageError::BuildError {
                        message: format!("Build script '{}' failed", script_name),
                    });
                }
            }
        }
    }
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    
    #[test]
    fn test_build_config_default() {
        let config = BuildConfig::default();
        assert_eq!(config.src_dir, PathBuf::from("src"));
        assert_eq!(config.out_dir, PathBuf::from("dist"));
        assert_eq!(config.opt_level, 0);
    }
    
    #[test]
    fn test_builder_creation() {
        let manifest = Manifest::default();
        let build_config = BuildConfig::default();
        let package_config = PackageConfig::default();
        
        let builder = Builder::new(manifest, build_config, package_config);
        assert!(builder.module_cache.is_empty());
    }
}