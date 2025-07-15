//! Module loading pipeline for FluentAI VM
//! 
//! This module handles the complete pipeline of loading modules:
//! 1. Resolving module paths
//! 2. Reading module files
//! 3. Parsing FLC source code
//! 4. Compiling to bytecode
//! 5. Registering with the module registry
//! 6. Executing module initialization

use crate::compiler::{Compiler, CompilerOptions};
use crate::VM;
use fluentai_parser::parse_flc;
use fluentai_core::ast::Graph;
use fluentai_core::value::Value;
use fluentai_bytecode::Bytecode;
use fluentai_optimizer::OptimizationLevel;
use anyhow::{anyhow, Result};
use std::path::PathBuf;
use std::sync::Arc;
use std::fs;

/// Configuration for the module loader
#[derive(Debug, Clone)]
pub struct ModuleLoaderConfig {
    /// Base directories to search for modules
    pub module_paths: Vec<PathBuf>,
    /// File extension for module files
    pub module_extension: String,
    /// Whether to enable module caching
    pub enable_cache: bool,
    /// Optimization level for module compilation
    pub optimization_level: OptimizationLevel,
}

impl Default for ModuleLoaderConfig {
    fn default() -> Self {
        Self {
            module_paths: vec![
                PathBuf::from("."),
                PathBuf::from("./modules"),
                PathBuf::from("./lib"),
            ],
            module_extension: "flc".to_string(),
            enable_cache: true,
            optimization_level: OptimizationLevel::Basic,
        }
    }
}

/// Module loader that handles the complete loading pipeline
pub struct ModuleLoader {
    config: ModuleLoaderConfig,
}

impl ModuleLoader {
    /// Create a new module loader with default configuration
    pub fn new() -> Self {
        Self::with_config(ModuleLoaderConfig::default())
    }
    
    /// Create a new module loader with custom configuration
    pub fn with_config(config: ModuleLoaderConfig) -> Self {
        Self { config }
    }
    
    /// Load a module by name, returning the module name after registration
    pub fn load_module(&self, vm: &mut VM, module_name: &str) -> Result<String> {
        // Check if module is already loaded
        if vm.has_module(module_name) {
            return Ok(module_name.to_string());
        }
        
        // Find the module file
        let module_path = self.resolve_module_path(module_name)?;
        
        // Read the module source
        let source = fs::read_to_string(&module_path)
            .map_err(|e| anyhow!("Failed to read module '{}': {}", module_path.display(), e))?;
        
        // Parse the module
        let graph = parse_flc(&source)
            .map_err(|e| anyhow!("Failed to parse module '{}': {}", module_name, e))?;
        
        // Compile the module
        let bytecode = self.compile_module(&graph, module_name)?;
        
        // Register with the VM's module registry
        let registered_name = vm.register_module(
            &graph,
            Arc::new(bytecode),
            Some(module_path.to_string_lossy().to_string())
        ).map_err(|e| anyhow!("Failed to register module '{}': {}", module_name, e))?;
        
        // Execute the module to populate exports
        self.execute_module(vm, &registered_name, &graph)?;
        
        Ok(registered_name)
    }
    
    /// Resolve a module name to a file path
    fn resolve_module_path(&self, module_name: &str) -> Result<PathBuf> {
        // Convert module name to path (e.g., "utils.math" -> "utils/math.flc")
        let relative_path = module_name.replace('.', "/");
        let filename = format!("{}.{}", relative_path, self.config.module_extension);
        
        // Search in all module paths
        for base_path in &self.config.module_paths {
            let full_path = base_path.join(&filename);
            if full_path.exists() && full_path.is_file() {
                return Ok(full_path);
            }
        }
        
        // Also check for direct file path
        let direct_path = PathBuf::from(module_name);
        if direct_path.exists() && direct_path.is_file() {
            return Ok(direct_path);
        }
        
        // Check with extension
        let with_ext = PathBuf::from(format!("{}.{}", module_name, self.config.module_extension));
        if with_ext.exists() && with_ext.is_file() {
            return Ok(with_ext);
        }
        
        Err(anyhow!(
            "Module '{}' not found. Searched in: {:?}",
            module_name,
            self.config.module_paths
        ))
    }
    
    /// Compile a module graph to bytecode
    fn compile_module(&self, graph: &Graph, module_name: &str) -> Result<Bytecode> {
        let options = CompilerOptions {
            optimization_level: self.config.optimization_level.clone(),
            debug_info: true,
        };
        
        let compiler = Compiler::with_options(options);
        compiler.compile(graph)
            .map_err(|e| anyhow!("Failed to compile module '{}': {}", module_name, e))
    }
    
    /// Execute a module to populate its exports
    fn execute_module(&self, vm: &mut VM, module_name: &str, graph: &Graph) -> Result<()> {
        // Get the module info and clone necessary data to avoid borrowing issues
        let (bytecode, exports) = {
            let module_info = vm.module_registry()
                .get_module(module_name)
                .ok_or_else(|| anyhow!("Module '{}' not found in registry", module_name))?;
            (module_info.bytecode.clone(), module_info.exports.clone())
        };
        
        // Create a new VM instance for module execution
        let mut module_vm = VM::with_shared_bytecode(bytecode);
        
        // Copy necessary context from parent VM
        #[cfg(feature = "std")]
        {
            module_vm.set_stdlib_registry(vm.get_stdlib_registry().clone());
            module_vm.set_effect_runtime(vm.get_effect_runtime());
        }
        
        // Run the module
        let result = module_vm.run()
            .map_err(|e| anyhow!("Failed to execute module '{}': {:?}", module_name, e))?;
        
        // Extract exported values and store them in the registry
        for export in &exports {
            // Look for the exported value in globals
            if let Some(value) = module_vm.get_global(&export.name) {
                vm.set_module_export(module_name, &export.name, value.clone())
                    .map_err(|e| anyhow!("Failed to set export '{}': {}", export.name, e))?;
            }
        }
        
        Ok(())
    }
    
    /// Load a module and return a specific export
    pub fn load_and_get_export(
        &self,
        vm: &mut VM,
        module_name: &str,
        export_name: &str
    ) -> Result<Value> {
        // Load the module
        self.load_module(vm, module_name)?;
        
        // Get the export
        vm.get_module_export(module_name, export_name)
            .map_err(|e| anyhow!("Failed to get export '{}' from module '{}': {}", 
                export_name, module_name, e))
    }
    
    /// Add a module search path
    pub fn add_module_path(&mut self, path: PathBuf) {
        if !self.config.module_paths.contains(&path) {
            self.config.module_paths.push(path);
        }
    }
    
    /// Get the current module search paths
    pub fn module_paths(&self) -> &[PathBuf] {
        &self.config.module_paths
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;
    
    #[test]
    fn test_module_path_resolution() {
        let temp_dir = TempDir::new().unwrap();
        let module_path = temp_dir.path().join("test_module.flc");
        fs::write(&module_path, "public function test() -> int { 42 }").unwrap();
        
        let mut config = ModuleLoaderConfig::default();
        config.module_paths = vec![temp_dir.path().to_path_buf()];
        
        let loader = ModuleLoader::with_config(config);
        let resolved = loader.resolve_module_path("test_module");
        
        assert!(resolved.is_ok());
        assert_eq!(resolved.unwrap(), module_path);
    }
    
    #[test]
    fn test_nested_module_path() {
        let temp_dir = TempDir::new().unwrap();
        let utils_dir = temp_dir.path().join("utils");
        fs::create_dir(&utils_dir).unwrap();
        
        let module_path = utils_dir.join("math.flc");
        fs::write(&module_path, "public function add(a: int, b: int) -> int { a + b }").unwrap();
        
        let mut config = ModuleLoaderConfig::default();
        config.module_paths = vec![temp_dir.path().to_path_buf()];
        
        let loader = ModuleLoader::with_config(config);
        let resolved = loader.resolve_module_path("utils.math");
        
        assert!(resolved.is_ok());
        assert_eq!(resolved.unwrap(), module_path);
    }
    
    #[test]
    fn test_module_not_found() {
        let loader = ModuleLoader::new();
        let result = loader.resolve_module_path("non_existent_module");
        
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not found"));
    }
}