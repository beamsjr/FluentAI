//! Application embedding support
//!
//! This module provides the machinery to embed the FluentAI runtime
//! into compiled applications, making them self-contained.

use crate::error::{Result, RuntimeError};
use fluentai_core::value::Value;
use fluentai_vm::{VM, Bytecode};
use fluentai_parser::Parser;
use fluentai_optimizer::GraphOptimizer;
use std::sync::Arc;
use parking_lot::RwLock;

/// Embedded application data
#[derive(Debug)]
pub struct EmbeddedApp {
    /// Application metadata
    pub metadata: AppMetadata,
    /// Compiled bytecode modules
    pub modules: Vec<CompiledModule>,
    /// Entry point module
    pub entry_point: String,
    /// Embedded resources
    pub resources: Vec<EmbeddedResource>,
}

#[derive(Debug, Clone)]
pub struct AppMetadata {
    pub name: String,
    pub version: String,
    pub authors: Vec<String>,
    pub description: String,
}

#[derive(Debug)]
pub struct CompiledModule {
    pub name: String,
    pub bytecode: Bytecode,
    pub source_map: Option<SourceMap>,
}

#[derive(Debug)]
pub struct SourceMap {
    // Maps bytecode positions to source locations
    pub mappings: Vec<(usize, SourceLocation)>,
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct EmbeddedResource {
    pub path: String,
    pub data: Vec<u8>,
}

/// Runtime for embedded applications
pub struct EmbeddedRuntime {
    /// The VM instance
    vm: Arc<RwLock<VM>>,
    /// JIT compiler (if enabled)
    #[cfg(feature = "jit")]
    jit: Option<fluentai_jit::JitCompiler>,
    /// Loaded modules
    modules: std::collections::HashMap<String, Bytecode>,
}

impl EmbeddedRuntime {
    /// Create a new embedded runtime
    pub fn new() -> Result<Self> {
        // Create a dummy bytecode for initialization
        // In practice, this will be replaced when loading the app
        let dummy_bytecode = Bytecode::new();
        let vm = Arc::new(RwLock::new(VM::new(dummy_bytecode)));
        
        #[cfg(feature = "jit")]
        let jit = fluentai_jit::JitCompiler::new().ok();
        
        Ok(Self {
            vm,
            #[cfg(feature = "jit")]
            jit,
            modules: std::collections::HashMap::new(),
        })
    }
    
    /// Load an embedded application
    pub fn load_app(&mut self, app: EmbeddedApp) -> Result<()> {
        // Load all modules
        for module in app.modules {
            self.modules.insert(module.name.clone(), module.bytecode);
        }
        
        // TODO: Load resources
        
        Ok(())
    }
    
    /// Run the application
    pub fn run(&mut self, args: Vec<String>) -> Result<Value> {
        // Find entry point
        let main_module = self.modules.get("main")
            .ok_or_else(|| anyhow::anyhow!("No main module found"))?;
        
        // Create a new VM with the bytecode
        let mut vm = VM::new(main_module.clone());
        
        // TODO: Pass command line arguments
        // For now, set args as a global variable
        let args_value = Value::List(
            args.into_iter()
                .map(Value::String)
                .collect()
        );
        vm.set_global("args".to_string(), args_value);
        
        // Run the VM
        vm.run().map_err(|e| RuntimeError::VmError(e))
    }
    
    /// Enable JIT compilation for hot functions
    #[cfg(feature = "jit")]
    pub fn enable_jit(&mut self, threshold: usize) {
        // TODO: Implement JIT threshold tracking
    }
}

/// Builder for creating embedded applications
pub struct EmbeddedAppBuilder {
    metadata: AppMetadata,
    sources: Vec<(String, String)>, // (filename, source)
    resources: Vec<EmbeddedResource>,
    optimization_level: u8,
}

impl EmbeddedAppBuilder {
    /// Create a new builder
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            metadata: AppMetadata {
                name: name.into(),
                version: "0.1.0".to_string(),
                authors: vec![],
                description: String::new(),
            },
            sources: vec![],
            resources: vec![],
            optimization_level: 2,
        }
    }
    
    /// Set version
    pub fn version(mut self, version: impl Into<String>) -> Self {
        self.metadata.version = version.into();
        self
    }
    
    /// Add author
    pub fn author(mut self, author: impl Into<String>) -> Self {
        self.metadata.authors.push(author.into());
        self
    }
    
    /// Set description
    pub fn description(mut self, desc: impl Into<String>) -> Self {
        self.metadata.description = desc.into();
        self
    }
    
    /// Add source file
    pub fn add_source(mut self, filename: impl Into<String>, source: impl Into<String>) -> Self {
        self.sources.push((filename.into(), source.into()));
        self
    }
    
    /// Add resource file
    pub fn add_resource(mut self, path: impl Into<String>, data: Vec<u8>) -> Self {
        self.resources.push(EmbeddedResource {
            path: path.into(),
            data,
        });
        self
    }
    
    /// Set optimization level
    pub fn optimization_level(mut self, level: u8) -> Self {
        self.optimization_level = level;
        self
    }
    
    /// Build the embedded application
    pub fn build(self) -> Result<EmbeddedApp> {
        let mut modules = Vec::new();
        
        // Compile all sources
        for (filename, source) in self.sources {
            // Parse
            let mut parser = Parser::new(&source);
            let ast = parser.parse()
                .map_err(|e| anyhow::anyhow!("Parse error in {}: {:?}", filename, e))?;
            
            // Optimize
            let ast = if self.optimization_level > 0 {
                let mut optimizer = GraphOptimizer::new();
                optimizer.optimize(&ast)
                    .map_err(|e| anyhow::anyhow!("Optimization error: {:?}", e))?
            } else {
                ast
            };
            
            // Compile to bytecode
            let compiler = fluentai_vm::compiler::Compiler::new();
            let bytecode = compiler.compile(&ast)
                .map_err(|e| anyhow::anyhow!("Compilation error: {:?}", e))?;
            
            // Extract module name from filename
            let module_name = std::path::Path::new(&filename)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown")
                .to_string();
            
            modules.push(CompiledModule {
                name: module_name,
                bytecode,
                source_map: None, // TODO: Generate source maps
            });
        }
        
        Ok(EmbeddedApp {
            metadata: self.metadata,
            modules,
            entry_point: "main".to_string(),
            resources: self.resources,
        })
    }
}

/// Macro to embed FluentAI application data at compile time
#[macro_export]
macro_rules! embed_fluentai_app {
    ($name:expr, $($source:expr),+) => {{
        use $crate::embed::{EmbeddedAppBuilder, EmbeddedRuntime};
        
        let mut builder = EmbeddedAppBuilder::new($name);
        $(
            builder = builder.add_source(
                stringify!($source),
                include_str!($source)
            );
        )+
        
        builder.build()
    }};
}

/// Generate a main function for standalone executables
pub fn generate_main_stub(app: &EmbeddedApp) -> String {
    format!(r#"
// Auto-generated main function for FluentAI application: {}
// Version: {}

fn main() {{
    // Initialize embedded runtime
    let mut runtime = fluentai_core_lib::embed::EmbeddedRuntime::new()
        .expect("Failed to create runtime");
    
    // Load embedded application
    let app = /* embedded app data */;
    runtime.load_app(app).expect("Failed to load app");
    
    // Get command line arguments
    let args: Vec<String> = std::env::args().collect();
    
    // Run the application
    match runtime.run(args) {{
        Ok(fluentai_core_lib::Value::Integer(code)) => std::process::exit(code as i32),
        Ok(_) => std::process::exit(0),
        Err(e) => {{
            eprintln!("Runtime error: {{}}", e);
            std::process::exit(1);
        }}
    }}
}}
"#, app.metadata.name, app.metadata.version)
}