//! Example demonstrating dependency injection in FluentAi
//!
//! This example shows how to:
//! 1. Use the DI container for service registration
//! 2. Use the VMBuilder for configuring the VM
//! 3. Create custom implementations of core traits
//! 4. Wire everything together with DI

use std::sync::Arc;
use std::collections::HashMap;
use anyhow::Result;

// Import DI framework
use fluentai_di::prelude::*;

// Import VM and builder
use fluentai_vm::{VMBuilder, VMConfig, VM, Bytecode};
use fluentai_vm::value::Value;

// Import effects
use fluentai_effects::{EffectContext, EffectRuntime};

// Import core traits
use fluentai_core::traits::{ModuleLoader, EffectHandler, StdlibProvider};
use fluentai_core::ast::Graph;

/// Custom module loader that loads from memory
#[derive(Clone)]
struct InMemoryModuleLoader {
    modules: HashMap<String, Arc<Graph>>,
}

impl InMemoryModuleLoader {
    fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }
    
    fn add_module(&mut self, name: &str, graph: Arc<Graph>) {
        self.modules.insert(name.to_string(), graph);
    }
}

impl ModuleLoader for InMemoryModuleLoader {
    fn load_module(&self, name: &str) -> Result<Arc<Graph>> {
        self.modules
            .get(name)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("Module not found: {}", name))
    }
    
    fn module_exists(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }
    
    fn search_paths(&self) -> &[String] {
        &[]
    }
}

/// Custom effect handler for testing
#[derive(Clone)]
struct MockIOHandler {
    output: Arc<parking_lot::Mutex<Vec<String>>>,
}

impl MockIOHandler {
    fn new() -> Self {
        Self {
            output: Arc::new(parking_lot::Mutex::new(Vec::new())),
        }
    }
    
    fn get_output(&self) -> Vec<String> {
        self.output.lock().clone()
    }
}

impl EffectHandler for MockIOHandler {
    fn effect_name(&self) -> &str {
        "io"
    }
    
    fn can_handle(&self, operation: &str) -> bool {
        matches!(operation, "print" | "print-line")
    }
    
    fn supported_operations(&self) -> Vec<String> {
        vec!["print".to_string(), "print-line".to_string()]
    }
}

/// Custom VM configuration for testing
struct TestEnvironmentConfig {
    mock_io: bool,
    module_loader: Option<InMemoryModuleLoader>,
}

impl VMConfig for TestEnvironmentConfig {
    fn configure(&self, builder: &mut VMBuilder) {
        builder.trace_mode = true;
        
        if let Some(loader) = &self.module_loader {
            // Note: This would require VM to support custom module loaders
            // builder.with_module_loader(loader.clone());
        }
    }
}

/// Example: Setting up a DI container for services
fn setup_di_container() -> Result<Container> {
    let mut builder = ContainerBuilder::new();
    
    // Register singleton logger
    builder.register_singleton(|| {
        println!("Creating logger service");
        Logger::new("app")
    });
    
    // Register transient request handler
    builder.register_transient(|| {
        println!("Creating new request handler");
        RequestHandler::new()
    });
    
    // Register a factory that depends on other services
    let container_for_db = builder.build();
    builder.register_singleton({
        let container = container_for_db.clone();
        move || {
            let logger = container.resolve::<Logger>().unwrap();
            Database::new(logger)
        }
    });
    
    Ok(builder.build())
}

/// Example: Using VMBuilder with custom dependencies
fn setup_vm_with_di() -> Result<VM> {
    // Create custom module loader
    let mut module_loader = InMemoryModuleLoader::new();
    module_loader.add_module("test", Arc::new(Graph::new()));
    
    // Create custom effect context
    let effect_context = Arc::new(EffectContext::default());
    
    // Create VM using builder
    let vm = VMBuilder::new()
        .with_bytecode(Bytecode::new())
        .with_effect_context(effect_context.clone())
        .with_trace_mode(true)
        .with_global("app_name", Value::String("DI Example".to_string()))
        .with_global("version", Value::String("1.0.0".to_string()))
        .with_config(TestEnvironmentConfig {
            mock_io: true,
            module_loader: Some(module_loader),
        })
        .build()?;
    
    Ok(vm)
}

/// Example: Using DI with async services
#[cfg(feature = "async")]
async fn setup_async_container() -> Result<()> {
    use fluentai_di::async_container::{AsyncContainer, AsyncContainerBuilder};
    
    let mut builder = AsyncContainerBuilder::new();
    
    // Register async service
    builder.register_async_singleton(|| async {
        // Simulate async initialization
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        AsyncService::new().await
    });
    
    let container = builder.build();
    
    // Resolve async service
    let service = container.resolve::<AsyncService>().await?;
    println!("Async service initialized: {:?}", service);
    
    Ok(())
}

/// Example: Configuration-based DI
#[cfg(feature = "config")]
fn setup_from_config() -> Result<Container> {
    use fluentai_di::config::ContainerConfig;
    
    let config_toml = r#"
        [[services]]
        service_type = "Logger"
        implementation_type = "ConsoleLogger"
        lifetime = "singleton"
        
        [[services]]
        service_type = "Database"
        implementation_type = "PostgresDatabase"
        lifetime = "scoped"
        
        [services.parameters]
        connection_string = "postgres://localhost/mydb"
        
        modules = ["core", "web"]
    "#;
    
    let config = ContainerConfig::from_toml(config_toml)?;
    
    let mut builder = ContainerBuilder::new();
    config.apply_to_builder(&mut builder)?;
    
    Ok(builder.build())
}

// Mock service types for the examples
#[derive(Clone)]
struct Logger {
    name: String,
}

impl Logger {
    fn new(name: &str) -> Self {
        Self { name: name.to_string() }
    }
}

#[derive(Clone)]
struct RequestHandler;

impl RequestHandler {
    fn new() -> Self {
        Self
    }
}

#[derive(Clone)]
struct Database {
    logger: Logger,
}

impl Database {
    fn new(logger: Logger) -> Self {
        Self { logger }
    }
}

#[cfg(feature = "async")]
#[derive(Clone, Debug)]
struct AsyncService {
    initialized_at: std::time::Instant,
}

#[cfg(feature = "async")]
impl AsyncService {
    async fn new() -> Self {
        Self {
            initialized_at: std::time::Instant::now(),
        }
    }
}

fn main() -> Result<()> {
    println!("=== FluentAi Dependency Injection Examples ===\n");
    
    // Example 1: Basic DI container
    println!("1. Setting up DI container...");
    let container = setup_di_container()?;
    
    let logger = container.resolve::<Logger>()?;
    println!("   Resolved logger: {}", logger.name);
    
    let db = container.resolve::<Database>()?;
    println!("   Resolved database with logger: {}", db.logger.name);
    
    // Example 2: VM with custom dependencies
    println!("\n2. Setting up VM with DI...");
    let vm = setup_vm_with_di()?;
    println!("   VM created with custom dependencies");
    println!("   Globals: {:?}", vm.globals.get("app_name"));
    
    // Example 3: Module system
    println!("\n3. Using DI with modules...");
    use fluentai_di::builder::Module;
    
    struct CoreModule;
    impl Module for CoreModule {
        fn configure(&self, builder: &mut ContainerBuilder) {
            builder
                .register_singleton(|| Logger::new("core"))
                .register_transient(|| RequestHandler::new());
        }
    }
    
    let container = ContainerBuilder::new()
        .add_module(CoreModule)
        .build();
    
    let logger = container.resolve::<Logger>()?;
    println!("   Module-registered logger: {}", logger.name);
    
    println!("\n=== Examples completed successfully ===");
    
    Ok(())
}