//! Example demonstrating custom effect handlers with DI

use std::sync::Arc;
use std::collections::HashMap;
use fluentai_effects::{
    EffectHandler, EffectHandlerProvider, EffectHandlerBuilder,
    EffectResult, EffectType,
};
use fluentai_core::{value::Value, error::Error};
use async_trait::async_trait;

/// Custom logging effect handler
struct LoggingHandler {
    log_level: String,
    logs: Arc<parking_lot::RwLock<Vec<String>>>,
}

impl LoggingHandler {
    fn new(log_level: &str) -> Self {
        Self {
            log_level: log_level.to_string(),
            logs: Arc::new(parking_lot::RwLock::new(Vec::new())),
        }
    }
    
    fn get_logs(&self) -> Vec<String> {
        self.logs.read().clone()
    }
}

#[async_trait]
impl EffectHandler for LoggingHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::IO
    }
    
    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "log:debug" => {
                if self.log_level == "debug" {
                    let msg = args.get(0)
                        .and_then(|v| match v {
                            Value::String(s) => Some(s.as_str()),
                            _ => None,
                        })
                        .unwrap_or("No message");
                    self.logs.write().push(format!("[DEBUG] {}", msg));
                }
                Ok(Value::Nil)
            }
            "log:info" => {
                let msg = args.get(0)
                    .and_then(|v| match v {
                        Value::String(s) => Some(s.as_str()),
                        _ => None,
                    })
                    .unwrap_or("No message");
                self.logs.write().push(format!("[INFO] {}", msg));
                Ok(Value::Nil)
            }
            "log:error" => {
                let msg = args.get(0)
                    .and_then(|v| match v {
                        Value::String(s) => Some(s.as_str()),
                        _ => None,
                    })
                    .unwrap_or("No message");
                self.logs.write().push(format!("[ERROR] {}", msg));
                Ok(Value::Nil)
            }
            "log:get_all" => {
                Ok(Value::List(
                    self.logs.read()
                        .iter()
                        .map(|s| Value::String(s.clone()))
                        .collect()
                ))
            }
            _ => Err(Error::Runtime(format!("Unknown log operation: {}", operation))),
        }
    }
}

/// Custom metrics effect handler
struct MetricsHandler {
    metrics: Arc<parking_lot::RwLock<HashMap<String, f64>>>,
}

impl MetricsHandler {
    fn new() -> Self {
        Self {
            metrics: Arc::new(parking_lot::RwLock::new(HashMap::new())),
        }
    }
}

#[async_trait]
impl EffectHandler for MetricsHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::State
    }
    
    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "metrics:increment" => {
                let name = match args.get(0) {
                    Some(Value::String(s)) => s.clone(),
                    _ => return Err(Error::Runtime("Increment requires metric name".into())),
                };
                
                let value = match args.get(1) {
                    Some(Value::Float(f)) => *f,
                    Some(Value::Integer(i)) => *i as f64,
                    _ => 1.0,
                };
                
                let mut metrics = self.metrics.write();
                *metrics.entry(name).or_insert(0.0) += value;
                
                Ok(Value::Nil)
            }
            "metrics:set" => {
                let name = match args.get(0) {
                    Some(Value::String(s)) => s.clone(),
                    _ => return Err(Error::Runtime("Set requires metric name".into())),
                };
                
                let value = match args.get(1) {
                    Some(Value::Float(f)) => *f,
                    Some(Value::Integer(i)) => *i as f64,
                    _ => return Err(Error::Runtime("Set requires numeric value".into())),
                };
                
                self.metrics.write().insert(name, value);
                Ok(Value::Nil)
            }
            "metrics:get" => {
                let name = match args.get(0) {
                    Some(Value::String(s)) => s,
                    _ => return Err(Error::Runtime("Get requires metric name".into())),
                };
                
                let metrics = self.metrics.read();
                match metrics.get(name) {
                    Some(value) => Ok(Value::Float(*value)),
                    None => Ok(Value::Nil),
                }
            }
            "metrics:get_all" => {
                let metrics = self.metrics.read();
                let map: HashMap<String, Value> = metrics.iter()
                    .map(|(k, v)| (k.clone(), Value::Float(*v)))
                    .collect();
                Ok(Value::Map(map))
            }
            _ => Err(Error::Runtime(format!("Unknown metrics operation: {}", operation))),
        }
    }
}

fn main() {
    println!("=== Custom Effect Handlers Example ===\n");
    
    // Create provider with custom handlers
    let provider = EffectHandlerBuilder::new()
        .with_defaults()
        .with_handler(Arc::new(LoggingHandler::new("debug")))
        .with_handler(Arc::new(MetricsHandler::new()))
        .build();
    
    // Create effect context
    let context = provider.create_context().unwrap();
    
    // Use logging handler
    println!("Testing logging handler:");
    context.perform_sync(EffectType::IO, "log:debug", &[
        Value::String("Starting application".to_string())
    ]).unwrap();
    
    context.perform_sync(EffectType::IO, "log:info", &[
        Value::String("Application initialized".to_string())
    ]).unwrap();
    
    context.perform_sync(EffectType::IO, "log:error", &[
        Value::String("Sample error message".to_string())
    ]).unwrap();
    
    // Get all logs
    if let Ok(Value::List(logs)) = context.perform_sync(EffectType::IO, "log:get_all", &[]) {
        for log in logs {
            if let Value::String(s) = log {
                println!("  {}", s);
            }
        }
    }
    
    // Use metrics handler
    println!("\nTesting metrics handler:");
    
    // Increment counters
    context.perform_sync(EffectType::State, "metrics:increment", &[
        Value::String("requests".to_string()),
        Value::Integer(5),
    ]).unwrap();
    
    context.perform_sync(EffectType::State, "metrics:increment", &[
        Value::String("requests".to_string()),
        Value::Integer(3),
    ]).unwrap();
    
    // Set gauge
    context.perform_sync(EffectType::State, "metrics:set", &[
        Value::String("cpu_usage".to_string()),
        Value::Float(65.5),
    ]).unwrap();
    
    // Get specific metric
    if let Ok(Value::Float(requests)) = context.perform_sync(EffectType::State, "metrics:get", &[
        Value::String("requests".to_string())
    ]) {
        println!("  Total requests: {}", requests);
    }
    
    // Get all metrics
    if let Ok(Value::Map(metrics)) = context.perform_sync(EffectType::State, "metrics:get_all", &[]) {
        println!("  All metrics:");
        for (name, value) in metrics {
            println!("    {}: {:?}", name, value);
        }
    }
    
    // Test hierarchical providers
    println!("\n=== Hierarchical Providers ===");
    
    let parent_provider = Arc::new(provider);
    let child_provider = EffectHandlerProvider::create_child(parent_provider.clone());
    
    // Override logging handler in child with different log level
    child_provider.register_singleton(Arc::new(LoggingHandler::new("info")));
    
    let child_context = child_provider.create_context().unwrap();
    
    // Debug log won't appear with info level
    child_context.perform_sync(EffectType::IO, "log:debug", &[
        Value::String("This won't be logged".to_string())
    ]).unwrap();
    
    child_context.perform_sync(EffectType::IO, "log:info", &[
        Value::String("This will be logged".to_string())
    ]).unwrap();
    
    if let Ok(Value::List(logs)) = child_context.perform_sync(EffectType::IO, "log:get_all", &[]) {
        println!("  Child provider logs (info level only):");
        for log in logs {
            if let Value::String(s) = log {
                println!("    {}", s);
            }
        }
    }
    
    println!("\n=== Dynamic Handler Registration ===");
    
    // Create a new provider
    let dynamic_provider = EffectHandlerProvider::new();
    
    // Register factory for creating handlers on demand
    dynamic_provider.register_factory(EffectType::Error, || {
        println!("  Creating error handler on demand...");
        Arc::new(crate::create_custom_error_handler())
    });
    
    // First access creates the handler
    let handler1 = dynamic_provider.get_handler(EffectType::Error).unwrap();
    // Second access reuses the cached handler
    let handler2 = dynamic_provider.get_handler(EffectType::Error).unwrap();
    
    println!("  Handlers are same instance: {}", Arc::ptr_eq(&handler1, &handler2));
}

/// Custom error handler for demonstration
struct CustomErrorHandler {
    error_count: Arc<parking_lot::RwLock<usize>>,
}

#[async_trait]
impl EffectHandler for CustomErrorHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::Error
    }
    
    fn handle_sync(&self, operation: &str, _args: &[Value]) -> EffectResult {
        match operation {
            "error:count" => {
                *self.error_count.write() += 1;
                Ok(Value::Integer(*self.error_count.read() as i64))
            }
            _ => Ok(Value::Nil),
        }
    }
}

fn create_custom_error_handler() -> impl EffectHandler {
    CustomErrorHandler {
        error_count: Arc::new(parking_lot::RwLock::new(0)),
    }
}