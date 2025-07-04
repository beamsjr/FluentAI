//! FluentAi Module System
//! 
//! This crate provides the module system for FluentAi, including:
//! - Module loading and caching
//! - Import/export resolution
//! - Module environments and isolation
//! - Circular dependency detection

pub mod error;
pub mod loader;
pub mod resolver;
pub mod environment;
pub mod cache;

pub use error::{ModuleError, Result};
pub use loader::ModuleLoader;
pub use resolver::ModuleResolver;
pub use environment::ModuleEnvironment;
pub use cache::ModuleCache;

use fluentai_core::ast::{Graph, NodeId};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Information about a loaded module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleInfo {
    /// The module's unique identifier (usually its path)
    pub id: String,
    
    /// The module's name
    pub name: String,
    
    /// Path to the module file
    pub path: PathBuf,
    
    /// The parsed AST graph
    pub graph: Graph,
    
    /// The root node of the module
    pub root: NodeId,
    
    /// List of exported names
    pub exports: Vec<String>,
    
    /// Dependencies (module IDs)
    pub dependencies: Vec<String>,
    
    /// Module metadata
    pub metadata: FxHashMap<String, serde_json::Value>,
}

/// Module system configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleConfig {
    /// Search paths for modules
    pub search_paths: Vec<PathBuf>,
    
    /// Whether to enable module caching
    pub enable_cache: bool,
    
    /// Maximum cache size (in number of modules)
    pub max_cache_size: usize,
    
    /// Whether to allow circular dependencies
    pub allow_circular: bool,
}

impl Default for ModuleConfig {
    fn default() -> Self {
        Self {
            search_paths: vec![
                PathBuf::from("."),
                PathBuf::from("./modules"),
                PathBuf::from("./stdlib/modules"),
            ],
            enable_cache: true,
            max_cache_size: 1000,
            allow_circular: false,
        }
    }
}
