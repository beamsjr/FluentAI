//! Runtime configuration

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Execution mode for the runtime
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExecutionMode {
    /// Interpret bytecode directly
    Interpreted,
    /// Use JIT compilation for hot code paths
    #[cfg(feature = "jit")]
    JIT {
        /// Threshold for JIT compilation
        threshold: u32,
    },
    /// Ahead-of-time compilation (future)
    #[cfg(feature = "aot")]
    AOT,
}

impl Default for ExecutionMode {
    fn default() -> Self {
        #[cfg(feature = "jit")]
        return ExecutionMode::JIT { threshold: 1000 };
        #[cfg(not(feature = "jit"))]
        return ExecutionMode::Interpreted;
    }
}

/// Memory configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryConfig {
    /// Initial heap size in bytes
    pub initial_heap_size: usize,
    /// Maximum heap size in bytes (0 = unlimited)
    pub max_heap_size: usize,
    /// Stack size in slots
    pub stack_size: usize,
    /// GC trigger threshold (percentage of heap used)
    pub gc_threshold: f32,
}

impl Default for MemoryConfig {
    fn default() -> Self {
        Self {
            initial_heap_size: 1024 * 1024, // 1MB
            max_heap_size: 0,               // Unlimited
            stack_size: 65536,              // 64K slots
            gc_threshold: 0.8,              // 80%
        }
    }
}

/// Module loading configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleConfig {
    /// Search paths for modules
    pub search_paths: Vec<PathBuf>,
    /// Enable module caching
    pub enable_cache: bool,
    /// Cache directory
    pub cache_dir: Option<PathBuf>,
}

impl Default for ModuleConfig {
    fn default() -> Self {
        Self {
            search_paths: vec![],
            enable_cache: true,
            cache_dir: None,
        }
    }
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityConfig {
    /// Enable sandboxing
    pub enable_sandbox: bool,
    /// Allow file system access
    pub allow_fs: bool,
    /// Allow network access
    pub allow_network: bool,
    /// Allow process spawning
    pub allow_process: bool,
    /// Maximum execution time in milliseconds (0 = unlimited)
    pub max_execution_time: u64,
}

impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            enable_sandbox: false,
            allow_fs: true,
            allow_network: true,
            allow_process: true,
            max_execution_time: 0,
        }
    }
}

/// Debug configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DebugConfig {
    /// Enable debug mode
    pub enabled: bool,
    /// Enable tracing
    pub enable_tracing: bool,
    /// Enable profiling
    pub enable_profiling: bool,
    /// Dump bytecode
    pub dump_bytecode: bool,
    /// Dump optimized AST
    pub dump_ast: bool,
}

impl Default for DebugConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            enable_tracing: false,
            enable_profiling: false,
            dump_bytecode: false,
            dump_ast: false,
        }
    }
}

/// Runtime configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuntimeConfig {
    /// Execution mode
    pub execution_mode: ExecutionMode,
    /// Memory configuration
    pub memory: MemoryConfig,
    /// Module configuration
    pub modules: ModuleConfig,
    /// Security configuration
    pub security: SecurityConfig,
    /// Debug configuration
    pub debug: DebugConfig,
    /// Number of worker threads (0 = auto)
    pub worker_threads: usize,
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            execution_mode: ExecutionMode::default(),
            memory: MemoryConfig::default(),
            modules: ModuleConfig::default(),
            security: SecurityConfig::default(),
            debug: DebugConfig::default(),
            worker_threads: 0,
        }
    }
}

impl RuntimeConfig {
    /// Create a new runtime configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a development configuration with debugging enabled
    pub fn development() -> Self {
        Self {
            debug: DebugConfig {
                enabled: true,
                enable_tracing: true,
                ..Default::default()
            },
            ..Default::default()
        }
    }

    /// Create a production configuration optimized for performance
    pub fn production() -> Self {
        Self {
            execution_mode: ExecutionMode::default(),
            memory: MemoryConfig {
                initial_heap_size: 16 * 1024 * 1024, // 16MB
                ..Default::default()
            },
            ..Default::default()
        }
    }

    /// Create a sandboxed configuration with restricted access
    pub fn sandboxed() -> Self {
        Self {
            security: SecurityConfig {
                enable_sandbox: true,
                allow_fs: false,
                allow_network: false,
                allow_process: false,
                max_execution_time: 30000, // 30 seconds
            },
            ..Default::default()
        }
    }

    /// Load configuration from a JSON file
    pub fn from_file(path: impl AsRef<std::path::Path>) -> anyhow::Result<Self> {
        let content = std::fs::read_to_string(path)?;
        Ok(serde_json::from_str(&content)?)
    }

    /// Save configuration to a JSON file
    pub fn to_file(&self, path: impl AsRef<std::path::Path>) -> anyhow::Result<()> {
        let content = serde_json::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }
}
