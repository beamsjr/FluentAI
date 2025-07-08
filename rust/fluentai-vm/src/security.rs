//! Enhanced security and sandboxing for the FluentAi VM
//!
//! This module provides comprehensive security features including:
//! - Capability-based security system
//! - Resource quotas and monitoring
//! - Secure module isolation
//! - Taint tracking for data flow security
//! - Execution time limits

use crate::bytecode::Value;
use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::atomic::{AtomicU64, AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};

/// Security capabilities that can be granted to code
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Capability {
    /// Can perform file system operations
    FileSystem { paths: Vec<String> },
    /// Can perform network operations
    Network { hosts: Vec<String> },
    /// Can execute system commands
    SystemCommand { commands: Vec<String> },
    /// Can access environment variables
    Environment { vars: Vec<String> },
    /// Can create threads
    Threading,
    /// Can allocate unlimited memory
    UnlimitedMemory,
    /// Can import modules
    ModuleImport { modules: Vec<String> },
    /// Can perform unsafe operations
    Unsafe,
    /// Can access global mutable state
    GlobalState,
    /// Can perform cryptographic operations
    Crypto,
    /// Can access time/date functions
    TimeAccess,
    /// Can perform random number generation
    Random,
    /// Custom capability for extensions
    Custom(String),
}

/// Security context for VM execution
#[derive(Debug)]
pub struct SecurityContext {
    /// Granted capabilities
    capabilities: RwLock<FxHashSet<Capability>>,
    /// Resource usage tracking
    resource_usage: ResourceUsage,
    /// Execution time limit
    time_limit: Option<Duration>,
    /// Start time of execution
    start_time: Instant,
    /// Whether execution has been terminated
    terminated: AtomicBool,
    /// Security policy
    policy: SecurityPolicy,
}

/// Resource usage tracking
#[derive(Debug)]
pub struct ResourceUsage {
    /// Memory allocated in bytes
    memory_bytes: AtomicU64,
    /// Number of allocations
    allocations: AtomicU64,
    /// CPU instructions executed
    instructions: AtomicU64,
    /// File handles opened
    _file_handles: AtomicU64,
    /// Network connections
    _network_connections: AtomicU64,
    /// Threads created
    _threads: AtomicU64,
}

/// Security policy configuration
#[derive(Debug, Clone)]
pub struct SecurityPolicy {
    /// Maximum memory that can be allocated
    pub max_memory: u64,
    /// Maximum number of allocations
    pub max_allocations: u64,
    /// Maximum CPU instructions
    pub max_instructions: u64,
    /// Maximum file handles
    pub max_file_handles: u64,
    /// Maximum network connections
    pub max_network_connections: u64,
    /// Maximum threads
    pub max_threads: u64,
    /// Whether to allow dynamic code execution
    pub allow_eval: bool,
    /// Whether to enforce strict mode
    pub strict_mode: bool,
    /// Allowed module imports
    pub allowed_modules: FxHashSet<String>,
    /// Denied module imports
    pub denied_modules: FxHashSet<String>,
}

impl Default for SecurityPolicy {
    fn default() -> Self {
        Self {
            max_memory: 100 * 1024 * 1024, // 100MB
            max_allocations: 100_000,
            max_instructions: 10_000_000,
            max_file_handles: 10,
            max_network_connections: 10,
            max_threads: 1,
            allow_eval: false,
            strict_mode: true,
            allowed_modules: FxHashSet::default(),
            denied_modules: FxHashSet::default(),
        }
    }
}

impl SecurityPolicy {
    /// Create a strict sandbox policy
    pub fn sandbox() -> Self {
        Self {
            max_memory: 10 * 1024 * 1024, // 10MB
            max_allocations: 10_000,
            max_instructions: 1_000_000,
            max_file_handles: 0,
            max_network_connections: 0,
            max_threads: 0,
            allow_eval: false,
            strict_mode: true,
            allowed_modules: FxHashSet::default(),
            denied_modules: FxHashSet::default(),
        }
    }
    
    /// Create a permissive policy for trusted code
    pub fn trusted() -> Self {
        Self {
            max_memory: u64::MAX,
            max_allocations: u64::MAX,
            max_instructions: u64::MAX,
            max_file_handles: 1000,
            max_network_connections: 1000,
            max_threads: 100,
            allow_eval: true,
            strict_mode: false,
            allowed_modules: FxHashSet::default(),
            denied_modules: FxHashSet::default(),
        }
    }
}

impl ResourceUsage {
    fn new() -> Self {
        Self {
            memory_bytes: AtomicU64::new(0),
            allocations: AtomicU64::new(0),
            instructions: AtomicU64::new(0),
            _file_handles: AtomicU64::new(0),
            _network_connections: AtomicU64::new(0),
            _threads: AtomicU64::new(0),
        }
    }
    
    pub fn track_allocation(&self, bytes: u64) -> Result<()> {
        self.memory_bytes.fetch_add(bytes, Ordering::Relaxed);
        self.allocations.fetch_add(1, Ordering::Relaxed);
        Ok(())
    }
    
    pub fn track_deallocation(&self, bytes: u64) {
        self.memory_bytes.fetch_sub(bytes, Ordering::Relaxed);
    }
    
    pub fn track_instruction(&self) {
        self.instructions.fetch_add(1, Ordering::Relaxed);
    }
    
    pub fn get_memory_usage(&self) -> u64 {
        self.memory_bytes.load(Ordering::Relaxed)
    }
    
    pub fn get_instruction_count(&self) -> u64 {
        self.instructions.load(Ordering::Relaxed)
    }
}

impl SecurityContext {
    pub fn new(policy: SecurityPolicy) -> Self {
        Self {
            capabilities: RwLock::new(FxHashSet::default()),
            resource_usage: ResourceUsage::new(),
            time_limit: None,
            start_time: Instant::now(),
            terminated: AtomicBool::new(false),
            policy,
        }
    }
    
    pub fn with_capabilities(self, capabilities: Vec<Capability>) -> Self {
        {
            let mut caps = self.capabilities.write().unwrap();
            caps.extend(capabilities);
        }
        self
    }
    
    pub fn with_time_limit(mut self, limit: Duration) -> Self {
        self.time_limit = Some(limit);
        self
    }
    
    /// Check if a capability is granted
    pub fn has_capability(&self, capability: &Capability) -> bool {
        self.capabilities.read().unwrap().contains(capability)
    }
    
    /// Grant a capability
    pub fn grant_capability(&self, capability: Capability) {
        self.capabilities.write().unwrap().insert(capability);
    }
    
    /// Revoke a capability
    pub fn revoke_capability(&self, capability: &Capability) {
        self.capabilities.write().unwrap().remove(capability);
    }
    
    /// Check resource limits
    pub fn check_limits(&self) -> Result<()> {
        // Check termination flag
        if self.terminated.load(Ordering::Relaxed) {
            return Err(anyhow!("Execution terminated"));
        }
        
        // Check time limit
        if let Some(limit) = self.time_limit {
            if self.start_time.elapsed() > limit {
                self.terminated.store(true, Ordering::Relaxed);
                return Err(anyhow!("Execution time limit exceeded"));
            }
        }
        
        // Check memory usage
        let mem = self.resource_usage.get_memory_usage();
        if mem > self.policy.max_memory {
            return Err(anyhow!("Memory limit exceeded: {} > {}", mem, self.policy.max_memory));
        }
        
        // Check instruction count
        let instructions = self.resource_usage.get_instruction_count();
        if instructions > self.policy.max_instructions {
            return Err(anyhow!("Instruction limit exceeded: {} > {}", instructions, self.policy.max_instructions));
        }
        
        Ok(())
    }
    
    /// Track memory allocation
    pub fn track_allocation(&self, bytes: u64) -> Result<()> {
        self.resource_usage.track_allocation(bytes)?;
        self.check_limits()
    }
    
    /// Track memory deallocation
    pub fn track_deallocation(&self, bytes: u64) {
        self.resource_usage.track_deallocation(bytes);
    }
    
    /// Track instruction execution
    pub fn track_instruction(&self) -> Result<()> {
        self.resource_usage.track_instruction();
        self.check_limits()
    }
    
    /// Check if module import is allowed
    pub fn check_module_import(&self, module: &str) -> Result<()> {
        // Check denied list first
        if self.policy.denied_modules.contains(module) {
            return Err(anyhow!("Module '{}' is denied by security policy", module));
        }
        
        // If allowed list is not empty, module must be in it
        if !self.policy.allowed_modules.is_empty() && !self.policy.allowed_modules.contains(module) {
            return Err(anyhow!("Module '{}' is not in allowed list", module));
        }
        
        // Check capability
        let cap = Capability::ModuleImport { modules: vec![module.to_string()] };
        if !self.has_capability(&cap) {
            // Check wildcard capability
            let wildcard = Capability::ModuleImport { modules: vec!["*".to_string()] };
            if !self.has_capability(&wildcard) {
                return Err(anyhow!("No capability to import module '{}'", module));
            }
        }
        
        Ok(())
    }
    
    /// Terminate execution
    pub fn terminate(&self) {
        self.terminated.store(true, Ordering::Relaxed);
    }
}

/// Taint tracking for information flow security
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TaintLevel {
    /// Untainted data
    Clean,
    /// Data from untrusted sources
    Untrusted,
    /// Data that has been sanitized
    Sanitized,
    /// Highly sensitive data
    Sensitive,
}

/// Taint tracker for values
pub struct TaintTracker {
    taints: RwLock<FxHashMap<u64, TaintLevel>>,
    next_id: AtomicU64,
}

impl TaintTracker {
    pub fn new() -> Self {
        Self {
            taints: RwLock::new(FxHashMap::default()),
            next_id: AtomicU64::new(1),
        }
    }
    
    pub fn create_taint_id(&self) -> u64 {
        self.next_id.fetch_add(1, Ordering::Relaxed)
    }
    
    pub fn set_taint(&self, id: u64, level: TaintLevel) {
        self.taints.write().unwrap().insert(id, level);
    }
    
    pub fn get_taint(&self, id: u64) -> TaintLevel {
        self.taints.read().unwrap()
            .get(&id)
            .cloned()
            .unwrap_or(TaintLevel::Clean)
    }
    
    pub fn propagate_taint(&self, from_id: u64, to_id: u64) {
        let taint = self.get_taint(from_id);
        if taint != TaintLevel::Clean {
            self.set_taint(to_id, taint);
        }
    }
    
    pub fn check_taint_flow(&self, from_id: u64, to_context: &str) -> Result<()> {
        let taint = self.get_taint(from_id);
        match taint {
            TaintLevel::Sensitive => {
                Err(anyhow!("Cannot flow sensitive data to {}", to_context))
            }
            TaintLevel::Untrusted if to_context.contains("eval") || to_context.contains("exec") => {
                Err(anyhow!("Cannot flow untrusted data to {}", to_context))
            }
            _ => Ok(())
        }
    }
}

/// Module isolation context
pub struct ModuleIsolation {
    /// Module-specific globals
    module_globals: RwLock<FxHashMap<String, FxHashMap<String, Value>>>,
    /// Module capabilities
    module_capabilities: RwLock<FxHashMap<String, FxHashSet<Capability>>>,
    /// Module resource limits
    module_limits: RwLock<FxHashMap<String, SecurityPolicy>>,
}

impl ModuleIsolation {
    pub fn new() -> Self {
        Self {
            module_globals: RwLock::new(FxHashMap::default()),
            module_capabilities: RwLock::new(FxHashMap::default()),
            module_limits: RwLock::new(FxHashMap::default()),
        }
    }
    
    pub fn create_module_context(&self, module: &str, policy: SecurityPolicy) {
        self.module_globals.write().unwrap().insert(module.to_string(), FxHashMap::default());
        self.module_capabilities.write().unwrap().insert(module.to_string(), FxHashSet::default());
        self.module_limits.write().unwrap().insert(module.to_string(), policy);
    }
    
    pub fn get_module_global(&self, module: &str, name: &str) -> Option<Value> {
        self.module_globals.read().unwrap()
            .get(module)
            .and_then(|globals| globals.get(name).cloned())
    }
    
    pub fn set_module_global(&self, module: &str, name: String, value: Value) {
        if let Some(globals) = self.module_globals.write().unwrap().get_mut(module) {
            globals.insert(name, value);
        }
    }
    
    pub fn check_module_capability(&self, module: &str, capability: &Capability) -> bool {
        self.module_capabilities.read().unwrap()
            .get(module)
            .map(|caps| caps.contains(capability))
            .unwrap_or(false)
    }
}

/// Security manager for the VM
pub struct SecurityManager {
    pub context: Arc<SecurityContext>,
    pub taint_tracker: Arc<TaintTracker>,
    pub module_isolation: Arc<ModuleIsolation>,
}

impl SecurityManager {
    pub fn new(policy: SecurityPolicy) -> Self {
        Self {
            context: Arc::new(SecurityContext::new(policy)),
            taint_tracker: Arc::new(TaintTracker::new()),
            module_isolation: Arc::new(ModuleIsolation::new()),
        }
    }
    
    /// Create a sandboxed security manager
    pub fn sandbox() -> Self {
        Self::new(SecurityPolicy::sandbox())
    }
    
    /// Create a security manager for trusted code
    pub fn trusted() -> Self {
        Self::new(SecurityPolicy::trusted())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_capability_checking() {
        let ctx = SecurityContext::new(SecurityPolicy::default());
        let cap = Capability::FileSystem { paths: vec!["/tmp".to_string()] };
        
        assert!(!ctx.has_capability(&cap));
        ctx.grant_capability(cap.clone());
        assert!(ctx.has_capability(&cap));
        ctx.revoke_capability(&cap);
        assert!(!ctx.has_capability(&cap));
    }
    
    #[test]
    fn test_resource_limits() {
        let policy = SecurityPolicy {
            max_memory: 1000,
            max_instructions: 100,
            ..Default::default()
        };
        let ctx = SecurityContext::new(policy);
        
        // Should succeed under limits
        assert!(ctx.track_allocation(500).is_ok());
        for _ in 0..50 {
            assert!(ctx.track_instruction().is_ok());
        }
        
        // Should fail when exceeding memory limit
        assert!(ctx.track_allocation(600).is_err());
        
        // Should fail when exceeding instruction limit
        for _ in 0..60 {
            let _ = ctx.track_instruction();
        }
        assert!(ctx.track_instruction().is_err());
    }
    
    #[test]
    fn test_taint_tracking() {
        let tracker = TaintTracker::new();
        let id1 = tracker.create_taint_id();
        let id2 = tracker.create_taint_id();
        
        tracker.set_taint(id1, TaintLevel::Untrusted);
        assert_eq!(tracker.get_taint(id1), TaintLevel::Untrusted);
        assert_eq!(tracker.get_taint(id2), TaintLevel::Clean);
        
        tracker.propagate_taint(id1, id2);
        assert_eq!(tracker.get_taint(id2), TaintLevel::Untrusted);
        
        // Check taint flow restrictions
        assert!(tracker.check_taint_flow(id1, "eval").is_err());
        assert!(tracker.check_taint_flow(id1, "print").is_ok());
    }
}