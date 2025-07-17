//! Comprehensive profiling infrastructure for runtime optimization
//!
//! This module provides detailed execution profiling to enable runtime
//! optimizations such as hot path inlining, value profiling, and branch
//! prediction optimization.

use fluentai_core::ast::NodeId;
use fluentai_bytecode::Opcode;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::Duration;
use web_time::Instant;

/// Custom serde module for Duration
mod duration_serde {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::time::Duration;
    
    pub fn serialize<S>(duration: &Duration, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        duration.as_secs_f64().serialize(serializer)
    }
    
    pub fn deserialize<'de, D>(deserializer: D) -> Result<Duration, D::Error>
    where
        D: Deserializer<'de>,
    {
        let secs = f64::deserialize(deserializer)?;
        Ok(Duration::from_secs_f64(secs))
    }
}

/// Profiling data for a single bytecode instruction
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct InstructionProfile {
    /// Number of times this instruction was executed
    pub execution_count: u64,
    /// Total time spent executing this instruction
    #[serde(with = "duration_serde")]
    pub total_time: Duration,
    /// Average execution time
    pub avg_time_ns: u64,
    /// Branch taken count (for conditional jumps)
    pub branch_taken: u64,
    /// Branch not taken count
    pub branch_not_taken: u64,
}

/// Profiling data for a function/chunk
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FunctionProfile {
    /// Chunk ID
    pub chunk_id: usize,
    /// Function name if available
    pub name: Option<String>,
    /// Total execution count
    pub call_count: u64,
    /// Total execution time
    #[serde(with = "duration_serde")]
    pub total_time: Duration,
    /// Average execution time
    pub avg_time_ns: u64,
    /// Maximum execution time
    pub max_time_ns: u64,
    /// Instruction-level profiles
    pub instructions: HashMap<usize, InstructionProfile>,
    /// Is this a hot function?
    pub is_hot: bool,
    /// Arguments profile (value -> count)
    pub arg_profiles: Vec<HashMap<String, u64>>,
}

/// Value profiling data
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ValueProfile {
    /// Node ID where value was observed
    pub node_id: NodeId,
    /// Value observations (serialized value -> count)
    pub observations: HashMap<String, u64>,
    /// Most common value if any
    pub most_common: Option<(String, u64)>,
    /// Total observations
    pub total_observations: u64,
}

/// Memory access pattern data
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MemoryProfile {
    /// Cache misses detected
    pub cache_misses: u64,
    /// Sequential access count
    pub sequential_accesses: u64,
    /// Random access count
    pub random_accesses: u64,
    /// Hot memory regions
    pub hot_regions: Vec<(usize, usize)>, // (start, size)
}

/// Loop profiling data
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LoopProfile {
    /// Node ID of the loop
    pub node_id: NodeId,
    /// Iteration counts observed
    pub iteration_counts: Vec<u64>,
    /// Average iteration count
    pub avg_iterations: f64,
    /// Maximum iteration count
    pub max_iterations: u64,
    /// Is this a hot loop?
    pub is_hot: bool,
}

/// Comprehensive profiler for the VM
pub struct Profiler {
    /// Function profiles indexed by chunk ID
    functions: Arc<RwLock<HashMap<usize, FunctionProfile>>>,
    /// Value profiles indexed by node ID
    values: Arc<RwLock<HashMap<NodeId, ValueProfile>>>,
    /// Loop profiles
    loops: Arc<RwLock<HashMap<NodeId, LoopProfile>>>,
    /// Global memory profile
    memory: Arc<RwLock<MemoryProfile>>,
    /// Profiling enabled flag
    enabled: Arc<RwLock<bool>>,
    /// Hot path threshold (execution count)
    hot_threshold: u64,
}

impl Profiler {
    /// Create a new profiler
    pub fn new() -> Self {
        Self {
            functions: Arc::new(RwLock::new(HashMap::new())),
            values: Arc::new(RwLock::new(HashMap::new())),
            loops: Arc::new(RwLock::new(HashMap::new())),
            memory: Arc::new(RwLock::new(MemoryProfile {
                cache_misses: 0,
                sequential_accesses: 0,
                random_accesses: 0,
                hot_regions: Vec::new(),
            })),
            enabled: Arc::new(RwLock::new(false)),
            hot_threshold: 1000,
        }
    }

    /// Enable profiling
    pub fn enable(&self) {
        *self.enabled.write().unwrap() = true;
    }

    /// Disable profiling
    pub fn disable(&self) {
        *self.enabled.write().unwrap() = false;
    }

    /// Check if profiling is enabled
    pub fn is_enabled(&self) -> bool {
        *self.enabled.read().unwrap()
    }

    /// Record function entry
    pub fn record_function_entry(&self, chunk_id: usize, name: Option<String>) -> Option<Instant> {
        if !self.is_enabled() {
            return None;
        }

        let mut functions = self.functions.write().unwrap();
        let profile = functions.entry(chunk_id).or_insert_with(|| FunctionProfile {
            chunk_id,
            name,
            call_count: 0,
            total_time: Duration::ZERO,
            avg_time_ns: 0,
            max_time_ns: 0,
            instructions: HashMap::new(),
            is_hot: false,
            arg_profiles: Vec::new(),
        });
        
        profile.call_count += 1;
        
        // Check if function became hot
        if profile.call_count >= self.hot_threshold {
            profile.is_hot = true;
        }
        
        Some(Instant::now())
    }

    /// Record function exit
    pub fn record_function_exit(&self, chunk_id: usize, start_time: Instant) {
        if !self.is_enabled() {
            return;
        }

        let elapsed = start_time.elapsed();
        let elapsed_ns = elapsed.as_nanos() as u64;

        let mut functions = self.functions.write().unwrap();
        if let Some(profile) = functions.get_mut(&chunk_id) {
            profile.total_time += elapsed;
            profile.avg_time_ns = profile.total_time.as_nanos() as u64 / profile.call_count;
            profile.max_time_ns = profile.max_time_ns.max(elapsed_ns);
        }
    }

    /// Record instruction execution
    pub fn record_instruction(&self, chunk_id: usize, pc: usize, opcode: Opcode, start_time: Instant) {
        if !self.is_enabled() {
            return;
        }

        let elapsed = start_time.elapsed();

        let mut functions = self.functions.write().unwrap();
        if let Some(func_profile) = functions.get_mut(&chunk_id) {
            let instr_profile = func_profile.instructions.entry(pc).or_default();
            instr_profile.execution_count += 1;
            instr_profile.total_time += elapsed;
            instr_profile.avg_time_ns = instr_profile.total_time.as_nanos() as u64 / instr_profile.execution_count;
        }
    }

    /// Record branch result
    pub fn record_branch(&self, chunk_id: usize, pc: usize, taken: bool) {
        if !self.is_enabled() {
            return;
        }

        let mut functions = self.functions.write().unwrap();
        if let Some(func_profile) = functions.get_mut(&chunk_id) {
            let instr_profile = func_profile.instructions.entry(pc).or_default();
            if taken {
                instr_profile.branch_taken += 1;
            } else {
                instr_profile.branch_not_taken += 1;
            }
        }
    }

    /// Record value observation
    pub fn record_value(&self, node_id: NodeId, value: &str) {
        if !self.is_enabled() {
            return;
        }

        let mut values = self.values.write().unwrap();
        let profile = values.entry(node_id).or_insert_with(|| ValueProfile {
            node_id,
            observations: HashMap::new(),
            most_common: None,
            total_observations: 0,
        });

        *profile.observations.entry(value.to_string()).or_insert(0) += 1;
        profile.total_observations += 1;

        // Update most common value
        let (most_common_val, count) = profile.observations
            .iter()
            .max_by_key(|(_, count)| *count)
            .map(|(v, c)| (v.clone(), *c))
            .unwrap_or_default();
        
        profile.most_common = Some((most_common_val, count));
    }

    /// Record loop iteration
    pub fn record_loop_iteration(&self, node_id: NodeId, iteration_count: u64) {
        if !self.is_enabled() {
            return;
        }

        let mut loops = self.loops.write().unwrap();
        let profile = loops.entry(node_id).or_insert_with(|| LoopProfile {
            node_id,
            iteration_counts: Vec::new(),
            avg_iterations: 0.0,
            max_iterations: 0,
            is_hot: false,
        });

        profile.iteration_counts.push(iteration_count);
        profile.max_iterations = profile.max_iterations.max(iteration_count);
        profile.avg_iterations = profile.iteration_counts.iter().sum::<u64>() as f64 
            / profile.iteration_counts.len() as f64;

        // Check if loop is hot (either by frequency or iteration count)
        if profile.iteration_counts.len() >= (self.hot_threshold / 10) as usize 
            || profile.avg_iterations > 100.0 {
            profile.is_hot = true;
        }
    }

    /// Record memory access pattern
    pub fn record_memory_access(&self, address: usize, is_sequential: bool) {
        if !self.is_enabled() {
            return;
        }

        let mut memory = self.memory.write().unwrap();
        if is_sequential {
            memory.sequential_accesses += 1;
        } else {
            memory.random_accesses += 1;
        }
    }

    /// Get hot functions
    pub fn get_hot_functions(&self) -> Vec<FunctionProfile> {
        self.functions.read().unwrap()
            .values()
            .filter(|p| p.is_hot)
            .cloned()
            .collect()
    }

    /// Get hot loops
    pub fn get_hot_loops(&self) -> Vec<LoopProfile> {
        self.loops.read().unwrap()
            .values()
            .filter(|p| p.is_hot)
            .cloned()
            .collect()
    }

    /// Get value profiles with high skew (good candidates for specialization)
    pub fn get_skewed_values(&self, threshold: f64) -> Vec<ValueProfile> {
        self.values.read().unwrap()
            .values()
            .filter(|p| {
                if let Some((_, count)) = &p.most_common {
                    (*count as f64 / p.total_observations as f64) > threshold
                } else {
                    false
                }
            })
            .cloned()
            .collect()
    }

    /// Get branch prediction data
    pub fn get_branch_bias(&self, chunk_id: usize, pc: usize) -> Option<f64> {
        let functions = self.functions.read().unwrap();
        functions.get(&chunk_id)
            .and_then(|f| f.instructions.get(&pc))
            .map(|i| {
                let total = i.branch_taken + i.branch_not_taken;
                if total > 0 {
                    i.branch_taken as f64 / total as f64
                } else {
                    0.5
                }
            })
    }

    /// Export profile data for offline analysis
    pub fn export_profile(&self) -> ProfileData {
        ProfileData {
            functions: self.functions.read().unwrap().clone(),
            values: self.values.read().unwrap().clone(),
            loops: self.loops.read().unwrap().clone(),
            memory: self.memory.read().unwrap().clone(),
        }
    }

    /// Import profile data from previous run
    pub fn import_profile(&self, data: ProfileData) {
        *self.functions.write().unwrap() = data.functions;
        *self.values.write().unwrap() = data.values;
        *self.loops.write().unwrap() = data.loops;
        *self.memory.write().unwrap() = data.memory;
    }

    /// Clear all profiling data
    pub fn clear(&self) {
        self.functions.write().unwrap().clear();
        self.values.write().unwrap().clear();
        self.loops.write().unwrap().clear();
        *self.memory.write().unwrap() = MemoryProfile {
            cache_misses: 0,
            sequential_accesses: 0,
            random_accesses: 0,
            hot_regions: Vec::new(),
        };
    }
}

/// Serializable profile data
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ProfileData {
    pub functions: HashMap<usize, FunctionProfile>,
    pub values: HashMap<NodeId, ValueProfile>,
    pub loops: HashMap<NodeId, LoopProfile>,
    pub memory: MemoryProfile,
}


impl Default for Profiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_profiling() {
        let profiler = Profiler::new();
        profiler.enable();

        // Record function execution
        let start = profiler.record_function_entry(1, Some("test_func".to_string())).unwrap();
        std::thread::sleep(Duration::from_millis(1));
        profiler.record_function_exit(1, start);

        let functions = profiler.functions.read().unwrap();
        let profile = functions.get(&1).unwrap();
        assert_eq!(profile.call_count, 1);
        assert!(profile.total_time.as_nanos() > 0);
    }

    #[test]
    fn test_hot_function_detection() {
        let profiler = Profiler::new();
        profiler.enable();

        // Execute function many times
        for _ in 0..1001 {
            let start = profiler.record_function_entry(1, None).unwrap();
            profiler.record_function_exit(1, start);
        }

        let hot_functions = profiler.get_hot_functions();
        assert_eq!(hot_functions.len(), 1);
        assert_eq!(hot_functions[0].chunk_id, 1);
    }

    #[test]
    fn test_value_profiling() {
        let profiler = Profiler::new();
        profiler.enable();

        let node_id = NodeId(std::num::NonZeroU32::new(1).unwrap());
        
        // Record values with skew
        for _ in 0..80 {
            profiler.record_value(node_id, "42");
        }
        for _ in 0..20 {
            profiler.record_value(node_id, "0");
        }

        let skewed = profiler.get_skewed_values(0.7);
        assert_eq!(skewed.len(), 1);
        assert_eq!(skewed[0].most_common.as_ref().unwrap().0, "42");
    }
}