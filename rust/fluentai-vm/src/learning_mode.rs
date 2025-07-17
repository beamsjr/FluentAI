//! Learning mode for runtime optimization exploration
//!
//! This module enables the VM to automatically explore different optimization
//! strategies during execution and learn which work best for different code patterns.

use crate::error::VMResult;
use fluentai_bytecode::Bytecode;
use fluentai_core::ast::{Graph, NodeId};
use rustc_hash::FxHashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Optimization strategy identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OptimizationStrategy {
    /// No optimization
    None,
    /// Basic optimizations (constant folding, dead code)
    Basic,
    /// Standard optimizations
    Standard,
    /// Aggressive optimizations
    Aggressive,
    /// Custom strategy with specific passes
    Custom(u32), // Bitmask of enabled passes
}

/// Performance metrics for a function execution
#[derive(Debug, Clone)]
pub struct ExecutionMetrics {
    /// Execution time
    pub duration: Duration,
    /// Number of instructions executed
    pub instruction_count: u64,
    /// Memory allocations
    pub allocations: u64,
    /// Cache misses (simulated)
    pub cache_misses: u64,
    /// Branch mispredictions (simulated)
    pub branch_mispredictions: u64,
}

/// A compiled variant of a function with a specific optimization strategy
#[derive(Debug, Clone)]
pub struct CompiledVariant {
    /// The optimization strategy used
    pub strategy: OptimizationStrategy,
    /// The compiled bytecode
    pub bytecode: Arc<Bytecode>,
    /// Compilation time
    pub compilation_time: Duration,
    /// Binary size
    pub binary_size: usize,
}

/// Tracks performance data for optimization strategies
#[derive(Debug)]
pub struct StrategyPerformance {
    /// Number of executions
    pub execution_count: u64,
    /// Average execution time
    pub avg_duration: Duration,
    /// Best execution time
    pub best_duration: Duration,
    /// Worst execution time
    pub worst_duration: Duration,
    /// Average instruction count
    pub avg_instructions: f64,
    /// Total reward (for RL)
    pub total_reward: f32,
}

impl Default for StrategyPerformance {
    fn default() -> Self {
        Self {
            execution_count: 0,
            avg_duration: Duration::ZERO,
            best_duration: Duration::MAX,
            worst_duration: Duration::ZERO,
            avg_instructions: 0.0,
            total_reward: 0.0,
        }
    }
}

/// Learning mode configuration
#[derive(Debug, Clone)]
pub struct LearningModeConfig {
    /// Minimum execution count before considering a function "hot"
    pub hot_threshold: u64,
    /// Maximum number of strategies to try per function
    pub max_strategies_per_function: usize,
    /// Whether to save learned data
    pub save_learned_data: bool,
    /// Path to save/load learned model
    pub model_path: Option<String>,
    /// Exploration rate (0.0 to 1.0)
    pub exploration_rate: f32,
    /// Whether to use RL agent for decisions
    pub use_rl_agent: bool,
    /// Exploration mode
    pub exploration_mode: ExplorationMode,
}

/// How to explore optimization combinations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExplorationMode {
    /// Smart exploration: test individuals first, then promising combinations
    Smart,
    /// Exhaustive exploration: test all 2^13 possible combinations
    Exhaustive,
    /// Quick exploration: only test predefined optimization levels
    Quick,
}

impl Default for LearningModeConfig {
    fn default() -> Self {
        Self {
            hot_threshold: 100,
            max_strategies_per_function: 4,
            save_learned_data: true,
            model_path: None,
            exploration_rate: 0.2,
            use_rl_agent: true,
            exploration_mode: ExplorationMode::Smart,
        }
    }
}

/// Manager for learning mode operations
pub struct LearningModeManager {
    /// Configuration
    pub config: LearningModeConfig,
    /// Function execution counts
    execution_counts: FxHashMap<NodeId, u64>,
    /// Compiled variants for each function
    function_variants: FxHashMap<NodeId, Vec<CompiledVariant>>,
    /// Performance data for each (function, strategy) pair
    performance_data: FxHashMap<(NodeId, OptimizationStrategy), StrategyPerformance>,
    /// Best strategy for each function
    best_strategies: FxHashMap<NodeId, OptimizationStrategy>,
    /// Functions currently being explored
    exploring_functions: FxHashMap<NodeId, ExplorationState>,
    /// RL agent connection (if enabled)
    #[cfg(feature = "ai-analysis")]
    rl_agent: Option<Box<dyn RLAgentInterface>>,
}

/// State of exploration for a function
#[derive(Debug)]
struct ExplorationState {
    /// Strategies that have been tried
    tried_strategies: Vec<OptimizationStrategy>,
    /// Strategies yet to try
    pending_strategies: Vec<OptimizationStrategy>,
    /// Current strategy being tested
    current_strategy: OptimizationStrategy,
    /// Number of executions with current strategy
    current_executions: u64,
}

/// Interface for RL agent integration
#[cfg(feature = "ai-analysis")]
pub trait RLAgentInterface: Send + Sync {
    /// Suggest optimization strategies for a function
    fn suggest_strategies(&self, graph: &Graph, node_id: NodeId) -> Vec<OptimizationStrategy>;
    
    /// Update agent with performance results
    fn update_performance(
        &mut self,
        node_id: NodeId,
        strategy: OptimizationStrategy,
        metrics: &ExecutionMetrics,
        reward: f32,
    );
    
    /// Save the learned model
    fn save_model(&self, path: &str) -> VMResult<()>;
    
    /// Load a learned model
    fn load_model(&mut self, path: &str) -> VMResult<()>;
}

impl LearningModeManager {
    /// Create a new learning mode manager
    pub fn new(config: LearningModeConfig) -> Self {
        Self {
            config,
            execution_counts: FxHashMap::default(),
            function_variants: FxHashMap::default(),
            performance_data: FxHashMap::default(),
            best_strategies: FxHashMap::default(),
            exploring_functions: FxHashMap::default(),
            #[cfg(feature = "ai-analysis")]
            rl_agent: None,
        }
    }
    
    /// Set the RL agent for intelligent optimization selection
    #[cfg(feature = "ai-analysis")]
    pub fn set_rl_agent(&mut self, agent: Box<dyn RLAgentInterface>) {
        self.rl_agent = Some(agent);
    }
    
    /// Record a function execution
    pub fn record_execution(&mut self, function_id: NodeId) -> bool {
        let count = self.execution_counts.entry(function_id).or_insert(0);
        *count += 1;
        
        // Check if function is now hot
        *count == self.config.hot_threshold
    }
    
    /// Check if a function should be explored
    pub fn should_explore(&self, function_id: NodeId) -> bool {
        if let Some(count) = self.execution_counts.get(&function_id) {
            let variant_count = self.function_variants.get(&function_id).map_or(0, |v| v.len());
            let max_allowed = match self.config.exploration_mode {
                ExplorationMode::Exhaustive => usize::MAX, // No limit in exhaustive mode
                _ => self.config.max_strategies_per_function,
            };
            
            *count >= self.config.hot_threshold
                && !self.exploring_functions.contains_key(&function_id)
                && variant_count < max_allowed
        } else {
            false
        }
    }
    
    /// Start exploring optimization strategies for a function
    pub fn start_exploration(&mut self, function_id: NodeId, graph: &Graph) -> Vec<OptimizationStrategy> {
        let strategies = if self.config.use_rl_agent {
            #[cfg(feature = "ai-analysis")]
            {
                if let Some(agent) = &self.rl_agent {
                    agent.suggest_strategies(graph, function_id)
                } else {
                    self.get_default_strategies()
                }
            }
            #[cfg(not(feature = "ai-analysis"))]
            self.get_default_strategies()
        } else {
            self.get_default_strategies()
        };
        
        let exploration = ExplorationState {
            tried_strategies: vec![],
            pending_strategies: strategies.clone(),
            current_strategy: strategies[0],
            current_executions: 0,
        };
        
        self.exploring_functions.insert(function_id, exploration);
        strategies
    }
    
    /// Get default optimization strategies to try
    fn get_default_strategies(&self) -> Vec<OptimizationStrategy> {
        match self.config.exploration_mode {
            ExplorationMode::Quick => {
                // Only test predefined optimization levels
                vec![
                    OptimizationStrategy::None,
                    OptimizationStrategy::Basic,
                    OptimizationStrategy::Standard,
                    OptimizationStrategy::Aggressive,
                ]
            }
            ExplorationMode::Smart => {
                // Smart exploration with individual optimizations and promising combinations
                let mut strategies = vec![
                    // Always start with baseline
                    OptimizationStrategy::None,
                    
                    // Basic strategies
                    OptimizationStrategy::Basic,
                    OptimizationStrategy::Standard,
                    OptimizationStrategy::Aggressive,
                ];
                
                // Add custom strategies for specific optimizations
                // Each bit in the mask enables a specific optimization
                
                // Constant folding only (bit 0)
                strategies.push(OptimizationStrategy::Custom(0x001));
                
                // Dead code elimination only (bit 1)
                strategies.push(OptimizationStrategy::Custom(0x002));
                
                // Common subexpression elimination (bit 2)
                strategies.push(OptimizationStrategy::Custom(0x004));
                
                // Function inlining (bit 3)
                strategies.push(OptimizationStrategy::Custom(0x008));
                
                // Loop optimization (bit 5)
                strategies.push(OptimizationStrategy::Custom(0x020));
                
                // Effect reordering (partial evaluation - bit 7)
                strategies.push(OptimizationStrategy::Custom(0x080));
                
                // Strength reduction (bit 8)
                strategies.push(OptimizationStrategy::Custom(0x100));
                
                // Loop invariant code motion (bit 10)
                strategies.push(OptimizationStrategy::Custom(0x400));
                
                // Function specialization (bit 11)
                strategies.push(OptimizationStrategy::Custom(0x800));
                
                // Memoization (bit 12) - cache pure function results
                strategies.push(OptimizationStrategy::Custom(0x1000));
                
                // Combination strategies
                // Constant folding + dead code (common pair)
                strategies.push(OptimizationStrategy::Custom(0x003));
                
                // Loop optimizations combo (loop opt + invariant motion)
                strategies.push(OptimizationStrategy::Custom(0x420));
                
                // Effect optimization combo (partial eval + CSE)
                strategies.push(OptimizationStrategy::Custom(0x084));
                
                // Memoization + function specialization (caching specialized functions)
                strategies.push(OptimizationStrategy::Custom(0x1800));
                
                strategies
            }
            ExplorationMode::Exhaustive => {
                // Generate all possible combinations (2^13 = 8192)
                let mut strategies = vec![OptimizationStrategy::None];
                
                // Generate all possible bit combinations from 1 to 0x1FFF (13 bits)
                for mask in 1..=0x1FFF {
                    strategies.push(OptimizationStrategy::Custom(mask));
                }
                
                // Also include the named strategies for comparison
                strategies.push(OptimizationStrategy::Basic);
                strategies.push(OptimizationStrategy::Standard);
                strategies.push(OptimizationStrategy::Aggressive);
                
                strategies
            }
        }
    }
    
    /// Add a compiled variant for a function
    pub fn add_variant(&mut self, function_id: NodeId, variant: CompiledVariant) {
        self.function_variants
            .entry(function_id)
            .or_insert_with(Vec::new)
            .push(variant);
    }
    
    /// Update performance data for a function execution
    pub fn update_performance(
        &mut self,
        function_id: NodeId,
        strategy: OptimizationStrategy,
        metrics: ExecutionMetrics,
    ) {
        let key = (function_id, strategy);
        
        // Calculate reward before mutable borrow
        let reward = self.calculate_reward(&metrics, function_id);
        
        let perf = self.performance_data.entry(key).or_default();
        
        // Update statistics
        perf.execution_count += 1;
        let count = perf.execution_count as f64;
        
        // Update average duration
        let total_nanos = perf.avg_duration.as_nanos() as f64 * (count - 1.0) 
            + metrics.duration.as_nanos() as f64;
        perf.avg_duration = Duration::from_nanos((total_nanos / count) as u64);
        
        // Update best/worst
        perf.best_duration = perf.best_duration.min(metrics.duration);
        perf.worst_duration = perf.worst_duration.max(metrics.duration);
        
        // Update average instructions
        perf.avg_instructions = (perf.avg_instructions * (count - 1.0) 
            + metrics.instruction_count as f64) / count;
        
        // Add reward
        perf.total_reward += reward;
        
        // Update RL agent if available
        #[cfg(feature = "ai-analysis")]
        {
            if let Some(agent) = &mut self.rl_agent {
                agent.update_performance(function_id, strategy, &metrics, reward);
            }
        }
        
        // Check if we should switch strategies during exploration
        if let Some(exploration) = self.exploring_functions.get_mut(&function_id) {
            exploration.current_executions += 1;
            
            // Switch to next strategy after sufficient executions
            if exploration.current_executions >= 10 {
                exploration.tried_strategies.push(exploration.current_strategy);
                if !exploration.pending_strategies.is_empty() {
                    exploration.current_strategy = exploration.pending_strategies.remove(0);
                    exploration.current_executions = 0;
                } else {
                    // Exploration complete, select best strategy
                    self.select_best_strategy(function_id);
                    self.exploring_functions.remove(&function_id);
                }
            }
        }
    }
    
    /// Calculate reward for performance metrics
    fn calculate_reward(&self, metrics: &ExecutionMetrics, function_id: NodeId) -> f32 {
        // Get baseline (no optimization) performance if available
        let baseline = self.performance_data
            .get(&(function_id, OptimizationStrategy::None))
            .map(|p| (p.avg_duration, p.avg_instructions));
        
        if let Some((base_duration, base_instructions)) = baseline {
            // Calculate improvements
            let time_improvement = (base_duration.as_nanos() as f32 - metrics.duration.as_nanos() as f32) 
                / base_duration.as_nanos() as f32;
            let instruction_improvement = (base_instructions - metrics.instruction_count as f64) 
                / base_instructions;
            
            // Weighted reward
            0.6 * time_improvement + 0.4 * instruction_improvement as f32
        } else {
            // No baseline yet, use absolute metrics
            let time_score = 1.0 / (1.0 + metrics.duration.as_secs_f32());
            let instruction_score = 1.0 / (1.0 + (metrics.instruction_count as f32 / 1000.0));
            
            0.6 * time_score + 0.4 * instruction_score
        }
    }
    
    /// Select the best strategy for a function based on collected data
    fn select_best_strategy(&mut self, function_id: NodeId) {
        let mut best_strategy = OptimizationStrategy::None;
        let mut best_score = f32::MIN;
        
        for (key, perf) in &self.performance_data {
            if key.0 == function_id && perf.execution_count > 0 {
                let score = perf.total_reward / perf.execution_count as f32;
                if score > best_score {
                    best_score = score;
                    best_strategy = key.1;
                }
            }
        }
        
        self.best_strategies.insert(function_id, best_strategy);
    }
    
    /// Get the best known strategy for a function
    pub fn get_best_strategy(&self, function_id: NodeId) -> Option<OptimizationStrategy> {
        self.best_strategies.get(&function_id).copied()
    }
    
    /// Get the best variant for a function
    pub fn get_best_variant(&self, function_id: NodeId) -> Option<&CompiledVariant> {
        let best_strategy = self.get_best_strategy(function_id)?;
        self.function_variants
            .get(&function_id)?
            .iter()
            .find(|v| v.strategy == best_strategy)
    }
    
    /// Save learned optimization data
    pub fn save_learned_data(&self, path: &str) -> VMResult<()> {
        // TODO: Implement serialization of learned data
        Ok(())
    }
    
    /// Load previously learned optimization data
    pub fn load_learned_data(&mut self, path: &str) -> VMResult<()> {
        // TODO: Implement deserialization of learned data
        Ok(())
    }
    
    /// Get statistics about learning progress
    pub fn get_statistics(&self) -> LearningStatistics {
        let exploration_progress = self.get_exploration_progress();
        
        LearningStatistics {
            functions_analyzed: self.execution_counts.len(),
            hot_functions: self.execution_counts.values().filter(|&&c| c >= self.config.hot_threshold).count(),
            functions_explored: self.best_strategies.len(),
            total_variants: self.function_variants.values().map(|v| v.len()).sum(),
            exploring_now: self.exploring_functions.len(),
            exploration_mode: self.config.exploration_mode,
            exploration_progress,
        }
    }
    
    /// Get exploration progress for all functions being explored
    pub fn get_exploration_progress(&self) -> Vec<(NodeId, usize, usize)> {
        self.exploring_functions.iter().map(|(id, state)| {
            let tested = state.tried_strategies.len();
            let total = tested + state.pending_strategies.len() + 1; // +1 for current
            (*id, tested, total)
        }).collect()
    }
}

/// Statistics about learning progress
#[derive(Debug)]
pub struct LearningStatistics {
    /// Total functions seen
    pub functions_analyzed: usize,
    /// Functions that became hot
    pub hot_functions: usize,
    /// Functions with completed exploration
    pub functions_explored: usize,
    /// Total compiled variants
    pub total_variants: usize,
    /// Functions currently being explored
    pub exploring_now: usize,
    /// Current exploration mode
    pub exploration_mode: ExplorationMode,
    /// Progress for functions being explored: (function_id, tested_count, total_count)
    pub exploration_progress: Vec<(NodeId, usize, usize)>,
}