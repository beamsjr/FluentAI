//! RL Environment for optimization learning

use super::{OptimizationAction, OptimizationState, OptimizationConfig, ResourceMetrics};
use crate::error::Result;
use fluentai_core::ast::Graph;
use std::time::Instant;
use std::collections::HashMap;

/// Environment state for RL
#[derive(Debug, Clone)]
pub struct EnvironmentState {
    /// Current AST graph
    pub graph: Graph,
    /// Current optimization configuration
    pub config: OptimizationConfig,
    /// Performance metrics from last run
    pub last_metrics: Option<PerformanceMetrics>,
    /// Step count in current episode
    pub step_count: usize,
}

/// Performance metrics from execution
#[derive(Debug, Clone, Copy)]
pub struct PerformanceMetrics {
    /// Execution time in microseconds
    pub execution_time_us: u64,
    /// Memory usage in bytes
    pub memory_bytes: u64,
    /// Binary size in bytes
    pub binary_size: u64,
    /// Number of instructions executed
    pub instruction_count: u64,
    /// Cache misses
    pub cache_misses: u64,
    /// Branch mispredictions
    pub branch_mispredictions: u64,
}

/// Optimization environment for RL
pub struct OptimizationEnvironment {
    /// Maximum steps per episode
    max_steps: usize,
    /// Performance baseline (no optimization)
    pub(crate) baseline_metrics: Option<PerformanceMetrics>,
    /// Compilation function
    compile_fn: Box<dyn Fn(&Graph, &HashMap<String, serde_json::Value>) -> Result<CompiledProgram>>,
    /// Execution function
    execute_fn: Box<dyn Fn(&CompiledProgram) -> Result<PerformanceMetrics>>,
}

/// Compiled program representation
pub struct CompiledProgram {
    pub bytecode: Vec<u8>,
    pub compilation_time_us: u64,
    pub binary_size: u64,
}

impl OptimizationEnvironment {
    /// Create a new environment
    pub fn new(
        max_steps: usize,
        compile_fn: impl Fn(&Graph, &HashMap<String, serde_json::Value>) -> Result<CompiledProgram> + 'static,
        execute_fn: impl Fn(&CompiledProgram) -> Result<PerformanceMetrics> + 'static,
    ) -> Self {
        Self {
            max_steps,
            baseline_metrics: None,
            compile_fn: Box::new(compile_fn),
            execute_fn: Box::new(execute_fn),
        }
    }
    
    /// Reset environment with new AST
    pub fn reset(&mut self, graph: Graph) -> Result<EnvironmentState> {
        // Establish baseline performance with no optimization
        let baseline_config = OptimizationConfig::default().to_dict();
        let baseline_program = (self.compile_fn)(&graph, &baseline_config)?;
        self.baseline_metrics = Some((self.execute_fn)(&baseline_program)?);
        
        Ok(EnvironmentState {
            graph,
            config: OptimizationConfig::default(),
            last_metrics: None,
            step_count: 0,
        })
    }
    
    /// Take a step in the environment
    pub fn step(&mut self, state: &mut EnvironmentState, action: OptimizationAction) -> Result<StepResult> {
        // Apply action to configuration
        state.config.apply_action(action);
        state.step_count += 1;
        
        // Compile with current configuration
        let optimizer_config = state.config.to_dict();
        let start_compile = Instant::now();
        let compiled = (self.compile_fn)(&state.graph, &optimizer_config)?;
        let compilation_time = start_compile.elapsed().as_micros() as u64;
        
        // Execute and measure performance
        let metrics = (self.execute_fn)(&compiled)?;
        
        // Calculate reward
        let reward = self.calculate_reward(
            metrics,
            self.baseline_metrics.unwrap(),
            compilation_time,
            compiled.binary_size,
        );
        
        // Create resource metrics
        let resource_metrics = ResourceMetrics {
            memory_bytes: metrics.memory_bytes,
            compilation_time_us: compilation_time,
            binary_size: compiled.binary_size,
        };
        
        // Update state
        state.last_metrics = Some(metrics);
        
        // Check if episode is done
        let done = state.step_count >= self.max_steps;
        
        Ok(StepResult {
            reward,
            done,
            metrics,
            resource_metrics,
        })
    }
    
    /// Calculate reward based on performance improvement
    fn calculate_reward(
        &self,
        current: PerformanceMetrics,
        baseline: PerformanceMetrics,
        compilation_time: u64,
        binary_size: u64,
    ) -> f32 {
        // Execution time improvement (higher is better)
        let exec_improvement = (baseline.execution_time_us as f32 - current.execution_time_us as f32) 
            / baseline.execution_time_us as f32;
        
        // Memory usage improvement (higher is better)
        let memory_improvement = (baseline.memory_bytes as f32 - current.memory_bytes as f32)
            / baseline.memory_bytes as f32;
        
        // Binary size improvement (higher is better)
        let size_improvement = (baseline.binary_size as f32 - binary_size as f32)
            / baseline.binary_size as f32;
        
        // Instruction count improvement
        let instruction_improvement = (baseline.instruction_count as f32 - current.instruction_count as f32)
            / baseline.instruction_count as f32;
        
        // Cache performance improvement
        let cache_improvement = if baseline.cache_misses > 0 {
            (baseline.cache_misses as f32 - current.cache_misses as f32)
                / baseline.cache_misses as f32
        } else {
            0.0
        };
        
        // Branch prediction improvement
        let branch_improvement = if baseline.branch_mispredictions > 0 {
            (baseline.branch_mispredictions as f32 - current.branch_mispredictions as f32)
                / baseline.branch_mispredictions as f32
        } else {
            0.0
        };
        
        // Compilation time penalty (normalized)
        let compile_penalty = (compilation_time as f32 / 1_000_000.0).min(1.0) * 0.1;
        
        // Weighted reward calculation
        let reward = 
            exec_improvement * 0.4 +        // Execution time is most important
            memory_improvement * 0.2 +       // Memory usage
            size_improvement * 0.1 +         // Binary size
            instruction_improvement * 0.15 + // Instruction efficiency
            cache_improvement * 0.1 +        // Cache performance
            branch_improvement * 0.05 -      // Branch prediction
            compile_penalty;                 // Compilation overhead
        
        // Clip reward to reasonable range
        reward.max(-1.0).min(1.0)
    }
    
    /// Get current state representation for the agent
    pub fn get_state(&self, env_state: &EnvironmentState) -> OptimizationState {
        // Extract AST features
        let ast_features = self.extract_ast_features(&env_state.graph);
        
        // Create performance history
        let performance_history = if let Some(metrics) = env_state.last_metrics {
            vec![metrics.execution_time_us as f32 / 1_000_000.0] // Convert to seconds
        } else {
            vec![]
        };
        
        // Create resource history
        let resource_history = if let Some(metrics) = env_state.last_metrics {
            vec![ResourceMetrics {
                memory_bytes: metrics.memory_bytes,
                compilation_time_us: 0, // Will be updated after compilation
                binary_size: 0,         // Will be updated after compilation
            }]
        } else {
            vec![]
        };
        
        OptimizationState {
            ast_features,
            current_config: env_state.config.clone(),
            performance_history,
            resource_history,
        }
    }
    
    /// Extract features from AST
    fn extract_ast_features(&self, graph: &Graph) -> Vec<f32> {
        #[cfg(feature = "ai-analysis")]
        {
            use crate::features::extract_graph_features;
            extract_graph_features(graph).unwrap_or_else(|_| vec![0.0; 32])
        }
        
        #[cfg(not(feature = "ai-analysis"))]
        {
            // Basic features when AI analysis is disabled
            vec![
                graph.nodes.len() as f32,
                0.0, // Would be edge count
                0.0, // Would be max depth
                0.0, // Would be branching factor
                // ... pad to expected size
            ]
        }
    }
}

/// Result of an environment step
#[derive(Debug)]
pub struct StepResult {
    /// Reward for the action
    pub reward: f32,
    /// Whether the episode is done
    pub done: bool,
    /// Performance metrics
    pub metrics: PerformanceMetrics,
    /// Resource usage metrics
    pub resource_metrics: ResourceMetrics,
}