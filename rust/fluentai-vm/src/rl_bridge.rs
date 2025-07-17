//! Bridge between VM and RL optimization agent
//!
//! This module provides the implementation of RLAgentInterface that connects
//! the fluentai-ai RL agent to the VM's learning mode.

#[cfg(feature = "ai-analysis")]
use crate::learning_mode::{RLAgentInterface, OptimizationStrategy, ExecutionMetrics};
use crate::error::VMResult;
use fluentai_core::ast::{Graph, NodeId};
use std::sync::{Arc, RwLock};

/// Bridge to connect fluentai-ai RL agent to VM
#[cfg(feature = "ai-analysis")]
pub struct RLAgentBridge {
    /// The actual RL agent from fluentai-ai
    /// We use dynamic dispatch here to avoid circular dependencies
    agent: Arc<RwLock<Box<dyn RLAgentTrait>>>,
}

/// Trait that the fluentai-ai agent must implement
#[cfg(feature = "ai-analysis")]
pub trait RLAgentTrait: Send + Sync {
    /// Suggest optimization strategies for a function
    fn suggest_strategies(&self, graph: &Graph, node_id: NodeId) -> Vec<OptimizationStrategy>;
    
    /// Update with performance results
    fn update(&mut self, node_id: NodeId, strategy: OptimizationStrategy, metrics: &ExecutionMetrics, reward: f32);
    
    /// Save model
    fn save(&self, path: &str) -> VMResult<()>;
    
    /// Load model
    fn load(&mut self, path: &str) -> VMResult<()>;
}

#[cfg(feature = "ai-analysis")]
impl RLAgentBridge {
    /// Create a new bridge with the given agent
    pub fn new(agent: Box<dyn RLAgentTrait>) -> Self {
        Self {
            agent: Arc::new(RwLock::new(agent)),
        }
    }
}

#[cfg(feature = "ai-analysis")]
impl RLAgentInterface for RLAgentBridge {
    fn suggest_strategies(&self, graph: &Graph, node_id: NodeId) -> Vec<OptimizationStrategy> {
        self.agent.read().unwrap().suggest_strategies(graph, node_id)
    }
    
    fn update_performance(
        &mut self,
        node_id: NodeId,
        strategy: OptimizationStrategy,
        metrics: &ExecutionMetrics,
        reward: f32,
    ) {
        self.agent.write().unwrap().update(node_id, strategy, metrics, reward)
    }
    
    fn save_model(&self, path: &str) -> VMResult<()> {
        self.agent.read().unwrap().save(path)
    }
    
    fn load_model(&mut self, path: &str) -> VMResult<()> {
        self.agent.write().unwrap().load(path)
    }
}

/// Convert between VM and optimizer OptimizationStrategy types
#[cfg(feature = "ai-analysis")]
pub fn vm_to_optimizer_strategy(strategy: OptimizationStrategy) -> fluentai_core::traits::OptimizationLevel {
    match strategy {
        OptimizationStrategy::None => fluentai_core::traits::OptimizationLevel::None,
        OptimizationStrategy::Basic => fluentai_core::traits::OptimizationLevel::Basic,
        OptimizationStrategy::Standard => fluentai_core::traits::OptimizationLevel::Standard,
        OptimizationStrategy::Aggressive => fluentai_core::traits::OptimizationLevel::Aggressive,
        OptimizationStrategy::Custom(_) => fluentai_core::traits::OptimizationLevel::Standard, // Default for custom
    }
}

/// Convert from optimizer to VM optimization strategy
#[cfg(feature = "ai-analysis")]
pub fn optimizer_to_vm_strategy(level: fluentai_core::traits::OptimizationLevel) -> OptimizationStrategy {
    match level {
        fluentai_core::traits::OptimizationLevel::None => OptimizationStrategy::None,
        fluentai_core::traits::OptimizationLevel::Basic => OptimizationStrategy::Basic,
        fluentai_core::traits::OptimizationLevel::Standard => OptimizationStrategy::Standard,
        fluentai_core::traits::OptimizationLevel::Aggressive => OptimizationStrategy::Aggressive,
    }
}