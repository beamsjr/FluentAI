//! Adapter to connect RL agent to VM learning mode
//!
//! This adapter provides a simplified interface that doesn't require
//! the RL agent to be thread-safe, which is important because Burn's
//! neural network layers are not Sync.

use super::{OptimizationAction, AgentConfig};
use fluentai_core::ast::{Graph, NodeId};
use std::sync::{Arc, Mutex};
use serde::{Serialize, Deserialize};

// Use the RLAgentInterface trait from fluentai-vm's learning_mode module
use fluentai_vm::learning_mode::{RLAgentInterface, ExecutionMetrics, OptimizationStrategy};

/// Performance entry for serialization
#[derive(Serialize, Deserialize)]
struct PerformanceEntry {
    strategy_name: String,
    reward: f32,
}

/// Simple adapter that implements the VM's RLAgentInterface
/// 
/// This adapter provides basic optimization suggestions without
/// maintaining a stateful RL agent, avoiding thread safety issues.
pub struct VMRLAdapter {
    /// Configuration for optimization decisions
    config: Arc<AgentConfig>,
    /// Performance history for basic heuristics
    history: Arc<Mutex<Vec<(OptimizationStrategy, f32)>>>,
}

impl VMRLAdapter {
    /// Create a new adapter with default configuration
    pub fn new() -> Self {
        Self {
            config: Arc::new(AgentConfig::default()),
            history: Arc::new(Mutex::new(Vec::new())),
        }
    }
    
    /// Create a new adapter with custom configuration
    pub fn with_config(config: AgentConfig) -> Self {
        Self {
            config: Arc::new(config),
            history: Arc::new(Mutex::new(Vec::new())),
        }
    }
    
    /// Convert VM optimization strategy to RL action
    fn strategy_to_action(strategy: OptimizationStrategy) -> OptimizationAction {
        match strategy {
            OptimizationStrategy::None => OptimizationAction::NoOp,
            OptimizationStrategy::Basic => {
                // Basic = constant folding + dead code
                OptimizationAction::Composite(0b0011)
            }
            OptimizationStrategy::Standard => {
                // Standard = basic + CSE + inline
                OptimizationAction::Composite(0b0111)
            }
            OptimizationStrategy::Aggressive => {
                // Aggressive = all optimizations
                OptimizationAction::Composite(0xFF)
            }
            OptimizationStrategy::Custom(mask) => {
                OptimizationAction::Composite(mask)
            }
        }
    }
    
    /// Convert RL action to VM optimization strategy
    fn action_to_strategy(action: OptimizationAction) -> OptimizationStrategy {
        match action {
            OptimizationAction::NoOp => OptimizationStrategy::None,
            OptimizationAction::ConstantFolding | OptimizationAction::DeadCodeElimination => {
                OptimizationStrategy::Basic
            }
            OptimizationAction::CSE | OptimizationAction::Inline(_) => {
                OptimizationStrategy::Standard
            }
            OptimizationAction::Composite(mask) => {
                OptimizationStrategy::Custom(mask)
            }
            _ => OptimizationStrategy::Aggressive,
        }
    }
}

/// Implementation of VM's RLAgentInterface
impl RLAgentInterface for VMRLAdapter {
    fn suggest_strategies(
        &self,
        graph: &Graph,
        _node_id: NodeId,
    ) -> Vec<OptimizationStrategy> {
        let mut strategies = Vec::new();
        
        // Always try no optimization as baseline
        strategies.push(OptimizationStrategy::None);
        
        // Use simple heuristics based on graph size
        let node_count = graph.nodes.len();
        
        if node_count < 50 {
            // Small graphs: try all strategies
            strategies.push(OptimizationStrategy::Basic);
            strategies.push(OptimizationStrategy::Standard);
            strategies.push(OptimizationStrategy::Aggressive);
        } else if node_count < 200 {
            // Medium graphs: skip aggressive
            strategies.push(OptimizationStrategy::Basic);
            strategies.push(OptimizationStrategy::Standard);
        } else {
            // Large graphs: only basic
            strategies.push(OptimizationStrategy::Basic);
        }
        
        // Check history for good performers
        if let Ok(history) = self.history.lock() {
            if history.len() > 10 {
                // Find best performing strategy from history
                if let Some((best_strategy, _)) = history.iter()
                    .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal))
                {
                    if !strategies.contains(best_strategy) {
                        strategies.push(*best_strategy);
                    }
                }
            }
        }
        
        strategies
    }
    
    fn update_performance(
        &mut self,
        _node_id: NodeId,
        strategy: OptimizationStrategy,
        metrics: &ExecutionMetrics,
        reward: f32,
    ) {
        // Store performance data
        if let Ok(mut history) = self.history.lock() {
            history.push((strategy, reward));
            
            // Keep only recent history
            if history.len() > 100 {
                history.drain(0..50);
            }
        }
        
        // Log performance for analysis
        tracing::debug!(
            "Optimization performance - Strategy: {:?}, Duration: {:?}, Instructions: {}, Reward: {}",
            strategy,
            metrics.duration,
            metrics.instruction_count,
            reward
        );
    }
    
    fn save_model(&self, path: &str) -> fluentai_vm::VMResult<()> {
        // Save history as simple JSON
        if let Ok(history) = self.history.lock() {
            // Convert to serializable format
            let entries: Vec<PerformanceEntry> = history.iter()
                .map(|(strategy, reward)| PerformanceEntry {
                    strategy_name: format!("{:?}", strategy),
                    reward: *reward,
                })
                .collect();
            
            let data = serde_json::json!({
                "config": &*self.config,
                "history": entries,
            });
            
            std::fs::write(path, serde_json::to_string_pretty(&data).unwrap())
                .map_err(|e| fluentai_vm::VMError::RuntimeError { 
                    message: format!("Failed to save model: {}", e), 
                    stack_trace: None 
                })?;
        }
        
        Ok(())
    }
    
    fn load_model(&mut self, path: &str) -> fluentai_vm::VMResult<()> {
        // Load history from JSON
        let data = std::fs::read_to_string(path)
            .map_err(|e| fluentai_vm::VMError::RuntimeError { 
                message: format!("Failed to load model: {}", e), 
                stack_trace: None 
            })?;
            
        let json: serde_json::Value = serde_json::from_str(&data)
            .map_err(|e| fluentai_vm::VMError::RuntimeError { 
                message: format!("Failed to parse model: {}", e), 
                stack_trace: None 
            })?;
            
        // Update history if available
        if let Some(_history_data) = json.get("history") {
            if let Ok(mut history) = self.history.lock() {
                history.clear();
                // Note: We're not deserializing the full history here for simplicity
                // In a real implementation, you'd properly deserialize the performance entries
            }
        }
        
        Ok(())
    }
}

/// Create a default VM adapter
pub fn create_vm_adapter() -> Box<dyn RLAgentInterface> {
    Box::new(VMRLAdapter::new())
}