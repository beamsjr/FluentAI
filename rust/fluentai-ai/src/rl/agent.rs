//! RL Agent for optimization decision making

use super::{OptimizationAction, OptimizationState, InlineLevel};
use crate::error::Result;
use burn::prelude::*;
use burn::module::Module;
use burn::nn;
use burn::tensor::backend::Backend;
use burn::record::{FullPrecisionSettings, Recorder, BinFileRecorder};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use rand;

/// Configuration for the RL agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentConfig {
    /// State dimension (AST features + config + history)
    pub state_dim: usize,
    /// Hidden layer dimensions
    pub hidden_dims: Vec<usize>,
    /// Action space size
    pub action_dim: usize,
    /// Learning rate
    pub learning_rate: f32,
    /// Discount factor
    pub gamma: f32,
    /// Exploration rate
    pub epsilon: f32,
    /// Epsilon decay rate
    pub epsilon_decay: f32,
    /// Minimum epsilon
    pub epsilon_min: f32,
}

impl Default for AgentConfig {
    fn default() -> Self {
        Self {
            state_dim: 64,  // Will be determined from actual state
            hidden_dims: vec![128, 64, 32],
            action_dim: 16, // Number of optimization actions
            learning_rate: 0.001,
            gamma: 0.95,
            epsilon: 1.0,
            epsilon_decay: 0.995,
            epsilon_min: 0.01,
        }
    }
}

/// Deep Q-Network for the agent
#[derive(Module, Debug)]
pub struct DQN<B: Backend> {
    fc1: nn::Linear<B>,
    fc2: nn::Linear<B>,
    fc3: nn::Linear<B>,
    output: nn::Linear<B>,
    activation: nn::Relu,
}

impl<B: Backend> DQN<B> {
    pub fn new(config: &AgentConfig, device: &B::Device) -> Self {
        let fc1 = nn::LinearConfig::new(config.state_dim, config.hidden_dims[0])
            .init(device);
        let fc2 = nn::LinearConfig::new(config.hidden_dims[0], config.hidden_dims[1])
            .init(device);
        let fc3 = nn::LinearConfig::new(config.hidden_dims[1], config.hidden_dims[2])
            .init(device);
        let output = nn::LinearConfig::new(config.hidden_dims[2], config.action_dim)
            .init(device);
        
        Self {
            fc1,
            fc2,
            fc3,
            output,
            activation: nn::Relu::new(),
        }
    }
    
    pub fn forward(&self, state: Tensor<B, 2>) -> Tensor<B, 2> {
        let x = self.activation.forward(self.fc1.forward(state));
        let x = self.activation.forward(self.fc2.forward(x));
        let x = self.activation.forward(self.fc3.forward(x));
        self.output.forward(x)
    }
}

/// RL Agent for optimization decisions
pub struct OptimizationAgent<B: Backend> {
    /// Q-network for action-value estimation
    pub(crate) q_network: DQN<B>,
    /// Target network for stable learning
    pub(crate) target_network: DQN<B>,
    /// Agent configuration
    pub(crate) config: AgentConfig,
    /// Device for tensor operations
    pub(crate) device: B::Device,
    /// Action history for analysis
    action_history: Vec<(OptimizationState, OptimizationAction, f32)>,
    /// Performance tracking
    performance_tracker: HashMap<String, Vec<f32>>,
}

impl<B: Backend> OptimizationAgent<B> {
    /// Create a new agent
    pub fn new(config: AgentConfig, device: B::Device) -> Self {
        let q_network = DQN::new(&config, &device);
        let target_network = DQN::new(&config, &device);
        
        Self {
            q_network,
            target_network,
            config,
            device,
            action_history: Vec::new(),
            performance_tracker: HashMap::new(),
        }
    }
    
    /// Select an action using epsilon-greedy policy
    pub fn select_action(&self, state: &OptimizationState, training: bool) -> OptimizationAction {
        if training && rand::random::<f32>() < self.config.epsilon {
            // Exploration: random action
            self.random_action()
        } else {
            // Exploitation: best action according to Q-network
            self.best_action(state)
        }
    }
    
    /// Get the best action according to Q-values
    fn best_action(&self, state: &OptimizationState) -> OptimizationAction {
        let state_tensor = self.state_to_tensor(state);
        let q_values = self.q_network.forward(state_tensor);
        
        // Get action with highest Q-value
        let action_idx = q_values
            .argmax(1)
            .into_scalar()
            .elem::<i32>();
        
        self.index_to_action(action_idx as usize)
    }
    
    /// Get a random action
    fn random_action(&self) -> OptimizationAction {
        let idx = rand::random::<usize>() % self.config.action_dim;
        self.index_to_action(idx)
    }
    
    /// Convert state to tensor
    fn state_to_tensor(&self, state: &OptimizationState) -> Tensor<B, 2> {
        let mut features = Vec::new();
        
        // AST features
        features.extend(&state.ast_features);
        
        // Current configuration as binary features
        features.push(if state.current_config.constant_folding { 1.0 } else { 0.0 });
        features.push(if state.current_config.dead_code_elimination { 1.0 } else { 0.0 });
        features.push(if state.current_config.cse { 1.0 } else { 0.0 });
        features.push(if state.current_config.inline { 1.0 } else { 0.0 });
        features.push(state.current_config.inline_threshold as f32 / 20.0);
        features.push(if state.current_config.tail_call_optimization { 1.0 } else { 0.0 });
        features.push(if state.current_config.loop_optimization { 1.0 } else { 0.0 });
        features.push(if state.current_config.beta_reduction { 1.0 } else { 0.0 });
        features.push(if state.current_config.partial_evaluation { 1.0 } else { 0.0 });
        features.push(if state.current_config.strength_reduction { 1.0 } else { 0.0 });
        features.push(if state.current_config.algebraic_simplification { 1.0 } else { 0.0 });
        features.push(if state.current_config.loop_invariant_code_motion { 1.0 } else { 0.0 });
        features.push(if state.current_config.function_specialization { 1.0 } else { 0.0 });
        
        // Performance history statistics
        if !state.performance_history.is_empty() {
            let mean = state.performance_history.iter().sum::<f32>() / state.performance_history.len() as f32;
            let variance = state.performance_history.iter()
                .map(|x| (x - mean).powi(2))
                .sum::<f32>() / state.performance_history.len() as f32;
            features.push(mean);
            features.push(variance.sqrt());
        } else {
            features.push(0.0);
            features.push(0.0);
        }
        
        // Resource usage statistics
        if !state.resource_history.is_empty() {
            let avg_memory = state.resource_history.iter()
                .map(|r| r.memory_bytes as f32)
                .sum::<f32>() / state.resource_history.len() as f32;
            let avg_compile_time = state.resource_history.iter()
                .map(|r| r.compilation_time_us as f32)
                .sum::<f32>() / state.resource_history.len() as f32;
            features.push(avg_memory / 1_000_000.0); // MB
            features.push(avg_compile_time / 1_000_000.0); // seconds
        } else {
            features.push(0.0);
            features.push(0.0);
        }
        
        // Pad to expected dimension
        while features.len() < self.config.state_dim {
            features.push(0.0);
        }
        features.truncate(self.config.state_dim);
        
        let tensor_1d = Tensor::<B, 1>::from_floats(features.as_slice(), &self.device);
        tensor_1d.unsqueeze::<2>() // Add batch dimension
    }
    
    /// Convert action index to action
    fn index_to_action(&self, idx: usize) -> OptimizationAction {
        match idx {
            0 => OptimizationAction::NoOp,
            1 => OptimizationAction::ConstantFolding,
            2 => OptimizationAction::DeadCodeElimination,
            3 => OptimizationAction::CSE,
            4 => OptimizationAction::Inline(InlineLevel::Conservative),
            5 => OptimizationAction::Inline(InlineLevel::Standard),
            6 => OptimizationAction::Inline(InlineLevel::Aggressive),
            7 => OptimizationAction::TailCall,
            8 => OptimizationAction::LoopOpt,
            9 => OptimizationAction::BetaReduction,
            10 => OptimizationAction::PartialEval,
            11 => OptimizationAction::StrengthReduction,
            12 => OptimizationAction::AlgebraicSimplification,
            13 => OptimizationAction::LoopInvariantMotion,
            14 => OptimizationAction::FunctionSpecialization,
            _ => OptimizationAction::Composite(0x0FF), // Common combination
        }
    }
    
    /// Update target network
    pub fn update_target_network(&mut self) {
        self.target_network = self.q_network.clone();
    }
    
    /// Decay epsilon for exploration
    pub fn decay_epsilon(&mut self) {
        self.config.epsilon = (self.config.epsilon * self.config.epsilon_decay)
            .max(self.config.epsilon_min);
    }
    
    /// Record action and performance
    pub fn record_action(&mut self, state: OptimizationState, action: OptimizationAction, reward: f32) {
        self.action_history.push((state, action, reward));
        
        // Track performance by action type
        let action_name = format!("{:?}", action);
        self.performance_tracker
            .entry(action_name)
            .or_insert_with(Vec::new)
            .push(reward);
    }
    
    /// Get performance statistics
    pub fn get_performance_stats(&self) -> HashMap<String, (f32, f32)> {
        self.performance_tracker
            .iter()
            .map(|(action, rewards)| {
                let mean = rewards.iter().sum::<f32>() / rewards.len() as f32;
                let variance = rewards.iter()
                    .map(|r| (r - mean).powi(2))
                    .sum::<f32>() / rewards.len() as f32;
                (action.clone(), (mean, variance.sqrt()))
            })
            .collect()
    }
    
    /// Save the Q-network to a file
    pub fn save_model(&self, path: &str) -> Result<()> {
        let recorder = BinFileRecorder::<FullPrecisionSettings>::new();
        self.q_network
            .clone()
            .save_file(path, &recorder)
            .map_err(|e| crate::error::AiError::model_loading(format!("Failed to save model: {}", e)))?;
        Ok(())
    }
    
    /// Load a Q-network from a file
    pub fn load_model(path: &str, config: &AgentConfig, device: B::Device) -> Result<Self> {
        let recorder = BinFileRecorder::<FullPrecisionSettings>::new();
        let q_network = DQN::<B>::new(config, &device)
            .load_file(path, &recorder, &device)
            .map_err(|e| crate::error::AiError::model_loading(format!("Failed to load model: {}", e)))?;
        
        let target_network = q_network.clone();
        
        Ok(Self {
            q_network,
            target_network,
            config: config.clone(),
            device,
            action_history: Vec::new(),
            performance_tracker: HashMap::new(),
        })
    }
}