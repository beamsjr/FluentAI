//! Training loop for RL optimization agent

use super::{
    OptimizationAgent, OptimizationEnvironment, ExperienceReplay, 
    Experience, RewardCalculator, AgentConfig, OptimizationState
};
use crate::error::Result;
use burn::prelude::*;
use burn::optim::AdamConfig;
use burn::tensor::backend::Backend;
use burn::nn;
use burn::nn::loss::Reduction;
use fluentai_core::ast::Graph;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Training configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrainingConfig {
    /// Number of episodes to train
    pub num_episodes: usize,
    /// Maximum steps per episode
    pub max_steps_per_episode: usize,
    /// Batch size for training
    pub batch_size: usize,
    /// Update target network every N episodes
    pub target_update_frequency: usize,
    /// Minimum experiences before training
    pub min_replay_size: usize,
    /// Training frequency (train every N steps)
    pub train_frequency: usize,
    /// Save model every N episodes
    pub save_frequency: usize,
    /// Model save path
    pub model_path: String,
    /// Experience save path
    pub experience_path: String,
}

impl Default for TrainingConfig {
    fn default() -> Self {
        Self {
            num_episodes: 1000,
            max_steps_per_episode: 50,
            batch_size: 32,
            target_update_frequency: 10,
            min_replay_size: 1000,
            train_frequency: 4,
            save_frequency: 100,
            model_path: "models/rl_optimizer.bin".to_string(),
            experience_path: "models/experiences.bin".to_string(),
        }
    }
}

/// RL trainer for optimization agent
pub struct Trainer<B: Backend> {
    agent: OptimizationAgent<B>,
    environment: OptimizationEnvironment,
    experience_replay: ExperienceReplay,
    reward_calculator: RewardCalculator,
    config: TrainingConfig,
    optimizer_config: AdamConfig,
    device: B::Device,
    /// Training statistics
    episode_rewards: Vec<f32>,
    episode_lengths: Vec<usize>,
}

impl<B: Backend> Trainer<B> {
    /// Create new trainer
    pub fn new(
        agent_config: AgentConfig,
        training_config: TrainingConfig,
        environment: OptimizationEnvironment,
        device: B::Device,
    ) -> Self {
        let agent = OptimizationAgent::new(agent_config, device.clone());
        let experience_replay = ExperienceReplay::new(10000);
        let reward_calculator = RewardCalculator::new(
            Default::default(),
            Default::default(),
        );
        
        let optimizer_config = AdamConfig::new()
            .with_epsilon(1e-8);
        
        Self {
            agent,
            environment,
            experience_replay,
            reward_calculator,
            config: training_config,
            optimizer_config,
            device,
            episode_rewards: Vec::new(),
            episode_lengths: Vec::new(),
        }
    }
    
    /// Train the agent on a dataset of AST graphs
    pub fn train(&mut self, graphs: Vec<Graph>) -> Result<TrainingStats> {
        println!("Starting RL training with {} graphs", graphs.len());
        
        let mut total_steps = 0;
        
        for episode in 0..self.config.num_episodes {
            // Select random graph for this episode
            let graph_idx = episode % graphs.len();
            let graph = graphs[graph_idx].clone();
            
            // Run episode
            let (episode_reward, episode_length) = self.run_episode(graph, true)?;
            
            self.episode_rewards.push(episode_reward);
            self.episode_lengths.push(episode_length);
            total_steps += episode_length;
            
            // Update target network
            if episode % self.config.target_update_frequency == 0 {
                self.agent.update_target_network();
            }
            
            // Save model periodically
            if episode % self.config.save_frequency == 0 && episode > 0 {
                self.save_checkpoint(episode)?;
            }
            
            // Print progress
            if episode % 10 == 0 {
                let recent_rewards: Vec<f32> = self.episode_rewards
                    .iter()
                    .rev()
                    .take(10)
                    .cloned()
                    .collect();
                let avg_reward = recent_rewards.iter().sum::<f32>() / recent_rewards.len() as f32;
                
                println!(
                    "Episode {}/{}: Avg Reward: {:.3}, Epsilon: {:.3}, Replay Size: {}",
                    episode,
                    self.config.num_episodes,
                    avg_reward,
                    self.agent.config.epsilon,
                    self.experience_replay.len()
                );
            }
            
            // Decay exploration
            self.agent.decay_epsilon();
        }
        
        // Final save
        self.save_checkpoint(self.config.num_episodes)?;
        
        Ok(TrainingStats {
            total_episodes: self.config.num_episodes,
            total_steps,
            final_epsilon: self.agent.config.epsilon,
            average_reward: self.episode_rewards.iter().sum::<f32>() / self.episode_rewards.len() as f32,
            performance_stats: self.agent.get_performance_stats(),
        })
    }
    
    /// Run single episode
    fn run_episode(&mut self, graph: Graph, training: bool) -> Result<(f32, usize)> {
        let mut state = self.environment.reset(graph)?;
        let mut total_reward = 0.0;
        let mut steps = 0;
        
        for _ in 0..self.config.max_steps_per_episode {
            // Get state representation for agent
            let opt_state = self.environment.get_state(&state);
            
            // Select action
            let action = self.agent.select_action(&opt_state, training);
            
            // Take action in environment
            let step_result = self.environment.step(&mut state, action)?;
            
            // Calculate detailed reward
            let reward_details = self.reward_calculator.calculate(
                step_result.metrics,
                self.environment.baseline_metrics.unwrap(),
                step_result.resource_metrics.compilation_time_us,
            );
            
            total_reward += reward_details.total_reward;
            steps += 1;
            
            if training {
                // Get next state
                let next_opt_state = self.environment.get_state(&state);
                
                // Store experience
                let experience = Experience {
                    state: opt_state.clone(),
                    action,
                    reward: reward_details.total_reward,
                    next_state: next_opt_state,
                    done: step_result.done,
                };
                self.experience_replay.push(experience);
                
                // Record action for statistics
                self.agent.record_action(opt_state, action, reward_details.total_reward);
                
                // Train if enough experiences
                if self.experience_replay.len() >= self.config.min_replay_size
                    && steps % self.config.train_frequency == 0
                {
                    self.train_step()?;
                }
            }
            
            if step_result.done {
                break;
            }
        }
        
        Ok((total_reward, steps))
    }
    
    /// Perform one training step
    fn train_step(&mut self) -> Result<()> {
        let batch = self.experience_replay.sample(self.config.batch_size);
        if batch.is_empty() {
            return Ok(());
        }
        
        // Convert batch to tensors
        let states = self.batch_states_to_tensor(&batch.iter().map(|e| &e.state).collect::<Vec<_>>());
        let actions = self.batch_actions_to_tensor(&batch.iter().map(|e| e.action).collect::<Vec<_>>());
        let rewards_vec: Vec<f32> = batch.iter().map(|e| e.reward).collect();
        let rewards = Tensor::<B, 1>::from_floats(
            rewards_vec.as_slice(),
            &self.device
        );
        let next_states = self.batch_states_to_tensor(&batch.iter().map(|e| &e.next_state).collect::<Vec<_>>());
        let dones_vec: Vec<f32> = batch.iter().map(|e| if e.done { 1.0 } else { 0.0 }).collect();
        let dones = Tensor::<B, 1>::from_floats(
            dones_vec.as_slice(),
            &self.device
        );
        
        // Compute target Q-values using Bellman equation
        let next_q_values = self.agent.target_network.forward(next_states);
        
        // Get max Q-value for each sample in the batch
        // next_q_values is [batch_size, action_dim], we want max over actions
        let max_next_q_indices = next_q_values.clone().argmax(1);
        
        // Create a one-hot mask to select the max Q-values
        let batch_size = self.config.batch_size;
        let action_dim = self.agent.config.action_dim;
        let mut max_mask = vec![0.0; batch_size * action_dim];
        
        // Extract indices and create mask
        let indices_data = max_next_q_indices.to_data();
        let indices = indices_data.as_slice::<i64>().unwrap();
        
        for (batch_idx, &max_action_idx) in indices.iter().enumerate() {
            let idx = batch_idx * action_dim + (max_action_idx as usize);
            if idx < max_mask.len() {
                max_mask[idx] = 1.0;
            }
        }
        
        let max_mask_tensor = Tensor::<B, 2>::from_floats(max_mask.as_slice(), &self.device)
            .reshape([batch_size, action_dim]);
        
        // Select max Q-values using the mask
        // sum_dim(1) reduces along the action dimension, leaving [batch_size]
        let max_next_q = (next_q_values * max_mask_tensor).sum_dim(1).squeeze(1);
        
        // Calculate (1 - done) mask
        let not_done = dones.clone().neg().add_scalar(1.0);
        
        // Bellman equation: Q(s,a) = r + γ * max_a' Q(s',a') * (1 - done)
        let gamma = self.agent.config.gamma;
        // Element-wise multiplication: max_next_q and not_done are both 1D tensors
        let discounted_future_rewards = max_next_q.mul(not_done).mul_scalar(gamma);
        let target_q = rewards.clone() + discounted_future_rewards;
        
        // Compute current Q-values
        let current_q_values = self.agent.q_network.forward(states.clone());
        let action_mask = self.create_action_mask(actions, self.config.batch_size);
        let current_q = (current_q_values * action_mask).sum_dim(1);
        
        // Compute loss - reshape tensors to match dimensions
        let current_q_2d: Tensor<B, 2> = current_q.unsqueeze_dim(1);
        let target_q_2d: Tensor<B, 2> = target_q.detach().unsqueeze_dim(1);
        let loss = nn::loss::MseLoss::new().forward(current_q_2d, target_q_2d, Reduction::Mean);
        
        // Gradient computation and optimization step
        // Note: This requires the autodiff backend to be enabled
        // When using burn-autodiff backend, you would:
        // 1. Create optimizer: let mut optimizer = self.optimizer_config.init();
        // 2. Compute gradients: let grads = loss.backward();
        // 3. Update weights: optimizer.step(lr, self.agent.q_network, grads);
        
        // For backends without autodiff support, we can implement a manual gradient approximation
        // or use finite differences, but for now we'll log the loss for monitoring
        println!("Training loss: {:.6}", loss.into_scalar());
        
        Ok(())
    }
    
    /// Convert batch of states to tensor
    fn batch_states_to_tensor(&self, states: &[&OptimizationState]) -> Tensor<B, 2> {
        let mut batch_features = Vec::new();
        
        for state in states {
            batch_features.extend(self.state_to_features(state));
        }
        
        let batch_size = states.len();
        let feature_size = self.agent.config.state_dim;
        
        Tensor::<B, 2>::from_floats(
            batch_features.as_slice(),
            &self.device
        ).reshape([batch_size, feature_size])
    }
    
    /// Convert state to feature vector
    fn state_to_features(&self, state: &OptimizationState) -> Vec<f32> {
        // This should match the agent's state_to_tensor logic
        let mut features = state.ast_features.clone();
        
        // Add configuration features
        features.extend(&[
            state.current_config.constant_folding as u8 as f32,
            state.current_config.dead_code_elimination as u8 as f32,
            // ... other config features
        ]);
        
        // Pad to expected size
        while features.len() < self.agent.config.state_dim {
            features.push(0.0);
        }
        features.truncate(self.agent.config.state_dim);
        
        features
    }
    
    /// Convert batch of actions to tensor indices
    fn batch_actions_to_tensor(&self, actions: &[super::OptimizationAction]) -> Tensor<B, 1, Int> {
        let indices: Vec<i32> = actions.iter().map(|a| self.action_to_index(a) as i32).collect();
        Tensor::from_ints(indices.as_slice(), &self.device)
    }
    
    /// Convert action to index
    fn action_to_index(&self, action: &super::OptimizationAction) -> usize {
        // This should match the agent's index_to_action logic
        match action {
            super::OptimizationAction::NoOp => 0,
            super::OptimizationAction::ConstantFolding => 1,
            super::OptimizationAction::DeadCodeElimination => 2,
            super::OptimizationAction::CSE => 3,
            super::OptimizationAction::Inline(super::InlineLevel::Conservative) => 4,
            super::OptimizationAction::Inline(super::InlineLevel::Standard) => 5,
            super::OptimizationAction::Inline(super::InlineLevel::Aggressive) => 6,
            super::OptimizationAction::TailCall => 7,
            super::OptimizationAction::LoopOpt => 8,
            super::OptimizationAction::BetaReduction => 9,
            super::OptimizationAction::PartialEval => 10,
            super::OptimizationAction::StrengthReduction => 11,
            super::OptimizationAction::AlgebraicSimplification => 12,
            super::OptimizationAction::LoopInvariantMotion => 13,
            super::OptimizationAction::FunctionSpecialization => 14,
            super::OptimizationAction::Composite(_) => 15,
        }
    }
    
    /// Create one-hot mask for actions
    fn create_action_mask(&self, actions: Tensor<B, 1, Int>, batch_size: usize) -> Tensor<B, 2> {
        let mut mask = vec![0.0; batch_size * self.agent.config.action_dim];
        
        // Extract action indices from tensor
        let action_data = actions.to_data();
        let action_indices = action_data.as_slice::<i32>().unwrap();
        
        // Create one-hot encoding for each action
        for (batch_idx, &action_idx) in action_indices.iter().enumerate() {
            let offset = batch_idx * self.agent.config.action_dim;
            let mask_idx = offset + (action_idx as usize);
            if mask_idx < mask.len() {
                mask[mask_idx] = 1.0;
            }
        }
        
        Tensor::<B, 2>::from_floats(mask.as_slice(), &self.device)
            .reshape([batch_size, self.agent.config.action_dim])
    }
    
    /// Save checkpoint
    fn save_checkpoint(&self, episode: usize) -> Result<()> {
        println!("Saving checkpoint at episode {}", episode);
        
        // Save model weights
        let model_path = Path::new(&self.config.model_path);
        if let Some(parent) = model_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        
        // Save the trained Q-network
        self.agent.save_model(&self.config.model_path)?;
        
        // Save experiences
        self.experience_replay.save(&self.config.experience_path)?;
        
        // Save training stats
        let stats = TrainingStats {
            total_episodes: episode,
            total_steps: self.episode_lengths.iter().sum(),
            final_epsilon: self.agent.config.epsilon,
            average_reward: self.episode_rewards.iter().sum::<f32>() / self.episode_rewards.len().max(1) as f32,
            performance_stats: self.agent.get_performance_stats(),
        };
        
        let stats_path = model_path.with_extension("json");
        let stats_json = serde_json::to_string_pretty(&stats)?;
        std::fs::write(stats_path, stats_json)?;
        
        Ok(())
    }
}

/// Training statistics
#[derive(Debug, Serialize, Deserialize)]
pub struct TrainingStats {
    pub total_episodes: usize,
    pub total_steps: usize,
    pub final_epsilon: f32,
    pub average_reward: f32,
    pub performance_stats: std::collections::HashMap<String, (f32, f32)>,
}