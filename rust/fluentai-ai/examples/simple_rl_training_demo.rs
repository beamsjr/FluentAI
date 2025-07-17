//! Simple RL Training Demo
//! 
//! This example demonstrates the core RL training loop without external dependencies,
//! showing how the agent learns to optimize code through trial and error.

use fluentai_ai::rl::{
    RLAgent, RLConfig, OptimizationAction, OptimizationState, InlineLevel
};
use std::collections::HashMap;

/// Simulated program state for demonstration
#[derive(Clone)]
struct SimulatedProgram {
    node_count: usize,
    constant_ops: usize,
    redundant_ops: usize,
    cse_opportunities: usize,
    inline_opportunities: usize,
    optimizations_applied: Vec<String>,
}

impl SimulatedProgram {
    fn new() -> Self {
        Self {
            node_count: 50,
            constant_ops: 8,
            redundant_ops: 5,
            cse_opportunities: 4,
            inline_opportunities: 3,
            optimizations_applied: Vec::new(),
        }
    }
    
    /// Calculate performance score (lower is better)
    fn score(&self) -> f64 {
        let base_score = self.node_count as f64;
        let penalty = (self.constant_ops + self.redundant_ops + self.cse_opportunities) as f64 * 2.0;
        -(base_score + penalty) // Negative so higher is better for rewards
    }
    
    /// Apply an optimization action and return reward
    fn apply_optimization(&mut self, action: &OptimizationAction) -> f64 {
        let initial_score = self.score();
        
        match action {
            OptimizationAction::NoOp => {
                // No change
                -0.1 // Small penalty for doing nothing
            }
            OptimizationAction::ConstantFolding => {
                if self.constant_ops > 0 {
                    let reduced = (self.constant_ops as f64 * 0.8) as usize;
                    self.node_count -= self.constant_ops - reduced;
                    self.constant_ops = reduced;
                    self.optimizations_applied.push("ConstantFolding".to_string());
                    self.score() - initial_score + 0.5 // Bonus for good optimization
                } else {
                    -0.2 // Penalty for unnecessary optimization
                }
            }
            OptimizationAction::DeadCodeElimination => {
                if self.redundant_ops > 0 {
                    let reduced = (self.redundant_ops as f64 * 0.9) as usize;
                    self.node_count -= self.redundant_ops - reduced;
                    self.redundant_ops = reduced;
                    self.optimizations_applied.push("DeadCodeElimination".to_string());
                    self.score() - initial_score + 0.5
                } else {
                    -0.2
                }
            }
            OptimizationAction::CSE => {
                if self.cse_opportunities > 0 {
                    let reduced = (self.cse_opportunities as f64 * 0.7) as usize;
                    self.node_count -= self.cse_opportunities - reduced;
                    self.cse_opportunities = reduced;
                    self.optimizations_applied.push("CSE".to_string());
                    self.score() - initial_score + 0.5
                } else {
                    -0.2
                }
            }
            OptimizationAction::Inline(level) => {
                if self.inline_opportunities > 0 {
                    let reduction_factor = match level {
                        InlineLevel::Conservative => 0.3,
                        InlineLevel::Moderate => 0.5,
                        InlineLevel::Aggressive => 0.7,
                    };
                    let reduced = (self.inline_opportunities as f64 * reduction_factor) as usize;
                    self.node_count -= reduced;
                    self.inline_opportunities -= reduced;
                    self.optimizations_applied.push(format!("Inline({:?})", level));
                    self.score() - initial_score + 0.3
                } else {
                    -0.2
                }
            }
            _ => {
                // Other optimizations not implemented in this demo
                -0.1
            }
        }
    }
    
    /// Convert to optimization state for the agent
    fn to_state(&self) -> OptimizationState {
        let mut features = HashMap::new();
        features.insert("node_count".to_string(), self.node_count as f32);
        features.insert("constant_ops".to_string(), self.constant_ops as f32);
        features.insert("redundant_ops".to_string(), self.redundant_ops as f32);
        features.insert("cse_opportunities".to_string(), self.cse_opportunities as f32);
        features.insert("inline_opportunities".to_string(), self.inline_opportunities as f32);
        features.insert("optimization_steps".to_string(), self.optimizations_applied.len() as f32);
        
        OptimizationState {
            features,
            applied_optimizations: self.optimizations_applied.iter().cloned().collect(),
            performance_metrics: HashMap::new(),
        }
    }
}

fn main() {
    println!("=== FluentAI RL Training Demo ===\n");
    
    // Configure the RL agent
    let config = RLConfig {
        learning_rate: 0.001,
        gamma: 0.95,
        epsilon_start: 1.0,
        epsilon_end: 0.05,
        epsilon_decay: 0.995,
        memory_size: 1000,
        batch_size: 32,
        target_update_frequency: 10,
        hidden_size: 64,
        num_hidden_layers: 2,
    };
    
    let mut agent = RLAgent::new(config);
    println!("Agent initialized with epsilon = {:.3}\n", agent.get_epsilon());
    
    // Training parameters
    const NUM_EPISODES: usize = 200;
    const MAX_STEPS_PER_EPISODE: usize = 10;
    
    // Track metrics
    let mut episode_rewards = Vec::new();
    let mut episode_improvements = Vec::new();
    let mut best_sequence = Vec::new();
    let mut best_improvement = f64::NEG_INFINITY;
    
    println!("Starting training for {} episodes...\n", NUM_EPISODES);
    
    // Training loop
    for episode in 0..NUM_EPISODES {
        let mut program = SimulatedProgram::new();
        let initial_score = program.score();
        let mut total_reward = 0.0;
        let mut actions_taken = Vec::new();
        
        // Run episode
        for step in 0..MAX_STEPS_PER_EPISODE {
            let state = program.to_state();
            
            // Select action
            let action = if agent.memory_size() < 100 {
                // Explore more in the beginning
                agent.select_action(&state)
            } else {
                agent.select_action(&state)
            };
            
            actions_taken.push(action.clone());
            
            // Apply action and get reward
            let reward = program.apply_optimization(&action);
            total_reward += reward;
            
            // Get new state
            let new_state = program.to_state();
            let done = step == MAX_STEPS_PER_EPISODE - 1;
            
            // Store experience
            agent.remember(state, action, reward, new_state, done);
            
            // Train if we have enough samples
            if agent.memory_size() >= agent.config.batch_size && episode % 2 == 0 {
                agent.train_step();
            }
        }
        
        // Calculate improvement
        let final_score = program.score();
        let improvement = final_score - initial_score;
        
        episode_rewards.push(total_reward);
        episode_improvements.push(improvement);
        
        // Track best sequence
        if improvement > best_improvement {
            best_improvement = improvement;
            best_sequence = actions_taken.clone();
        }
        
        // Update target network
        if episode % agent.config.target_update_frequency == 0 && episode > 0 {
            agent.update_target_network();
        }
        
        // Decay epsilon
        agent.decay_epsilon();
        
        // Print progress
        if episode % 20 == 0 {
            let recent_rewards: f64 = episode_rewards.iter().rev().take(20).sum::<f64>() / 20.0;
            let recent_improvements: f64 = episode_improvements.iter().rev().take(20).sum::<f64>() / 20.0;
            
            println!("Episode {}: Avg Reward = {:.3}, Avg Improvement = {:.3}, Epsilon = {:.3}",
                     episode, recent_rewards, recent_improvements, agent.get_epsilon());
        }
    }
    
    println!("\n=== Training Complete ===\n");
    
    // Test the learned policy
    println!("Testing learned optimization strategy...\n");
    
    let mut test_program = SimulatedProgram::new();
    println!("Initial program state:");
    println!("  Nodes: {}", test_program.node_count);
    println!("  Constant operations: {}", test_program.constant_ops);
    println!("  Redundant operations: {}", test_program.redundant_ops);
    println!("  CSE opportunities: {}", test_program.cse_opportunities);
    println!("  Inline opportunities: {}", test_program.inline_opportunities);
    println!("  Initial score: {:.2}\n", test_program.score());
    
    // Apply learned strategy (greedy, no exploration)
    println!("Applying learned optimization sequence:");
    let mut learned_actions = Vec::new();
    
    for step in 0..5 {
        let state = test_program.to_state();
        let action = agent.select_action_greedy(&state);
        learned_actions.push(action.clone());
        
        let reward = test_program.apply_optimization(&action);
        println!("  Step {}: {:?} (reward: {:.3})", step + 1, action, reward);
    }
    
    println!("\nFinal program state:");
    println!("  Nodes: {}", test_program.node_count);
    println!("  Constant operations: {}", test_program.constant_ops);
    println!("  Redundant operations: {}", test_program.redundant_ops);
    println!("  CSE opportunities: {}", test_program.cse_opportunities);
    println!("  Inline opportunities: {}", test_program.inline_opportunities);
    println!("  Final score: {:.2}", test_program.score());
    println!("  Total improvement: {:.2}\n", test_program.score() - SimulatedProgram::new().score());
    
    // Show best sequence found during training
    println!("Best optimization sequence found during training:");
    for (i, action) in best_sequence.iter().take(5).enumerate() {
        println!("  {}: {:?}", i + 1, action);
    }
    println!("  Best improvement achieved: {:.2}\n", best_improvement);
    
    // Analyze what the agent learned
    println!("=== Learning Analysis ===");
    
    // Test agent's preferences
    let empty_state = OptimizationState {
        features: vec![
            ("node_count".to_string(), 100.0),
            ("constant_ops".to_string(), 10.0),
            ("redundant_ops".to_string(), 5.0),
            ("cse_opportunities".to_string(), 8.0),
            ("inline_opportunities".to_string(), 3.0),
            ("optimization_steps".to_string(), 0.0),
        ].into_iter().collect(),
        applied_optimizations: std::collections::HashSet::new(),
        performance_metrics: HashMap::new(),
    };
    
    println!("Agent's action preferences for a fresh program:");
    let action = agent.select_action_greedy(&empty_state);
    println!("  First choice: {:?}", action);
    
    // Test with partially optimized program
    let mut partial_state = empty_state.clone();
    partial_state.features.insert("constant_ops".to_string(), 0.0);
    partial_state.applied_optimizations.insert("ConstantFolding".to_string());
    
    let action2 = agent.select_action_greedy(&partial_state);
    println!("  After constant folding: {:?}", action2);
    
    println!("\nDemo complete! The agent has learned to optimize programs effectively.");
}