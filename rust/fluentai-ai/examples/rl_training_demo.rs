//! RL Training Demo with Complete Working Example
//! 
//! This demonstrates the RL optimization learning system in action.

#![allow(unused_imports)]

use fluentai_ai::rl::{
    OptimizationAction, OptimizationState, OptimizationConfig,
    InlineLevel, ResourceMetrics,
    OptimizationEnvironment, EnvironmentState, PerformanceMetrics,
    ExperienceReplay, Experience,
    RewardCalculator, RewardDetails, PerformanceTargets,
    TrainingConfig, Trainer,
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal, BinaryOp};
use fluentai_parser::parse_flc;
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Simple program to optimize
fn create_test_program() -> &'static str {
    r#"
    private function calculate() {
        // Constant folding opportunities
        let a = 2 + 3;
        let b = 4 * 5;
        
        // Common subexpression elimination
        let x = a + b;
        let y = a + b;
        
        // Dead code elimination
        if (false) {
            let unused = 100;
            $(f"Never executed: {unused}").print();
        }
        
        // Algebraic simplification
        let result = x * 1 + 0;
        
        // Tail recursion opportunity
        let rec factorial = (n, acc) => {
            if (n <= 1) { acc }
            else { factorial(n - 1, n * acc) }
        };
        
        // Inline candidate
        let double = (x) => x * 2;
        let quad = (x) => double(double(x));
        
        $(f"Result: {result}, Factorial: {factorial(5, 1)}, Quad: {quad(3)}").print();
    }
    
    calculate()
    "#
}

/// Measure simulated performance based on AST complexity
fn measure_performance(graph: &Graph) -> PerformanceMetrics {
    let mut metrics = PerformanceMetrics {
        execution_time_us: 0,
        memory_bytes: 0,
        binary_size: 0,
        instruction_count: 0,
        cache_misses: 0,
        branch_mispredictions: 0,
    };
    
    // Count nodes and estimate performance
    let mut node_count = 0;
    let mut constant_ops = 0;
    let mut function_calls = 0;
    let mut branches = 0;
    
    for (_, node) in graph.nodes() {
        node_count += 1;
        match node {
            Node::Application { function, args } => {
                // Check if it's an arithmetic operation with constant arguments
                if args.len() == 2 {
                    if let (Some(Node::Literal(_)), Some(Node::Literal(_))) = 
                        (graph.get_node(args[0]), graph.get_node(args[1])) {
                        constant_ops += 1;
                    }
                }
                function_calls += 1;
            }
            Node::If { .. } => branches += 1,
            _ => {}
        }
    }
    
    // Simulate metrics based on counts
    metrics.execution_time_us = (node_count * 10) as u64;
    metrics.memory_bytes = (node_count * 64) as u64;
    metrics.binary_size = (node_count * 32) as u64;
    metrics.instruction_count = node_count as u64;
    metrics.cache_misses = branches as u64;
    metrics.branch_mispredictions = (branches / 4) as u64;
    
    metrics
}

/// Simple RL Agent for demonstration
struct SimpleRLAgent {
    epsilon: f32,
    epsilon_decay: f32,
    epsilon_min: f32,
    q_table: HashMap<String, HashMap<OptimizationAction, f32>>,
    learning_rate: f32,
    gamma: f32,
}

impl SimpleRLAgent {
    fn new() -> Self {
        Self {
            epsilon: 1.0,
            epsilon_decay: 0.995,
            epsilon_min: 0.01,
            q_table: HashMap::new(),
            learning_rate: 0.1,
            gamma: 0.95,
        }
    }
    
    fn state_key(state: &OptimizationState) -> String {
        // Simple state representation based on applied optimizations
        let mut applied: Vec<_> = state.current_config.to_dict()
            .into_iter()
            .filter(|(_, v)| v.as_bool().unwrap_or(false))
            .map(|(k, _)| k)
            .collect();
        applied.sort();
        applied.join(",")
    }
    
    fn get_q_value(&self, state: &str, action: &OptimizationAction) -> f32 {
        self.q_table
            .get(state)
            .and_then(|actions| actions.get(action))
            .copied()
            .unwrap_or(0.0)
    }
    
    fn update_q_value(&mut self, state: &str, action: OptimizationAction, value: f32) {
        self.q_table
            .entry(state.to_string())
            .or_insert_with(HashMap::new)
            .insert(action, value);
    }
    
    fn select_action(&self, state: &OptimizationState) -> OptimizationAction {
        let state_key = Self::state_key(state);
        
        // Epsilon-greedy policy
        if rand::random::<f32>() < self.epsilon {
            // Explore: random action
            self.random_action()
        } else {
            // Exploit: best known action
            self.best_action(&state_key)
        }
    }
    
    fn random_action(&self) -> OptimizationAction {
        let actions = vec![
            OptimizationAction::NoOp,
            OptimizationAction::ConstantFolding,
            OptimizationAction::DeadCodeElimination,
            OptimizationAction::CSE,
            OptimizationAction::Inline(InlineLevel::Standard),
            OptimizationAction::BetaReduction,
            OptimizationAction::TailCall,
            OptimizationAction::AlgebraicSimplification,
        ];
        actions[rand::random::<usize>() % actions.len()]
    }
    
    fn best_action(&self, state_key: &str) -> OptimizationAction {
        let actions = vec![
            OptimizationAction::NoOp,
            OptimizationAction::ConstantFolding,
            OptimizationAction::DeadCodeElimination,
            OptimizationAction::CSE,
            OptimizationAction::Inline(InlineLevel::Standard),
            OptimizationAction::BetaReduction,
            OptimizationAction::TailCall,
            OptimizationAction::AlgebraicSimplification,
        ];
        
        actions.into_iter()
            .max_by(|a, b| {
                let q_a = self.get_q_value(state_key, a);
                let q_b = self.get_q_value(state_key, b);
                q_a.partial_cmp(&q_b).unwrap()
            })
            .unwrap_or(OptimizationAction::NoOp)
    }
    
    fn learn(&mut self, state: &str, action: OptimizationAction, reward: f32, next_state: &str) {
        let current_q = self.get_q_value(state, &action);
        
        // Find max Q-value for next state
        let next_actions = vec![
            OptimizationAction::NoOp,
            OptimizationAction::ConstantFolding,
            OptimizationAction::DeadCodeElimination,
            OptimizationAction::CSE,
            OptimizationAction::Inline(InlineLevel::Standard),
            OptimizationAction::BetaReduction,
            OptimizationAction::TailCall,
            OptimizationAction::AlgebraicSimplification,
        ];
        
        let max_next_q = next_actions.iter()
            .map(|a| self.get_q_value(next_state, a))
            .fold(f32::NEG_INFINITY, f32::max);
        
        // Q-learning update
        let new_q = current_q + self.learning_rate * (reward + self.gamma * max_next_q - current_q);
        self.update_q_value(state, action, new_q);
    }
    
    fn decay_epsilon(&mut self) {
        self.epsilon = (self.epsilon * self.epsilon_decay).max(self.epsilon_min);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== FluentAI RL Training Demo ===\n");
    
    // Parse the test program
    let code = create_test_program();
    let original_graph = parse_flc(code)?;
    let node_count = original_graph.nodes().count();
    println!("Parsed program with {} nodes", node_count);
    
    // Measure baseline performance
    let baseline_perf = measure_performance(&original_graph);
    println!("Baseline performance:");
    println!("  Execution time: {} µs", baseline_perf.execution_time_us);
    println!("  Memory usage: {} bytes", baseline_perf.memory_bytes);
    println!("  Binary size: {} bytes\n", baseline_perf.binary_size);
    
    // Initialize RL components
    let mut agent = SimpleRLAgent::new();
    let mut environment = OptimizationEnvironment::new(Default::default());
    let reward_calc = RewardCalculator::new(PerformanceTargets {
        exec_time_reduction: 20.0,  // 20% reduction target
        memory_reduction: 10.0,      // 10% reduction target
        size_reduction: 15.0,        // 15% reduction target
    });
    
    // Training parameters
    const NUM_EPISODES: usize = 100;
    const MAX_STEPS: usize = 10;
    
    println!("Starting training for {} episodes...\n", NUM_EPISODES);
    
    let mut episode_rewards = Vec::new();
    let mut best_sequence = Vec::new();
    let mut best_improvement = 0.0;
    
    // Training loop
    for episode in 0..NUM_EPISODES {
        // Reset environment
        environment.reset(original_graph.clone())?;
        let mut total_reward = 0.0;
        let mut action_sequence = Vec::new();
        
        // Initial state
        let mut state = OptimizationState {
            ast_features: vec![node_count as f32],
            current_config: OptimizationConfig::default(),
            performance_history: vec![baseline_perf.execution_time_us as f32],
            resource_history: vec![ResourceMetrics {
                memory_bytes: baseline_perf.memory_bytes,
                compilation_time_us: 0,
                binary_size: baseline_perf.binary_size,
            }],
        };
        
        // Episode steps
        for step in 0..MAX_STEPS {
            let state_key = SimpleRLAgent::state_key(&state);
            
            // Select action
            let action = agent.select_action(&state);
            action_sequence.push(action);
            
            // Apply action in environment
            let env_state = environment.step(action)?;
            
            // Calculate reward
            let current_perf = measure_performance(environment.current_graph());
            let reward_details = reward_calc.calculate_reward(
                &baseline_perf,
                &current_perf,
                action,
            );
            let reward = reward_details.total_reward;
            total_reward += reward;
            
            // Update state
            state.current_config.apply_action(action);
            state.ast_features = vec![environment.current_graph().nodes().count() as f32];
            state.performance_history.push(current_perf.execution_time_us as f32);
            state.resource_history.push(ResourceMetrics {
                memory_bytes: current_perf.memory_bytes,
                compilation_time_us: 0,
                binary_size: current_perf.binary_size,
            });
            
            let next_state_key = SimpleRLAgent::state_key(&state);
            
            // Learn from experience
            agent.learn(&state_key, action, reward, &next_state_key);
            
            // Check if done
            if env_state.is_done || step == MAX_STEPS - 1 {
                break;
            }
        }
        
        episode_rewards.push(total_reward);
        
        // Track best sequence
        let final_perf = measure_performance(environment.current_graph());
        let improvement = (baseline_perf.execution_time_us as f32 - 
                          final_perf.execution_time_us as f32) / 
                          baseline_perf.execution_time_us as f32;
        
        if improvement > best_improvement {
            best_improvement = improvement;
            best_sequence = action_sequence;
        }
        
        // Decay epsilon
        agent.decay_epsilon();
        
        // Progress report
        if episode % 20 == 0 {
            let avg_reward = episode_rewards.iter().rev().take(20).sum::<f32>() / 20.0;
            println!("Episode {}: Avg Reward = {:.3}, Epsilon = {:.3}", 
                     episode, avg_reward, agent.epsilon);
        }
    }
    
    println!("\n=== Training Complete ===\n");
    
    // Show what the agent learned
    println!("Best optimization sequence found:");
    for (i, action) in best_sequence.iter().enumerate() {
        println!("  Step {}: {:?}", i + 1, action);
    }
    println!("  Total improvement: {:.1}%\n", best_improvement * 100.0);
    
    // Apply the best sequence
    println!("Applying best sequence to original program:");
    environment.reset(original_graph.clone());
    
    for action in &best_sequence {
        let before_nodes = environment.current_graph().nodes().count();
        environment.step(*action)?;
        let after_nodes = environment.current_graph().nodes().count();
        println!("  {:?}: {} -> {} nodes", action, before_nodes, after_nodes);
    }
    
    let final_perf = measure_performance(environment.current_graph());
    println!("\nFinal performance:");
    println!("  Execution time: {} µs (was {} µs)", 
             final_perf.execution_time_us, baseline_perf.execution_time_us);
    println!("  Memory usage: {} bytes (was {} bytes)", 
             final_perf.memory_bytes, baseline_perf.memory_bytes);
    println!("  Binary size: {} bytes (was {} bytes)", 
             final_perf.binary_size, baseline_perf.binary_size);
    
    // Show Q-table insights
    println!("\n=== Learning Insights ===");
    println!("Q-table entries: {}", agent.q_table.len());
    
    // Find most valuable actions
    let mut action_values: HashMap<OptimizationAction, Vec<f32>> = HashMap::new();
    for (_, actions) in &agent.q_table {
        for (action, value) in actions {
            action_values.entry(*action).or_insert_with(Vec::new).push(*value);
        }
    }
    
    println!("\nAverage Q-values by action:");
    for (action, values) in &action_values {
        let avg = values.iter().sum::<f32>() / values.len() as f32;
        println!("  {:?}: {:.3}", action, avg);
    }
    
    println!("\nDemo complete! The RL agent successfully learned to optimize FluentAI programs.");
    
    Ok(())
}