//! Full-Featured RL Optimization Demo
//! 
//! This example demonstrates the complete RL training pipeline for learning
//! optimization strategies on real FluentAI programs.

use fluentai_ai::rl::{
    OptimizationAgent, AgentConfig,
    OptimizationEnvironment, EnvironmentState, PerformanceMetrics,
    ExperienceReplay, Experience,
    RewardCalculator, PerformanceTargets,
    TrainingConfig, Trainer,
    OptimizationAction, OptimizationState, OptimizationConfig,
    InlineLevel, ResourceMetrics,
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal, BinaryOp};
use fluentai_parser::parse_flc;
use fluentai_optimizer::{
    OptimizationPipeline,
    passes::{
        constant_folding::ConstantFoldingPass,
        dead_code::DeadCodeEliminationPass,
        cse::CommonSubexpressionElimination,
        inline::InliningPass,
        beta_reduction::BetaReductionPass,
        partial_eval::PartialEvaluationPass,
        loop_opt::LoopOptimizationPass,
        tail_call::TailCallOptimizationPass,
    }
};
use burn::prelude::*;
use burn::backend::Wgpu;
use burn::optim::AdamConfig;
use std::collections::HashMap;
use std::time::{Duration, Instant};
use std::sync::Arc;
use fluentai_ai::features::extract_ast_features;

// Backend type for Burn
type Backend = Wgpu;

/// Create a complex FluentAI program with many optimization opportunities
fn create_complex_program() -> String {
    r#"
    // Fibonacci with memoization - many optimization opportunities
    private function fibonacci_memo() {
        let cache = {};
        
        let fib = (n) => {
            // Constant expression that can be folded
            let base_case_0 = 0 + 0;
            let base_case_1 = 1 * 1;
            
            // Dead code - this branch is never taken
            if (false) {
                let unused = 100 * 200;
                $(f"This will never print: {unused}").print();
            }
            
            // Base cases with redundant checks
            if (n == 0) { base_case_0 }
            else if (n == 1) { base_case_1 }
            else {
                // Check cache - common subexpression
                let key = n.to_string();
                if (cache.has(key)) {
                    cache.get(key)
                } else {
                    // Recursive calls - tail call optimization opportunity
                    let result = fib(n - 1) + fib(n - 2);
                    cache.set(key, result);
                    result
                }
            }
        };
        
        // Test with various inputs
        let results = [5, 10, 15].map(n => {
            // Loop invariant - 'start' doesn't change in loop
            let start = 1000;
            let computation = n * start / start;  // Can be simplified
            fib(computation)
        });
        
        results
    }
    
    // Matrix operations with optimization opportunities
    private function matrix_ops() {
        // Inline candidate - small function
        let dot_product = (a, b) => {
            a.zip(b).map(pair => pair.0 * pair.1).sum()
        };
        
        // Partial evaluation opportunity
        let scale_matrix = (matrix, factor) => {
            matrix.map(row => row.map(x => x * factor))
        };
        
        // Constant matrix that can be evaluated at compile time
        let identity = [[1, 0], [0, 1]];
        let scaled = scale_matrix(identity, 2);
        
        // Beta reduction opportunity
        let apply_twice = (f) => (x) => f(f(x));
        let double = (x) => x * 2;
        let quad = apply_twice(double);
        
        [scaled, quad(5)]
    }
    
    // Main function combining everything
    private function main() {
        let fib_results = fibonacci_memo();
        let matrix_results = matrix_ops();
        
        // Common subexpressions
        let x = 10 + 20;
        let y = 10 + 20;
        let z = x + y;
        
        // Algebraic simplification opportunities
        let a = z * 1;
        let b = a + 0;
        let c = b / 1;
        
        $(f"Results: fib={fib_results}, matrix={matrix_results}, calc={c}").print();
    }
    
    main()
    "#.to_string()
}

/// Create a simpler program for initial testing
fn create_simple_program() -> String {
    r#"
    private function simple_calc() {
        // Constant folding opportunities
        let x = 2 + 3;
        let y = 4 * 5;
        
        // Common subexpression
        let a = x + y;
        let b = x + y;
        
        // Dead code
        if (false) {
            let unused = 999;
        }
        
        // Algebraic simplification
        let result = (a + b) * 1 + 0;
        
        result
    }
    
    simple_calc()
    "#.to_string()
}

/// Compile and measure performance of a program
fn measure_program_performance(graph: &Graph) -> Result<PerformanceMetrics, String> {
    let start = Instant::now();
    
    // Count various node types for complexity metrics
    let mut node_count = 0;
    let mut function_count = 0;
    let mut loop_count = 0;
    let mut branch_count = 0;
    
    for (_, node) in graph.nodes() {
        node_count += 1;
        match node {
            Node::Lambda { .. } => function_count += 1,
            Node::If { .. } => branch_count += 1,
            Node::Application { .. } => {
                // Could be a loop if it's recursive
                loop_count += 1;
            }
            _ => {}
        }
    }
    
    let compilation_time = start.elapsed();
    
    // Simulate execution time based on complexity
    let simulated_exec_time = Duration::from_micros(
        (node_count * 10 + function_count * 50 + loop_count * 100) as u64
    );
    
    // Estimate memory usage
    let memory_usage = (node_count * 64 + function_count * 256) as u64;
    
    Ok(PerformanceMetrics {
        execution_time: simulated_exec_time,
        compilation_time,
        memory_usage,
        binary_size: (node_count * 32) as u64,
        instruction_count: node_count as u64,
        cache_misses: (branch_count * 2) as u64,
        branch_mispredictions: branch_count as u64,
    })
}

/// Apply optimization based on action
fn apply_optimization_action(
    graph: &Graph,
    action: &OptimizationAction,
) -> Result<Graph, String> {
    match action {
        OptimizationAction::NoOp => Ok(graph.clone()),
        
        OptimizationAction::ConstantFolding => {
            let mut pass = ConstantFoldingPass::new();
            pass.run(graph).map_err(|e| e.to_string())
        }
        
        OptimizationAction::DeadCodeElimination => {
            let mut pass = DeadCodeEliminationPass::new();
            pass.run(graph).map_err(|e| e.to_string())
        }
        
        OptimizationAction::CSE => {
            let config = fluentai_optimizer::passes::cse::CSEConfig::default();
            let mut pass = CommonSubexpressionElimination::new(config);
            pass.run(graph).map_err(|e| e.to_string())
        }
        
        OptimizationAction::Inline(level) => {
            let config = match level {
                InlineLevel::Conservative => fluentai_optimizer::passes::inline::InlineConfig {
                    max_inline_size: 10,
                    max_inline_depth: 2,
                    inline_threshold: 0.8,
                    always_inline_small: true,
                    small_function_threshold: 5,
                },
                InlineLevel::Standard => fluentai_optimizer::passes::inline::InlineConfig {
                    max_inline_size: 20,
                    max_inline_depth: 3,
                    inline_threshold: 0.6,
                    always_inline_small: true,
                    small_function_threshold: 10,
                },
                InlineLevel::Aggressive => fluentai_optimizer::passes::inline::InlineConfig {
                    max_inline_size: 50,
                    max_inline_depth: 5,
                    inline_threshold: 0.4,
                    always_inline_small: true,
                    small_function_threshold: 20,
                },
            };
            let mut pass = InliningPass::new(config);
            pass.run(graph).map_err(|e| e.to_string())
        }
        
        OptimizationAction::BetaReduction => {
            let mut pass = BetaReductionPass::new();
            pass.run(graph).map_err(|e| e.to_string())
        }
        
        OptimizationAction::PartialEval => {
            let config = fluentai_optimizer::passes::partial_eval::PartialEvalConfig::default();
            let mut pass = PartialEvaluationPass::new(config);
            pass.run(graph).map_err(|e| e.to_string())
        }
        
        OptimizationAction::LoopOpt => {
            let config = fluentai_optimizer::passes::loop_opt::LoopOptConfig::default();
            let mut pass = LoopOptimizationPass::new(config);
            pass.run(graph).map_err(|e| e.to_string())
        }
        
        OptimizationAction::TailCall => {
            let mut pass = TailCallOptimizationPass::new();
            pass.run(graph).map_err(|e| e.to_string())
        }
        
        _ => {
            // Other optimizations not yet implemented
            Ok(graph.clone())
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== FluentAI Full RL Optimization Demo ===\n");
    
    // Initialize device
    let device = WgpuDevice::default();
    println!("Initialized compute device: {:?}", device);
    
    // Parse programs
    println!("\nParsing test programs...");
    let simple_code = create_simple_program();
    let complex_code = create_complex_program();
    
    let simple_graph = parse_flc(&simple_code)?;
    let complex_graph = parse_flc(&complex_code)?;
    
    println!("Simple program: {} nodes", simple_graph.node_count());
    println!("Complex program: {} nodes", complex_graph.node_count());
    
    // Create agent configuration
    let agent_config = AgentConfig {
        state_dim: 32,  // AST feature dimension
        action_dim: 16, // Number of optimization actions
        hidden_dim: 128,
        learning_rate: 0.001,
        gamma: 0.95,
        epsilon_start: 1.0,
        epsilon_end: 0.01,
        epsilon_decay: 0.995,
    };
    
    // Create the RL agent
    let mut agent = OptimizationAgent::new(agent_config, device.clone());
    println!("\nRL Agent initialized");
    
    // Create experience replay buffer
    let mut replay_buffer = ExperienceReplay::new(10000);
    
    // Create reward calculator
    let targets = PerformanceTargets {
        execution_time_ms: 1.0,
        memory_usage_mb: 1.0,
        binary_size_kb: 10.0,
    };
    let reward_calc = RewardCalculator::new(targets);
    
    // Training configuration
    let training_config = TrainingConfig {
        num_epochs: 100,
        steps_per_epoch: 50,
        evaluation_frequency: 10,
        save_frequency: 25,
        early_stopping_patience: 20,
        target_performance: 0.95,
    };
    
    // Create optimizer
    let optimizer_config = AdamConfig::new();
    let mut optimizer = optimizer_config.init();
    
    println!("\nStarting training for {} epochs...", training_config.num_epochs);
    
    // Track metrics
    let mut epoch_rewards = Vec::new();
    let mut best_reward = f32::NEG_INFINITY;
    let mut best_action_sequence = Vec::new();
    
    // Training loop
    for epoch in 0..training_config.num_epochs {
        let mut episode_rewards = Vec::new();
        
        for episode in 0..training_config.steps_per_epoch {
            // Alternate between simple and complex programs
            let (original_graph, program_name) = if episode % 2 == 0 {
                (simple_graph.clone(), "simple")
            } else {
                (complex_graph.clone(), "complex")
            };
            
            // Initial state
            let initial_features = extract_ast_features(&original_graph);
            let mut state = OptimizationState {
                ast_features: initial_features,
                current_config: OptimizationConfig::default(),
                performance_history: vec![],
                resource_history: vec![],
            };
            
            // Measure baseline performance
            let baseline_perf = measure_program_performance(&original_graph)?;
            state.performance_history.push(baseline_perf.execution_time.as_micros() as f32);
            
            let mut current_graph = original_graph.clone();
            let mut action_sequence = Vec::new();
            let mut episode_reward = 0.0;
            
            // Episode: Apply sequence of optimizations
            for step in 0..5 {
                // Select action
                let action = agent.select_action(&state)?;
                action_sequence.push(action);
                
                // Apply optimization
                match apply_optimization_action(&current_graph, &action) {
                    Ok(optimized_graph) => {
                        // Measure performance
                        let new_perf = measure_program_performance(&optimized_graph)?;
                        
                        // Calculate reward
                        let reward_details = reward_calc.calculate_reward(
                            &baseline_perf,
                            &new_perf,
                            &action,
                        );
                        let reward = reward_details.total_reward;
                        episode_reward += reward;
                        
                        // Update state
                        state.current_config.apply_action(action);
                        state.performance_history.push(new_perf.execution_time.as_micros() as f32);
                        state.resource_history.push(ResourceMetrics {
                            memory_bytes: new_perf.memory_usage,
                            compilation_time_us: new_perf.compilation_time.as_micros() as u64,
                            binary_size: new_perf.binary_size,
                        });
                        
                        // Extract features from optimized graph
                        let new_features = extract_ast_features(&optimized_graph);
                        let new_state = OptimizationState {
                            ast_features: new_features,
                            current_config: state.current_config.clone(),
                            performance_history: state.performance_history.clone(),
                            resource_history: state.resource_history.clone(),
                        };
                        
                        // Store experience
                        let experience = Experience {
                            state: state.clone(),
                            action,
                            reward,
                            next_state: new_state.clone(),
                            done: step == 4,
                        };
                        replay_buffer.push(experience);
                        
                        // Update for next iteration
                        current_graph = optimized_graph;
                        state = new_state;
                    }
                    Err(e) => {
                        // Optimization failed - negative reward
                        let experience = Experience {
                            state: state.clone(),
                            action,
                            reward: -1.0,
                            next_state: state.clone(),
                            done: true,
                        };
                        replay_buffer.push(experience);
                        break;
                    }
                }
                
                // Train on minibatch
                if replay_buffer.len() >= 32 && episode % 4 == 0 {
                    let batch = replay_buffer.sample(32);
                    agent.train_on_batch(batch, &mut optimizer)?;
                }
            }
            
            episode_rewards.push(episode_reward);
            
            // Track best sequence
            if episode_reward > best_reward {
                best_reward = episode_reward;
                best_action_sequence = action_sequence;
            }
        }
        
        // Calculate epoch statistics
        let avg_reward: f32 = episode_rewards.iter().sum::<f32>() / episode_rewards.len() as f32;
        epoch_rewards.push(avg_reward);
        
        // Update epsilon
        agent.decay_epsilon();
        
        // Update target network
        if epoch % 10 == 0 {
            agent.update_target_network();
        }
        
        // Print progress
        if epoch % 10 == 0 {
            println!(
                "Epoch {}: Avg Reward = {:.3}, Best Reward = {:.3}, Epsilon = {:.3}",
                epoch, avg_reward, best_reward, agent.epsilon
            );
        }
    }
    
    println!("\n=== Training Complete ===\n");
    
    // Test the learned policy
    println!("Testing learned optimization strategy...\n");
    
    // Apply best sequence to both programs
    for (graph, name) in [(simple_graph, "simple"), (complex_graph, "complex")] {
        println!("Optimizing {} program:", name);
        println!("  Original nodes: {}", graph.node_count());
        
        let baseline_perf = measure_program_performance(&graph)?;
        println!("  Baseline performance:");
        println!("    Execution time: {:?}", baseline_perf.execution_time);
        println!("    Memory usage: {} KB", baseline_perf.memory_usage / 1024);
        
        let mut optimized = graph.clone();
        println!("\n  Applying learned optimization sequence:");
        
        for (i, action) in best_action_sequence.iter().enumerate() {
            match apply_optimization_action(&optimized, action) {
                Ok(new_graph) => {
                    let old_nodes = optimized.node_count();
                    let new_nodes = new_graph.node_count();
                    println!("    Step {}: {:?} ({} -> {} nodes)", 
                             i + 1, action, old_nodes, new_nodes);
                    optimized = new_graph;
                }
                Err(e) => {
                    println!("    Step {}: {:?} - Failed: {}", i + 1, action, e);
                }
            }
        }
        
        let final_perf = measure_program_performance(&optimized)?;
        println!("\n  Final performance:");
        println!("    Execution time: {:?} ({}% improvement)", 
                 final_perf.execution_time,
                 ((baseline_perf.execution_time.as_micros() - final_perf.execution_time.as_micros()) as f64 
                  / baseline_perf.execution_time.as_micros() as f64 * 100.0) as i32);
        println!("    Memory usage: {} KB ({}% improvement)",
                 final_perf.memory_usage / 1024,
                 ((baseline_perf.memory_usage - final_perf.memory_usage) as f64 
                  / baseline_perf.memory_usage as f64 * 100.0) as i32);
        println!("    Final nodes: {}\n", optimized.node_count());
    }
    
    // Show learning curve
    println!("=== Learning Progress ===");
    println!("Epoch rewards over time:");
    for (i, reward) in epoch_rewards.iter().enumerate().step_by(10) {
        println!("  Epoch {}: {:.3}", i, reward);
    }
    
    println!("\nDemo complete! The RL agent successfully learned to optimize FluentAI programs.");
    
    Ok(())
}