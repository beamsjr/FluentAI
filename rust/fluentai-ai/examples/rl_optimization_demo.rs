//! Reinforcement Learning Optimization Demo
//! 
//! This example demonstrates how the RL agent learns to optimize FluentAI programs
//! by exploring different optimization strategies and learning from performance feedback.

use fluentai_ai::rl::{
    RLAgent, RLConfig, OptimizationAction, OptimizationState, 
    RLEnvironment, EnvironmentConfig, TrainingLoop, TrainingConfig
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use fluentai_core::traits::OptimizationLevel;
use fluentai_optimizer::{
    OptimizationConfig, OptimizationPipeline,
    passes::{
        constant_folding::ConstantFoldingPass,
        dead_code::DeadCodeEliminationPass,
        cse::CommonSubexpressionElimination,
        inline::InliningPass,
    }
};
use std::time::{Duration, Instant};
use std::collections::HashMap;

/// Create a sample program that can benefit from optimization
fn create_sample_program() -> Graph {
    let mut graph = Graph::new();
    
    // Create a program with optimization opportunities:
    // let x = 2 + 3;              // Can be constant folded
    // let y = 2 + 3;              // Common subexpression
    // let z = x * 0;              // Can be optimized to 0
    // let w = if (true) { 10 } else { y };  // Dead code in else branch
    // let result = x + y + z + w; // Final computation
    
    // Constants
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
    let three = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();
    let zero = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
    let ten = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
    let true_val = graph.add_node(Node::Literal(Literal::Boolean(true))).unwrap();
    
    // x = 2 + 3
    let x_add = graph.add_node(Node::BinaryOp {
        op: fluentai_core::ast::BinaryOp::Add,
        left: two,
        right: three,
    }).unwrap();
    
    // y = 2 + 3 (duplicate computation)
    let two2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
    let three2 = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();
    let y_add = graph.add_node(Node::BinaryOp {
        op: fluentai_core::ast::BinaryOp::Add,
        left: two2,
        right: three2,
    }).unwrap();
    
    // z = x * 0
    let z_mul = graph.add_node(Node::BinaryOp {
        op: fluentai_core::ast::BinaryOp::Multiply,
        left: x_add,
        right: zero,
    }).unwrap();
    
    // w = if (true) { 10 } else { y }
    let w_if = graph.add_node(Node::If {
        condition: true_val,
        then_branch: ten,
        else_branch: y_add,
    }).unwrap();
    
    // result = x + y + z + w
    let xy_add = graph.add_node(Node::BinaryOp {
        op: fluentai_core::ast::BinaryOp::Add,
        left: x_add,
        right: y_add,
    }).unwrap();
    
    let xyz_add = graph.add_node(Node::BinaryOp {
        op: fluentai_core::ast::BinaryOp::Add,
        left: xy_add,
        right: z_mul,
    }).unwrap();
    
    let result = graph.add_node(Node::BinaryOp {
        op: fluentai_core::ast::BinaryOp::Add,
        left: xyz_add,
        right: w_if,
    }).unwrap();
    
    // Wrap in let bindings
    let bindings = vec![
        ("x".to_string(), x_add),
        ("y".to_string(), y_add),
        ("z".to_string(), z_mul),
        ("w".to_string(), w_if),
    ];
    
    let let_node = graph.add_node(Node::Let {
        bindings,
        body: result,
    }).unwrap();
    
    graph.root_id = Some(let_node);
    graph
}

/// Measure the performance of a graph (simulated)
fn measure_performance(graph: &Graph) -> f64 {
    // In a real implementation, this would compile and run the program
    // For demo purposes, we'll estimate based on node count and types
    
    let mut score = 0.0;
    let mut node_count = 0;
    let mut constant_ops = 0;
    let mut redundant_ops = 0;
    
    // Count nodes and identify optimization opportunities
    for (_, node) in graph.nodes() {
        node_count += 1;
        
        match node {
            Node::BinaryOp { op, left, right } => {
                // Check if it's a constant operation
                if let (Some(Node::Literal(_)), Some(Node::Literal(_))) = 
                    (graph.get_node(*left), graph.get_node(*right)) {
                    constant_ops += 1;
                }
                
                // Check for multiply by zero
                if matches!(op, fluentai_core::ast::BinaryOp::Multiply) {
                    if matches!(graph.get_node(*right), Some(Node::Literal(Literal::Integer(0)))) {
                        redundant_ops += 1;
                    }
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                // Check for constant condition
                if matches!(graph.get_node(*condition), Some(Node::Literal(Literal::Boolean(_)))) {
                    redundant_ops += 1; // One branch is dead code
                }
            }
            _ => {}
        }
    }
    
    // Lower score is better (fewer nodes, fewer redundant operations)
    score = node_count as f64 + (constant_ops as f64 * 2.0) + (redundant_ops as f64 * 3.0);
    
    // Return negative score so higher is better for the RL agent
    -score
}

/// Apply a specific optimization pass to a graph
fn apply_optimization(graph: &Graph, action: &OptimizationAction) -> Result<Graph, String> {
    match action {
        OptimizationAction::ConstantFolding => {
            let mut pass = ConstantFoldingPass::new();
            pass.run(graph).map_err(|e| e.to_string())
        }
        OptimizationAction::DeadCodeElimination => {
            let mut pass = DeadCodeEliminationPass::new();
            pass.run(graph).map_err(|e| e.to_string())
        }
        OptimizationAction::CSE => {
            let mut pass = CommonSubexpressionElimination::new(Default::default());
            pass.run(graph).map_err(|e| e.to_string())
        }
        OptimizationAction::Inline(level) => {
            let config = match level {
                fluentai_ai::rl::InlineLevel::Conservative => {
                    fluentai_optimizer::passes::inline::InlineConfig {
                        max_inline_size: 10,
                        max_inline_depth: 2,
                        inline_threshold: 0.8,
                        always_inline_small: true,
                        small_function_threshold: 5,
                    }
                }
                fluentai_ai::rl::InlineLevel::Moderate => {
                    fluentai_optimizer::passes::inline::InlineConfig {
                        max_inline_size: 20,
                        max_inline_depth: 3,
                        inline_threshold: 0.6,
                        always_inline_small: true,
                        small_function_threshold: 10,
                    }
                }
                fluentai_ai::rl::InlineLevel::Aggressive => {
                    fluentai_optimizer::passes::inline::InlineConfig {
                        max_inline_size: 50,
                        max_inline_depth: 5,
                        inline_threshold: 0.4,
                        always_inline_small: true,
                        small_function_threshold: 20,
                    }
                }
            };
            let mut pass = InliningPass::new(config);
            pass.run(graph).map_err(|e| e.to_string())
        }
        _ => {
            // For demo, just return the original graph for unimplemented passes
            Ok(graph.clone())
        }
    }
}

fn main() {
    println!("=== FluentAI RL Optimization Demo ===\n");
    
    // Create the sample program
    let original_graph = create_sample_program();
    println!("Original program created with {} nodes", original_graph.node_count());
    println!("Original performance score: {:.2}\n", measure_performance(&original_graph));
    
    // Initialize the RL agent
    let agent_config = RLConfig {
        learning_rate: 0.001,
        gamma: 0.95,
        epsilon_start: 1.0,
        epsilon_end: 0.01,
        epsilon_decay: 0.995,
        memory_size: 1000,
        batch_size: 32,
        target_update_frequency: 10,
        hidden_size: 128,
        num_hidden_layers: 2,
    };
    
    let mut agent = RLAgent::new(agent_config);
    println!("RL Agent initialized with config: {:?}\n", agent.config);
    
    // Initialize the environment
    let env_config = EnvironmentConfig {
        max_optimization_steps: 10,
        reward_improvement_weight: 1.0,
        reward_compilation_weight: 0.1,
        penalty_per_step: 0.01,
        enable_profiling: true,
    };
    
    let mut env = RLEnvironment::new(env_config);
    
    // Training configuration
    let training_config = TrainingConfig {
        num_epochs: 50,
        steps_per_epoch: 20,
        evaluation_frequency: 10,
        save_frequency: 25,
        early_stopping_patience: 10,
        target_performance: 0.9,
    };
    
    println!("Starting training for {} epochs...\n", training_config.num_epochs);
    
    // Track best optimization sequence
    let mut best_score = measure_performance(&original_graph);
    let mut best_actions = Vec::new();
    
    // Training loop
    for epoch in 0..training_config.num_epochs {
        let mut epoch_rewards = Vec::new();
        let mut epoch_improvements = Vec::new();
        
        for step in 0..training_config.steps_per_epoch {
            // Reset environment with the original graph
            let mut current_graph = original_graph.clone();
            let initial_score = measure_performance(&current_graph);
            
            // Create initial state
            let state = OptimizationState::from_graph(&current_graph);
            
            // Episode: Apply sequence of optimizations
            let mut episode_actions = Vec::new();
            let mut episode_reward = 0.0;
            
            for opt_step in 0..env_config.max_optimization_steps {
                // Agent selects action
                let action = agent.select_action(&state);
                episode_actions.push(action.clone());
                
                // Apply optimization
                match apply_optimization(&current_graph, &action) {
                    Ok(optimized_graph) => {
                        let new_score = measure_performance(&optimized_graph);
                        let improvement = new_score - measure_performance(&current_graph);
                        
                        // Calculate reward (improvement minus step penalty)
                        let reward = improvement - env_config.penalty_per_step;
                        episode_reward += reward;
                        
                        // Update current graph
                        current_graph = optimized_graph;
                        
                        // Create new state
                        let new_state = OptimizationState::from_graph(&current_graph);
                        
                        // Store experience
                        agent.remember(state.clone(), action, reward, new_state.clone(), false);
                        
                        // Train on minibatch if we have enough samples
                        if agent.memory_size() >= agent.config.batch_size {
                            agent.train_step();
                        }
                    }
                    Err(e) => {
                        // Optimization failed, give negative reward
                        let reward = -1.0;
                        episode_reward += reward;
                        
                        // Create terminal state
                        let new_state = state.clone();
                        agent.remember(state.clone(), action, reward, new_state, true);
                        break;
                    }
                }
            }
            
            // Track episode results
            let final_score = measure_performance(&current_graph);
            let total_improvement = final_score - initial_score;
            epoch_rewards.push(episode_reward);
            epoch_improvements.push(total_improvement);
            
            // Update best if this is better
            if final_score > best_score {
                best_score = final_score;
                best_actions = episode_actions.clone();
            }
        }
        
        // Calculate epoch statistics
        let avg_reward: f64 = epoch_rewards.iter().sum::<f64>() / epoch_rewards.len() as f64;
        let avg_improvement: f64 = epoch_improvements.iter().sum::<f64>() / epoch_improvements.len() as f64;
        
        // Update target network periodically
        if epoch % agent.config.target_update_frequency == 0 {
            agent.update_target_network();
        }
        
        // Print progress
        if epoch % 5 == 0 {
            println!("Epoch {}: Avg Reward: {:.3}, Avg Improvement: {:.3}, Epsilon: {:.3}", 
                     epoch, avg_reward, avg_improvement, agent.get_epsilon());
        }
        
        // Decay epsilon
        agent.decay_epsilon();
    }
    
    println!("\n=== Training Complete ===\n");
    
    // Apply best learned optimization sequence
    println!("Best optimization sequence found:");
    let mut optimized_graph = original_graph.clone();
    
    for (i, action) in best_actions.iter().enumerate() {
        println!("  Step {}: {:?}", i + 1, action);
        
        if let Ok(new_graph) = apply_optimization(&optimized_graph, action) {
            let old_score = measure_performance(&optimized_graph);
            let new_score = measure_performance(&new_graph);
            println!("    Score: {:.2} -> {:.2} (improvement: {:.2})", 
                     old_score, new_score, new_score - old_score);
            optimized_graph = new_graph;
        }
    }
    
    println!("\n=== Results ===");
    println!("Original nodes: {}", original_graph.node_count());
    println!("Optimized nodes: {}", optimized_graph.node_count());
    println!("Original score: {:.2}", measure_performance(&original_graph));
    println!("Optimized score: {:.2}", measure_performance(&optimized_graph));
    println!("Total improvement: {:.2}", measure_performance(&optimized_graph) - measure_performance(&original_graph));
    
    // Demonstrate learned behavior
    println!("\n=== Learned Optimization Strategy ===");
    
    // Create a new similar program to test generalization
    let test_graph = create_sample_program();
    let test_state = OptimizationState::from_graph(&test_graph);
    
    // Get agent's top 3 recommended actions
    println!("Agent's recommended optimization sequence for similar programs:");
    let mut temp_state = test_state;
    for i in 0..3 {
        let action = agent.select_action_greedy(&temp_state);
        println!("  {}: {:?}", i + 1, action);
        
        // Update state based on action (simplified)
        temp_state.applied_optimizations.insert(format!("{:?}", action));
    }
    
    println!("\nDemo complete!");
}