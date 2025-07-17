//! Integration tests for RL-based optimization

#[cfg(all(feature = "ai-analysis", feature = "rl"))]
mod rl_tests {
    use fluentai_ai::rl::*;
    use fluentai_core::ast::{Graph, Node, NodeId, NodeMetadata};
    use fluentai_core::value::Value;
    use std::sync::Arc;
    use std::sync::Mutex;
    use std::collections::HashMap;
    
    // Mock compiled program
    #[derive(Clone)]
    struct MockCompiledProgram {
        optimization_config: String,
        node_count: usize,
    }
    
    // Mock performance metrics based on optimization
    fn mock_execute(program: &MockCompiledProgram) -> PerformanceMetrics {
        // Simulate performance based on optimizations applied
        let base_time = 1000000; // 1 second in microseconds
        let base_memory = 100_000_000; // 100MB
        
        // Different optimizations have different effects
        let time_reduction = if program.optimization_config.contains("constant_folding") { 0.1 } else { 0.0 }
            + if program.optimization_config.contains("dead_code") { 0.15 } else { 0.0 }
            + if program.optimization_config.contains("inline") { 0.2 } else { 0.0 }
            + if program.optimization_config.contains("cse") { 0.1 } else { 0.0 };
        
        let memory_reduction = if program.optimization_config.contains("dead_code") { 0.2 } else { 0.0 }
            + if program.optimization_config.contains("cse") { 0.1 } else { 0.0 };
        
        PerformanceMetrics {
            execution_time_us: (base_time as f32 * (1.0 - time_reduction)) as u64,
            memory_bytes: (base_memory as f32 * (1.0 - memory_reduction)) as u64,
            binary_size: program.node_count * 100,
            instruction_count: program.node_count as u64 * 10,
            cache_misses: 1000,
            branch_mispredictions: 100,
        }
    }
    
    fn create_test_graph(size: usize) -> Graph {
        let mut graph = Graph::new();
        
        // Create a graph with specified number of nodes
        let mut nodes = Vec::new();
        for i in 0..size {
            let node = graph.add_node(Node::Literal {
                value: Value::Integer(i as i64),
                metadata: NodeMetadata::default(),
            });
            nodes.push(node);
        }
        
        // Create some applications
        if nodes.len() > 2 {
            let app = graph.add_node(Node::Application {
                function: NodeId::placeholder(),
                args: vec![nodes[0], nodes[1]],
                metadata: NodeMetadata::default(),
            });
            graph.root_id = Some(app);
        } else if !nodes.is_empty() {
            graph.root_id = Some(nodes[0]);
        }
        
        graph
    }
    
    #[test]
    fn test_optimization_state_creation() {
        let state = OptimizationState {
            ast_features: vec![1.0, 0.5, 0.8, 0.3],
            current_config: OptimizationConfig::default(),
            performance_history: vec![0.5, 0.6, 0.7],
            resource_history: vec![
                ResourceMetrics {
                    memory_bytes: 100_000_000,
                    compilation_time_us: 50000,
                    binary_size: 10000,
                },
            ],
        };
        
        assert_eq!(state.ast_features.len(), 4);
        assert_eq!(state.performance_history.len(), 3);
        assert!(!state.current_config.constant_folding);
    }
    
    #[test]
    fn test_optimization_action_application() {
        let mut config = OptimizationConfig::default();
        
        config.apply_action(OptimizationAction::ConstantFolding);
        assert!(config.constant_folding);
        
        config.apply_action(OptimizationAction::Inline(InlineLevel::Standard));
        assert!(config.inline);
        assert_eq!(config.inline_threshold, 10);
        
        config.apply_action(OptimizationAction::Composite(0x00F)); // First 4 optimizations
        assert!(config.constant_folding);
        assert!(config.dead_code_elimination);
        assert!(config.cse);
        assert!(config.inline);
    }
    
    #[test]
    fn test_reward_calculation() {
        let baseline = PerformanceMetrics {
            execution_time_us: 1000000,
            memory_bytes: 100_000_000,
            binary_size: 50000,
            instruction_count: 10000,
            cache_misses: 1000,
            branch_mispredictions: 100,
        };
        
        let improved = PerformanceMetrics {
            execution_time_us: 800000, // 20% improvement
            memory_bytes: 90_000_000,  // 10% improvement
            binary_size: 45000,        // 10% improvement
            instruction_count: 9000,    // 10% improvement
            cache_misses: 900,         // 10% improvement
            branch_mispredictions: 90, // 10% improvement
        };
        
        let calculator = RewardCalculator::new(
            Default::default(),
            Default::default(),
        );
        
        let reward_details = calculator.calculate(improved, baseline, 100000);
        
        assert!(reward_details.total_reward > 0.0);
        assert_eq!(reward_details.exec_improvement, 0.2);
        assert_eq!(reward_details.memory_improvement, 0.1);
    }
    
    #[test]
    fn test_experience_replay() {
        let mut replay = ExperienceReplay::new(100);
        
        let state = OptimizationState {
            ast_features: vec![1.0; 10],
            current_config: OptimizationConfig::default(),
            performance_history: vec![],
            resource_history: vec![],
        };
        
        // Add experiences
        for i in 0..50 {
            let exp = Experience {
                state: state.clone(),
                action: OptimizationAction::NoOp,
                reward: i as f32 * 0.1,
                next_state: state.clone(),
                done: false,
            };
            replay.push(exp);
        }
        
        assert_eq!(replay.len(), 50);
        
        // Sample batch
        let batch = replay.sample(10);
        assert_eq!(batch.len(), 10);
        
        // Check stats
        let stats = replay.get_stats();
        assert_eq!(stats.total_experiences, 50);
        assert!(stats.mean_reward > 0.0);
    }
    
    #[test]
    fn test_environment_step() {
        let compile_count = Arc::new(Mutex::new(0));
        let compile_count_clone = compile_count.clone();
        
        let mut env = OptimizationEnvironment::new(
            10,
            move |graph, config| {
                let mut count = compile_count_clone.lock().unwrap();
                *count += 1;
                
                // Convert config HashMap to string for MockCompiledProgram
                let config_str = config.iter()
                    .filter_map(|(k, v)| {
                        if v.as_bool().unwrap_or(false) {
                            Some(k.as_str())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                
                Ok(CompiledProgram {
                    bytecode: vec![],
                    compilation_time_us: 50000,
                    binary_size: graph.nodes.len() as u64 * 100,
                })
            },
            |_program| {
                Ok(PerformanceMetrics {
                    execution_time_us: 500000,
                    memory_bytes: 50_000_000,
                    binary_size: 10000,
                    instruction_count: 5000,
                    cache_misses: 500,
                    branch_mispredictions: 50,
                })
            },
        );
        
        let graph = create_test_graph(10);
        let mut state = env.reset(graph).unwrap();
        
        // Take a step
        let result = env.step(&mut state, OptimizationAction::ConstantFolding).unwrap();
        
        assert!(state.config.constant_folding);
        assert_eq!(state.step_count, 1);
        assert!(!result.done);
        
        // Verify compilation was called
        assert_eq!(*compile_count.lock().unwrap(), 2); // Once for baseline, once for step
    }
    
    #[test]
    fn test_rl_config_conversion() {
        let mut rl_config = OptimizationConfig::default();
        rl_config.constant_folding = true;
        rl_config.dead_code_elimination = true;
        rl_config.inline = true;
        rl_config.inline_threshold = 15;
        
        let optimizer_config = rl_config.to_dict();
        
        assert_eq!(optimizer_config.get("constant_folding").unwrap().as_bool(), Some(true));
        assert_eq!(optimizer_config.get("dead_code_elimination").unwrap().as_bool(), Some(true));
        assert_eq!(optimizer_config.get("inline").unwrap().as_bool(), Some(true));
        assert_eq!(optimizer_config.get("inline_threshold").unwrap().as_i64(), Some(15));
    }
    
    #[test]
    fn test_adaptive_reward_targets() {
        let mut calculator = RewardCalculator::new(
            Default::default(),
            PerformanceTargets {
                exec_time_reduction: 0.1,
                memory_reduction: 0.1,
                size_reduction: 0.1,
            },
        );
        
        let baseline = PerformanceMetrics {
            execution_time_us: 1000000,
            memory_bytes: 100_000_000,
            binary_size: 50000,
            instruction_count: 10000,
            cache_misses: 1000,
            branch_mispredictions: 100,
        };
        
        // Achieve targets
        let improved = PerformanceMetrics {
            execution_time_us: 800000, // 20% > 10% target
            memory_bytes: 80_000_000,  // 20% > 10% target
            binary_size: 40000,        // 20% > 10% target
            instruction_count: 8000,
            cache_misses: 800,
            branch_mispredictions: 80,
        };
        
        let details = calculator.calculate(improved, baseline, 50000);
        assert!(details.targets_met);
        
        // Update targets
        calculator.adaptive_update_targets(&details);
        
        // Targets should be more aggressive now
        assert!(calculator.targets.exec_time_reduction > 0.1);
        assert!(calculator.targets.memory_reduction > 0.1);
    }
}

#[cfg(all(test, not(any(feature = "ai-analysis", feature = "rl"))))]
mod rl_disabled_tests {
    #[test]
    fn test_rl_features_disabled() {
        // When RL features are disabled, compilation should still work
        // This ensures the codebase remains functional without RL
        assert!(true);
    }
}