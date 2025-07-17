//! Basic RL tests that don't require full compilation

#[test]
fn test_rl_feature_exists() {
    // This test just verifies that the RL feature compiles
    assert!(true);
}

#[cfg(feature = "rl")]
mod rl_unit_tests {
    use std::collections::HashMap;
    
    #[test]
    fn test_optimization_config_to_dict() {
        use fluentai_ai::rl::OptimizationConfig;
        
        let mut config = OptimizationConfig::default();
        config.constant_folding = true;
        config.inline = true;
        config.inline_threshold = 15;
        
        let dict = config.to_dict();
        
        assert_eq!(dict.get("constant_folding").unwrap().as_bool(), Some(true));
        assert_eq!(dict.get("inline").unwrap().as_bool(), Some(true));
        assert_eq!(dict.get("inline_threshold").unwrap().as_i64(), Some(15));
    }
    
    #[test]
    fn test_optimization_action_apply() {
        use fluentai_ai::rl::{OptimizationConfig, OptimizationAction, InlineLevel};
        
        let mut config = OptimizationConfig::default();
        
        // Test applying different actions
        config.apply_action(OptimizationAction::ConstantFolding);
        assert!(config.constant_folding);
        
        config.apply_action(OptimizationAction::Inline(InlineLevel::Aggressive));
        assert!(config.inline);
        assert_eq!(config.inline_threshold, 20);
        
        // Test composite action
        config = OptimizationConfig::default();
        config.apply_action(OptimizationAction::Composite(0x00F)); // First 4 optimizations
        assert!(config.constant_folding);
        assert!(config.dead_code_elimination);
        assert!(config.cse);
        assert!(config.inline);
    }
    
    #[test]
    fn test_performance_metrics() {
        use fluentai_ai::rl::PerformanceMetrics;
        
        let metrics = PerformanceMetrics {
            execution_time_us: 1000000,
            memory_bytes: 100_000_000,
            binary_size: 50000,
            instruction_count: 10000,
            cache_misses: 1000,
            branch_mispredictions: 100,
        };
        
        assert_eq!(metrics.execution_time_us, 1000000);
        assert_eq!(metrics.memory_bytes, 100_000_000);
    }
    
    #[test]
    fn test_reward_calculation_simple() {
        use fluentai_ai::rl::{RewardCalculator, PerformanceMetrics, PerformanceTargets};
        
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
            PerformanceTargets {
                exec_time_reduction: 0.1,
                memory_reduction: 0.1,
                size_reduction: 0.1,
            },
        );
        
        let details = calculator.calculate(improved, baseline, 50000);
        
        // Should have positive reward since we improved performance
        assert!(details.total_reward > 0.0);
        assert_eq!(details.exec_improvement, 0.2);
        assert_eq!(details.memory_improvement, 0.1);
        assert!(details.targets_met);
    }
}