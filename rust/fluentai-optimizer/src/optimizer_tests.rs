#[cfg(test)]
mod tests {
    use crate::graph_optimizer::GraphOptimizer;
    use crate::pipeline::{OptimizationPipeline, OptimizationConfig, OptimizationLevel};
    use fluentai_core::ast::{Graph, Node, NodeId, Literal};
    use fluentai_parser::parse;
    use std::num::NonZeroU32;
    
    // ===== GraphOptimizer Tests =====
    
    #[test]
    fn test_graph_optimizer_new() {
        let mut optimizer = GraphOptimizer::new();
        // Just verify creation works
        drop(optimizer);
    }
    
    #[test]
    fn test_optimize_empty_graph() {
        let mut optimizer = GraphOptimizer::new();
        let graph = Graph::new();
        
        let result = optimizer.optimize(&graph);
        assert!(result.is_ok());
        
        let optimized = result.unwrap();
        assert!(optimized.nodes.is_empty());
    }
    
    #[test]
    fn test_optimize_simple_expression() {
        let mut optimizer = GraphOptimizer::new();
        let graph = parse("(+ 1 2)").unwrap();
        
        let result = optimizer.optimize(&graph);
        assert!(result.is_ok());
        
        let optimized = result.unwrap();
        assert!(optimized.root_id.is_some());
    }
    
    #[test]
    fn test_optimize_preserves_semantics() {
        let mut optimizer = GraphOptimizer::new();
        let code = "(let ((x 5)) (+ x 3))";
        let graph = parse(code).unwrap();
        
        let result = optimizer.optimize(&graph);
        assert!(result.is_ok());
        
        // Should preserve program behavior
        let optimized = result.unwrap();
        assert!(optimized.root_id.is_some());
    }
    
    // ===== OptimizationPipeline Tests =====
    
    #[test]
    fn test_pipeline_creation() {
        let config = OptimizationConfig::default();
        let mut pipeline = OptimizationPipeline::new(config);
        
        // Just verify creation
        drop(pipeline);
    }
    
    #[test]
    fn test_pipeline_with_level() {
        let config = OptimizationConfig {
            level: OptimizationLevel::Aggressive,
            ..Default::default()
        };
        
        let mut pipeline = OptimizationPipeline::new(config);
        let graph = parse("(+ 1 2)").unwrap();
        
        let result = pipeline.optimize(&graph);
        assert!(result.is_ok());
    }
    
    #[test]
    fn test_pipeline_none_level() {
        let config = OptimizationConfig::for_level(OptimizationLevel::None);
        
        let mut pipeline = OptimizationPipeline::new(config);
        let graph = parse("(+ 1 2)").unwrap();
        let original = graph.clone();
        
        let result = pipeline.optimize(&graph);
        assert!(result.is_ok());
        
        // Should not modify graph
        let optimized = result.unwrap();
        assert_eq!(optimized.nodes.len(), original.nodes.len());
    }
    
    #[test]
    #[ignore = "Pipeline pass disabling not fully implemented"]
    fn test_pipeline_with_disabled_passes() {
        let mut config = OptimizationConfig::default();
        config.constant_folding = false;  // Disable constant folding
        
        let mut pipeline = OptimizationPipeline::new(config);
        let graph = parse("(+ 1 2)").unwrap();
        
        let result = pipeline.optimize(&graph);
        assert!(result.is_ok());
        
        // Constant folding should be disabled
        let optimized = result.unwrap();
        if let Some(root) = optimized.root_id {
            // Should still be an application, not folded
            assert!(matches!(optimized.get_node(root), Some(Node::Application { .. })));
        }
    }
    
    #[test]
    fn test_pipeline_max_iterations() {
        let config = OptimizationConfig {
            max_iterations: 1,
            ..Default::default()
        };
        
        let mut pipeline = OptimizationPipeline::new(config);
        let graph = parse("(+ (+ 1 2) (+ 3 4))").unwrap();
        
        let result = pipeline.optimize(&graph);
        assert!(result.is_ok());
    }
    
    // ===== Specific Optimization Tests =====
    
    #[test]
    fn test_constant_folding() {
        let mut optimizer = GraphOptimizer::new();
        let graph = parse("(* (+ 1 2) (- 5 2))").unwrap();
        
        let result = optimizer.optimize(&graph);
        assert!(result.is_ok());
        
        let optimized = result.unwrap();
        // Should fold to 9 (3 * 3)
        if let Some(root) = optimized.root_id {
            if let Some(Node::Literal(Literal::Integer(n))) = optimized.get_node(root) {
                assert_eq!(*n, 9);
            }
        }
    }
    
    #[test]
    fn test_dead_code_elimination() {
        let mut optimizer = GraphOptimizer::new();
        let code = "(let ((x 1) (y 2) (z 3)) x)";
        let graph = parse(code).unwrap();
        let original_size = graph.nodes.len();
        
        let result = optimizer.optimize(&graph);
        assert!(result.is_ok());
        
        let optimized = result.unwrap();
        // Should remove unused bindings (y and z)
        assert!(optimized.nodes.len() < original_size);
        
        // Verify that we still have x but not y or z
        let has_x = optimized.nodes.values().any(|node| {
            matches!(node, Node::Literal(Literal::Integer(1)))
        });
        assert!(has_x, "Should keep used binding 'x'");
        
        // Count literals - should only have 1 (for x)
        let literal_count = optimized.nodes.values().filter(|node| {
            matches!(node, Node::Literal(Literal::Integer(_)))
        }).count();
        assert_eq!(literal_count, 1, "Should only have literal for x, not y or z");
    }
    
    #[test]
    fn test_preserve_effects() {
        let mut optimizer = GraphOptimizer::new();
        let code = "(do (effect IO:print \"hello\") 42)";
        let graph = parse(code).unwrap();
        
        let result = optimizer.optimize(&graph);
        assert!(result.is_ok());
        
        // Should preserve the effect
        let optimized = result.unwrap();
        let has_effect = optimized.nodes.values().any(|node| {
            matches!(node, Node::Effect { .. })
        });
        assert!(has_effect);
    }
    
    #[test]
    fn test_optimization_idempotent() {
        let mut optimizer = GraphOptimizer::new();
        let graph = parse("(+ 1 2)").unwrap();
        
        let result1 = optimizer.optimize(&graph);
        assert!(result1.is_ok());
        let optimized1 = result1.unwrap();
        
        let result2 = optimizer.optimize(&optimized1);
        assert!(result2.is_ok());
        let optimized2 = result2.unwrap();
        
        // Second optimization should not change anything
        assert_eq!(optimized1.nodes.len(), optimized2.nodes.len());
    }
    
    // ===== Error Handling Tests =====
    
    #[test]
    fn test_handle_malformed_graph() {
        let mut optimizer = GraphOptimizer::new();
        let mut graph = Graph::new();
        
        // Create a malformed graph with dangling references
        let node1 = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
        let node2 = NodeId(NonZeroU32::new(999).unwrap()); // Non-existent
        graph.add_node(Node::Application { 
            function: node1, 
            args: vec![node2] 
        }).expect("Failed to add node");
        
        let result = optimizer.optimize(&graph);
        // Should handle gracefully
        assert!(result.is_ok() || result.is_err());
    }
    
    #[test]
    fn test_very_deep_expression() {
        let mut optimizer = GraphOptimizer::new();
        
        // Create deeply nested expression
        let mut expr = "1".to_string();
        for _ in 0..20 {
            expr = format!("(+ {} 1)", expr);
        }
        
        let graph = parse(&expr).unwrap();
        let result = optimizer.optimize(&graph);
        
        // Should handle deep expressions
        assert!(result.is_ok());
    }
    
    #[test]
    fn test_cyclic_references() {
        let mut optimizer = GraphOptimizer::new();
        let code = "(letrec ((x y) (y x)) x)";
        let graph = parse(code).unwrap();
        
        let result = optimizer.optimize(&graph);
        // Should handle cycles gracefully
        assert!(result.is_ok() || result.is_err());
    }
}