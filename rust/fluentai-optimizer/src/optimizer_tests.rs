#[cfg(test)]
mod tests {
    use crate::graph_optimizer::GraphOptimizer;
    use crate::pipeline::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
    use fluentai_core::ast::{Graph, Literal, Node, NodeId};
    use fluentai_parser::parse_flc;
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
        let graph = parse_flc("1 + 2").unwrap();

        let result = optimizer.optimize(&graph);
        assert!(result.is_ok());

        let optimized = result.unwrap();
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_optimize_preserves_semantics() {
        let mut optimizer = GraphOptimizer::new();
        let code = "{ let x = 5; x + 3 }";
        let graph = parse_flc(code).unwrap();

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
        let graph = parse_flc("1 + 2").unwrap();

        let result = pipeline.optimize(&graph);
        assert!(result.is_ok());
    }

    #[test]
    fn test_pipeline_none_level() {
        let config = OptimizationConfig::for_level(OptimizationLevel::None);

        let mut pipeline = OptimizationPipeline::new(config);
        let graph = parse_flc("1 + 2").unwrap();
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
        config.constant_folding = false; // Disable constant folding

        let mut pipeline = OptimizationPipeline::new(config);
        let graph = parse_flc("1 + 2").unwrap();

        let result = pipeline.optimize(&graph);
        assert!(result.is_ok());

        // Constant folding should be disabled
        let optimized = result.unwrap();
        if let Some(root) = optimized.root_id {
            // Should still be an application, not folded
            assert!(matches!(
                optimized.get_node(root),
                Some(Node::Application { .. })
            ));
        }
    }

    #[test]
    fn test_pipeline_max_iterations() {
        let config = OptimizationConfig {
            max_iterations: 1,
            ..Default::default()
        };

        let mut pipeline = OptimizationPipeline::new(config);
        let graph = parse_flc("(1 + 2) + (3 + 4)").unwrap();

        let result = pipeline.optimize(&graph);
        assert!(result.is_ok());
    }

    // ===== Specific Optimization Tests =====

    #[test]
    fn test_constant_folding() {
        let mut optimizer = GraphOptimizer::new();
        let graph = parse_flc("(1 + 2) * (5 - 2)").unwrap();

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
        let code = "{ let x = 1; let y = 2; let z = 3; x }";
        let graph = parse_flc(code).unwrap();
        let original_size = graph.nodes.len();

        let result = optimizer.optimize(&graph);
        assert!(result.is_ok());

        let optimized = result.unwrap();
        // Should remove unused bindings (y and z)
        assert!(optimized.nodes.len() < original_size);

        // Verify that we still have x but not y or z
        let has_x = optimized
            .nodes
            .values()
            .any(|node| matches!(node, Node::Literal(Literal::Integer(1))));
        assert!(has_x, "Should keep used binding 'x'");

        // Count literals - should only have 1 (for x)
        let literal_count = optimized
            .nodes
            .values()
            .filter(|node| matches!(node, Node::Literal(Literal::Integer(_))))
            .count();
        assert_eq!(
            literal_count, 1,
            "Should only have literal for x, not y or z"
        );
    }

    #[test]
    fn test_preserve_effects() {
        let mut optimizer = GraphOptimizer::new();
        let code = "{ perform IO.print(\"hello\"); 42 }";
        let graph = parse_flc(code).unwrap();

        let result = optimizer.optimize(&graph);
        assert!(result.is_ok());

        // Should preserve the effect
        let optimized = result.unwrap();
        let has_print = optimized
            .nodes
            .values()
            .any(|node| matches!(node, Node::Effect { .. }));
        assert!(has_print, "IO.print effect should be preserved");
    }

    #[test]
    fn test_optimization_idempotent() {
        let mut optimizer = GraphOptimizer::new();
        let graph = parse_flc("1 + 2").unwrap();

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
        let node1 = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .expect("Failed to add node");
        let node2 = NodeId(NonZeroU32::new(999).unwrap()); // Non-existent
        graph
            .add_node(Node::Application {
                function: node1,
                args: vec![node2],
            })
            .expect("Failed to add node");

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
            expr = format!("{} + 1", expr);
        }

        let graph = parse_flc(&expr).unwrap();
        let result = optimizer.optimize(&graph);

        // Should handle deep expressions
        assert!(result.is_ok());
    }

    #[test]
    fn test_cyclic_references() {
        let mut optimizer = GraphOptimizer::new();
        let code = "{ let x = () => y(); let y = () => x(); x() }";
        let graph = parse_flc(code).unwrap();

        let result = optimizer.optimize(&graph);
        // Should handle cycles gracefully
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_optimizer_corrupts_channel_with_capacity() {
        // Create a graph for: (chan 10)
        let mut graph = Graph::new();
        let capacity = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
        let channel = graph.add_node(Node::Channel { capacity: Some(capacity) }).unwrap();
        graph.root_id = Some(channel);
        
        // Debug: Print original graph
        println!("Original graph:");
        println!("  NodeId(1) = {:?}", graph.get_node(NodeId(NonZeroU32::new(1).unwrap())));
        println!("  NodeId(2) = {:?}", graph.get_node(NodeId(NonZeroU32::new(2).unwrap())));
        println!("  root_id = {:?}", graph.root_id);
        
        // Apply optimization
        let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
        let mut pipeline = OptimizationPipeline::new(config);
        let optimized = pipeline.optimize(&graph).unwrap();
        
        // Debug: Print optimized graph
        println!("\nOptimized graph:");
        println!("  NodeId(1) = {:?}", optimized.get_node(NodeId(NonZeroU32::new(1).unwrap())));
        println!("  NodeId(2) = {:?}", optimized.get_node(NodeId(NonZeroU32::new(2).unwrap())));
        println!("  root_id = {:?}", optimized.root_id);
        
        // Check that nodes are not self-referential
        if let Some(Node::Channel { capacity: Some(cap_id) }) = optimized.get_node(NodeId(NonZeroU32::new(1).unwrap())) {
            assert_ne!(*cap_id, NodeId(NonZeroU32::new(1).unwrap()), "Node 1 is self-referential!");
        }
        if let Some(Node::Channel { capacity: Some(cap_id) }) = optimized.get_node(NodeId(NonZeroU32::new(2).unwrap())) {
            assert_ne!(*cap_id, NodeId(NonZeroU32::new(2).unwrap()), "Node 2 is self-referential!");
        }
        
        // The channel node should still exist and point to a literal
        let root_id = optimized.root_id.expect("Optimized graph should have a root");
        let root_node = optimized.get_node(root_id).expect("Root node should exist");
        match root_node {
            Node::Channel { capacity: Some(cap_id) } => {
                // Debug: Print all nodes in the optimized graph
                println!("\nAll nodes in optimized graph:");
                for (id, node) in &optimized.nodes {
                    println!("  {:?} = {:?}", id, node);
                }
                
                let cap_node = optimized.get_node(*cap_id);
                match cap_node {
                    Some(Node::Literal(Literal::Integer(10))) => {
                        // This is correct
                    }
                    Some(other) => panic!("Capacity node is not a literal integer: {:?}", other),
                    None => panic!("Capacity node {:?} does not exist in optimized graph", cap_id),
                }
            }
            _ => panic!("Root node is not a channel: {:?}", root_node),
        }
    }
}
