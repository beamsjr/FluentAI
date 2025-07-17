//! Integration tests for AI-driven optimization

#[cfg(feature = "ai-analysis")]
mod ai_optimization_tests {
    use fluentai_core::ast::{Graph, Node, NodeId, NodeMetadata};
    use fluentai_core::value::Value;
    use fluentai_optimizer::{
        ai_hints_to_optimization_config, hybrid_optimization_config,
        OptimizationConfig, OptimizationLevel, OptimizationPipeline, OptimizationSource,
    };
    use fluentai_ai::{
        analyze_ast, AiAnalysisResult, DetectedPattern, OptimizationSuggestion, OptimizationType,
        PatternType,
    };
    use std::collections::HashMap;

    fn create_test_graph() -> Graph {
        let mut graph = Graph::new();
        
        // Create a simple graph with map-reduce pattern
        let const1 = graph.add_node(Node::Literal {
            value: Value::Integer(1),
            metadata: NodeMetadata::default(),
        });
        
        let const2 = graph.add_node(Node::Literal {
            value: Value::Integer(2),
            metadata: NodeMetadata::default(),
        });
        
        let add_node = graph.add_node(Node::Application {
            function: NodeId::placeholder(),
            args: vec![const1, const2],
            metadata: NodeMetadata::default(),
        });
        
        graph.root_id = Some(add_node);
        graph
    }

    #[test]
    fn test_ai_hints_to_optimization_config() {
        let graph = create_test_graph();
        
        // Create a mock AI analysis result
        let analysis = AiAnalysisResult {
            optimizations: vec![
                OptimizationSuggestion {
                    optimization_type: OptimizationType::ConstantFold,
                    target_nodes: vec![NodeId::placeholder()],
                    expected_improvement: 0.8,
                    description: "Fold constant expressions".to_string(),
                },
                OptimizationSuggestion {
                    optimization_type: OptimizationType::Inline,
                    target_nodes: vec![NodeId::placeholder()],
                    expected_improvement: 0.9,
                    description: "Inline small functions".to_string(),
                },
            ],
            patterns: vec![
                DetectedPattern {
                    pattern_type: PatternType::MapReduce,
                    nodes: vec![],
                    confidence: 0.85,
                },
            ],
            embeddings: HashMap::new(),
            performance_score: 0.7,
            confidence: 0.8,
        };
        
        let config = ai_hints_to_optimization_config(&graph, &analysis);
        
        // Verify the configuration
        assert!(config.constant_folding);
        assert!(config.inline);
        assert!(config.strength_reduction); // From MapReduce pattern
        assert!(config.algebraic_simplification); // From MapReduce pattern
        assert_eq!(config.inline_threshold, 20); // High confidence inline
        assert_eq!(config.max_iterations, 3); // High overall confidence
        assert!(!config.debug_mode); // High confidence = no debug
    }

    #[test]
    fn test_hybrid_optimization_config() {
        let graph = create_test_graph();
        
        // Create AI analysis with different suggestions
        let analysis = AiAnalysisResult {
            optimizations: vec![
                OptimizationSuggestion {
                    optimization_type: OptimizationType::LoopUnroll,
                    target_nodes: vec![],
                    expected_improvement: 0.6,
                    description: "Unroll small loops".to_string(),
                },
                OptimizationSuggestion {
                    optimization_type: OptimizationType::CSE,
                    target_nodes: vec![],
                    expected_improvement: 0.7,
                    description: "Eliminate common subexpressions".to_string(),
                },
            ],
            patterns: vec![],
            embeddings: HashMap::new(),
            performance_score: 0.65,
            confidence: 0.6,
        };
        
        let config = hybrid_optimization_config(OptimizationLevel::Basic, &graph, &analysis);
        
        // Basic enables constant folding and dead code elimination
        assert!(config.constant_folding);
        assert!(config.dead_code_elimination);
        
        // AI adds loop optimization and CSE
        assert!(config.loop_optimization);
        assert!(config.cse);
        
        // Verify other settings
        assert_eq!(config.max_iterations, 2); // Medium confidence
    }

    #[test]
    fn test_optimization_pipeline_from_ai_source() {
        let graph = create_test_graph();
        
        // Create pipeline from AI source
        let pipeline = OptimizationPipeline::from_source(
            OptimizationSource::AIHints,
            &graph
        );
        
        // Should succeed (though actual AI analysis might return conservative config)
        assert!(pipeline.is_ok());
    }

    #[test]
    fn test_low_confidence_ai_analysis() {
        let graph = create_test_graph();
        
        // Create low confidence analysis
        let analysis = AiAnalysisResult {
            optimizations: vec![],
            patterns: vec![],
            embeddings: HashMap::new(),
            performance_score: 0.3,
            confidence: 0.2,
        };
        
        let config = ai_hints_to_optimization_config(&graph, &analysis);
        
        // Low confidence should result in conservative settings
        assert_eq!(config.max_iterations, 1);
        assert!(config.debug_mode); // Low confidence = debug mode
    }

    #[test]
    fn test_pattern_based_optimization_config() {
        let graph = create_test_graph();
        
        // Test different patterns
        let patterns = vec![
            (PatternType::Recursion, 0.8),
            (PatternType::HigherOrder, 0.7),
            (PatternType::MonadicChain, 0.6),
        ];
        
        for (pattern_type, confidence) in patterns {
            let analysis = AiAnalysisResult {
                optimizations: vec![],
                patterns: vec![
                    DetectedPattern {
                        pattern_type,
                        nodes: vec![],
                        confidence,
                    },
                ],
                embeddings: HashMap::new(),
                performance_score: 0.5,
                confidence: 0.5,
            };
            
            let config = ai_hints_to_optimization_config(&graph, &analysis);
            
            match pattern_type {
                PatternType::Recursion => {
                    assert!(config.tail_call_optimization);
                    if confidence > 0.7 {
                        assert!(config.loop_optimization);
                    }
                }
                PatternType::HigherOrder => {
                    assert!(config.beta_reduction);
                    assert!(config.partial_evaluation);
                }
                PatternType::MonadicChain => {
                    assert!(config.beta_reduction);
                    assert!(config.inline);
                }
                _ => {}
            }
        }
    }

    #[test]
    fn test_targeted_optimization_hints() {
        use fluentai_optimizer::TargetedOptimizationHints;
        
        let node1 = NodeId::placeholder();
        let node2 = NodeId::placeholder();
        
        let analysis = AiAnalysisResult {
            optimizations: vec![
                OptimizationSuggestion {
                    optimization_type: OptimizationType::Inline,
                    target_nodes: vec![node1],
                    expected_improvement: 0.8,
                    description: "Inline this function".to_string(),
                },
                OptimizationSuggestion {
                    optimization_type: OptimizationType::ConstantFold,
                    target_nodes: vec![node2],
                    expected_improvement: 0.9,
                    description: "Fold this constant".to_string(),
                },
            ],
            patterns: vec![],
            embeddings: HashMap::new(),
            performance_score: 0.7,
            confidence: 0.75,
        };
        
        let hints = TargetedOptimizationHints::from_analysis(&analysis);
        
        assert!(hints.inline_targets.contains(&node1));
        assert!(hints.constant_fold_targets.contains(&node2));
        assert!(hints.inline_targets.len() == 1);
        assert!(hints.constant_fold_targets.len() == 1);
    }

    #[test]
    fn test_adaptive_inline_threshold() {
        let graph = create_test_graph();
        
        // Test different confidence levels for inline optimization
        let confidence_levels = vec![(0.9, 20), (0.6, 10), (0.3, 5)];
        
        for (confidence, expected_threshold) in confidence_levels {
            let analysis = AiAnalysisResult {
                optimizations: vec![
                    OptimizationSuggestion {
                        optimization_type: OptimizationType::Inline,
                        target_nodes: vec![],
                        expected_improvement: confidence,
                        description: "Inline functions".to_string(),
                    },
                ],
                patterns: vec![],
                embeddings: HashMap::new(),
                performance_score: 0.5,
                confidence: 0.5,
            };
            
            let config = ai_hints_to_optimization_config(&graph, &analysis);
            
            assert!(config.inline);
            assert_eq!(config.inline_threshold, expected_threshold);
        }
    }
}

#[cfg(all(test, not(feature = "ai-analysis")))]
mod ai_optimization_disabled_tests {
    use fluentai_optimizer::{OptimizationLevel, OptimizationPipeline, OptimizationSource};
    use fluentai_core::ast::Graph;
    
    #[test]
    fn test_ai_optimization_disabled() {
        let graph = Graph::new();
        
        // When AI analysis is disabled, should fall back to manual optimization
        let pipeline = OptimizationPipeline::from_source(
            OptimizationSource::Manual(OptimizationLevel::Standard),
            &graph
        );
        
        assert!(pipeline.is_ok());
    }
}