//! Integration tests for dead code elimination with effect analysis

use fluentai_core::ast::{EffectType, Graph, Literal, Node, NodeId};
use fluentai_optimizer::passes::{dead_code::DeadCodeEliminationPass, OptimizationPass};
use fluentai_parser::parse;
use std::num::NonZeroU32;

#[test]
fn test_dead_code_preserves_all_effect_types() {
    // Create a graph with all effect types
    let mut graph = Graph::new();
    
    // Create effect nodes for each effect type
    let io_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "print".to_string(),
        args: vec![],
    }).unwrap();
    
    let state_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::State,
        operation: "set".to_string(),
        args: vec![],
    }).unwrap();
    
    let error_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::Error,
        operation: "throw".to_string(),
        args: vec![],
    }).unwrap();
    
    let time_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::Time,
        operation: "sleep".to_string(),
        args: vec![],
    }).unwrap();
    
    let random_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::Random,
        operation: "random".to_string(),
        args: vec![],
    }).unwrap();
    
    let network_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::Network,
        operation: "fetch".to_string(),
        args: vec![],
    }).unwrap();
    
    let async_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::Async,
        operation: "async".to_string(),
        args: vec![],
    }).unwrap();
    
    let concurrent_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::Concurrent,
        operation: "spawn".to_string(),
        args: vec![],
    }).unwrap();
    
    let dom_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::Dom,
        operation: "querySelector".to_string(),
        args: vec![],
    }).unwrap();
    
    // Create a pure node that should be removed
    let unused_pure = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    
    // Create a let binding with all these effects (unused) and one used value
    let used_value = graph.add_node(Node::Literal(Literal::Integer(100))).unwrap();
    
    let bindings = vec![
        ("io".to_string(), io_effect),
        ("state".to_string(), state_effect),
        ("error".to_string(), error_effect),
        ("time".to_string(), time_effect),
        ("random".to_string(), random_effect),
        ("network".to_string(), network_effect),
        ("async".to_string(), async_effect),
        ("concurrent".to_string(), concurrent_effect),
        ("dom".to_string(), dom_effect),
        ("unused_pure".to_string(), unused_pure),
        ("used".to_string(), used_value),
    ];
    
    let body = graph.add_node(Node::Variable { name: "used".to_string() }).unwrap();
    
    let let_node = graph.add_node(Node::Let { bindings, body }).unwrap();
    graph.root_id = Some(let_node);
    
    // Run dead code elimination
    let mut pass = DeadCodeEliminationPass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // All effect nodes should be preserved
    let effect_types_found: Vec<_> = optimized.nodes.values()
        .filter_map(|node| {
            if let Node::Effect { effect_type, .. } = node {
                Some(*effect_type)
            } else {
                None
            }
        })
        .collect();
    
    assert_eq!(effect_types_found.len(), 9, "All 9 effect types should be preserved");
    
    // The unused pure literal should be removed
    let literal_count = optimized.nodes.values()
        .filter(|node| matches!(node, Node::Literal(Literal::Integer(42))))
        .count();
    
    assert_eq!(literal_count, 0, "Unused pure literal should be removed");
}

#[test]
fn test_dead_code_effect_primitive_recognition() {
    // Test that effect primitives are recognized correctly
    let test_cases = vec![
        // IO effects
        ("(let ((x (print \"hello\"))) 1)", true),
        ("(let ((x (println \"world\"))) 1)", true),
        ("(let ((x (read-file \"test.txt\"))) 1)", true),
        ("(let ((x (write-file \"out.txt\" \"data\"))) 1)", true),
        
        // State effects
        ("(let ((x (set! y 42))) 1)", true),
        ("(let ((x (ref 10))) 1)", true),
        ("(let ((x (atom 5))) 1)", true),
        
        // Pure functions that should be removed
        ("(let ((x (+ 1 2))) 1)", false),
        ("(let ((x (* 3 4))) 1)", false),
        ("(let ((x (str-concat \"a\" \"b\"))) 1)", false),
        ("(let ((x (car (list 1 2)))) 1)", false),
    ];
    
    for (code, should_preserve) in test_cases {
        let graph = parse(code).unwrap();
        let original_size = graph.nodes.len();
        
        let mut pass = DeadCodeEliminationPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        if should_preserve {
            // Effect should be preserved, so size reduction should be minimal
            assert!(
                optimized.nodes.len() >= original_size - 3,
                "Effect in '{}' should be preserved",
                code
            );
        } else {
            // Pure computation should be removed, significant size reduction
            assert!(
                optimized.nodes.len() < original_size - 2,
                "Pure computation in '{}' should be removed",
                code
            );
        }
    }
}

#[test]
fn test_dead_code_nested_effects() {
    // Test that nested effects are preserved
    let code = r#"
        (let ((outer (let ((inner (print "nested effect")))
                         42)))
          100)
    "#;
    
    let graph = parse(code).unwrap();
    let mut pass = DeadCodeEliminationPass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // The print effect should be preserved even though it's nested
    let has_print = optimized.nodes.values().any(|node| {
        matches!(node, Node::Variable { name } if name == "print")
    });
    
    assert!(has_print, "Nested print effect should be preserved");
}

#[test]
fn test_dead_code_effect_in_unused_lambda() {
    // Effects inside unused lambdas should be removed
    // (since the lambda is never called)
    let code = r#"
        (let ((unused-fn (lambda () (print "never called")))
              (result 42))
          result)
    "#;
    
    let graph = parse(code).unwrap();
    let mut pass = DeadCodeEliminationPass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // The lambda and its body should be removed
    let has_print = optimized.nodes.values().any(|node| {
        matches!(node, Node::Variable { name } if name == "print")
    });
    
    assert!(!has_print, "Effect in unused lambda should be removed");
}

#[test]
fn test_dead_code_effect_in_used_lambda() {
    // Effects inside used lambdas should be preserved
    let code = r#"
        (let ((used-fn (lambda () (print "will be called"))))
          (used-fn))
    "#;
    
    let graph = parse(code).unwrap();
    let mut pass = DeadCodeEliminationPass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // The lambda and its body should be preserved
    let has_print = optimized.nodes.values().any(|node| {
        matches!(node, Node::Variable { name } if name == "print")
    });
    
    assert!(has_print, "Effect in used lambda should be preserved");
}

#[test]
fn test_dead_code_channel_operations() {
    // Test channel operations are handled correctly
    let code = r#"
        (let ((ch (channel))
              (unused-ch (channel)))
          (chan-send! ch "message"))
    "#;
    
    let graph = parse(code).unwrap();
    let mut pass = DeadCodeEliminationPass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // Count channel creations
    let channel_count = optimized.nodes.values()
        .filter(|node| matches!(node, Node::Variable { name } if name == "channel"))
        .count();
    
    // Should have one channel (the used one)
    // The unused channel might be removed depending on whether channel creation
    // itself is considered an effect
    assert!(channel_count >= 1, "Used channel should be preserved");
}

#[test]
fn test_dead_code_async_await_chain() {
    // Test async/await chains
    let code = r#"
        (let ((promise (async (http-get "/api"))))
          (await promise))
    "#;
    
    let graph = parse(code).unwrap();
    let mut pass = DeadCodeEliminationPass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // Both async and await should be preserved
    let has_async = optimized.nodes.values().any(|node| {
        matches!(node, Node::Variable { name } if name == "async")
    });
    let has_await = optimized.nodes.values().any(|node| {
        matches!(node, Node::Variable { name } if name == "await")
    });
    
    assert!(has_async, "Async should be preserved");
    assert!(has_await, "Await should be preserved");
}

#[test]
fn test_dead_code_mixed_pure_and_effectful() {
    // Test mixed scenarios with both pure and effectful computations
    let code = r#"
        (let ((a (+ 1 2))           ; pure, unused
              (b (* 3 4))           ; pure, unused
              (c (print "effect"))  ; effectful, unused but preserved
              (d (+ 5 6))           ; pure, used
              (e (- 10 3)))         ; pure, unused
          (+ d 100))
    "#;
    
    let graph = parse(code).unwrap();
    let mut pass = DeadCodeEliminationPass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // Check what's preserved
    let has_print = optimized.nodes.values().any(|node| {
        matches!(node, Node::Variable { name } if name == "print")
    });
    
    // Count arithmetic operations
    let arithmetic_ops = optimized.nodes.values()
        .filter(|node| {
            matches!(node, Node::Variable { name } if 
                name == "+" || name == "*" || name == "-")
        })
        .count();
    
    assert!(has_print, "Print effect should be preserved");
    // Should have at most 2 arithmetic ops: the (+ 5 6) and the final (+ d 100)
    assert!(arithmetic_ops <= 2, "Unused pure computations should be removed");
}