use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};

#[test]
fn test_optimizer_preserves_let_bindings_with_closures() {
    // Create the same graph as in the spawn test
    let mut graph = Graph::new();

    // Create channel: (chan)
    let channel = graph.add_node(Node::Channel { capacity: None }).unwrap();

    // Create lambda: (lambda () (send! ch 42))
    let ch_var = graph.add_node(Node::Variable { name: "ch".to_string() }).unwrap();
    let value = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let send = graph.add_node(Node::Send { channel: ch_var, value }).unwrap();
    let lambda = graph.add_node(Node::Lambda {
        params: vec![],
        body: send,
    }).unwrap();

    // Create spawn: (spawn lambda)
    let spawn = graph.add_node(Node::Spawn { expr: lambda }).unwrap();

    // Create receive: (receive! ch)
    let ch_var2 = graph.add_node(Node::Variable { name: "ch".to_string() }).unwrap();
    let receive = graph.add_node(Node::Receive { channel: ch_var2 }).unwrap();

    // Create sequence: spawn then receive
    let sequence = graph.add_node(Node::List(vec![spawn, receive])).unwrap();

    // Create let binding: (let ((ch channel)) sequence)
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("ch".to_string(), channel)],
        body: sequence,
    }).unwrap();

    graph.root_id = Some(let_node);

    // Apply standard optimization
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&graph).unwrap();

    // Debug: print the optimized graph
    println!("Original root: {:?}", graph.root_id);
    println!("Optimized root: {:?}", optimized.root_id);
    
    // Check the root is still a Let node
    assert!(optimized.root_id.is_some(), "Optimized graph has no root");
    let root_id = optimized.root_id.unwrap();
    let root_node = optimized.get_node(root_id).expect("Root node not found");
    
    println!("Root node in optimized graph: {:?}", root_node);
    
    // Print all nodes to see what happened
    println!("\nAll nodes in optimized graph:");
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    // Check the structure is preserved at the root
    match root_node {
        Node::Let { bindings, body } => {
            assert_eq!(bindings.len(), 1, "Let binding count was modified");
            assert_eq!(bindings[0].0, "ch", "Let binding name was changed");
            
            // Check that the bound value is a Channel
            let channel_id = bindings[0].1;
            let channel_node = optimized.get_node(channel_id).expect("Channel node not found");
            assert!(matches!(channel_node, Node::Channel { .. }), "Let binding value is not a Channel");
            
            // Check that the body contains spawn and receive
            let body_node = optimized.get_node(*body).expect("Let body not found");
            if let Node::List(items) = body_node {
                assert_eq!(items.len(), 2, "Body should have 2 items (spawn and receive)");
                
                // Check first item is a spawn
                let spawn_node = optimized.get_node(items[0]).expect("Spawn node not found");
                assert!(matches!(spawn_node, Node::Spawn { .. }), "First item should be Spawn");
                
                // Check second item is a receive
                let receive_node = optimized.get_node(items[1]).expect("Receive node not found");
                assert!(matches!(receive_node, Node::Receive { .. }), "Second item should be Receive");
            } else {
                panic!("Let body should be a List, got: {:?}", body_node);
            }
        }
        _ => panic!("Root should be a Let node, got: {:?}", root_node),
    }
}

#[test]
fn test_individual_passes_on_let_binding() {
    use fluentai_optimizer::passes::*;
    
    // Create the same graph
    let mut graph = Graph::new();
    let channel = graph.add_node(Node::Channel { capacity: None }).unwrap();
    let ch_var = graph.add_node(Node::Variable { name: "ch".to_string() }).unwrap();
    let value = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let send = graph.add_node(Node::Send { channel: ch_var, value }).unwrap();
    let lambda = graph.add_node(Node::Lambda { params: vec![], body: send }).unwrap();
    let spawn = graph.add_node(Node::Spawn { expr: lambda }).unwrap();
    let ch_var2 = graph.add_node(Node::Variable { name: "ch".to_string() }).unwrap();
    let receive = graph.add_node(Node::Receive { channel: ch_var2 }).unwrap();
    let sequence = graph.add_node(Node::List(vec![spawn, receive])).unwrap();
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("ch".to_string(), channel)],
        body: sequence,
    }).unwrap();
    graph.root_id = Some(let_node);
    
    // Test each pass individually
    let passes: Vec<(&str, Box<dyn OptimizationPass>)> = vec![
        ("constant_folding", Box::new(constant_folding::ConstantFoldingPass::new())),
        ("dead_code", Box::new(dead_code::DeadCodeEliminationPass::new())),
        ("cse", Box::new(cse::CommonSubexpressionEliminationPass::new())),
        ("inline", Box::new(inline::InlinePass::new(10))),
        ("tail_call", Box::new(tail_call::TailCallOptimizationPass::new())),
        ("beta_reduction", Box::new(beta_reduction::BetaReductionPass::new())),
        ("effect_aware", Box::new(effect_aware::EffectAwarePass::new())),
    ];
    
    for (name, mut pass) in passes {
        println!("\nTesting pass: {}", name);
        let result = pass.run(&graph).unwrap();
        
        if result.root_id != graph.root_id {
            println!("  Root changed from {:?} to {:?}", graph.root_id, result.root_id);
        }
        
        // Check if the root is still a Let node
        if let Some(root_id) = result.root_id {
            if let Some(root_node) = result.get_node(root_id) {
                if !matches!(root_node, Node::Let { .. }) {
                    println!("  Root is no longer a Let node: {:?}", root_node);
                } else if let Node::Let { bindings, .. } = root_node {
                    if bindings.len() != 1 || bindings[0].0 != "ch" {
                        println!("  Let bindings were modified: {:?}", bindings);
                    }
                    // Check that the channel still exists (might have new ID)
                    let channel_id = bindings[0].1;
                    if let Some(channel_node) = result.get_node(channel_id) {
                        if !matches!(channel_node, Node::Channel { .. }) {
                            println!("  Channel node type changed: {:?}", channel_node);
                        }
                    } else {
                        println!("  Channel node was removed!");
                    }
                }
            }
        } else {
            println!("  Root was removed!");
        }
    }
}

#[test]
fn test_optimizer_preserves_channel_side_effects() {
    let mut graph = Graph::new();
    
    // Just a channel in a let binding that's "unused"
    let channel = graph.add_node(Node::Channel { capacity: None }).unwrap();
    let nil = graph.add_node(Node::Literal(Literal::Nil)).unwrap();
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("unused_ch".to_string(), channel)],
        body: nil,
    }).unwrap();
    
    graph.root_id = Some(let_node);
    
    // Apply optimization
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&graph).unwrap();
    
    // The root should still be a Let node
    assert!(optimized.root_id.is_some(), "Optimized graph has no root");
    let root_node = optimized.get_node(optimized.root_id.unwrap()).expect("Root node not found");
    
    // Check the structure is preserved
    match root_node {
        Node::Let { bindings, body } => {
            assert_eq!(bindings.len(), 1, "Let binding count was modified");
            assert_eq!(bindings[0].0, "unused_ch", "Let binding name was changed");
            
            // Check that the bound value is a Channel
            let channel_id = bindings[0].1;
            let channel_node = optimized.get_node(channel_id).expect("Channel node not found");
            assert!(matches!(channel_node, Node::Channel { .. }), "Channel with side effects was removed");
            
            // Check that the body is still Nil
            let body_node = optimized.get_node(*body).expect("Let body not found");
            assert!(matches!(body_node, Node::Literal(Literal::Nil)), "Let body should be Nil");
        }
        _ => panic!("Root should be a Let node, got: {:?}", root_node),
    }
}