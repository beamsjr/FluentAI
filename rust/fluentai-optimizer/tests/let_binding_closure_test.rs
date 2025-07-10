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
    
    // Check that the let binding still exists
    let let_node_opt = optimized.get_node(let_node);
    println!("Let node in optimized graph: {:?}", let_node_opt);
    
    // Check what happened to the root
    if let Some(root_id) = optimized.root_id {
        let root_node = optimized.get_node(root_id);
        println!("Root node in optimized graph: {:?}", root_node);
    }
    
    // Print all nodes to see what happened
    println!("\nAll nodes in optimized graph:");
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    assert!(let_node_opt.is_some(), "Let node was removed by optimizer");
    
    // Check that the channel node still exists
    let channel_opt = optimized.get_node(channel);
    assert!(channel_opt.is_some(), "Channel node was removed by optimizer");
    
    // Check the structure is preserved
    if let Some(Node::Let { bindings, body }) = let_node_opt {
        assert_eq!(bindings.len(), 1, "Let binding was modified");
        assert_eq!(bindings[0].0, "ch", "Let binding name was changed");
        assert_eq!(bindings[0].1, channel, "Let binding value was changed");
        assert_eq!(*body, sequence, "Let body was changed");
    } else {
        panic!("Let node type was changed: {:?}", let_node_opt);
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
        
        if let Some(let_node_after) = result.get_node(let_node) {
            if !matches!(let_node_after, Node::Let { .. }) {
                println!("  Let node type changed to: {:?}", let_node_after);
            }
        } else {
            println!("  Let node was removed!");
        }
        
        if result.get_node(channel).is_none() {
            println!("  Channel node was removed!");
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
    
    // Channel should still exist because it has side effects
    let channel_opt = optimized.get_node(channel);
    assert!(channel_opt.is_some(), "Channel with side effects was removed");
}