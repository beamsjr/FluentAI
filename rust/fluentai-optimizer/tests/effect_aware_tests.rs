//! Tests for effect-aware optimization pass

use fluentai_core::ast::{Graph, Node, Literal, EffectType};
use fluentai_optimizer::passes::{OptimizationPass, effect_aware::EffectAwarePass};

#[test]
fn test_hoist_pure_computations() {
    let mut graph = Graph::new();
    
    // Create a pure computation inside an effectful context
    // (let ((x (+ 1 2))      ; pure
    //       (y (io-read))    ; effectful
    //       (z (* 3 4)))     ; pure
    //   (+ x y z))
    
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    let three = graph.add_node(Node::Literal(Literal::Integer(3))).expect("Failed to add node");
    let four = graph.add_node(Node::Literal(Literal::Integer(4))).expect("Failed to add node");
    
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).expect("Failed to add node");
    let times = graph.add_node(Node::Variable { name: "*".to_string() }).expect("Failed to add node");
    
    // Pure computation: (+ 1 2)
    let x_value = graph.add_node(Node::Application {
        function: plus,
        args: vec![one, two],
    }).expect("Failed to add node");
    
    // Effectful computation: (io-read)
    let y_value = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "read".to_string(),
        args: vec![],
    }).expect("Failed to add node");
    
    // Pure computation: (* 3 4)
    let z_value = graph.add_node(Node::Application {
        function: times,
        args: vec![three, four],
    }).expect("Failed to add node");
    
    // Variables
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
    let y_var = graph.add_node(Node::Variable { name: "y".to_string() }).expect("Failed to add node");
    let z_var = graph.add_node(Node::Variable { name: "z".to_string() }).expect("Failed to add node");
    
    // Body: (+ x y z)
    let body1 = graph.add_node(Node::Application {
        function: plus,
        args: vec![x_var, y_var],
    }).expect("Failed to add node");
    let body = graph.add_node(Node::Application {
        function: plus,
        args: vec![body1, z_var],
    }).expect("Failed to add node");
    
    // Let expression
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("x".to_string(), x_value),
            ("y".to_string(), y_value),
            ("z".to_string(), z_value),
        ],
        body,
    }).expect("Failed to add node");
    
    graph.root_id = Some(let_node);
    
    // Run optimization
    let mut pass = EffectAwarePass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // Check that pure computations were hoisted
    let stats = pass.stats();
    assert!(stats.contains("1 pure computations hoisted"), "Expected pure computations to be hoisted: {}", stats);
}

#[test]
fn test_remove_duplicate_pure_expressions() {
    let mut graph = Graph::new();
    
    // Create duplicate pure expressions
    // (let ((a (* 2 3))
    //       (b (* 2 3))     ; duplicate
    //       (c (+ a b)))
    //   c)
    
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    let three = graph.add_node(Node::Literal(Literal::Integer(3))).expect("Failed to add node");
    let times = graph.add_node(Node::Variable { name: "*".to_string() }).expect("Failed to add node");
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).expect("Failed to add node");
    
    // First (* 2 3)
    let a_value = graph.add_node(Node::Application {
        function: times,
        args: vec![two, three],
    }).expect("Failed to add node");
    
    // Duplicate (* 2 3)
    let b_value = graph.add_node(Node::Application {
        function: times,
        args: vec![two, three],
    }).expect("Failed to add node");
    
    let a_var = graph.add_node(Node::Variable { name: "a".to_string() }).expect("Failed to add node");
    let b_var = graph.add_node(Node::Variable { name: "b".to_string() }).expect("Failed to add node");
    
    // (+ a b)
    let c_value = graph.add_node(Node::Application {
        function: plus,
        args: vec![a_var, b_var],
    }).expect("Failed to add node");
    
    let c_var = graph.add_node(Node::Variable { name: "c".to_string() }).expect("Failed to add node");
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("a".to_string(), a_value),
            ("b".to_string(), b_value),
            ("c".to_string(), c_value),
        ],
        body: c_var,
    }).expect("Failed to add node");
    
    graph.root_id = Some(let_node);
    
    // Run optimization
    let mut pass = EffectAwarePass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // Check that duplicates were removed
    let stats = pass.stats();
    assert!(stats.contains("duplicates removed"), "Expected duplicates to be removed: {}", stats);
}

#[test]
fn test_preserve_effect_ordering() {
    let mut graph = Graph::new();
    
    // Create effectful operations that must maintain order
    // (let ((a (io-write "first"))
    //       (b (io-write "second")))
    //   42)
    
    let first = graph.add_node(Node::Literal(Literal::String("first".to_string()))).expect("Failed to add node");
    let second = graph.add_node(Node::Literal(Literal::String("second".to_string()))).expect("Failed to add node");
    
    let a_value = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "write".to_string(),
        args: vec![first],
    }).expect("Failed to add node");
    
    let b_value = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "write".to_string(),
        args: vec![second],
    }).expect("Failed to add node");
    
    let result = graph.add_node(Node::Literal(Literal::Integer(42))).expect("Failed to add node");
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("a".to_string(), a_value),
            ("b".to_string(), b_value),
        ],
        body: result,
    }).expect("Failed to add node");
    
    graph.root_id = Some(let_node);
    
    let before_count = graph.nodes.len();
    
    // Run optimization
    let mut pass = EffectAwarePass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // Check that effect ordering is preserved
    assert_eq!(optimized.nodes.len(), before_count, "Effect ordering should be preserved");
    
    // Verify no effects were reordered
    let stats = pass.stats();
    assert!(stats.contains("0 effects reordered"), "Expected no effects to be reordered: {}", stats);
}

#[test]
fn test_mixed_pure_and_effectful() {
    let mut graph = Graph::new();
    
    // Mixed pure and effectful computations
    // (let ((pure1 (+ 1 2))
    //       (effect1 (io-read))
    //       (pure2 (* 3 4))
    //       (effect2 (state-get))
    //       (pure3 (- 5 6)))
    //   (+ pure1 effect1 pure2 effect2 pure3))
    
    let nums: Vec<_> = (1..=6).map(|i| 
        graph.add_node(Node::Literal(Literal::Integer(i as i64))).expect("Failed to add node")
    ).collect();
    
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).expect("Failed to add node");
    let times = graph.add_node(Node::Variable { name: "*".to_string() }).expect("Failed to add node");
    let minus = graph.add_node(Node::Variable { name: "-".to_string() }).expect("Failed to add node");
    
    // Pure computations
    let pure1_value = graph.add_node(Node::Application {
        function: plus,
        args: vec![nums[0], nums[1]],
    }).expect("Failed to add node");
    
    let pure2_value = graph.add_node(Node::Application {
        function: times,
        args: vec![nums[2], nums[3]],
    }).expect("Failed to add node");
    
    let pure3_value = graph.add_node(Node::Application {
        function: minus,
        args: vec![nums[4], nums[5]],
    }).expect("Failed to add node");
    
    // Effectful computations
    let effect1_value = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "read".to_string(),
        args: vec![],
    }).expect("Failed to add node");
    
    let effect2_value = graph.add_node(Node::Effect {
        effect_type: EffectType::State,
        operation: "get".to_string(),
        args: vec![],
    }).expect("Failed to add node");
    
    // Variables
    let vars: Vec<_> = ["pure1", "effect1", "pure2", "effect2", "pure3"]
        .iter()
        .map(|name| graph.add_node(Node::Variable { name: name.to_string() }).expect("Failed to add node"))
        .collect();
    
    // Body: add all values
    let mut body = vars[0];
    for i in 1..vars.len() {
        body = graph.add_node(Node::Application {
            function: plus,
            args: vec![body, vars[i]],
        }).expect("Failed to add node");
    }
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("pure1".to_string(), pure1_value),
            ("effect1".to_string(), effect1_value),
            ("pure2".to_string(), pure2_value),
            ("effect2".to_string(), effect2_value),
            ("pure3".to_string(), pure3_value),
        ],
        body,
    }).expect("Failed to add node");
    
    graph.root_id = Some(let_node);
    
    // Run optimization
    let mut pass = EffectAwarePass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // Check that some pure computations were hoisted
    let stats = pass.stats();
    assert!(stats.contains("pure computations hoisted"), "Expected pure computations to be hoisted: {}", stats);
}