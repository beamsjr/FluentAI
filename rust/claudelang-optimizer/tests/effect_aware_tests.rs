//! Tests for effect-aware optimization pass

use claudelang_core::ast::{Graph, Node, Literal, EffectType};
use claudelang_optimizer::passes::{OptimizationPass, effect_aware::EffectAwarePass};

#[test]
fn test_hoist_pure_computations() {
    let mut graph = Graph::new();
    
    // Create a pure computation inside an effectful context
    // (let ((x (+ 1 2))      ; pure
    //       (y (io-read))    ; effectful
    //       (z (* 3 4)))     ; pure
    //   (+ x y z))
    
    let one = graph.add_node(Node::Literal(Literal::Integer(1)));
    let two = graph.add_node(Node::Literal(Literal::Integer(2)));
    let three = graph.add_node(Node::Literal(Literal::Integer(3)));
    let four = graph.add_node(Node::Literal(Literal::Integer(4)));
    
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let times = graph.add_node(Node::Variable { name: "*".to_string() });
    
    // Pure computation: (+ 1 2)
    let x_value = graph.add_node(Node::Application {
        function: plus,
        args: vec![one, two],
    });
    
    // Effectful computation: (io-read)
    let y_value = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "read".to_string(),
        args: vec![],
    });
    
    // Pure computation: (* 3 4)
    let z_value = graph.add_node(Node::Application {
        function: times,
        args: vec![three, four],
    });
    
    // Variables
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var = graph.add_node(Node::Variable { name: "y".to_string() });
    let z_var = graph.add_node(Node::Variable { name: "z".to_string() });
    
    // Body: (+ x y z)
    let body1 = graph.add_node(Node::Application {
        function: plus,
        args: vec![x_var, y_var],
    });
    let body = graph.add_node(Node::Application {
        function: plus,
        args: vec![body1, z_var],
    });
    
    // Let expression
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("x".to_string(), x_value),
            ("y".to_string(), y_value),
            ("z".to_string(), z_value),
        ],
        body,
    });
    
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
    
    let two = graph.add_node(Node::Literal(Literal::Integer(2)));
    let three = graph.add_node(Node::Literal(Literal::Integer(3)));
    let times = graph.add_node(Node::Variable { name: "*".to_string() });
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    
    // First (* 2 3)
    let a_value = graph.add_node(Node::Application {
        function: times,
        args: vec![two, three],
    });
    
    // Duplicate (* 2 3)
    let b_value = graph.add_node(Node::Application {
        function: times,
        args: vec![two, three],
    });
    
    let a_var = graph.add_node(Node::Variable { name: "a".to_string() });
    let b_var = graph.add_node(Node::Variable { name: "b".to_string() });
    
    // (+ a b)
    let c_value = graph.add_node(Node::Application {
        function: plus,
        args: vec![a_var, b_var],
    });
    
    let c_var = graph.add_node(Node::Variable { name: "c".to_string() });
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("a".to_string(), a_value),
            ("b".to_string(), b_value),
            ("c".to_string(), c_value),
        ],
        body: c_var,
    });
    
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
    
    let first = graph.add_node(Node::Literal(Literal::String("first".to_string())));
    let second = graph.add_node(Node::Literal(Literal::String("second".to_string())));
    
    let a_value = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "write".to_string(),
        args: vec![first],
    });
    
    let b_value = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "write".to_string(),
        args: vec![second],
    });
    
    let result = graph.add_node(Node::Literal(Literal::Integer(42)));
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("a".to_string(), a_value),
            ("b".to_string(), b_value),
        ],
        body: result,
    });
    
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
        graph.add_node(Node::Literal(Literal::Integer(i as i64)))
    ).collect();
    
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let times = graph.add_node(Node::Variable { name: "*".to_string() });
    let minus = graph.add_node(Node::Variable { name: "-".to_string() });
    
    // Pure computations
    let pure1_value = graph.add_node(Node::Application {
        function: plus,
        args: vec![nums[0], nums[1]],
    });
    
    let pure2_value = graph.add_node(Node::Application {
        function: times,
        args: vec![nums[2], nums[3]],
    });
    
    let pure3_value = graph.add_node(Node::Application {
        function: minus,
        args: vec![nums[4], nums[5]],
    });
    
    // Effectful computations
    let effect1_value = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "read".to_string(),
        args: vec![],
    });
    
    let effect2_value = graph.add_node(Node::Effect {
        effect_type: EffectType::State,
        operation: "get".to_string(),
        args: vec![],
    });
    
    // Variables
    let vars: Vec<_> = ["pure1", "effect1", "pure2", "effect2", "pure3"]
        .iter()
        .map(|name| graph.add_node(Node::Variable { name: name.to_string() }))
        .collect();
    
    // Body: add all values
    let mut body = vars[0];
    for i in 1..vars.len() {
        body = graph.add_node(Node::Application {
            function: plus,
            args: vec![body, vars[i]],
        });
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
    });
    
    graph.root_id = Some(let_node);
    
    // Run optimization
    let mut pass = EffectAwarePass::new();
    let optimized = pass.run(&graph).unwrap();
    
    // Check that some pure computations were hoisted
    let stats = pass.stats();
    assert!(stats.contains("pure computations hoisted"), "Expected pure computations to be hoisted: {}", stats);
}