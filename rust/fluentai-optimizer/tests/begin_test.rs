use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use fluentai_optimizer::graph_optimizer::GraphOptimizer;

#[test]
fn test_let_with_begin_body() {
    // Test case: (let ((x 42)) (begin (print x) x))
    // The print effect should be preserved even though it's inside a Begin
    let mut graph = Graph::new();
    
    // Create the literal 42
    let lit_42 = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    
    // Create variable x
    let var_x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
    
    // Create print function variable
    let print_var = graph.add_node(Node::Variable { name: "print".to_string() }).unwrap();
    
    // Create (print x) - this is an effect
    let print_x = graph.add_node(Node::Application {
        function: print_var,
        args: vec![var_x],
    }).unwrap();
    
    // Create the Begin node with (print x) and x
    let begin_body = graph.add_node(Node::Begin {
        exprs: vec![print_x, var_x],
    }).unwrap();
    
    // Create the let node
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), lit_42)],
        body: begin_body,
    }).unwrap();
    
    graph.root_id = Some(let_node);
    
    // Optimize the graph
    let mut optimizer = GraphOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();
    
    // The print effect should still be present in the optimized graph
    let mut has_print_effect = false;
    for (_, node) in &optimized.nodes {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = optimized.get_node(*function) {
                if name == "print" {
                    has_print_effect = true;
                    break;
                }
            }
        }
    }
    
    assert!(has_print_effect, "Print effect was eliminated when it should have been preserved");
}

#[test]
fn test_letrec_with_begin_body() {
    // Test case: (letrec ((fact (lambda (n) ...))) (begin (print "Starting") (fact 5)))
    let mut graph = Graph::new();
    
    // Create string literal
    let str_lit = graph.add_node(Node::Literal(Literal::String("Starting".to_string()))).unwrap();
    
    // Create print function variable
    let print_var = graph.add_node(Node::Variable { name: "print".to_string() }).unwrap();
    
    // Create (print "Starting")
    let print_starting = graph.add_node(Node::Application {
        function: print_var,
        args: vec![str_lit],
    }).unwrap();
    
    // Create fact variable
    let fact_var = graph.add_node(Node::Variable { name: "fact".to_string() }).unwrap();
    
    // Create literal 5
    let lit_5 = graph.add_node(Node::Literal(Literal::Integer(5))).unwrap();
    
    // Create (fact 5)
    let fact_5 = graph.add_node(Node::Application {
        function: fact_var,
        args: vec![lit_5],
    }).unwrap();
    
    // Create Begin body
    let begin_body = graph.add_node(Node::Begin {
        exprs: vec![print_starting, fact_5],
    }).unwrap();
    
    // Create a dummy lambda for fact (simplified)
    let n_param = graph.add_node(Node::Variable { name: "n".to_string() }).unwrap();
    let fact_lambda = graph.add_node(Node::Lambda {
        params: vec!["n".to_string()],
        body: n_param, // Simplified body
    }).unwrap();
    
    // Create letrec node
    let letrec_node = graph.add_node(Node::Letrec {
        bindings: vec![("fact".to_string(), fact_lambda)],
        body: begin_body,
    }).unwrap();
    
    graph.root_id = Some(letrec_node);
    
    // Optimize the graph
    let mut optimizer = GraphOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();
    
    // The print effect should still be present
    let mut has_print_effect = false;
    for (_, node) in &optimized.nodes {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = optimized.get_node(*function) {
                if name == "print" {
                    has_print_effect = true;
                    break;
                }
            }
        }
    }
    
    assert!(has_print_effect, "Print effect in Begin was eliminated when it should have been preserved");
}

#[test]
fn test_begin_with_multiple_effects() {
    // Test that all effects in a Begin are preserved
    let mut graph = Graph::new();
    
    // Create effect nodes
    let print_var = graph.add_node(Node::Variable { name: "print".to_string() }).unwrap();
    let write_var = graph.add_node(Node::Variable { name: "write-file".to_string() }).unwrap();
    
    let str1 = graph.add_node(Node::Literal(Literal::String("First".to_string()))).unwrap();
    let str2 = graph.add_node(Node::Literal(Literal::String("Second".to_string()))).unwrap();
    
    let print1 = graph.add_node(Node::Application {
        function: print_var,
        args: vec![str1],
    }).unwrap();
    
    let write1 = graph.add_node(Node::Application {
        function: write_var,
        args: vec![str2],
    }).unwrap();
    
    let result = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    
    // Create Begin with multiple effects
    let begin_node = graph.add_node(Node::Begin {
        exprs: vec![print1, write1, result],
    }).unwrap();
    
    // Wrap in a let to test the optimizer's handling
    let let_node = graph.add_node(Node::Let {
        bindings: vec![],
        body: begin_node,
    }).unwrap();
    
    graph.root_id = Some(let_node);
    
    // Optimize
    let mut optimizer = GraphOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();
    
    // Count effects
    let mut effect_count = 0;
    for (_, node) in &optimized.nodes {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = optimized.get_node(*function) {
                if name == "print" || name == "write-file" {
                    effect_count += 1;
                }
            }
        }
    }
    
    assert_eq!(effect_count, 2, "Not all effects in Begin were preserved");
}