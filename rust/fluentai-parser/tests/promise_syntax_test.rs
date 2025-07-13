use fluentai_parser::parse_flc;

#[test]
fn test_promise_block_syntax() {
    let source = r#"
        let p = promise {
            42
        };
        p
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Should parse as: let p = promise_new(() => 42)
    // Check that we have a let binding
    match &graph.nodes[&graph.root_id.unwrap()] {
        fluentai_core::ast::Node::Let { bindings, body } => {
            assert_eq!(bindings.len(), 1);
            assert_eq!(bindings[0].0, "p");
            
            // Check the binding value is an application of promise_new
            match &graph.nodes[&bindings[0].1] {
                fluentai_core::ast::Node::Application { function, args } => {
                    // Check it's calling promise_new
                    match &graph.nodes[function] {
                        fluentai_core::ast::Node::Variable { name } => {
                            assert_eq!(name, "promise_new");
                        }
                        _ => panic!("Expected promise_new variable"),
                    }
                    
                    // Check it has one argument (the lambda)
                    assert_eq!(args.len(), 1);
                    match &graph.nodes[&args[0]] {
                        fluentai_core::ast::Node::Lambda { params, body: _ } => {
                            assert_eq!(params.len(), 0); // No parameters
                        }
                        _ => panic!("Expected lambda argument"),
                    }
                }
                _ => panic!("Expected application node for promise_new"),
            }
        }
        _ => panic!("Expected let node at root"),
    }
}

#[test]
fn test_promise_all_syntax() {
    let source = r#"
        Promise.all([p1, p2, p3])
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Should parse as: promise_all([p1, p2, p3])
    match &graph.nodes[&graph.root_id.unwrap()] {
        fluentai_core::ast::Node::Application { function, args } => {
            // Check it's calling promise_all
            match &graph.nodes[function] {
                fluentai_core::ast::Node::Variable { name } => {
                    assert_eq!(name, "promise_all");
                }
                _ => panic!("Expected promise_all variable"),
            }
            
            // Check it has one argument (the list)
            assert_eq!(args.len(), 1);
            match &graph.nodes[&args[0]] {
                fluentai_core::ast::Node::List(items) => {
                    assert_eq!(items.len(), 3);
                }
                _ => panic!("Expected list argument"),
            }
        }
        _ => panic!("Expected application node at root"),
    }
}

#[test]
fn test_promise_race_syntax() {
    let source = r#"
        Promise.race([p1, p2])
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Should parse as: promise_race([p1, p2])
    match &graph.nodes[&graph.root_id.unwrap()] {
        fluentai_core::ast::Node::Application { function, args } => {
            // Check it's calling promise_race
            match &graph.nodes[function] {
                fluentai_core::ast::Node::Variable { name } => {
                    assert_eq!(name, "promise_race");
                }
                _ => panic!("Expected promise_race variable"),
            }
            
            // Check it has one argument (the list)
            assert_eq!(args.len(), 1);
        }
        _ => panic!("Expected application node at root"),
    }
}

#[test]
fn test_promise_timeout_syntax() {
    let source = r#"
        p.timeout(1000)
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Should parse as: with_timeout(p, 1000)
    match &graph.nodes[&graph.root_id.unwrap()] {
        fluentai_core::ast::Node::Application { function, args } => {
            // Check it's calling with_timeout
            match &graph.nodes[function] {
                fluentai_core::ast::Node::Variable { name } => {
                    assert_eq!(name, "with_timeout");
                }
                _ => panic!("Expected with_timeout variable"),
            }
            
            // Check it has two arguments
            assert_eq!(args.len(), 2);
            
            // First arg should be variable p
            match &graph.nodes[&args[0]] {
                fluentai_core::ast::Node::Variable { name } => {
                    assert_eq!(name, "p");
                }
                _ => panic!("Expected variable p"),
            }
            
            // Second arg should be 1000
            match &graph.nodes[&args[1]] {
                fluentai_core::ast::Node::Literal(fluentai_core::ast::Literal::Integer(n)) => {
                    assert_eq!(*n, 1000);
                }
                _ => panic!("Expected integer 1000"),
            }
        }
        _ => panic!("Expected application node at root"),
    }
}

#[test]
fn test_promise_chaining() {
    let source = r#"
        let p = promise { 42 };
        p.timeout(5000).await()
    "#;
    
    // Just verify it parses without error
    let graph = parse_flc(source).unwrap();
    assert!(graph.root_id.is_some());
}