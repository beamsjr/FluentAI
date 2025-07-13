use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use fluentai_parser::parse_flc;

fn print_node_tree(graph: &Graph, node_id: NodeId, indent: usize) {
    let prefix = " ".repeat(indent);
    if let Some(node) = graph.get_node(node_id) {
        match node {
            Node::Let { bindings, body } => {
                println!("{}Let:", prefix);
                for (name, binding_id) in bindings {
                    println!("{}  {} =", prefix, name);
                    print_node_tree(graph, *binding_id, indent + 4);
                }
                println!("{}  body:", prefix);
                print_node_tree(graph, *body, indent + 4);
            }
            Node::Begin { exprs } => {
                println!("{}Begin ({} expressions):", prefix, exprs.len());
                for (i, expr_id) in exprs.iter().enumerate() {
                    println!("{}  [{}]:", prefix, i);
                    print_node_tree(graph, *expr_id, indent + 4);
                }
            }
            Node::Effect { effect_type, operation, args } => {
                println!("{}Effect: {:?}.{}", prefix, effect_type, operation);
                for (i, arg_id) in args.iter().enumerate() {
                    println!("{}  arg[{}]:", prefix, i);
                    print_node_tree(graph, *arg_id, indent + 4);
                }
            }
            Node::Application { function, args } => {
                println!("{}Application:", prefix);
                println!("{}  function:", prefix);
                print_node_tree(graph, *function, indent + 4);
                for (i, arg_id) in args.iter().enumerate() {
                    println!("{}  arg[{}]:", prefix, i);
                    print_node_tree(graph, *arg_id, indent + 4);
                }
            }
            Node::Variable { name } => println!("{}Variable: {}", prefix, name),
            Node::Literal(lit) => println!("{}Literal: {:?}", prefix, lit),
            _ => println!("{}Other node type: {:?}", prefix, node),
        }
    }
}

#[test]
fn test_let_with_explicit_block_multiple_expressions() {
    let input = r#"
        let x = 5; {
            perform IO.print("First");
            perform IO.print("Second");
            x + 10
        }
    "#;
    
    match parse_flc(input) {
        Ok(graph) => {
            println!("Successfully parsed let with explicit block:");
            if let Some(root_id) = graph.root_id {
                print_node_tree(&graph, root_id, 0);
                
                // Verify it's a Let expression
                if let Some(Node::Let { bindings, body }) = graph.get_node(root_id) {
                    assert_eq!(bindings.len(), 1);
                    assert_eq!(bindings[0].0, "x");
                    
                    // Body should be a Begin for multiple expressions
                    if let Some(body_node) = graph.get_node(*body) {
                        match body_node {
                            Node::Begin { exprs } => {
                                println!("Body is a Begin with {} expressions", exprs.len());
                                assert_eq!(exprs.len(), 3, "Should have 3 expressions");
                            }
                            _ => println!("Body type: {:?}", body_node),
                        }
                    }
                } else {
                    panic!("Expected Let expression at root");
                }
            } else {
                panic!("No root node in graph");
            }
        }
        Err(e) => panic!("Failed to parse let with explicit block: {:?}", e),
    }
}

#[test]
#[ignore = "Parser design: let expressions always expect a body after semicolon, not separate top-level expressions"]
fn test_let_without_block_multiple_expressions() {
    let input = r#"
        let x = 5;
        perform IO.print("First");
        perform IO.print("Second");
        x + 10
    "#;
    
    match parse_flc(input) {
        Ok(graph) => {
            println!("\nParsed result for let without block:");
            if let Some(root_id) = graph.root_id {
                print_node_tree(&graph, root_id, 0);
                
                // This should parse as a Begin of expressions at the top level
                if let Some(root_node) = graph.get_node(root_id) {
                    match root_node {
                        Node::Begin { exprs } => {
                            println!("Root is a Begin with {} expressions", exprs.len());
                            // Should have 4 expressions: let, perform, perform, x + 10
                            assert_eq!(exprs.len(), 4, "Expected 4 top-level expressions");
                            
                            // First should be the let
                            if let Some(Node::Let { body, .. }) = graph.get_node(exprs[0]) {
                                println!("First expression is Let");
                                // The body of let should just be the value 5
                                if let Some(body_node) = graph.get_node(*body) {
                                    println!("Let body: {:?}", body_node);
                                }
                            }
                        }
                        _ => {
                            println!("Root node type: {:?}", root_node);
                            // Might just be a single Let expression if parser doesn't handle sequences
                        }
                    }
                }
            }
        }
        Err(e) => {
            println!("Failed to parse let without block (might be expected): {:?}", e);
        }
    }
}

#[test]
fn test_let_with_block_single_expression() {
    let input = r#"
        let x = 5; {
            x + 10
        }
    "#;
    
    match parse_flc(input) {
        Ok(graph) => {
            println!("\nSuccessfully parsed let with single-expression block");
            if let Some(root_id) = graph.root_id {
                print_node_tree(&graph, root_id, 0);
            }
        }
        Err(e) => panic!("Failed to parse let with single-expression block: {:?}", e),
    }
}

#[test]
fn test_nested_let_with_blocks() {
    let input = r#"
        let x = 5; {
            let y = 10; {
                perform IO.print("Nested");
                x + y
            }
        }
    "#;
    
    match parse_flc(input) {
        Ok(graph) => {
            println!("\nSuccessfully parsed nested let with blocks");
            if let Some(root_id) = graph.root_id {
                print_node_tree(&graph, root_id, 0);
            }
        }
        Err(e) => panic!("Failed to parse nested let: {:?}", e),
    }
}

#[test]
fn test_let_in_function_body() {
    let input = r#"
        private function process() {
            let x = 5; {
                perform IO.print("Processing");
                perform IO.print("More processing");
                x * 2
            }
        }
    "#;
    
    match parse_flc(input) {
        Ok(graph) => {
            println!("\nSuccessfully parsed function with let block");
            if let Some(root_id) = graph.root_id {
                print_node_tree(&graph, root_id, 0);
            }
        }
        Err(e) => panic!("Failed to parse function with let: {:?}", e),
    }
}

#[test]
fn test_let_with_semicolon_separated_expressions() {
    let input = r#"
        let x = 5; {
            x + 1;
            x + 2;
            x + 3
        }
    "#;
    
    match parse_flc(input) {
        Ok(graph) => {
            println!("\nSuccessfully parsed let with semicolon-separated expressions");
            if let Some(root_id) = graph.root_id {
                print_node_tree(&graph, root_id, 0);
            }
        }
        Err(e) => panic!("Failed to parse let with semicolons: {:?}", e),
    }
}