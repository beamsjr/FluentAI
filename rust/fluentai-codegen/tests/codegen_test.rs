//! Integration tests for native code generation

use fluentai_codegen::{compile_to_native, CodegenOptions, OutputFormat};
use fluentai_core::ast::{Graph, Literal, Node};
use tempfile::TempDir;

/// Create a simple AST for testing
fn create_test_ast() -> Graph {
    let mut graph = Graph::new();
    
    // Create AST for: 40 + 2
    // In FLC, + is a function application
    let forty = graph.add_node(Node::Literal(Literal::Integer(40))).unwrap();
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
    let add = graph.add_node(Node::Application {
        function: plus,
        args: vec![forty, two],
    }).unwrap();
    
    graph.root_id = Some(add);
    graph
}

#[test]
fn test_compile_simple_expression() {
    let ast = create_test_ast();
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("test.o");
    
    let options = CodegenOptions {
        output_format: OutputFormat::ObjectFile,
        ..Default::default()
    };
    
    let result = compile_to_native(&ast, &output_path, options);
    
    // Should succeed in generating object file
    assert!(result.is_ok(), "Failed to compile: {:?}", result);
    
    // Check that output file was created
    assert!(output_path.exists(), "Output file not created");
    
    // Check that file is not empty
    let metadata = std::fs::metadata(&output_path).unwrap();
    assert!(metadata.len() > 0, "Output file is empty");
}

/// Test compilation of a more complex expression
fn create_complex_ast() -> Graph {
    let mut graph = Graph::new();
    
    // Create AST for: let x = 10; let y = 20; x + y
    let ten = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
    let twenty = graph.add_node(Node::Literal(Literal::Integer(20))).unwrap();
    
    let x_ref = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
    let y_ref = graph.add_node(Node::Variable { name: "y".to_string() }).unwrap();
    
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
    let add = graph.add_node(Node::Application {
        function: plus,
        args: vec![x_ref, y_ref],
    }).unwrap();
    
    // In FLC, Let uses bindings: Vec<(String, NodeId)>
    let inner_let = graph.add_node(Node::Let {
        bindings: vec![("y".to_string(), twenty)],
        body: add,
    }).unwrap();
    
    let let_x = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), ten)],
        body: inner_let,
    }).unwrap();
    
    graph.root_id = Some(let_x);
    graph
}

#[test]
fn test_compile_with_variables() {
    let ast = create_complex_ast();
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("test_vars.o");
    
    let options = CodegenOptions {
        output_format: OutputFormat::ObjectFile,
        ..Default::default()
    };
    
    let result = compile_to_native(&ast, &output_path, options);
    
    assert!(result.is_ok(), "Failed to compile with variables: {:?}", result);
    
    // Check that output file was created
    assert!(output_path.exists(), "Output file not created");
    
    // Check that file is not empty
    let metadata = std::fs::metadata(&output_path).unwrap();
    assert!(metadata.len() > 0, "Output file is empty");
}