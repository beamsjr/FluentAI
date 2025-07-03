//! Basic tests for the visualization system

use claudelang_viz::{
    debug::{DebugEvent, debug_channel, serialize_value, timestamp_micros},
    layout::ASTLayouter,
};
use claudelang_core::ast::{Graph, Node, Literal};
use claudelang_vm::bytecode::Value;

#[test]
fn test_debug_channel() {
    let (tx, mut rx) = debug_channel();
    
    let event = DebugEvent::Started {
        timestamp: timestamp_micros(),
    };
    
    tx.send(event.clone()).unwrap();
    
    let received = rx.try_recv().unwrap();
    matches!(received, DebugEvent::Started { .. });
}

#[test]
fn test_value_serialization() {
    assert_eq!(serialize_value(&Value::Nil), "nil");
    assert_eq!(serialize_value(&Value::Bool(true)), "true");
    assert_eq!(serialize_value(&Value::Int(42)), "42");
    assert_eq!(serialize_value(&Value::Float(3.14)), "3.14");
    assert_eq!(serialize_value(&Value::String("hello".to_string())), "\"hello\"");
    
    let list = Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
    assert_eq!(serialize_value(&list), "[1, 2, 3]");
}

#[test]
fn test_ast_layout() {
    let mut graph = Graph::new();
    
    // Create a simple AST: (+ 1 2)
    let one = graph.add_node(Node::Literal(Literal::Integer(1)));
    let two = graph.add_node(Node::Literal(Literal::Integer(2)));
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let app = graph.add_node(Node::Application {
        function: plus,
        args: vec![one, two],
    });
    
    graph.root_id = Some(app);
    
    let layouter = ASTLayouter::default();
    let layout = layouter.layout(&graph);
    
    assert_eq!(layout.nodes.len(), 4);
    assert_eq!(layout.edges.len(), 3);
    assert!(layout.width > 0.0);
    assert!(layout.height > 0.0);
}

#[test]
fn test_server_config() {
    use claudelang_viz::ServerConfig;
    use std::path::PathBuf;
    
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: 8080,
        static_dir: PathBuf::from("static"),
    };
    
    assert_eq!(config.host, "127.0.0.1");
    assert_eq!(config.port, 8080);
}