use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_parser::parse;
use fluentai_vm::{Compiler, VM, VMBuilder};
use fluentai_effects::runtime::EffectRuntime;
use std::sync::Arc;

#[tokio::test]
async fn test_basic_await() {
    // Test that await properly blocks until promise resolves
    let code = r#"{
    let f = () => 42;
    spawn { f() }.await()
}"#;

    let graph = parse(code).unwrap();
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    // Use from_current since we're already in a tokio runtime from #[tokio::test]
    let effect_runtime = Arc::new(EffectRuntime::from_current());
    let mut vm = VMBuilder::new()
        .with_bytecode(bytecode)
        .with_effect_runtime(effect_runtime)
        .build()
        .unwrap();
    
    let result = vm.run();
    
    if let Err(e) = &result {
        eprintln!("VM error: {:?}", e);
    }
    
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), fluentai_core::value::Value::Integer(42));
}

#[tokio::test]
async fn test_await_with_computation() {
    // Test await with actual async computation
    let code = r#"{
    let compute = () => 10 + 20;
    let result = spawn { compute() }.await();
    result
}"#;

    let graph = parse(code).unwrap();
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    // Use from_current since we're already in a tokio runtime from #[tokio::test]
    let effect_runtime = Arc::new(EffectRuntime::from_current());
    let mut vm = VMBuilder::new()
        .with_bytecode(bytecode)
        .with_effect_runtime(effect_runtime)
        .build()
        .unwrap();
    
    let result = vm.run();
    
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), fluentai_core::value::Value::Integer(30));
}

#[tokio::test]
async fn test_multiple_awaits() {
    // Test multiple awaits in sequence
    let code = r#"{
    let f1 = () => 10;
    let f2 = () => 20;
    let r1 = spawn { f1() }.await();
    let r2 = spawn { f2() }.await();
    r1 + r2
}"#;

    let graph = parse(code).unwrap();
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    // Use from_current since we're already in a tokio runtime from #[tokio::test]
    let effect_runtime = Arc::new(EffectRuntime::from_current());
    let mut vm = VMBuilder::new()
        .with_bytecode(bytecode)
        .with_effect_runtime(effect_runtime)
        .build()
        .unwrap();
    
    let result = vm.run();
    
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), fluentai_core::value::Value::Integer(30));
}

#[test]
fn test_await_without_runtime() {
    // Test that await fails gracefully without runtime when promise not ready
    let mut graph = Graph::new();
    
    // Create a simple lambda that returns 42
    let body = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    
    let func = graph.add_node(Node::Lambda {
        params: vec![],
        body,
    }).unwrap();
    
    // Spawn and await
    let spawn_node = graph.add_node(Node::Spawn {
        expr: func,
    }).unwrap();
    
    let await_node = graph.add_node(Node::Await {
        expr: spawn_node,
    }).unwrap();
    
    // Create a begin node as root
    let root = graph.add_node(Node::Begin {
        exprs: vec![await_node],
    }).unwrap();
    
    // Set the root using the correct Graph API
    graph.root_id = Some(root);
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    // Create VM without async runtime
    let mut vm = VM::new(bytecode);
    let result = vm.run();
    
    // Should fail since we don't have a runtime and promise won't be ready
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("no async runtime"));
}