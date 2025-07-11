use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_core::value::Value;
use fluentai_vm::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
use fluentai_vm::compiler::Compiler;
use fluentai_vm::{VMBuilder, VM};

#[test]
fn test_stdlib_map_function() -> Result<()> {
    // Create AST directly instead of parsing
    let mut graph = Graph::new();

    // Create lambda: fn x -> x * 2
    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let mul_fn = graph
        .add_node(Node::Variable {
            name: "*".to_string(),
        })
        .expect("Failed to add node");
    let mul_app = graph
        .add_node(Node::Application {
            function: mul_fn,
            args: vec![x_var, two],
        })
        .expect("Failed to add node");
    let lambda = graph
        .add_node(Node::Lambda {
            params: vec!["x".to_string()],
            body: mul_app,
        })
        .expect("Failed to add node");

    // Create list [1, 2, 3]
    let list_items = vec![
        graph
            .add_node(Node::Literal(Literal::Integer(1)))
            .expect("Failed to add node"),
        graph
            .add_node(Node::Literal(Literal::Integer(2)))
            .expect("Failed to add node"),
        graph
            .add_node(Node::Literal(Literal::Integer(3)))
            .expect("Failed to add node"),
    ];
    let list = graph
        .add_node(Node::List(list_items))
        .expect("Failed to add node");

    // Create map application
    let map_var = graph
        .add_node(Node::Variable {
            name: "map".to_string(),
        })
        .expect("Failed to add node");
    let map_app = graph
        .add_node(Node::Application {
            function: map_var,
            args: vec![lambda, list],
        })
        .expect("Failed to add node");

    graph.root_id = Some(map_app);

    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;

    // Check that LoadGlobal for "map" is in the bytecode
    let has_map_global = bytecode.chunks.iter().any(|chunk| {
        chunk.constants.iter().any(|c| {
            if let Value::String(s) = c {
                s == "map"
            } else {
                false
            }
        })
    });

    assert!(has_map_global, "Expected 'map' to be loaded as a global");

    // Run VM
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Integer(2));
            assert_eq!(items[1], Value::Integer(4));
            assert_eq!(items[2], Value::Integer(6));
        }
        _ => panic!("Expected list result from map, got: {:?}", result),
    }

    Ok(())
}

#[test]
fn test_stdlib_filter_function() -> Result<()> {
    // Skip this test for now - focus on map first
    Ok(())
}

#[test]
fn test_stdlib_fold_function() -> Result<()> {
    // Skip this test for now - focus on map first
    Ok(())
}

#[test]
fn test_loadglobal_creates_stdlib_prefix() -> Result<()> {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));

    // Add "map" as a constant
    let map_idx = chunk.add_constant(Value::String("map".to_string()));

    // LoadGlobal instruction followed by Return (not Halt, which clears stack)
    chunk.add_instruction(Instruction::with_arg(Opcode::LoadGlobal, map_idx as u32));
    chunk.add_instruction(Instruction::new(Opcode::Return));

    let chunk_id = bytecode.add_chunk(chunk);
    bytecode.main_chunk = chunk_id;

    let mut vm = VM::new(bytecode);
    let result = vm.run()?;

    match result {
        Value::String(s) => {
            assert_eq!(
                s, "__stdlib__map",
                "Expected LoadGlobal to create __stdlib__ prefix for map"
            );
        }
        _ => panic!("Expected string value for map function, got: {:?}", result),
    }

    Ok(())
}

#[test]
fn test_direct_stdlib_call() -> Result<()> {
    let mut graph = Graph::new();

    // Create a simple function: fn x -> x * 2
    let x_param = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let mul_fn = graph
        .add_node(Node::Variable {
            name: "*".to_string(),
        })
        .expect("Failed to add node");
    let mul = graph
        .add_node(Node::Application {
            function: mul_fn,
            args: vec![x_param, two],
        })
        .expect("Failed to add node");
    let double_fn = graph
        .add_node(Node::Lambda {
            params: vec!["x".to_string()],
            body: mul,
        })
        .expect("Failed to add node");

    // Create a list [1, 2, 3]
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let three = graph
        .add_node(Node::Literal(Literal::Integer(3)))
        .expect("Failed to add node");
    let list = graph
        .add_node(Node::List(vec![one, two, three]))
        .expect("Failed to add node");

    // Call map(double_fn, list)
    let map_var = graph
        .add_node(Node::Variable {
            name: "map".to_string(),
        })
        .expect("Failed to add node");
    let map_call = graph
        .add_node(Node::Application {
            function: map_var,
            args: vec![double_fn, list],
        })
        .expect("Failed to add node");

    graph.root_id = Some(map_call);

    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;

    let mut vm = VM::new(bytecode);
    let result = vm.run()?;

    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Integer(2));
            assert_eq!(items[1], Value::Integer(4));
            assert_eq!(items[2], Value::Integer(6));
        }
        _ => panic!("Expected list result from map, got: {:?}", result),
    }

    Ok(())
}
