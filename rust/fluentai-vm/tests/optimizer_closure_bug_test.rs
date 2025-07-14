use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_optimizer::OptimizationLevel;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, Opcode};

#[test]
fn test_optimizer_preserves_closures() {
    // Create a graph for: (let ((ch (chan))) (lambda () (send! ch 42)))
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

    // Create let binding: (let ((ch channel)) lambda)
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("ch".to_string(), channel)],
        body: lambda,
    }).unwrap();

    graph.root_id = Some(let_node);

    // Test 1: Compile without optimization
    let mut options = CompilerOptions::default();
    options.optimization_level = OptimizationLevel::None;
    let compiler = Compiler::with_options(options);
    let bytecode_no_opt = compiler.compile(&graph).unwrap();

    println!("=== Without optimization ===");
    for (i, chunk) in bytecode_no_opt.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }

    // Test 2: Compile with optimization
    let compiler_opt = Compiler::new(); // Uses default optimization
    let bytecode_opt = compiler_opt.compile(&graph).unwrap();

    println!("\n=== With optimization ===");
    for (i, chunk) in bytecode_opt.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }

    // Verify critical instructions are preserved
    let main_chunk_opt = &bytecode_opt.chunks[0];
    
    // Channel instruction should exist
    assert!(
        main_chunk_opt.instructions.iter().any(|i| i.opcode == Opcode::Channel),
        "Channel instruction was removed by optimizer"
    );
    
    // Should use MakeClosure, not MakeFunc
    assert!(
        main_chunk_opt.instructions.iter().any(|i| i.opcode == Opcode::MakeClosure),
        "MakeClosure was replaced with MakeFunc by optimizer"
    );
    
    // Lambda chunk should use LoadCaptured, not LoadGlobal
    if bytecode_opt.chunks.len() > 1 {
        let lambda_chunk = &bytecode_opt.chunks[1];
        assert!(
            lambda_chunk.instructions.iter().any(|i| i.opcode == Opcode::LoadCaptured),
            "LoadCaptured was replaced with LoadGlobal in lambda"
        );
    }
}

#[test]
fn test_optimizer_preserves_local_variables() {
    // Create a graph for: (let ((x 10)) x)
    let mut graph = Graph::new();

    let ten = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), ten)],
        body: x_var,
    }).unwrap();

    graph.root_id = Some(let_node);

    // Compile with optimization
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();

    println!("\n=== Local variable test ===");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }

    let main_chunk = &bytecode.chunks[0];
    
    // Should use LoadLocal, not LoadGlobal
    assert!(
        main_chunk.instructions.iter().any(|i| matches!(i.opcode, 
            Opcode::LoadLocal0 | Opcode::LoadLocal1 | Opcode::LoadLocal2 | 
            Opcode::LoadLocal3 | Opcode::LoadLocal
        )),
        "LoadLocal was replaced with LoadGlobal by optimizer"
    );
}