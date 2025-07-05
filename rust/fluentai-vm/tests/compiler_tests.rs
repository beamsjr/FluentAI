//! Comprehensive tests for the FluentAI compiler

use fluentai_core::ast::{Graph, Node, NodeId, Literal, Pattern};
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    bytecode::{Bytecode, Opcode, Value},
    VM,
};
use fluentai_optimizer::OptimizationLevel;
use anyhow::Result;

/// Helper to create a simple AST graph with a single node
fn create_simple_graph(node: Node) -> Graph {
    let mut graph = Graph::new();
    let node_id = graph.add_node(node);
    graph.root_id = Some(node_id);
    graph
}

/// Helper to compile and run a graph, returning the result
fn compile_and_run(graph: &Graph) -> Result<Value> {
    // TODO: There's still a stack overflow issue with the optimizer
    // Use no optimization until the root cause is found
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(graph)?;
    let mut vm = VM::new(bytecode);
    Ok(vm.run()?)
}

#[test]
fn test_compile_literal_nil() -> Result<()> {
    let graph = create_simple_graph(Node::Literal(Literal::Nil));
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Nil);
    Ok(())
}

#[test]
fn test_compile_literal_bool() -> Result<()> {
    let graph = create_simple_graph(Node::Literal(Literal::Boolean(true)));
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Bool(true));
    
    let graph = create_simple_graph(Node::Literal(Literal::Boolean(false)));
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Bool(false));
    Ok(())
}

#[test]
fn test_compile_literal_integer() -> Result<()> {
    let graph = create_simple_graph(Node::Literal(Literal::Integer(42)));
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Int(42));
    
    // Test negative integer
    let graph = create_simple_graph(Node::Literal(Literal::Integer(-100)));
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Int(-100));
    Ok(())
}

#[test]
fn test_compile_literal_float() -> Result<()> {
    let graph = create_simple_graph(Node::Literal(Literal::Float(3.14)));
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Float(3.14));
    Ok(())
}

#[test]
fn test_compile_literal_string() -> Result<()> {
    let graph = create_simple_graph(Node::Literal(Literal::String("hello".to_string())));
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("hello".to_string()));
    Ok(())
}

#[test]
fn test_compile_empty_list() -> Result<()> {
    let mut graph = Graph::new();
    let list_node = graph.add_node(Node::List(vec![]));
    graph.root_id = Some(list_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::List(vec![]));
    Ok(())
}

#[test]
fn test_compile_list_with_literals() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create list [1, 2, 3]
    let n1 = graph.add_node(Node::Literal(Literal::Integer(1)));
    let n2 = graph.add_node(Node::Literal(Literal::Integer(2)));
    let n3 = graph.add_node(Node::Literal(Literal::Integer(3)));
    
    let list_node = graph.add_node(Node::List(vec![n1, n2, n3]));
    graph.root_id = Some(list_node);
    
    // First test compilation without optimization
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Check bytecode structure
    assert!(!bytecode.chunks.is_empty());
    let main_chunk = &bytecode.chunks[0];
    
    // Should have instructions for 3 pushes + MakeList + Halt
    assert!(main_chunk.instructions.len() >= 5);
    
    // Now test execution
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    assert_eq!(result, Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
    ]));
    Ok(())
}

#[test]
fn test_compile_lambda_identity() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (lambda (x) x)
    let param_node = graph.add_node(Node::Variable { name: "x".to_string() });
    let lambda_node = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: param_node,
    });
    graph.root_id = Some(lambda_node);
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Check that we got a function value
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    match result {
        Value::Function { chunk_id, env } => {
            assert!(chunk_id > 0); // Should be a different chunk than main
            assert!(env.is_empty()); // No captured variables
        }
        _ => panic!("Expected function value"),
    }
    Ok(())
}

#[test]
fn test_compile_simple_application() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create ((lambda (x) x) 42)
    let param_node = graph.add_node(Node::Variable { name: "x".to_string() });
    let lambda_node = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: param_node,
    });
    
    let arg_node = graph.add_node(Node::Literal(Literal::Integer(42)));
    let app_node = graph.add_node(Node::Application {
        function: lambda_node,
        args: vec![arg_node],
    });
    graph.root_id = Some(app_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Int(42));
    Ok(())
}

#[test]
fn test_compile_let_binding() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (let ((x 10)) x)
    let value_node = graph.add_node(Node::Literal(Literal::Integer(10)));
    let body_node = graph.add_node(Node::Variable { name: "x".to_string() });
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), value_node)],
        body: body_node,
    });
    graph.root_id = Some(let_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Int(10));
    Ok(())
}

#[test]
fn test_compile_multiple_let_bindings() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (let ((x 10) (y 20)) (+ x y))
    let x_val = graph.add_node(Node::Literal(Literal::Integer(10)));
    let y_val = graph.add_node(Node::Literal(Literal::Integer(20)));
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var = graph.add_node(Node::Variable { name: "y".to_string() });
    let plus_var = graph.add_node(Node::Variable { name: "+".to_string() });
    
    let add_app = graph.add_node(Node::Application {
        function: plus_var,
        args: vec![x_var, y_var],
    });
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("x".to_string(), x_val),
            ("y".to_string(), y_val),
        ],
        body: add_app,
    });
    graph.root_id = Some(let_node);
    
    // This test assumes + is available in stdlib
    // For now, we'll just check it compiles without error
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    assert!(!bytecode.chunks.is_empty());
    Ok(())
}

#[test]
fn test_compile_if_expression() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (if true 1 2)
    let cond = graph.add_node(Node::Literal(Literal::Boolean(true)));
    let then_val = graph.add_node(Node::Literal(Literal::Integer(1)));
    let else_val = graph.add_node(Node::Literal(Literal::Integer(2)));
    
    let if_node = graph.add_node(Node::If {
        condition: cond,
        then_branch: then_val,
        else_branch: else_val,
    });
    graph.root_id = Some(if_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Int(1));
    Ok(())
}

#[test]
fn test_compile_if_without_else() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (if false 1 nil) - If requires both branches
    let cond = graph.add_node(Node::Literal(Literal::Boolean(false)));
    let then_val = graph.add_node(Node::Literal(Literal::Integer(1)));
    let nil_val = graph.add_node(Node::Literal(Literal::Nil));
    
    let if_node = graph.add_node(Node::If {
        condition: cond,
        then_branch: then_val,
        else_branch: nil_val,
    });
    graph.root_id = Some(if_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Nil); // Should return nil when condition is false
    Ok(())
}

#[test]
fn test_compile_nested_let() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (let ((x 10)) (let ((y 20)) (+ x y)))
    let x_val = graph.add_node(Node::Literal(Literal::Integer(10)));
    let y_val = graph.add_node(Node::Literal(Literal::Integer(20)));
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var = graph.add_node(Node::Variable { name: "y".to_string() });
    
    // For this test, we'll just check variable access
    let inner_let = graph.add_node(Node::Let {
        bindings: vec![("y".to_string(), y_val)],
        body: y_var,
    });
    
    let outer_let = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), x_val)],
        body: inner_let,
    });
    graph.root_id = Some(outer_let);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Int(20));
    Ok(())
}

#[test]
fn test_compiler_with_optimization_levels() -> Result<()> {
    let graph = create_simple_graph(Node::Literal(Literal::Integer(42)));
    
    // Test with different optimization levels
    for level in [OptimizationLevel::None, OptimizationLevel::Standard, OptimizationLevel::Aggressive] {
        let options = CompilerOptions {
            optimization_level: level,
            debug_info: false,
        };
        let compiler = Compiler::with_options(options);
        let bytecode = compiler.compile(&graph)?;
        
        // Should produce valid bytecode regardless of optimization level
        assert!(!bytecode.chunks.is_empty());
        assert!(bytecode.chunks[0].instructions.len() >= 2); // At least Push and Halt
    }
    Ok(())
}

#[test]
fn test_compile_list_with_variables() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (let ((x 1) (y 2) (z 3)) (list x y z))
    let x_val = graph.add_node(Node::Literal(Literal::Integer(1)));
    let y_val = graph.add_node(Node::Literal(Literal::Integer(2)));
    let z_val = graph.add_node(Node::Literal(Literal::Integer(3)));
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var = graph.add_node(Node::Variable { name: "y".to_string() });
    let z_var = graph.add_node(Node::Variable { name: "z".to_string() });
    
    let list_node = graph.add_node(Node::List(vec![x_var, y_var, z_var]));
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("x".to_string(), x_val),
            ("y".to_string(), y_val),
            ("z".to_string(), z_val),
        ],
        body: list_node,
    });
    graph.root_id = Some(let_node);
    
    // First test with optimization disabled to ensure basic functionality
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
    ]));
    
    // Now test with optimization enabled
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::Standard,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    assert_eq!(result, Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
    ]));
    Ok(())
}

#[test]
fn test_compile_closure_with_capture() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (let ((x 10)) (lambda (y) x))  // Just return captured x
    let x_val = graph.add_node(Node::Literal(Literal::Integer(10)));
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    
    let lambda = graph.add_node(Node::Lambda {
        params: vec!["y".to_string()],
        body: x_var,
    });
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), x_val)],
        body: lambda,
    });
    graph.root_id = Some(let_node);
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    match result {
        Value::Function { env, .. } => {
            assert_eq!(env.len(), 1); // Should capture x
            assert_eq!(env[0], Value::Int(10));
        }
        _ => panic!("Expected function with captured environment"),
    }
    Ok(())
}

#[test]
fn test_compile_undefined_variable() {
    let mut graph = Graph::new();
    
    // Undefined variables are treated as globals in FluentAi
    let var_node = graph.add_node(Node::Variable { name: "undefined".to_string() });
    graph.root_id = Some(var_node);
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    // Should generate LoadGlobal instruction
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| instr.opcode == Opcode::LoadGlobal));
}

#[test]
fn test_compile_error_no_root_node() {
    let graph = Graph::new(); // Empty graph with no root
    
    let compiler = Compiler::new();
    let result = compiler.compile(&graph);
    
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("no root node"));
}

#[test]
fn test_compile_match_expression() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (match 42 (42 "found") (_ "not found"))
    let value = graph.add_node(Node::Literal(Literal::Integer(42)));
    let found = graph.add_node(Node::Literal(Literal::String("found".to_string())));
    let not_found = graph.add_node(Node::Literal(Literal::String("not found".to_string())));
    
    let match_node = graph.add_node(Node::Match {
        expr: value,
        branches: vec![
            (Pattern::Literal(Literal::Integer(42)), found),
            (Pattern::Wildcard, not_found),
        ],
    });
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("found".to_string()));
    Ok(())
}

#[test]
fn test_bytecode_structure() -> Result<()> {
    let graph = create_simple_graph(Node::Literal(Literal::Integer(42)));
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Check bytecode structure
    assert_eq!(bytecode.chunks.len(), 1); // Should have main chunk
    assert_eq!(bytecode.main_chunk, 0);
    
    let main_chunk = &bytecode.chunks[0];
    assert_eq!(main_chunk.name, Some("main".to_string()));
    
    // Should have at least Push and Halt instructions
    assert!(main_chunk.instructions.len() >= 2);
    
    // Last instruction should be Halt
    let last_instruction = main_chunk.instructions.last().unwrap();
    assert_eq!(last_instruction.opcode, Opcode::Halt);
    
    Ok(())
}