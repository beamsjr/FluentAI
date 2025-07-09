//! Additional tests to improve compiler coverage

use anyhow::Result;
use fluentai_core::ast::{EffectType, ExportItem, Graph, ImportItem, Literal, Node};
use fluentai_core::value::Value;
use fluentai_optimizer::OptimizationLevel;
use fluentai_vm::{
    bytecode::Opcode,
    compiler::{Compiler, CompilerOptions},
    VM,
};

fn compile_and_check_opcodes(graph: &Graph, expected_opcodes: &[Opcode]) -> Result<()> {
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(graph)?;

    let main_chunk = &bytecode.chunks[0];
    for opcode in expected_opcodes {
        assert!(
            main_chunk
                .instructions
                .iter()
                .any(|instr| instr.opcode == *opcode),
            "Expected opcode {:?} not found",
            opcode
        );
    }

    Ok(())
}

// Test compile_module with actual export operations
#[test]
fn test_compile_module_with_exports() -> Result<()> {
    let mut graph = Graph::new();

    // Create a module that defines and exports multiple values
    let a = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let b = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let c = graph
        .add_node(Node::Literal(Literal::Integer(3)))
        .expect("Failed to add node");

    let bindings_body = graph
        .add_node(Node::Literal(Literal::Nil))
        .expect("Failed to add node");
    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![
                ("a".to_string(), a),
                ("b".to_string(), b),
                ("c".to_string(), c),
            ],
            body: bindings_body,
        })
        .expect("Failed to add node");

    let module_node = graph
        .add_node(Node::Module {
            name: "test_module".to_string(),
            exports: vec!["a".to_string(), "b".to_string(), "c".to_string()],
            body: let_node,
        })
        .expect("Failed to add node");
    graph.root_id = Some(module_node);

    compile_and_check_opcodes(
        &graph,
        &[
            Opcode::BeginModule,
            Opcode::EndModule,
            Opcode::ExportBinding,
        ],
    )?;
    Ok(())
}

// Test compile_import with alias
#[test]
fn test_compile_import_with_alias() -> Result<()> {
    let mut graph = Graph::new();

    // Create (import "math" ((add as plus) (sub as minus)))
    let import_node = graph
        .add_node(Node::Import {
            module_path: "math".to_string(),
            import_list: vec![
                ImportItem {
                    name: "add".to_string(),
                    alias: Some("plus".to_string()),
                },
                ImportItem {
                    name: "sub".to_string(),
                    alias: Some("minus".to_string()),
                },
            ],
            import_all: false,
        })
        .expect("Failed to add node");
    graph.root_id = Some(import_node);

    compile_and_check_opcodes(
        &graph,
        &[Opcode::LoadModule, Opcode::ImportBinding, Opcode::Store],
    )?;
    Ok(())
}

// Test compile_export with aliases
#[test]
fn test_compile_export_with_alias() -> Result<()> {
    let mut graph = Graph::new();

    // Create (export ((internal_add as add) (internal_sub as sub)))
    let export_node = graph
        .add_node(Node::Export {
            export_list: vec![
                ExportItem {
                    name: "internal_add".to_string(),
                    alias: Some("add".to_string()),
                },
                ExportItem {
                    name: "internal_sub".to_string(),
                    alias: Some("sub".to_string()),
                },
            ],
        })
        .expect("Failed to add node");
    graph.root_id = Some(export_node);

    compile_and_check_opcodes(&graph, &[Opcode::ExportBinding])?;
    Ok(())
}

// Test compile_qualified_variable in complex expression
#[test]
fn test_compile_qualified_variable_complex() -> Result<()> {
    let mut graph = Graph::new();

    // Create (+ (math:add 1 2) (string:length "hello"))
    let math_add = graph
        .add_node(Node::QualifiedVariable {
            module_name: "math".to_string(),
            variable_name: "add".to_string(),
        })
        .expect("Failed to add node");
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let math_call = graph
        .add_node(Node::Application {
            function: math_add,
            args: vec![one, two],
        })
        .expect("Failed to add node");

    let string_length = graph
        .add_node(Node::QualifiedVariable {
            module_name: "string".to_string(),
            variable_name: "length".to_string(),
        })
        .expect("Failed to add node");
    let hello = graph
        .add_node(Node::Literal(Literal::String("hello".to_string())))
        .expect("Failed to add node");
    let string_call = graph
        .add_node(Node::Application {
            function: string_length,
            args: vec![hello],
        })
        .expect("Failed to add node");

    let plus = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let final_app = graph
        .add_node(Node::Application {
            function: plus,
            args: vec![math_call, string_call],
        })
        .expect("Failed to add node");
    graph.root_id = Some(final_app);

    compile_and_check_opcodes(&graph, &[Opcode::LoadQualified])?;
    Ok(())
}

// Test all effect types
#[test]
fn test_compile_all_effect_types() -> Result<()> {
    let mut graph = Graph::new();

    // Test IO effect
    let msg = graph
        .add_node(Node::Literal(Literal::String("test".to_string())))
        .expect("Failed to add node");
    let io_effect = graph
        .add_node(Node::Effect {
            effect_type: EffectType::IO,
            operation: "write".to_string(),
            args: vec![msg],
        })
        .expect("Failed to add node");

    // Test State effect
    let key = graph
        .add_node(Node::Literal(Literal::String("counter".to_string())))
        .expect("Failed to add node");
    let value = graph
        .add_node(Node::Literal(Literal::Integer(42)))
        .expect("Failed to add node");
    let state_effect = graph
        .add_node(Node::Effect {
            effect_type: EffectType::State,
            operation: "update".to_string(),
            args: vec![key, value],
        })
        .expect("Failed to add node");

    // Test Network effect
    let url = graph
        .add_node(Node::Literal(Literal::String(
            "http://example.com".to_string(),
        )))
        .expect("Failed to add node");
    let network_effect = graph
        .add_node(Node::Effect {
            effect_type: EffectType::Network,
            operation: "post".to_string(),
            args: vec![url],
        })
        .expect("Failed to add node");

    // Test Random effect
    let random_effect = graph
        .add_node(Node::Effect {
            effect_type: EffectType::Random,
            operation: "int".to_string(),
            args: vec![],
        })
        .expect("Failed to add node");

    // Test Time effect
    let time_effect = graph
        .add_node(Node::Effect {
            effect_type: EffectType::Time,
            operation: "now".to_string(),
            args: vec![],
        })
        .expect("Failed to add node");

    // Create a sequence of all effects
    let seq = graph
        .add_node(Node::List(vec![
            io_effect,
            state_effect,
            network_effect,
            random_effect,
            time_effect,
        ]))
        .expect("Failed to add node");
    graph.root_id = Some(seq);

    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;

    let main_chunk = &bytecode.chunks[0];
    // Count Effect opcodes - should be 5
    let effect_count = main_chunk
        .instructions
        .iter()
        .filter(|instr| instr.opcode == Opcode::Effect)
        .count();
    assert_eq!(effect_count, 5, "Expected 5 Effect opcodes");

    Ok(())
}

// Test async with complex body
#[test]
fn test_compile_async_complex() -> Result<()> {
    let mut graph = Graph::new();

    // Create (async
    //          (let ((x (await (fetch "url")))
    //                (y (await (process x))))
    //            (+ x y)))
    let fetch = graph
        .add_node(Node::Variable {
            name: "fetch".to_string(),
        })
        .expect("Failed to add node");
    let url = graph
        .add_node(Node::Literal(Literal::String("url".to_string())))
        .expect("Failed to add node");
    let fetch_call = graph
        .add_node(Node::Application {
            function: fetch,
            args: vec![url],
        })
        .expect("Failed to add node");
    let await1 = graph
        .add_node(Node::Await { expr: fetch_call })
        .expect("Failed to add node");

    let process = graph
        .add_node(Node::Variable {
            name: "process".to_string(),
        })
        .expect("Failed to add node");
    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let process_call = graph
        .add_node(Node::Application {
            function: process,
            args: vec![x_var],
        })
        .expect("Failed to add node");
    let await2 = graph
        .add_node(Node::Await { expr: process_call })
        .expect("Failed to add node");

    let x_var2 = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let y_var = graph
        .add_node(Node::Variable {
            name: "y".to_string(),
        })
        .expect("Failed to add node");
    let plus = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let add = graph
        .add_node(Node::Application {
            function: plus,
            args: vec![x_var2, y_var],
        })
        .expect("Failed to add node");

    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![("x".to_string(), await1), ("y".to_string(), await2)],
            body: add,
        })
        .expect("Failed to add node");

    let async_node = graph
        .add_node(Node::Async { body: let_node })
        .expect("Failed to add node");
    graph.root_id = Some(async_node);

    compile_and_check_opcodes(&graph, &[Opcode::Await])?;
    Ok(())
}

// Test spawn with lambda
#[test]
fn test_compile_spawn_lambda() -> Result<()> {
    let mut graph = Graph::new();

    // Create (spawn (lambda () (print "worker")))
    let print_var = graph
        .add_node(Node::Variable {
            name: "print".to_string(),
        })
        .expect("Failed to add node");
    let msg = graph
        .add_node(Node::Literal(Literal::String("worker".to_string())))
        .expect("Failed to add node");
    let print_call = graph
        .add_node(Node::Application {
            function: print_var,
            args: vec![msg],
        })
        .expect("Failed to add node");

    let lambda = graph
        .add_node(Node::Lambda {
            params: vec![],
            body: print_call,
        })
        .expect("Failed to add node");

    let spawn_node = graph
        .add_node(Node::Spawn { expr: lambda })
        .expect("Failed to add node");
    graph.root_id = Some(spawn_node);

    compile_and_check_opcodes(&graph, &[Opcode::Spawn])?;
    Ok(())
}

// Test send/receive with complex expressions
#[test]
fn test_compile_channel_complex() -> Result<()> {
    let mut graph = Graph::new();

    // Create (let ((ch1 (channel))
    //              (ch2 (channel)))
    //          (send ch1 (receive ch2)))
    let ch1 = graph.add_node(Node::Channel).expect("Failed to add node");
    let ch2 = graph.add_node(Node::Channel).expect("Failed to add node");

    let ch2_var = graph
        .add_node(Node::Variable {
            name: "ch2".to_string(),
        })
        .expect("Failed to add node");
    let recv = graph
        .add_node(Node::Receive { channel: ch2_var })
        .expect("Failed to add node");

    let ch1_var = graph
        .add_node(Node::Variable {
            name: "ch1".to_string(),
        })
        .expect("Failed to add node");
    let send = graph
        .add_node(Node::Send {
            channel: ch1_var,
            value: recv,
        })
        .expect("Failed to add node");

    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![("ch1".to_string(), ch1), ("ch2".to_string(), ch2)],
            body: send,
        })
        .expect("Failed to add node");
    graph.root_id = Some(let_node);

    compile_and_check_opcodes(&graph, &[Opcode::Channel, Opcode::Send, Opcode::Receive])?;
    Ok(())
}

// Test helper methods coverage - enter_scope/exit_scope are called internally
#[test]
fn test_nested_scopes() -> Result<()> {
    let mut graph = Graph::new();

    // Create deeply nested let expressions to exercise scope management
    let a = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let _a_var = graph
        .add_node(Node::Variable {
            name: "a".to_string(),
        })
        .expect("Failed to add node");

    let b = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let b_var = graph
        .add_node(Node::Variable {
            name: "b".to_string(),
        })
        .expect("Failed to add node");

    let c = graph
        .add_node(Node::Literal(Literal::Integer(3)))
        .expect("Failed to add node");
    let c_var = graph
        .add_node(Node::Variable {
            name: "c".to_string(),
        })
        .expect("Failed to add node");

    let plus = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let inner_add = graph
        .add_node(Node::Application {
            function: plus,
            args: vec![b_var, c_var],
        })
        .expect("Failed to add node");

    let inner_let = graph
        .add_node(Node::Let {
            bindings: vec![("c".to_string(), c)],
            body: inner_add,
        })
        .expect("Failed to add node");

    let middle_let = graph
        .add_node(Node::Let {
            bindings: vec![("b".to_string(), b)],
            body: inner_let,
        })
        .expect("Failed to add node");

    let outer_let = graph
        .add_node(Node::Let {
            bindings: vec![("a".to_string(), a)],
            body: middle_let,
        })
        .expect("Failed to add node");

    graph.root_id = Some(outer_let);

    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;

    // This exercises enter_scope and exit_scope methods
    assert!(!bytecode.chunks.is_empty());

    Ok(())
}

// Test module with function exports to hit more paths
#[test]
fn test_module_function_exports() -> Result<()> {
    let mut graph = Graph::new();

    // Create a module that exports functions
    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let times = graph
        .add_node(Node::Variable {
            name: "*".to_string(),
        })
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let double_body = graph
        .add_node(Node::Application {
            function: times,
            args: vec![x_var, two],
        })
        .expect("Failed to add node");
    let double_fn = graph
        .add_node(Node::Lambda {
            params: vec!["x".to_string()],
            body: double_body,
        })
        .expect("Failed to add node");

    let y_var = graph
        .add_node(Node::Variable {
            name: "y".to_string(),
        })
        .expect("Failed to add node");
    let y_var2 = graph
        .add_node(Node::Variable {
            name: "y".to_string(),
        })
        .expect("Failed to add node");
    let times2 = graph
        .add_node(Node::Variable {
            name: "*".to_string(),
        })
        .expect("Failed to add node");
    let square_body = graph
        .add_node(Node::Application {
            function: times2,
            args: vec![y_var, y_var2],
        })
        .expect("Failed to add node");
    let square_fn = graph
        .add_node(Node::Lambda {
            params: vec!["y".to_string()],
            body: square_body,
        })
        .expect("Failed to add node");

    let body = graph
        .add_node(Node::Literal(Literal::Nil))
        .expect("Failed to add node");
    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![
                ("double".to_string(), double_fn),
                ("square".to_string(), square_fn),
            ],
            body,
        })
        .expect("Failed to add node");

    let module_node = graph
        .add_node(Node::Module {
            name: "utils".to_string(),
            exports: vec!["double".to_string(), "square".to_string()],
            body: let_node,
        })
        .expect("Failed to add node");
    graph.root_id = Some(module_node);

    compile_and_check_opcodes(
        &graph,
        &[
            Opcode::BeginModule,
            Opcode::ExportBinding,
            Opcode::EndModule,
        ],
    )?;
    Ok(())
}

// Test import with empty import list but not import_all
#[test]
fn test_import_empty_list() -> Result<()> {
    let mut graph = Graph::new();

    let import_node = graph
        .add_node(Node::Import {
            module_path: "empty_module".to_string(),
            import_list: vec![],
            import_all: false,
        })
        .expect("Failed to add node");
    graph.root_id = Some(import_node);

    compile_and_check_opcodes(&graph, &[Opcode::LoadModule])?;
    Ok(())
}

// Test effect with no arguments
#[test]
fn test_effect_no_args() -> Result<()> {
    let mut graph = Graph::new();

    let effect_node = graph
        .add_node(Node::Effect {
            effect_type: EffectType::Time,
            operation: "now".to_string(),
            args: vec![],
        })
        .expect("Failed to add node");
    graph.root_id = Some(effect_node);

    compile_and_check_opcodes(&graph, &[Opcode::Effect])?;
    Ok(())
}

// Test effect with many arguments
#[test]
fn test_effect_many_args() -> Result<()> {
    let mut graph = Graph::new();

    let args: Vec<_> = (0..5)
        .map(|i| {
            graph
                .add_node(Node::Literal(Literal::Integer(i)))
                .expect("Failed to add node")
        })
        .collect();

    let effect_node = graph
        .add_node(Node::Effect {
            effect_type: EffectType::Dom, // Using Dom as an example
            operation: "multi_arg".to_string(),
            args,
        })
        .expect("Failed to add node");
    graph.root_id = Some(effect_node);

    compile_and_check_opcodes(&graph, &[Opcode::Effect])?;
    Ok(())
}

// Test module with no exports
#[test]
fn test_module_no_exports() -> Result<()> {
    let mut graph = Graph::new();

    let body = graph
        .add_node(Node::Literal(Literal::String("private module".to_string())))
        .expect("Failed to add node");
    let module_node = graph
        .add_node(Node::Module {
            name: "private".to_string(),
            exports: vec![],
            body,
        })
        .expect("Failed to add node");
    graph.root_id = Some(module_node);

    compile_and_check_opcodes(&graph, &[Opcode::BeginModule, Opcode::EndModule])?;
    Ok(())
}

// Test qualified variable in different positions
#[test]
fn test_qualified_variable_positions() -> Result<()> {
    let mut graph = Graph::new();

    // As function in application
    let qual_var = graph
        .add_node(Node::QualifiedVariable {
            module_name: "mod".to_string(),
            variable_name: "func".to_string(),
        })
        .expect("Failed to add node");
    let arg = graph
        .add_node(Node::Literal(Literal::Integer(5)))
        .expect("Failed to add node");
    let app = graph
        .add_node(Node::Application {
            function: qual_var,
            args: vec![arg],
        })
        .expect("Failed to add node");

    // In let binding
    let qual_var2 = graph
        .add_node(Node::QualifiedVariable {
            module_name: "constants".to_string(),
            variable_name: "PI".to_string(),
        })
        .expect("Failed to add node");
    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![("pi".to_string(), qual_var2)],
            body: app,
        })
        .expect("Failed to add node");

    graph.root_id = Some(let_node);

    compile_and_check_opcodes(&graph, &[Opcode::LoadQualified])?;
    Ok(())
}
