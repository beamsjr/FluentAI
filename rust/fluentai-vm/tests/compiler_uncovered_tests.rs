//! Tests for uncovered compiler code paths
//! Targets: module system edge cases, async/await/spawn complex scenarios,
//! constructor handling, and GC special forms

use fluentai_core::ast::{Graph, Node, NodeId, Literal, ImportItem, ExportItem};
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    bytecode::Opcode,
};
use fluentai_optimizer::OptimizationLevel;
use anyhow::Result;

/// Helper to compile and check for expected opcodes
fn compile_and_check_opcodes(graph: &Graph, expected_opcodes: &[Opcode]) -> Result<()> {
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::Standard, // Use optimization since it's fixed
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(graph)?;
    
    let main_chunk = &bytecode.chunks[0];
    for opcode in expected_opcodes {
        assert!(
            main_chunk.instructions.iter().any(|instr| instr.opcode == *opcode),
            "Expected opcode {:?} not found", opcode
        );
    }
    
    Ok(())
}

/// Helper to compile and expect an error
fn compile_and_expect_error(graph: &Graph, error_substring: &str) -> Result<()> {
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::Standard,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let result = compiler.compile(graph);
    
    assert!(result.is_err(), "Expected compilation to fail");
    let error = result.unwrap_err();
    assert!(
        error.to_string().contains(error_substring),
        "Expected error containing '{}', got: {}",
        error_substring,
        error
    );
    
    Ok(())
}

// ========== Module System Tests ==========

#[test]
fn test_import_all_not_implemented() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (import "math" *)
    let import_node = graph.add_node(Node::Import {
        module_path: "math".to_string(),
        import_list: vec![],
        import_all: true,
    });
    graph.root_id = Some(import_node);
    
    compile_and_expect_error(&graph, "Import * not yet implemented")?;
    Ok(())
}

#[test]
fn test_module_with_complex_exports() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create a module that exports values, functions, and re-exports
    let const_val = graph.add_node(Node::Literal(Literal::Integer(42)));
    
    // Create a function to export
    let x_var1 = graph.add_node(Node::Variable { name: "x".to_string() });
    let x_var2 = graph.add_node(Node::Variable { name: "x".to_string() });
    let double_body = graph.add_node(Node::List(vec![x_var1, x_var2]));
    let double_fn = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: double_body,
    });
    
    // Import something to re-export
    let import_node = graph.add_node(Node::Import {
        module_path: "base".to_string(),
        import_list: vec![ImportItem { name: "print".to_string(), alias: None }],
        import_all: false,
    });
    
    // Create bindings
    let bindings_body = graph.add_node(Node::Literal(Literal::Nil));
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("PI".to_string(), const_val),
            ("double".to_string(), double_fn),
        ],
        body: bindings_body,
    });
    
    // Sequence import and let
    let body = graph.add_node(Node::List(vec![import_node, let_node]));
    
    let module_node = graph.add_node(Node::Module {
        name: "utils".to_string(),
        exports: vec!["PI".to_string(), "double".to_string(), "print".to_string()],
        body,
    });
    graph.root_id = Some(module_node);
    
    compile_and_check_opcodes(&graph, &[
        Opcode::BeginModule,
        Opcode::LoadModule,
        Opcode::ImportBinding,
        Opcode::ExportBinding,
        Opcode::EndModule,
    ])?;
    Ok(())
}

#[test]
fn test_import_with_complex_aliases() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (import "long-module-name" 
    //          ((very-long-function-name as fn)
    //           (another-long-name as aln)
    //           (short as s)))
    let import_node = graph.add_node(Node::Import {
        module_path: "long-module-name".to_string(),
        import_list: vec![
            ImportItem { 
                name: "very-long-function-name".to_string(), 
                alias: Some("fn".to_string()) 
            },
            ImportItem { 
                name: "another-long-name".to_string(), 
                alias: Some("aln".to_string()) 
            },
            ImportItem { 
                name: "short".to_string(), 
                alias: Some("s".to_string()) 
            },
        ],
        import_all: false,
    });
    graph.root_id = Some(import_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::LoadModule, Opcode::ImportBinding, Opcode::Store])?;
    Ok(())
}

#[test]
fn test_export_with_complex_aliases() -> Result<()> {
    let mut graph = Graph::new();
    
    // Define some internal functions
    let nil_body = graph.add_node(Node::Literal(Literal::Nil));
    let internal_fn1 = graph.add_node(Node::Lambda {
        params: vec![],
        body: nil_body,
    });
    let int_body = graph.add_node(Node::Literal(Literal::Integer(42)));
    let internal_fn2 = graph.add_node(Node::Lambda {
        params: vec![],
        body: int_body,
    });
    
    let body = graph.add_node(Node::Literal(Literal::Nil));
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("_internal_helper".to_string(), internal_fn1),
            ("_private_impl".to_string(), internal_fn2),
        ],
        body,
    });
    
    // Export with aliases
    let export_node = graph.add_node(Node::Export {
        export_list: vec![
            ExportItem { 
                name: "_internal_helper".to_string(), 
                alias: Some("publicHelper".to_string()) 
            },
            ExportItem { 
                name: "_private_impl".to_string(), 
                alias: Some("api".to_string()) 
            },
        ],
    });
    
    let seq = graph.add_node(Node::List(vec![let_node, export_node]));
    graph.root_id = Some(seq);
    
    compile_and_check_opcodes(&graph, &[Opcode::ExportBinding])?;
    Ok(())
}

#[test]
fn test_module_with_no_body() -> Result<()> {
    let mut graph = Graph::new();
    
    // This should create an invalid node ID
    let module_node = graph.add_node(Node::Module {
        name: "empty".to_string(),
        exports: vec!["undefined".to_string()],
        body: NodeId(std::num::NonZeroU32::new(9999).unwrap()), // Invalid node ID
    });
    graph.root_id = Some(module_node);
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let result = compiler.compile(&graph);
    assert!(result.is_err());
    Ok(())
}

#[test]
fn test_qualified_variable_in_nested_contexts() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create simple usage of qualified variable
    // (math:pi)
    
    let math_pi = graph.add_node(Node::QualifiedVariable {
        module_name: "math".to_string(),
        variable_name: "pi".to_string(),
    });
    graph.root_id = Some(math_pi);
    
    compile_and_check_opcodes(&graph, &[Opcode::LoadQualified])?;
    Ok(())
}

// ========== Async/Await/Spawn Tests ==========

#[test]
fn test_async_with_error_handling() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (async 
    //          (try
    //            (await (risky-operation))
    //            (catch e
    //              (print e))))
    
    let risky_op = graph.add_node(Node::Variable { name: "risky-operation".to_string() });
    let risky_call = graph.add_node(Node::Application {
        function: risky_op,
        args: vec![],
    });
    let await_node = graph.add_node(Node::Await { expr: risky_call });
    
    // For now, simulate try-catch with conditional
    let error_var = graph.add_node(Node::Variable { name: "error?".to_string() });
    let check_error = graph.add_node(Node::Application {
        function: error_var,
        args: vec![await_node.clone()],
    });
    
    let print_var = graph.add_node(Node::Variable { name: "print".to_string() });
    let e_var = graph.add_node(Node::Variable { name: "e".to_string() });
    let print_error = graph.add_node(Node::Application {
        function: print_var,
        args: vec![e_var],
    });
    
    let if_node = graph.add_node(Node::If {
        condition: check_error,
        then_branch: print_error,
        else_branch: await_node,
    });
    
    let async_node = graph.add_node(Node::Async { body: if_node });
    graph.root_id = Some(async_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::Await, Opcode::JumpIfNot])?;
    Ok(())
}

#[test]
fn test_nested_async_operations() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (async
    //          (let ((inner (async
    //                         (await (fetch "url")))))
    //            (await inner)))
    
    let fetch_var = graph.add_node(Node::Variable { name: "fetch".to_string() });
    let url = graph.add_node(Node::Literal(Literal::String("url".to_string())));
    let fetch_call = graph.add_node(Node::Application {
        function: fetch_var,
        args: vec![url],
    });
    let inner_await = graph.add_node(Node::Await { expr: fetch_call });
    let inner_async = graph.add_node(Node::Async { body: inner_await });
    
    let inner_var = graph.add_node(Node::Variable { name: "inner".to_string() });
    let outer_await = graph.add_node(Node::Await { expr: inner_var });
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("inner".to_string(), inner_async)],
        body: outer_await,
    });
    
    let outer_async = graph.add_node(Node::Async { body: let_node });
    graph.root_id = Some(outer_async);
    
    compile_and_check_opcodes(&graph, &[Opcode::Await])?;
    Ok(())
}

#[test]
fn test_spawn_with_captured_variables() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (let ((x 10)
    //              (y 20))
    //          (spawn (lambda ()
    //                   (+ x y))))
    
    let x_val = graph.add_node(Node::Literal(Literal::Integer(10)));
    let y_val = graph.add_node(Node::Literal(Literal::Integer(20)));
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var = graph.add_node(Node::Variable { name: "y".to_string() });
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let add = graph.add_node(Node::Application {
        function: plus,
        args: vec![x_var, y_var],
    });
    
    let lambda = graph.add_node(Node::Lambda {
        params: vec![],
        body: add,
    });
    
    let spawn_node = graph.add_node(Node::Spawn { expr: lambda });
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("x".to_string(), x_val),
            ("y".to_string(), y_val),
        ],
        body: spawn_node,
    });
    graph.root_id = Some(let_node);
    
    // We should find Spawn in the main chunk, and the lambda should be compiled as a closure
    compile_and_check_opcodes(&graph, &[Opcode::Spawn])?;
    
    // Also verify the code compiles without errors
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::Standard,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Should have at least 2 chunks (main + lambda)
    assert!(bytecode.chunks.len() >= 2, "Should have main and lambda chunks");
    
    Ok(())
}

#[test]
fn test_complex_await_chains() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (async
    //          (let* ((a (await (fetch "url1")))
    //                 (b (await (process a)))
    //                 (c (await (transform b)))
    //                 (d (await (finalize c))))
    //            d))
    
    let fetch = graph.add_node(Node::Variable { name: "fetch".to_string() });
    let url1 = graph.add_node(Node::Literal(Literal::String("url1".to_string())));
    let fetch_call = graph.add_node(Node::Application {
        function: fetch,
        args: vec![url1],
    });
    let await_a = graph.add_node(Node::Await { expr: fetch_call });
    
    let process = graph.add_node(Node::Variable { name: "process".to_string() });
    let a_var = graph.add_node(Node::Variable { name: "a".to_string() });
    let process_call = graph.add_node(Node::Application {
        function: process,
        args: vec![a_var],
    });
    let await_b = graph.add_node(Node::Await { expr: process_call });
    
    let transform = graph.add_node(Node::Variable { name: "transform".to_string() });
    let b_var = graph.add_node(Node::Variable { name: "b".to_string() });
    let transform_call = graph.add_node(Node::Application {
        function: transform,
        args: vec![b_var],
    });
    let await_c = graph.add_node(Node::Await { expr: transform_call });
    
    let finalize = graph.add_node(Node::Variable { name: "finalize".to_string() });
    let c_var = graph.add_node(Node::Variable { name: "c".to_string() });
    let finalize_call = graph.add_node(Node::Application {
        function: finalize,
        args: vec![c_var],
    });
    let await_d = graph.add_node(Node::Await { expr: finalize_call });
    
    let d_var = graph.add_node(Node::Variable { name: "d".to_string() });
    
    // Nested lets to simulate let*
    let let4 = graph.add_node(Node::Let {
        bindings: vec![("d".to_string(), await_d)],
        body: d_var,
    });
    let let3 = graph.add_node(Node::Let {
        bindings: vec![("c".to_string(), await_c)],
        body: let4,
    });
    let let2 = graph.add_node(Node::Let {
        bindings: vec![("b".to_string(), await_b)],
        body: let3,
    });
    let let1 = graph.add_node(Node::Let {
        bindings: vec![("a".to_string(), await_a)],
        body: let2,
    });
    
    let async_node = graph.add_node(Node::Async { body: let1 });
    graph.root_id = Some(async_node);
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::Standard,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Count Await opcodes - should be 4
    let main_chunk = &bytecode.chunks[0];
    let await_count = main_chunk.instructions.iter()
        .filter(|instr| instr.opcode == Opcode::Await)
        .count();
    assert_eq!(await_count, 4, "Expected 4 Await opcodes");
    
    Ok(())
}

#[test]
fn test_async_in_conditional_branches() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (if condition
    //          (async (await (fast-op)))
    //          (async (await (slow-op))))
    
    let condition = graph.add_node(Node::Variable { name: "condition".to_string() });
    
    let fast_op = graph.add_node(Node::Variable { name: "fast-op".to_string() });
    let fast_call = graph.add_node(Node::Application {
        function: fast_op,
        args: vec![],
    });
    let fast_await = graph.add_node(Node::Await { expr: fast_call });
    let then_async = graph.add_node(Node::Async { body: fast_await });
    
    let slow_op = graph.add_node(Node::Variable { name: "slow-op".to_string() });
    let slow_call = graph.add_node(Node::Application {
        function: slow_op,
        args: vec![],
    });
    let slow_await = graph.add_node(Node::Await { expr: slow_call });
    let else_async = graph.add_node(Node::Async { body: slow_await });
    
    let if_node = graph.add_node(Node::If {
        condition,
        then_branch: then_async,
        else_branch: else_async,
    });
    graph.root_id = Some(if_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::JumpIfNot, Opcode::Await])?;
    Ok(())
}

// ========== Constructor Tests ==========

#[test]
fn test_constructor_zero_args() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (Nil)
    let nil_ctor = graph.add_node(Node::Variable { name: "Nil".to_string() });
    let nil_app = graph.add_node(Node::Application {
        function: nil_ctor,
        args: vec![],
    });
    graph.root_id = Some(nil_app);
    
    compile_and_check_opcodes(&graph, &[Opcode::MakeTagged])?;
    Ok(())
}

#[test]
fn test_constructor_many_args() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (Record "field1" 1 "field2" 2 "field3" 3 "field4" 4 "field5" 5)
    let record_ctor = graph.add_node(Node::Variable { name: "Record".to_string() });
    
    let args: Vec<NodeId> = (1..=5).flat_map(|i| {
        vec![
            graph.add_node(Node::Literal(Literal::String(format!("field{}", i)))),
            graph.add_node(Node::Literal(Literal::Integer(i))),
        ]
    }).collect();
    
    let record_app = graph.add_node(Node::Application {
        function: record_ctor,
        args,
    });
    graph.root_id = Some(record_app);
    
    compile_and_check_opcodes(&graph, &[Opcode::MakeTagged])?;
    Ok(())
}

#[test]
fn test_nested_constructors() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (Cons 1 (Cons 2 (Cons 3 (Nil))))
    let nil_ctor = graph.add_node(Node::Variable { name: "Nil".to_string() });
    let nil_app = graph.add_node(Node::Application {
        function: nil_ctor,
        args: vec![],
    });
    
    let cons_ctor1 = graph.add_node(Node::Variable { name: "Cons".to_string() });
    let three = graph.add_node(Node::Literal(Literal::Integer(3)));
    let cons3 = graph.add_node(Node::Application {
        function: cons_ctor1,
        args: vec![three, nil_app],
    });
    
    let cons_ctor2 = graph.add_node(Node::Variable { name: "Cons".to_string() });
    let two = graph.add_node(Node::Literal(Literal::Integer(2)));
    let cons2 = graph.add_node(Node::Application {
        function: cons_ctor2,
        args: vec![two, cons3],
    });
    
    let cons_ctor3 = graph.add_node(Node::Variable { name: "Cons".to_string() });
    let one = graph.add_node(Node::Literal(Literal::Integer(1)));
    let cons1 = graph.add_node(Node::Application {
        function: cons_ctor3,
        args: vec![one, cons2],
    });
    
    graph.root_id = Some(cons1);
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::Standard,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Should have 4 MakeTagged instructions (3 Cons + 1 Nil)
    let main_chunk = &bytecode.chunks[0];
    let tagged_count = main_chunk.instructions.iter()
        .filter(|instr| instr.opcode == Opcode::MakeTagged)
        .count();
    assert_eq!(tagged_count, 4, "Expected 4 MakeTagged opcodes");
    
    Ok(())
}

#[test]
fn test_unicode_uppercase_constructor() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (Örnek 42) - Turkish uppercase letter
    let ctor = graph.add_node(Node::Variable { name: "Örnek".to_string() });
    let arg = graph.add_node(Node::Literal(Literal::Integer(42)));
    let app = graph.add_node(Node::Application {
        function: ctor,
        args: vec![arg],
    });
    graph.root_id = Some(app);
    
    compile_and_check_opcodes(&graph, &[Opcode::MakeTagged])?;
    Ok(())
}

#[test]
fn test_constructor_as_first_class_value() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (let ((make-nil Nil))
    //          (make-nil))
    let nil_ctor = graph.add_node(Node::Variable { name: "Nil".to_string() });
    
    let make_nil_var = graph.add_node(Node::Variable { name: "make-nil".to_string() });
    let call_make_nil = graph.add_node(Node::Application {
        function: make_nil_var,
        args: vec![],
    });
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("make-nil".to_string(), nil_ctor)],
        body: call_make_nil,
    });
    graph.root_id = Some(let_node);
    
    // Constructor stored as variable should be loaded as global
    compile_and_check_opcodes(&graph, &[Opcode::LoadGlobal])?;
    Ok(())
}

// ========== GC Special Forms Tests ==========

#[test]
fn test_gc_let_multiple_bindings() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (gc:let ((a (list 1 2 3))
    //                  (b (list 4 5 6))
    //                  (c (list 7 8 9)))
    //          (list a b c))
    
    let gc_let = graph.add_node(Node::Variable { name: "gc:let".to_string() });
    
    // Create three lists
    let n1 = graph.add_node(Node::Literal(Literal::Integer(1)));
    let n2 = graph.add_node(Node::Literal(Literal::Integer(2)));
    let n3 = graph.add_node(Node::Literal(Literal::Integer(3)));
    let list1 = graph.add_node(Node::List(vec![n1, n2, n3]));
    
    let n4 = graph.add_node(Node::Literal(Literal::Integer(4)));
    let n5 = graph.add_node(Node::Literal(Literal::Integer(5)));
    let n6 = graph.add_node(Node::Literal(Literal::Integer(6)));
    let list2 = graph.add_node(Node::List(vec![n4, n5, n6]));
    
    let n7 = graph.add_node(Node::Literal(Literal::Integer(7)));
    let n8 = graph.add_node(Node::Literal(Literal::Integer(8)));
    let n9 = graph.add_node(Node::Literal(Literal::Integer(9)));
    let list3 = graph.add_node(Node::List(vec![n7, n8, n9]));
    
    // Create bindings
    let a_sym = graph.add_node(Node::Variable { name: "a".to_string() });
    let b_sym = graph.add_node(Node::Variable { name: "b".to_string() });
    let c_sym = graph.add_node(Node::Variable { name: "c".to_string() });
    
    let binding1 = graph.add_node(Node::List(vec![a_sym, list1]));
    let binding2 = graph.add_node(Node::List(vec![b_sym, list2]));
    let binding3 = graph.add_node(Node::List(vec![c_sym, list3]));
    
    let bindings = graph.add_node(Node::List(vec![binding1, binding2, binding3]));
    
    // Create body
    let a_var = graph.add_node(Node::Variable { name: "a".to_string() });
    let b_var = graph.add_node(Node::Variable { name: "b".to_string() });
    let c_var = graph.add_node(Node::Variable { name: "c".to_string() });
    let body = graph.add_node(Node::List(vec![a_var, b_var, c_var]));
    
    let gc_let_app = graph.add_node(Node::Application {
        function: gc_let,
        args: vec![bindings, body],
    });
    graph.root_id = Some(gc_let_app);
    
    compile_and_check_opcodes(&graph, &[Opcode::GcAlloc])?;
    Ok(())
}

#[test]
fn test_gc_let_with_complex_expressions() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (gc:let ((fn (lambda (x) (* x x)))
    //                  (data (map fn (list 1 2 3))))
    //          (reduce + 0 data))
    
    let gc_let = graph.add_node(Node::Variable { name: "gc:let".to_string() });
    
    // Lambda for squaring
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let x_var2 = graph.add_node(Node::Variable { name: "x".to_string() });
    let times = graph.add_node(Node::Variable { name: "*".to_string() });
    let square_body = graph.add_node(Node::Application {
        function: times,
        args: vec![x_var, x_var2],
    });
    let square_fn = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: square_body,
    });
    
    // Map application
    let map_var = graph.add_node(Node::Variable { name: "map".to_string() });
    let fn_var = graph.add_node(Node::Variable { name: "fn".to_string() });
    let n1 = graph.add_node(Node::Literal(Literal::Integer(1)));
    let n2 = graph.add_node(Node::Literal(Literal::Integer(2)));
    let n3 = graph.add_node(Node::Literal(Literal::Integer(3)));
    let list_123 = graph.add_node(Node::List(vec![n1, n2, n3]));
    let map_app = graph.add_node(Node::Application {
        function: map_var,
        args: vec![fn_var, list_123],
    });
    
    // Bindings
    let fn_sym = graph.add_node(Node::Variable { name: "fn".to_string() });
    let data_sym = graph.add_node(Node::Variable { name: "data".to_string() });
    let binding1 = graph.add_node(Node::List(vec![fn_sym, square_fn]));
    let binding2 = graph.add_node(Node::List(vec![data_sym, map_app]));
    let bindings = graph.add_node(Node::List(vec![binding1, binding2]));
    
    // Body
    let reduce_var = graph.add_node(Node::Variable { name: "reduce".to_string() });
    let plus_var = graph.add_node(Node::Variable { name: "+".to_string() });
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    let data_var = graph.add_node(Node::Variable { name: "data".to_string() });
    let body = graph.add_node(Node::Application {
        function: reduce_var,
        args: vec![plus_var, zero, data_var],
    });
    
    let gc_let_app = graph.add_node(Node::Application {
        function: gc_let,
        args: vec![bindings, body],
    });
    graph.root_id = Some(gc_let_app);
    
    compile_and_check_opcodes(&graph, &[Opcode::GcAlloc])?;
    Ok(())
}

#[test]
fn test_gc_let_error_missing_bindings() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (gc:let) - no arguments
    let gc_let = graph.add_node(Node::Variable { name: "gc:let".to_string() });
    let gc_let_app = graph.add_node(Node::Application {
        function: gc_let,
        args: vec![],
    });
    graph.root_id = Some(gc_let_app);
    
    compile_and_expect_error(&graph, "gc:let requires at least one argument")?;
    Ok(())
}

#[test]
fn test_gc_let_error_invalid_binding_format() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (gc:let (x 10) x) - wrong format, should be ((x 10))
    let gc_let = graph.add_node(Node::Variable { name: "gc:let".to_string() });
    let x_sym = graph.add_node(Node::Variable { name: "x".to_string() });
    let ten = graph.add_node(Node::Literal(Literal::Integer(10)));
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    
    let gc_let_app = graph.add_node(Node::Application {
        function: gc_let,
        args: vec![x_sym, ten, x_var],
    });
    graph.root_id = Some(gc_let_app);
    
    compile_and_expect_error(&graph, "gc:let bindings must be a list")?;
    Ok(())
}

#[test]
fn test_gc_let_error_no_body() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (gc:let ((x 10))) - no body
    let gc_let = graph.add_node(Node::Variable { name: "gc:let".to_string() });
    
    let x_sym = graph.add_node(Node::Variable { name: "x".to_string() });
    let ten = graph.add_node(Node::Literal(Literal::Integer(10)));
    let binding = graph.add_node(Node::List(vec![x_sym, ten]));
    let bindings = graph.add_node(Node::List(vec![binding]));
    
    let gc_let_app = graph.add_node(Node::Application {
        function: gc_let,
        args: vec![bindings],
    });
    graph.root_id = Some(gc_let_app);
    
    compile_and_expect_error(&graph, "gc:let requires a body")?;
    Ok(())
}

#[test]
fn test_gc_deref_operations() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create code that uses GC references
    // (let ((ref (gc:alloc 42)))
    //   (gc:deref ref))
    
    let gc_alloc = graph.add_node(Node::Variable { name: "gc:alloc".to_string() });
    let value = graph.add_node(Node::Literal(Literal::Integer(42)));
    let alloc_app = graph.add_node(Node::Application {
        function: gc_alloc,
        args: vec![value],
    });
    
    let gc_deref = graph.add_node(Node::Variable { name: "gc:deref".to_string() });
    let ref_var = graph.add_node(Node::Variable { name: "ref".to_string() });
    let deref_app = graph.add_node(Node::Application {
        function: gc_deref,
        args: vec![ref_var],
    });
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("ref".to_string(), alloc_app)],
        body: deref_app,
    });
    graph.root_id = Some(let_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::LoadGlobal])?;
    Ok(())
}

#[test]
fn test_gc_collect_special_form() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (gc:collect)
    let gc_collect = graph.add_node(Node::Variable { name: "gc:collect".to_string() });
    let collect_app = graph.add_node(Node::Application {
        function: gc_collect,
        args: vec![],
    });
    graph.root_id = Some(collect_app);
    
    compile_and_check_opcodes(&graph, &[Opcode::LoadGlobal])?;
    Ok(())
}

#[test]
fn test_nested_gc_let() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (gc:let ((outer (list 1 2)))
    //          (gc:let ((inner (cons 3 outer)))
    //            inner))
    
    let gc_let = graph.add_node(Node::Variable { name: "gc:let".to_string() });
    
    // Outer list
    let n1 = graph.add_node(Node::Literal(Literal::Integer(1)));
    let n2 = graph.add_node(Node::Literal(Literal::Integer(2)));
    let outer_list = graph.add_node(Node::List(vec![n1, n2]));
    
    // Inner cons
    let cons_var = graph.add_node(Node::Variable { name: "cons".to_string() });
    let three = graph.add_node(Node::Literal(Literal::Integer(3)));
    let outer_var = graph.add_node(Node::Variable { name: "outer".to_string() });
    let cons_app = graph.add_node(Node::Application {
        function: cons_var,
        args: vec![three, outer_var],
    });
    
    // Inner gc:let
    let gc_let2 = graph.add_node(Node::Variable { name: "gc:let".to_string() });
    let inner_sym = graph.add_node(Node::Variable { name: "inner".to_string() });
    let inner_binding = graph.add_node(Node::List(vec![inner_sym, cons_app]));
    let inner_bindings = graph.add_node(Node::List(vec![inner_binding]));
    let inner_var = graph.add_node(Node::Variable { name: "inner".to_string() });
    
    let inner_gc_let = graph.add_node(Node::Application {
        function: gc_let2,
        args: vec![inner_bindings, inner_var],
    });
    
    // Outer gc:let
    let outer_sym = graph.add_node(Node::Variable { name: "outer".to_string() });
    let outer_binding = graph.add_node(Node::List(vec![outer_sym, outer_list]));
    let outer_bindings = graph.add_node(Node::List(vec![outer_binding]));
    
    let outer_gc_let = graph.add_node(Node::Application {
        function: gc_let,
        args: vec![outer_bindings, inner_gc_let],
    });
    graph.root_id = Some(outer_gc_let);
    
    compile_and_check_opcodes(&graph, &[Opcode::GcAlloc])?;
    Ok(())
}

// ========== Integration Tests ==========

#[test]
fn test_module_system_with_optimization() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create a simple module with optimization enabled
    // (module opt-test
    //   (export (value))
    //   (define value 42))
    
    let value = graph.add_node(Node::Literal(Literal::Integer(42)));
    
    let body = graph.add_node(Node::Literal(Literal::Nil));
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("value".to_string(), value)],
        body,
    });
    
    let module_node = graph.add_node(Node::Module {
        name: "opt-test".to_string(),
        exports: vec!["value".to_string()],
        body: let_node,
    });
    graph.root_id = Some(module_node);
    
    // Compile with optimization (Standard to avoid stack overflow)
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::Standard,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    assert!(!bytecode.chunks.is_empty());
    Ok(())
}