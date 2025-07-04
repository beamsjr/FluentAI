//! Module system tests for FluentAI compiler

use fluentai_core::ast::{Graph, Node, Literal, ImportItem, ExportItem};
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    bytecode::{Opcode, Value},
    VM,
};
use fluentai_optimizer::OptimizationLevel;
use anyhow::Result;

fn compile_and_check(graph: &Graph) -> Result<()> {
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(graph)?;
    
    // Just verify it compiles without error
    assert!(!bytecode.chunks.is_empty());
    Ok(())
}

#[test]
fn test_compile_module_declaration() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (module math
    //          (export (add sub))
    //          (define add (lambda (x y) (+ x y)))
    //          (define sub (lambda (x y) (- x y))))
    
    // Define add function
    let x_var1 = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var1 = graph.add_node(Node::Variable { name: "y".to_string() });
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let add_body = graph.add_node(Node::Application {
        function: plus,
        args: vec![x_var1, y_var1],
    });
    let add_lambda = graph.add_node(Node::Lambda {
        params: vec!["x".to_string(), "y".to_string()],
        body: add_body,
    });
    
    // Define sub function
    let x_var2 = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var2 = graph.add_node(Node::Variable { name: "y".to_string() });
    let minus = graph.add_node(Node::Variable { name: "-".to_string() });
    let sub_body = graph.add_node(Node::Application {
        function: minus,
        args: vec![x_var2, y_var2],
    });
    let sub_lambda = graph.add_node(Node::Lambda {
        params: vec!["x".to_string(), "y".to_string()],
        body: sub_body,
    });
    
    // Create let bindings for functions
    let bindings_body = graph.add_node(Node::Literal(Literal::Nil));
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("add".to_string(), add_lambda),
            ("sub".to_string(), sub_lambda),
        ],
        body: bindings_body,
    });
    
    // Create module
    let module_node = graph.add_node(Node::Module {
        name: "math".to_string(),
        exports: vec!["add".to_string(), "sub".to_string()],
        body: let_node,
    });
    graph.root_id = Some(module_node);
    
    compile_and_check(&graph)?;
    Ok(())
}

#[test]
fn test_compile_import_single() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (import "math" (add))
    let import_node = graph.add_node(Node::Import {
        module_path: "math".to_string(),
        import_list: vec![ImportItem { name: "add".to_string(), alias: None }],
        import_all: false,
    });
    graph.root_id = Some(import_node);
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Check that it compiled successfully
    assert!(!bytecode.chunks.is_empty());
    
    Ok(())
}

#[test]
fn test_compile_import_all() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (import "math" *)
    let import_node = graph.add_node(Node::Import {
        module_path: "math".to_string(),
        import_list: vec![],
        import_all: true,
    });
    graph.root_id = Some(import_node);
    
    // This should fail with "Import * not yet implemented"
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let result = compiler.compile(&graph);
    
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Import * not yet implemented"));
    Ok(())
}

#[test]
fn test_compile_export() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (export (foo bar))
    let export_node = graph.add_node(Node::Export {
        export_list: vec![
            ExportItem { name: "foo".to_string(), alias: None },
            ExportItem { name: "bar".to_string(), alias: None },
        ],
    });
    graph.root_id = Some(export_node);
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Check that it compiled successfully
    assert!(!bytecode.chunks.is_empty());
    
    Ok(())
}

#[test]
fn test_compile_qualified_variable() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create math:add
    let qualified_var = graph.add_node(Node::QualifiedVariable {
        module_name: "math".to_string(),
        variable_name: "add".to_string(),
    });
    graph.root_id = Some(qualified_var);
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Check that it compiled (LoadQualified opcode)
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| instr.opcode == Opcode::LoadQualified));
    
    Ok(())
}

#[test]
fn test_compile_module_with_nested_imports() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create a module that imports from another module
    let import_node = graph.add_node(Node::Import {
        module_path: "utils".to_string(),
        import_list: vec![ImportItem { name: "helper".to_string(), alias: None }],
        import_all: false,
    });
    
    // Use the imported function
    let helper_var = graph.add_node(Node::Variable { name: "helper".to_string() });
    let ten = graph.add_node(Node::Literal(Literal::Integer(10)));
    let app = graph.add_node(Node::Application {
        function: helper_var,
        args: vec![ten],
    });
    
    // Create a sequence
    let seq_body = graph.add_node(Node::List(vec![import_node, app]));
    
    let module_node = graph.add_node(Node::Module {
        name: "my-module".to_string(),
        exports: vec![],
        body: seq_body,
    });
    graph.root_id = Some(module_node);
    
    compile_and_check(&graph)?;
    Ok(())
}

#[test]
fn test_compile_multiple_exports() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create module with multiple exports
    let a_val = graph.add_node(Node::Literal(Literal::Integer(1)));
    let b_val = graph.add_node(Node::Literal(Literal::Integer(2)));
    let c_val = graph.add_node(Node::Literal(Literal::Integer(3)));
    
    let bindings_body = graph.add_node(Node::Literal(Literal::Nil));
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("a".to_string(), a_val),
            ("b".to_string(), b_val),
            ("c".to_string(), c_val),
        ],
        body: bindings_body,
    });
    
    let module_node = graph.add_node(Node::Module {
        name: "constants".to_string(),
        exports: vec!["a".to_string(), "b".to_string(), "c".to_string()],
        body: let_node,
    });
    graph.root_id = Some(module_node);
    
    compile_and_check(&graph)?;
    Ok(())
}

#[test]
fn test_compile_import_multiple_modules() -> Result<()> {
    let mut graph = Graph::new();
    
    // Import from multiple modules
    let import1 = graph.add_node(Node::Import {
        module_path: "math".to_string(),
        import_list: vec![ImportItem { name: "add".to_string(), alias: None }],
        import_all: false,
    });
    
    let import2 = graph.add_node(Node::Import {
        module_path: "string".to_string(),
        import_list: vec![ImportItem { name: "concat".to_string(), alias: None }],
        import_all: false,
    });
    
    // Use both imports
    let add_var = graph.add_node(Node::Variable { name: "add".to_string() });
    let one = graph.add_node(Node::Literal(Literal::Integer(1)));
    let two = graph.add_node(Node::Literal(Literal::Integer(2)));
    let add_app = graph.add_node(Node::Application {
        function: add_var,
        args: vec![one, two],
    });
    
    let seq = graph.add_node(Node::List(vec![import1, import2, add_app]));
    graph.root_id = Some(seq);
    
    compile_and_check(&graph)?;
    Ok(())
}

#[test]
fn test_compile_empty_module() -> Result<()> {
    let mut graph = Graph::new();
    
    // Empty module
    let body = graph.add_node(Node::Literal(Literal::Nil));
    let module_node = graph.add_node(Node::Module {
        name: "empty".to_string(),
        exports: vec![],
        body,
    });
    graph.root_id = Some(module_node);
    
    compile_and_check(&graph)?;
    Ok(())
}

#[test]
fn test_compile_qualified_function_call() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (math:add 5 3)
    let qualified_add = graph.add_node(Node::QualifiedVariable {
        module_name: "math".to_string(),
        variable_name: "add".to_string(),
    });
    
    let five = graph.add_node(Node::Literal(Literal::Integer(5)));
    let three = graph.add_node(Node::Literal(Literal::Integer(3)));
    
    let app = graph.add_node(Node::Application {
        function: qualified_add,
        args: vec![five, three],
    });
    graph.root_id = Some(app);
    
    compile_and_check(&graph)?;
    Ok(())
}