use fluentai_core::ast::{Graph, ImportItem, Literal, Node};
use fluentai_modules::{ModuleConfig, ModuleLoader};
use fluentai_vm::{Compiler, CompilerOptions, OptimizationLevel, VMBuilder, VMError, Value, VM};
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Helper to create a test module file
fn create_test_module(dir: &Path, name: &str, content: &str) -> PathBuf {
    let file_path = dir.join(format!("{}.ai", name));
    fs::write(&file_path, content).unwrap();
    file_path
}

#[test]
#[ignore = "Module loading requires define or set-global to be implemented for creating exportable bindings"]
fn test_module_loading_and_execution() {
    // Create a temporary directory for test modules
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    // Create a math module
    // For now, we'll create an empty module that exports will be handled differently
    let math_file = create_test_module(
        temp_path, "math", r#"
nil
"#,
    );
    println!("Created math module at: {:?}", math_file);
    assert!(math_file.exists(), "Math module file should exist");

    // Create main code that imports and uses the module
    let mut graph = Graph::new();

    // First create the function calls that will use imported functions
    let five = graph.add_node(Node::Literal(Literal::Integer(5))).unwrap();
    let double_var = graph
        .add_node(Node::Variable {
            name: "double".to_string(),
        })
        .unwrap();
    let call_double = graph
        .add_node(Node::Application {
            function: double_var,
            args: vec![five],
        })
        .unwrap();

    // Import math module - this will be the root node
    let import_node = graph
        .add_node(Node::Import {
            module_path: "math".to_string(),
            import_list: vec![
                ImportItem {
                    name: "double".to_string(),
                    alias: None,
                },
                ImportItem {
                    name: "square".to_string(),
                    alias: None,
                },
            ],
            import_all: false,
        })
        .unwrap();

    // The import node returns nil, so we need to sequence it with the actual call
    // Let's create a let binding that imports and then calls
    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![("_".to_string(), import_node)],
            body: call_double,
        })
        .unwrap();

    graph.root_id = Some(let_node);

    // Compile and run
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();

    // Configure module loader with temp directory
    let mut module_config = ModuleConfig::default();
    module_config.search_paths = vec![temp_path.to_path_buf()];

    // Build VM with module loader
    let module_loader = ModuleLoader::new(module_config);
    let mut vm = VMBuilder::new()
        .with_bytecode(bytecode)
        .with_module_loader(module_loader)
        .build()
        .unwrap();

    let result = vm.run().unwrap();

    // Should return 10 (double of 5)
    match result {
        Value::Integer(n) => assert_eq!(n, 10),
        _ => panic!("Expected integer result, got {:?}", result),
    }
}

#[test]
#[ignore = "Module loading requires define or set-global to be implemented for creating exportable bindings"]
fn test_module_import_all() {
    // Create a temporary directory for test modules
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    // Create a utils module
    create_test_module(
        temp_path,
        "utils",
        r#"
(let ((add1 (lambda (x) (+ x 1)))
      (sub1 (lambda (x) (- x 1))))
  (export add1 sub1)
  nil)
"#,
    );

    // Create main code that imports all from utils
    let mut graph = Graph::new();

    // Use imported function: (add1 10)
    let ten = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
    let add1_var = graph
        .add_node(Node::Variable {
            name: "add1".to_string(),
        })
        .unwrap();
    let call_add1 = graph
        .add_node(Node::Application {
            function: add1_var,
            args: vec![ten],
        })
        .unwrap();

    // Import all from utils module
    let import_node = graph
        .add_node(Node::Import {
            module_path: "utils".to_string(),
            import_list: vec![],
            import_all: true,
        })
        .unwrap();

    // Sequence import and call
    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![("_".to_string(), import_node)],
            body: call_add1,
        })
        .unwrap();

    graph.root_id = Some(let_node);

    // Compile and run
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();

    // Configure module loader with temp directory
    let mut module_config = ModuleConfig::default();
    module_config.search_paths = vec![temp_path.to_path_buf()];

    // Build VM with module loader
    let module_loader = ModuleLoader::new(module_config);
    let mut vm = VMBuilder::new()
        .with_bytecode(bytecode)
        .with_module_loader(module_loader)
        .build()
        .unwrap();

    let result = vm.run().unwrap();

    // Should return 11 (10 + 1)
    match result {
        Value::Integer(n) => assert_eq!(n, 11),
        _ => panic!("Expected integer result, got {:?}", result),
    }
}

#[test]
fn test_module_not_found_error() {
    let mut graph = Graph::new();

    // Try to import non-existent module
    let import_node = graph
        .add_node(Node::Import {
            module_path: "nonexistent".to_string(),
            import_list: vec![],
            import_all: true,
        })
        .unwrap();

    graph.root_id = Some(import_node);

    // Compile and run
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();

    let mut vm = VM::new(bytecode);
    let result = vm.run();

    // Should fail with module error
    assert!(result.is_err());
    if let Err(e) = result {
        match e {
            VMError::ModuleError { module_name, .. } => {
                assert_eq!(module_name, "nonexistent");
            }
            _ => panic!("Expected ModuleError, got {:?}", e),
        }
    }
}
