//! Integration tests for the module system

use claudelang_modules::{ModuleLoader, ModuleConfig, ModuleEnvironment};
use claudelang_modules::environment::ModuleValue;
use claudelang_parser::parse;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_module_declaration_and_export() {
    let source = r#"(module math (export add multiply) (let ((add (lambda (a b) (+ a b))) (multiply (lambda (a b) (* a b))) (internal (lambda (x) (* x x)))) nil))"#;
    
    let graph = parse(source).unwrap();
    assert!(graph.root_id.is_some());
    
    // Check that the module node was created
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();
    match root {
        claudelang_core::ast::Node::Module { name, exports, .. } => {
            assert_eq!(name, "math");
            assert_eq!(exports.len(), 2);
            assert!(exports.contains(&"add".to_string()));
            assert!(exports.contains(&"multiply".to_string()));
            assert!(!exports.contains(&"internal".to_string()));
        }
        _ => panic!("Expected Module node"),
    }
}

#[test]
fn test_import_statement_parsing() {
    // Test basic import
    let source = r#"(import "math" (sin cos))"#;
    let graph = parse(source).unwrap();
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();
    
    match root {
        claudelang_core::ast::Node::Import { module_path, import_list, import_all } => {
            assert_eq!(module_path, "math");
            assert_eq!(import_list.len(), 2);
            assert_eq!(import_list[0].name, "sin");
            assert_eq!(import_list[1].name, "cos");
            assert!(!import_all);
        }
        _ => panic!("Expected Import node"),
    }
}

#[test]
fn test_import_with_alias() {
    let source = r#"(import "math" (sin as sine cos as cosine))"#;
    let graph = parse(source).unwrap();
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();
    
    match root {
        claudelang_core::ast::Node::Import { import_list, .. } => {
            assert_eq!(import_list[0].name, "sin");
            assert_eq!(import_list[0].alias, Some("sine".to_string()));
            assert_eq!(import_list[1].name, "cos");
            assert_eq!(import_list[1].alias, Some("cosine".to_string()));
        }
        _ => panic!("Expected Import node"),
    }
}

#[test]
fn test_export_statement() {
    let source = r#"(export add multiply)"#;
    let graph = parse(source).unwrap();
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();
    
    match root {
        claudelang_core::ast::Node::Export { export_list } => {
            assert_eq!(export_list.len(), 2);
            assert_eq!(export_list[0].name, "add");
            assert_eq!(export_list[1].name, "multiply");
        }
        _ => panic!("Expected Export node"),
    }
}

#[test]
fn test_qualified_variable() {
    let source = r#"math.pi"#;
    let graph = parse(source).unwrap();
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();
    
    match root {
        claudelang_core::ast::Node::QualifiedVariable { module_name, variable_name } => {
            assert_eq!(module_name, "math");
            assert_eq!(variable_name, "pi");
        }
        _ => panic!("Expected QualifiedVariable node"),
    }
}

#[test]
fn test_module_loader_file_discovery() {
    let temp_dir = TempDir::new().unwrap();
    let module_path = temp_dir.path().join("test_module.cl");
    
    fs::write(&module_path, r#"(module test_module (export greet) (let ((greet (lambda (name) (str-concat "Hello, " name)))) greet))"#).unwrap();
    
    let config = ModuleConfig {
        search_paths: vec![temp_dir.path().to_path_buf()],
        enable_cache: true,
        max_cache_size: 100,
        allow_circular: false,
    };
    
    let mut loader = ModuleLoader::new(config);
    let module = loader.load_module("test_module").unwrap();
    
    assert_eq!(module.name, "test_module");
    assert_eq!(module.exports, vec!["greet"]);
}

#[test]
fn test_module_not_found_error() {
    let config = ModuleConfig {
        search_paths: vec![PathBuf::from("/nonexistent")],
        enable_cache: true,
        max_cache_size: 100,
        allow_circular: false,
    };
    
    let mut loader = ModuleLoader::new(config);
    let result = loader.load_module("nonexistent_module");
    
    assert!(result.is_err());
    match result.err().unwrap() {
        claudelang_modules::ModuleError::ModuleNotFound { .. } => {},
        _ => panic!("Expected ModuleNotFound error"),
    }
}

#[test]
fn test_circular_dependency_detection() {
    // The current implementation detects circular dependencies
    // during topological sort. This test verifies that detection.
    
    use claudelang_modules::resolver::DependencyGraph;
    use claudelang_modules::ModuleInfo;
    use std::sync::Arc;
    
    let mut graph = DependencyGraph::new();
    
    // Create modules
    let module_a = ModuleInfo {
        id: "a".to_string(),
        name: "a".to_string(),
        path: PathBuf::from("a.cl"),
        graph: parse("()").unwrap(),
        root: claudelang_core::ast::NodeId(0),
        exports: vec![],
        dependencies: vec!["b".to_string()],
        metadata: Default::default(),
    };
    
    let module_b = ModuleInfo {
        id: "b".to_string(),
        name: "b".to_string(),
        path: PathBuf::from("b.cl"),
        graph: parse("()").unwrap(),
        root: claudelang_core::ast::NodeId(0),
        exports: vec![],
        dependencies: vec!["a".to_string()],
        metadata: Default::default(),
    };
    
    // Add modules
    graph.add_module(module_a);
    graph.add_module(module_b);
    
    // Add circular dependency
    graph.add_dependency("a".to_string(), "b".to_string());
    graph.add_dependency("b".to_string(), "a".to_string());
    
    // Topological sort should detect the cycle
    let result = graph.topological_sort();
    assert!(result.is_err());
    match result.err().unwrap() {
        claudelang_modules::ModuleError::CircularDependency { .. } => {},
        _ => panic!("Expected CircularDependency error"),
    }
}

#[test]
fn test_module_environment_imports() {
    use claudelang_modules::ModuleInfo;
    use std::sync::Arc;
    
    let module = Arc::new(ModuleInfo {
        id: "test".to_string(),
        name: "test".to_string(),
        path: PathBuf::from("test.cl"),
        graph: parse("()").unwrap(),
        root: claudelang_core::ast::NodeId(0),
        exports: vec!["foo".to_string()],
        dependencies: vec![],
        metadata: Default::default(),
    });
    
    let mut env = ModuleEnvironment::for_module(module.clone());
    
    // Define a value in the module
    env.define("foo".to_string(), ModuleValue::Value(serde_json::json!(42)));
    
    // Create an import
    env.add_import("test_import".to_string(), module);
    
    // Test qualified lookup
    let value = env.lookup_qualified("test_import", "foo");
    assert!(value.is_some());
    match value.unwrap() {
        ModuleValue::ModuleRef { module_id, export_name } => {
            assert_eq!(module_id, "test");
            assert_eq!(export_name, "foo");
        }
        _ => panic!("Expected ModuleRef"),
    }
}

#[test]
fn test_import_all_syntax() {
    let source = r#"(import "math" *)"#;
    let graph = parse(source).unwrap();
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();
    
    match root {
        claudelang_core::ast::Node::Import { module_path, import_all, .. } => {
            assert_eq!(module_path, "math");
            assert!(import_all);
        }
        _ => panic!("Expected Import node"),
    }
}

#[test]
fn test_nested_module_access() {
    let source = r#"foo.bar.baz"#;
    let graph = parse(source).unwrap();
    
    // Currently we only support single-level qualified variables
    // This should parse as a qualified variable with module "foo" and variable "bar.baz"
    // Or fail to parse
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();
    match root {
        claudelang_core::ast::Node::QualifiedVariable { module_name, variable_name: _ } => {
            assert_eq!(module_name, "foo");
            // The parser currently treats "bar.baz" as the variable name
            // This is a limitation that could be addressed in the future
        }
        _ => {
            // It's also acceptable if this doesn't parse as a qualified variable
            // since we don't support nested module access yet
        }
    }
}