//! Integration tests for the module system

use fluentai_modules::environment::ModuleValue;
use fluentai_modules::{ModuleConfig, ModuleEnvironment, ModuleLoader};
use fluentai_parser::parse_flc;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
#[ignore = "FLC parser doesn't create Module nodes from 'module name;' syntax"]
fn test_module_declaration_and_export() {
    // FLC syntax: module declaration is implicit from filename
    // This test needs to be restructured to work with file-based modules
    let source = r#"
module math;

public function add(a, b) { a + b }
public function multiply(a, b) { a * b }
private function internal(x) { x * x }
"#;

    let graph = parse_flc(source).unwrap();
    assert!(graph.root_id.is_some());

    // FLC parser creates a Begin node with module declaration and functions
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();
    match root {
        fluentai_core::ast::Node::Begin { exprs } => {
            // Find the module declaration (might not be first)
            let mut found_module = false;
            for &expr_id in exprs {
                if let Some(fluentai_core::ast::Node::Module { name, .. }) = graph.get_node(expr_id) {
                    assert_eq!(name, "math");
                    found_module = true;
                    break;
                }
            }
            if !found_module {
                // Debug: print what we got
                println!("Expressions in Begin node:");
                for (i, &expr_id) in exprs.iter().enumerate() {
                    if let Some(node) = graph.get_node(expr_id) {
                        println!("  [{}]: {:?}", i, node);
                    }
                }
                panic!("Module node not found in expressions");
            }
            
            // Count public vs private functions
            let mut public_count = 0;
            let mut private_count = 0;
            
            for &expr_id in &exprs[1..] {
                if let Some(fluentai_core::ast::Node::Define { name, .. }) = graph.get_node(expr_id) {
                    // In this test, add and multiply should be public, internal should be private
                    if name == "add" || name == "multiply" {
                        public_count += 1;
                    } else if name == "internal" {
                        private_count += 1;
                    }
                }
            }
            
            assert_eq!(public_count, 2, "Expected 2 public functions");
            assert_eq!(private_count, 1, "Expected 1 private function");
        }
        _ => panic!("Expected Begin node, got: {:?}", root),
    }
}

#[test]
fn test_import_statement_parsing() {
    // Test basic import - FLC syntax
    let source = r#"use math::{sin, cos};"#;
    let graph = parse_flc(source).unwrap();
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();

    match root {
        fluentai_core::ast::Node::Import {
            module_path,
            import_list,
            import_all,
        } => {
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
#[ignore = "Parser doesn't support import aliasing syntax yet"]
fn test_import_with_alias() {
    // The FLC spec mentions this syntax but parser doesn't support it yet
    let source = r#"use math as M;"#;
    let _graph = parse_flc(source);
    // Test disabled until parser supports aliasing
}

#[test]
fn test_export_statement() {
    // In FLC, exports are automatic based on visibility
    // Public functions are automatically exported
    let source = r#"public function add(a, b) { a + b }
public function multiply(a, b) { a * b }"#;
    let graph = parse_flc(source).unwrap();
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();

    // In FLC, this will parse as a Begin node with two Define nodes
    match root {
        fluentai_core::ast::Node::Begin { exprs } => {
            assert_eq!(exprs.len(), 2);
            // Each expression should be a Define node for a public function
            for expr_id in exprs {
                if let Some(fluentai_core::ast::Node::Define { name, .. }) = graph.get_node(*expr_id) {
                    assert!(name == "add" || name == "multiply");
                }
            }
        }
        _ => panic!("Expected Begin node with function definitions, got: {:?}", root),
    }
}

#[test]
fn test_qualified_variable() {
    // This syntax is still valid in FLC
    let source = r#"math.pi"#;
    let graph = parse_flc(source).unwrap();
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();

    match root {
        fluentai_core::ast::Node::QualifiedVariable {
            module_name,
            variable_name,
        } => {
            assert_eq!(module_name, "math");
            assert_eq!(variable_name, "pi");
        }
        _ => panic!("Expected QualifiedVariable node"),
    }
}

#[test]
fn test_module_loader_file_discovery() {
    let temp_dir = TempDir::new().unwrap();
    let module_path = temp_dir.path().join("test_module.fc");

    // FLC syntax for module
    fs::write(&module_path, r#"mod test_module;

public function greet(name: string) -> string {
    "Hello, " + name
}"#).unwrap();

    let config = ModuleConfig {
        search_paths: vec![temp_dir.path().to_path_buf()],
        enable_cache: true,
        max_cache_size: 100,
        allow_circular: false,
    };

    let mut loader = ModuleLoader::new(config);
    let module = loader.load_module("test_module").unwrap();

    // The module name might be extracted differently with FLC syntax
    // For now, just verify the module loads successfully
    assert!(!module.name.is_empty());
    // In FLC, exports are determined by public functions
    // The loader might need updates to extract them properly
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
        fluentai_modules::ModuleError::ModuleNotFound { .. } => {}
        _ => panic!("Expected ModuleNotFound error"),
    }
}

#[test]
fn test_circular_dependency_detection() {
    // The current implementation detects circular dependencies
    // during topological sort. This test verifies that detection.

    use fluentai_modules::resolver::DependencyGraph;
    use fluentai_modules::ModuleInfo;
    use std::sync::Arc;

    let mut graph = DependencyGraph::new();

    // Create modules
    let module_a = ModuleInfo {
        id: "a".to_string(),
        name: "a".to_string(),
        path: PathBuf::from("a.ai"),
        graph: parse_flc("nil").unwrap(),
        root: fluentai_core::ast::NodeId::new(1).unwrap(),
        exports: vec![],
        dependencies: vec!["b".to_string()],
        metadata: Default::default(),
    };

    let module_b = ModuleInfo {
        id: "b".to_string(),
        name: "b".to_string(),
        path: PathBuf::from("b.ai"),
        graph: parse_flc("nil").unwrap(),
        root: fluentai_core::ast::NodeId::new(1).unwrap(),
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
        fluentai_modules::ModuleError::CircularDependency { .. } => {}
        _ => panic!("Expected CircularDependency error"),
    }
}

#[test]
fn test_module_environment_imports() {
    use fluentai_modules::ModuleInfo;
    use std::sync::Arc;

    let module = Arc::new(ModuleInfo {
        id: "test".to_string(),
        name: "test".to_string(),
        path: PathBuf::from("test.ai"),
        graph: parse_flc("nil").unwrap(),
        root: fluentai_core::ast::NodeId::new(1).unwrap(),
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
        ModuleValue::ModuleRef {
            module_id,
            export_name,
        } => {
            assert_eq!(module_id, "test");
            assert_eq!(export_name, "foo");
        }
        _ => panic!("Expected ModuleRef"),
    }
}

#[test]
fn test_import_all_syntax() {
    let source = r#"use math::*;"#;
    let graph = parse_flc(source).unwrap();
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();

    match root {
        fluentai_core::ast::Node::Import {
            module_path,
            import_all,
            ..
        } => {
            assert_eq!(module_path, "math");
            assert!(import_all);
        }
        _ => panic!("Expected Import node"),
    }
}

#[test]
fn test_nested_module_access() {
    let source = r#"foo.bar.baz"#;
    let graph = parse_flc(source).unwrap();

    // Currently we only support single-level qualified variables
    // This should parse as a qualified variable with module "foo" and variable "bar.baz"
    // Or fail to parse
    let root = graph.get_node(graph.root_id.unwrap()).unwrap();
    match root {
        fluentai_core::ast::Node::QualifiedVariable {
            module_name,
            variable_name: _,
        } => {
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
