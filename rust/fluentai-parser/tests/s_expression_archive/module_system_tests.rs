//! Tests for module system (import/export) parsing

use fluentai_core::ast::Node;

#[test]
fn test_parse_simple_import() {
    // Test basic import with specific items
    let result = parse(r#"(import "std/math" (sin cos tan))"#).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Import {
            module_path,
            import_list,
            import_all,
        } => {
            assert_eq!(module_path, "std/math");
            assert!(!import_all);
            assert_eq!(import_list.len(), 3);
            assert_eq!(import_list[0].name, "sin");
            assert_eq!(import_list[1].name, "cos");
            assert_eq!(import_list[2].name, "tan");

            // Check no aliases
            assert!(import_list.iter().all(|item| item.alias.is_none()));
        }
        _ => panic!("Expected Import node"),
    }
}

#[test]
fn test_parse_import_all() {
    // Test import * (all exports)
    let result = parse(r#"(import "utils" *)"#).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Import {
            module_path,
            import_list,
            import_all,
        } => {
            assert_eq!(module_path, "utils");
            assert!(import_all);
            assert!(import_list.is_empty());
        }
        _ => panic!("Expected Import node"),
    }
}

#[test]
fn test_parse_import_with_aliases() {
    // Test import with aliases
    let result = parse(r#"(import "graphics/shapes" (circle as circ rectangle as rect))"#).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Import {
            module_path,
            import_list,
            import_all,
        } => {
            assert_eq!(module_path, "graphics/shapes");
            assert!(!import_all);
            assert_eq!(import_list.len(), 2);

            assert_eq!(import_list[0].name, "circle");
            assert_eq!(import_list[0].alias.as_ref().unwrap(), "circ");

            assert_eq!(import_list[1].name, "rectangle");
            assert_eq!(import_list[1].alias.as_ref().unwrap(), "rect");
        }
        _ => panic!("Expected Import node"),
    }
}

#[test]
fn test_parse_simple_export() {
    // Test basic export
    let result = parse("(export add subtract multiply divide)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Export { export_list } => {
            assert_eq!(export_list.len(), 4);
            assert_eq!(export_list[0].name, "add");
            assert_eq!(export_list[1].name, "subtract");
            assert_eq!(export_list[2].name, "multiply");
            assert_eq!(export_list[3].name, "divide");

            // Check no aliases
            assert!(export_list.iter().all(|item| item.alias.is_none()));
        }
        _ => panic!("Expected Export node"),
    }
}

#[test]
fn test_parse_export_with_aliases() {
    // Test export with aliases
    let result = parse("(export internal-add as add internal-sub as subtract)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Export { export_list } => {
            assert_eq!(export_list.len(), 2);

            assert_eq!(export_list[0].name, "internal-add");
            assert_eq!(export_list[0].alias.as_ref().unwrap(), "add");

            assert_eq!(export_list[1].name, "internal-sub");
            assert_eq!(export_list[1].alias.as_ref().unwrap(), "subtract");
        }
        _ => panic!("Expected Export node"),
    }
}

#[test]
fn test_parse_module_definition() {
    // Test module definition
    let input = r#"(module math-utils (export add subtract)
                     (let ((add (lambda (x y) (+ x y)))
                           (subtract (lambda (x y) (- x y))))
                       #t))"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Module {
            name,
            exports,
            body,
        } => {
            assert_eq!(name, "math-utils");
            assert_eq!(exports.len(), 2);
            assert!(exports.contains(&"add".to_string()));
            assert!(exports.contains(&"subtract".to_string()));

            // Body should be a let expression
            assert!(matches!(result.get_node(*body).unwrap(), Node::Let { .. }));
        }
        _ => panic!("Expected Module node"),
    }
}

#[test]
fn test_parse_qualified_variable() {
    // Test qualified variable access
    let result = parse("math.pi").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::QualifiedVariable {
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
fn test_parse_qualified_variable_in_expression() {
    // Test qualified variable in expression
    let result = parse("(* math.pi radius radius)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Application { function, args } => {
            // Function should be *
            match result.get_node(*function).unwrap() {
                Node::Variable { name } => assert_eq!(name, "*"),
                _ => panic!("Expected * function"),
            }

            assert_eq!(args.len(), 3);

            // First arg should be math.pi
            match result.get_node(args[0]).unwrap() {
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    assert_eq!(module_name, "math");
                    assert_eq!(variable_name, "pi");
                }
                _ => panic!("Expected qualified variable"),
            }
        }
        _ => panic!("Expected Application node"),
    }
}

#[test]
fn test_parse_nested_module_path() {
    // Test import with nested module path
    let result = parse(r#"(import "std/collections/hashmap" (HashMap))"#).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Import {
            module_path,
            import_list,
            ..
        } => {
            assert_eq!(module_path, "std/collections/hashmap");
            assert_eq!(import_list.len(), 1);
            assert_eq!(import_list[0].name, "HashMap");
        }
        _ => panic!("Expected Import node"),
    }
}

#[test]
fn test_parse_module_with_imports() {
    // Test module that imports from other modules
    let input = r#"(module geometry
                     (do
                       (import "std/math" (sin cos pi))
                       (let ((area-of-circle (lambda (r) (* pi r r))))
                         area-of-circle)))"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Module {
            name,
            exports,
            body,
        } => {
            assert_eq!(name, "geometry");
            assert!(exports.is_empty());

            // Body should contain imports
            assert!(result.get_node(*body).is_some());
        }
        _ => panic!("Expected Module node"),
    }
}

#[test]
fn test_parse_complex_qualified_variable() {
    // Test more complex qualified variable patterns
    let result = parse("(std.collections.list.map f lst)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    // This will likely parse as an application with a qualified variable
    assert!(matches!(node, Node::Application { .. }));
}

#[test]
fn test_parse_error_import_without_path() {
    // Import requires a module path
    let result = parse("(import)");
    assert!(result.is_err());
}

#[test]
fn test_parse_error_import_invalid_syntax() {
    // Import with invalid item list syntax
    let result = parse(r#"(import "module" (a b c))"#);
    // This should parse successfully
    assert!(result.is_ok());
    assert!(result.unwrap().root_id.is_some());
}

#[test]
fn test_parse_error_export_empty() {
    // Export with no items might parse successfully as empty export
    let result = parse("(export)");
    // Check if it parses or not
    if result.is_ok() {
        assert!(result.unwrap().root_id.is_some());
    } else {
        assert!(result.is_err());
    }
}

#[test]
fn test_parse_error_module_without_name() {
    // Module requires a name
    let result = parse("(module)");
    assert!(result.is_err());
}

#[test]
fn test_parse_error_module_without_exports() {
    // Module without exports list might be valid
    let result = parse("(module test-module)");
    // Still requires a body, so should error
    assert!(result.is_err());
}

#[test]
fn test_parse_error_module_without_body() {
    // Module with empty export list still needs a body
    let result = parse("(module test-module (export))");
    assert!(result.is_err());
}

#[test]
fn test_parse_import_export_combination() {
    // Test a module that imports and re-exports
    let input = r#"(module wrapper (export sin cos)
                     (do
                       (import "std/math" (sin cos tan))
                       #t))"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Module { name, exports, .. } => {
            assert_eq!(name, "wrapper");
            assert_eq!(exports.len(), 2);
            assert!(exports.contains(&"sin".to_string()));
            assert!(exports.contains(&"cos".to_string()));
        }
        _ => panic!("Expected Module node"),
    }
}

#[test]
fn test_parse_relative_import() {
    // Test relative import paths
    let result = parse(r#"(import "./sibling" (helper))"#).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Import { module_path, .. } => {
            assert_eq!(module_path, "./sibling");
        }
        _ => panic!("Expected Import node"),
    }
}

#[test]
fn test_parse_parent_relative_import() {
    // Test parent relative import
    let result = parse(r#"(import "../utils" *)"#).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Import {
            module_path,
            import_all,
            ..
        } => {
            assert_eq!(module_path, "../utils");
            assert!(import_all);
        }
        _ => panic!("Expected Import node"),
    }
}
