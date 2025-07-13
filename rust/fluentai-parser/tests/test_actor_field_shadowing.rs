use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

#[test]
fn test_actor_field_shadowing_with_let() {
    let source = r#"
        private actor Counter {
            count: int = 0;
            
            private handle inc() {
                let count = 100;
                count
            }
            
            private handle get() {
                count
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with field shadowing: {:?}", result);
    
    let graph = result.unwrap();
    
    // Check that in inc handler, 'count' is NOT transformed to get_count()
    let has_local_count_var = graph.nodes.values().any(|node| {
        if let Node::Define { name, value } = node {
            if name == "handle_inc" {
                // Check the handler body for a Variable node with name "count"
                // (not get_count)
                return check_for_variable(&graph, *value, "count");
            }
        }
        false
    });
    
    assert!(has_local_count_var, "Local variable 'count' should not be transformed to getter");
    
    // Check that in get handler, 'count' IS transformed to get_count()
    let has_getter_call = graph.nodes.values().any(|node| {
        if let Node::Define { name, value } = node {
            if name == "handle_get" {
                // Check the handler body for a Variable node with name "get_count"
                // Also check for application with getter
                return check_for_getter_application(&graph, *value, "get_count");
            }
        }
        false
    });
    
    
    assert!(has_getter_call, "Field 'count' should be transformed to get_count()");
}

#[test]
fn test_actor_field_shadowing_with_lambda() {
    let source = r#"
        private actor Calculator {
            value: float = 0.0;
            
            private handle process(items: List<float>) {
                // Lambda parameter shadows field
                items.map(value => value * 2.0)  // 'value' here is lambda param, not field
            }
            
            private handle get_field_value() {
                value  // This should transform to get_value()
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with lambda shadowing: {:?}", result);
    
    let graph = result.unwrap();
    
    // Verify that the lambda parameter 'value' is not transformed
    let has_lambda_with_value_param = graph.nodes.values().any(|node| {
        if let Node::Lambda { params, .. } = node {
            return params.contains(&"value".to_string());
        }
        false
    });
    
    assert!(has_lambda_with_value_param, "Lambda should have 'value' parameter");
    
    // Verify that the field access in get_field_value is transformed
    let has_get_value_getter = graph.nodes.values().any(|node| {
        if let Node::Define { name, value } = node {
            if name == "handle_get_field_value" {
                return check_for_getter_application(&graph, *value, "get_value");
            }
        }
        false
    });
    
    assert!(has_get_value_getter, "Field 'value' should be transformed to get_value()");
}

#[test]
fn test_actor_nested_shadowing() {
    let source = r#"
        private actor Complex {
            data: string = "initial";
            count: int = 0;
            
            private handle complex_shadowing() {
                let data = "local";  // Shadows field
                let result = {
                    let count = 42;  // Shadows field in inner scope
                    f"{data}:{count}"  // Should use local variables
                };
                f"{data}:{count}"  // data is local, count is field (get_count)
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with nested shadowing: {:?}", result);
}

// Helper function to check if a Variable node with specific name exists in the AST
fn check_for_variable(graph: &fluentai_core::ast::Graph, node_id: fluentai_core::ast::NodeId, var_name: &str) -> bool {
    if let Some(node) = graph.nodes.get(&node_id) {
        match node {
            Node::Variable { name } => name == var_name,
            Node::Lambda { body, .. } => check_for_variable(graph, *body, var_name),
            Node::Let { body, .. } => check_for_variable(graph, *body, var_name),
            Node::Application { function, args } => {
                check_for_variable(graph, *function, var_name) ||
                args.iter().any(|arg| check_for_variable(graph, *arg, var_name))
            }
            Node::Begin { exprs } => {
                exprs.iter().any(|expr| check_for_variable(graph, *expr, var_name))
            }
            _ => false,
        }
    } else {
        false
    }
}

// Helper function to check if a getter application exists (e.g., get_count(state))
fn check_for_getter_application(graph: &fluentai_core::ast::Graph, node_id: fluentai_core::ast::NodeId, getter_name: &str) -> bool {
    if let Some(node) = graph.nodes.get(&node_id) {
        match node {
            Node::Application { function, args } => {
                // Check if the function is the getter we're looking for
                if let Some(func_node) = graph.nodes.get(function) {
                    if let Node::Variable { name } = func_node {
                        if name == getter_name {
                            return true;
                        }
                    }
                }
                // Also check recursively in args
                args.iter().any(|arg| check_for_getter_application(graph, *arg, getter_name))
            }
            Node::Lambda { body, .. } => check_for_getter_application(graph, *body, getter_name),
            Node::Let { body, .. } => check_for_getter_application(graph, *body, getter_name),
            Node::Begin { exprs } => {
                exprs.iter().any(|expr| check_for_getter_application(graph, *expr, getter_name))
            }
            _ => false,
        }
    } else {
        false
    }
}

