//! Tests for effect primitive detection in the optimizer

use fluentai_core::ast::{EffectType, Graph, Literal, Node, NodeId};
use fluentai_optimizer::analysis::{is_effect_primitive, EffectAnalysis};
use fluentai_parser::parse_flc;
use rustc_hash::FxHashSet;
use std::num::NonZeroU32;

#[test]
fn test_io_effect_primitives() {
    // Test IO effect primitives
    let io_primitives = vec![
        ("print", EffectType::IO),
        ("println", EffectType::IO),
        ("read-line", EffectType::IO),
        ("read-file", EffectType::IO),
        ("write-file", EffectType::IO),
        ("append-file", EffectType::IO),
        ("delete-file", EffectType::IO),
        ("file-exists?", EffectType::IO),
        ("display", EffectType::IO),
        ("newline", EffectType::IO),
    ];

    for (prim, expected_effect) in io_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(
            effect,
            Some(expected_effect),
            "Primitive '{}' should have {:?} effect",
            prim,
            expected_effect
        );
    }
}

#[test]
fn test_state_effect_primitives() {
    // Test State effect primitives
    let state_primitives = vec![
        ("set!", EffectType::State),
        ("ref", EffectType::State),
        ("ref-set!", EffectType::State),
        ("ref-get", EffectType::State),
        ("atom", EffectType::State),
        ("swap!", EffectType::State),
        ("reset!", EffectType::State),
        ("compare-and-set!", EffectType::State),
    ];

    for (prim, expected_effect) in state_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(
            effect,
            Some(expected_effect),
            "Primitive '{}' should have {:?} effect",
            prim,
            expected_effect
        );
    }
}

#[test]
fn test_error_effect_primitives() {
    // Test Error effect primitives
    let error_primitives = vec![
        ("raise", EffectType::Error),
        ("error", EffectType::Error),
        ("throw", EffectType::Error),
        ("assert", EffectType::Error),
        ("panic", EffectType::Error),
    ];

    for (prim, expected_effect) in error_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(
            effect,
            Some(expected_effect),
            "Primitive '{}' should have {:?} effect",
            prim,
            expected_effect
        );
    }
}

#[test]
fn test_time_effect_primitives() {
    // Test Time effect primitives
    let time_primitives = vec![
        ("sleep", EffectType::Time),
        ("current-time", EffectType::Time),
        ("current-milliseconds", EffectType::Time),
    ];

    for (prim, expected_effect) in time_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(
            effect,
            Some(expected_effect),
            "Primitive '{}' should have {:?} effect",
            prim,
            expected_effect
        );
    }
}

#[test]
fn test_random_effect_primitives() {
    // Test Random effect primitives that mutate state
    let random_primitives = vec![
        ("random", EffectType::Random),
        ("random-int", EffectType::Random),
        ("random-float", EffectType::Random),
        ("random-seed!", EffectType::Random),
    ];

    for (prim, expected_effect) in random_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(
            effect,
            Some(expected_effect),
            "Primitive '{}' should have {:?} effect",
            prim,
            expected_effect
        );
    }
}

#[test]
fn test_network_effect_primitives() {
    // Test Network effect primitives
    let network_primitives = vec![
        ("http-get", EffectType::Network),
        ("http-post", EffectType::Network),
        ("http-put", EffectType::Network),
        ("http-delete", EffectType::Network),
        ("fetch", EffectType::Network),
        ("websocket-connect", EffectType::Network),
        ("tcp-connect", EffectType::Network),
    ];

    for (prim, expected_effect) in network_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(
            effect,
            Some(expected_effect),
            "Primitive '{}' should have {:?} effect",
            prim,
            expected_effect
        );
    }
}

#[test]
fn test_async_effect_primitives() {
    // Test Async effect primitives
    let async_primitives = vec![
        ("spawn", EffectType::Async),
        ("await", EffectType::Async),
        ("promise", EffectType::Async),
        ("future", EffectType::Async),
        ("async", EffectType::Async),
    ];

    for (prim, expected_effect) in async_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(
            effect,
            Some(expected_effect),
            "Primitive '{}' should have {:?} effect",
            prim,
            expected_effect
        );
    }
}

#[test]
fn test_concurrent_effect_primitives() {
    // Test Concurrent effect primitives
    let concurrent_primitives = vec![
        ("channel", EffectType::Concurrent),
        ("chan-send!", EffectType::Concurrent),
        ("chan-receive", EffectType::Concurrent),
        ("mutex", EffectType::Concurrent),
        ("lock!", EffectType::Concurrent),
        ("unlock!", EffectType::Concurrent),
        ("thread-spawn", EffectType::Concurrent),
    ];

    for (prim, expected_effect) in concurrent_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(
            effect,
            Some(expected_effect),
            "Primitive '{}' should have {:?} effect",
            prim,
            expected_effect
        );
    }
}

#[test]
fn test_pure_primitives_have_no_effects() {
    // Test that pure primitives return None
    let pure_primitives = vec![
        "+",
        "-",
        "*",
        "/",
        "mod",
        "<",
        ">",
        "<=",
        ">=",
        "=",
        "!=",
        "and",
        "or",
        "not",
        "car",
        "cdr",
        "cons",
        "list",
        "list-len",
        "list-empty?",
        "str-len",
        "str-concat",
        "str-upper",
        "str-lower",
        "abs",
        "min",
        "max",
        "sqrt",
    ];

    for prim in pure_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(effect, None, "Primitive '{}' should have no effects", prim);
    }
}

#[test]
fn test_effect_detection_in_simple_application() {
    // Test that effect primitives are detected in function applications
    let code = "(print \"hello world\")";
    let ast = parse_flc(code).unwrap();

    let analysis = EffectAnalysis::analyze(&ast);

    // The application node should have IO effects
    let app_node = ast
        .nodes
        .iter()
        .find(|(_, node)| matches!(node, Node::Application { .. }));

    assert!(app_node.is_some());
    let (app_id, _) = app_node.unwrap();

    assert!(!analysis.pure_nodes.contains(app_id));
    assert!(analysis.node_effects[app_id].contains(&EffectType::IO));
}

#[test]
fn test_effect_detection_in_nested_application() {
    // Test effects in nested expressions
    let code = "(+ 1 (print \"side effect\") 3)";
    let ast = parse_flc(code).unwrap();

    let analysis = EffectAnalysis::analyze(&ast);

    // The outer + application should have effects from the print
    if let Some(root_id) = ast.root_id {
        assert!(!analysis.pure_nodes.contains(&root_id));
        assert!(analysis.node_effects[&root_id].contains(&EffectType::IO));
    }
}

#[test]
fn test_multiple_effects_in_expression() {
    // Test detection of multiple effect types
    let code = "(list (print \"io\") (set! x 42) (random))";
    let ast = parse_flc(code).unwrap();

    let analysis = EffectAnalysis::analyze(&ast);

    // The list should have IO, State, and Random effects
    if let Some(root_id) = ast.root_id {
        let effects = &analysis.node_effects[&root_id];
        assert!(effects.contains(&EffectType::IO));
        assert!(effects.contains(&EffectType::State));
        assert!(effects.contains(&EffectType::Random));
    }
}

#[test]
fn test_effect_propagation_through_let() {
    // Test that effects propagate through let bindings
    let code = r#"
        (let ((x (print "effect in binding")))
          (+ x 10))
    "#;
    let ast = parse_flc(code).unwrap();

    let analysis = EffectAnalysis::analyze(&ast);

    // The let node should have effects
    let let_node = ast
        .nodes
        .iter()
        .find(|(_, node)| matches!(node, Node::Let { .. }));

    if let Some((let_id, _)) = let_node {
        assert!(!analysis.pure_nodes.contains(let_id));
        assert!(analysis.node_effects[let_id].contains(&EffectType::IO));
    }
}

#[test]
fn test_effect_propagation_through_if() {
    // Test that effects propagate through conditionals
    let code = r#"
        (if (> x 0)
            (print "positive")
            (error "non-positive"))
    "#;
    let ast = parse_flc(code).unwrap();

    let analysis = EffectAnalysis::analyze(&ast);

    // The if node should have both IO and Error effects
    let if_node = ast
        .nodes
        .iter()
        .find(|(_, node)| matches!(node, Node::If { .. }));

    if let Some((if_id, _)) = if_node {
        let effects = &analysis.node_effects[if_id];
        assert!(effects.contains(&EffectType::IO));
        assert!(effects.contains(&EffectType::Error));
    }
}

#[test]
fn test_effect_in_function_argument() {
    // Test effects in function arguments are detected
    let code = "(map print (list 1 2 3))";
    let ast = parse_flc(code).unwrap();

    let analysis = EffectAnalysis::analyze(&ast);

    // The map application should be marked as effectful
    // because print has effects
    if let Some(root_id) = ast.root_id {
        // Note: This depends on how we want to handle higher-order functions
        // For now, just verify the analysis completes
        assert!(analysis.node_effects.contains_key(&root_id));
    }
}

#[test]
fn test_pure_expression_optimization_hint() {
    // Test that pure expressions are marked for optimization
    let code = "(+ (* 2 3) (/ 10 2))";
    let ast = parse_flc(code).unwrap();

    let analysis = EffectAnalysis::analyze(&ast);

    // All nodes should be pure and const-evaluable
    for (node_id, node) in &ast.nodes {
        if matches!(node, Node::Application { .. } | Node::Literal(_)) {
            assert!(analysis.pure_nodes.contains(node_id));
            assert!(analysis.const_evaluable.contains(node_id));
        }
    }
}

#[test]
fn test_mixed_pure_and_effectful() {
    // Test mixed pure and effectful expressions
    let code = r#"
        (let ((pure-val (* 2 3))
              (effect-val (read-line)))
          (+ pure-val (str-len effect-val)))
    "#;
    let ast = parse_flc(code).unwrap();

    let analysis = EffectAnalysis::analyze(&ast);

    // Find the multiplication - should be pure
    let mult_node = ast.nodes.iter().find(|(_, node)| {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = ast.get_node(*function) {
                name == "*"
            } else {
                false
            }
        } else {
            false
        }
    });

    if let Some((mult_id, _)) = mult_node {
        assert!(analysis.pure_nodes.contains(mult_id));
        assert!(analysis.const_evaluable.contains(mult_id));
    }

    // The overall expression should have effects
    if let Some(root_id) = ast.root_id {
        assert!(!analysis.pure_nodes.contains(&root_id));
        assert!(analysis.node_effects[&root_id].contains(&EffectType::IO));
    }
}

#[test]
fn test_effect_free_lambda_body() {
    // Test that lambda definitions themselves are pure
    // even if their body contains effect primitives
    let code = "(lambda (x) (print x))";
    let ast = parse_flc(code).unwrap();

    let analysis = EffectAnalysis::analyze(&ast);

    // The lambda node itself should be pure
    let lambda_node = ast
        .nodes
        .iter()
        .find(|(_, node)| matches!(node, Node::Lambda { .. }));

    if let Some((lambda_id, _)) = lambda_node {
        assert!(analysis.pure_nodes.contains(lambda_id));
    }
}

#[test]
fn test_effect_primitive_variations() {
    // Test various naming conventions for effect primitives
    let variations = vec![
        ("display", Some(EffectType::IO)),
        ("PRINT", None),                       // Case sensitive
        ("print!", None),                      // Not a recognized variant
        ("set-car!", Some(EffectType::State)), // Mutation primitive
        ("vector-set!", Some(EffectType::State)),
    ];

    for (name, expected) in variations {
        let effect = is_effect_primitive(name);
        assert_eq!(
            effect, expected,
            "Primitive '{}' effect detection mismatch",
            name
        );
    }
}

#[test]
fn test_dom_effect_primitives() {
    // Test DOM effect primitives
    let dom_primitives = vec![
        ("dom-get-element", EffectType::Dom),
        ("dom-create-element", EffectType::Dom),
        ("dom-set-attribute", EffectType::Dom),
        ("dom-add-event-listener", EffectType::Dom),
        ("dom-remove-element", EffectType::Dom),
        ("dom-query-selector", EffectType::Dom),
    ];

    for (prim, expected_effect) in dom_primitives {
        let effect = is_effect_primitive(prim);
        assert_eq!(
            effect,
            Some(expected_effect),
            "Primitive '{}' should have {:?} effect",
            prim,
            expected_effect
        );
    }
}
