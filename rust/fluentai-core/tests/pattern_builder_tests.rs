//! Tests for the new pattern matching builder APIs

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, Pattern};

#[test]
fn test_pattern_builder_methods() {
    // Test all pattern builder methods
    let var_pattern = Pattern::var("x");
    assert!(matches!(var_pattern, Pattern::Variable(ref s) if s == "x"));

    let int_pattern = Pattern::int(42);
    assert!(matches!(
        int_pattern,
        Pattern::Literal(Literal::Integer(42))
    ));

    let string_pattern = Pattern::string("hello");
    assert!(matches!(string_pattern, Pattern::Literal(Literal::String(ref s)) if s == "hello"));

    let bool_pattern = Pattern::bool(true);
    assert!(matches!(
        bool_pattern,
        Pattern::Literal(Literal::Boolean(true))
    ));

    let nil_lit_pattern = Pattern::nil_lit();
    assert!(matches!(nil_lit_pattern, Pattern::Literal(Literal::Nil)));

    let wildcard_pattern = Pattern::wildcard();
    assert!(matches!(wildcard_pattern, Pattern::Wildcard));

    let underscore_pattern = Pattern::underscore();
    assert!(matches!(underscore_pattern, Pattern::Wildcard));
}

#[test]
fn test_cons_and_nil_patterns() {
    let nil_pattern = Pattern::nil();
    assert!(
        matches!(nil_pattern, Pattern::Constructor { ref name, ref patterns } if name == "nil" && patterns.is_empty())
    );

    let cons_pattern = Pattern::cons(Pattern::var("h"), Pattern::var("t"));
    match cons_pattern {
        Pattern::Constructor { name, patterns } => {
            assert_eq!(name, "cons");
            assert_eq!(patterns.len(), 2);
            assert!(matches!(patterns[0], Pattern::Variable(ref s) if s == "h"));
            assert!(matches!(patterns[1], Pattern::Variable(ref s) if s == "t"));
        }
        _ => panic!("Expected constructor pattern"),
    }
}

#[test]
fn test_custom_constructor_pattern() {
    let custom = Pattern::constructor("Point", vec![Pattern::var("x"), Pattern::var("y")]);
    match custom {
        Pattern::Constructor { name, patterns } => {
            assert_eq!(name, "Point");
            assert_eq!(patterns.len(), 2);
        }
        _ => panic!("Expected constructor pattern"),
    }
}

#[test]
fn test_match_builder_simple() -> Result<()> {
    let mut graph = Graph::new();

    // Create a value to match
    let value = graph.add_node(Node::Literal(Literal::Integer(42)))?;
    let result1 = graph.add_node(Node::Literal(Literal::String("forty-two".to_string())))?;
    let result2 = graph.add_node(Node::Literal(Literal::String("not 42".to_string())))?;

    // Use the match builder
    let match_node = graph
        .build_match()
        .expr(value)
        .int_case(42, result1)
        .default(result2)
        .build()?;

    // Verify the match node was created
    assert!(graph.get_node(match_node).is_some());
    if let Some(Node::Match { expr, branches }) = graph.get_node(match_node) {
        assert_eq!(*expr, value);
        assert_eq!(branches.len(), 2);
        assert!(matches!(
            branches[0].0,
            Pattern::Literal(Literal::Integer(42))
        ));
        assert!(matches!(branches[1].0, Pattern::Wildcard));
    } else {
        panic!("Expected Match node");
    }

    Ok(())
}

#[test]
fn test_match_builder_multiple_cases() -> Result<()> {
    let mut graph = Graph::new();

    let value = graph.add_node(Node::Variable {
        name: "input".to_string(),
    })?;
    let r1 = graph.add_node(Node::Literal(Literal::String("one".to_string())))?;
    let r2 = graph.add_node(Node::Literal(Literal::String("two".to_string())))?;
    let r3 = graph.add_node(Node::Literal(Literal::String("three".to_string())))?;
    let r4 = graph.add_node(Node::Literal(Literal::String("hello".to_string())))?;
    let r5 = graph.add_node(Node::Literal(Literal::String("yes".to_string())))?;
    let default = graph.add_node(Node::Literal(Literal::String("other".to_string())))?;

    let match_node = graph
        .build_match()
        .expr(value)
        .int_case(1, r1)
        .int_case(2, r2)
        .int_case(3, r3)
        .string_case("hello", r4)
        .bool_case(true, r5)
        .default(default)
        .build()?;

    if let Some(Node::Match { branches, .. }) = graph.get_node(match_node) {
        assert_eq!(branches.len(), 6);
    } else {
        panic!("Expected Match node");
    }

    Ok(())
}

#[test]
fn test_match_builder_with_patterns() -> Result<()> {
    let mut graph = Graph::new();

    let list = graph.add_node(Node::List(vec![]))?;
    let empty_result = graph.add_node(Node::Literal(Literal::String("empty".to_string())))?;
    let cons_result = graph.add_node(Node::Literal(Literal::String("has items".to_string())))?;

    let match_node = graph
        .build_match()
        .expr(list)
        .branch(Pattern::nil(), empty_result)
        .branch(
            Pattern::cons(Pattern::var("h"), Pattern::var("t")),
            cons_result,
        )
        .build()?;

    assert!(graph.get_node(match_node).is_some());
    Ok(())
}

#[test]
fn test_match_builder_error_no_expr() {
    let mut graph = Graph::new();
    let result = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();

    let match_result = graph.build_match().int_case(1, result).build();

    assert!(match_result.is_err());
    assert!(match_result
        .unwrap_err()
        .to_string()
        .contains("Match expression required"));
}

#[test]
fn test_match_builder_error_no_branches() -> Result<()> {
    let mut graph = Graph::new();
    let expr = graph.add_node(Node::Literal(Literal::Integer(1)))?;

    let match_result = graph.build_match().expr(expr).build();

    assert!(match_result.is_err());
    assert!(match_result
        .unwrap_err()
        .to_string()
        .contains("at least one branch"));
    Ok(())
}

#[test]
fn test_match_value_helper() -> Result<()> {
    let mut graph = Graph::new();

    let value = graph.add_node(Node::Literal(Literal::Integer(2)))?;
    let r1 = graph.add_node(Node::Literal(Literal::String("one".to_string())))?;
    let r2 = graph.add_node(Node::Literal(Literal::String("two".to_string())))?;
    let r3 = graph.add_node(Node::Literal(Literal::String("three".to_string())))?;
    let default = graph.add_node(Node::Literal(Literal::String("other".to_string())))?;

    let match_node = graph.match_value(
        value,
        vec![
            (Literal::Integer(1), r1),
            (Literal::Integer(2), r2),
            (Literal::Integer(3), r3),
        ],
        default,
    )?;

    if let Some(Node::Match { expr, branches }) = graph.get_node(match_node) {
        assert_eq!(*expr, value);
        assert_eq!(branches.len(), 4); // 3 cases + default
        assert!(matches!(branches[3].0, Pattern::Wildcard));
    } else {
        panic!("Expected Match node");
    }

    Ok(())
}

#[test]
fn test_match_list_helper() -> Result<()> {
    let mut graph = Graph::new();

    // Create a list
    let one = graph.add_node(Node::Literal(Literal::Integer(1)))?;
    let two = graph.add_node(Node::Literal(Literal::Integer(2)))?;
    let list = graph.add_node(Node::List(vec![one, two]))?;

    let empty_result = graph.add_node(Node::Literal(Literal::String("empty".to_string())))?;

    let match_node = graph.match_list(list, empty_result, |_g, head, _tail| {
        // Return the head of the list
        Ok(head)
    })?;

    if let Some(Node::Match { expr, branches }) = graph.get_node(match_node) {
        assert_eq!(*expr, list);
        assert_eq!(branches.len(), 2);
        // First branch should be nil pattern
        assert!(matches!(branches[0].0, Pattern::Constructor { ref name, .. } if name == "nil"));
        // Second branch should be cons pattern
        assert!(matches!(branches[1].0, Pattern::Constructor { ref name, .. } if name == "cons"));
    } else {
        panic!("Expected Match node");
    }

    Ok(())
}

#[test]
fn test_complex_pattern_matching_example() -> Result<()> {
    let mut graph = Graph::new();

    // Create a nested list structure
    let one = graph.add_node(Node::Literal(Literal::Integer(1)))?;
    let two = graph.add_node(Node::Literal(Literal::Integer(2)))?;
    let inner_list = graph.add_node(Node::List(vec![one, two]))?;

    let outer_list = graph.add_node(Node::List(vec![inner_list]))?;

    let empty_result = graph.add_node(Node::Literal(Literal::String("empty".to_string())))?;
    let has_nested = graph.add_node(Node::Literal(Literal::String(
        "has nested list".to_string(),
    )))?;

    // Match on the outer list
    let match_node = graph
        .build_match()
        .expr(outer_list)
        .branch(Pattern::nil(), empty_result)
        .branch(
            Pattern::cons(Pattern::var("first"), Pattern::var("rest")),
            has_nested,
        )
        .build()?;

    assert!(graph.get_node(match_node).is_some());
    Ok(())
}

#[test]
fn test_old_vs_new_api_comparison() -> Result<()> {
    let mut graph1 = Graph::new();
    let mut graph2 = Graph::new();

    // Old way of creating pattern matching
    let value1 = graph1.add_node(Node::Literal(Literal::Integer(42)))?;
    let result1_1 = graph1.add_node(Node::Literal(Literal::String("forty-two".to_string())))?;
    let result2_1 = graph1.add_node(Node::Literal(Literal::String("not 42".to_string())))?;

    let match_node_old = graph1.add_node(Node::Match {
        expr: value1,
        branches: vec![
            (Pattern::Literal(Literal::Integer(42)), result1_1),
            (Pattern::Wildcard, result2_1),
        ],
    })?;

    // New way using builder API
    let value2 = graph2.add_node(Node::Literal(Literal::Integer(42)))?;
    let result1_2 = graph2.add_node(Node::Literal(Literal::String("forty-two".to_string())))?;
    let result2_2 = graph2.add_node(Node::Literal(Literal::String("not 42".to_string())))?;

    let match_node_new = graph2
        .build_match()
        .expr(value2)
        .int_case(42, result1_2)
        .default(result2_2)
        .build()?;

    // Both should create equivalent structures
    assert!(graph1.get_node(match_node_old).is_some());
    assert!(graph2.get_node(match_node_new).is_some());

    Ok(())
}
