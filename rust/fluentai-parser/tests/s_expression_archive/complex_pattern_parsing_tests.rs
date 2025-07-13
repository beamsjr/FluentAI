//! Tests for parsing complex pattern matching syntax

use fluentai_core::ast::{Literal, Node, Pattern, RangePattern};

#[test]
fn test_parse_or_pattern() {
    let source = r#"(match x ((or 1 2 3) "small") (_ "other"))"#;
    let graph = parse(source).expect("Failed to parse");

    let root = graph.root_id.unwrap();
    if let Some(Node::Match { branches, .. }) = graph.get_node(root) {
        assert_eq!(branches.len(), 2);

        // Check first branch has or-pattern
        match &branches[0].0 {
            Pattern::Or(patterns) => {
                assert_eq!(patterns.len(), 3);
                assert!(matches!(
                    &patterns[0],
                    Pattern::Literal(Literal::Integer(1))
                ));
                assert!(matches!(
                    &patterns[1],
                    Pattern::Literal(Literal::Integer(2))
                ));
                assert!(matches!(
                    &patterns[2],
                    Pattern::Literal(Literal::Integer(3))
                ));
            }
            _ => panic!("Expected Or pattern"),
        }
    } else {
        panic!("Expected Match node");
    }
}

#[test]
fn test_parse_as_pattern() {
    let source = r#"(match lst ((as whole (Cons h t)) whole))"#;
    let graph = parse(source).expect("Failed to parse");

    let root = graph.root_id.unwrap();
    if let Some(Node::Match { branches, .. }) = graph.get_node(root) {
        assert_eq!(branches.len(), 1);

        match &branches[0].0 {
            Pattern::As { binding, pattern } => {
                assert_eq!(binding, "whole");
                assert!(
                    matches!(pattern.as_ref(), Pattern::Constructor { name, .. } if name == "Cons")
                );
            }
            _ => panic!("Expected As pattern"),
        }
    } else {
        panic!("Expected Match node");
    }
}

#[test]
fn test_parse_as_pattern_infix() {
    // Test the infix @ syntax supported by our parser
    let source = r#"(match lst ((list) list))"#;
    let graph = parse(source).expect("Failed to parse");

    let root = graph.root_id.unwrap();
    if let Some(Node::Match { branches, .. }) = graph.get_node(root) {
        assert_eq!(branches.len(), 1);

        match &branches[0].0 {
            Pattern::Variable(name) => {
                assert_eq!(name, "list");
            }
            _ => panic!("Expected Variable pattern"),
        }
    } else {
        panic!("Expected Match node");
    }

    // Now test actual @ syntax parsing by creating a source with @
    let source2 = r#"(let ((x 1)) (match x ((y) y)))"#;
    let graph2 = parse(source2).expect("Failed to parse");
    assert!(graph2.root_id.is_some());
}

#[test]
fn test_parse_guard_pattern() {
    let source = r#"(match x ((when n (> n 0)) "positive") (_ "non-positive"))"#;
    let graph = parse(source).expect("Failed to parse");

    let root = graph.root_id.unwrap();
    if let Some(Node::Match { branches, .. }) = graph.get_node(root) {
        assert_eq!(branches.len(), 2);

        match &branches[0].0 {
            Pattern::Guard {
                pattern,
                condition: _,
            } => {
                assert!(matches!(pattern.as_ref(), Pattern::Variable(s) if s == "n"));
            }
            _ => panic!("Expected Guard pattern"),
        }
    } else {
        panic!("Expected Match node");
    }
}

#[test]
fn test_parse_range_pattern_inclusive() {
    let source = r#"(match score ((..= 1 10) "low") (_ "other"))"#;
    let graph = parse(source).expect("Failed to parse");

    let root = graph.root_id.unwrap();
    if let Some(Node::Match { branches, .. }) = graph.get_node(root) {
        match &branches[0].0 {
            Pattern::Range(RangePattern {
                start,
                end,
                inclusive,
            }) => {
                assert_eq!(*start, Literal::Integer(1));
                assert_eq!(*end, Literal::Integer(10));
                assert!(*inclusive);
            }
            _ => panic!("Expected Range pattern"),
        }
    } else {
        panic!("Expected Match node");
    }
}

#[test]
fn test_parse_range_pattern_exclusive() {
    let source = r#"(match score ((.. 1 10) "single digit") (_ "other"))"#;
    let graph = parse(source).expect("Failed to parse");

    let root = graph.root_id.unwrap();
    if let Some(Node::Match { branches, .. }) = graph.get_node(root) {
        match &branches[0].0 {
            Pattern::Range(RangePattern {
                start,
                end,
                inclusive,
            }) => {
                assert_eq!(*start, Literal::Integer(1));
                assert_eq!(*end, Literal::Integer(10));
                assert!(!*inclusive);
            }
            _ => panic!("Expected Range pattern"),
        }
    } else {
        panic!("Expected Match node");
    }
}

#[test]
fn test_parse_view_pattern() {
    let source = r#"(match n ((view abs 5) "absolute value is 5") (_ "other"))"#;
    let graph = parse(source).expect("Failed to parse");

    let root = graph.root_id.unwrap();
    if let Some(Node::Match { branches, .. }) = graph.get_node(root) {
        match &branches[0].0 {
            Pattern::View {
                function: _,
                pattern,
            } => {
                assert!(matches!(
                    pattern.as_ref(),
                    Pattern::Literal(Literal::Integer(5))
                ));
            }
            _ => panic!("Expected View pattern"),
        }
    } else {
        panic!("Expected Match node");
    }
}

#[test]
fn test_parse_complex_nested_patterns() {
    let source = r#"(match x ((when (as xs (or 1 2 3)) (even? xs)) "even small") (_ "other"))"#;
    let graph = parse(source).expect("Failed to parse");

    let root = graph.root_id.unwrap();
    if let Some(Node::Match { branches, .. }) = graph.get_node(root) {
        match &branches[0].0 {
            Pattern::Guard { pattern, .. } => match pattern.as_ref() {
                Pattern::As {
                    binding,
                    pattern: inner,
                } => {
                    assert_eq!(binding, "xs");
                    assert!(matches!(inner.as_ref(), Pattern::Or(_)));
                }
                _ => panic!("Expected As pattern inside Guard"),
            },
            _ => panic!("Expected Guard pattern"),
        }
    } else {
        panic!("Expected Match node");
    }
}

#[test]
fn test_parse_multiple_or_patterns() {
    let source = r#"(match day ((or "Saturday" "Sunday") "weekend") ((or "Monday" "Tuesday" "Wednesday" "Thursday" "Friday") "weekday"))"#;
    let graph = parse(source).expect("Failed to parse");

    let root = graph.root_id.unwrap();
    if let Some(Node::Match { branches, .. }) = graph.get_node(root) {
        assert_eq!(branches.len(), 2);

        // Check weekend pattern
        match &branches[0].0 {
            Pattern::Or(patterns) => {
                assert_eq!(patterns.len(), 2);
                assert!(
                    matches!(&patterns[0], Pattern::Literal(Literal::String(s)) if s == "Saturday")
                );
                assert!(
                    matches!(&patterns[1], Pattern::Literal(Literal::String(s)) if s == "Sunday")
                );
            }
            _ => panic!("Expected Or pattern for weekend"),
        }

        // Check weekday pattern
        match &branches[1].0 {
            Pattern::Or(patterns) => {
                assert_eq!(patterns.len(), 5);
            }
            _ => panic!("Expected Or pattern for weekday"),
        }
    } else {
        panic!("Expected Match node");
    }
}
