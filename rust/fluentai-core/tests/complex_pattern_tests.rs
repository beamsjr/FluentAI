//! Tests for complex pattern matching features

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, Pattern, RangePattern};

#[test]
fn test_guard_pattern_creation() {
    let mut graph = Graph::new();
    let condition = graph
        .add_node(Node::Literal(Literal::Boolean(true)))
        .unwrap();

    let guard_pattern = Pattern::guard(Pattern::var("x"), condition);

    match guard_pattern {
        Pattern::Guard {
            pattern,
            condition: cond,
        } => {
            assert!(matches!(*pattern, Pattern::Variable(ref s) if s == "x"));
            assert_eq!(cond, condition);
        }
        _ => panic!("Expected Guard pattern"),
    }
}

#[test]
fn test_as_pattern_creation() {
    let as_pattern =
        Pattern::as_pattern("list", Pattern::cons(Pattern::var("h"), Pattern::var("t")));

    match as_pattern {
        Pattern::As { binding, pattern } => {
            assert_eq!(binding, "list");
            assert!(matches!(*pattern, Pattern::Constructor { ref name, .. } if name == "cons"));
        }
        _ => panic!("Expected As pattern"),
    }
}

#[test]
fn test_or_pattern_creation() {
    let or_pattern = Pattern::or(vec![Pattern::int(1), Pattern::int(2), Pattern::int(3)]);

    match or_pattern {
        Pattern::Or(patterns) => {
            assert_eq!(patterns.len(), 3);
            for (i, p) in patterns.iter().enumerate() {
                assert!(matches!(p, Pattern::Literal(Literal::Integer(n)) if *n == (i + 1) as i64));
            }
        }
        _ => panic!("Expected Or pattern"),
    }
}

#[test]
fn test_range_pattern_creation() {
    // Test inclusive range
    let inclusive_range = Pattern::range_inclusive(Literal::Integer(1), Literal::Integer(10));
    match inclusive_range {
        Pattern::Range(RangePattern {
            start,
            end,
            inclusive,
        }) => {
            assert_eq!(start, Literal::Integer(1));
            assert_eq!(end, Literal::Integer(10));
            assert!(inclusive);
        }
        _ => panic!("Expected Range pattern"),
    }

    // Test exclusive range
    let exclusive_range = Pattern::range_exclusive(Literal::Integer(1), Literal::Integer(10));
    match exclusive_range {
        Pattern::Range(RangePattern {
            start,
            end,
            inclusive,
        }) => {
            assert_eq!(start, Literal::Integer(1));
            assert_eq!(end, Literal::Integer(10));
            assert!(!inclusive);
        }
        _ => panic!("Expected Range pattern"),
    }

    // Test int_range helper
    let int_range = Pattern::int_range(5, 15);
    match int_range {
        Pattern::Range(RangePattern {
            start,
            end,
            inclusive,
        }) => {
            assert_eq!(start, Literal::Integer(5));
            assert_eq!(end, Literal::Integer(15));
            assert!(inclusive);
        }
        _ => panic!("Expected Range pattern"),
    }
}

#[test]
fn test_view_pattern_creation() {
    let mut graph = Graph::new();
    let abs_fn = graph
        .add_node(Node::Variable {
            name: "abs".to_string(),
        })
        .unwrap();

    let view_pattern = Pattern::view(abs_fn, Pattern::int(5));

    match view_pattern {
        Pattern::View { function, pattern } => {
            assert_eq!(function, abs_fn);
            assert!(matches!(*pattern, Pattern::Literal(Literal::Integer(5))));
        }
        _ => panic!("Expected View pattern"),
    }
}

#[test]
fn test_complex_nested_patterns() {
    let mut graph = Graph::new();
    let condition = graph
        .add_node(Node::Literal(Literal::Boolean(true)))
        .unwrap();

    // Create a complex nested pattern: xs @ (1 | 2 | 3) when condition
    let or_pattern = Pattern::or(vec![Pattern::int(1), Pattern::int(2), Pattern::int(3)]);
    let as_pattern = Pattern::as_pattern("xs", or_pattern);
    let guard_pattern = Pattern::guard(as_pattern, condition);

    // Verify the structure
    match guard_pattern {
        Pattern::Guard {
            pattern,
            condition: _,
        } => match pattern.as_ref() {
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
}

#[test]
fn test_match_builder_with_guard() -> Result<()> {
    let mut graph = Graph::new();

    let value = graph.add_node(Node::Variable {
        name: "x".to_string(),
    })?;
    let gt_fn = graph.add_node(Node::Variable {
        name: ">".to_string(),
    })?;
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)))?;
    let gt_zero = graph.add_node(Node::Application {
        function: gt_fn,
        args: vec![value, zero],
    })?;

    let positive = graph.add_node(Node::Literal(Literal::String("positive".to_string())))?;
    let non_positive =
        graph.add_node(Node::Literal(Literal::String("non-positive".to_string())))?;

    // Build match with guard
    let match_node = graph
        .build_match()
        .expr(value)
        .branch(Pattern::guard(Pattern::var("x"), gt_zero), positive)
        .default(non_positive)
        .build()?;

    assert!(graph.get_node(match_node).is_some());
    Ok(())
}

#[test]
fn test_match_builder_with_or_pattern() -> Result<()> {
    let mut graph = Graph::new();

    let value = graph.add_node(Node::Variable {
        name: "day".to_string(),
    })?;
    let weekend = graph.add_node(Node::Literal(Literal::String("weekend".to_string())))?;
    let weekday = graph.add_node(Node::Literal(Literal::String("weekday".to_string())))?;

    // Match Saturday or Sunday
    let match_node = graph
        .build_match()
        .expr(value)
        .branch(
            Pattern::or(vec![Pattern::string("Saturday"), Pattern::string("Sunday")]),
            weekend,
        )
        .default(weekday)
        .build()?;

    assert!(graph.get_node(match_node).is_some());
    Ok(())
}

#[test]
fn test_match_builder_with_range() -> Result<()> {
    let mut graph = Graph::new();

    let score = graph.add_node(Node::Variable {
        name: "score".to_string(),
    })?;
    let grade_a = graph.add_node(Node::Literal(Literal::String("A".to_string())))?;
    let grade_b = graph.add_node(Node::Literal(Literal::String("B".to_string())))?;
    let grade_c = graph.add_node(Node::Literal(Literal::String("C".to_string())))?;
    let grade_f = graph.add_node(Node::Literal(Literal::String("F".to_string())))?;

    // Grade based on score ranges
    let match_node = graph
        .build_match()
        .expr(score)
        .branch(Pattern::int_range(90, 100), grade_a)
        .branch(Pattern::int_range(80, 89), grade_b)
        .branch(Pattern::int_range(70, 79), grade_c)
        .default(grade_f)
        .build()?;

    assert!(graph.get_node(match_node).is_some());
    Ok(())
}

#[test]
fn test_match_builder_with_as_pattern() -> Result<()> {
    let mut graph = Graph::new();

    let one = graph.add_node(Node::Literal(Literal::Integer(1)))?;
    let two = graph.add_node(Node::Literal(Literal::Integer(2)))?;
    let list = graph.add_node(Node::List(vec![one, two]))?;

    let whole_list = graph.add_node(Node::Variable {
        name: "whole".to_string(),
    })?;
    let empty_result = graph.add_node(Node::Literal(Literal::Nil))?;

    // Match non-empty list and bind both the whole list and its parts
    let match_node = graph
        .build_match()
        .expr(list)
        .branch(
            Pattern::as_pattern("whole", Pattern::cons(Pattern::var("h"), Pattern::var("t"))),
            whole_list,
        )
        .branch(Pattern::nil(), empty_result)
        .build()?;

    assert!(graph.get_node(match_node).is_some());
    Ok(())
}

#[test]
fn test_complex_pattern_combination() -> Result<()> {
    let mut graph = Graph::new();

    let input = graph.add_node(Node::Variable {
        name: "input".to_string(),
    })?;

    // Create various results
    let small_even = graph.add_node(Node::Literal(Literal::String("small even".to_string())))?;
    let small_odd = graph.add_node(Node::Literal(Literal::String("small odd".to_string())))?;
    let medium = graph.add_node(Node::Literal(Literal::String("medium".to_string())))?;
    let large = graph.add_node(Node::Literal(Literal::String("large".to_string())))?;
    let other = graph.add_node(Node::Literal(Literal::String("other".to_string())))?;

    // Create guard conditions
    let even_fn = graph.add_node(Node::Variable {
        name: "even?".to_string(),
    })?;
    let is_even = graph.add_node(Node::Application {
        function: even_fn,
        args: vec![input],
    })?;

    let odd_fn = graph.add_node(Node::Variable {
        name: "odd?".to_string(),
    })?;
    let is_odd = graph.add_node(Node::Application {
        function: odd_fn,
        args: vec![input],
    })?;

    // Complex pattern matching with ranges, guards, and or-patterns
    let match_node = graph
        .build_match()
        .expr(input)
        .branch(
            Pattern::guard(Pattern::int_range(1, 10), is_even),
            small_even,
        )
        .branch(Pattern::guard(Pattern::int_range(1, 10), is_odd), small_odd)
        .branch(Pattern::int_range(11, 50), medium)
        .branch(
            Pattern::or(vec![
                Pattern::int_range(51, 100),
                Pattern::int_range(101, 1000),
            ]),
            large,
        )
        .default(other)
        .build()?;

    assert!(graph.get_node(match_node).is_some());

    // Verify the match structure
    if let Some(Node::Match { branches, .. }) = graph.get_node(match_node) {
        assert_eq!(branches.len(), 5); // 4 specific + 1 default

        // Check first branch is a guarded range
        assert!(matches!(&branches[0].0, Pattern::Guard { .. }));
    }

    Ok(())
}
