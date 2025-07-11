//! Comprehensive test suite for FLC parser

use fluentai_core::ast::{Graph, Node, NodeId};
use fluentai_parser::flc_parser::Parser as FlcParser;
use fluentai_parser::parser::Parser as SExprParser;

/// Compare two AST graphs for structural equality
fn compare_graphs(flc_graph: &Graph, sexpr_graph: &Graph) -> bool {
    match (flc_graph.root_id, sexpr_graph.root_id) {
        (Some(flc_root), Some(sexpr_root)) => compare_nodes(flc_graph, flc_root, sexpr_graph, sexpr_root),
        (None, None) => true,
        _ => false,
    }
}

/// Compare two nodes for structural equality
fn compare_nodes(flc_graph: &Graph, flc_id: NodeId, sexpr_graph: &Graph, sexpr_id: NodeId) -> bool {
    let flc_node = flc_graph.nodes.get(&flc_id);
    let sexpr_node = sexpr_graph.nodes.get(&sexpr_id);
    
    match (flc_node, sexpr_node) {
        (Some(flc), Some(sexpr)) => match (flc, sexpr) {
            (Node::Literal(lit1), Node::Literal(lit2)) => lit1 == lit2,
            (Node::Variable { name: n1 }, Node::Variable { name: n2 }) => n1 == n2,
            (Node::Lambda { params: p1, body: b1 }, Node::Lambda { params: p2, body: b2 }) => {
                p1 == p2 && compare_nodes(flc_graph, *b1, sexpr_graph, *b2)
            }
            (Node::Application { function: f1, args: a1 }, Node::Application { function: f2, args: a2 }) => {
                compare_nodes(flc_graph, *f1, sexpr_graph, *f2) &&
                a1.len() == a2.len() &&
                a1.iter().zip(a2.iter()).all(|(id1, id2)| compare_nodes(flc_graph, *id1, sexpr_graph, *id2))
            }
            (Node::Let { bindings: b1, body: body1 }, Node::Let { bindings: b2, body: body2 }) => {
                b1.len() == b2.len() &&
                b1.iter().zip(b2.iter()).all(|((n1, v1), (n2, v2))| {
                    n1 == n2 && compare_nodes(flc_graph, *v1, sexpr_graph, *v2)
                }) &&
                compare_nodes(flc_graph, *body1, sexpr_graph, *body2)
            }
            (Node::If { condition: c1, then_branch: t1, else_branch: e1 },
             Node::If { condition: c2, then_branch: t2, else_branch: e2 }) => {
                compare_nodes(flc_graph, *c1, sexpr_graph, *c2) &&
                compare_nodes(flc_graph, *t1, sexpr_graph, *t2) &&
                compare_nodes(flc_graph, *e1, sexpr_graph, *e2)
            }
            (Node::List(items1), Node::List(items2)) => {
                items1.len() == items2.len() &&
                items1.iter().zip(items2.iter()).all(|(id1, id2)| compare_nodes(flc_graph, *id1, sexpr_graph, *id2))
            }
            (Node::Begin { exprs: exprs1 }, Node::Begin { exprs: exprs2 }) => {
                exprs1.len() == exprs2.len() &&
                exprs1.iter().zip(exprs2.iter()).all(|(id1, id2)| compare_nodes(flc_graph, *id1, sexpr_graph, *id2))
            }
            (Node::Define { name: n1, value: v1 },
             Node::Define { name: n2, value: v2 }) => {
                n1 == n2 && compare_nodes(flc_graph, *v1, sexpr_graph, *v2)
            }
            _ => false, // Different node types
        },
        _ => false,
    }
}

#[test]
fn test_literals() {
    let test_cases = vec![
        ("42", "42"),
        ("3.14", "3.14"),
        ("\"hello\"", "\"hello\""),
        ("true", "true"),
        ("false", "false"),
        ("nil", "nil"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_arithmetic() {
    let test_cases = vec![
        ("1 + 2", "(+ 1 2)"),
        ("1 + 2 + 3", "(+ (+ 1 2) 3)"),
        ("1 + 2 * 3", "(+ 1 (* 2 3))"),
        ("(1 + 2) * 3", "(* (+ 1 2) 3)"),
        ("1 - 2 / 3", "(- 1 (/ 2 3))"),
        ("a + b * c", "(+ a (* b c))"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_comparison() {
    let test_cases = vec![
        ("x == y", "(== x y)"),
        ("x != y", "(!= x y)"),
        ("x < y", "(< x y)"),
        ("x > y", "(> x y)"),
        ("x <= y", "(<= x y)"),
        ("x >= y", "(>= x y)"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_logical() {
    let test_cases = vec![
        ("a && b", "(and a b)"),
        ("a || b", "(or a b)"),
        ("!a", "(not a)"),
        ("a && b || c", "(or (and a b) c)"),
        ("a || b && c", "(or a (and b c))"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_function_calls() {
    let test_cases = vec![
        ("print(\"hello\")", "(print \"hello\")"),
        ("add(1, 2)", "(add 1 2)"),
        ("foo()", "(foo)"),
        ("bar(x, y, z)", "(bar x y z)"),
        ("f(g(x))", "(f (g x))"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_method_calls() {
    let test_cases = vec![
        ("list.map(f)", "(map f list)"),
        ("list.filter(pred)", "(filter pred list)"),
        ("list.map(f).filter(pred)", "(filter pred (map f list))"),
        ("obj.method()", "(method obj)"),
        ("obj.method(arg)", "(method obj arg)"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_lambda() {
    let test_cases = vec![
        ("{ |x| x }", "(lambda (x) x)"),
        ("{ |x| x * 2 }", "(lambda (x) (* x 2))"),
        ("{ |x, y| x + y }", "(lambda (x y) (+ x y))"),
        ("{ || 42 }", "(lambda () 42)"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_let_bindings() {
    let test_cases = vec![
        ("let x = 10; x", "(let ((x 10)) x)"),
        ("let x = 10; let y = 20; x + y", "(let ((x 10) (y 20)) (+ x y))"),
        ("let x = 10; x + 1", "(let ((x 10)) (+ x 1))"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_if_expression() {
    let test_cases = vec![
        ("if (x) { y }", "(if x y)"),
        ("if (x) { y } else { z }", "(if x y z)"),
        ("if (x > 0) { x } else { -x }", "(if (> x 0) x (neg x))"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_lists() {
    let test_cases = vec![
        ("[1, 2, 3]", "(list 1 2 3)"),
        ("[]", "(list)"),
        ("[a, b, c]", "(list a b c)"),
        ("[[1, 2], [3, 4]]", "(list (list 1 2) (list 3 4))"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_pipe_operator() {
    let test_cases = vec![
        ("x |> f", "(f x)"),
        ("x |> f |> g", "(g (f x))"),
        ("list |> map(f) |> filter(pred)", "(filter pred (map f list))"),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}

#[test]
fn test_function_definition() {
    let test_cases = vec![
        ("def fn add(x, y) { x + y }", "(define (add x y) (+ x y))"),
        ("def fn square(x) { x * x }", "(define (square x) (* x x))"),
        ("def pub fn foo() { 42 }", "(define (foo) 42)"), // Note: exported flag handled separately
    ];
    
    for (flc, sexpr) in test_cases {
        let _flc_graph = FlcParser::new(flc).parse().unwrap();
        let _sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        // For function definitions, we need more detailed comparison
        // TODO: Add proper comparison for functions
    }
}

#[test]
fn test_complex_expressions() {
    let test_cases = vec![
        (
            "list.map { |x| x * 2 }.filter { |x| x > 10 }", 
            "(filter (lambda (x) (> x 10)) (map (lambda (x) (* x 2)) list))"
        ),
        (
            "if (x > 0) { x } else { if (x < 0) { -x } else { 0 } }",
            "(if (> x 0) x (if (< x 0) (neg x) 0))"
        ),
        (
            "let f = { |x| x + 1 }; f(10)",
            "(let ((f (lambda (x) (+ x 1)))) (f 10))"
        ),
    ];
    
    for (flc, sexpr) in test_cases {
        let flc_graph = FlcParser::new(flc).parse().unwrap();
        let sexpr_graph = SExprParser::new(sexpr).parse().unwrap();
        assert!(compare_graphs(&flc_graph, &sexpr_graph), 
                "Failed to match: FLC '{}' vs S-expr '{}'", flc, sexpr);
    }
}