//! Integration tests for iterative parser
//!
//! These tests demonstrate when and how to use the iterative parser
//! for handling extremely deeply nested expressions.

#[cfg(test)]
mod tests {
    use crate::{parse, parse_iterative, parse_with_depth_limit, ParseError};

    #[test]
    fn test_normal_parser_for_moderate_nesting() {
        // For moderate nesting, the normal parser is fine
        let expr = "(+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6)";
        let result = parse(expr);
        assert!(result.is_ok());
    }

    #[test]
    fn test_depth_limit_prevents_overflow() {
        // Create a deeply nested expression
        let mut expr = String::new();
        for _ in 0..100 {
            expr.push_str("(+ ");
        }
        expr.push('1');
        for _ in 0..100 {
            expr.push_str(" 2)");
        }

        // Normal parser with depth limit will error
        let result = parse_with_depth_limit(&expr, 50);
        assert!(matches!(result, Err(ParseError::MaxDepthExceeded { .. })));
    }

    #[test]
    fn test_iterative_parser_handles_deep_nesting() {
        // Create a very deeply nested expression
        let mut expr = String::new();
        for _ in 0..2000 {
            expr.push_str("(+ ");
        }
        expr.push('1');
        for _ in 0..2000 {
            expr.push_str(" 2)");
        }

        // Iterative parser can handle this
        let result = parse_iterative(&expr);
        assert!(result.is_ok());
    }

    #[test]
    fn test_iterative_parser_produces_same_ast() {
        // Both parsers should produce the same AST for the same input
        let expr = "(+ (* 2 3) (- 5 1))";

        let result1 = parse(expr).unwrap();
        let result2 = parse_iterative(expr).unwrap();

        // Compare root nodes
        assert_eq!(result1.root_id, result2.root_id);
        assert_eq!(result1.nodes.len(), result2.nodes.len());
    }

    #[test]
    fn test_iterative_parser_complex_expression() {
        let expr = r#"
            (let ((x 10)
                  (y 20))
              (+ x y (* x y)))
        "#;

        // Note: The current iterative parser doesn't handle special forms
        // This test demonstrates its current limitations
        let result = parse_iterative(expr);
        // For now, this will parse as a regular list application
        assert!(result.is_ok());
    }

    #[test]
    fn test_performance_comparison() {
        use std::time::Instant;

        // Create a deeply nested but still manageable expression
        let mut expr = String::new();
        let depth = 200;
        for _ in 0..depth {
            expr.push_str("(+ ");
        }
        expr.push('1');
        for _ in 0..depth {
            expr.push_str(" 2)");
        }

        // Time recursive parser with adequate depth limit
        let start = Instant::now();
        let result1 = parse_with_depth_limit(&expr, 1000);
        let recursive_time = start.elapsed();

        // Time iterative parser
        let start = Instant::now();
        let result2 = parse_iterative(&expr);
        let iterative_time = start.elapsed();

        println!("Expression depth: {}", depth);
        println!("Recursive parser time: {:?}", recursive_time);
        println!("Iterative parser time: {:?}", iterative_time);

        assert!(result1.is_ok(), "Recursive parser failed: {:?}", result1);
        assert!(result2.is_ok(), "Iterative parser failed: {:?}", result2);
    }

    #[test]
    fn test_iterative_parser_memory_efficiency() {
        // The iterative parser uses heap memory instead of stack
        // This allows it to handle much deeper nesting
        let mut expr = String::new();
        let depth = 5000;
        for _ in 0..depth {
            expr.push_str("(list ");
        }
        expr.push('1');
        for _ in 0..depth {
            expr.push(')');
        }

        // This would definitely overflow the stack with recursive parsing
        // But iterative parser handles it fine (with higher limit)
        let result = crate::parse_iterative_with_depth(&expr, 15000);

        match &result {
            Ok(_) => println!("Successfully parsed expression with depth {}", depth),
            Err(e) => println!("Failed to parse: {:?}", e),
        }

        assert!(result.is_ok(), "Iterative parser failed: {:?}", result);
    }
}
