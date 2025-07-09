//! Tests for parser depth tracking to prevent stack overflow

#[cfg(test)]
mod tests {
    use crate::{ParseError, Parser};

    #[test]
    fn test_depth_limit_prevents_stack_overflow() {
        // Create deeply nested expression that would cause stack overflow
        let mut input = String::new();
        let depth = 2000;

        // Build nested expression: (+ (+ (+ ... 1 ...)))
        for _ in 0..depth {
            input.push_str("(+ ");
        }
        input.push('1');
        for _ in 0..depth {
            input.push_str(" 2)");
        }

        // Parse with default depth limit
        let mut parser = Parser::new(&input);
        let result = parser.parse();

        // Should fail with max depth error
        assert!(result.is_err());
        if let Err(ParseError::MaxDepthExceeded { depth, max_depth }) = result {
            assert!(depth >= 1000);
            assert_eq!(max_depth, 1000);
        } else {
            panic!("Expected MaxDepthExceeded error");
        }
    }

    #[test]
    fn test_custom_depth_limit() {
        // Create nested expression
        let mut input = String::new();
        let depth = 50;

        for _ in 0..depth {
            input.push_str("(list ");
        }
        input.push('1');
        for _ in 0..depth {
            input.push(')');
        }

        // Parse with custom depth limit
        let mut parser = Parser::new(&input).with_max_depth(30);
        let result = parser.parse();

        // Should fail with max depth error
        assert!(result.is_err());
        if let Err(ParseError::MaxDepthExceeded { depth, max_depth }) = result {
            assert!(depth >= 30);
            assert_eq!(max_depth, 30);
        } else {
            panic!("Expected MaxDepthExceeded error, got: {:?}", result);
        }
    }

    #[test]
    fn test_reasonable_depth_succeeds() {
        // Create moderately nested expression
        let mut input = String::new();
        let depth = 10;

        for _ in 0..depth {
            input.push_str("(list ");
        }
        input.push('1');
        for _ in 0..depth {
            input.push(')');
        }

        // Parse with default depth limit
        let mut parser = Parser::new(&input);
        let result = parser.parse();

        // Should succeed
        assert!(result.is_ok());
    }

    #[test]
    fn test_depth_tracking_in_let_bindings() {
        // Test depth tracking through let bindings
        let mut input = String::new();
        let depth = 100;

        // Build: (let ((a (let ((b (let ... ))))))
        for i in 0..depth {
            input.push_str(&format!("(let ((x{} ", i));
        }
        input.push('1');
        for i in 0..depth {
            input.push_str(&format!(")) x{})", i));
        }

        // Parse with custom limit
        let mut parser = Parser::new(&input).with_max_depth(50);
        let result = parser.parse();

        // Should fail
        assert!(matches!(result, Err(ParseError::MaxDepthExceeded { .. })));
    }

    #[test]
    fn test_depth_reset_between_expressions() {
        // Test that depth is properly reset between top-level expressions
        let input = r#"
            (list 1 2 3)
            (list 4 5 6)
            (list 7 8 9)
        "#;

        let mut parser = Parser::new(input).with_max_depth(10);
        let result = parser.parse();

        // Should succeed - each expression is parsed independently
        assert!(result.is_ok());
    }
}
