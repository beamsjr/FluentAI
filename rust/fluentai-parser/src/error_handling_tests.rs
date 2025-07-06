//! Tests for parser error handling paths

#[cfg(test)]
mod tests {
    use crate::{parse, ParseError};
    use crate::error::ErrorKind;
    
    #[test]
    fn test_error_kind() {
        // Test that error kinds are properly categorized
        let eof_error = ParseError::UnexpectedEof;
        assert_eq!(eof_error.kind(), ErrorKind::UnexpectedEof);
        
        let syntax_error = ParseError::InvalidSyntax("test".to_string());
        assert_eq!(syntax_error.kind(), ErrorKind::InvalidSyntax);
        
        let token_error = ParseError::UnexpectedToken {
            position: 0,
            expected: "symbol".to_string(),
            found: "number".to_string(),
        };
        assert_eq!(token_error.kind(), ErrorKind::UnexpectedToken);
        
        let depth_error = ParseError::MaxDepthExceeded {
            depth: 100,
            max_depth: 50,
        };
        assert_eq!(depth_error.kind(), ErrorKind::MaxDepthExceeded);
    }
    
    #[test]
    fn test_error_display() {
        // Test error message formatting
        let eof_error = ParseError::UnexpectedEof;
        assert_eq!(eof_error.to_string(), "Unexpected end of input");
        
        let syntax_error = ParseError::InvalidSyntax("missing closing paren".to_string());
        assert_eq!(syntax_error.to_string(), "Invalid syntax: missing closing paren");
        
        let token_error = ParseError::UnexpectedToken {
            position: 5,
            expected: ")".to_string(),
            found: "$".to_string(),
        };
        assert!(token_error.to_string().contains("Unexpected token"));
        
        let depth_error = ParseError::MaxDepthExceeded {
            depth: 100,
            max_depth: 50,
        };
        assert!(depth_error.to_string().contains("Maximum parsing depth exceeded"));
    }
    
    #[test]
    fn test_unclosed_paren() {
        let result = parse("(+ 1 2");
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::UnexpectedEof => {},
            e => panic!("Expected UnexpectedEof, got {:?}", e),
        }
    }
    
    #[test]
    fn test_extra_closing_paren() {
        let result = parse("(+ 1 2))");
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::UnclosedDelimiter(_) => {},
            e => panic!("Expected UnclosedDelimiter, got {:?}", e),
        }
    }
    
    #[test]
    fn test_invalid_number() {
        let result = parse("123abc");
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::InvalidNumber(_) => {},
            e => panic!("Expected InvalidNumber, got {:?}", e),
        }
    }
    
    #[test]
    fn test_unclosed_string() {
        let result = parse(r#""hello world"#);
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::InvalidEscape(_) | ParseError::UnexpectedEof => {},
            e => panic!("Expected InvalidEscape or UnexpectedEof, got {:?}", e),
        }
    }
    
    #[test]
    fn test_invalid_escape_sequence() {
        let result = parse(r#""hello\q""#);
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::InvalidEscape(s) => assert!(s.contains('q')),
            e => panic!("Expected InvalidEscape, got {:?}", e),
        }
    }
    
    #[test]
    fn test_invalid_special_form() {
        // Test invalid lambda syntax
        let result = parse("(lambda)");
        assert!(result.is_err());
        
        // Test invalid let syntax
        let result = parse("(let)");
        assert!(result.is_err());
        
        // Test invalid if syntax
        let result = parse("(if true)");
        assert!(result.is_err());
    }
    
    #[test]
    fn test_invalid_pattern_syntax() {
        // Test invalid pattern in match
        let result = parse("(match x (@ bad pattern) 'result)");
        assert!(result.is_err());
    }
    
    #[test]
    fn test_depth_limit() {
        // Create deeply nested expression
        let mut expr = String::new();
        for _ in 0..10 {
            expr.push('(');
        }
        expr.push_str("1");
        for _ in 0..10 {
            expr.push(')');
        }
        
        // Should work with default depth
        let result = parse(&expr);
        assert!(result.is_ok());
        
        // The parser module doesn't expose with_depth_limit yet
        // We'll test depth limit through parse_with_depth_limit function once available
    }
    
    #[test]
    fn test_invalid_symbol_parsing() {
        // Test parsing something that should be a symbol but isn't
        let result = parse("(define 123 value)"); // 123 should be a symbol
        assert!(result.is_err());
    }
    
    #[test]
    fn test_module_parsing_errors() {
        // Invalid module name
        let result = parse("(module 123)");
        assert!(result.is_err());
        
        // Missing module body
        let result = parse("(module test-module)");
        assert!(result.is_err());
        
        // Invalid export syntax
        let result = parse("(module test-module (export 123))");
        assert!(result.is_err());
    }
    
    #[test]
    fn test_contract_parsing_errors() {
        // Missing contract name
        let result = parse("(contract)");
        assert!(result.is_err());
        
        // Invalid contract clause
        let result = parse("(contract foo (invalid-clause x))");
        assert!(result.is_err());
    }
    
    #[test]
    fn test_qualified_symbol_errors() {
        // Invalid qualified symbol syntax
        let result = parse("module/123"); // number after slash
        assert!(result.is_err());
        
        let result = parse("/symbol"); // missing module
        assert!(result.is_err());
    }
    
    #[test]
    fn test_effect_parsing_errors() {
        // Invalid effect type
        let result = parse("(effect invalid-type op args)");
        assert!(result.is_err());
        
        // Missing operation
        let result = parse("(effect io)");
        assert!(result.is_err());
    }
    
    #[test]
    fn test_string_escape_edge_cases() {
        // Test trailing backslash
        let result = parse(r#""hello\"#);
        assert!(result.is_err());
        
        // Test null character
        let result = parse(r#""hello\0world""#);
        assert!(result.is_ok()); // Should handle null
    }
    
    #[test]
    fn test_lexer_error_token() {
        // Test characters that should produce error tokens
        let result = parse("#invalid");
        assert!(result.is_err());
    }
    
    #[test]
    fn test_multiple_expressions_error() {
        // Parser should handle multiple top-level expressions appropriately
        let result = parse("1 2 3");
        // This might not be an error depending on parser design
        // but we should test the behavior
        match result {
            Ok(graph) => {
                // Should have parsed only the first expression
                assert!(graph.root_id.is_some());
            }
            Err(_) => {
                // Or it might be an error
            }
        }
    }
    
    #[test]
    fn test_empty_input() {
        let result = parse("");
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::UnexpectedEof => {},
            e => panic!("Expected UnexpectedEof for empty input, got {:?}", e),
        }
    }
    
    #[test]
    fn test_whitespace_only() {
        let result = parse("   \n\t  ");
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::UnexpectedEof => {},
            e => panic!("Expected UnexpectedEof for whitespace-only input, got {:?}", e),
        }
    }
    
    #[test]
    fn test_comment_only() {
        let result = parse("; just a comment");
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::UnexpectedEof => {},
            e => panic!("Expected UnexpectedEof for comment-only input, got {:?}", e),
        }
    }
}