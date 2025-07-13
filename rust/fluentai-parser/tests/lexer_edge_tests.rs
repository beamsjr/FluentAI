//! Edge case tests for lexer to improve coverage

#[cfg(test)]
mod lexer_edge_tests {
    use fluentai_parser::flc_lexer::{Lexer, Token};

    // ===== Phase 1: slice() method tests =====

    #[test]
    fn test_slice_method() {
        let source = "hello 42 world";
        let mut lexer = Lexer::new(source);

        // Get first token
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("hello")));
        // slice() should return the original slice that was lexed
        assert_eq!(lexer.slice(), "hello");

        // Get second token
        assert_eq!(lexer.next_token(), Some(Token::Integer(42)));
        assert_eq!(lexer.slice(), "42");

        // Get third token
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("world")));
        assert_eq!(lexer.slice(), "world");
    }

    #[test]
    fn test_slice_with_different_tokens() {
        let source = r#"(define "test" 3.14)"#;
        let mut lexer = Lexer::new(source);

        // Test slice for different token types
        assert_eq!(lexer.next_token(), Some(Token::LParen));
        assert_eq!(lexer.slice(), "(");

        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("define")));
        assert_eq!(lexer.slice(), "define");

        assert_eq!(lexer.next_token(), Some(Token::String("test".to_string())));
        assert_eq!(lexer.slice(), r#""test""#);

        assert_eq!(lexer.next_token(), Some(Token::Float(3.14)));
        assert_eq!(lexer.slice(), "3.14");

        assert_eq!(lexer.next_token(), Some(Token::RParen));
        assert_eq!(lexer.slice(), ")");
    }

    // ===== Phase 2: peek/next interaction tests =====

    #[test]
    fn test_peek_then_next() {
        let mut lexer = Lexer::new("foo bar baz");

        // Peek at first token
        assert_eq!(lexer.peek_token(), Some(&Token::LowerIdent("foo")));

        // Next should return the peeked token (covering line 111)
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("foo")));

        // Peek at second token
        assert_eq!(lexer.peek_token(), Some(&Token::LowerIdent("bar")));

        // Next should return the peeked token again
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("bar")));

        // One more time
        assert_eq!(lexer.peek_token(), Some(&Token::LowerIdent("baz")));
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("baz")));

        // End of input
        assert_eq!(lexer.peek_token(), None);
        assert_eq!(lexer.next_token(), None);
    }

    #[test]
    fn test_span_after_peek() {
        let mut lexer = Lexer::new("hello world");

        // Peek first token
        assert_eq!(lexer.peek_token(), Some(&Token::LowerIdent("hello")));

        // Get span after peek (should use peeked span, covering line 129)
        assert_eq!(lexer.span(), 0..5);

        // Consume the peeked token
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("hello")));

        // Peek next token
        assert_eq!(lexer.peek_token(), Some(&Token::LowerIdent("world")));

        // Get span after peek again
        assert_eq!(lexer.span(), 6..11);

        // Consume it
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("world")));

        // Span after consuming last token (no peeked token)
        // The span remains at the last consumed token
        assert_eq!(lexer.span(), 6..11);
    }

    #[test]
    fn test_multiple_peek_span_interactions() {
        let mut lexer = Lexer::new("(+ 123 456)");

        // Test span behavior with different token types
        assert_eq!(lexer.peek_token(), Some(&Token::LParen));
        assert_eq!(lexer.span(), 0..1);
        assert_eq!(lexer.next_token(), Some(Token::LParen));

        assert_eq!(lexer.peek_token(), Some(&Token::Plus));
        assert_eq!(lexer.span(), 1..2);
        assert_eq!(lexer.next_token(), Some(Token::Plus));

        assert_eq!(lexer.peek_token(), Some(&Token::Integer(123)));
        assert_eq!(lexer.span(), 3..6);
        assert_eq!(lexer.next_token(), Some(Token::Integer(123)));

        assert_eq!(lexer.peek_token(), Some(&Token::Integer(456)));
        assert_eq!(lexer.span(), 7..10);
        assert_eq!(lexer.next_token(), Some(Token::Integer(456)));

        assert_eq!(lexer.peek_token(), Some(&Token::RParen));
        assert_eq!(lexer.span(), 10..11);
        assert_eq!(lexer.next_token(), Some(Token::RParen));
    }

    // ===== Phase 3: Escape sequence edge case =====

    #[test]
    fn test_string_ending_with_backslash() {
        // Test string that ends with a backslash (covers line 86)
        let source = r#""test\"#;
        let mut lexer = Lexer::new(source);

        // This should not match the string regex properly, so we get None
        assert_eq!(lexer.next_token(), None);
    }

    #[test]
    fn test_string_with_trailing_backslash_in_content() {
        // Test a properly closed string that contains a backslash at the end
        // This actually creates an escaped quote, not a trailing backslash
        let source = r#""content\\""#;
        let mut lexer = Lexer::new(source);

        // This should parse as a string with a trailing backslash
        assert_eq!(
            lexer.next_token(),
            Some(Token::String("content\\".to_string()))
        );
    }

    // Additional tests to increase coverage

    #[test]
    fn test_string_escapes_through_lexer() {
        // Test string escape processing through the lexer API
        let mut lexer = Lexer::new(r#""hello\nworld""#);
        assert_eq!(lexer.next_token(), Some(Token::String("hello\nworld".to_string())));

        let mut lexer = Lexer::new(r#""tab\there""#);
        assert_eq!(lexer.next_token(), Some(Token::String("tab\there".to_string())));

        let mut lexer = Lexer::new(r#""quote\"test""#);
        assert_eq!(lexer.next_token(), Some(Token::String("quote\"test".to_string())));

        let mut lexer = Lexer::new(r#""backslash\\test""#);
        assert_eq!(lexer.next_token(), Some(Token::String("backslash\\test".to_string())));

        // Test invalid escape sequence - should preserve backslash
        let mut lexer = Lexer::new(r#""invalid\xescape""#);
        assert_eq!(lexer.next_token(), Some(Token::String("invalid\\xescape".to_string())));
        
        // Test edge case - string without closing quote won't tokenize
        let mut lexer = Lexer::new(r#""unclosed"#);
        assert_eq!(lexer.next_token(), None);
    }

    #[test]
    fn test_peek_error_token() {
        // Try to cover the error case in peek_token
        // Logos skips unrecognized characters instead of producing Error tokens
        // Let's test with symbols that can be lexed
        let source = "@ # $"; // These will be skipped or lexed as symbols
        let _lexer = Lexer::new(source);

        // Try with recognized symbols
        let mut lexer2 = Lexer::new("test 123");
        assert_eq!(lexer2.peek_token(), Some(&Token::LowerIdent("test")));
        assert_eq!(lexer2.next_token(), Some(Token::LowerIdent("test")));
        assert_eq!(lexer2.peek_token(), Some(&Token::Integer(123)));
        assert_eq!(lexer2.next_token(), Some(Token::Integer(123)));
        assert_eq!(lexer2.peek_token(), None);
    }

    #[test]
    fn test_span_with_peeked_token_coverage() {
        // More thorough test for span() with peeked token
        let mut lexer = Lexer::new("test 123");

        // First peek and check span
        lexer.peek_token();
        let span1 = lexer.span();
        assert_eq!(span1, 0..4);

        // Consume and check again
        lexer.next_token();

        // Now peek the next token
        lexer.peek_token();
        let span2 = lexer.span();
        assert_eq!(span2, 5..8);
    }

    #[test]
    fn test_next_without_peek() {
        // Test next_token when there's no peeked token (covers line 112-113)
        let mut lexer = Lexer::new("hello world");

        // Call next_token directly without peek (goes through else branch)
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("hello")));

        // Peek then next (goes through if branch)
        assert_eq!(lexer.peek_token(), Some(&Token::LowerIdent("world")));
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("world")));

        // End of input
        assert_eq!(lexer.next_token(), None);
    }
}
