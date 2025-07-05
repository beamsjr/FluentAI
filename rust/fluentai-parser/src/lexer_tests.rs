#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token};
    
    // ===== Basic Token Tests =====
    
    #[test]
    fn test_delimiters() {
        let mut lexer = Lexer::new("()[]");
        assert_eq!(lexer.next_token(), Some(Token::LParen));
        assert_eq!(lexer.next_token(), Some(Token::RParen));
        assert_eq!(lexer.next_token(), Some(Token::LBracket));
        assert_eq!(lexer.next_token(), Some(Token::RBracket));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_special_tokens() {
        let mut lexer = Lexer::new(", :");
        assert_eq!(lexer.next_token(), Some(Token::Comma));
        assert_eq!(lexer.next_token(), Some(Token::Colon));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Integer Tests =====
    
    #[test]
    fn test_positive_integers() {
        let mut lexer = Lexer::new("0 1 42 1000 999999");
        assert_eq!(lexer.next_token(), Some(Token::Integer(0)));
        assert_eq!(lexer.next_token(), Some(Token::Integer(1)));
        assert_eq!(lexer.next_token(), Some(Token::Integer(42)));
        assert_eq!(lexer.next_token(), Some(Token::Integer(1000)));
        assert_eq!(lexer.next_token(), Some(Token::Integer(999999)));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_negative_integers() {
        let mut lexer = Lexer::new("-1 -42 -1000");
        assert_eq!(lexer.next_token(), Some(Token::Integer(-1)));
        assert_eq!(lexer.next_token(), Some(Token::Integer(-42)));
        assert_eq!(lexer.next_token(), Some(Token::Integer(-1000)));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_integer_edge_cases() {
        let input = format!("{} {}", i64::MAX, i64::MIN);
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next_token(), Some(Token::Integer(i64::MAX)));
        assert_eq!(lexer.next_token(), Some(Token::Integer(i64::MIN)));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Float Tests =====
    
    #[test]
    fn test_simple_floats() {
        let mut lexer = Lexer::new("3.14 0.0 1.0 -2.5");
        assert_eq!(lexer.next_token(), Some(Token::Float(3.14)));
        assert_eq!(lexer.next_token(), Some(Token::Float(0.0)));
        assert_eq!(lexer.next_token(), Some(Token::Float(1.0)));
        assert_eq!(lexer.next_token(), Some(Token::Float(-2.5)));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_scientific_notation() {
        let mut lexer = Lexer::new("1.5e10 2.0E-5 -3.14e+2");
        assert_eq!(lexer.next_token(), Some(Token::Float(1.5e10)));
        assert_eq!(lexer.next_token(), Some(Token::Float(2.0e-5)));
        assert_eq!(lexer.next_token(), Some(Token::Float(-3.14e2)));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== String Tests =====
    
    #[test]
    fn test_simple_strings() {
        let mut lexer = Lexer::new(r#""hello" "world" "123""#);
        assert_eq!(lexer.next_token(), Some(Token::String("hello".to_string())));
        assert_eq!(lexer.next_token(), Some(Token::String("world".to_string())));
        assert_eq!(lexer.next_token(), Some(Token::String("123".to_string())));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_empty_string() {
        let mut lexer = Lexer::new(r#""""#);
        assert_eq!(lexer.next_token(), Some(Token::String("".to_string())));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_string_escapes() {
        let mut lexer = Lexer::new(r#""hello\nworld" "tab\there" "quote\"test" "slash\\test" "return\rtest""#);
        assert_eq!(lexer.next_token(), Some(Token::String("hello\nworld".to_string())));
        assert_eq!(lexer.next_token(), Some(Token::String("tab\there".to_string())));
        assert_eq!(lexer.next_token(), Some(Token::String("quote\"test".to_string())));
        assert_eq!(lexer.next_token(), Some(Token::String("slash\\test".to_string())));
        assert_eq!(lexer.next_token(), Some(Token::String("return\rtest".to_string())));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_string_unknown_escape() {
        let mut lexer = Lexer::new(r#""hello\xworld""#);
        assert_eq!(lexer.next_token(), Some(Token::String("hello\\xworld".to_string())));
    }
    
    #[test]
    fn test_string_with_spaces() {
        let mut lexer = Lexer::new(r#""hello world with spaces""#);
        assert_eq!(lexer.next_token(), Some(Token::String("hello world with spaces".to_string())));
    }
    
    // ===== Boolean Tests =====
    
    #[test]
    fn test_boolean_literals() {
        let mut lexer = Lexer::new("true false #t #f");
        assert_eq!(lexer.next_token(), Some(Token::Boolean(true)));
        assert_eq!(lexer.next_token(), Some(Token::Boolean(false)));
        assert_eq!(lexer.next_token(), Some(Token::Boolean(true)));
        assert_eq!(lexer.next_token(), Some(Token::Boolean(false)));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Symbol Tests =====
    
    #[test]
    fn test_simple_symbols() {
        let mut lexer = Lexer::new("foo bar baz x y123");
        assert_eq!(lexer.next_token(), Some(Token::Symbol("foo")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("bar")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("baz")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("x")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("y123")));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_operator_symbols() {
        let mut lexer = Lexer::new("+ - * / = < > <= >= != and or not");
        assert_eq!(lexer.next_token(), Some(Token::Symbol("+")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("-")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("*")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("/")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("=")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("<")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol(">")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("<=")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol(">=")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("!=")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("and")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("or")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("not")));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_special_symbols() {
        let mut lexer = Lexer::new("lambda let if do set! define call/cc");
        assert_eq!(lexer.next_token(), Some(Token::Symbol("lambda")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("let")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("if")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("do")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("set!")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("define")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("call/cc")));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_symbols_with_hyphens() {
        let mut lexer = Lexer::new("string-length list-ref make-vector");
        assert_eq!(lexer.next_token(), Some(Token::Symbol("string-length")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("list-ref")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("make-vector")));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_symbols_with_question_marks() {
        let mut lexer = Lexer::new("null? pair? zero?");
        assert_eq!(lexer.next_token(), Some(Token::Symbol("null?")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("pair?")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("zero?")));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_symbols_with_colons() {
        let mut lexer = Lexer::new("spec:contract io:print async:spawn");
        assert_eq!(lexer.next_token(), Some(Token::Symbol("spec:contract")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("io:print")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("async:spawn")));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Qualified Symbol Tests =====
    
    #[test]
    fn test_qualified_symbols() {
        let mut lexer = Lexer::new("std.print math.sin module.export");
        assert_eq!(lexer.next_token(), Some(Token::QualifiedSymbol("std.print")));
        assert_eq!(lexer.next_token(), Some(Token::QualifiedSymbol("math.sin")));
        assert_eq!(lexer.next_token(), Some(Token::QualifiedSymbol("module.export")));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_qualified_symbols_with_underscores() {
        let mut lexer = Lexer::new("my_module.my_function test_lib.helper_fn");
        assert_eq!(lexer.next_token(), Some(Token::QualifiedSymbol("my_module.my_function")));
        assert_eq!(lexer.next_token(), Some(Token::QualifiedSymbol("test_lib.helper_fn")));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Keyword Tests =====
    
    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new(":key :value :requires :ensures :pure");
        assert_eq!(lexer.next_token(), Some(Token::Keyword(":key")));
        assert_eq!(lexer.next_token(), Some(Token::Keyword(":value")));
        assert_eq!(lexer.next_token(), Some(Token::Keyword(":requires")));
        assert_eq!(lexer.next_token(), Some(Token::Keyword(":ensures")));
        assert_eq!(lexer.next_token(), Some(Token::Keyword(":pure")));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_keywords_with_hyphens() {
        let mut lexer = Lexer::new(":my-key :another-key :test-123");
        assert_eq!(lexer.next_token(), Some(Token::Keyword(":my-key")));
        assert_eq!(lexer.next_token(), Some(Token::Keyword(":another-key")));
        assert_eq!(lexer.next_token(), Some(Token::Keyword(":test-123")));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Whitespace and Comment Tests =====
    
    #[test]
    fn test_whitespace_handling() {
        let mut lexer = Lexer::new("  ( \t+ \n1   2\r\n)  ");
        assert_eq!(lexer.next_token(), Some(Token::LParen));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("+")));
        assert_eq!(lexer.next_token(), Some(Token::Integer(1)));
        assert_eq!(lexer.next_token(), Some(Token::Integer(2)));
        assert_eq!(lexer.next_token(), Some(Token::RParen));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("; This is a comment\n42 ; Another comment");
        assert_eq!(lexer.next_token(), Some(Token::Integer(42)));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_comment_at_end() {
        let mut lexer = Lexer::new("42 ; Final comment");
        assert_eq!(lexer.next_token(), Some(Token::Integer(42)));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Complex Expression Tests =====
    
    #[test]
    fn test_complex_expression() {
        let mut lexer = Lexer::new("(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))");
        let expected = vec![
            Token::LParen,
            Token::Symbol("define"),
            Token::LParen,
            Token::Symbol("factorial"),
            Token::Symbol("n"),
            Token::RParen,
            Token::LParen,
            Token::Symbol("if"),
            Token::LParen,
            Token::Symbol("<="),
            Token::Symbol("n"),
            Token::Integer(1),
            Token::RParen,
            Token::Integer(1),
            Token::LParen,
            Token::Symbol("*"),
            Token::Symbol("n"),
            Token::LParen,
            Token::Symbol("factorial"),
            Token::LParen,
            Token::Symbol("-"),
            Token::Symbol("n"),
            Token::Integer(1),
            Token::RParen,
            Token::RParen,
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];
        
        for expected_token in expected {
            assert_eq!(lexer.next_token(), Some(expected_token));
        }
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_mixed_literals() {
        let mut lexer = Lexer::new(r#"[42 3.14 "hello" true nil]"#);
        assert_eq!(lexer.next_token(), Some(Token::LBracket));
        assert_eq!(lexer.next_token(), Some(Token::Integer(42)));
        assert_eq!(lexer.next_token(), Some(Token::Float(3.14)));
        assert_eq!(lexer.next_token(), Some(Token::String("hello".to_string())));
        assert_eq!(lexer.next_token(), Some(Token::Boolean(true)));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("nil")));
        assert_eq!(lexer.next_token(), Some(Token::RBracket));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Peek Tests =====
    
    #[test]
    fn test_peek_token() {
        let mut lexer = Lexer::new("42 foo");
        
        // Peek doesn't consume
        assert_eq!(lexer.peek_token(), Some(&Token::Integer(42)));
        assert_eq!(lexer.peek_token(), Some(&Token::Integer(42)));
        
        // Next consumes
        assert_eq!(lexer.next_token(), Some(Token::Integer(42)));
        
        // Now peek shows next token
        assert_eq!(lexer.peek_token(), Some(&Token::Symbol("foo")));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("foo")));
        
        // End of input
        assert_eq!(lexer.peek_token(), None);
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Span Tests =====
    
    #[test]
    fn test_span() {
        let mut lexer = Lexer::new("foo bar");
        
        assert_eq!(lexer.next_token(), Some(Token::Symbol("foo")));
        assert_eq!(lexer.span(), 0..3);
        
        assert_eq!(lexer.next_token(), Some(Token::Symbol("bar")));
        assert_eq!(lexer.span(), 4..7);
    }
    
    // ===== Error Cases =====
    
    #[test]
    fn test_unclosed_string() {
        let mut lexer = Lexer::new(r#""unclosed string"#);
        // The regex won't match an unclosed string, so logos skips it
        // In this implementation, it seems to return None rather than Error
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Unicode Tests =====
    
    #[test]
    fn test_unicode_in_strings() {
        let mut lexer = Lexer::new(r#""Hello, 世界" "🦀""#);
        assert_eq!(lexer.next_token(), Some(Token::String("Hello, 世界".to_string())));
        assert_eq!(lexer.next_token(), Some(Token::String("🦀".to_string())));
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_unicode_in_comments() {
        let mut lexer = Lexer::new("; Comment with 中文\n42");
        assert_eq!(lexer.next_token(), Some(Token::Integer(42)));
        assert_eq!(lexer.next_token(), None);
    }
    
    // ===== Edge Case Tests =====
    
    #[test]
    fn test_numbers_followed_by_symbols() {
        let mut lexer = Lexer::new("123abc");
        // This should be lexed as two separate tokens due to regex boundaries
        assert_eq!(lexer.next_token(), Some(Token::Integer(123)));
        assert_eq!(lexer.next_token(), Some(Token::Symbol("abc")));
    }
    
    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("");
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_only_whitespace() {
        let mut lexer = Lexer::new("   \t\n\r  ");
        assert_eq!(lexer.next_token(), None);
    }
    
    #[test]
    fn test_only_comments() {
        let mut lexer = Lexer::new("; comment 1\n; comment 2");
        assert_eq!(lexer.next_token(), None);
    }
}