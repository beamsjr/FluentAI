//! Comprehensive tests for parser error types and error handling

use fluentai_parser::error::{ParseError, ErrorKind};

#[test]
fn test_unexpected_token_error() {
    let error = ParseError::UnexpectedToken {
        position: 42,
        expected: "identifier".to_string(),
        found: "number".to_string(),
    };
    
    // Test Display trait
    let error_msg = error.to_string();
    assert!(error_msg.contains("Unexpected token at position 42"));
    assert!(error_msg.contains("expected identifier"));
    assert!(error_msg.contains("found number"));
    
    // Test kind
    assert_eq!(error.kind(), ErrorKind::UnexpectedToken);
}

#[test]
fn test_unexpected_eof_error() {
    let error = ParseError::UnexpectedEof;
    
    // Test Display trait
    assert_eq!(error.to_string(), "Unexpected end of input");
    
    // Test kind
    assert_eq!(error.kind(), ErrorKind::UnexpectedEof);
}

#[test]
fn test_invalid_number_error() {
    let error = ParseError::InvalidNumber("123.456.789".to_string());
    
    // Test Display trait
    let error_msg = error.to_string();
    assert!(error_msg.contains("Invalid number literal"));
    assert!(error_msg.contains("123.456.789"));
    
    // Test kind
    assert_eq!(error.kind(), ErrorKind::InvalidNumber);
    
    // Test with different invalid numbers
    let hex_error = ParseError::InvalidNumber("0xGHI".to_string());
    assert!(hex_error.to_string().contains("0xGHI"));
    
    let binary_error = ParseError::InvalidNumber("0b123".to_string());
    assert!(binary_error.to_string().contains("0b123"));
}

#[test]
fn test_invalid_escape_error() {
    let error = ParseError::InvalidEscape("\\q".to_string());
    
    // Test Display trait
    let error_msg = error.to_string();
    assert!(error_msg.contains("Invalid escape sequence in string"));
    assert!(error_msg.contains("\\q"));
    
    // Test kind
    assert_eq!(error.kind(), ErrorKind::InvalidEscape);
    
    // Test with different escape sequences
    let unicode_error = ParseError::InvalidEscape("\\u{GGGG}".to_string());
    assert!(unicode_error.to_string().contains("\\u{GGGG}"));
    
    let hex_error = ParseError::InvalidEscape("\\xZZ".to_string());
    assert!(hex_error.to_string().contains("\\xZZ"));
}

#[test]
fn test_unclosed_delimiter_error() {
    let error = ParseError::UnclosedDelimiter("(".to_string());
    
    // Test Display trait
    let error_msg = error.to_string();
    assert!(error_msg.contains("Unclosed delimiter"));
    assert!(error_msg.contains("("));
    
    // Test kind
    assert_eq!(error.kind(), ErrorKind::UnclosedDelimiter);
    
    // Test with different delimiters
    let bracket_error = ParseError::UnclosedDelimiter("[".to_string());
    assert!(bracket_error.to_string().contains("["));
    
    let brace_error = ParseError::UnclosedDelimiter("{".to_string());
    assert!(brace_error.to_string().contains("{"));
    
    let string_error = ParseError::UnclosedDelimiter("\"".to_string());
    assert!(string_error.to_string().contains("\""));
}

#[test]
fn test_invalid_syntax_error() {
    let error = ParseError::InvalidSyntax("Expected expression after 'if'".to_string());
    
    // Test Display trait
    let error_msg = error.to_string();
    assert!(error_msg.contains("Invalid syntax"));
    assert!(error_msg.contains("Expected expression after 'if'"));
    
    // Test kind
    assert_eq!(error.kind(), ErrorKind::InvalidSyntax);
    
    // Test with different syntax errors
    let let_error = ParseError::InvalidSyntax("'let' requires a binding".to_string());
    assert!(let_error.to_string().contains("'let' requires a binding"));
    
    let contract_error = ParseError::InvalidSyntax("Invalid contract specification".to_string());
    assert!(contract_error.to_string().contains("Invalid contract specification"));
}

#[test]
fn test_max_depth_exceeded_error() {
    let error = ParseError::MaxDepthExceeded {
        depth: 150,
        max_depth: 100,
    };
    
    // Test Display trait
    let error_msg = error.to_string();
    assert!(error_msg.contains("Maximum parsing depth exceeded"));
    assert!(error_msg.contains("depth 150"));
    assert!(error_msg.contains("limit of 100"));
    
    // Test kind
    assert_eq!(error.kind(), ErrorKind::MaxDepthExceeded);
    
    // Test with different depths
    let shallow_error = ParseError::MaxDepthExceeded {
        depth: 11,
        max_depth: 10,
    };
    let shallow_msg = shallow_error.to_string();
    assert!(shallow_msg.contains("depth 11"));
    assert!(shallow_msg.contains("limit of 10"));
}

#[test]
fn test_error_kind_equality() {
    // Test that ErrorKind derives PartialEq and Eq correctly
    assert_eq!(ErrorKind::UnexpectedToken, ErrorKind::UnexpectedToken);
    assert_ne!(ErrorKind::UnexpectedToken, ErrorKind::UnexpectedEof);
    
    assert_eq!(ErrorKind::InvalidNumber, ErrorKind::InvalidNumber);
    assert_ne!(ErrorKind::InvalidNumber, ErrorKind::InvalidEscape);
    
    // Test all variants for equality
    let kinds = vec![
        ErrorKind::UnexpectedToken,
        ErrorKind::UnexpectedEof,
        ErrorKind::InvalidNumber,
        ErrorKind::InvalidEscape,
        ErrorKind::UnclosedDelimiter,
        ErrorKind::InvalidSyntax,
        ErrorKind::MaxDepthExceeded,
    ];
    
    for (i, kind1) in kinds.iter().enumerate() {
        for (j, kind2) in kinds.iter().enumerate() {
            if i == j {
                assert_eq!(kind1, kind2);
            } else {
                assert_ne!(kind1, kind2);
            }
        }
    }
}

#[test]
fn test_error_clone() {
    // Test that ParseError derives Clone correctly
    let original = ParseError::UnexpectedToken {
        position: 10,
        expected: "operator".to_string(),
        found: "keyword".to_string(),
    };
    
    let cloned = original.clone();
    assert_eq!(original.to_string(), cloned.to_string());
    assert_eq!(original.kind(), cloned.kind());
    
    // Test cloning all error types
    let errors: Vec<ParseError> = vec![
        ParseError::UnexpectedEof,
        ParseError::InvalidNumber("bad".to_string()),
        ParseError::InvalidEscape("\\z".to_string()),
        ParseError::UnclosedDelimiter("]".to_string()),
        ParseError::InvalidSyntax("test".to_string()),
        ParseError::MaxDepthExceeded { depth: 5, max_depth: 3 },
    ];
    
    for error in errors {
        let cloned = error.clone();
        assert_eq!(error.to_string(), cloned.to_string());
        assert_eq!(error.kind(), cloned.kind());
    }
}

#[test]
fn test_error_debug() {
    // Test Debug trait implementation
    let error = ParseError::UnexpectedToken {
        position: 25,
        expected: "expression".to_string(),
        found: "EOF".to_string(),
    };
    
    let debug_str = format!("{:?}", error);
    assert!(debug_str.contains("UnexpectedToken"));
    assert!(debug_str.contains("position"));
    assert!(debug_str.contains("25"));
    
    // Test ErrorKind Debug
    let kind = ErrorKind::InvalidNumber;
    let kind_debug = format!("{:?}", kind);
    assert_eq!(kind_debug, "InvalidNumber");
}

#[test]
fn test_error_kind_copy() {
    // Test that ErrorKind derives Copy correctly
    let kind1 = ErrorKind::UnexpectedToken;
    let kind2 = kind1; // This should copy, not move
    let kind3 = kind1; // We can still use kind1
    
    assert_eq!(kind1, kind2);
    assert_eq!(kind1, kind3);
    assert_eq!(kind2, kind3);
}

#[test]
fn test_error_display_formatting() {
    // Test specific formatting details
    let error = ParseError::UnexpectedToken {
        position: 0,
        expected: "".to_string(),
        found: "".to_string(),
    };
    
    // Even with empty strings, the error should format correctly
    let msg = error.to_string();
    assert!(msg.contains("position 0"));
    
    // Test with very long strings
    let long_error = ParseError::InvalidSyntax("a".repeat(1000));
    let long_msg = long_error.to_string();
    assert!(long_msg.len() > 1000); // Should include the full message
}

#[test]
fn test_error_patterns() {
    // Test common error patterns that might occur during parsing
    
    // Missing closing parenthesis
    let paren_error = ParseError::UnclosedDelimiter("(".to_string());
    assert_eq!(paren_error.kind(), ErrorKind::UnclosedDelimiter);
    
    // Invalid number format
    let number_error = ParseError::InvalidNumber("12e".to_string());
    assert!(number_error.to_string().contains("12e"));
    
    // Unexpected EOF in expression
    let eof_error = ParseError::UnexpectedEof;
    assert_eq!(eof_error.kind(), ErrorKind::UnexpectedEof);
    
    // Complex nested structure exceeding depth
    let depth_error = ParseError::MaxDepthExceeded {
        depth: 200,
        max_depth: 100,
    };
    assert!(depth_error.to_string().contains("200"));
}