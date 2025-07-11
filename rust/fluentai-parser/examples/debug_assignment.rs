use fluentai_parser::flc_parser::Parser;
use fluentai_parser::flc_lexer::{Lexer, Token};

fn main() {
    // Test lexing first
    println!("=== Testing Lexer ===");
    let mut lexer = Lexer::new("(y = 42)");
    while let Some(token) = lexer.next_token() {
        println!("Token: {:?}", token);
    }
    
    println!("\n=== Testing Parser ===");
    // Test parsing a simple assignment
    test_parse("y = 42", "simple assignment");
    
    // Test parsing parenthesized assignment
    test_parse("(y = 42)", "parenthesized assignment");
    
    // Test in let context
    test_parse("let x = (y = 42); x", "assignment in let");
}

fn test_parse(input: &str, description: &str) {
    println!("\nTesting: {} - Input: {}", description, input);
    
    let parser = Parser::new(input);
    match parser.parse() {
        Ok(graph) => {
            println!("✓ Parse successful!");
        }
        Err(e) => {
            println!("✗ Parse failed: {}", e);
        }
    }
}