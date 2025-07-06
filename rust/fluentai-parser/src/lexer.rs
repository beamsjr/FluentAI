//! Fast lexer for FluentAi S-expressions using logos

use logos::{Logos, Lexer as LogosLexer};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token<'a> {
    // Delimiters
    #[token("(")]
    LParen,
    
    #[token(")")]
    RParen,
    
    #[token("[")]
    LBracket,
    
    #[token("]")]
    RBracket,
    
    // Literals (higher priority than symbols)
    #[regex(r"-?[0-9]+", priority = 2, callback = |lex| lex.slice().parse::<i64>().ok())]
    Integer(i64),
    
    #[regex(r"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", priority = 2, callback = |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),
    
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // Remove quotes and process escapes
        let content = &s[1..s.len()-1];
        Some(process_string_escapes(content))
    })]
    String(String),
    
    #[token("true", |_| true)]
    #[token("#t", |_| true)]
    #[token("false", |_| false)]
    #[token("#f", |_| false)]
    Boolean(bool),
    
    // Qualified variables (module.name) - higher priority than regular symbols
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*\.[a-zA-Z_][a-zA-Z0-9_]*", priority = 2, callback = |lex| lex.slice())]
    QualifiedSymbol(&'a str),
    
    // Keywords (tokens starting with :)
    #[regex(r":[a-zA-Z_][a-zA-Z0-9_-]*", priority = 2, callback = |lex| lex.slice())]
    Keyword(&'a str),
    
    // Symbols (lower priority to avoid conflicts with numbers and qualified symbols)
    // Updated to include colon in symbol names for spec:contract
    #[regex(r"[a-zA-Z_+\-*/=<>!?][a-zA-Z0-9_+\-*/=<>!?:]*", priority = 1, callback = |lex| lex.slice())]
    Symbol(&'a str),
    
    // Special tokens
    #[token(",")]
    Comma,
    
    #[token(":")]
    Colon,
    
    // Comments and whitespace (automatically skipped)
    #[regex(r";[^\n]*", logos::skip)]
    #[regex(r"[ \t\n\r]+", logos::skip)]
    
    // Error token
    Error,
}

/// Process escape sequences in strings
#[doc(hidden)]
pub fn process_string_escapes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(ch);
        }
    }
    
    result
}

pub struct Lexer<'a> {
    inner: LogosLexer<'a, Token<'a>>,
    peeked: Option<(Token<'a>, std::ops::Range<usize>)>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            inner: Token::lexer(source),
            peeked: None,
        }
    }
    
    pub fn next_token(&mut self) -> Option<Token<'a>> {
        if let Some((token, _)) = self.peeked.take() {
            Some(token)
        } else {
            self.inner.next().and_then(Result::ok)
        }
    }
    
    pub fn peek_token(&mut self) -> Option<&Token<'a>> {
        if self.peeked.is_none() {
            if let Some(Ok(token)) = self.inner.next() {
                let span = self.inner.span();
                self.peeked = Some((token, span));
            }
        }
        self.peeked.as_ref().map(|(token, _)| token)
    }
    
    pub fn span(&self) -> std::ops::Range<usize> {
        if let Some((_, span)) = &self.peeked {
            span.clone()
        } else {
            self.inner.span()
        }
    }
    
    pub fn slice(&self) -> &'a str {
        self.inner.slice()
    }
}

#[cfg(test)]
#[path = "lexer_tests.rs"]
mod tests;

#[cfg(test)]
#[path = "lexer_edge_tests.rs"]
mod edge_tests;