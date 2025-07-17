//! Lexer for FLC (Fluent Lambda Chain) syntax using logos

use logos::{Lexer as LogosLexer, Logos};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token<'a> {
    // Keywords - Definition keywords (highest priority)
    #[token("private", priority = 10)]
    Private,
    #[token("public", priority = 10)]
    Public,
    #[token("function", priority = 10)]
    Function,
    #[token("struct", priority = 10)]
    Struct,
    #[token("enum", priority = 10)]
    Enum,
    #[token("trait", priority = 10)]
    Trait,
    // 'impl' removed - using 'as' for trait implementation
    #[token("type", priority = 10)]
    Type,
    #[token("actor", priority = 10)]
    Actor,
    #[token("receive", priority = 10)]
    Receive,
    #[token("become", priority = 10)]
    Become,
    #[token("effect", priority = 10)]
    Effect,
    #[token("macro", priority = 10)]
    Macro,
    #[token("extern", priority = 10)]
    Extern,

    // Continuum UI keywords
    #[token("surface", priority = 10)]
    Surface,
    #[token("space", priority = 10)]
    Space,
    #[token("element", priority = 10)]
    Element,
    #[token("state_field", priority = 10)]
    StateField,
    #[token("disturb", priority = 10)]
    Disturb,

    // Control flow keywords
    #[token("if", priority = 10)]
    If,
    #[token("else", priority = 10)]
    Else,
    #[token("match", priority = 10)]
    Match,
    #[token("case", priority = 10)]
    Case,
    #[token("for", priority = 10)]
    For,
    #[token("while", priority = 10)]
    While,
    #[token("when", priority = 10)]
    When,
    #[token("in", priority = 10)]
    In,
    #[token("let", priority = 10)]
    Let,
    #[token("const", priority = 10)]
    Const,

    // Concurrency keywords
    #[token("async", priority = 10)]
    Async,
    #[token("await", priority = 10)]
    Await,
    #[token("spawn", priority = 10)]
    Spawn,
    #[token("parallel", priority = 10)]
    Parallel,
    #[token("handle", priority = 10)]
    Handle,
    #[token("promise", priority = 10)]
    Promise,

    // Error handling keywords
    #[token("try", priority = 10)]
    Try,
    #[token("catch", priority = 10)]
    Catch,
    #[token("finally", priority = 10)]
    Finally,

    // Module keywords
    #[token("use", priority = 10)]
    Use,
    #[token("mod", priority = 10)]
    Mod,
    #[token("export", priority = 10)]
    Export,

    // Other keywords
    #[token("true", priority = 10)]
    True,
    #[token("false", priority = 10)]
    False,
    #[token("self", priority = 10)]
    Self_,
    #[token("super", priority = 10)]
    Super,
    #[token("unsafe", priority = 10)]
    Unsafe,
    #[token("dyn", priority = 10)]
    Dyn,
    #[token("with", priority = 10)]
    With,
    #[token("perform", priority = 10)]
    Perform,
    #[token("nil", priority = 10)]
    Nil,
    #[token("rec", priority = 10)]
    Rec,
    #[token("as", priority = 10)]
    As,
    #[token("from", priority = 10)]
    From,
    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    // Operators - Chaining (highest priority after keywords)
    #[token(".", priority = 9)]
    Dot,
    #[token("|>", priority = 9)]
    Pipe,
    #[token(".?", priority = 9)]
    OptionalChain,

    // Operators - Arithmetic
    #[token("+", priority = 8)]
    Plus,
    #[token("-", priority = 8)]
    Minus,
    #[token("*", priority = 8)]
    Star,
    #[token("/", priority = 8)]
    Slash,
    #[token("%", priority = 8)]
    Percent,
    #[token("**", priority = 9)]
    StarStar,

    // Operators - Comparison
    #[token("==", priority = 8)]
    EqEq,
    #[token("!=", priority = 8)]
    NotEq,
    #[token("<", priority = 8)]
    Less,
    #[token(">", priority = 8)]
    Greater,
    #[token("<=", priority = 8)]
    LessEq,
    #[token(">=", priority = 8)]
    GreaterEq,

    // Operators - Logical
    #[token("&&", priority = 8)]
    AndAnd,
    #[token("||", priority = 8)]
    OrOr,
    #[token("|", priority = 7)]
    Or,
    #[token("!", priority = 8)]
    Bang,

    // Operators - Assignment
    #[token("=", priority = 8)]
    Eq,
    #[token(":=", priority = 8)]
    ColonEq,

    // Operators - Other
    #[token("::", priority = 8)]
    ColonColon,
    #[token("->", priority = 8)]
    Arrow,
    #[token("=>", priority = 8)]
    FatArrow,
    #[token("..", priority = 8)]
    DotDot,
    #[token("...", priority = 8)]
    DotDotDot,
    #[token("++", priority = 8)]
    PlusPlus,

    // Punctuation
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("@")]
    At,
    #[token("#")]
    Hash,
    #[token("?")]
    Question,
    #[token("_")]
    Underscore,
    #[token("$")]
    Dollar,

    // Literals
    #[regex(r"[0-9]+", priority = 5, callback = |lex| lex.slice().parse::<i64>().ok())]
    Integer(i64),

    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", priority = 5, callback = |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    // String literals - regular strings
    #[regex(r#""([^"\\]|\\.)*""#, priority = 5, callback = |lex| {
        let s = lex.slice();
        let content = &s[1..s.len()-1];
        process_string_escapes(content)
    })]
    String(String),

    // Symbol literals - start with single quote
    #[regex(r"'[a-zA-Z_][a-zA-Z0-9_-]*", priority = 5, callback = |lex| {
        let s = lex.slice();
        s[1..].to_string() // Remove the leading quote
    })]
    Symbol(String),

    // F-strings (formatted strings)
    #[regex(r#"f"([^"\\]|\\.)*""#, priority = 6, callback = |lex| {
        let s = lex.slice();
        &s[2..s.len()-1]
    })]
    FString(&'a str),

    // Identifiers (after keywords to avoid conflicts)
    // snake_case for variables/functions
    #[regex(r"[a-z_][a-z0-9_]*", priority = 3)]
    LowerIdent(&'a str),

    // PascalCase for types
    #[regex(r"[A-Z][a-zA-Z0-9_]*", priority = 3)]
    UpperIdent(&'a str),

    // SCREAMING_SNAKE_CASE for constants
    #[regex(r"[A-Z][A-Z0-9_]*", priority = 4)]
    ConstIdent(&'a str),

    // Module paths (e.g., std::collections::HashMap)
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*(::[a-zA-Z_][a-zA-Z0-9_]*)+", priority = 7)]
    ModulePath(&'a str),

    // Comments and whitespace (automatically skipped)
    #[regex(r"//[^\n]*", logos::skip)]
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)]
    #[regex(r"[ \t\n\r]+", logos::skip)]
    // Error token
    Error,
}

/// Process escape sequences in strings
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
                Some('0') => result.push('\0'),
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

#[derive(Clone)]
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
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("private public function struct let if true false");
        assert_eq!(lexer.next_token(), Some(Token::Private));
        assert_eq!(lexer.next_token(), Some(Token::Public));
        assert_eq!(lexer.next_token(), Some(Token::Function));
        assert_eq!(lexer.next_token(), Some(Token::Struct));
        assert_eq!(lexer.next_token(), Some(Token::Let));
        assert_eq!(lexer.next_token(), Some(Token::If));
        assert_eq!(lexer.next_token(), Some(Token::True));
        assert_eq!(lexer.next_token(), Some(Token::False));
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Lexer::new("user_name UserType MAX_SIZE");
        assert_eq!(lexer.next_token(), Some(Token::LowerIdent("user_name")));
        assert_eq!(lexer.next_token(), Some(Token::UpperIdent("UserType")));
        assert_eq!(lexer.next_token(), Some(Token::ConstIdent("MAX_SIZE")));
    }

    #[test]
    fn test_literals() {
        let mut lexer = Lexer::new(r#"42 3.14 "hello" f"Hello {name}""#);
        assert_eq!(lexer.next_token(), Some(Token::Integer(42)));
        assert_eq!(lexer.next_token(), Some(Token::Float(3.14)));
        assert_eq!(lexer.next_token(), Some(Token::String("hello".to_string())));
        assert_eq!(lexer.next_token(), Some(Token::FString("Hello {name}")));
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new(". |> + == && = :=");
        assert_eq!(lexer.next_token(), Some(Token::Dot));
        assert_eq!(lexer.next_token(), Some(Token::Pipe));
        assert_eq!(lexer.next_token(), Some(Token::Plus));
        assert_eq!(lexer.next_token(), Some(Token::EqEq));
        assert_eq!(lexer.next_token(), Some(Token::AndAnd));
        assert_eq!(lexer.next_token(), Some(Token::Eq));
        assert_eq!(lexer.next_token(), Some(Token::ColonEq));
    }

    #[test]
    fn test_module_paths() {
        let mut lexer = Lexer::new("std::collections::HashMap");
        assert_eq!(
            lexer.next_token(),
            Some(Token::ModulePath("std::collections::HashMap"))
        );
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("private // comment\n function /* block */ struct");
        assert_eq!(lexer.next_token(), Some(Token::Private));
        assert_eq!(lexer.next_token(), Some(Token::Function));
        assert_eq!(lexer.next_token(), Some(Token::Struct));
    }

    #[test]
    fn test_continuum_keywords() {
        let mut lexer = Lexer::new("surface space element state_field disturb when");
        assert_eq!(lexer.next_token(), Some(Token::Surface));
        assert_eq!(lexer.next_token(), Some(Token::Space));
        assert_eq!(lexer.next_token(), Some(Token::Element));
        assert_eq!(lexer.next_token(), Some(Token::StateField));
        assert_eq!(lexer.next_token(), Some(Token::Disturb));
        assert_eq!(lexer.next_token(), Some(Token::When)); // Already exists
    }
}
