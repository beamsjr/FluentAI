//! FLC-specific error handling with improved error messages

use crate::error::ParseError;
use crate::flc_lexer::Token;

/// Convert FLC parser errors to ParseError with context-aware messages
pub fn flc_error(message: String, position: usize, current_token: Option<&Token>) -> ParseError {
    match current_token {
        Some(token) => {
            let found = format_token(token);
            ParseError::UnexpectedToken {
                position,
                expected: extract_expected(&message),
                found,
            }
        }
        None => ParseError::UnexpectedEof,
    }
}

/// Format a token for error messages
fn format_token(token: &Token) -> String {
    match token {
        Token::Private => "keyword 'private'".to_string(),
        Token::Public => "keyword 'public'".to_string(),
        Token::Function => "keyword 'function'".to_string(),
        Token::Let => "keyword 'let'".to_string(),
        Token::If => "keyword 'if'".to_string(),
        Token::Else => "keyword 'else'".to_string(),
        Token::Match => "keyword 'match'".to_string(),
        Token::Case => "keyword 'case'".to_string(),
        Token::Try => "keyword 'try'".to_string(),
        Token::Catch => "keyword 'catch'".to_string(),
        Token::Finally => "keyword 'finally'".to_string(),
        Token::Async => "keyword 'async'".to_string(),
        Token::Await => "keyword 'await'".to_string(),
        Token::LowerIdent(name) => format!("identifier '{}'", name),
        Token::UpperIdent(name) => format!("type identifier '{}'", name),
        Token::ConstIdent(name) => format!("constant identifier '{}'", name),
        Token::Integer(n) => format!("integer {}", n),
        Token::Float(f) => format!("float {}", f),
        Token::String(s) => format!("string \"{}\"", s),
        Token::FString(s) => format!("f-string \"{}\"", s),
        Token::True => "boolean 'true'".to_string(),
        Token::False => "boolean 'false'".to_string(),
        Token::LParen => "'('".to_string(),
        Token::RParen => "')'".to_string(),
        Token::LBrace => "'{'".to_string(),
        Token::RBrace => "'}'".to_string(),
        Token::LBracket => "'['".to_string(),
        Token::RBracket => "']'".to_string(),
        Token::Semicolon => "';'".to_string(),
        Token::Comma => "','".to_string(),
        Token::Dot => "'.'".to_string(),
        Token::Plus => "'+'".to_string(),
        Token::Minus => "'-'".to_string(),
        Token::Star => "'*'".to_string(),
        Token::Slash => "'/'".to_string(),
        Token::Percent => "'%'".to_string(),
        Token::Eq => "'='".to_string(),
        Token::EqEq => "'=='".to_string(),
        Token::NotEq => "'!='".to_string(),
        Token::Less => "'<'".to_string(),
        Token::Greater => "'>'".to_string(),
        Token::LessEq => "'<='".to_string(),
        Token::GreaterEq => "'>='".to_string(),
        Token::AndAnd => "'&&'".to_string(),
        Token::Or => "'||'".to_string(),
        Token::Bang => "'!'".to_string(),
        Token::Arrow => "'->'".to_string(),
        Token::FatArrow => "'=>'".to_string(),
        Token::Pipe => "'|>'".to_string(),
        Token::Colon => "':'".to_string(),
        Token::ColonColon => "'::'".to_string(),
        Token::Question => "'?'".to_string(),
        Token::At => "'@'".to_string(),
        Token::Hash => "'#'".to_string(),
        Token::Underscore => "'_'".to_string(),
        Token::Error => "invalid token".to_string(),
        _ => "unknown token".to_string(),
    }
}

/// Extract expected item from error message
fn extract_expected(message: &str) -> String {
    if message.contains("function name") {
        "function name".to_string()
    } else if message.contains("variable name") {
        "variable name".to_string()
    } else if message.contains("parameter") {
        "parameter name".to_string()
    } else if message.contains("module") {
        "module name".to_string()
    } else if message.contains("definition") {
        "definition (fn, struct, enum, etc.)".to_string()
    } else if message.contains("expression") {
        "expression".to_string()
    } else if message.contains("method") {
        "method name".to_string()
    } else {
        "valid syntax".to_string()
    }
}

/// Helper function to create syntax suggestions
pub fn suggest_fix(error: &ParseError, source: &str) -> Option<String> {
    match error {
        ParseError::UnexpectedToken {
            position,
            expected,
            found,
        } => {
            // Get context around the error
            let line_start = source[..*position].rfind('\n').map(|i| i + 1).unwrap_or(0);
            let line_end = source[*position..]
                .find('\n')
                .map(|i| *position + i)
                .unwrap_or(source.len());
            let _error_line = &source[line_start..line_end];
            let _column = position - line_start;

            // Create suggestion based on common mistakes
            if found.contains("'='") && expected.contains("expression") {
                Some("Did you mean '==' for comparison?".to_string())
            } else if found.contains("keyword") && expected.contains("expression") {
                Some(format!(
                    "Unexpected {}. Try wrapping in parentheses or braces.",
                    found
                ))
            } else if found.contains("';'") && expected.contains("expression") {
                Some("Unexpected semicolon. FLC uses semicolons only after statements.".to_string())
            } else {
                None
            }
        }
        ParseError::UnclosedDelimiter(delim) => Some(format!(
            "Missing closing {} - check your parentheses, braces, or brackets",
            match delim.as_str() {
                "(" => "')'",
                "{" => "'}'",
                "[" => "']'",
                _ => delim,
            }
        )),
        _ => None,
    }
}
