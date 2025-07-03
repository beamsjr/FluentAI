//! Syntax highlighting for the REPL

use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::completion::Completer;
use rustyline::validate::Validator;
use rustyline::Helper;
use colored::*;
use std::borrow::Cow;

/// Syntax highlighter for ClaudeLang REPL
pub struct ReplHighlighter {
    /// Keywords to highlight
    keywords: Vec<&'static str>,
    /// Built-in functions
    builtins: Vec<&'static str>,
}

impl ReplHighlighter {
    /// Create a new highlighter
    pub fn new() -> Self {
        Self {
            keywords: vec![
                "define", "lambda", "let", "let*", "letrec",
                "if", "cond", "case", "and", "or", "not",
                "begin", "do", "while", "for",
                "contract", "requires", "ensures", "invariant",
                "async", "await", "spawn", "yield",
                "try", "catch", "finally", "throw",
                "module", "import", "export", "use",
            ],
            builtins: vec![
                "+", "-", "*", "/", "=", ">", "<", ">=", "<=",
                "list", "cons", "car", "cdr", "null?", "list?",
                "number?", "string?", "boolean?", "procedure?",
                "map", "filter", "reduce", "fold", "append",
                "length", "reverse", "sort", "member", "assoc",
                "print", "println", "format", "error",
                "read", "write", "open", "close",
            ],
        }
    }

    /// Check if a word is a keyword
    fn is_keyword(&self, word: &str) -> bool {
        self.keywords.contains(&word)
    }

    /// Check if a word is a builtin
    fn is_builtin(&self, word: &str) -> bool {
        self.builtins.contains(&word)
    }

    /// Tokenize input for highlighting
    fn tokenize(&self, input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = input.char_indices().peekable();
        
        while let Some((i, ch)) = chars.next() {
            match ch {
                // Comments
                ';' => {
                    let start = i;
                    while let Some((j, _)) = chars.peek() {
                        if input[*j..].starts_with('\n') {
                            break;
                        }
                        chars.next();
                    }
                    let end = chars.peek().map(|(j, _)| *j).unwrap_or(input.len());
                    tokens.push(Token::Comment(start, end));
                }
                // Strings
                '"' => {
                    let start = i;
                    let mut escaped = false;
                    while let Some((j, ch)) = chars.next() {
                        if escaped {
                            escaped = false;
                        } else if ch == '\\' {
                            escaped = true;
                        } else if ch == '"' {
                            tokens.push(Token::String(start, j + 1));
                            break;
                        }
                    }
                }
                // Numbers
                '0'..='9' | '-' => {
                    let start = i;
                    if ch == '-' {
                        // Check if it's a negative number or just minus
                        if let Some((_, next_ch)) = chars.peek() {
                            if !next_ch.is_numeric() {
                                // It's just a minus operator
                                if self.is_builtin("-") {
                                    tokens.push(Token::Builtin(start, start + 1));
                                } else {
                                    tokens.push(Token::Symbol(start, start + 1));
                                }
                                continue;
                            }
                        }
                    }
                    
                    while let Some((j, ch)) = chars.peek() {
                        if ch.is_numeric() || *ch == '.' {
                            chars.next();
                        } else {
                            tokens.push(Token::Number(start, *j));
                            break;
                        }
                    }
                    if chars.peek().is_none() {
                        tokens.push(Token::Number(start, input.len()));
                    }
                }
                // Parentheses
                '(' | ')' | '[' | ']' | '{' | '}' => {
                    tokens.push(Token::Paren(i, i + 1));
                }
                // Commands
                ':' if i == 0 || input[..i].trim().is_empty() => {
                    let start = i;
                    while let Some((j, ch)) = chars.peek() {
                        if ch.is_whitespace() {
                            break;
                        }
                        chars.next();
                    }
                    let end = chars.peek().map(|(j, _)| *j).unwrap_or(input.len());
                    tokens.push(Token::Command(start, end));
                }
                // Whitespace
                ch if ch.is_whitespace() => {
                    // Skip whitespace
                }
                // Symbols/identifiers
                _ => {
                    let start = i;
                    while let Some((j, ch)) = chars.peek() {
                        if ch.is_whitespace() || "()[]{}\"".contains(*ch) {
                            break;
                        }
                        chars.next();
                    }
                    let end = chars.peek().map(|(j, _)| *j).unwrap_or(input.len());
                    let word = &input[start..end];
                    
                    if word == "true" || word == "false" {
                        tokens.push(Token::Boolean(start, end));
                    } else if word == "nil" {
                        tokens.push(Token::Nil(start, end));
                    } else if self.is_keyword(word) {
                        tokens.push(Token::Keyword(start, end));
                    } else if self.is_builtin(word) {
                        tokens.push(Token::Builtin(start, end));
                    } else {
                        tokens.push(Token::Symbol(start, end));
                    }
                }
            }
        }
        
        tokens
    }
}

/// Token types for highlighting
#[derive(Debug, Clone)]
enum Token {
    Keyword(usize, usize),
    Builtin(usize, usize),
    Symbol(usize, usize),
    String(usize, usize),
    Number(usize, usize),
    Boolean(usize, usize),
    Nil(usize, usize),
    Comment(usize, usize),
    Paren(usize, usize),
    Command(usize, usize),
}

impl Highlighter for ReplHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let tokens = self.tokenize(line);
        
        if tokens.is_empty() {
            return Cow::Borrowed(line);
        }
        
        let mut result = String::with_capacity(line.len() * 2);
        let mut last_end = 0;
        
        for token in tokens {
            let (start, end, color) = match token {
                Token::Keyword(s, e) => (s, e, "magenta"),
                Token::Builtin(s, e) => (s, e, "cyan"),
                Token::Symbol(s, e) => (s, e, "white"),
                Token::String(s, e) => (s, e, "green"),
                Token::Number(s, e) => (s, e, "yellow"),
                Token::Boolean(s, e) => (s, e, "bright_yellow"),
                Token::Nil(s, e) => (s, e, "bright_black"),
                Token::Comment(s, e) => (s, e, "bright_black"),
                Token::Paren(s, e) => (s, e, "bright_white"),
                Token::Command(s, e) => (s, e, "bright_blue"),
            };
            
            // Add any text between tokens
            if last_end < start {
                result.push_str(&line[last_end..start]);
            }
            
            // Add colored token
            let text = &line[start..end];
            let colored_text = match color {
                "magenta" => text.magenta(),
                "cyan" => text.cyan(),
                "white" => text.white(),
                "green" => text.green(),
                "yellow" => text.yellow(),
                "bright_yellow" => text.bright_yellow(),
                "bright_black" => text.bright_black(),
                "bright_white" => text.bright_white(),
                "bright_blue" => text.bright_blue(),
                _ => text.normal(),
            };
            
            result.push_str(&colored_text.to_string());
            last_end = end;
        }
        
        // Add any remaining text
        if last_end < line.len() {
            result.push_str(&line[last_end..]);
        }
        
        Cow::Owned(result)
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        Cow::Owned(prompt.bright_green().to_string())
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned(hint.bright_black().to_string())
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        _completion: rustyline::CompletionType,
    ) -> Cow<'c, str> {
        Cow::Borrowed(candidate)
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _forced: bool) -> bool {
        true
    }
}

impl Default for ReplHighlighter {
    fn default() -> Self {
        Self::new()
    }
}

/// Combined helper for rustyline
pub struct ReplHelper {
    pub highlighter: ReplHighlighter,
    pub completer: crate::completer::ReplCompleter,
}

impl ReplHelper {
    pub fn new() -> Self {
        Self {
            highlighter: ReplHighlighter::new(),
            completer: crate::completer::ReplCompleter::new(),
        }
    }
}

impl Helper for ReplHelper {}

impl Completer for ReplHelper {
    type Candidate = <crate::completer::ReplCompleter as Completer>::Candidate;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        self.completer.complete(line, pos, ctx)
    }
}

impl Hinter for ReplHelper {
    type Hint = String;
}

impl Highlighter for ReplHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        self.highlighter.highlight_prompt(prompt, default)
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        self.highlighter.highlight_hint(hint)
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        completion: rustyline::CompletionType,
    ) -> Cow<'c, str> {
        self.highlighter.highlight_candidate(candidate, completion)
    }

    fn highlight_char(&self, line: &str, pos: usize, forced: bool) -> bool {
        self.highlighter.highlight_char(line, pos, forced)
    }
}

impl Validator for ReplHelper {}