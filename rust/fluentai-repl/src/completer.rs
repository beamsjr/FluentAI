//! Auto-completion support for the REPL

use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::{Context, Result};
use std::collections::HashSet;

/// Auto-completer for FluentAi REPL
pub struct ReplCompleter {
    /// Built-in functions
    builtins: HashSet<String>,
    /// REPL commands
    commands: HashSet<String>,
    /// User-defined names
    user_names: HashSet<String>,
    /// Keywords
    keywords: HashSet<String>,
}

impl ReplCompleter {
    /// Create a new completer
    pub fn new() -> Self {
        let mut completer = Self {
            builtins: HashSet::new(),
            commands: HashSet::new(),
            user_names: HashSet::new(),
            keywords: HashSet::new(),
        };
        
        // Initialize with default completions
        completer.init_defaults();
        completer
    }

    /// Initialize default completions
    fn init_defaults(&mut self) {
        // Built-in functions
        self.builtins.extend(vec![
            "+", "-", "*", "/", "=", ">", "<", ">=", "<=",
            "list", "cons", "car", "cdr", "null?", "list?",
            "number?", "string?", "boolean?", "procedure?",
            "map", "filter", "reduce", "fold", "append",
            "length", "reverse", "sort", "member", "assoc",
            "print", "println", "format", "error",
            "read", "write", "open", "close",
            "let", "let*", "letrec", "define", "lambda",
            "if", "cond", "case", "and", "or", "not",
            "begin", "do", "while", "for",
            "contract", "requires", "ensures", "invariant",
            "async", "await", "spawn", "yield",
            "try", "catch", "finally", "throw",
            "module", "import", "export", "use",
        ].into_iter().map(String::from));

        // REPL commands
        self.commands.extend(vec![
            ":help", ":h", ":?",
            ":exit", ":quit", ":q",
            ":clear", ":cls",
            ":mode", ":m",
            ":debug", ":d",
            ":vars", ":v",
            ":set", ":s",
            ":load", ":l",
            ":save",
            ":reset",
            ":time", ":t",
        ].into_iter().map(String::from));

        // Keywords
        self.keywords.extend(vec![
            "define", "lambda", "let", "let*", "letrec",
            "if", "cond", "case", "and", "or", "not",
            "begin", "do", "while", "for",
            "contract", "requires", "ensures", "invariant",
            "async", "await", "spawn", "yield",
            "try", "catch", "finally", "throw",
            "module", "import", "export", "use",
            "true", "false", "nil",
        ].into_iter().map(String::from));
    }

    /// Add a user-defined name
    pub fn add_user_name(&mut self, name: String) {
        self.user_names.insert(name);
    }

    /// Remove a user-defined name
    pub fn remove_user_name(&mut self, name: &str) {
        self.user_names.remove(name);
    }

    /// Clear all user-defined names
    pub fn clear_user_names(&mut self) {
        self.user_names.clear();
    }

    /// Get all possible completions
    fn get_completions(&self, prefix: &str) -> Vec<String> {
        let mut completions = Vec::new();
        
        // Check if it's a command
        if prefix.starts_with(':') {
            for cmd in &self.commands {
                if cmd.starts_with(prefix) {
                    completions.push(cmd.clone());
                }
            }
        } else {
            // Add builtins
            for builtin in &self.builtins {
                if builtin.starts_with(prefix) {
                    completions.push(builtin.clone());
                }
            }
            
            // Add keywords
            for keyword in &self.keywords {
                if keyword.starts_with(prefix) {
                    completions.push(keyword.clone());
                }
            }
            
            // Add user names
            for name in &self.user_names {
                if name.starts_with(prefix) {
                    completions.push(name.clone());
                }
            }
        }
        
        completions.sort();
        completions.dedup();
        completions
    }

    /// Extract the word being completed
    fn extract_word<'a>(&self, line: &'a str, pos: usize) -> (&'a str, usize) {
        let before = &line[..pos];
        let start = before
            .rfind(|c: char| c.is_whitespace() || "()[]{}".contains(c))
            .map(|i| i + 1)
            .unwrap_or(0);
        
        (&line[start..pos], start)
    }
}

impl Completer for ReplCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Pair>)> {
        let (word, start) = self.extract_word(line, pos);
        
        let completions = self.get_completions(word);
        let pairs: Vec<Pair> = completions
            .into_iter()
            .map(|s| Pair {
                display: s.clone(),
                replacement: s,
            })
            .collect();
        
        Ok((start, pairs))
    }
}

impl Default for ReplCompleter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_completer() {
        let completer = ReplCompleter::new();
        
        // Test builtin completion
        let completions = completer.get_completions("con");
        assert!(completions.contains(&"cons".to_string()));
        assert!(completions.contains(&"cond".to_string()));
        assert!(completions.contains(&"contract".to_string()));
        
        // Test command completion
        let completions = completer.get_completions(":h");
        assert!(completions.contains(&":h".to_string()));
        assert!(completions.contains(&":help".to_string()));
        
        // Test keyword completion
        let completions = completer.get_completions("lam");
        assert!(completions.contains(&"lambda".to_string()));
    }

    #[test]
    fn test_user_names() {
        let mut completer = ReplCompleter::new();
        
        completer.add_user_name("my-function".to_string());
        completer.add_user_name("my-variable".to_string());
        
        let completions = completer.get_completions("my-");
        assert_eq!(completions.len(), 2);
        assert!(completions.contains(&"my-function".to_string()));
        assert!(completions.contains(&"my-variable".to_string()));
        
        completer.remove_user_name("my-function");
        let completions = completer.get_completions("my-");
        assert_eq!(completions.len(), 1);
        assert!(completions.contains(&"my-variable".to_string()));
    }
}