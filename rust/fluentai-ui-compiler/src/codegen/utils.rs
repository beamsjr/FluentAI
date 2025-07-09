//! Common utilities for code generation

/// JavaScript code builder
pub struct JsBuilder {
    code: Vec<String>,
    indent_level: usize,
}

impl JsBuilder {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            indent_level: 0,
        }
    }

    pub fn line(&mut self, line: &str) {
        self.code.push(format!("{}{}", self.indent(), line));
    }

    pub fn indent(&self) -> String {
        "  ".repeat(self.indent_level)
    }

    pub fn increase_indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn decrease_indent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    pub fn build(self) -> String {
        self.code.join("\n")
    }
}

/// Create indentation string
pub fn indent(level: usize) -> String {
    "  ".repeat(level)
}
