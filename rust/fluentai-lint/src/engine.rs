//! Lint engine for running rules on FluentAi code

use crate::config::{LintConfig, RuleLevel};
use crate::diagnostic::{LintDiagnostic, DiagnosticKind};
use crate::rules::{Rule, RuleRegistry};
use fluentai_core::ast::Graph;
use fluentai_parser::parse;
use anyhow::Result;
use std::path::Path;
use rustc_hash::FxHashMap;

/// Result of linting a file or project
#[derive(Debug)]
pub struct LintResult {
    /// Diagnostics grouped by file
    pub diagnostics: FxHashMap<String, Vec<LintDiagnostic>>,
    /// Total error count
    pub error_count: usize,
    /// Total warning count
    pub warning_count: usize,
}

impl LintResult {
    fn new() -> Self {
        Self {
            diagnostics: FxHashMap::default(),
            error_count: 0,
            warning_count: 0,
        }
    }
    
    fn add_diagnostic(&mut self, file: String, diagnostic: LintDiagnostic) {
        match diagnostic.kind {
            DiagnosticKind::Error => self.error_count += 1,
            DiagnosticKind::Warning => self.warning_count += 1,
            _ => {}
        }
        
        self.diagnostics
            .entry(file)
            .or_insert_with(Vec::new)
            .push(diagnostic);
    }
    
    /// Check if linting passed (no errors)
    pub fn is_success(&self) -> bool {
        self.error_count == 0
    }
    
    /// Get total diagnostic count
    pub fn total_diagnostics(&self) -> usize {
        self.diagnostics.values().map(|v| v.len()).sum()
    }
}

/// Lint engine
pub struct LintEngine {
    config: LintConfig,
    registry: RuleRegistry,
}

impl LintEngine {
    /// Create a new lint engine with default configuration
    pub fn new() -> Self {
        Self {
            config: LintConfig::default(),
            registry: RuleRegistry::with_builtin_rules(),
        }
    }
    
    /// Create with a specific configuration
    pub fn with_config(config: LintConfig) -> Self {
        Self {
            config,
            registry: RuleRegistry::with_builtin_rules(),
        }
    }
    
    /// Set the configuration
    pub fn set_config(&mut self, config: LintConfig) {
        self.config = config;
    }
    
    /// Add a custom rule
    pub fn add_rule(&mut self, rule: Box<dyn Rule>) {
        self.registry.add_rule(rule);
    }
    
    /// Lint a string of FluentAi code
    pub fn lint_string(&self, source: &str, filename: &str) -> Result<LintResult> {
        let graph = parse(source)?;
        self.lint_graph(&graph, filename)
    }
    
    /// Lint a parsed graph
    pub fn lint_graph(&self, graph: &Graph, filename: &str) -> Result<LintResult> {
        let mut result = LintResult::new();
        
        // Run each enabled rule
        for rule in self.registry.all_rules() {
            if !self.config.is_rule_enabled(rule.id()) {
                continue;
            }
            
            let level = self.config.get_rule_level(rule.id());
            let diagnostics = rule.check(graph);
            
            for mut diagnostic in diagnostics {
                // Apply rule level
                match level {
                    RuleLevel::Error => diagnostic.kind = DiagnosticKind::Error,
                    RuleLevel::Warn => diagnostic.kind = DiagnosticKind::Warning,
                    RuleLevel::Off => continue,
                }
                
                result.add_diagnostic(filename.to_string(), diagnostic);
            }
            
            // Check if we've hit the error limit
            if let Some(max_errors) = self.config.max_errors {
                if result.error_count >= max_errors {
                    break;
                }
            }
        }
        
        Ok(result)
    }
    
    /// Lint a file
    pub fn lint_file(&self, path: &Path) -> Result<LintResult> {
        if !self.config.should_include(path) || self.config.should_ignore(path) {
            return Ok(LintResult::new());
        }
        
        let source = std::fs::read_to_string(path)?;
        let filename = path.to_string_lossy().to_string();
        self.lint_string(&source, &filename)
    }
    
    /// Lint a directory recursively
    pub fn lint_directory(&self, path: &Path) -> Result<LintResult> {
        let mut combined_result = LintResult::new();
        
        for entry in walkdir::WalkDir::new(path) {
            let entry = entry?;
            let path = entry.path();
            
            if path.is_file() && self.config.should_include(path) && !self.config.should_ignore(path) {
                match self.lint_file(path) {
                    Ok(result) => {
                        // Merge results
                        for (file, diagnostics) in result.diagnostics {
                            for diagnostic in diagnostics {
                                combined_result.add_diagnostic(file.clone(), diagnostic);
                            }
                        }
                        
                        // Check error limit
                        if let Some(max_errors) = self.config.max_errors {
                            if combined_result.error_count >= max_errors {
                                break;
                            }
                        }
                    }
                    Err(e) => {
                        // Add parse error as diagnostic
                        combined_result.add_diagnostic(
                            path.to_string_lossy().to_string(),
                            LintDiagnostic::error(
                                "parse-error",
                                format!("Failed to parse file: {}", e)
                            )
                        );
                    }
                }
            }
        }
        
        Ok(combined_result)
    }
}

/// Format diagnostics for display
pub fn format_diagnostics(result: &LintResult) -> String {
    use std::fmt::Write;
    
    let mut output = String::new();
    
    for (file, diagnostics) in &result.diagnostics {
        if !diagnostics.is_empty() {
            writeln!(&mut output, "\n{}:", file).unwrap();
            
            for diagnostic in diagnostics {
                let kind_str = match diagnostic.kind {
                    DiagnosticKind::Error => "error",
                    DiagnosticKind::Warning => "warning",
                    DiagnosticKind::Note => "note",
                    DiagnosticKind::Help => "help",
                };
                
                writeln!(
                    &mut output,
                    "  {}: [{}] {}",
                    kind_str,
                    diagnostic.rule_id,
                    diagnostic.message
                ).unwrap();
                
                for note in &diagnostic.notes {
                    writeln!(&mut output, "    note: {}", note).unwrap();
                }
                
                for suggestion in &diagnostic.suggestions {
                    writeln!(&mut output, "    help: {}", suggestion.message).unwrap();
                }
            }
        }
    }
    
    // Summary
    writeln!(&mut output).unwrap();
    write!(
        &mut output,
        "Found {} error{} and {} warning{}",
        result.error_count,
        if result.error_count == 1 { "" } else { "s" },
        result.warning_count,
        if result.warning_count == 1 { "" } else { "s" }
    ).unwrap();
    
    output
}

