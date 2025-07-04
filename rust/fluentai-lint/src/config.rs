//! Configuration for the linter

use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::path::Path;
use anyhow::Result;

/// Linter configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct LintConfig {
    /// Global rule configuration
    pub rules: RuleConfig,
    /// Paths to ignore
    pub ignore: Vec<String>,
    /// File patterns to include
    pub include: Vec<String>,
    /// Additional rule directories
    pub rule_paths: Vec<String>,
    /// Maximum number of errors before stopping
    pub max_errors: Option<usize>,
}

/// Rule configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct RuleConfig {
    /// Default level for all rules
    pub default_level: RuleLevel,
    /// Specific rule levels
    pub levels: FxHashMap<String, RuleLevel>,
    /// Rule-specific configuration
    pub config: FxHashMap<String, toml::Value>,
}

/// Rule severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RuleLevel {
    /// Rule is disabled
    Off,
    /// Rule produces warnings
    Warn,
    /// Rule produces errors
    Error,
}

impl Default for LintConfig {
    fn default() -> Self {
        Self {
            rules: RuleConfig::default(),
            ignore: vec![
                "**/target/**".to_string(),
                "**/node_modules/**".to_string(),
                "**/.git/**".to_string(),
            ],
            include: vec!["**/*.fl".to_string(), "**/*.fluentai".to_string()],
            rule_paths: vec![],
            max_errors: Some(100),
        }
    }
}

impl Default for RuleConfig {
    fn default() -> Self {
        Self {
            default_level: RuleLevel::Warn,
            levels: FxHashMap::default(),
            config: FxHashMap::default(),
        }
    }
}

impl LintConfig {
    /// Load configuration from a file
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let config: LintConfig = toml::from_str(&content)?;
        Ok(config)
    }
    
    /// Load configuration from current directory
    pub fn from_current_dir() -> Result<Self> {
        let paths = [
            ".fluentai-lint.toml",
            "fluentai-lint.toml",
            ".fluentai/lint.toml",
        ];
        
        for path in &paths {
            if Path::new(path).exists() {
                return Self::from_file(Path::new(path));
            }
        }
        
        Ok(Self::default())
    }
    
    /// Get the level for a specific rule
    pub fn get_rule_level(&self, rule_id: &str) -> RuleLevel {
        self.rules.levels
            .get(rule_id)
            .copied()
            .unwrap_or(self.rules.default_level)
    }
    
    /// Check if a rule is enabled
    pub fn is_rule_enabled(&self, rule_id: &str) -> bool {
        self.get_rule_level(rule_id) != RuleLevel::Off
    }
    
    /// Get configuration for a specific rule
    pub fn get_rule_config(&self, rule_id: &str) -> Option<&toml::Value> {
        self.rules.config.get(rule_id)
    }
    
    /// Check if a path should be ignored
    pub fn should_ignore(&self, path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        
        for pattern in &self.ignore {
            if glob::Pattern::new(pattern).ok()
                .map(|p| p.matches(&path_str))
                .unwrap_or(false)
            {
                return true;
            }
        }
        
        false
    }
    
    /// Check if a path should be included
    pub fn should_include(&self, path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        
        for pattern in &self.include {
            if glob::Pattern::new(pattern).ok()
                .map(|p| p.matches(&path_str))
                .unwrap_or(false)
            {
                return true;
            }
        }
        
        false
    }
}

/// Configuration builder
pub struct ConfigBuilder {
    config: LintConfig,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        Self {
            config: LintConfig::default(),
        }
    }
    
    pub fn with_default_level(mut self, level: RuleLevel) -> Self {
        self.config.rules.default_level = level;
        self
    }
    
    pub fn with_rule_level(mut self, rule_id: impl Into<String>, level: RuleLevel) -> Self {
        self.config.rules.levels.insert(rule_id.into(), level);
        self
    }
    
    pub fn with_ignore_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.config.ignore.push(pattern.into());
        self
    }
    
    pub fn with_include_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.config.include.push(pattern.into());
        self
    }
    
    pub fn build(self) -> LintConfig {
        self.config
    }
}