//! Configuration handling for FluentAi CLI

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub visualization: VisualizationConfig,
    
    #[serde(default)]
    pub repl: ReplConfig,
    
    #[serde(default)]
    pub package: PackageConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisualizationConfig {
    #[serde(default = "default_viz_port")]
    pub port: u16,
    
    #[serde(default = "default_true")]
    pub auto_open_browser: bool,
    
    #[serde(default = "default_theme")]
    pub theme: String,
    
    #[serde(default)]
    pub delay_ms: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReplConfig {
    #[serde(default = "default_true")]
    pub show_ast: bool,
    
    #[serde(default)]
    pub history_file: Option<PathBuf>,
    
    #[serde(default = "default_prompt")]
    pub prompt: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageConfig {
    #[serde(default = "default_registry")]
    pub registry: String,
    
    #[serde(default)]
    pub offline: bool,
    
    #[serde(default)]
    pub cache_dir: Option<PathBuf>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            visualization: VisualizationConfig::default(),
            repl: ReplConfig::default(),
            package: PackageConfig::default(),
        }
    }
}

impl Default for VisualizationConfig {
    fn default() -> Self {
        Self {
            port: default_viz_port(),
            auto_open_browser: default_true(),
            theme: default_theme(),
            delay_ms: 0,
        }
    }
}

impl Default for ReplConfig {
    fn default() -> Self {
        Self {
            show_ast: default_true(),
            history_file: None,
            prompt: default_prompt(),
        }
    }
}

impl Default for PackageConfig {
    fn default() -> Self {
        Self {
            registry: default_registry(),
            offline: false,
            cache_dir: None,
        }
    }
}

fn default_viz_port() -> u16 { 8080 }
fn default_true() -> bool { true }
fn default_theme() -> String { "dark".to_string() }
fn default_prompt() -> String { "claude> ".to_string() }
fn default_registry() -> String { "https://registry.claudelang.org".to_string() }

/// Load configuration from file or use defaults
pub fn load_config(path: Option<PathBuf>) -> Result<Config> {
    if let Some(path) = path {
        let content = std::fs::read_to_string(&path)?;
        let config: Config = toml::from_str(&content)?;
        Ok(config)
    } else if let Some(home) = dirs::home_dir() {
        let default_path = home.join(".claudelang").join("config.toml");
        if default_path.exists() {
            let content = std::fs::read_to_string(&default_path)?;
            let config: Config = toml::from_str(&content)?;
            Ok(config)
        } else {
            Ok(Config::default())
        }
    } else {
        Ok(Config::default())
    }
}

/// Save configuration to file
pub fn save_config(config: &Config, path: &Path) -> Result<()> {
    let content = toml::to_string_pretty(config)?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, content)?;
    Ok(())
}