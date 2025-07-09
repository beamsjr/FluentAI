//! Package manager configuration

use crate::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Package manager configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageConfig {
    /// Default registry URL
    #[serde(default = "default_registry")]
    pub registry: String,

    /// Alternative registries
    #[serde(default)]
    pub registries: Vec<RegistryConfig>,

    /// Global packages directory
    #[serde(default = "default_global_dir")]
    pub global_dir: PathBuf,

    /// Cache directory
    #[serde(default = "default_cache_dir")]
    pub cache_dir: PathBuf,

    /// Temporary directory for downloads
    #[serde(default = "default_temp_dir")]
    pub temp_dir: PathBuf,

    /// Whether to use colors in output
    #[serde(default = "default_color")]
    pub color: bool,

    /// Whether to verify checksums
    #[serde(default = "default_verify_checksums")]
    pub verify_checksums: bool,

    /// Whether to use offline mode
    #[serde(default)]
    pub offline: bool,

    /// HTTP timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout: u64,

    /// Maximum number of parallel downloads
    #[serde(default = "default_max_parallel")]
    pub max_parallel_downloads: usize,

    /// Authentication tokens for registries
    #[serde(default)]
    pub auth_tokens: std::collections::HashMap<String, String>,
}

/// Registry configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryConfig {
    /// Registry name
    pub name: String,

    /// Registry URL
    pub url: String,

    /// Authentication token
    #[serde(skip_serializing_if = "Option::is_none")]
    pub token: Option<String>,
}

fn default_registry() -> String {
    "https://registry.fluentai.org".to_string()
}

fn default_global_dir() -> PathBuf {
    dirs::data_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join("fluentai")
        .join("packages")
}

fn default_cache_dir() -> PathBuf {
    dirs::cache_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join("fluentai")
        .join("packages")
}

fn default_temp_dir() -> PathBuf {
    std::env::temp_dir().join("fluentai-package")
}

fn default_color() -> bool {
    true
}

fn default_verify_checksums() -> bool {
    true
}

fn default_timeout() -> u64 {
    300 // 5 minutes
}

fn default_max_parallel() -> usize {
    4
}

impl Default for PackageConfig {
    fn default() -> Self {
        Self {
            registry: default_registry(),
            registries: Vec::new(),
            global_dir: default_global_dir(),
            cache_dir: default_cache_dir(),
            temp_dir: default_temp_dir(),
            color: default_color(),
            verify_checksums: default_verify_checksums(),
            offline: false,
            timeout: default_timeout(),
            max_parallel_downloads: default_max_parallel(),
            auth_tokens: std::collections::HashMap::new(),
        }
    }
}

impl PackageConfig {
    /// Load configuration from default locations
    pub fn load() -> Result<Self> {
        // Try to load from config file
        if let Some(config_path) = Self::config_path() {
            if config_path.exists() {
                let content = std::fs::read_to_string(&config_path)?;
                let config: Self = toml::from_str(&content)?;
                return Ok(config);
            }
        }

        // Return default config
        Ok(Self::default())
    }

    /// Save configuration to file
    pub fn save(&self) -> Result<()> {
        if let Some(config_path) = Self::config_path() {
            // Create parent directory if needed
            if let Some(parent) = config_path.parent() {
                std::fs::create_dir_all(parent)?;
            }

            let content = toml::to_string_pretty(self)?;
            std::fs::write(&config_path, content)?;
        }

        Ok(())
    }

    /// Get the configuration file path
    pub fn config_path() -> Option<PathBuf> {
        dirs::config_dir().map(|d| d.join("fluentai").join("package.toml"))
    }

    /// Get registry URL by name
    pub fn get_registry_url(&self, name: Option<&str>) -> &str {
        if let Some(name) = name {
            self.registries
                .iter()
                .find(|r| r.name == name)
                .map(|r| r.url.as_str())
                .unwrap_or(&self.registry)
        } else {
            &self.registry
        }
    }

    /// Get authentication token for a registry
    pub fn get_auth_token(&self, registry_url: &str) -> Option<&str> {
        // Check specific registry configs first
        for reg in &self.registries {
            if reg.url == registry_url {
                if let Some(token) = &reg.token {
                    return Some(token);
                }
            }
        }

        // Check general auth tokens
        self.auth_tokens.get(registry_url).map(|s| s.as_str())
    }

    /// Add or update a registry
    pub fn add_registry(&mut self, name: String, url: String, token: Option<String>) {
        // Remove existing registry with same name
        self.registries.retain(|r| r.name != name);

        // Add new registry
        self.registries.push(RegistryConfig { name, url, token });
    }

    /// Set authentication token for a registry
    pub fn set_auth_token(&mut self, registry_url: String, token: String) {
        self.auth_tokens.insert(registry_url, token);
    }

    /// Create necessary directories
    pub fn ensure_directories(&self) -> Result<()> {
        std::fs::create_dir_all(&self.global_dir)?;
        std::fs::create_dir_all(&self.cache_dir)?;
        std::fs::create_dir_all(&self.temp_dir)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = PackageConfig::default();
        assert_eq!(config.registry, "https://registry.fluentai.org");
        assert!(config.verify_checksums);
        assert!(!config.offline);
    }

    #[test]
    fn test_registry_lookup() {
        let mut config = PackageConfig::default();
        config.add_registry(
            "custom".to_string(),
            "https://custom.registry.com".to_string(),
            Some("token123".to_string()),
        );

        assert_eq!(
            config.get_registry_url(None),
            "https://registry.fluentai.org"
        );
        assert_eq!(
            config.get_registry_url(Some("custom")),
            "https://custom.registry.com"
        );
        assert_eq!(
            config.get_auth_token("https://custom.registry.com"),
            Some("token123")
        );
    }
}
