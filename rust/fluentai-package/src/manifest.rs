//! Package manifest (claude.json) handling

use crate::{PackageError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Package manifest structure (claude.json)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Manifest {
    /// Package name
    pub name: String,
    
    /// Package version
    pub version: String,
    
    /// Package description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    
    /// Package author(s)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub author: Option<String>,
    
    /// Package license
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
    
    /// Homepage URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub homepage: Option<String>,
    
    /// Repository URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
    
    /// Keywords for package discovery
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub keywords: Vec<String>,
    
    /// Main entry point
    #[serde(skip_serializing_if = "Option::is_none")]
    pub main: Option<String>,
    
    /// Runtime dependencies
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub dependencies: HashMap<String, Dependency>,
    
    /// Development dependencies
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub dev_dependencies: HashMap<String, Dependency>,
    
    /// Package scripts
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub scripts: HashMap<String, Script>,
    
    /// Files to include in the package
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub files: Vec<String>,
    
    /// Minimum FluentAi version required
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fluentai_version: Option<String>,
    
    /// Required effects for the package
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub effects: Vec<String>,
    
    /// Binary targets
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub bin: HashMap<String, String>,
    
    /// Library configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lib: Option<LibConfig>,
    
    /// Test configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub test: Option<TestConfig>,
    
    /// Additional metadata
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    /// Simple version string
    Version(String),
    
    /// Detailed dependency specification
    Detailed {
        /// Version requirement
        version: String,
        
        /// Optional registry URL
        #[serde(skip_serializing_if = "Option::is_none")]
        registry: Option<String>,
        
        /// Git repository URL
        #[serde(skip_serializing_if = "Option::is_none")]
        git: Option<String>,
        
        /// Git branch/tag/commit
        #[serde(skip_serializing_if = "Option::is_none")]
        rev: Option<String>,
        
        /// Local path
        #[serde(skip_serializing_if = "Option::is_none")]
        path: Option<String>,
        
        /// Optional features to enable
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        features: Vec<String>,
        
        /// Whether this is an optional dependency
        #[serde(default)]
        optional: bool,
    },
}

/// Script definition
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Script {
    /// Simple command string
    Command(String),
    
    /// Script with environment variables
    Detailed {
        /// Command to run
        cmd: String,
        
        /// Environment variables
        #[serde(default)]
        env: HashMap<String, String>,
        
        /// Working directory
        #[serde(skip_serializing_if = "Option::is_none")]
        cwd: Option<String>,
    },
}

/// Library configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LibConfig {
    /// Library name (if different from package name)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    
    /// Library entry point
    pub path: String,
    
    /// Whether this is a procedural macro library
    #[serde(default)]
    pub proc_macro: bool,
}

/// Test configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TestConfig {
    /// Test directory
    #[serde(default = "default_test_dir")]
    pub dir: String,
    
    /// Test pattern
    #[serde(default = "default_test_pattern")]
    pub pattern: String,
}

fn default_test_dir() -> String {
    "test".to_string()
}

fn default_test_pattern() -> String {
    "**/*_test.cl".to_string()
}

impl Manifest {
    /// Load manifest from a file
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = fs::read_to_string(path)
            .map_err(|e| PackageError::InvalidManifest {
                path: path.to_path_buf(),
                message: format!("Failed to read file: {}", e),
            })?;
        
        Self::from_str(&content)
            .map_err(|e| PackageError::InvalidManifest {
                path: path.to_path_buf(),
                message: format!("Failed to parse JSON: {}", e),
            })
    }
    
    /// Parse manifest from a string
    pub fn from_str(s: &str) -> Result<Self> {
        let manifest: Self = serde_json::from_str(s)?;
        manifest.validate()?;
        Ok(manifest)
    }
    
    /// Save manifest to a file
    pub fn save(&self, path: &Path) -> Result<()> {
        let content = serde_json::to_string_pretty(self)?;
        fs::write(path, content)?;
        Ok(())
    }
    
    /// Validate the manifest
    pub fn validate(&self) -> Result<()> {
        // Validate package name
        if self.name.is_empty() {
            return Err(PackageError::InvalidManifest {
                path: PathBuf::new(),
                message: "Package name cannot be empty".to_string(),
            });
        }
        
        if !is_valid_package_name(&self.name) {
            return Err(PackageError::InvalidManifest {
                path: PathBuf::new(),
                message: format!("Invalid package name: {}", self.name),
            });
        }
        
        // Validate version
        semver::Version::parse(&self.version)
            .map_err(|_| PackageError::InvalidVersion {
                version: self.version.clone(),
            })?;
        
        // Validate dependencies
        for (name, dep) in &self.dependencies {
            validate_dependency(name, dep)?;
        }
        
        for (name, dep) in &self.dev_dependencies {
            validate_dependency(name, dep)?;
        }
        
        Ok(())
    }
    
    /// Get all dependencies (including dev dependencies if requested)
    pub fn all_dependencies(&self, include_dev: bool) -> HashMap<String, Dependency> {
        let mut deps = self.dependencies.clone();
        if include_dev {
            deps.extend(self.dev_dependencies.clone());
        }
        deps
    }
    
    /// Check if a dependency is optional
    pub fn is_optional(&self, name: &str) -> bool {
        if let Some(dep) = self.dependencies.get(name) {
            matches!(dep, Dependency::Detailed { optional: true, .. })
        } else {
            false
        }
    }
}

/// Check if a package name is valid
fn is_valid_package_name(name: &str) -> bool {
    !name.is_empty() 
        && name.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        && name.chars().next().unwrap().is_alphabetic()
}

/// Validate a dependency specification
fn validate_dependency(name: &str, dep: &Dependency) -> Result<()> {
    if !is_valid_package_name(name) {
        return Err(PackageError::InvalidManifest {
            path: PathBuf::new(),
            message: format!("Invalid dependency name: {}", name),
        });
    }
    
    match dep {
        Dependency::Version(v) => {
            semver::VersionReq::parse(v)
                .map_err(|_| PackageError::InvalidVersion {
                    version: v.clone(),
                })?;
        }
        Dependency::Detailed { version, .. } => {
            semver::VersionReq::parse(version)
                .map_err(|_| PackageError::InvalidVersion {
                    version: version.clone(),
                })?;
        }
    }
    
    Ok(())
}

impl Default for Manifest {
    fn default() -> Self {
        Self {
            name: String::new(),
            version: "0.1.0".to_string(),
            description: None,
            author: None,
            license: None,
            homepage: None,
            repository: None,
            keywords: Vec::new(),
            main: Some("src/main.cl".to_string()),
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            scripts: HashMap::new(),
            files: Vec::new(),
            fluentai_version: None,
            effects: Vec::new(),
            bin: HashMap::new(),
            lib: None,
            test: None,
            metadata: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_manifest_parsing() {
        let json = r#"{
            "name": "test-package",
            "version": "1.0.0",
            "description": "A test package",
            "dependencies": {
                "foo": "^1.0.0",
                "bar": {
                    "version": "2.0.0",
                    "features": ["async"]
                }
            }
        }"#;
        
        let manifest = Manifest::from_str(json).unwrap();
        assert_eq!(manifest.name, "test-package");
        assert_eq!(manifest.version, "1.0.0");
        assert_eq!(manifest.dependencies.len(), 2);
    }
    
    #[test]
    fn test_invalid_package_name() {
        let json = r#"{
            "name": "123-invalid",
            "version": "1.0.0"
        }"#;
        
        assert!(Manifest::from_str(json).is_err());
    }
    
    #[test]
    fn test_dependency_validation() {
        let mut manifest = Manifest::default();
        manifest.name = "test".to_string();
        manifest.dependencies.insert(
            "foo".to_string(),
            Dependency::Version("invalid version".to_string()),
        );
        
        assert!(manifest.validate().is_err());
    }
}