//! Script representation and loading

use std::path::{Path, PathBuf};
use std::fs;
use crate::error::{Error, Result};

/// A FluentAI script
#[derive(Debug, Clone)]
pub struct Script {
    /// Script name
    name: String,
    /// Script source code
    source: String,
    /// Source path (if loaded from file)
    path: Option<PathBuf>,
    /// Script metadata
    metadata: ScriptMetadata,
}

/// Script metadata
#[derive(Debug, Clone, Default)]
pub struct ScriptMetadata {
    /// Script version
    pub version: Option<String>,
    /// Script description
    pub description: Option<String>,
    /// Script author
    pub author: Option<String>,
    /// Required modules
    pub requires: Vec<String>,
}

impl Script {
    /// Create a new script from source
    pub fn new(name: impl Into<String>, source: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            source: source.into(),
            path: None,
            metadata: ScriptMetadata::default(),
        }
    }
    
    /// Load a script from file
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let source = fs::read_to_string(path)?;
        
        let name = path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unnamed")
            .to_string();
        
        let metadata = Self::parse_metadata(&source);
        
        Ok(Self {
            name,
            source,
            path: Some(path.to_path_buf()),
            metadata,
        })
    }
    
    /// Get script name
    pub fn name(&self) -> &str {
        &self.name
    }
    
    /// Get script source
    pub fn source(&self) -> &str {
        &self.source
    }
    
    /// Get script path
    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }
    
    /// Get script metadata
    pub fn metadata(&self) -> &ScriptMetadata {
        &self.metadata
    }
    
    /// Parse metadata from source comments
    fn parse_metadata(source: &str) -> ScriptMetadata {
        let mut metadata = ScriptMetadata::default();
        
        for line in source.lines() {
            let line = line.trim();
            if !line.starts_with(";;") {
                continue;
            }
            
            if let Some(version) = line.strip_prefix(";; @version") {
                metadata.version = Some(version.trim().to_string());
            } else if let Some(desc) = line.strip_prefix(";; @description") {
                metadata.description = Some(desc.trim().to_string());
            } else if let Some(author) = line.strip_prefix(";; @author") {
                metadata.author = Some(author.trim().to_string());
            } else if let Some(req) = line.strip_prefix(";; @requires") {
                metadata.requires.push(req.trim().to_string());
            }
        }
        
        metadata
    }
    
    /// Validate the script
    pub fn validate(&self) -> Result<()> {
        // Basic validation - check if source is not empty
        if self.source.trim().is_empty() {
            return Err(Error::script_error("Script source is empty"));
        }
        
        // TODO: Add more validation (syntax check, etc.)
        
        Ok(())
    }
    
    /// Get required modules
    pub fn required_modules(&self) -> &[String] {
        &self.metadata.requires
    }
}

/// Script builder
pub struct ScriptBuilder {
    name: String,
    source: Option<String>,
    metadata: ScriptMetadata,
}

impl ScriptBuilder {
    /// Create a new builder
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            source: None,
            metadata: ScriptMetadata::default(),
        }
    }
    
    /// Set source code
    pub fn source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }
    
    /// Set version
    pub fn version(mut self, version: impl Into<String>) -> Self {
        self.metadata.version = Some(version.into());
        self
    }
    
    /// Set description
    pub fn description(mut self, desc: impl Into<String>) -> Self {
        self.metadata.description = Some(desc.into());
        self
    }
    
    /// Set author
    pub fn author(mut self, author: impl Into<String>) -> Self {
        self.metadata.author = Some(author.into());
        self
    }
    
    /// Add required module
    pub fn requires(mut self, module: impl Into<String>) -> Self {
        self.metadata.requires.push(module.into());
        self
    }
    
    /// Build the script
    pub fn build(self) -> Result<Script> {
        let source = self.source
            .ok_or_else(|| Error::script_error("Script source is required"))?;
        
        let script = Script {
            name: self.name,
            source,
            path: None,
            metadata: self.metadata,
        };
        
        script.validate()?;
        Ok(script)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;
    use std::io::Write;
    
    #[test]
    fn test_script_creation() {
        let script = Script::new("test", "(+ 1 2)");
        assert_eq!(script.name(), "test");
        assert_eq!(script.source(), "(+ 1 2)");
        assert!(script.path().is_none());
    }
    
    #[test]
    fn test_script_from_file() {
        let mut file = NamedTempFile::new().unwrap();
        writeln!(file, ";; @version 1.0.0").unwrap();
        writeln!(file, ";; @description Test script").unwrap();
        writeln!(file, ";; @author Test Author").unwrap();
        writeln!(file, ";; @requires math").unwrap();
        writeln!(file, "(+ 1 2)").unwrap();
        file.flush().unwrap();
        
        let script = Script::from_file(file.path()).unwrap();
        assert_eq!(script.metadata.version.as_deref(), Some("1.0.0"));
        assert_eq!(script.metadata.description.as_deref(), Some("Test script"));
        assert_eq!(script.metadata.author.as_deref(), Some("Test Author"));
        assert_eq!(script.metadata.requires, vec!["math"]);
    }
    
    #[test]
    fn test_script_builder() {
        let script = ScriptBuilder::new("test")
            .source("(+ 1 2)")
            .version("1.0.0")
            .description("Test script")
            .author("Test Author")
            .requires("math")
            .build()
            .unwrap();
        
        assert_eq!(script.name(), "test");
        assert_eq!(script.metadata.version.as_deref(), Some("1.0.0"));
        assert_eq!(script.metadata.requires, vec!["math"]);
    }
}