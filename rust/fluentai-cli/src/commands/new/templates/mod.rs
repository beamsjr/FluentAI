//! FluentAI project templates
//!
//! This module contains all available project templates for the `fluentai new` command.

use anyhow::Result;
use std::path::Path;

pub mod api;
pub mod cli;
pub mod console;
pub mod library;
pub mod web;
pub mod webservice;
pub mod worker;

/// Template trait that all templates must implement
pub trait Template {
    /// Get the template name
    fn name(&self) -> &'static str;
    
    /// Get the template description
    fn description(&self) -> &'static str;
    
    /// Get template aliases (alternative names)
    fn aliases(&self) -> Vec<&'static str> {
        vec![]
    }
    
    /// Get template category
    fn category(&self) -> TemplateCategory;
    
    /// Create a new project from this template
    fn create(&self, path: &Path, name: &str, options: &TemplateOptions) -> Result<()>;
    
    /// Get available options for this template
    fn options(&self) -> Vec<TemplateOption> {
        vec![]
    }
}

/// Template categories for organization
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TemplateCategory {
    Application,
    Service,
    Library,
    Tool,
}

impl TemplateCategory {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Application => "Application",
            Self::Service => "Service",
            Self::Library => "Library",
            Self::Tool => "Tool",
        }
    }
}

/// Options that can be passed to templates
#[derive(Debug, Default)]
pub struct TemplateOptions {
    /// Authentication type (jwt, session, oauth, etc.)
    pub auth: Option<String>,
    /// Database type (postgres, mysql, sqlite, etc.)
    pub database: Option<String>,
    /// Frontend framework (if applicable)
    pub frontend: Option<String>,
    /// Include Docker support
    pub docker: bool,
    /// Include CI/CD configuration
    pub ci: bool,
    /// Custom options as key-value pairs
    pub custom: std::collections::HashMap<String, String>,
}

/// Template option definition
#[derive(Debug, Clone)]
pub struct TemplateOption {
    pub name: &'static str,
    pub description: &'static str,
    pub default: Option<&'static str>,
    pub choices: Vec<&'static str>,
}

/// Registry of all available templates
pub struct TemplateRegistry {
    templates: Vec<Box<dyn Template>>,
}

impl TemplateRegistry {
    /// Create a new template registry with all built-in templates
    pub fn new() -> Self {
        let templates: Vec<Box<dyn Template>> = vec![
            Box::new(console::ConsoleTemplate),
            Box::new(library::LibraryTemplate),
            Box::new(webservice::WebServiceTemplate),
            Box::new(api::ApiTemplate),
            Box::new(web::WebTemplate),
            Box::new(cli::CliTemplate),
            Box::new(worker::WorkerTemplate),
        ];
        
        Self { templates }
    }
    
    /// Find a template by name or alias
    pub fn find(&self, name: &str) -> Option<&dyn Template> {
        let name_lower = name.to_lowercase();
        
        self.templates.iter()
            .find(|t| {
                t.name().to_lowercase() == name_lower ||
                t.aliases().iter().any(|alias| alias.to_lowercase() == name_lower)
            })
            .map(|t| t.as_ref())
    }
    
    /// Get all templates
    pub fn all(&self) -> &[Box<dyn Template>] {
        &self.templates
    }
    
    /// Get templates by category
    pub fn by_category(&self, category: TemplateCategory) -> Vec<&dyn Template> {
        self.templates.iter()
            .filter(|t| t.category() == category)
            .map(|t| t.as_ref())
            .collect()
    }
}

/// Helper functions for template creation
pub mod helpers {
    use super::*;
    use std::fs;
    
    /// Create a project file (.aiproj)
    pub fn create_project_file(
        path: &Path,
        name: &str,
        output_type: &str,
        packages: &[(&str, &str)],
    ) -> Result<()> {
        let mut content = format!(
            r#"<Project Sdk="FluentAI.Sdk/1.0">
  <PropertyGroup>
    <OutputType>{}</OutputType>
    <TargetFramework>fluentai1.0</TargetFramework>
    <RootNamespace>{}</RootNamespace>
  </PropertyGroup>"#,
            output_type, name
        );
        
        if !packages.is_empty() {
            content.push_str("\n  \n  <ItemGroup>");
            for (package, version) in packages {
                content.push_str(&format!(
                    "\n    <PackageReference Include=\"{}\" Version=\"{}\" />",
                    package, version
                ));
            }
            content.push_str("\n  </ItemGroup>");
        }
        
        content.push_str("\n</Project>\n");
        
        fs::write(path.join(format!("{}.aiproj", name)), content)?;
        Ok(())
    }
    
    /// Create a gitignore file
    pub fn create_gitignore(path: &Path) -> Result<()> {
        let content = r#"# FluentAI
*.ailib
*.aipkg
/target/
/packages/
packages.lock

# IDE
.vscode/
.idea/
*.swp
*.swo

# OS
.DS_Store
Thumbs.db

# Environment
.env
.env.local
*.env

# Logs
*.log
logs/

# Testing
coverage/
.coverage

# Build artifacts
dist/
build/
"#;
        fs::write(path.join(".gitignore"), content)?;
        Ok(())
    }
    
    /// Create a README file
    pub fn create_readme(path: &Path, name: &str, description: &str) -> Result<()> {
        let content = format!(
            r#"# {}

{}

## Getting Started

### Prerequisites

- FluentAI SDK 1.0 or higher

### Installation

```bash
fluentai restore
```

### Running

```bash
fluentai run
```

### Testing

```bash
fluentai test
```

## Building

### Debug build

```bash
fluentai build
```

### Release build

```bash
fluentai build -c Release
```

## Deployment

```bash
fluentai publish -c Release
```

## License

This project is licensed under the MIT License.
"#,
            name, description
        );
        
        fs::write(path.join("README.md"), content)?;
        Ok(())
    }
    
    /// Create standard directory structure
    pub fn create_directories(path: &Path, dirs: &[&str]) -> Result<()> {
        for dir in dirs {
            fs::create_dir_all(path.join(dir))?;
        }
        Ok(())
    }
}