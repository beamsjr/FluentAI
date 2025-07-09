//! Desktop application template

use super::{Template, TemplateCategory, TemplateOptions, helpers};
use anyhow::Result;
use std::path::Path;

pub struct DesktopTemplate;

impl Template for DesktopTemplate {
    fn name(&self) -> &'static str {
        "desktop"
    }
    
    fn description(&self) -> &'static str {
        "Native desktop application with GUI"
    }
    
    fn aliases(&self) -> Vec<&'static str> {
        vec!["gui", "app"]
    }
    
    fn category(&self) -> TemplateCategory {
        TemplateCategory::Application
    }
    
    fn create(&self, path: &Path, name: &str, _options: &TemplateOptions) -> Result<()> {
        // TODO: Implement desktop template
        helpers::create_project_file(path, name, "Exe", &[
            ("FluentAI.Desktop", "1.0.0"),
            ("FluentAI.UI", "1.0.0"),
        ])?;
        helpers::create_gitignore(path)?;
        helpers::create_readme(path, name, "A desktop application.")?;
        Ok(())
    }
}