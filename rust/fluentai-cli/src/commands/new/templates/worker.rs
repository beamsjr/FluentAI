//! Background worker template

use super::{Template, TemplateCategory, TemplateOptions, helpers};
use anyhow::Result;
use std::path::Path;

pub struct WorkerTemplate;

impl Template for WorkerTemplate {
    fn name(&self) -> &'static str {
        "worker"
    }
    
    fn description(&self) -> &'static str {
        "Background job processor with queue integration"
    }
    
    fn category(&self) -> TemplateCategory {
        TemplateCategory::Service
    }
    
    fn create(&self, path: &Path, name: &str, _options: &TemplateOptions) -> Result<()> {
        // TODO: Implement worker template
        helpers::create_project_file(path, name, "Exe", &[
            ("FluentAI.Queue", "1.0.0"),
            ("FluentAI.Scheduler", "1.0.0"),
        ])?;
        helpers::create_gitignore(path)?;
        helpers::create_readme(path, name, "A background worker service.")?;
        Ok(())
    }
}