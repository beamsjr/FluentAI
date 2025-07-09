//! Microservice template

use super::{Template, TemplateCategory, TemplateOptions, helpers};
use anyhow::Result;
use std::path::Path;

pub struct MicroserviceTemplate;

impl Template for MicroserviceTemplate {
    fn name(&self) -> &'static str {
        "microservice"
    }
    
    fn description(&self) -> &'static str {
        "Cloud-native microservice with health checks and metrics"
    }
    
    fn aliases(&self) -> Vec<&'static str> {
        vec!["service", "micro"]
    }
    
    fn category(&self) -> TemplateCategory {
        TemplateCategory::Service
    }
    
    fn create(&self, path: &Path, name: &str, _options: &TemplateOptions) -> Result<()> {
        // TODO: Implement microservice template
        helpers::create_project_file(path, name, "Exe", &[
            ("FluentAI.Http", "1.0.0"),
            ("FluentAI.Metrics", "1.0.0"),
            ("FluentAI.Discovery", "1.0.0"),
        ])?;
        helpers::create_gitignore(path)?;
        helpers::create_readme(path, name, "A cloud-native microservice.")?;
        Ok(())
    }
}