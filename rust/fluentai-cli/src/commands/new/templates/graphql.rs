//! GraphQL server template

use super::{Template, TemplateCategory, TemplateOptions, helpers};
use anyhow::Result;
use std::path::Path;

pub struct GraphQLTemplate;

impl Template for GraphQLTemplate {
    fn name(&self) -> &'static str {
        "graphql"
    }
    
    fn description(&self) -> &'static str {
        "GraphQL API server with schema-first design"
    }
    
    fn aliases(&self) -> Vec<&'static str> {
        vec!["gql"]
    }
    
    fn category(&self) -> TemplateCategory {
        TemplateCategory::Service
    }
    
    fn create(&self, path: &Path, name: &str, _options: &TemplateOptions) -> Result<()> {
        // TODO: Implement GraphQL template
        helpers::create_project_file(path, name, "Exe", &[
            ("FluentAI.GraphQL", "1.0.0"),
            ("FluentAI.Http", "1.0.0"),
        ])?;
        helpers::create_gitignore(path)?;
        helpers::create_readme(path, name, "A GraphQL API server.")?;
        Ok(())
    }
}