//! Game template

use super::{Template, TemplateCategory, TemplateOptions, helpers};
use anyhow::Result;
use std::path::Path;

pub struct GameTemplate;

impl Template for GameTemplate {
    fn name(&self) -> &'static str {
        "game"
    }
    
    fn description(&self) -> &'static str {
        "Game with rendering pipeline and input handling"
    }
    
    fn category(&self) -> TemplateCategory {
        TemplateCategory::Game
    }
    
    fn create(&self, path: &Path, name: &str, _options: &TemplateOptions) -> Result<()> {
        // TODO: Implement game template
        helpers::create_project_file(path, name, "Exe", &[
            ("FluentAI.Game", "1.0.0"),
            ("FluentAI.Graphics", "1.0.0"),
            ("FluentAI.Audio", "1.0.0"),
        ])?;
        helpers::create_gitignore(path)?;
        helpers::create_readme(path, name, "A game built with FluentAI.")?;
        Ok(())
    }
}