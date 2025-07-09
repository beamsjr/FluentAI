//! Create new FluentAI projects from templates

use anyhow::{Context, Result};
use std::path::PathBuf;
use colored::*;

pub mod templates;
use templates::{TemplateRegistry, TemplateOptions};

/// Create a new FluentAI project
pub async fn new_project(
    template: &str,
    name: &str,
    path: Option<PathBuf>,
) -> Result<()> {
    let registry = TemplateRegistry::new();
    
    // Handle special commands
    if template == "--list" || template == "list" {
        list_templates(&registry);
        return Ok(());
    }
    
    // Find template
    let template_impl = registry.find(template)
        .with_context(|| {
            format!("Unknown template '{}'. Use 'fluentai new --list' to see available templates.", template)
        })?;
    
    let project_path = path.unwrap_or_else(|| PathBuf::from(name));
    
    // Check if directory already exists
    if project_path.exists() {
        anyhow::bail!("Directory '{}' already exists", project_path.display());
    }
    
    // Create project directory
    std::fs::create_dir_all(&project_path)?;
    
    // TODO: Parse template options from command line
    let options = TemplateOptions::default();
    
    // Create project from template
    template_impl.create(&project_path, name, &options)?;
    
    println!("{} Created new {} project '{}'", 
             "✓".green().bold(),
             template_impl.name(),
             name);
    println!("\nNext steps:");
    println!("  cd {}", project_path.display());
    println!("  fluentai restore");
    println!("  fluentai run");
    
    Ok(())
}

/// List available templates
fn list_templates(registry: &TemplateRegistry) {
    println!("{}", "Available project templates:".bold());
    println!();
    
    // Group by category
    let categories = [
        templates::TemplateCategory::Application,
        templates::TemplateCategory::Service,
        templates::TemplateCategory::Library,
        templates::TemplateCategory::Tool,
    ];
    
    for category in &categories {
        let templates = registry.by_category(*category);
        if !templates.is_empty() {
            println!("{}:", category.as_str().yellow());
            for template in templates {
                let aliases = if template.aliases().is_empty() {
                    String::new()
                } else {
                    format!(" ({})", template.aliases().join(", "))
                };
                println!("  {} {}{} - {}", 
                         "•".blue(),
                         template.name().bold(),
                         aliases.dimmed(),
                         template.description());
            }
            println!();
        }
    }
    
    println!("Usage:");
    println!("  fluentai new <template> <name> [options]");
    println!();
    println!("Examples:");
    println!("  fluentai new console MyApp");
    println!("  fluentai new api MyAPI --auth jwt --database postgres");
    println!("  fluentai new web MyWebApp --frontend htmx");
}