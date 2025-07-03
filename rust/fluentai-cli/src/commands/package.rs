//! Package management command implementation

use anyhow::Result;
use crate::config::Config;
use crate::PackageCommands;
use fluentai_package::{Manifest, PackageInstaller, Registry};
use fluentai_package::registry::{LocalRegistry, HttpRegistry};
use std::sync::Arc;

pub async fn handle_package_command(cmd: PackageCommands, config: &Config) -> Result<()> {
    // Create package config from CLI config
    let mut pkg_config = fluentai_package::PackageConfig::load()
        .unwrap_or_default();
    
    pkg_config.offline = config.package.offline;
    pkg_config.registry = config.package.registry.clone();
    
    // Get current directory
    let cwd = std::env::current_dir()?;
    
    // Create registry
    let registry: Arc<dyn Registry> = if pkg_config.offline {
        Arc::new(LocalRegistry::new(pkg_config.cache_dir.clone()))
    } else {
        Arc::new(HttpRegistry::new(pkg_config.registry.clone()))
    };
    
    match cmd {
        PackageCommands::Init { name } => {
            init_package(&cwd, name)?;
        }
        
        PackageCommands::Install { dev } => {
            let manifest = load_manifest()?;
            let installer = PackageInstaller::new(pkg_config, registry, cwd);
            installer.install(&manifest, dev)?;
        }
        
        PackageCommands::Add { package, version, dev } => {
            let mut manifest = load_manifest()?;
            add_dependency(&mut manifest, &package, version, dev)?;
            save_manifest(&manifest)?;
            
            // Install the new dependency
            let installer = PackageInstaller::new(pkg_config, registry, cwd);
            installer.install(&manifest, dev)?;
        }
        
        PackageCommands::Remove { package } => {
            let mut manifest = load_manifest()?;
            remove_dependency(&mut manifest, &package)?;
            save_manifest(&manifest)?;
        }
        
        PackageCommands::Update { packages } => {
            let manifest = load_manifest()?;
            let installer = PackageInstaller::new(pkg_config, registry, cwd);
            
            if packages.is_empty() {
                installer.update(&manifest, None)?;
            } else {
                installer.update(&manifest, Some(packages))?;
            }
        }
        
        PackageCommands::List { tree } => {
            let manifest = load_manifest()?;
            if tree {
                println!("Package dependency tree:");
                // TODO: Implement tree display
            } else {
                println!("Installed packages:");
                for (name, _dep) in &manifest.dependencies {
                    println!("  {}", name);
                }
            }
        }
        
        PackageCommands::Search { query, limit } => {
            let results = registry.search(&query, limit)?;
            
            println!("Search results for '{}':", query);
            for pkg in results {
                let version = pkg.latest
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "unknown".to_string());
                println!("  {} ({})", pkg.name, version);
                if let Some(desc) = &pkg.description {
                    println!("    {}", desc);
                }
            }
        }
        
        PackageCommands::Publish { token } => {
            let manifest = load_manifest()?;
            // TODO: Build and publish package
            println!("Publishing {} v{}", manifest.name, manifest.version);
            println!("Token: {}", token);
        }
        
        PackageCommands::Run { script, args } => {
            let manifest = load_manifest()?;
            run_script(&manifest, &script, args)?;
        }
        
        PackageCommands::Clean => {
            let installer = PackageInstaller::new(pkg_config, registry, cwd);
            installer.clean()?;
        }
    }
    
    Ok(())
}

fn init_package(dir: &std::path::Path, name: Option<String>) -> Result<()> {
    let name = name.unwrap_or_else(|| {
        dir.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("my-package")
            .to_string()
    });
    
    let manifest = Manifest {
        name: name.clone(),
        version: "0.1.0".to_string(),
        description: Some(format!("{} package", name)),
        ..Default::default()
    };
    
    save_manifest(&manifest)?;
    println!("Initialized package: {}", name);
    
    Ok(())
}

fn load_manifest() -> Result<Manifest> {
    let path = std::path::Path::new("claude.json");
    if !path.exists() {
        anyhow::bail!("No claude.json found. Run 'claudelang package init' to create one.");
    }
    
    let content = std::fs::read_to_string(path)?;
    let manifest: Manifest = serde_json::from_str(&content)?;
    Ok(manifest)
}

fn save_manifest(manifest: &Manifest) -> Result<()> {
    let content = serde_json::to_string_pretty(manifest)?;
    std::fs::write("claude.json", content)?;
    Ok(())
}

fn add_dependency(
    manifest: &mut Manifest,
    name: &str,
    version: Option<String>,
    dev: bool,
) -> Result<()> {
    use fluentai_package::manifest::Dependency;
    
    let dep = match version {
        Some(v) => Dependency::Version(v),
        None => Dependency::Version("*".to_string()),
    };
    
    if dev {
        manifest.dev_dependencies.insert(name.to_string(), dep);
    } else {
        manifest.dependencies.insert(name.to_string(), dep);
    }
    
    println!("Added {} dependency: {}", if dev { "dev" } else { "runtime" }, name);
    Ok(())
}

fn remove_dependency(manifest: &mut Manifest, name: &str) -> Result<()> {
    let removed = manifest.dependencies.remove(name).is_some()
        || manifest.dev_dependencies.remove(name).is_some();
    
    if removed {
        println!("Removed dependency: {}", name);
    } else {
        println!("Dependency not found: {}", name);
    }
    
    Ok(())
}

fn run_script(manifest: &Manifest, script_name: &str, args: Vec<String>) -> Result<()> {
    let script = manifest.scripts.get(script_name)
        .ok_or_else(|| anyhow::anyhow!("Script '{}' not found", script_name))?;
    
    match script {
        fluentai_package::manifest::Script::Command(cmd) => {
            // Simple command execution
            let mut parts = cmd.split_whitespace();
            let program = parts.next().unwrap();
            let mut cmd_args: Vec<_> = parts.map(|s| s.to_string()).collect();
            cmd_args.extend(args);
            
            let status = std::process::Command::new(program)
                .args(&cmd_args)
                .status()?;
            
            if !status.success() {
                anyhow::bail!("Script '{}' failed", script_name);
            }
        }
        fluentai_package::manifest::Script::Detailed { cmd, env, cwd } => {
            let mut command = std::process::Command::new(cmd);
            
            for (key, value) in env {
                command.env(key, value);
            }
            
            if let Some(working_dir) = cwd {
                command.current_dir(working_dir);
            }
            
            command.args(&args);
            
            let status = command.status()?;
            
            if !status.success() {
                anyhow::bail!("Script '{}' failed", script_name);
            }
        }
    }
    
    Ok(())
}