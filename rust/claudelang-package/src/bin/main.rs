//! ClaudeLang package manager CLI

use anyhow::Result;
use clap::{Parser, Subcommand};
use claudelang_package::{
    Manifest, PackageConfig, PackageInstaller, Registry,
    registry::{LocalRegistry, HttpRegistry},
};
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Parser)]
#[command(name = "claudelang")]
#[command(about = "ClaudeLang package manager", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
    
    /// Path to manifest file
    #[arg(long, default_value = "claude.json")]
    manifest_path: PathBuf,
    
    /// Use offline mode
    #[arg(long)]
    offline: bool,
    
    /// Custom registry URL
    #[arg(long)]
    registry: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new package
    Init {
        /// Package name
        #[arg(long)]
        name: Option<String>,
    },
    
    /// Install dependencies
    Install {
        /// Include dev dependencies
        #[arg(long)]
        dev: bool,
    },
    
    /// Add a dependency
    Add {
        /// Package name
        package: String,
        
        /// Version requirement
        #[arg(long)]
        version: Option<String>,
        
        /// Add as dev dependency
        #[arg(long)]
        dev: bool,
    },
    
    /// Remove a dependency
    Remove {
        /// Package name
        package: String,
    },
    
    /// Update dependencies
    Update {
        /// Specific packages to update
        packages: Vec<String>,
    },
    
    /// List installed packages
    List {
        /// Show as tree
        #[arg(long)]
        tree: bool,
    },
    
    /// Search for packages
    Search {
        /// Search query
        query: String,
        
        /// Maximum results
        #[arg(long, default_value = "20")]
        limit: usize,
    },
    
    /// Publish a package
    Publish {
        /// Authentication token
        #[arg(long, env = "CLAUDELANG_TOKEN")]
        token: String,
    },
    
    /// Run a script
    Run {
        /// Script name
        script: String,
        
        /// Script arguments
        args: Vec<String>,
    },
    
    /// Clean unused packages
    Clean,
}

fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();
    
    let cli = Cli::parse();
    
    // Load configuration
    let mut config = PackageConfig::load()?;
    if cli.offline {
        config.offline = true;
    }
    
    // Get current directory
    let cwd = std::env::current_dir()?;
    
    // Create registry
    let registry: Arc<dyn Registry> = if config.offline {
        Arc::new(LocalRegistry::new(config.cache_dir.clone()))
    } else {
        let registry_url = cli.registry
            .as_deref()
            .or(Some(&config.registry))
            .unwrap()
            .to_string();
        Arc::new(HttpRegistry::new(registry_url))
    };
    
    // Execute command
    match cli.command {
        Commands::Init { name } => {
            init_package(&cwd, name)?;
        }
        
        Commands::Install { dev } => {
            let manifest = load_manifest(&cli.manifest_path)?;
            let installer = PackageInstaller::new(config, registry, cwd);
            installer.install(&manifest, dev)?;
        }
        
        Commands::Add { package, version, dev } => {
            let mut manifest = load_manifest(&cli.manifest_path)?;
            let installer = PackageInstaller::new(config, registry, cwd);
            installer.add(&mut manifest, &package, version.as_deref(), dev)?;
        }
        
        Commands::Remove { package } => {
            let mut manifest = load_manifest(&cli.manifest_path)?;
            let installer = PackageInstaller::new(config, registry, cwd);
            installer.remove(&mut manifest, &package)?;
        }
        
        Commands::Update { packages } => {
            let manifest = load_manifest(&cli.manifest_path)?;
            let installer = PackageInstaller::new(config, registry, cwd);
            let packages = if packages.is_empty() { None } else { Some(packages) };
            installer.update(&manifest, packages)?;
        }
        
        Commands::List { tree } => {
            let installer = PackageInstaller::new(config, registry, cwd);
            installer.list(tree)?;
        }
        
        Commands::Search { query, limit } => {
            let results = registry.search(&query, limit)?;
            for package in results {
                println!("{} - {}", 
                    package.name,
                    package.description.as_deref().unwrap_or("No description")
                );
                if let Some(latest) = package.latest {
                    println!("  Latest: {}", latest);
                }
            }
        }
        
        Commands::Publish { token } => {
            let manifest = load_manifest(&cli.manifest_path)?;
            publish_package(&manifest, &registry, &token)?;
        }
        
        Commands::Run { script, args } => {
            let manifest = load_manifest(&cli.manifest_path)?;
            run_script(&manifest, &script, args)?;
        }
        
        Commands::Clean => {
            let installer = PackageInstaller::new(config, registry, cwd);
            installer.clean()?;
        }
    }
    
    Ok(())
}

fn init_package(dir: &PathBuf, name: Option<String>) -> Result<()> {
    let manifest_path = dir.join("claude.json");
    
    if manifest_path.exists() {
        eprintln!("Package already initialized");
        return Ok(());
    }
    
    // Get package name
    let package_name = name.or_else(|| {
        dir.file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.to_string())
    }).unwrap_or_else(|| "my-package".to_string());
    
    // Create default manifest
    let mut manifest = Manifest::default();
    manifest.name = package_name;
    manifest.version = "0.1.0".to_string();
    manifest.description = Some("A new ClaudeLang package".to_string());
    
    // Add default scripts
    manifest.scripts.insert(
        "test".to_string(),
        claudelang_package::manifest::Script::Command("claudelang test".to_string()),
    );
    
    // Save manifest
    manifest.save(&manifest_path)?;
    
    // Create source directory
    std::fs::create_dir_all(dir.join("src"))?;
    
    // Create main.cl
    let main_content = r#"; Main entry point for the package

(define main ()
  (println "Hello from ClaudeLang!"))

(export main)
"#;
    std::fs::write(dir.join("src").join("main.cl"), main_content)?;
    
    println!("Initialized package: {}", manifest.name);
    Ok(())
}

fn load_manifest(path: &PathBuf) -> Result<Manifest> {
    if !path.exists() {
        anyhow::bail!("No claude.json found. Run 'claudelang init' to create one.");
    }
    
    Ok(Manifest::from_file(path)?)
}

fn publish_package(manifest: &Manifest, registry: &Arc<dyn Registry>, token: &str) -> Result<()> {
    // Validate manifest
    manifest.validate()?;
    
    // Create tarball
    let temp_dir = tempfile::tempdir()?;
    let tarball_path = temp_dir.path().join(format!("{}-{}.tar.gz", manifest.name, manifest.version));
    
    // TODO: Create tarball from package files
    std::fs::write(&tarball_path, b"dummy tarball")?;
    
    // Publish to registry
    registry.publish(manifest, &tarball_path, token)?;
    
    println!("Published {}@{}", manifest.name, manifest.version);
    Ok(())
}

fn run_script(manifest: &Manifest, script_name: &str, args: Vec<String>) -> Result<()> {
    let script = manifest.scripts.get(script_name)
        .ok_or_else(|| anyhow::anyhow!("Script '{}' not found", script_name))?;
    
    match script {
        claudelang_package::manifest::Script::Command(cmd) => {
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
        claudelang_package::manifest::Script::Detailed { cmd, env, cwd } => {
            // Detailed command with environment
            let mut parts = cmd.split_whitespace();
            let program = parts.next().unwrap();
            let mut cmd_args: Vec<_> = parts.map(|s| s.to_string()).collect();
            cmd_args.extend(args);
            
            let mut command = std::process::Command::new(program);
            command.args(&cmd_args);
            
            // Set environment variables
            for (key, value) in env {
                command.env(key, value);
            }
            
            // Set working directory
            if let Some(cwd) = cwd {
                command.current_dir(cwd);
            }
            
            let status = command.status()?;
            
            if !status.success() {
                anyhow::bail!("Script '{}' failed", script_name);
            }
        }
    }
    
    Ok(())
}