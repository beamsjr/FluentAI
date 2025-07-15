//! Package management command implementation

use crate::config::Config;
use crate::PackageCommands;
use anyhow::Result;
use fluentai_package::registry::{HttpRegistry, LocalRegistry};
use fluentai_package::{Manifest, PackageInstaller, Registry};
use std::sync::Arc;
use std::collections::HashSet;

pub async fn handle_package_command(cmd: PackageCommands, config: &Config) -> Result<()> {
    // Create package config from CLI config
    let mut pkg_config = fluentai_package::PackageConfig::load().unwrap_or_default();

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

        PackageCommands::Add {
            package,
            version,
            dev,
        } => {
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
                display_dependency_tree(&manifest, &registry)?;
            } else {
                println!("Installed packages:");
                for (name, dep) in &manifest.dependencies {
                    let version = match dep {
                        fluentai_package::manifest::Dependency::Version(v) => v.clone(),
                        fluentai_package::manifest::Dependency::Detailed { version, .. } => version.clone(),
                    };
                    println!("  {} @ {}", name, version);
                }
                if !manifest.dev_dependencies.is_empty() {
                    println!("\nDev dependencies:");
                    for (name, dep) in &manifest.dev_dependencies {
                        let version = match dep {
                            fluentai_package::manifest::Dependency::Version(v) => v.clone(),
                            fluentai_package::manifest::Dependency::Detailed { version, .. } => version.clone(),
                        };
                        println!("  {} @ {} (dev)", name, version);
                    }
                }
            }
        }

        PackageCommands::Search { query, limit } => {
            let results = registry.search(&query, limit)?;

            println!("Search results for '{}':", query);
            for pkg in results {
                let version = pkg
                    .latest
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
            publish_package(&manifest, &registry, &token)?;
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
    let path = std::path::Path::new("fluentai.json");
    if !path.exists() {
        anyhow::bail!("No fluentai.json found. Run 'fluentai package init' to create one.");
    }

    let content = std::fs::read_to_string(path)?;
    let manifest: Manifest = serde_json::from_str(&content)?;
    Ok(manifest)
}

fn save_manifest(manifest: &Manifest) -> Result<()> {
    let content = serde_json::to_string_pretty(manifest)?;
    std::fs::write("fluentai.json", content)?;
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

    println!(
        "Added {} dependency: {}",
        if dev { "dev" } else { "runtime" },
        name
    );
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
    let script = manifest
        .scripts
        .get(script_name)
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

/// Display dependency tree in a hierarchical format
fn display_dependency_tree(manifest: &Manifest, registry: &Arc<dyn Registry>) -> Result<()> {
    use std::collections::HashSet;
    
    // Track visited packages to avoid cycles
    let mut visited = HashSet::new();
    
    // Display root package
    println!("{} @ {}", manifest.name, manifest.version);
    
    // Display dependencies
    if !manifest.dependencies.is_empty() {
        println!("├── dependencies:");
        let deps: Vec<_> = manifest.dependencies.iter().collect();
        for (i, (name, dep)) in deps.iter().enumerate() {
            let is_last = i == deps.len() - 1;
            let prefix = if is_last { "└── " } else { "├── " };
            let child_prefix = if is_last { "    " } else { "│   " };
            
            display_dependency(name, dep, registry, &mut visited, prefix, child_prefix, 1)?;
        }
    }
    
    // Display dev dependencies
    if !manifest.dev_dependencies.is_empty() {
        println!("└── dev-dependencies:");
        let dev_deps: Vec<_> = manifest.dev_dependencies.iter().collect();
        for (i, (name, dep)) in dev_deps.iter().enumerate() {
            let is_last = i == dev_deps.len() - 1;
            let prefix = if is_last { "    └── " } else { "    ├── " };
            let child_prefix = if is_last { "        " } else { "    │   " };
            
            display_dependency(name, dep, registry, &mut visited, prefix, child_prefix, 1)?;
        }
    }
    
    Ok(())
}

/// Display a single dependency and its transitive dependencies
fn display_dependency(
    name: &str,
    dep: &fluentai_package::manifest::Dependency,
    registry: &Arc<dyn Registry>,
    visited: &mut HashSet<String>,
    prefix: &str,
    child_prefix: &str,
    depth: usize,
) -> Result<()> {
    let version = match dep {
        fluentai_package::manifest::Dependency::Version(v) => v.clone(),
        fluentai_package::manifest::Dependency::Detailed { version, .. } => version.clone(),
    };
    
    // Display the dependency
    println!("{}{} @ {}", prefix, name, version);
    
    // Avoid infinite recursion
    if depth > 5 || visited.contains(name) {
        if visited.contains(name) {
            println!("{}(circular dependency)", child_prefix);
        }
        return Ok(());
    }
    
    visited.insert(name.to_string());
    
    // Try to fetch package metadata to show transitive dependencies
    match fluentai_package::Version::parse(&version) {
        Ok(version_obj) => {
            match registry.get_package_version(name, &version_obj) {
            Ok(pkg_version) => {
                // If we have manifest data, show transitive dependencies
                if let Some(manifest_data) = pkg_version.manifest {
                    if !manifest_data.dependencies.is_empty() {
                        let trans_deps: Vec<_> = manifest_data.dependencies.iter().collect();
                        for (i, (trans_name, trans_dep)) in trans_deps.iter().enumerate() {
                            let is_last = i == trans_deps.len() - 1;
                            let next_prefix = format!("{}{}", child_prefix, if is_last { "└── " } else { "├── " });
                            let next_child_prefix = format!("{}{}", child_prefix, if is_last { "    " } else { "│   " });
                            
                            display_dependency(
                                trans_name,
                                trans_dep,
                                registry,
                                visited,
                                &next_prefix,
                                &next_child_prefix,
                                depth + 1,
                            )?;
                        }
                    }
                }
            }
            Err(_) => {
                // Error fetching package info - just skip
            }
        }
        }
        Err(_) => {
            // Invalid version string - skip
        }
    }
    
    visited.remove(name);
    Ok(())
}

/// Publish a package to the registry
fn publish_package(manifest: &Manifest, registry: &Arc<dyn Registry>, token: &str) -> Result<()> {
    use std::fs;
    use std::io::Write;
    use tar::Builder;
    use flate2::write::GzEncoder;
    use flate2::Compression;
    use tempfile::NamedTempFile;
    
    println!("Publishing {} v{}...", manifest.name, manifest.version);
    
    // Validate manifest
    validate_manifest_for_publish(manifest)?;
    
    // Create a temporary tarball
    let mut tarball = NamedTempFile::new()?;
    let encoder = GzEncoder::new(&mut tarball, Compression::default());
    let mut tar_builder = Builder::new(encoder);
    
    // Add files to tarball
    let files_to_include = get_files_to_include(manifest)?;
    let base_path = std::env::current_dir()?;
    
    for file_path in files_to_include {
        let relative_path = file_path.strip_prefix(&base_path)
            .unwrap_or(&file_path);
        
        if file_path.is_file() {
            let mut file = fs::File::open(&file_path)?;
            tar_builder.append_file(relative_path, &mut file)?;
        }
    }
    
    // Add manifest
    let manifest_json = serde_json::to_vec_pretty(manifest)?;
    let mut header = tar::Header::new_gnu();
    header.set_path("fluentai.json")?;
    header.set_size(manifest_json.len() as u64);
    header.set_mode(0o644);
    header.set_cksum();
    tar_builder.append(&header, &manifest_json[..])?;
    
    // Finish the tarball
    tar_builder.into_inner()?.finish()?;
    tarball.flush()?;
    
    // Publish to registry
    let tarball_path = tarball.path();
    println!("Uploading package...");
    
    match registry.publish(manifest, tarball_path, token) {
        Ok(_) => {
            println!("✓ Successfully published {} v{}", manifest.name, manifest.version);
            println!("  View at: https://registry.fluentai.dev/packages/{}", manifest.name);
        }
        Err(e) => {
            return Err(anyhow::anyhow!("Failed to publish package: {}", e));
        }
    }
    
    Ok(())
}

/// Validate manifest before publishing
fn validate_manifest_for_publish(manifest: &Manifest) -> Result<()> {
    // Check required fields
    if manifest.name.is_empty() {
        return Err(anyhow::anyhow!("Package name is required"));
    }
    
    if manifest.version.is_empty() {
        return Err(anyhow::anyhow!("Package version is required"));
    }
    
    // Validate package name format
    if !is_valid_package_name(&manifest.name) {
        return Err(anyhow::anyhow!(
            "Invalid package name '{}'. Package names must be lowercase, \
             start with a letter, and contain only letters, numbers, and hyphens.",
            manifest.name
        ));
    }
    
    // Validate version format
    if fluentai_package::Version::parse(&manifest.version).is_err() {
        return Err(anyhow::anyhow!(
            "Invalid version '{}'. Version must follow semantic versioning (e.g., 1.0.0)",
            manifest.version
        ));
    }
    
    // Warn about missing optional fields
    if manifest.description.is_none() {
        eprintln!("Warning: No description provided");
    }
    
    if manifest.license.is_none() {
        eprintln!("Warning: No license specified");
    }
    
    Ok(())
}

/// Check if a package name is valid
fn is_valid_package_name(name: &str) -> bool {
    !name.is_empty() 
        && name.chars().all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-')
        && name.chars().next().map_or(false, |c| c.is_ascii_lowercase())
        && !name.starts_with('-')
        && !name.ends_with('-')
        && !name.contains("--")
}

/// Get list of files to include in the package
fn get_files_to_include(manifest: &Manifest) -> Result<Vec<std::path::PathBuf>> {
    use glob::glob;
    
    let mut files = Vec::new();
    let base_path = std::env::current_dir()?;
    
    // If files are explicitly listed in manifest, use those
    if !manifest.files.is_empty() {
        for pattern in &manifest.files {
            for entry in glob(pattern)? {
                if let Ok(path) = entry {
                    files.push(path);
                }
            }
        }
    } else {
        // Default patterns to include
        let default_patterns = vec![
            "**/*.flc",
            "README*",
            "LICENSE*",
            "CHANGELOG*",
        ];
        
        for pattern in default_patterns {
            for entry in glob(pattern)? {
                if let Ok(path) = entry {
                    // Exclude certain directories
                    let path_str = path.to_string_lossy();
                    if !path_str.contains("node_modules") 
                        && !path_str.contains("target")
                        && !path_str.contains(".git")
                        && !path_str.starts_with(".") {
                        files.push(path);
                    }
                }
            }
        }
    }
    
    // Always include the manifest
    files.push(base_path.join("fluentai.json"));
    
    // Remove duplicates
    files.sort();
    files.dedup();
    
    if files.len() == 1 {  // Only manifest
        return Err(anyhow::anyhow!("No source files found to publish"));
    }
    
    Ok(files)
}
