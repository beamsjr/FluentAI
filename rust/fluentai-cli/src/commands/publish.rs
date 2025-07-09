//! Publish FluentAI projects

use anyhow::{Context, Result};
use colored::*;
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use zip::write::FileOptions;

/// Publish configuration
#[derive(Debug, Clone)]
pub struct PublishConfig {
    pub configuration: String,  // Debug or Release
    pub runtime: RuntimeOption, // Framework-dependent or Self-contained
    pub target: PublishTarget,  // Platform target
    pub output_path: Option<PathBuf>,
    pub single_file: bool, // Package as single executable
    pub trimmed: bool,     // Remove unused code
}

/// Runtime deployment option
#[derive(Debug, Clone)]
pub enum RuntimeOption {
    FrameworkDependent, // Requires FluentAI runtime installed
    SelfContained,      // Includes runtime in package
}

/// Publish target platform
#[derive(Debug, Clone)]
pub enum PublishTarget {
    Current, // Current platform
    Windows64,
    Linux64,
    MacOS64,
    MacOSArm64,
    WebAssembly,
}

impl PublishTarget {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "win-x64" => Some(PublishTarget::Windows64),
            "linux-x64" => Some(PublishTarget::Linux64),
            "osx-x64" => Some(PublishTarget::MacOS64),
            "osx-arm64" => Some(PublishTarget::MacOSArm64),
            "wasm" => Some(PublishTarget::WebAssembly),
            _ => None,
        }
    }

    pub fn to_string(&self) -> &'static str {
        match self {
            PublishTarget::Current => "current",
            PublishTarget::Windows64 => "win-x64",
            PublishTarget::Linux64 => "linux-x64",
            PublishTarget::MacOS64 => "osx-x64",
            PublishTarget::MacOSArm64 => "osx-arm64",
            PublishTarget::WebAssembly => "wasm",
        }
    }
}

/// Publish a FluentAI project
pub async fn publish(project_path: Option<PathBuf>, config: PublishConfig) -> Result<()> {
    let project_path = project_path.unwrap_or_else(|| PathBuf::from("."));

    println!("{} Publishing FluentAI application", "→".blue().bold());
    println!("  Configuration: {}", config.configuration);
    println!("  Runtime: {:?}", config.runtime);
    println!("  Target: {}", config.target.to_string());

    // Find project file
    let project_file = find_project_file(&project_path)?;
    let project = load_project(&project_file)?;

    // Determine output directory
    let output_dir = match config.output_path.as_ref() {
        Some(path) => path.clone(),
        None => project_path
            .join("publish")
            .join(&config.configuration.to_lowercase())
            .join(config.target.to_string()),
    };
    fs::create_dir_all(&output_dir)?;

    // Build the project first
    println!("\n{} Building project...", "→".cyan());
    build_project(&project_path, &config).await?;

    // Prepare publish artifacts
    let pb = ProgressBar::new_spinner();
    pb.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.green} {msg}")
            .unwrap(),
    );

    pb.set_message("Preparing publish artifacts...");

    // Copy compiled output
    let build_output = project_path
        .join("target")
        .join(&config.configuration.to_lowercase());

    let artifacts = collect_artifacts(&build_output, &project)?;

    pb.set_message("Packaging application...");

    // Package based on configuration
    let package_path = match config.runtime {
        RuntimeOption::FrameworkDependent => {
            package_framework_dependent(&artifacts, &output_dir, &project.name, &config)?
        }
        RuntimeOption::SelfContained => {
            package_self_contained(&artifacts, &output_dir, &project.name, &config)?
        }
    };

    pb.finish_and_clear();

    // Create deployment manifest
    create_deployment_manifest(&output_dir, &project, &config)?;

    println!("\n{} Publish succeeded", "✓".green().bold());
    println!("  Output: {}", package_path.display());

    // Print deployment instructions
    print_deployment_instructions(&config);

    Ok(())
}

/// Project info
struct Project {
    name: String,
    version: String,
    output_type: String,
}

/// Artifact info
struct Artifacts {
    main_executable: PathBuf,
    libraries: Vec<PathBuf>,
    resources: Vec<PathBuf>,
}

/// Find project file
fn find_project_file(path: &Path) -> Result<PathBuf> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("aiproj") {
            return Ok(path);
        }
    }
    anyhow::bail!("No .aiproj file found")
}

/// Load project info
fn load_project(project_file: &Path) -> Result<Project> {
    let content = fs::read_to_string(project_file)?;

    // Simple parsing (in real implementation, use XML parser)
    Ok(Project {
        name: project_file
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("Unknown")
            .to_string(),
        version: "1.0.0".to_string(),
        output_type: "Exe".to_string(),
    })
}

/// Build the project
async fn build_project(project_path: &Path, config: &PublishConfig) -> Result<()> {
    use crate::commands::build::{build, BuildConfig, BuildTarget};

    let build_config = BuildConfig {
        configuration: config.configuration.clone(),
        output_path: None,
        target: match config.target {
            PublishTarget::WebAssembly => BuildTarget::WebAssembly,
            _ => BuildTarget::Executable,
        },
        optimization_level: if config.configuration == "Release" {
            3
        } else {
            0
        },
        verbose: false,
    };

    build(Some(project_path.to_path_buf()), build_config).await?;

    Ok(())
}

/// Collect build artifacts
fn collect_artifacts(build_dir: &Path, project: &Project) -> Result<Artifacts> {
    let exe_name = if cfg!(windows) {
        format!("{}.exe", project.name)
    } else {
        project.name.clone()
    };

    let main_executable = build_dir.join(exe_name);
    if !main_executable.exists() {
        anyhow::bail!("Main executable not found: {}", main_executable.display());
    }

    // Collect libraries (*.ailib files)
    let mut libraries = Vec::new();
    for entry in fs::read_dir(build_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("ailib") {
            libraries.push(path);
        }
    }

    // Collect resources
    let resources = Vec::new(); // TODO: Implement resource collection

    Ok(Artifacts {
        main_executable,
        libraries,
        resources,
    })
}

/// Package framework-dependent deployment
fn package_framework_dependent(
    artifacts: &Artifacts,
    output_dir: &Path,
    project_name: &str,
    config: &PublishConfig,
) -> Result<PathBuf> {
    // Copy main executable
    let exe_name = artifacts.main_executable.file_name().unwrap();
    let output_exe = output_dir.join(exe_name);
    fs::copy(&artifacts.main_executable, &output_exe)?;

    // Copy libraries
    for lib in &artifacts.libraries {
        let lib_name = lib.file_name().unwrap();
        fs::copy(lib, output_dir.join(lib_name))?;
    }

    // Create runtime config
    let runtime_config = serde_json::json!({
        "runtimeOptions": {
            "framework": {
                "name": "FluentAI.Runtime",
                "version": "1.0.0"
            }
        }
    });

    let config_file = output_dir.join(format!("{}.runtimeconfig.json", project_name));
    fs::write(&config_file, serde_json::to_string_pretty(&runtime_config)?)?;

    Ok(output_dir.to_path_buf())
}

/// Package self-contained deployment
fn package_self_contained(
    artifacts: &Artifacts,
    output_dir: &Path,
    project_name: &str,
    config: &PublishConfig,
) -> Result<PathBuf> {
    if config.single_file {
        // Create single executable with embedded runtime
        let output_file = output_dir.join(if cfg!(windows) {
            format!("{}.exe", project_name)
        } else {
            project_name.to_string()
        });

        create_single_file_executable(artifacts, &output_file, config)?;

        Ok(output_file)
    } else {
        // Copy all files including runtime
        package_framework_dependent(artifacts, output_dir, project_name, config)?;

        // Copy runtime files
        copy_runtime_files(output_dir, &config.target)?;

        Ok(output_dir.to_path_buf())
    }
}

/// Create single-file executable
fn create_single_file_executable(
    artifacts: &Artifacts,
    output_file: &Path,
    config: &PublishConfig,
) -> Result<()> {
    // In a real implementation, this would:
    // 1. Create a native executable stub
    // 2. Embed the FluentAI runtime
    // 3. Embed the compiled bytecode
    // 4. Add resource extraction logic

    // For now, create a simple archive
    let file = fs::File::create(output_file)?;
    let mut zip = zip::ZipWriter::new(file);

    // Add main executable
    let options = FileOptions::default()
        .compression_method(zip::CompressionMethod::Stored)
        .unix_permissions(0o755);

    zip.start_file("main", options)?;
    let exe_data = fs::read(&artifacts.main_executable)?;
    zip.write_all(&exe_data)?;

    // Add libraries
    for lib in &artifacts.libraries {
        let lib_name = lib.file_name().unwrap().to_str().unwrap();
        zip.start_file(format!("lib/{}", lib_name), options)?;
        let lib_data = fs::read(lib)?;
        zip.write_all(&lib_data)?;
    }

    zip.finish()?;

    // Make executable on Unix
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(output_file)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(output_file, perms)?;
    }

    Ok(())
}

/// Copy runtime files for self-contained deployment
fn copy_runtime_files(output_dir: &Path, target: &PublishTarget) -> Result<()> {
    // In a real implementation, this would copy the FluentAI runtime
    // libraries for the target platform

    // For now, create placeholder files
    let runtime_files = vec![
        "fluentai_core_lib.dll",
        "fluentai_core.dll",
        "fluentai_stdlib.dll",
    ];

    for file in runtime_files {
        let runtime_file = output_dir.join(file);
        fs::write(&runtime_file, b"FluentAI Runtime Library")?;
    }

    Ok(())
}

/// Create deployment manifest
fn create_deployment_manifest(
    output_dir: &Path,
    project: &Project,
    config: &PublishConfig,
) -> Result<()> {
    let manifest = serde_json::json!({
        "name": project.name,
        "version": project.version,
        "runtime": match config.runtime {
            RuntimeOption::FrameworkDependent => "framework-dependent",
            RuntimeOption::SelfContained => "self-contained",
        },
        "target": config.target.to_string(),
        "configuration": config.configuration,
        "timestamp": chrono::Utc::now().to_rfc3339(),
    });

    let manifest_file = output_dir.join("deployment.json");
    fs::write(&manifest_file, serde_json::to_string_pretty(&manifest)?)?;

    Ok(())
}

/// Print deployment instructions
fn print_deployment_instructions(config: &PublishConfig) {
    println!("\n{} Deployment Instructions", "📦".blue());

    match config.runtime {
        RuntimeOption::FrameworkDependent => {
            println!("\nFramework-dependent deployment:");
            println!("1. Ensure FluentAI Runtime is installed on target machine:");
            println!("   curl -sSL https://fluentai.dev/install-runtime.sh | sh");
            println!("2. Copy the published files to the target machine");
            println!("3. Run with: fluentai-runtime <app-name>");
        }
        RuntimeOption::SelfContained => {
            println!("\nSelf-contained deployment:");
            println!("1. Copy the published files to the target machine");
            println!("2. No runtime installation required");
            println!("3. Run the executable directly");
        }
    }

    if let PublishTarget::WebAssembly = config.target {
        println!("\nWebAssembly deployment:");
        println!("1. Host the .wasm file on a web server");
        println!("2. Use the FluentAI JavaScript SDK to load and run");
        println!("3. See https://fluentai.dev/docs/wasm for details");
    }
}
