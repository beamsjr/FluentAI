//! Linking support for native executables

use anyhow::{anyhow, Result};
use std::path::{Path, PathBuf};
use std::process::Command;
use target_lexicon::OperatingSystem;

use crate::target::{CompilationTarget, OutputFormat};

/// Linker configuration
pub struct LinkerConfig {
    /// Target specification
    pub target: CompilationTarget,
    
    /// Output format
    pub output_format: OutputFormat,
    
    /// Libraries to link
    pub libraries: Vec<String>,
    
    /// Library search paths
    pub library_paths: Vec<PathBuf>,
    
    /// Additional linker flags
    pub flags: Vec<String>,
    
    /// Runtime library path
    pub runtime_lib: Option<PathBuf>,
}

impl LinkerConfig {
    /// Create default linker configuration
    pub fn new(target: CompilationTarget, output_format: OutputFormat) -> Self {
        Self {
            target,
            output_format,
            libraries: vec![],
            library_paths: vec![],
            flags: vec![],
            runtime_lib: None,
        }
    }
    
    /// Add the FLC runtime library
    pub fn with_runtime(&mut self, runtime_path: PathBuf) -> &mut Self {
        self.runtime_lib = Some(runtime_path);
        self
    }
    
    /// Add a library to link
    pub fn link_library(&mut self, lib: String) -> &mut Self {
        self.libraries.push(lib);
        self
    }
    
    /// Add a library search path
    pub fn add_library_path(&mut self, path: PathBuf) -> &mut Self {
        self.library_paths.push(path);
        self
    }
}

/// Link object files into an executable or library
pub fn link(
    object_files: &[PathBuf],
    output_path: &Path,
    config: &LinkerConfig,
) -> Result<()> {
    let linker = get_linker(&config.target)?;
    
    let mut cmd = Command::new(linker);
    
    // Add object files
    for obj in object_files {
        cmd.arg(obj);
    }
    
    // Output file
    cmd.arg("-o").arg(output_path);
    
    // Add runtime library if specified
    if let Some(runtime) = &config.runtime_lib {
        cmd.arg(runtime);
    }
    
    // Add library search paths
    for path in &config.library_paths {
        cmd.arg(format!("-L{}", path.display()));
    }
    
    // Add libraries
    for lib in &config.libraries {
        cmd.arg(format!("-l{}", lib));
    }
    
    // Platform-specific flags
    match config.target.triple.operating_system {
        OperatingSystem::Darwin => {
            // macOS specific flags
            if config.output_format == OutputFormat::DynamicLib {
                cmd.arg("-dynamiclib");
            }
        }
        OperatingSystem::Linux => {
            // Linux specific flags
            if config.output_format == OutputFormat::DynamicLib {
                cmd.arg("-shared");
            }
            if config.target.pic {
                cmd.arg("-fPIC");
            }
        }
        OperatingSystem::Windows => {
            // Windows specific flags
            if config.output_format == OutputFormat::DynamicLib {
                cmd.arg("/DLL");
            }
        }
        _ => {}
    }
    
    // Add custom flags
    for flag in &config.flags {
        cmd.arg(flag);
    }
    
    // Standard libraries
    match config.target.triple.operating_system {
        OperatingSystem::Windows => {
            // Link Windows CRT
            cmd.arg("msvcrt.lib");
        }
        _ => {
            // Link standard C library
            cmd.arg("-lc");
            // Link math library
            cmd.arg("-lm");
            // Link pthread for threading
            cmd.arg("-lpthread");
        }
    }
    
    // Execute linker
    let output = cmd.output()?;
    
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!("Linking failed: {}", stderr));
    }
    
    Ok(())
}

/// Get the appropriate linker for the target
fn get_linker(target: &CompilationTarget) -> Result<String> {
    // Check if a specific linker is set in environment
    if let Ok(linker) = std::env::var("FLC_LINKER") {
        return Ok(linker);
    }
    
    // Use platform defaults
    match target.triple.operating_system {
        OperatingSystem::Darwin => Ok("ld".to_string()),
        OperatingSystem::Linux => Ok("ld".to_string()),
        OperatingSystem::Windows => Ok("link.exe".to_string()),
        _ => {
            // Try to find a suitable linker
            if which::which("ld").is_ok() {
                Ok("ld".to_string())
            } else if which::which("lld").is_ok() {
                Ok("lld".to_string())
            } else if which::which("gold").is_ok() {
                Ok("gold".to_string())
            } else {
                Err(anyhow!("No suitable linker found for target"))
            }
        }
    }
}

/// Build the FLC runtime library
pub fn build_runtime_library(output_path: &Path) -> Result<()> {
    // TODO: Implement runtime library building
    // This would compile the runtime support functions
    // (memory management, string operations, etc.)
    // into a static library that can be linked with compiled programs
    
    Err(anyhow!("Runtime library building not yet implemented"))
}