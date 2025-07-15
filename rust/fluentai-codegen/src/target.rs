//! Target configuration and output formats

use std::str::FromStr;
use target_lexicon::{Architecture, OperatingSystem, Triple};

/// Compilation target specification
#[derive(Debug, Clone)]
pub struct CompilationTarget {
    /// Target triple
    pub triple: Triple,
    
    /// CPU features to enable
    pub cpu_features: Vec<String>,
    
    /// Whether to use position-independent code
    pub pic: bool,
}

impl CompilationTarget {
    /// Create a target for the host system
    pub fn host() -> Self {
        Self {
            triple: target_lexicon::HOST,
            cpu_features: vec![],
            pic: true,
        }
    }
    
    /// Create a target from a triple string
    pub fn from_triple(triple: &str) -> Result<Self, String> {
        let triple = Triple::from_str(triple)
            .map_err(|e| format!("Invalid target triple: {}", e))?;
        
        // Determine default PIC setting based on OS
        let pic = match triple.operating_system {
            OperatingSystem::Linux | OperatingSystem::Darwin => true,
            _ => false,
        };
        
        Ok(Self {
            triple,
            cpu_features: vec![],
            pic,
        })
    }
    
    /// Check if this is a 64-bit target
    pub fn is_64bit(&self) -> bool {
        matches!(
            self.triple.architecture,
            Architecture::X86_64 
            | Architecture::Aarch64(_)
            | Architecture::Riscv64(_)
        )
    }
    
    /// Get pointer width in bytes
    pub fn pointer_width(&self) -> usize {
        if self.is_64bit() { 8 } else { 4 }
    }
}

/// Output format for compilation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Standalone executable
    Executable,
    
    /// Static library (.a)
    StaticLib,
    
    /// Dynamic library (.so, .dylib, .dll)
    DynamicLib,
    
    /// Object file (.o)
    ObjectFile,
    
    /// LLVM bitcode
    Bitcode,
    
    /// Assembly text
    Assembly,
}

impl OutputFormat {
    /// Get file extension for this format on the given OS
    pub fn extension(&self, os: OperatingSystem) -> &'static str {
        match self {
            OutputFormat::Executable => {
                if os == OperatingSystem::Windows { ".exe" } else { "" }
            }
            OutputFormat::StaticLib => {
                if os == OperatingSystem::Windows { ".lib" } else { ".a" }
            }
            OutputFormat::DynamicLib => match os {
                OperatingSystem::Windows => ".dll",
                OperatingSystem::Darwin => ".dylib",
                _ => ".so",
            },
            OutputFormat::ObjectFile => {
                if os == OperatingSystem::Windows { ".obj" } else { ".o" }
            }
            OutputFormat::Bitcode => ".bc",
            OutputFormat::Assembly => ".s",
        }
    }
    
    /// Check if this format requires linking
    pub fn requires_linking(&self) -> bool {
        matches!(self, OutputFormat::Executable | OutputFormat::DynamicLib)
    }
}