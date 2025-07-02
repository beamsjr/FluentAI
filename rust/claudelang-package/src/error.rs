//! Package manager error types

use thiserror::Error;
use std::path::PathBuf;

pub type Result<T> = std::result::Result<T, PackageError>;

#[derive(Error, Debug)]
pub enum PackageError {
    #[error("Package not found: {name}@{version}")]
    PackageNotFound { name: String, version: String },
    
    #[error("Version conflict: {package} requires {required}, but {found} is already installed")]
    VersionConflict {
        package: String,
        required: String,
        found: String,
    },
    
    #[error("Circular dependency detected: {cycle}")]
    CircularDependency { cycle: String },
    
    #[error("Invalid manifest at {path}: {message}")]
    InvalidManifest { path: PathBuf, message: String },
    
    #[error("Invalid version string: {version}")]
    InvalidVersion { version: String },
    
    #[error("Registry error: {message}")]
    RegistryError { message: String },
    
    #[error("Network error: {message}")]
    NetworkError { message: String },
    
    #[error("Installation failed for {package}: {message}")]
    InstallationFailed { package: String, message: String },
    
    #[error("Package {name} is already installed")]
    AlreadyInstalled { name: String },
    
    #[error("No compatible version found for {package} with constraint {constraint}")]
    NoCompatibleVersion { package: String, constraint: String },
    
    #[error("Checksum mismatch for {package}: expected {expected}, got {actual}")]
    ChecksumMismatch {
        package: String,
        expected: String,
        actual: String,
    },
    
    #[error("Lock file error: {message}")]
    LockfileError { message: String },
    
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
    
    #[error("TOML parse error: {0}")]
    TomlParse(#[from] toml::de::Error),
    
    #[error("TOML serialize error: {0}")]
    TomlSerialize(#[from] toml::ser::Error),
    
    #[error("Semver error: {0}")]
    Semver(#[from] semver::Error),
    
    #[error("HTTP error: {0}")]
    Http(#[from] reqwest::Error),
    
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}