//! FluentAi Package Manager
//! 
//! This crate provides package management functionality for FluentAi, including:
//! - Package manifest (fluentai.json) handling
//! - Dependency resolution with semantic versioning
//! - Package registry operations (local and remote)
//! - Package installation and management
//! - CLI tools for package operations

pub mod error;
pub mod manifest;
pub mod version;
pub mod resolver;
pub mod registry;
pub mod installer;
pub mod config;
pub mod lockfile;

pub use error::{PackageError, Result};
pub use manifest::{Manifest, Dependency, Script};
pub use version::{Version, VersionReq};
pub use resolver::DependencyResolver;
pub use registry::{Registry, Package};
pub use installer::PackageInstaller;
pub use config::PackageConfig;
pub use lockfile::Lockfile;