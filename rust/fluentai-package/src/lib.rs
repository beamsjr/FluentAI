//! FluentAi Package Manager
//!
//! This crate provides package management functionality for FluentAi, including:
//! - Package manifest (fluentai.json) handling
//! - Dependency resolution with semantic versioning
//! - Package registry operations (local and remote)
//! - Package installation and management
//! - CLI tools for package operations

pub mod config;
pub mod error;
pub mod installer;
pub mod lockfile;
pub mod manifest;
pub mod registry;
pub mod resolver;
pub mod version;

pub use config::PackageConfig;
pub use error::{PackageError, Result};
pub use installer::PackageInstaller;
pub use lockfile::Lockfile;
pub use manifest::{Dependency, Manifest, Script};
pub use registry::{Package, Registry};
pub use resolver::DependencyResolver;
pub use version::{Version, VersionReq};
