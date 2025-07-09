//! Lock file handling for reproducible builds

use crate::{PackageError, Result, Version};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Lock file structure (claude-lock.json)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Lockfile {
    /// Lock file version
    pub version: u32,

    /// When this lockfile was generated
    pub generated_at: DateTime<Utc>,

    /// Resolved packages
    pub packages: HashMap<String, LockedPackage>,

    /// Integrity information
    #[serde(default)]
    pub integrity: HashMap<String, String>,
}

/// Information about a locked package
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LockedPackage {
    /// Package name
    pub name: String,

    /// Resolved version
    pub version: Version,

    /// Registry URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub registry: Option<String>,

    /// Download URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,

    /// SHA256 checksum
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checksum: Option<String>,

    /// Dependencies with resolved versions
    #[serde(default)]
    pub dependencies: HashMap<String, String>,

    /// Development dependencies
    #[serde(default)]
    pub dev_dependencies: HashMap<String, String>,

    /// Whether this is a direct dependency
    #[serde(default)]
    pub direct: bool,

    /// Whether this is a dev dependency
    #[serde(default)]
    pub dev: bool,
}

impl Lockfile {
    /// Current lockfile version
    pub const CURRENT_VERSION: u32 = 1;

    /// Create a new empty lockfile
    pub fn new() -> Self {
        Self {
            version: Self::CURRENT_VERSION,
            generated_at: Utc::now(),
            packages: HashMap::new(),
            integrity: HashMap::new(),
        }
    }

    /// Load lockfile from disk
    pub fn load(path: &Path) -> Result<Self> {
        let content = fs::read_to_string(path).map_err(|e| PackageError::LockfileError {
            message: format!("Failed to read lockfile: {}", e),
        })?;

        let lockfile: Self =
            serde_json::from_str(&content).map_err(|e| PackageError::LockfileError {
                message: format!("Failed to parse lockfile: {}", e),
            })?;

        lockfile.validate()?;
        Ok(lockfile)
    }

    /// Save lockfile to disk
    pub fn save(&self, path: &Path) -> Result<()> {
        let content = serde_json::to_string_pretty(self)?;
        fs::write(path, content)?;
        Ok(())
    }

    /// Validate lockfile
    fn validate(&self) -> Result<()> {
        if self.version > Self::CURRENT_VERSION {
            return Err(PackageError::LockfileError {
                message: format!(
                    "Lockfile version {} is newer than supported version {}",
                    self.version,
                    Self::CURRENT_VERSION
                ),
            });
        }

        Ok(())
    }

    /// Add a locked package
    pub fn add_package(&mut self, package: LockedPackage) {
        let key = format!("{}@{}", package.name, package.version);
        self.packages.insert(key, package);
        self.generated_at = Utc::now();
    }

    /// Get a locked package by name and version
    pub fn get_package(&self, name: &str, version: &Version) -> Option<&LockedPackage> {
        let key = format!("{}@{}", name, version);
        self.packages.get(&key)
    }

    /// Remove a package
    pub fn remove_package(&mut self, name: &str, version: &Version) -> Option<LockedPackage> {
        let key = format!("{}@{}", name, version);
        let removed = self.packages.remove(&key);
        if removed.is_some() {
            self.generated_at = Utc::now();
        }
        removed
    }

    /// Get all direct dependencies
    pub fn direct_dependencies(&self) -> Vec<&LockedPackage> {
        self.packages
            .values()
            .filter(|p| p.direct && !p.dev)
            .collect()
    }

    /// Get all dev dependencies
    pub fn dev_dependencies(&self) -> Vec<&LockedPackage> {
        self.packages
            .values()
            .filter(|p| p.direct && p.dev)
            .collect()
    }

    /// Update integrity hash for a package
    pub fn set_integrity(&mut self, package_key: &str, hash: String) {
        self.integrity.insert(package_key.to_string(), hash);
    }

    /// Verify integrity of all packages
    pub fn verify_integrity(&self) -> Result<()> {
        for (key, package) in &self.packages {
            if let Some(expected_hash) = &package.checksum {
                if let Some(stored_hash) = self.integrity.get(key) {
                    if expected_hash != stored_hash {
                        return Err(PackageError::ChecksumMismatch {
                            package: key.clone(),
                            expected: expected_hash.clone(),
                            actual: stored_hash.clone(),
                        });
                    }
                }
            }
        }
        Ok(())
    }

    /// Merge another lockfile into this one
    pub fn merge(&mut self, other: &Lockfile) -> Result<()> {
        for (key, package) in &other.packages {
            if !self.packages.contains_key(key) {
                self.packages.insert(key.clone(), package.clone());
            }
        }

        for (key, hash) in &other.integrity {
            if !self.integrity.contains_key(key) {
                self.integrity.insert(key.clone(), hash.clone());
            }
        }

        self.generated_at = Utc::now();
        Ok(())
    }

    /// Check if a package is locked
    pub fn has_package(&self, name: &str, version: &Version) -> bool {
        let key = format!("{}@{}", name, version);
        self.packages.contains_key(&key)
    }

    /// Get all packages sorted by name
    pub fn packages_sorted(&self) -> Vec<&LockedPackage> {
        let mut packages: Vec<_> = self.packages.values().collect();
        packages.sort_by(|a, b| a.name.cmp(&b.name));
        packages
    }
}

impl Default for Lockfile {
    fn default() -> Self {
        Self::new()
    }
}

/// Create a locked package from manifest information
pub fn create_locked_package(
    name: String,
    version: Version,
    registry: Option<String>,
    url: Option<String>,
    checksum: Option<String>,
    dependencies: HashMap<String, String>,
    dev_dependencies: HashMap<String, String>,
    direct: bool,
    dev: bool,
) -> LockedPackage {
    LockedPackage {
        name,
        version,
        registry,
        url,
        checksum,
        dependencies,
        dev_dependencies,
        direct,
        dev,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lockfile_creation() {
        let mut lockfile = Lockfile::new();
        assert_eq!(lockfile.version, Lockfile::CURRENT_VERSION);
        assert!(lockfile.packages.is_empty());

        let package = LockedPackage {
            name: "test".to_string(),
            version: Version::new(1, 0, 0),
            registry: None,
            url: None,
            checksum: Some("abc123".to_string()),
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            direct: true,
            dev: false,
        };

        lockfile.add_package(package);
        assert_eq!(lockfile.packages.len(), 1);
        assert!(lockfile.has_package("test", &Version::new(1, 0, 0)));
    }

    #[test]
    fn test_lockfile_serialization() {
        let lockfile = Lockfile::new();
        let json = serde_json::to_string(&lockfile).unwrap();
        let deserialized: Lockfile = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.version, lockfile.version);
    }

    #[test]
    fn test_direct_dependencies() {
        let mut lockfile = Lockfile::new();

        lockfile.add_package(LockedPackage {
            name: "direct".to_string(),
            version: Version::new(1, 0, 0),
            registry: None,
            url: None,
            checksum: None,
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            direct: true,
            dev: false,
        });

        lockfile.add_package(LockedPackage {
            name: "transitive".to_string(),
            version: Version::new(1, 0, 0),
            registry: None,
            url: None,
            checksum: None,
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            direct: false,
            dev: false,
        });

        let direct = lockfile.direct_dependencies();
        assert_eq!(direct.len(), 1);
        assert_eq!(direct[0].name, "direct");
    }
}
