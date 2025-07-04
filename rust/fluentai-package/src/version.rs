//! Version and version requirement handling

use crate::{PackageError, Result};
use serde::{Deserialize, Serialize};
use std::fmt;

/// Re-export semver types with additional functionality
pub use semver::{Version as SemverVersion, VersionReq as SemverVersionReq};

/// Wrapper around semver::Version with additional methods
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Version(pub SemverVersion);

/// Wrapper around semver::VersionReq with additional methods
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VersionReq(pub SemverVersionReq);

impl Version {
    /// Parse a version string
    pub fn parse(s: &str) -> Result<Self> {
        SemverVersion::parse(s)
            .map(Version)
            .map_err(|_e| PackageError::InvalidVersion {
                version: s.to_string(),
            })
    }
    
    /// Create a new version
    pub fn new(major: u64, minor: u64, patch: u64) -> Self {
        Version(SemverVersion::new(major, minor, patch))
    }
    
    /// Check if this version satisfies a requirement
    pub fn satisfies(&self, req: &VersionReq) -> bool {
        req.0.matches(&self.0)
    }
    
    /// Get the major version
    pub fn major(&self) -> u64 {
        self.0.major
    }
    
    /// Get the minor version
    pub fn minor(&self) -> u64 {
        self.0.minor
    }
    
    /// Get the patch version
    pub fn patch(&self) -> u64 {
        self.0.patch
    }
    
    /// Check if this is a pre-release version
    pub fn is_prerelease(&self) -> bool {
        !self.0.pre.is_empty()
    }
}

impl VersionReq {
    /// Parse a version requirement string
    pub fn parse(s: &str) -> Result<Self> {
        // Handle common npm-style version requirements
        let normalized = normalize_version_req(s);
        
        SemverVersionReq::parse(&normalized)
            .map(VersionReq)
            .map_err(|_| PackageError::InvalidVersion {
                version: s.to_string(),
            })
    }
    
    /// Create a version requirement that matches any version
    pub fn any() -> Self {
        VersionReq(SemverVersionReq::STAR)
    }
    
    /// Check if a version satisfies this requirement
    pub fn matches(&self, version: &Version) -> bool {
        self.0.matches(&version.0)
    }
    
    /// Get the minimal version that satisfies this requirement
    pub fn minimal_version(&self) -> Option<Version> {
        // This is a simplified implementation
        // In practice, this would need to parse the requirement
        // and determine the minimal satisfying version
        if self.0 == SemverVersionReq::STAR {
            Some(Version::new(0, 0, 0))
        } else {
            // Parse the requirement string to extract a base version
            None
        }
    }
}

/// Normalize npm-style version requirements to semver format
fn normalize_version_req(s: &str) -> String {
    let trimmed = s.trim();
    
    // Handle npm-style ranges first
    if trimmed.contains(" - ") {
        let parts: Vec<&str> = trimmed.split(" - ").collect();
        if parts.len() == 2 {
            return format!(">={}, <={}", parts[0], parts[1]);
        }
    }
    
    // Handle exact versions (no operator)
    if trimmed.chars().next().map(|c| c.is_numeric()).unwrap_or(false) {
        return format!("={}", trimmed);
    }
    
    // Handle caret and tilde (already supported by semver)
    if trimmed.starts_with('^') || trimmed.starts_with('~') {
        return trimmed.to_string();
    }
    
    // Handle 'latest' tag
    if trimmed == "latest" || trimmed == "*" {
        return "*".to_string();
    }
    
    trimmed.to_string()
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for VersionReq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Serialize for Version {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.to_string().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Version {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Version::parse(&s).map_err(serde::de::Error::custom)
    }
}

impl Serialize for VersionReq {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.to_string().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for VersionReq {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        VersionReq::parse(&s).map_err(serde::de::Error::custom)
    }
}

impl From<SemverVersion> for Version {
    fn from(v: SemverVersion) -> Self {
        Version(v)
    }
}

impl From<Version> for SemverVersion {
    fn from(v: Version) -> Self {
        v.0
    }
}

/// Compare two versions to determine which is newer
pub fn compare_versions(a: &Version, b: &Version) -> std::cmp::Ordering {
    a.0.cmp(&b.0)
}

/// Find the highest version from a list
pub fn find_highest_version<'a>(versions: &'a [Version]) -> Option<&'a Version> {
    versions.iter().max()
}

/// Filter versions that satisfy a requirement
pub fn filter_matching_versions<'a>(
    versions: &'a [Version],
    req: &VersionReq,
) -> Vec<&'a Version> {
    versions
        .iter()
        .filter(|v| req.matches(v))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_version_parsing() {
        let v = Version::parse("1.2.3").unwrap();
        assert_eq!(v.major(), 1);
        assert_eq!(v.minor(), 2);
        assert_eq!(v.patch(), 3);
    }
    
    #[test]
    fn test_version_req_parsing() {
        let req = VersionReq::parse("^1.2.3").unwrap();
        assert!(req.matches(&Version::parse("1.2.4").unwrap()));
        assert!(req.matches(&Version::parse("1.3.0").unwrap()));
        assert!(!req.matches(&Version::parse("2.0.0").unwrap()));
    }
    
    #[test]
    fn test_normalize_version_req() {
        assert_eq!(normalize_version_req("1.2.3"), "=1.2.3");
        assert_eq!(normalize_version_req("^1.2.3"), "^1.2.3");
        assert_eq!(normalize_version_req("latest"), "*");
        assert_eq!(normalize_version_req("1.0.0 - 2.0.0"), ">=1.0.0, <=2.0.0");
    }
    
    #[test]
    fn test_version_comparison() {
        let v1 = Version::parse("1.2.3").unwrap();
        let v2 = Version::parse("1.2.4").unwrap();
        let v3 = Version::parse("2.0.0").unwrap();
        
        assert!(v1 < v2);
        assert!(v2 < v3);
        
        let versions = vec![v3.clone(), v1.clone(), v2.clone()];
        let highest = find_highest_version(&versions).unwrap();
        assert_eq!(highest, &v3);
    }
}