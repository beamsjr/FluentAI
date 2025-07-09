//! Migration versioning schemes

use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

/// Version scheme for migrations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum VersionScheme {
    /// Sequential numeric versions (001, 002, etc.)
    Sequential,
    /// Timestamp-based versions (20240101120000)
    Timestamp,
    /// Semantic versioning (1.0.0)
    Semantic,
}

/// Migration version
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Version {
    value: String,
    scheme: VersionScheme,
}

impl Version {
    /// Create a new version
    pub fn new(value: impl Into<String>, scheme: VersionScheme) -> Self {
        Self {
            value: value.into(),
            scheme,
        }
    }

    /// Parse a version string with auto-detection
    pub fn parse(value: impl Into<String>) -> Self {
        let value = value.into();
        let scheme = Self::detect_scheme(&value);
        Self { value, scheme }
    }

    /// Get the version value
    pub fn value(&self) -> &str {
        &self.value
    }

    /// Get the version scheme
    pub fn scheme(&self) -> VersionScheme {
        self.scheme
    }

    /// Detect version scheme from string
    fn detect_scheme(value: &str) -> VersionScheme {
        // Check if it's a timestamp (14 digits)
        if value.len() == 14 && value.chars().all(|c| c.is_ascii_digit()) {
            return VersionScheme::Timestamp;
        }

        // Check if it's semantic versioning (contains dots)
        if value.contains('.')
            && value.split('.').all(|part| {
                part.parse::<u32>().is_ok() || (part.contains('-') || part.contains('+'))
            })
        {
            return VersionScheme::Semantic;
        }

        // Default to sequential
        VersionScheme::Sequential
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.scheme, other.scheme) {
            (VersionScheme::Sequential, VersionScheme::Sequential) => {
                // Compare as numbers if possible
                match (self.value.parse::<u64>(), other.value.parse::<u64>()) {
                    (Ok(a), Ok(b)) => a.cmp(&b),
                    _ => self.value.cmp(&other.value),
                }
            }
            (VersionScheme::Timestamp, VersionScheme::Timestamp) => {
                // Timestamps are already sortable as strings
                self.value.cmp(&other.value)
            }
            (VersionScheme::Semantic, VersionScheme::Semantic) => {
                // Parse semantic versions
                let self_parts: Vec<_> = self.value.split('.').collect();
                let other_parts: Vec<_> = other.value.split('.').collect();

                for i in 0..self_parts.len().min(other_parts.len()) {
                    let self_num = self_parts[i]
                        .split('-')
                        .next()
                        .and_then(|s| s.parse::<u32>().ok())
                        .unwrap_or(0);
                    let other_num = other_parts[i]
                        .split('-')
                        .next()
                        .and_then(|s| s.parse::<u32>().ok())
                        .unwrap_or(0);

                    match self_num.cmp(&other_num) {
                        Ordering::Equal => continue,
                        ord => return ord,
                    }
                }

                self_parts.len().cmp(&other_parts.len())
            }
            _ => {
                // Different schemes, compare as strings
                self.value.cmp(&other.value)
            }
        }
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// Version generator for creating new versions
pub struct VersionGenerator {
    scheme: VersionScheme,
    prefix: Option<String>,
}

impl VersionGenerator {
    /// Create a new version generator
    pub fn new(scheme: VersionScheme) -> Self {
        Self {
            scheme,
            prefix: None,
        }
    }

    /// Set a prefix for generated versions
    pub fn with_prefix(mut self, prefix: impl Into<String>) -> Self {
        self.prefix = Some(prefix.into());
        self
    }

    /// Generate the next version
    pub fn next(&self, current: Option<&Version>) -> Version {
        let value = match self.scheme {
            VersionScheme::Sequential => {
                let next_num = if let Some(current) = current {
                    current.value.parse::<u64>().unwrap_or(0) + 1
                } else {
                    1
                };
                format!("{:03}", next_num)
            }
            VersionScheme::Timestamp => {
                use chrono::Utc;
                Utc::now().format("%Y%m%d%H%M%S").to_string()
            }
            VersionScheme::Semantic => {
                if let Some(current) = current {
                    self.increment_semantic(&current.value)
                } else {
                    "0.1.0".to_string()
                }
            }
        };

        let final_value = if let Some(prefix) = &self.prefix {
            format!("{}_{}", prefix, value)
        } else {
            value
        };

        Version::new(final_value, self.scheme)
    }

    /// Increment semantic version (patch level)
    fn increment_semantic(&self, version: &str) -> String {
        let parts: Vec<_> = version.split('.').collect();
        if parts.len() >= 3 {
            if let Ok(patch) = parts[2].parse::<u32>() {
                return format!("{}.{}.{}", parts[0], parts[1], patch + 1);
            }
        }
        "0.1.0".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_detection() {
        let v1 = Version::parse("001");
        assert_eq!(v1.scheme(), VersionScheme::Sequential);

        let v2 = Version::parse("20240101120000");
        assert_eq!(v2.scheme(), VersionScheme::Timestamp);

        let v3 = Version::parse("1.2.3");
        assert_eq!(v3.scheme(), VersionScheme::Semantic);
    }

    #[test]
    fn test_version_ordering() {
        // Sequential
        let v1 = Version::new("001", VersionScheme::Sequential);
        let v2 = Version::new("002", VersionScheme::Sequential);
        assert!(v1 < v2);

        // Timestamp
        let v3 = Version::new("20240101120000", VersionScheme::Timestamp);
        let v4 = Version::new("20240102120000", VersionScheme::Timestamp);
        assert!(v3 < v4);

        // Semantic
        let v5 = Version::new("1.0.0", VersionScheme::Semantic);
        let v6 = Version::new("1.0.1", VersionScheme::Semantic);
        let v7 = Version::new("1.1.0", VersionScheme::Semantic);
        assert!(v5 < v6);
        assert!(v6 < v7);
    }

    #[test]
    fn test_version_generator() {
        // Sequential
        let gen = VersionGenerator::new(VersionScheme::Sequential);
        let v1 = gen.next(None);
        assert_eq!(v1.value(), "001");

        let v2 = gen.next(Some(&v1));
        assert_eq!(v2.value(), "002");

        // With prefix
        let gen_prefixed = VersionGenerator::new(VersionScheme::Sequential).with_prefix("V");
        let v3 = gen_prefixed.next(None);
        assert!(v3.value().starts_with("V_"));
    }
}
