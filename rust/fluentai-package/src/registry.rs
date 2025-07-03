//! Package registry operations

use crate::{PackageError, Result, Version, VersionReq, Manifest};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use sha2::{Sha256, Digest};
use std::io::Read;

/// Package metadata in registry
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Package {
    /// Package name
    pub name: String,
    
    /// Package description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    
    /// Available versions
    pub versions: Vec<PackageVersion>,
    
    /// Latest stable version
    #[serde(skip_serializing_if = "Option::is_none")]
    pub latest: Option<Version>,
    
    /// Package keywords
    #[serde(default)]
    pub keywords: Vec<String>,
    
    /// Package author
    #[serde(skip_serializing_if = "Option::is_none")]
    pub author: Option<String>,
    
    /// License
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
    
    /// Repository URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
    
    /// Total downloads
    #[serde(default)]
    pub downloads: u64,
}

/// Version-specific package information
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PackageVersion {
    /// Version number
    pub version: Version,
    
    /// Tarball URL
    pub tarball: String,
    
    /// SHA256 checksum
    pub checksum: String,
    
    /// Dependencies
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
    
    /// Dev dependencies
    #[serde(default)]
    pub dev_dependencies: HashMap<String, String>,
    
    /// Published date
    pub published_at: chrono::DateTime<chrono::Utc>,
    
    /// Package size in bytes
    pub size: u64,
    
    /// Manifest content
    #[serde(skip_serializing_if = "Option::is_none")]
    pub manifest: Option<Manifest>,
}

/// Registry trait for different registry implementations
pub trait Registry: Send + Sync {
    /// Get package metadata
    fn get_package(&self, name: &str) -> Result<Package>;
    
    /// Get specific version of a package
    fn get_package_version(&self, name: &str, version: &Version) -> Result<PackageVersion>;
    
    /// Search for packages
    fn search(&self, query: &str, limit: usize) -> Result<Vec<Package>>;
    
    /// Download a package tarball
    fn download(&self, name: &str, version: &Version, dest: &Path) -> Result<PathBuf>;
    
    /// Publish a package
    fn publish(&self, manifest: &Manifest, tarball: &Path, token: &str) -> Result<()>;
    
    /// Get all versions matching a requirement
    fn get_matching_versions(&self, name: &str, req: &VersionReq) -> Result<Vec<Version>>;
}

/// Local file-based registry
pub struct LocalRegistry {
    path: PathBuf,
}

impl LocalRegistry {
    /// Create a new local registry
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
    
    /// Get the path for a package
    fn package_path(&self, name: &str) -> PathBuf {
        self.path.join(name)
    }
    
    /// Get the metadata file path for a package
    fn metadata_path(&self, name: &str) -> PathBuf {
        self.package_path(name).join("metadata.json")
    }
    
    /// Get the tarball path for a specific version
    fn tarball_path(&self, name: &str, version: &Version) -> PathBuf {
        self.package_path(name)
            .join(format!("{}-{}.tar.gz", name, version))
    }
}

impl Registry for LocalRegistry {
    fn get_package(&self, name: &str) -> Result<Package> {
        let metadata_path = self.metadata_path(name);
        if !metadata_path.exists() {
            return Err(PackageError::PackageNotFound {
                name: name.to_string(),
                version: "*".to_string(),
            });
        }
        
        let content = std::fs::read_to_string(&metadata_path)?;
        let package: Package = serde_json::from_str(&content)?;
        Ok(package)
    }
    
    fn get_package_version(&self, name: &str, version: &Version) -> Result<PackageVersion> {
        let package = self.get_package(name)?;
        
        package.versions
            .into_iter()
            .find(|v| &v.version == version)
            .ok_or_else(|| PackageError::PackageNotFound {
                name: name.to_string(),
                version: version.to_string(),
            })
    }
    
    fn search(&self, query: &str, limit: usize) -> Result<Vec<Package>> {
        let mut results = Vec::new();
        
        // Simple search implementation
        if let Ok(entries) = std::fs::read_dir(&self.path) {
            for entry in entries.flatten() {
                if entry.file_type()?.is_dir() {
                    if let Some(name) = entry.file_name().to_str() {
                        if name.contains(query) {
                            if let Ok(package) = self.get_package(name) {
                                results.push(package);
                                if results.len() >= limit {
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
        
        Ok(results)
    }
    
    fn download(&self, name: &str, version: &Version, dest: &Path) -> Result<PathBuf> {
        let tarball_path = self.tarball_path(name, version);
        if !tarball_path.exists() {
            return Err(PackageError::PackageNotFound {
                name: name.to_string(),
                version: version.to_string(),
            });
        }
        
        let dest_path = dest.join(format!("{}-{}.tar.gz", name, version));
        std::fs::copy(&tarball_path, &dest_path)?;
        
        Ok(dest_path)
    }
    
    fn publish(&self, manifest: &Manifest, tarball: &Path, _token: &str) -> Result<()> {
        let package_dir = self.package_path(&manifest.name);
        std::fs::create_dir_all(&package_dir)?;
        
        // Copy tarball
        let tarball_dest = self.tarball_path(&manifest.name, &Version::parse(&manifest.version)?);
        std::fs::copy(tarball, &tarball_dest)?;
        
        // Calculate checksum
        let mut file = std::fs::File::open(tarball)?;
        let mut hasher = Sha256::new();
        let mut buffer = [0; 8192];
        loop {
            let n = file.read(&mut buffer)?;
            if n == 0 {
                break;
            }
            hasher.update(&buffer[..n]);
        }
        let checksum = hex::encode(hasher.finalize());
        
        // Update or create metadata
        let mut package = if let Ok(existing) = self.get_package(&manifest.name) {
            existing
        } else {
            Package {
                name: manifest.name.clone(),
                description: manifest.description.clone(),
                versions: Vec::new(),
                latest: None,
                keywords: manifest.keywords.clone(),
                author: manifest.author.clone(),
                license: manifest.license.clone(),
                repository: manifest.repository.clone(),
                downloads: 0,
            }
        };
        
        // Add new version
        let version = Version::parse(&manifest.version)?;
        let package_version = PackageVersion {
            version: version.clone(),
            tarball: tarball_dest.to_string_lossy().to_string(),
            checksum,
            dependencies: manifest.dependencies
                .iter()
                .map(|(k, v)| {
                    let version_str = match v {
                        crate::manifest::Dependency::Version(v) => v.clone(),
                        crate::manifest::Dependency::Detailed { version, .. } => version.clone(),
                    };
                    (k.clone(), version_str)
                })
                .collect(),
            dev_dependencies: manifest.dev_dependencies
                .iter()
                .map(|(k, v)| {
                    let version_str = match v {
                        crate::manifest::Dependency::Version(v) => v.clone(),
                        crate::manifest::Dependency::Detailed { version, .. } => version.clone(),
                    };
                    (k.clone(), version_str)
                })
                .collect(),
            published_at: chrono::Utc::now(),
            size: std::fs::metadata(tarball)?.len(),
            manifest: Some(manifest.clone()),
        };
        
        // Remove old version if exists
        package.versions.retain(|v| v.version != version);
        package.versions.push(package_version);
        
        // Update latest version
        package.latest = package.versions
            .iter()
            .map(|v| &v.version)
            .max()
            .cloned();
        
        // Save metadata
        let metadata_content = serde_json::to_string_pretty(&package)?;
        std::fs::write(self.metadata_path(&manifest.name), metadata_content)?;
        
        Ok(())
    }
    
    fn get_matching_versions(&self, name: &str, req: &VersionReq) -> Result<Vec<Version>> {
        let package = self.get_package(name)?;
        
        let mut matching: Vec<_> = package.versions
            .into_iter()
            .map(|v| v.version)
            .filter(|v| req.matches(v))
            .collect();
        
        matching.sort();
        matching.reverse(); // Latest first
        
        Ok(matching)
    }
}

/// HTTP-based registry client
pub struct HttpRegistry {
    base_url: String,
    client: reqwest::blocking::Client,
}

impl HttpRegistry {
    /// Create a new HTTP registry client
    pub fn new(base_url: String) -> Self {
        let client = reqwest::blocking::Client::builder()
            .timeout(std::time::Duration::from_secs(300))
            .build()
            .unwrap();
        
        Self { base_url, client }
    }
    
    /// Make an authenticated request
    fn request(&self, url: &str, token: Option<&str>) -> reqwest::blocking::RequestBuilder {
        let mut req = self.client.get(url);
        if let Some(token) = token {
            req = req.header("Authorization", format!("Bearer {}", token));
        }
        req
    }
}

impl Registry for HttpRegistry {
    fn get_package(&self, name: &str) -> Result<Package> {
        let url = format!("{}/packages/{}", self.base_url, name);
        let response = self.request(&url, None).send()?;
        
        if response.status() == 404 {
            return Err(PackageError::PackageNotFound {
                name: name.to_string(),
                version: "*".to_string(),
            });
        }
        
        let package: Package = response.json()?;
        Ok(package)
    }
    
    fn get_package_version(&self, name: &str, version: &Version) -> Result<PackageVersion> {
        let url = format!("{}/packages/{}/{}", self.base_url, name, version);
        let response = self.request(&url, None).send()?;
        
        if response.status() == 404 {
            return Err(PackageError::PackageNotFound {
                name: name.to_string(),
                version: version.to_string(),
            });
        }
        
        let version: PackageVersion = response.json()?;
        Ok(version)
    }
    
    fn search(&self, query: &str, limit: usize) -> Result<Vec<Package>> {
        let url = format!("{}/search?q={}&limit={}", self.base_url, query, limit);
        let response = self.request(&url, None).send()?;
        let results: Vec<Package> = response.json()?;
        Ok(results)
    }
    
    fn download(&self, name: &str, version: &Version, dest: &Path) -> Result<PathBuf> {
        let package_version = self.get_package_version(name, version)?;
        let dest_path = dest.join(format!("{}-{}.tar.gz", name, version));
        
        let mut response = self.client.get(&package_version.tarball).send()?;
        let mut file = std::fs::File::create(&dest_path)?;
        std::io::copy(&mut response, &mut file)?;
        
        // Verify checksum
        let mut file = std::fs::File::open(&dest_path)?;
        let mut hasher = Sha256::new();
        let mut buffer = [0; 8192];
        loop {
            let n = file.read(&mut buffer)?;
            if n == 0 {
                break;
            }
            hasher.update(&buffer[..n]);
        }
        let actual_checksum = hex::encode(hasher.finalize());
        
        if actual_checksum != package_version.checksum {
            std::fs::remove_file(&dest_path)?;
            return Err(PackageError::ChecksumMismatch {
                package: format!("{}@{}", name, version),
                expected: package_version.checksum,
                actual: actual_checksum,
            });
        }
        
        Ok(dest_path)
    }
    
    fn publish(&self, manifest: &Manifest, tarball: &Path, token: &str) -> Result<()> {
        let url = format!("{}/packages", self.base_url);
        
        let form = reqwest::blocking::multipart::Form::new()
            .text("manifest", serde_json::to_string(manifest)?)
            .file("tarball", tarball)?;
        
        let response = self.client
            .post(&url)
            .header("Authorization", format!("Bearer {}", token))
            .multipart(form)
            .send()?;
        
        if !response.status().is_success() {
            let error_text = response.text()?;
            return Err(PackageError::RegistryError {
                message: format!("Failed to publish package: {}", error_text),
            });
        }
        
        Ok(())
    }
    
    fn get_matching_versions(&self, name: &str, req: &VersionReq) -> Result<Vec<Version>> {
        let package = self.get_package(name)?;
        
        let mut matching: Vec<_> = package.versions
            .into_iter()
            .map(|v| v.version)
            .filter(|v| req.matches(v))
            .collect();
        
        matching.sort();
        matching.reverse(); // Latest first
        
        Ok(matching)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    
    #[test]
    fn test_local_registry() {
        let temp_dir = TempDir::new().unwrap();
        let registry = LocalRegistry::new(temp_dir.path().to_path_buf());
        
        // Test empty registry
        assert!(registry.get_package("nonexistent").is_err());
        
        // Test search in empty registry
        let results = registry.search("test", 10).unwrap();
        assert!(results.is_empty());
    }
}