//! Package installation and management

use crate::{
    PackageError, Result, Registry, DependencyResolver,
    Manifest, Lockfile, PackageConfig, Version,
    resolver::ResolvedDependency,
    lockfile::LockedPackage,
};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{debug, info};

/// Package installer
pub struct PackageInstaller {
    /// Package configuration
    config: PackageConfig,
    
    /// Registry to use
    registry: Arc<dyn Registry>,
    
    /// Current working directory
    cwd: PathBuf,
}

impl PackageInstaller {
    /// Create a new package installer
    pub fn new(config: PackageConfig, registry: Arc<dyn Registry>, cwd: PathBuf) -> Self {
        Self {
            config,
            registry,
            cwd,
        }
    }
    
    /// Install dependencies from manifest
    pub fn install(&self, manifest: &Manifest, dev: bool) -> Result<()> {
        info!("Installing dependencies for {}", manifest.name);
        
        // Ensure directories exist
        self.config.ensure_directories()?;
        
        // Resolve dependencies
        let mut resolver = DependencyResolver::new(Arc::clone(&self.registry));
        let resolved = resolver.resolve(manifest, dev)?;
        
        info!("Resolved {} dependencies", resolved.len());
        
        // Load existing lockfile if present
        let lockfile_path = self.cwd.join("claude-lock.json");
        let mut lockfile = if lockfile_path.exists() {
            Lockfile::load(&lockfile_path)?
        } else {
            Lockfile::new()
        };
        
        // Install each dependency
        for dep in &resolved {
            self.install_package(dep, &mut lockfile)?;
        }
        
        // Save updated lockfile
        lockfile.save(&lockfile_path)?;
        
        info!("Installation complete");
        Ok(())
    }
    
    /// Install a single package
    fn install_package(&self, dep: &ResolvedDependency, lockfile: &mut Lockfile) -> Result<()> {
        let install_path = self.get_install_path(&dep.name, &dep.version);
        
        // Check if already installed
        if install_path.exists() {
            debug!("Package {}@{} already installed", dep.name, dep.version);
            return Ok(());
        }
        
        info!("Installing {}@{}", dep.name, dep.version);
        
        // Download package
        let tarball_path = self.registry.download(
            &dep.name,
            &dep.version,
            &self.config.temp_dir,
        )?;
        
        // Extract package
        self.extract_package(&tarball_path, &install_path)?;
        
        // Clean up tarball
        fs::remove_file(&tarball_path).ok();
        
        // Update lockfile
        let locked = LockedPackage {
            name: dep.name.clone(),
            version: dep.version.clone(),
            registry: dep.registry.clone(),
            url: None, // TODO: Get from registry
            checksum: None, // TODO: Calculate
            dependencies: dep.dependencies
                .iter()
                .map(|name| {
                    // Find version in resolved dependencies
                    (name.clone(), "*".to_string()) // TODO: Get actual version
                })
                .collect(),
            dev_dependencies: Default::default(),
            direct: dep.direct,
            dev: dep.dev,
        };
        
        lockfile.add_package(locked);
        
        Ok(())
    }
    
    /// Get installation path for a package
    fn get_install_path(&self, name: &str, version: &Version) -> PathBuf {
        self.cwd
            .join("fluentai_modules")
            .join(format!("{}@{}", name, version))
    }
    
    /// Extract a package tarball
    fn extract_package(&self, tarball: &Path, dest: &Path) -> Result<()> {
        // Create destination directory
        fs::create_dir_all(dest)?;
        
        // Open tarball
        let file = fs::File::open(tarball)?;
        let decoder = flate2::read::GzDecoder::new(file);
        let mut archive = tar::Archive::new(decoder);
        
        // Extract files
        archive.unpack(dest)?;
        
        Ok(())
    }
    
    /// Add a dependency to the manifest
    pub fn add(&self, manifest: &mut Manifest, name: &str, version: Option<&str>, dev: bool) -> Result<()> {
        // Determine version requirement
        let version_req = if let Some(v) = version {
            v.to_string()
        } else {
            // Get latest version from registry
            let package = self.registry.get_package(name)?;
            if let Some(latest) = package.latest {
                format!("^{}", latest)
            } else {
                "*".to_string()
            }
        };
        
        // Add to manifest
        let dep = crate::manifest::Dependency::Version(version_req);
        if dev {
            manifest.dev_dependencies.insert(name.to_string(), dep);
        } else {
            manifest.dependencies.insert(name.to_string(), dep);
        }
        
        // Save manifest
        manifest.save(&self.cwd.join("claude.json"))?;
        
        // Install the new dependency
        self.install(manifest, dev)?;
        
        Ok(())
    }
    
    /// Remove a dependency
    pub fn remove(&self, manifest: &mut Manifest, name: &str) -> Result<()> {
        // Remove from manifest
        let removed = manifest.dependencies.remove(name).is_some()
            || manifest.dev_dependencies.remove(name).is_some();
        
        if !removed {
            return Err(PackageError::PackageNotFound {
                name: name.to_string(),
                version: "*".to_string(),
            });
        }
        
        // Save manifest
        manifest.save(&self.cwd.join("claude.json"))?;
        
        // TODO: Remove orphaned packages
        
        Ok(())
    }
    
    /// Update dependencies
    pub fn update(&self, manifest: &Manifest, packages: Option<Vec<String>>) -> Result<()> {
        info!("Updating dependencies");
        
        // If specific packages requested, update only those
        if let Some(packages) = packages {
            for package in packages {
                self.update_package(manifest, &package)?;
            }
        } else {
            // Update all dependencies
            self.install(manifest, true)?;
        }
        
        Ok(())
    }
    
    /// Update a specific package
    fn update_package(&self, manifest: &Manifest, name: &str) -> Result<()> {
        // Check if package is a dependency
        if !manifest.dependencies.contains_key(name) && !manifest.dev_dependencies.contains_key(name) {
            return Err(PackageError::PackageNotFound {
                name: name.to_string(),
                version: "*".to_string(),
            });
        }
        
        // Re-resolve and install
        self.install(manifest, true)?;
        
        Ok(())
    }
    
    /// List installed packages
    pub fn list(&self, tree: bool) -> Result<()> {
        let lockfile_path = self.cwd.join("claude-lock.json");
        if !lockfile_path.exists() {
            println!("No packages installed");
            return Ok(());
        }
        
        let lockfile = Lockfile::load(&lockfile_path)?;
        
        if tree {
            // Display as tree
            self.print_tree(&lockfile)?;
        } else {
            // Display as list
            for package in lockfile.packages_sorted() {
                let marker = if package.direct {
                    if package.dev { " (dev)" } else { "" }
                } else {
                    " (transitive)"
                };
                println!("{}@{}{}", package.name, package.version, marker);
            }
        }
        
        Ok(())
    }
    
    /// Print dependency tree
    fn print_tree(&self, lockfile: &Lockfile) -> Result<()> {
        let direct = lockfile.direct_dependencies();
        let dev = lockfile.dev_dependencies();
        
        if !direct.is_empty() {
            println!("Dependencies:");
            for dep in direct {
                self.print_tree_node(lockfile, dep, "", true)?;
            }
        }
        
        if !dev.is_empty() {
            println!("\nDev Dependencies:");
            for dep in dev {
                self.print_tree_node(lockfile, dep, "", true)?;
            }
        }
        
        Ok(())
    }
    
    /// Print a single node in the dependency tree
    fn print_tree_node(
        &self,
        lockfile: &Lockfile,
        package: &LockedPackage,
        prefix: &str,
        is_last: bool,
    ) -> Result<()> {
        let connector = if is_last { "└── " } else { "├── " };
        println!("{}{}{}@{}", prefix, connector, package.name, package.version);
        
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };
        
        let deps: Vec<_> = package.dependencies.keys().collect();
        for (i, dep_name) in deps.iter().enumerate() {
            // Find the dependency in lockfile
            if let Some(dep) = lockfile.packages.values().find(|p| &p.name == *dep_name) {
                let is_last_child = i == deps.len() - 1;
                self.print_tree_node(lockfile, dep, &child_prefix, is_last_child)?;
            }
        }
        
        Ok(())
    }
    
    /// Clean unused packages
    pub fn clean(&self) -> Result<()> {
        info!("Cleaning unused packages");
        
        let lockfile_path = self.cwd.join("claude-lock.json");
        if !lockfile_path.exists() {
            return Ok(());
        }
        
        let lockfile = Lockfile::load(&lockfile_path)?;
        let modules_dir = self.cwd.join("fluentai_modules");
        
        if !modules_dir.exists() {
            return Ok(());
        }
        
        // Get all installed packages
        let mut removed = 0;
        for entry in fs::read_dir(&modules_dir)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() {
                if let Some(name) = entry.file_name().to_str() {
                    // Parse package name and version
                    if let Some(at_pos) = name.find('@') {
                        let pkg_name = &name[..at_pos];
                        let pkg_version = &name[at_pos + 1..];
                        
                        if let Ok(version) = Version::parse(pkg_version) {
                            // Check if in lockfile
                            if !lockfile.has_package(pkg_name, &version) {
                                // Remove unused package
                                fs::remove_dir_all(entry.path())?;
                                info!("Removed {}@{}", pkg_name, version);
                                removed += 1;
                            }
                        }
                    }
                }
            }
        }
        
        info!("Removed {} unused packages", removed);
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::LocalRegistry;
    use tempfile::TempDir;
    
    #[test]
    fn test_installer_creation() {
        let temp_dir = TempDir::new().unwrap();
        let config = PackageConfig::default();
        let registry = Arc::new(LocalRegistry::new(temp_dir.path().to_path_buf()));
        
        let installer = PackageInstaller::new(
            config,
            registry,
            temp_dir.path().to_path_buf(),
        );
        
        // Test with empty manifest
        let manifest = Manifest::default();
        // This should succeed but do nothing
        assert!(installer.install(&manifest, false).is_ok());
    }
}