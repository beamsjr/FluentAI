//! Dependency resolution with version constraints

use crate::{manifest::Dependency, Manifest, PackageError, Registry, Result, Version, VersionReq};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;
use std::sync::Arc;
use tracing::debug;

/// Resolved dependency information
#[derive(Debug, Clone)]
pub struct ResolvedDependency {
    /// Package name
    pub name: String,

    /// Resolved version
    pub version: Version,

    /// Registry URL
    pub registry: Option<String>,

    /// Whether this is a direct dependency
    pub direct: bool,

    /// Whether this is a dev dependency
    pub dev: bool,

    /// Dependencies of this package
    pub dependencies: Vec<String>,
}

/// Dependency resolver
pub struct DependencyResolver {
    /// Registry to use for resolution
    registry: Arc<dyn Registry>,

    /// Cache of package metadata
    cache: FxHashMap<String, Vec<Version>>,
}

impl DependencyResolver {
    /// Create a new dependency resolver
    pub fn new(registry: Arc<dyn Registry>) -> Self {
        Self {
            registry,
            cache: FxHashMap::default(),
        }
    }

    /// Resolve dependencies for a manifest
    pub fn resolve(
        &mut self,
        manifest: &Manifest,
        include_dev: bool,
    ) -> Result<Vec<ResolvedDependency>> {
        let mut resolved = FxHashMap::default();
        let mut queue = VecDeque::new();

        // Add direct dependencies to queue
        for (name, dep) in manifest.all_dependencies(include_dev) {
            let req = self.parse_dependency_req(&dep)?;
            queue.push_back((name.clone(), req, true, false));
        }

        // Add dev dependencies if requested
        if include_dev {
            for (name, dep) in &manifest.dev_dependencies {
                let req = self.parse_dependency_req(dep)?;
                queue.push_back((name.clone(), req, true, true));
            }
        }

        // Resolve dependencies
        while let Some((name, req, direct, dev)) = queue.pop_front() {
            // Skip if already resolved
            if resolved.contains_key(&name) {
                // Check version compatibility
                let existing: &ResolvedDependency = &resolved[&name];
                if !req.matches(&existing.version) {
                    return Err(PackageError::VersionConflict {
                        package: name,
                        required: req.to_string(),
                        found: existing.version.to_string(),
                    });
                }
                continue;
            }

            // Find best matching version
            let version = self.find_best_version(&name, &req)?;
            debug!("Resolved {} to version {}", name, version);

            // Get package metadata
            let package_version = self.registry.get_package_version(&name, &version)?;

            // Add transitive dependencies to queue
            for (dep_name, dep_req_str) in &package_version.dependencies {
                let dep_req = VersionReq::parse(dep_req_str)?;
                queue.push_back((dep_name.clone(), dep_req, false, false));
            }

            // Create resolved dependency
            let resolved_dep = ResolvedDependency {
                name: name.clone(),
                version: version.clone(),
                registry: None, // TODO: Get from package metadata
                direct,
                dev,
                dependencies: package_version.dependencies.keys().cloned().collect(),
            };

            resolved.insert(name, resolved_dep);
        }

        // Check for cycles
        self.check_cycles(&resolved)?;

        Ok(resolved.into_values().collect())
    }

    /// Parse a dependency specification into a version requirement
    fn parse_dependency_req(&self, dep: &Dependency) -> Result<VersionReq> {
        match dep {
            Dependency::Version(v) => VersionReq::parse(v),
            Dependency::Detailed { version, .. } => VersionReq::parse(version),
        }
    }

    /// Find the best matching version for a package
    fn find_best_version(&mut self, name: &str, req: &VersionReq) -> Result<Version> {
        // Check cache first
        let versions = if let Some(cached) = self.cache.get(name) {
            cached.clone()
        } else {
            // Get all versions from registry
            let all_versions = self
                .registry
                .get_matching_versions(name, &VersionReq::any())?;
            self.cache.insert(name.to_string(), all_versions.clone());
            all_versions
        };

        // Find matching versions
        let mut matching: Vec<_> = versions.into_iter().filter(|v| req.matches(v)).collect();

        if matching.is_empty() {
            return Err(PackageError::NoCompatibleVersion {
                package: name.to_string(),
                constraint: req.to_string(),
            });
        }

        // Sort by version (highest first)
        matching.sort();
        matching.reverse();

        // Prefer stable versions over pre-releases
        if let Some(stable) = matching.iter().find(|v| !v.is_prerelease()) {
            Ok(stable.clone())
        } else {
            Ok(matching[0].clone())
        }
    }

    /// Check for circular dependencies
    fn check_cycles(&self, resolved: &FxHashMap<String, ResolvedDependency>) -> Result<()> {
        let mut visited = FxHashSet::default();
        let mut rec_stack = FxHashSet::default();

        for name in resolved.keys() {
            if !visited.contains(name) {
                if self.has_cycle(name, resolved, &mut visited, &mut rec_stack)? {
                    return Err(PackageError::CircularDependency {
                        cycle: rec_stack.iter().cloned().collect::<Vec<_>>().join(" -> "),
                    });
                }
            }
        }

        Ok(())
    }

    /// Check if there's a cycle starting from the given package
    fn has_cycle(
        &self,
        name: &str,
        resolved: &FxHashMap<String, ResolvedDependency>,
        visited: &mut FxHashSet<String>,
        rec_stack: &mut FxHashSet<String>,
    ) -> Result<bool> {
        visited.insert(name.to_string());
        rec_stack.insert(name.to_string());

        if let Some(dep) = resolved.get(name) {
            for child in &dep.dependencies {
                if !visited.contains(child) {
                    if self.has_cycle(child, resolved, visited, rec_stack)? {
                        return Ok(true);
                    }
                } else if rec_stack.contains(child) {
                    return Ok(true);
                }
            }
        }

        rec_stack.remove(name);
        Ok(false)
    }
}

/// Build a dependency graph from resolved dependencies
pub fn build_dependency_graph(resolved: &[ResolvedDependency]) -> FxHashMap<String, Vec<String>> {
    let mut graph = FxHashMap::default();

    for dep in resolved {
        graph.insert(dep.name.clone(), dep.dependencies.clone());
    }

    graph
}

/// Perform a topological sort of resolved dependencies
pub fn topological_sort(resolved: &[ResolvedDependency]) -> Result<Vec<String>> {
    let graph = build_dependency_graph(resolved);
    let mut sorted = Vec::new();
    let mut visited = FxHashSet::default();
    let mut rec_stack = FxHashSet::default();

    for dep in resolved {
        if !visited.contains(&dep.name) {
            topological_sort_util(&dep.name, &graph, &mut visited, &mut rec_stack, &mut sorted)?;
        }
    }

    Ok(sorted)
}

fn topological_sort_util(
    name: &str,
    graph: &FxHashMap<String, Vec<String>>,
    visited: &mut FxHashSet<String>,
    rec_stack: &mut FxHashSet<String>,
    sorted: &mut Vec<String>,
) -> Result<()> {
    visited.insert(name.to_string());
    rec_stack.insert(name.to_string());

    if let Some(deps) = graph.get(name) {
        for dep in deps {
            if !visited.contains(dep) {
                topological_sort_util(dep, graph, visited, rec_stack, sorted)?;
            } else if rec_stack.contains(dep) {
                return Err(PackageError::CircularDependency {
                    cycle: format!("{} -> {}", name, dep),
                });
            }
        }
    }

    rec_stack.remove(name);
    sorted.push(name.to_string());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::LocalRegistry;
    use std::path::PathBuf;
    use tempfile::TempDir;

    #[test]
    fn test_dependency_resolution() {
        let temp_dir = TempDir::new().unwrap();
        let registry = Arc::new(LocalRegistry::new(temp_dir.path().to_path_buf()));
        let mut resolver = DependencyResolver::new(registry);

        let mut manifest = Manifest::default();
        manifest.name = "test-package".to_string();
        manifest.version = "1.0.0".to_string();

        // Test with no dependencies
        let resolved = resolver.resolve(&manifest, false).unwrap();
        assert!(resolved.is_empty());
    }

    #[test]
    fn test_circular_dependency_detection() {
        let mut resolved = FxHashMap::default();

        resolved.insert(
            "a".to_string(),
            ResolvedDependency {
                name: "a".to_string(),
                version: Version::new(1, 0, 0),
                registry: None,
                direct: true,
                dev: false,
                dependencies: vec!["b".to_string()],
            },
        );

        resolved.insert(
            "b".to_string(),
            ResolvedDependency {
                name: "b".to_string(),
                version: Version::new(1, 0, 0),
                registry: None,
                direct: false,
                dev: false,
                dependencies: vec!["a".to_string()],
            },
        );

        let resolver = DependencyResolver::new(Arc::new(LocalRegistry::new(PathBuf::new())));
        assert!(resolver.check_cycles(&resolved).is_err());
    }

    #[test]
    fn test_topological_sort() {
        let resolved = vec![
            ResolvedDependency {
                name: "a".to_string(),
                version: Version::new(1, 0, 0),
                registry: None,
                direct: true,
                dev: false,
                dependencies: vec!["b".to_string()],
            },
            ResolvedDependency {
                name: "b".to_string(),
                version: Version::new(1, 0, 0),
                registry: None,
                direct: false,
                dev: false,
                dependencies: vec!["c".to_string()],
            },
            ResolvedDependency {
                name: "c".to_string(),
                version: Version::new(1, 0, 0),
                registry: None,
                direct: false,
                dev: false,
                dependencies: vec![],
            },
        ];

        let sorted = topological_sort(&resolved).unwrap();
        assert_eq!(sorted, vec!["c", "b", "a"]);
    }
}
