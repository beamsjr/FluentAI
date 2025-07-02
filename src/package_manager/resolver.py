"""
Dependency resolver for ClaudeLang packages

Implements version resolution and conflict detection.
"""

from typing import Dict, List, Set, Optional, Tuple, Any
from dataclasses import dataclass, field
from collections import defaultdict

from .manifest import Manifest, Dependency, HAS_SEMVER, SimpleVersion
from .registry import Registry, PackageInfo

if HAS_SEMVER:
    import semver


@dataclass
class ResolvedPackage:
    """A resolved package with specific version"""
    name: str
    version: str
    info: PackageInfo
    dependencies: List['ResolvedPackage'] = field(default_factory=list)
    
    def flatten(self) -> Dict[str, 'ResolvedPackage']:
        """Flatten dependency tree into a dict"""
        result = {self.name: self}
        for dep in self.dependencies:
            result.update(dep.flatten())
        return result


@dataclass
class ResolutionConflict:
    """Represents a version conflict"""
    package: str
    requested_by: List[Tuple[str, str]]  # [(package, version_spec), ...]
    
    def __str__(self) -> str:
        requests = []
        for pkg, spec in self.requested_by:
            requests.append(f"{pkg} requires {self.package}@{spec}")
        return f"Version conflict for {self.package}:\n  " + "\n  ".join(requests)


class DependencyResolver:
    """Resolves package dependencies"""
    
    def __init__(self, registry: Registry):
        self.registry = registry
        self._resolution_cache: Dict[str, ResolvedPackage] = {}
    
    def resolve(self, manifest: Manifest, include_dev: bool = False) -> Dict[str, ResolvedPackage]:
        """
        Resolve all dependencies for a manifest
        
        Returns a flat dictionary of package name -> ResolvedPackage
        """
        # Reset cache for new resolution
        self._resolution_cache = {}
        
        # Get all direct dependencies
        deps = manifest.get_all_dependencies(include_dev)
        
        # Track requirements for conflict detection
        requirements: Dict[str, List[Tuple[str, Dependency]]] = defaultdict(list)
        
        # Add direct dependencies to requirements
        for name, dep in deps.items():
            requirements[name].append((manifest.name, dep))
        
        # Resolve each dependency
        resolved = {}
        for name, dep in deps.items():
            pkg = self._resolve_package(name, dep, requirements, manifest.name)
            if pkg:
                resolved[name] = pkg
        
        # Check for conflicts
        conflicts = self._find_conflicts(requirements, resolved)
        if conflicts:
            raise ResolutionError(conflicts)
        
        return resolved
    
    def _resolve_package(
        self, 
        name: str, 
        dep: Dependency,
        requirements: Dict[str, List[Tuple[str, Dependency]]],
        parent: str
    ) -> Optional[ResolvedPackage]:
        """Resolve a single package and its dependencies"""
        # Check cache
        cache_key = f"{name}@{dep.version_spec}"
        if cache_key in self._resolution_cache:
            return self._resolution_cache[cache_key]
        
        # Get available versions
        package_versions = self.registry.get_package(name)
        if not package_versions:
            raise PackageNotFoundError(f"Package not found: {name}")
        
        # Find best matching version
        best_version = self._find_best_version(dep, package_versions)
        if not best_version:
            raise VersionNotFoundError(
                f"No version of {name} satisfies {dep.version_spec} (required by {parent})"
            )
        
        info = package_versions[best_version]
        
        # Create resolved package
        resolved = ResolvedPackage(
            name=name,
            version=best_version,
            info=info
        )
        
        # Cache it
        self._resolution_cache[cache_key] = resolved
        
        # Resolve transitive dependencies
        for dep_name, dep_spec in info.manifest.dependencies.items():
            # Add to requirements tracking
            requirements[dep_name].append((name, dep_spec))
            
            # Resolve recursively
            sub_pkg = self._resolve_package(dep_name, dep_spec, requirements, name)
            if sub_pkg:
                resolved.dependencies.append(sub_pkg)
        
        return resolved
    
    def _find_best_version(
        self, 
        dep: Dependency, 
        versions: Dict[str, PackageInfo]
    ) -> Optional[str]:
        """Find the best matching version for a dependency"""
        matching_versions = []
        
        for version, info in versions.items():
            if dep.matches_version(version):
                matching_versions.append(version)
        
        if not matching_versions:
            return None
        
        # Sort by semantic version (newest first)
        if HAS_SEMVER:
            matching_versions.sort(
                key=lambda v: semver.VersionInfo.parse(v),
                reverse=True
            )
        else:
            # Simple version sorting
            matching_versions.sort(
                key=lambda v: SimpleVersion(v),
                reverse=True
            )
        
        return matching_versions[0]
    
    def _find_conflicts(
        self,
        requirements: Dict[str, List[Tuple[str, Dependency]]],
        resolved: Dict[str, ResolvedPackage]
    ) -> List[ResolutionConflict]:
        """Find version conflicts in resolved packages"""
        conflicts = []
        
        # Flatten all resolved packages
        all_resolved = {}
        for pkg in resolved.values():
            all_resolved.update(pkg.flatten())
        
        # Check each package's requirements
        for pkg_name, reqs in requirements.items():
            if pkg_name not in all_resolved:
                continue
                
            resolved_version = all_resolved[pkg_name].version
            
            # Check if all requirements are satisfied
            unsatisfied = []
            for parent, dep in reqs:
                if not dep.matches_version(resolved_version):
                    unsatisfied.append((parent, dep.version_spec))
            
            if unsatisfied:
                conflicts.append(ResolutionConflict(
                    package=pkg_name,
                    requested_by=unsatisfied
                ))
        
        return conflicts


class ResolutionError(Exception):
    """Raised when dependencies cannot be resolved"""
    def __init__(self, conflicts: List[ResolutionConflict]):
        self.conflicts = conflicts
        messages = [str(c) for c in conflicts]
        super().__init__("Failed to resolve dependencies:\n" + "\n\n".join(messages))


class PackageNotFoundError(ResolutionError):
    """Raised when a package is not found in the registry"""
    pass


class VersionNotFoundError(ResolutionError):
    """Raised when no matching version is found"""
    pass


def create_lock_file(resolved: Dict[str, ResolvedPackage]) -> Dict[str, Any]:
    """Create a lock file from resolved dependencies"""
    lock = {
        "version": "1.0",
        "packages": {}
    }
    
    # Flatten all packages
    all_packages = {}
    for pkg in resolved.values():
        all_packages.update(pkg.flatten())
    
    # Add each package to lock
    for name, pkg in all_packages.items():
        lock["packages"][name] = {
            "version": pkg.version,
            "checksum": pkg.info.checksum,
            "dependencies": {
                dep.name: dep.version for dep in pkg.dependencies
            }
        }
    
    return lock