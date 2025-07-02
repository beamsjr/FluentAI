"""
Package manifest handling for ClaudeLang packages

A manifest (claude.json) describes a package and its dependencies.
"""

import json
import os
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from pathlib import Path

# Make semver optional
try:
    import semver
    HAS_SEMVER = True
except ImportError:
    HAS_SEMVER = False
    # Simple version comparison fallback
    class SimpleVersion:
        def __init__(self, version_str: str):
            self.version_str = version_str
            parts = version_str.split('.')
            self.major = int(parts[0]) if parts else 0
            self.minor = int(parts[1]) if len(parts) > 1 else 0
            self.patch = int(parts[2]) if len(parts) > 2 else 0
        
        def match(self, spec: str) -> bool:
            """Simple version matching"""
            if spec == "*":
                return True
            if spec.startswith("^"):
                # Caret: compatible with version
                min_ver = SimpleVersion(spec[1:])
                return self.major == min_ver.major and self >= min_ver
            if spec.startswith("~"):
                # Tilde: reasonably close to version
                min_ver = SimpleVersion(spec[1:])
                return (self.major == min_ver.major and 
                       self.minor == min_ver.minor and 
                       self.patch >= min_ver.patch)
            if spec.startswith(">="):
                return self >= SimpleVersion(spec[2:])
            if spec.startswith(">"):
                return self > SimpleVersion(spec[1:])
            if spec.startswith("<="):
                return self <= SimpleVersion(spec[2:])
            if spec.startswith("<"):
                return self < SimpleVersion(spec[1:])
            # Exact match
            return self.version_str == spec
        
        def __ge__(self, other):
            return (self.major, self.minor, self.patch) >= (other.major, other.minor, other.patch)
        
        def __gt__(self, other):
            return (self.major, self.minor, self.patch) > (other.major, other.minor, other.patch)
        
        def __le__(self, other):
            return (self.major, self.minor, self.patch) <= (other.major, other.minor, other.patch)
        
        def __lt__(self, other):
            return (self.major, self.minor, self.patch) < (other.major, other.minor, other.patch)


@dataclass
class Dependency:
    """Represents a package dependency"""
    name: str
    version_spec: str  # Semantic version specification
    source: Optional[str] = None  # Optional source URL
    dev: bool = False  # Development dependency
    
    def matches_version(self, version: str) -> bool:
        """Check if a version satisfies this dependency"""
        if HAS_SEMVER:
            try:
                return semver.VersionInfo.parse(version).match(self.version_spec)
            except:
                # Fallback for exact version match
                return version == self.version_spec
        else:
            # Use simple version matching
            try:
                return SimpleVersion(version).match(self.version_spec)
            except:
                return version == self.version_spec
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization"""
        d = {"version": self.version_spec}
        if self.source:
            d["source"] = self.source
        if self.dev:
            d["dev"] = True
        return d
    
    @classmethod
    def from_dict(cls, name: str, data: Dict[str, Any]) -> 'Dependency':
        """Create from dictionary"""
        if isinstance(data, str):
            # Simple version string
            return cls(name=name, version_spec=data)
        
        return cls(
            name=name,
            version_spec=data.get("version", "*"),
            source=data.get("source"),
            dev=data.get("dev", False)
        )


@dataclass
class Manifest:
    """Package manifest (claude.json)"""
    name: str
    version: str
    description: str = ""
    author: str = ""
    license: str = "MIT"
    homepage: Optional[str] = None
    repository: Optional[str] = None
    keywords: List[str] = field(default_factory=list)
    
    # Entry points
    main: Optional[str] = None  # Main module
    bin: Dict[str, str] = field(default_factory=dict)  # Binary scripts
    
    # Dependencies
    dependencies: Dict[str, Dependency] = field(default_factory=dict)
    dev_dependencies: Dict[str, Dependency] = field(default_factory=dict)
    
    # ClaudeLang specific
    claude_version: str = ">=0.1.0"
    effects: List[str] = field(default_factory=list)  # Required effects
    platforms: List[str] = field(default_factory=list)  # Supported platforms
    
    # Build configuration
    include: List[str] = field(default_factory=lambda: ["**/*.cl", "README.md", "LICENSE"])
    exclude: List[str] = field(default_factory=lambda: ["tests/**", "examples/**", ".*"])
    
    @classmethod
    def load(cls, path: Path) -> 'Manifest':
        """Load manifest from claude.json file"""
        manifest_path = path / "claude.json" if path.is_dir() else path
        
        if not manifest_path.exists():
            raise FileNotFoundError(f"No claude.json found at {manifest_path}")
        
        with open(manifest_path, 'r') as f:
            data = json.load(f)
        
        return cls.from_dict(data)
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'Manifest':
        """Create manifest from dictionary"""
        # Parse dependencies
        deps = {}
        for name, dep_data in data.get("dependencies", {}).items():
            deps[name] = Dependency.from_dict(name, dep_data)
        
        dev_deps = {}
        for name, dep_data in data.get("devDependencies", {}).items():
            dep = Dependency.from_dict(name, dep_data)
            dep.dev = True
            dev_deps[name] = dep
        
        return cls(
            name=data["name"],
            version=data["version"],
            description=data.get("description", ""),
            author=data.get("author", ""),
            license=data.get("license", "MIT"),
            homepage=data.get("homepage"),
            repository=data.get("repository"),
            keywords=data.get("keywords", []),
            main=data.get("main"),
            bin=data.get("bin", {}),
            dependencies=deps,
            dev_dependencies=dev_deps,
            claude_version=data.get("claudeVersion", ">=0.1.0"),
            effects=data.get("effects", []),
            platforms=data.get("platforms", []),
            include=data.get("include", cls.include.default_factory()),
            exclude=data.get("exclude", cls.exclude.default_factory())
        )
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization"""
        data = {
            "name": self.name,
            "version": self.version,
            "description": self.description,
            "author": self.author,
            "license": self.license
        }
        
        if self.homepage:
            data["homepage"] = self.homepage
        if self.repository:
            data["repository"] = self.repository
        if self.keywords:
            data["keywords"] = self.keywords
        
        if self.main:
            data["main"] = self.main
        if self.bin:
            data["bin"] = self.bin
        
        if self.dependencies:
            data["dependencies"] = {
                name: dep.to_dict() if isinstance(dep.to_dict(), dict) else dep.version_spec
                for name, dep in self.dependencies.items()
            }
        
        if self.dev_dependencies:
            data["devDependencies"] = {
                name: dep.to_dict() if isinstance(dep.to_dict(), dict) else dep.version_spec
                for name, dep in self.dev_dependencies.items()
            }
        
        data["claudeVersion"] = self.claude_version
        
        if self.effects:
            data["effects"] = self.effects
        if self.platforms:
            data["platforms"] = self.platforms
        
        data["include"] = self.include
        data["exclude"] = self.exclude
        
        return data
    
    def save(self, path: Path) -> None:
        """Save manifest to claude.json file"""
        manifest_path = path / "claude.json" if path.is_dir() else path
        
        with open(manifest_path, 'w') as f:
            json.dump(self.to_dict(), f, indent=2)
    
    def validate(self) -> List[str]:
        """Validate manifest and return list of errors"""
        errors = []
        
        # Validate required fields
        if not self.name:
            errors.append("Package name is required")
        elif not self.name.replace("-", "").replace("_", "").isalnum():
            errors.append("Package name must be alphanumeric with hyphens/underscores")
        
        if not self.version:
            errors.append("Package version is required")
        else:
            if HAS_SEMVER:
                try:
                    semver.VersionInfo.parse(self.version)
                except:
                    errors.append(f"Invalid version format: {self.version}")
            else:
                # Basic version format check
                parts = self.version.split('.')
                if len(parts) != 3 or not all(p.isdigit() for p in parts):
                    errors.append(f"Invalid version format: {self.version} (expected X.Y.Z)")
        
        # Validate entry points
        if self.main and not self.main.endswith(".cl"):
            errors.append(f"Main entry point must be a .cl file: {self.main}")
        
        for bin_name, bin_path in self.bin.items():
            if not bin_path.endswith(".cl"):
                errors.append(f"Binary entry point must be a .cl file: {bin_name} -> {bin_path}")
        
        # Validate dependencies
        for name, dep in self.dependencies.items():
            if name == self.name:
                errors.append("Package cannot depend on itself")
        
        return errors
    
    def get_all_dependencies(self, include_dev: bool = False) -> Dict[str, Dependency]:
        """Get all dependencies (optionally including dev)"""
        deps = self.dependencies.copy()
        if include_dev:
            deps.update(self.dev_dependencies)
        return deps


def init_manifest(path: Path, **kwargs) -> Manifest:
    """Initialize a new manifest with defaults"""
    name = kwargs.get("name", path.name)
    
    manifest = Manifest(
        name=name,
        version=kwargs.get("version", "0.1.0"),
        description=kwargs.get("description", f"A ClaudeLang package"),
        author=kwargs.get("author", ""),
        main=kwargs.get("main", "src/main.cl")
    )
    
    return manifest