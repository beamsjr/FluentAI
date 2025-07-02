"""
Package installer for ClaudeLang

Handles downloading and installing packages.
"""

import os
import shutil
import tarfile
import tempfile
from pathlib import Path
from typing import Dict, List, Optional, Set
import json

from .manifest import Manifest
from .registry import Registry
from .resolver import DependencyResolver, ResolvedPackage, create_lock_file


class PackageInstaller:
    """Installs ClaudeLang packages"""
    
    def __init__(self, registry: Registry, install_dir: Optional[Path] = None):
        self.registry = registry
        self.install_dir = install_dir or Path.cwd() / "claude_modules"
        self.resolver = DependencyResolver(registry)
        
    def install(
        self, 
        manifest: Manifest,
        include_dev: bool = False,
        force: bool = False,
        use_lock: bool = True
    ) -> Dict[str, ResolvedPackage]:
        """
        Install all dependencies for a manifest
        
        Args:
            manifest: Package manifest
            include_dev: Include development dependencies
            force: Force reinstall even if already installed
            use_lock: Use claude.lock if it exists
            
        Returns:
            Dictionary of installed packages
        """
        # Check for lock file
        lock_path = Path.cwd() / "claude.lock"
        if use_lock and lock_path.exists():
            return self._install_from_lock(lock_path, force)
        
        # Resolve dependencies
        print("Resolving dependencies...")
        resolved = self.resolver.resolve(manifest, include_dev)
        
        # Install each package
        installed = {}
        for name, package in resolved.items():
            if self._install_package(package, force):
                installed[name] = package
        
        # Create lock file
        lock_data = create_lock_file(resolved)
        with open(lock_path, 'w') as f:
            json.dump(lock_data, f, indent=2)
        
        print(f"\nInstalled {len(installed)} packages")
        return installed
    
    def install_package(
        self, 
        name: str, 
        version_spec: str = "*",
        force: bool = False
    ) -> Optional[ResolvedPackage]:
        """Install a single package by name"""
        from .manifest import Dependency
        
        dep = Dependency(name=name, version_spec=version_spec)
        
        # Resolve with empty requirements
        requirements = {name: [("direct", dep)]}
        package = self.resolver._resolve_package(name, dep, requirements, "direct")
        
        if package and self._install_package(package, force):
            return package
        
        return None
    
    def _install_package(self, package: ResolvedPackage, force: bool = False) -> bool:
        """Install a single resolved package"""
        pkg_dir = self.install_dir / package.name / package.version
        
        # Check if already installed
        if pkg_dir.exists() and not force:
            print(f"✓ {package.name}@{package.version} (already installed)")
            return False
        
        print(f"Installing {package.name}@{package.version}...")
        
        # Create temporary directory for download
        with tempfile.TemporaryDirectory() as tmp_dir:
            tmp_path = Path(tmp_dir)
            
            # Download package
            try:
                tarball_path = self.registry.download_package(
                    package.name,
                    package.version,
                    tmp_path
                )
            except Exception as e:
                print(f"✗ Failed to download {package.name}@{package.version}: {e}")
                return False
            
            # Extract package
            extract_dir = tmp_path / "extract"
            extract_dir.mkdir()
            
            with tarfile.open(tarball_path, "r:gz") as tar:
                tar.extractall(extract_dir)
            
            # Find package root (might be nested)
            pkg_root = extract_dir
            if (extract_dir / package.name).exists():
                pkg_root = extract_dir / package.name
            elif len(list(extract_dir.iterdir())) == 1:
                # Single directory in archive
                pkg_root = list(extract_dir.iterdir())[0]
            
            # Ensure target directory exists
            pkg_dir.parent.mkdir(parents=True, exist_ok=True)
            
            # Remove old installation if forcing
            if pkg_dir.exists() and force:
                shutil.rmtree(pkg_dir)
            
            # Copy to installation directory
            shutil.copytree(pkg_root, pkg_dir)
        
        # Install transitive dependencies
        for dep in package.dependencies:
            self._install_package(dep, force)
        
        print(f"✓ {package.name}@{package.version}")
        return True
    
    def _install_from_lock(self, lock_path: Path, force: bool = False) -> Dict[str, ResolvedPackage]:
        """Install packages from lock file"""
        with open(lock_path, 'r') as f:
            lock_data = json.load(f)
        
        if lock_data.get("version") != "1.0":
            raise ValueError(f"Unsupported lock file version: {lock_data.get('version')}")
        
        installed = {}
        packages = lock_data.get("packages", {})
        
        # Install in dependency order
        installed_names: Set[str] = set()
        
        def install_with_deps(name: str, info: Dict) -> None:
            if name in installed_names:
                return
            
            # Install dependencies first
            for dep_name in info.get("dependencies", {}):
                if dep_name in packages:
                    install_with_deps(dep_name, packages[dep_name])
            
            # Install this package
            pkg_info = self.registry.get_package_info(name, info["version"])
            if pkg_info:
                package = ResolvedPackage(
                    name=name,
                    version=info["version"],
                    info=pkg_info
                )
                
                if self._install_package(package, force):
                    installed[name] = package
                    installed_names.add(name)
        
        for name, info in packages.items():
            install_with_deps(name, info)
        
        return installed
    
    def uninstall(self, name: str, version: Optional[str] = None) -> bool:
        """Uninstall a package"""
        if version:
            # Uninstall specific version
            pkg_dir = self.install_dir / name / version
            if pkg_dir.exists():
                shutil.rmtree(pkg_dir)
                print(f"Uninstalled {name}@{version}")
                
                # Remove empty parent directory
                if not list(pkg_dir.parent.iterdir()):
                    pkg_dir.parent.rmdir()
                
                return True
            else:
                print(f"Package {name}@{version} is not installed")
                return False
        else:
            # Uninstall all versions
            pkg_dir = self.install_dir / name
            if pkg_dir.exists():
                versions = list(pkg_dir.iterdir())
                shutil.rmtree(pkg_dir)
                print(f"Uninstalled {name} ({len(versions)} version(s))")
                return True
            else:
                print(f"Package {name} is not installed")
                return False
    
    def list_installed(self) -> Dict[str, List[str]]:
        """List all installed packages and their versions"""
        installed = {}
        
        if not self.install_dir.exists():
            return installed
        
        for pkg_dir in self.install_dir.iterdir():
            if pkg_dir.is_dir():
                versions = []
                for version_dir in pkg_dir.iterdir():
                    if version_dir.is_dir():
                        versions.append(version_dir.name)
                
                if versions:
                    installed[pkg_dir.name] = sorted(versions)
        
        return installed
    
    def clean(self) -> int:
        """Remove packages not in current manifest"""
        # Load current manifest
        try:
            manifest = Manifest.load(Path.cwd())
        except FileNotFoundError:
            print("No claude.json found in current directory")
            return 0
        
        # Resolve current dependencies
        resolved = self.resolver.resolve(manifest, include_dev=False)
        needed = set()
        
        # Collect all needed packages
        for pkg in resolved.values():
            all_deps = pkg.flatten()
            for name, dep_pkg in all_deps.items():
                needed.add(f"{name}@{dep_pkg.version}")
        
        # Find installed packages
        installed = self.list_installed()
        removed = 0
        
        for name, versions in installed.items():
            for version in versions:
                pkg_id = f"{name}@{version}"
                if pkg_id not in needed:
                    if self.uninstall(name, version):
                        removed += 1
        
        print(f"\nRemoved {removed} unused packages")
        return removed