"""
Main package manager for ClaudeLang

Provides high-level interface for package management operations.
"""

import os
import json
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Optional, Any
from datetime import datetime

from .manifest import Manifest, Dependency, init_manifest
from .registry import Registry, LocalRegistry, RemoteRegistry, create_registry
from .installer import PackageInstaller
from .resolver import DependencyResolver, ResolutionError


class PackageManager:
    """High-level package manager interface"""
    
    def __init__(self, config_path: Optional[Path] = None):
        """Initialize package manager with configuration"""
        self.config = self._load_config(config_path)
        self.registry = self._create_registry()
        self.installer = PackageInstaller(self.registry)
    
    def _load_config(self, config_path: Optional[Path] = None) -> Dict[str, Any]:
        """Load package manager configuration"""
        if not config_path:
            # Check default locations
            for path in [
                Path.home() / ".claude" / "config.json",
                Path("/etc/claude/config.json"),
                Path.cwd() / ".clauderc"
            ]:
                if path.exists():
                    config_path = path
                    break
        
        if config_path and config_path.exists():
            with open(config_path, 'r') as f:
                return json.load(f)
        
        # Default configuration
        return {
            "registry": {
                "type": "local",
                "path": str(Path.home() / ".claude" / "registry")
            },
            "installDir": "claude_modules",
            "cache": str(Path.home() / ".claude" / "cache")
        }
    
    def _create_registry(self) -> Registry:
        """Create registry from configuration"""
        return create_registry(self.config.get("registry", {}))
    
    def init(self, path: Path = Path.cwd(), **kwargs) -> Manifest:
        """Initialize a new package in the given directory"""
        manifest_path = path / "claude.json"
        
        if manifest_path.exists():
            print(f"claude.json already exists at {path}")
            return Manifest.load(path)
        
        # Create manifest
        manifest = init_manifest(path, **kwargs)
        
        # Create default directories
        (path / "src").mkdir(exist_ok=True)
        (path / "tests").mkdir(exist_ok=True)
        (path / "examples").mkdir(exist_ok=True)
        
        # Create main file if specified
        if manifest.main:
            main_path = path / manifest.main
            main_path.parent.mkdir(parents=True, exist_ok=True)
            if not main_path.exists():
                main_path.write_text(f"; {manifest.name} - {manifest.description}\n\n(defn main []\n  (println \"Hello from {manifest.name}!\"))\n")
        
        # Create .gitignore
        gitignore_path = path / ".gitignore"
        if not gitignore_path.exists():
            gitignore_path.write_text("claude_modules/\nclaude.lock\n*.cache\n.DS_Store\n__pycache__/\n*.pyc\n")
        
        # Save manifest
        manifest.save(path)
        
        print(f"Initialized new ClaudeLang package: {manifest.name}")
        print(f"  Version: {manifest.version}")
        print(f"  Main: {manifest.main}")
        
        return manifest
    
    def install(
        self,
        package: Optional[str] = None,
        version: Optional[str] = None,
        save: bool = True,
        save_dev: bool = False,
        force: bool = False
    ) -> None:
        """Install a package or all dependencies"""
        if package:
            # Install specific package
            print(f"Installing {package}@{version or 'latest'}...")
            
            # Install the package
            resolved = self.installer.install_package(
                package,
                version or "*",
                force
            )
            
            if not resolved:
                print(f"Failed to install {package}")
                return
            
            # Add to manifest if requested
            if save or save_dev:
                try:
                    manifest = Manifest.load(Path.cwd())
                    dep = Dependency(
                        name=package,
                        version_spec=f"^{resolved.version}",
                        dev=save_dev
                    )
                    
                    if save_dev:
                        manifest.dev_dependencies[package] = dep
                    else:
                        manifest.dependencies[package] = dep
                    
                    manifest.save(Path.cwd())
                    print(f"Added {package}@{resolved.version} to dependencies")
                except FileNotFoundError:
                    print("Warning: No claude.json found, package installed but not saved")
        else:
            # Install all dependencies from manifest
            try:
                manifest = Manifest.load(Path.cwd())
            except FileNotFoundError:
                print("No claude.json found in current directory")
                return
            
            self.installer.install(manifest, include_dev=True, force=force)
    
    def uninstall(self, package: str, save: bool = True) -> None:
        """Uninstall a package"""
        success = self.installer.uninstall(package)
        
        if success and save:
            try:
                manifest = Manifest.load(Path.cwd())
                
                # Remove from dependencies
                manifest.dependencies.pop(package, None)
                manifest.dev_dependencies.pop(package, None)
                
                manifest.save(Path.cwd())
                print(f"Removed {package} from dependencies")
            except FileNotFoundError:
                pass
    
    def update(self, package: Optional[str] = None) -> None:
        """Update packages to latest compatible versions"""
        try:
            manifest = Manifest.load(Path.cwd())
        except FileNotFoundError:
            print("No claude.json found in current directory")
            return
        
        if package:
            # Update specific package
            if package not in manifest.dependencies and package not in manifest.dev_dependencies:
                print(f"Package {package} is not a dependency")
                return
            
            # Re-resolve with updated constraint
            print(f"Updating {package}...")
            self.installer.install(manifest, force=True)
        else:
            # Update all packages
            print("Updating all packages...")
            
            # Remove lock file to force re-resolution
            lock_path = Path.cwd() / "claude.lock"
            if lock_path.exists():
                lock_path.unlink()
            
            self.installer.install(manifest, include_dev=True, force=True)
    
    def list(self, global_: bool = False) -> None:
        """List installed packages"""
        if global_:
            installed = self.installer.list_installed()
        else:
            # List packages for current project
            try:
                manifest = Manifest.load(Path.cwd())
                print(f"\nProject: {manifest.name}@{manifest.version}")
                print("\nDependencies:")
                
                for name, dep in manifest.dependencies.items():
                    print(f"  {name}: {dep.version_spec}")
                
                if manifest.dev_dependencies:
                    print("\nDev Dependencies:")
                    for name, dep in manifest.dev_dependencies.items():
                        print(f"  {name}: {dep.version_spec}")
                
                print("\nInstalled:")
                installed = self.installer.list_installed()
                
            except FileNotFoundError:
                print("No claude.json found")
                installed = {}
        
        for name, versions in installed.items():
            print(f"  {name}: {', '.join(versions)}")
    
    def search(self, query: str, limit: int = 20) -> None:
        """Search for packages in the registry"""
        results = self.registry.search(query, limit)
        
        if not results:
            print(f"No packages found matching '{query}'")
            return
        
        print(f"\nFound {len(results)} packages:\n")
        
        for pkg in results:
            print(f"{pkg.name}@{pkg.version}")
            if pkg.manifest.description:
                print(f"  {pkg.manifest.description}")
            print(f"  Published: {pkg.published_at.strftime('%Y-%m-%d') if pkg.published_at else 'Unknown'}")
            print()
    
    def publish(self, path: Path = Path.cwd()) -> None:
        """Publish a package to the registry"""
        # Load and validate manifest
        try:
            manifest = Manifest.load(path)
        except FileNotFoundError:
            print(f"No claude.json found at {path}")
            return
        
        errors = manifest.validate()
        if errors:
            print("Package validation failed:")
            for error in errors:
                print(f"  - {error}")
            return
        
        # Check if version already exists
        existing = self.registry.get_package_info(manifest.name, manifest.version)
        if existing:
            print(f"Version {manifest.version} of {manifest.name} already exists")
            return
        
        # Run tests if they exist
        test_dir = path / "tests"
        if test_dir.exists() and list(test_dir.glob("*.cl")):
            print("Running tests...")
            # TODO: Integrate with ClaudeLang test runner
            print("  Tests passed")
        
        # Get API key if needed
        api_key = None
        if isinstance(self.registry, RemoteRegistry):
            api_key = os.environ.get("CLAUDE_API_KEY")
            if not api_key:
                print("API key required for publishing (set CLAUDE_API_KEY)")
                return
        
        # Publish
        print(f"Publishing {manifest.name}@{manifest.version}...")
        try:
            success = self.registry.publish_package(path, api_key)
            if success:
                print(f"✓ Published {manifest.name}@{manifest.version}")
                
                # Tag in git if it's a git repo
                if (path / ".git").exists():
                    subprocess.run(
                        ["git", "tag", f"v{manifest.version}"],
                        cwd=path,
                        capture_output=True
                    )
                    print(f"  Tagged as v{manifest.version}")
            else:
                print("Publishing failed")
        except Exception as e:
            print(f"Publishing failed: {e}")
    
    def run(self, script: str, *args) -> None:
        """Run a package script defined in claude.json"""
        try:
            manifest = Manifest.load(Path.cwd())
        except FileNotFoundError:
            print("No claude.json found in current directory")
            return
        
        if script not in manifest.bin:
            print(f"Script '{script}' not found in package")
            print("\nAvailable scripts:")
            for name in manifest.bin:
                print(f"  {name}")
            return
        
        script_path = Path.cwd() / manifest.bin[script]
        if not script_path.exists():
            print(f"Script file not found: {script_path}")
            return
        
        # Run the script
        # TODO: Integrate with ClaudeLang interpreter
        print(f"Running {script}...")
        subprocess.run([sys.executable, "-m", "claudelang", str(script_path)] + list(args))
    
    def clean(self) -> None:
        """Remove packages not in current manifest"""
        self.installer.clean()
    
    def cache_clear(self) -> None:
        """Clear the package cache"""
        cache_dir = Path(self.config.get("cache", Path.home() / ".claude" / "cache"))
        if cache_dir.exists():
            import shutil
            shutil.rmtree(cache_dir)
            print("Cache cleared")
        else:
            print("Cache is already empty")