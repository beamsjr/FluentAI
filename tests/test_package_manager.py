"""
Tests for ClaudeLang package manager
"""

import unittest
import tempfile
import shutil
import json
from pathlib import Path
from datetime import datetime
from typing import Dict

from src.package_manager import (
    PackageManager, Manifest, Dependency, 
    LocalRegistry, PackageInfo,
    DependencyResolver, ResolutionError
)


class TestManifest(unittest.TestCase):
    """Test package manifest handling"""
    
    def test_create_manifest(self):
        """Test creating a manifest"""
        manifest = Manifest(
            name="test-package",
            version="1.0.0",
            description="A test package",
            author="Test Author"
        )
        
        self.assertEqual(manifest.name, "test-package")
        self.assertEqual(manifest.version, "1.0.0")
        self.assertEqual(manifest.license, "MIT")
    
    def test_manifest_validation(self):
        """Test manifest validation"""
        # Valid manifest
        manifest = Manifest(name="valid-pkg", version="1.0.0")
        errors = manifest.validate()
        self.assertEqual(len(errors), 0)
        
        # Invalid name
        manifest = Manifest(name="invalid name!", version="1.0.0")
        errors = manifest.validate()
        self.assertGreater(len(errors), 0)
        
        # Invalid version
        manifest = Manifest(name="valid-pkg", version="invalid")
        errors = manifest.validate()
        self.assertGreater(len(errors), 0)
    
    def test_dependency_parsing(self):
        """Test dependency parsing from dict"""
        # Simple version string
        dep = Dependency.from_dict("test-dep", "^1.0.0")
        self.assertEqual(dep.name, "test-dep")
        self.assertEqual(dep.version_spec, "^1.0.0")
        self.assertFalse(dep.dev)
        
        # Complex dependency object
        dep = Dependency.from_dict("complex-dep", {
            "version": ">=2.0.0",
            "source": "https://example.com/package.git",
            "dev": True
        })
        self.assertEqual(dep.version_spec, ">=2.0.0")
        self.assertEqual(dep.source, "https://example.com/package.git")
        self.assertTrue(dep.dev)
    
    def test_manifest_serialization(self):
        """Test manifest to/from dict conversion"""
        manifest = Manifest(
            name="test-pkg",
            version="1.2.3",
            description="Test package",
            dependencies={
                "dep1": Dependency("dep1", "^1.0.0"),
                "dep2": Dependency("dep2", "~2.1.0")
            }
        )
        
        # Convert to dict
        data = manifest.to_dict()
        self.assertEqual(data["name"], "test-pkg")
        self.assertEqual(data["version"], "1.2.3")
        self.assertIn("dependencies", data)
        
        # Convert back
        manifest2 = Manifest.from_dict(data)
        self.assertEqual(manifest2.name, manifest.name)
        self.assertEqual(manifest2.version, manifest.version)
        self.assertEqual(len(manifest2.dependencies), 2)


class TestLocalRegistry(unittest.TestCase):
    """Test local registry operations"""
    
    def setUp(self):
        """Create temporary registry directory"""
        self.temp_dir = tempfile.mkdtemp()
        self.registry = LocalRegistry(Path(self.temp_dir))
    
    def tearDown(self):
        """Clean up temporary directory"""
        shutil.rmtree(self.temp_dir)
    
    def test_publish_package(self):
        """Test publishing a package"""
        # Create a test package
        pkg_dir = Path(self.temp_dir) / "test-package"
        pkg_dir.mkdir()
        
        manifest = Manifest(
            name="test-package",
            version="1.0.0",
            description="A test package"
        )
        manifest.save(pkg_dir)
        
        # Add some content
        (pkg_dir / "main.cl").write_text("(defn main [] (println \"Hello\"))")
        
        # Publish
        success = self.registry.publish_package(pkg_dir)
        self.assertTrue(success)
        
        # Verify it's in the registry
        pkg_info = self.registry.get_package_info("test-package", "1.0.0")
        self.assertIsNotNone(pkg_info)
        self.assertEqual(pkg_info.name, "test-package")
        self.assertEqual(pkg_info.version, "1.0.0")
    
    def test_search_packages(self):
        """Test searching for packages"""
        # Publish some test packages
        for i in range(3):
            pkg_dir = Path(self.temp_dir) / f"test-pkg-{i}"
            pkg_dir.mkdir()
            
            manifest = Manifest(
                name=f"test-pkg-{i}",
                version="1.0.0",
                description=f"Test package number {i}"
            )
            manifest.save(pkg_dir)
            
            self.registry.publish_package(pkg_dir)
        
        # Search
        results = self.registry.search("test-pkg")
        self.assertEqual(len(results), 3)
        
        results = self.registry.search("number 1")
        self.assertEqual(len(results), 1)
        self.assertEqual(results[0].name, "test-pkg-1")


class TestDependencyResolver(unittest.TestCase):
    """Test dependency resolution"""
    
    def setUp(self):
        """Set up test registry with packages"""
        self.temp_dir = tempfile.mkdtemp()
        self.registry = LocalRegistry(Path(self.temp_dir))
        
        # Create some test packages with dependencies
        self._create_test_packages()
        
        self.resolver = DependencyResolver(self.registry)
    
    def tearDown(self):
        """Clean up"""
        shutil.rmtree(self.temp_dir)
    
    def _create_test_packages(self):
        """Create test packages in registry"""
        # Package A v1.0.0 (no deps)
        self._publish_package("pkg-a", "1.0.0", {})
        
        # Package A v2.0.0 (no deps)
        self._publish_package("pkg-a", "2.0.0", {})
        
        # Package B v1.0.0 (depends on A ^1.0.0)
        self._publish_package("pkg-b", "1.0.0", {
            "pkg-a": "^1.0.0"
        })
        
        # Package C v1.0.0 (depends on A ~1.0.0 and B ^1.0.0)
        self._publish_package("pkg-c", "1.0.0", {
            "pkg-a": "~1.0.0",
            "pkg-b": "^1.0.0"
        })
    
    def _publish_package(self, name: str, version: str, deps: Dict[str, str]):
        """Helper to publish a test package"""
        pkg_dir = Path(self.temp_dir) / f"{name}-{version}"
        pkg_dir.mkdir()
        
        manifest = Manifest(
            name=name,
            version=version,
            dependencies={
                dep_name: Dependency(dep_name, spec)
                for dep_name, spec in deps.items()
            }
        )
        manifest.save(pkg_dir)
        
        self.registry.publish_package(pkg_dir)
    
    def test_simple_resolution(self):
        """Test resolving simple dependencies"""
        manifest = Manifest(
            name="my-app",
            version="1.0.0",
            dependencies={
                "pkg-a": Dependency("pkg-a", "^1.0.0")
            }
        )
        
        resolved = self.resolver.resolve(manifest)
        
        self.assertEqual(len(resolved), 1)
        self.assertIn("pkg-a", resolved)
        self.assertEqual(resolved["pkg-a"].version, "1.0.0")
    
    def test_transitive_resolution(self):
        """Test resolving transitive dependencies"""
        manifest = Manifest(
            name="my-app",
            version="1.0.0",
            dependencies={
                "pkg-c": Dependency("pkg-c", "^1.0.0")
            }
        )
        
        resolved = self.resolver.resolve(manifest)
        
        # Should resolve C and its transitive deps (A and B)
        self.assertEqual(len(resolved), 1)  # Direct dependency
        
        # Check transitive deps
        all_packages = resolved["pkg-c"].flatten()
        self.assertEqual(len(all_packages), 3)  # C, B, and A
        self.assertIn("pkg-a", all_packages)
        self.assertIn("pkg-b", all_packages)
    
    def test_version_conflict_detection(self):
        """Test detection of version conflicts"""
        # Create a conflict: D requires A ^2.0.0, but C requires A ~1.0.0
        self._publish_package("pkg-d", "1.0.0", {
            "pkg-a": "^2.0.0"
        })
        
        manifest = Manifest(
            name="my-app",
            version="1.0.0",
            dependencies={
                "pkg-c": Dependency("pkg-c", "^1.0.0"),  # wants A ~1.0.0
                "pkg-d": Dependency("pkg-d", "^1.0.0")   # wants A ^2.0.0
            }
        )
        
        # Should raise ResolutionError due to conflict
        with self.assertRaises(ResolutionError) as ctx:
            self.resolver.resolve(manifest)
        
        self.assertIn("conflict", str(ctx.exception).lower())


class TestPackageManagerIntegration(unittest.TestCase):
    """Integration tests for package manager"""
    
    def setUp(self):
        """Set up test environment"""
        self.temp_dir = tempfile.mkdtemp()
        self.original_cwd = Path.cwd()
        
        # Create test project directory
        self.project_dir = Path(self.temp_dir) / "test-project"
        self.project_dir.mkdir()
        
        # Change to project directory
        import os
        os.chdir(self.project_dir)
        
        # Create package manager with local registry
        self.pm = PackageManager()
        self.pm.registry = LocalRegistry(Path(self.temp_dir) / "registry")
    
    def tearDown(self):
        """Clean up"""
        import os
        os.chdir(self.original_cwd)
        shutil.rmtree(self.temp_dir)
    
    def test_init_project(self):
        """Test initializing a new project"""
        manifest = self.pm.init(
            name="my-project",
            version="0.1.0",
            description="Test project"
        )
        
        self.assertEqual(manifest.name, "my-project")
        self.assertTrue((self.project_dir / "claude.json").exists())
        self.assertTrue((self.project_dir / "src").exists())
        self.assertTrue((self.project_dir / ".gitignore").exists())
    
    def test_install_save_workflow(self):
        """Test installing and saving dependencies"""
        # Init project
        self.pm.init(name="test-app")
        
        # Create and publish a test package
        pkg_dir = Path(self.temp_dir) / "test-lib"
        pkg_dir.mkdir()
        lib_manifest = Manifest(name="test-lib", version="1.0.0")
        lib_manifest.save(pkg_dir)
        self.pm.registry.publish_package(pkg_dir)
        
        # Install it
        self.pm.install("test-lib", save=True)
        
        # Check it was added to manifest
        manifest = Manifest.load(self.project_dir)
        self.assertIn("test-lib", manifest.dependencies)
        
        # Check it was installed
        modules_dir = self.project_dir / "claude_modules" / "test-lib" / "1.0.0"
        self.assertTrue(modules_dir.exists())


if __name__ == "__main__":
    unittest.main()