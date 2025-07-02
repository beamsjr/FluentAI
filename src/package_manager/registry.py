"""
Package registry interface for ClaudeLang

Supports both local and remote registries.
"""

import json
import os
import shutil
import tempfile
import tarfile
import hashlib
from abc import ABC, abstractmethod
from typing import Dict, List, Optional, Tuple, Any
from pathlib import Path
from datetime import datetime
from dataclasses import dataclass

from .manifest import Manifest, Dependency

# Make requests optional for remote registry
try:
    import requests
    HAS_REQUESTS = True
except ImportError:
    HAS_REQUESTS = False


@dataclass
class PackageInfo:
    """Information about a package in the registry"""
    name: str
    version: str
    manifest: Manifest
    tarball_url: Optional[str] = None
    checksum: Optional[str] = None
    published_at: Optional[datetime] = None
    downloads: int = 0
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        return {
            "name": self.name,
            "version": self.version,
            "manifest": self.manifest.to_dict(),
            "tarball": self.tarball_url,
            "checksum": self.checksum,
            "publishedAt": self.published_at.isoformat() if self.published_at else None,
            "downloads": self.downloads
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'PackageInfo':
        """Create from dictionary"""
        return cls(
            name=data["name"],
            version=data["version"],
            manifest=Manifest.from_dict(data["manifest"]),
            tarball_url=data.get("tarball"),
            checksum=data.get("checksum"),
            published_at=datetime.fromisoformat(data["publishedAt"]) if data.get("publishedAt") else None,
            downloads=data.get("downloads", 0)
        )


class Registry(ABC):
    """Abstract base class for package registries"""
    
    @abstractmethod
    def search(self, query: str, limit: int = 20) -> List[PackageInfo]:
        """Search for packages"""
        pass
    
    @abstractmethod
    def get_package(self, name: str) -> Optional[Dict[str, PackageInfo]]:
        """Get all versions of a package"""
        pass
    
    @abstractmethod
    def get_package_info(self, name: str, version: str) -> Optional[PackageInfo]:
        """Get specific version of a package"""
        pass
    
    @abstractmethod
    def download_package(self, name: str, version: str, dest: Path) -> Path:
        """Download package tarball"""
        pass
    
    @abstractmethod
    def publish_package(self, package_path: Path, api_key: Optional[str] = None) -> bool:
        """Publish a package to the registry"""
        pass


class LocalRegistry(Registry):
    """Local file-based registry for development"""
    
    def __init__(self, path: Path):
        self.path = Path(path)
        self.path.mkdir(parents=True, exist_ok=True)
        self.index_path = self.path / "index.json"
        self.packages_path = self.path / "packages"
        self.packages_path.mkdir(exist_ok=True)
        
        # Load or create index
        self.index = self._load_index()
    
    def _load_index(self) -> Dict[str, Dict[str, PackageInfo]]:
        """Load the package index"""
        if self.index_path.exists():
            with open(self.index_path, 'r') as f:
                data = json.load(f)
                
            index = {}
            for pkg_name, versions in data.items():
                index[pkg_name] = {}
                for version, info in versions.items():
                    index[pkg_name][version] = PackageInfo.from_dict(info)
            return index
        return {}
    
    def _save_index(self) -> None:
        """Save the package index"""
        data = {}
        for pkg_name, versions in self.index.items():
            data[pkg_name] = {}
            for version, info in versions.items():
                data[pkg_name][version] = info.to_dict()
        
        with open(self.index_path, 'w') as f:
            json.dump(data, f, indent=2)
    
    def search(self, query: str, limit: int = 20) -> List[PackageInfo]:
        """Search for packages by name or description"""
        results = []
        query_lower = query.lower()
        
        for pkg_name, versions in self.index.items():
            if query_lower in pkg_name.lower():
                # Get latest version
                latest = max(versions.values(), key=lambda p: p.published_at or datetime.min)
                results.append(latest)
            else:
                # Check description
                for info in versions.values():
                    if query_lower in info.manifest.description.lower():
                        latest = max(versions.values(), key=lambda p: p.published_at or datetime.min)
                        results.append(latest)
                        break
        
        return results[:limit]
    
    def get_package(self, name: str) -> Optional[Dict[str, PackageInfo]]:
        """Get all versions of a package"""
        return self.index.get(name)
    
    def get_package_info(self, name: str, version: str) -> Optional[PackageInfo]:
        """Get specific version of a package"""
        pkg = self.get_package(name)
        if pkg:
            return pkg.get(version)
        return None
    
    def download_package(self, name: str, version: str, dest: Path) -> Path:
        """Download (copy) package tarball"""
        info = self.get_package_info(name, version)
        if not info:
            raise ValueError(f"Package {name}@{version} not found")
        
        tarball_name = f"{name}-{version}.tar.gz"
        src = self.packages_path / tarball_name
        
        if not src.exists():
            raise FileNotFoundError(f"Tarball not found: {src}")
        
        dest_file = dest / tarball_name
        shutil.copy2(src, dest_file)
        
        return dest_file
    
    def publish_package(self, package_path: Path, api_key: Optional[str] = None) -> bool:
        """Publish a package to the local registry"""
        # Load manifest
        manifest = Manifest.load(package_path)
        
        # Validate
        errors = manifest.validate()
        if errors:
            raise ValueError(f"Invalid manifest: {', '.join(errors)}")
        
        # Create tarball
        tarball_name = f"{manifest.name}-{manifest.version}.tar.gz"
        tarball_path = self.packages_path / tarball_name
        
        with tarfile.open(tarball_path, "w:gz") as tar:
            for item in package_path.iterdir():
                if item.name not in [".git", "node_modules", "claude_modules", "__pycache__"]:
                    tar.add(item, arcname=item.name)
        
        # Calculate checksum
        with open(tarball_path, 'rb') as f:
            checksum = hashlib.sha256(f.read()).hexdigest()
        
        # Create package info
        info = PackageInfo(
            name=manifest.name,
            version=manifest.version,
            manifest=manifest,
            tarball_url=f"file://{tarball_path}",
            checksum=checksum,
            published_at=datetime.now()
        )
        
        # Update index
        if manifest.name not in self.index:
            self.index[manifest.name] = {}
        
        self.index[manifest.name][manifest.version] = info
        self._save_index()
        
        return True


class RemoteRegistry(Registry):
    """Remote HTTP-based registry"""
    
    def __init__(self, url: str, api_key: Optional[str] = None):
        if not HAS_REQUESTS:
            raise ImportError("requests library required for remote registry support")
            
        self.url = url.rstrip('/')
        self.api_key = api_key
        self.session = requests.Session()
        if api_key:
            self.session.headers["Authorization"] = f"Bearer {api_key}"
    
    def search(self, query: str, limit: int = 20) -> List[PackageInfo]:
        """Search for packages"""
        response = self.session.get(
            f"{self.url}/api/search",
            params={"q": query, "limit": limit}
        )
        response.raise_for_status()
        
        results = []
        for item in response.json()["results"]:
            results.append(PackageInfo.from_dict(item))
        
        return results
    
    def get_package(self, name: str) -> Optional[Dict[str, PackageInfo]]:
        """Get all versions of a package"""
        response = self.session.get(f"{self.url}/api/packages/{name}")
        
        if response.status_code == 404:
            return None
        
        response.raise_for_status()
        
        versions = {}
        for version, data in response.json()["versions"].items():
            versions[version] = PackageInfo.from_dict(data)
        
        return versions
    
    def get_package_info(self, name: str, version: str) -> Optional[PackageInfo]:
        """Get specific version of a package"""
        response = self.session.get(f"{self.url}/api/packages/{name}/{version}")
        
        if response.status_code == 404:
            return None
        
        response.raise_for_status()
        
        return PackageInfo.from_dict(response.json())
    
    def download_package(self, name: str, version: str, dest: Path) -> Path:
        """Download package tarball"""
        info = self.get_package_info(name, version)
        if not info or not info.tarball_url:
            raise ValueError(f"Package {name}@{version} not found")
        
        # Download tarball
        response = self.session.get(info.tarball_url, stream=True)
        response.raise_for_status()
        
        tarball_name = f"{name}-{version}.tar.gz"
        dest_file = dest / tarball_name
        
        with open(dest_file, 'wb') as f:
            for chunk in response.iter_content(chunk_size=8192):
                f.write(chunk)
        
        # Verify checksum if provided
        if info.checksum:
            with open(dest_file, 'rb') as f:
                actual_checksum = hashlib.sha256(f.read()).hexdigest()
            
            if actual_checksum != info.checksum:
                dest_file.unlink()
                raise ValueError(f"Checksum mismatch for {name}@{version}")
        
        return dest_file
    
    def publish_package(self, package_path: Path, api_key: Optional[str] = None) -> bool:
        """Publish a package to the remote registry"""
        # Use provided API key or instance key
        key = api_key or self.api_key
        if not key:
            raise ValueError("API key required for publishing")
        
        # Create tarball
        with tempfile.NamedTemporaryFile(suffix='.tar.gz', delete=False) as tmp:
            with tarfile.open(tmp.name, "w:gz") as tar:
                for item in package_path.iterdir():
                    if item.name not in [".git", "node_modules", "claude_modules", "__pycache__"]:
                        tar.add(item, arcname=item.name)
            
            tmp_path = Path(tmp.name)
        
        try:
            # Upload
            with open(tmp_path, 'rb') as f:
                files = {'package': (tmp_path.name, f, 'application/gzip')}
                headers = {"Authorization": f"Bearer {key}"}
                
                response = requests.post(
                    f"{self.url}/api/publish",
                    files=files,
                    headers=headers
                )
            
            response.raise_for_status()
            return True
            
        finally:
            tmp_path.unlink()


def create_registry(config: Dict[str, Any]) -> Registry:
    """Create a registry from configuration"""
    registry_type = config.get("type", "local")
    
    if registry_type == "local":
        return LocalRegistry(Path(config.get("path", "~/.claude/registry")).expanduser())
    elif registry_type == "remote":
        return RemoteRegistry(
            url=config["url"],
            api_key=config.get("apiKey")
        )
    else:
        raise ValueError(f"Unknown registry type: {registry_type}")