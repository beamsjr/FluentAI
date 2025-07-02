"""
ClaudeLang Package Manager

A package manager for distributing and managing ClaudeLang packages.
"""

from .package_manager import PackageManager
from .registry import Registry, LocalRegistry, RemoteRegistry, PackageInfo
from .manifest import Manifest, Dependency
from .resolver import DependencyResolver, ResolutionError
from .installer import PackageInstaller

__all__ = [
    'PackageManager',
    'Registry',
    'LocalRegistry',
    'RemoteRegistry',
    'PackageInfo',
    'Manifest',
    'Dependency',
    'DependencyResolver',
    'ResolutionError',
    'PackageInstaller'
]