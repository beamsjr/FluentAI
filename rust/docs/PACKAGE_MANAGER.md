# ClaudeLang Package Manager

The ClaudeLang package manager provides dependency management, versioning, and distribution capabilities for ClaudeLang projects.

## Table of Contents
- [Overview](#overview)
- [Getting Started](#getting-started)
- [Package Manifest](#package-manifest)
- [Commands](#commands)
- [Dependency Management](#dependency-management)
- [Publishing Packages](#publishing-packages)
- [Registry Configuration](#registry-configuration)
- [Best Practices](#best-practices)

## Overview

The package manager allows you to:
- Manage project dependencies
- Publish and share packages
- Version your code with semantic versioning
- Ensure reproducible builds with lock files
- Search and discover packages

## Getting Started

### Initialize a New Package

```bash
claudelang-package init
```

This creates a `claude.json` manifest file in your project.

### Install Dependencies

```bash
claudelang-package install
```

Installs all dependencies listed in `claude.json`.

## Package Manifest

The `claude.json` file describes your package:

```json
{
  "name": "my-awesome-lib",
  "version": "1.0.0",
  "description": "An awesome ClaudeLang library",
  "author": "Your Name <you@example.com>",
  "license": "MIT",
  "main": "src/main.cl",
  "dependencies": {
    "math-utils": "^2.0.0",
    "string-helpers": "~1.5.0"
  },
  "devDependencies": {
    "test-framework": "^3.0.0"
  },
  "scripts": {
    "test": "claudelang test/*.cl",
    "build": "claudelang compile src/"
  },
  "repository": {
    "type": "git",
    "url": "https://github.com/username/my-awesome-lib"
  },
  "keywords": ["math", "utility", "helpers"]
}
```

### Manifest Fields

- **name** (required): Package name (lowercase, hyphens allowed)
- **version** (required): Semantic version (major.minor.patch)
- **description**: Brief description of the package
- **author**: Author information
- **license**: License identifier (MIT, Apache-2.0, etc.)
- **main**: Entry point module
- **dependencies**: Runtime dependencies
- **devDependencies**: Development-only dependencies
- **scripts**: Custom commands
- **repository**: Source code location
- **keywords**: For package discovery

## Commands

### init

Initialize a new package:

```bash
claudelang-package init [--name <name>] [--version <version>]
```

### install

Install dependencies:

```bash
# Install all dependencies
claudelang-package install

# Install and add to dependencies
claudelang-package install math-utils

# Install and add to devDependencies
claudelang-package install --dev test-framework

# Install specific version
claudelang-package install math-utils@2.1.0
```

### publish

Publish package to registry:

```bash
claudelang-package publish [--registry <url>]
```

### search

Search for packages:

```bash
claudelang-package search math
claudelang-package search --author "Jane Doe"
```

## Dependency Management

### Version Specifications

The package manager supports npm-style version ranges:

- **Exact**: `"1.2.3"` - Exactly version 1.2.3
- **Range**: `">=1.0.0 <2.0.0"` - Any 1.x version
- **Caret**: `"^1.2.3"` - Compatible with 1.2.3 (>=1.2.3 <2.0.0)
- **Tilde**: `"~1.2.3"` - Approximately 1.2.3 (>=1.2.3 <1.3.0)
- **Wildcard**: `"*"` or `"latest"` - Any version

### Lock File

The `claude-lock.json` file ensures reproducible installations:

```json
{
  "version": 1,
  "generatedAt": "2024-01-15T10:30:00Z",
  "packages": {
    "math-utils@2.1.0": {
      "name": "math-utils",
      "version": "2.1.0",
      "registry": "https://registry.claudelang.org",
      "checksum": "sha256:abc123...",
      "dependencies": {
        "base-math": "1.0.0"
      }
    }
  }
}
```

### Dependency Resolution

The resolver uses a deterministic algorithm:
1. Builds dependency graph
2. Resolves version constraints
3. Detects conflicts and circular dependencies
4. Produces minimal set of packages

## Publishing Packages

### Pre-publish Checklist

1. **Version your package**: Update version in `claude.json`
2. **Test thoroughly**: Run all tests
3. **Update documentation**: README, API docs
4. **Check license**: Ensure proper licensing
5. **Verify manifest**: All fields correct

### Publishing Process

```bash
# Login to registry (if required)
claudelang-package login

# Publish
claudelang-package publish

# Publish with specific tag
claudelang-package publish --tag beta
```

### Package Structure

Recommended structure:

```
my-package/
├── claude.json          # Package manifest
├── claude-lock.json     # Lock file (don't publish)
├── README.md           # Documentation
├── LICENSE             # License file
├── src/                # Source code
│   ├── main.cl        # Entry point
│   └── lib/           # Library modules
├── test/              # Tests
├── examples/          # Usage examples
└── docs/              # Additional docs
```

## Registry Configuration

### Default Registry

The default registry is configured in the package manager:

```toml
# ~/.claudelang/config.toml
[package]
registry = "https://registry.claudelang.org"
```

### Custom Registries

Add custom registries:

```bash
claudelang-package config add-registry company https://registry.company.com
```

Use in manifest:

```json
{
  "dependencies": {
    "internal-lib": {
      "version": "1.0.0",
      "registry": "https://registry.company.com"
    }
  }
}
```

### Local Registry

For development or private packages:

```bash
# Set up local registry
claudelang-package config set registry file:///path/to/local/registry

# Publish to local registry
claudelang-package publish --registry file:///path/to/local/registry
```

## Package Discovery

### Searching Packages

```bash
# Search by name
claudelang-package search json

# Search by keyword
claudelang-package search --keyword parsing

# Search by author
claudelang-package search --author "John Doe"

# Combined search
claudelang-package search --keyword math --keyword statistics
```

### Package Information

```bash
# Show package details
claudelang-package info math-utils

# Show specific version
claudelang-package info math-utils@2.0.0

# Show versions
claudelang-package versions math-utils
```

## Best Practices

### 1. Semantic Versioning

Follow [semver](https://semver.org/):
- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes

```bash
# Bump version
claudelang-package version patch  # 1.0.0 -> 1.0.1
claudelang-package version minor  # 1.0.1 -> 1.1.0
claudelang-package version major  # 1.1.0 -> 2.0.0
```

### 2. Minimal Dependencies

Only include necessary dependencies:

```json
{
  "dependencies": {
    "truly-needed": "^1.0.0"
  },
  "devDependencies": {
    "test-only": "^2.0.0"
  }
}
```

### 3. Clear Package Names

Use descriptive, unique names:

```
Good:
- claudelang-http-client
- math-matrix-operations
- json-parser

Avoid:
- utils
- helpers
- lib
```

### 4. Comprehensive Documentation

Include:
- README with usage examples
- API documentation
- CHANGELOG
- Contributing guidelines

### 5. Proper Licensing

Always specify a license:

```json
{
  "license": "MIT",
  "licenses": [
    {
      "type": "MIT",
      "url": "https://opensource.org/licenses/MIT"
    }
  ]
}
```

## Security Considerations

### Checksum Verification

All packages include SHA-256 checksums:

```json
{
  "packages": {
    "some-package@1.0.0": {
      "checksum": "sha256:1234567890abcdef..."
    }
  }
}
```

### Audit Dependencies

Regularly audit for vulnerabilities:

```bash
claudelang-package audit
```

### Registry Authentication

For private registries:

```bash
# Login
claudelang-package login --registry https://private.registry.com

# Logout
claudelang-package logout --registry https://private.registry.com
```

## Troubleshooting

### Common Issues

**Dependency conflicts:**
```bash
# Clear cache and reinstall
claudelang-package cache clean
claudelang-package install --force
```

**Network issues:**
```bash
# Use offline mode
claudelang-package install --offline

# Configure proxy
export HTTPS_PROXY=http://proxy.company.com:8080
```

**Corrupted downloads:**
```bash
# Verify checksums
claudelang-package verify

# Re-download
claudelang-package install --force
```

## Advanced Usage

### Workspaces

Manage multiple packages:

```json
{
  "workspaces": [
    "packages/*",
    "libs/*"
  ]
}
```

### Local Development

Link local packages:

```bash
# In package directory
claudelang-package link

# In consuming project
claudelang-package link my-local-package
```

### Custom Scripts

Define custom commands:

```json
{
  "scripts": {
    "test": "claudelang test/*.cl",
    "build": "claudelang compile src/",
    "fmt": "claudelang fmt src/**/*.cl"
  }
}
```

Run with:
```bash
claudelang-package run test
```

## Integration with Build Tools

### Continuous Integration

```yaml
# .github/workflows/ci.yml
- name: Install dependencies
  run: claudelang-package install
  
- name: Run tests
  run: claudelang-package run test
  
- name: Build
  run: claudelang-package run build
```

### Docker

```dockerfile
FROM claudelang:latest
WORKDIR /app
COPY claude.json claude-lock.json ./
RUN claudelang-package install --production
COPY . .
CMD ["claudelang", "src/main.cl"]
```