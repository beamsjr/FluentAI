# claudelang-package

Package manager for ClaudeLang, providing dependency management, versioning, and package distribution.

## Features

- **Dependency Management**: Resolve and install package dependencies
- **Semantic Versioning**: Support for npm-style version ranges
- **Lock Files**: Ensure reproducible builds
- **Registry Support**: Local and HTTP-based package registries
- **Package Publishing**: Share packages with the community
- **CLI Interface**: User-friendly command-line tool

## Installation

```bash
cargo install claudelang-package
```

## Usage

### Initialize a Package

```bash
claudelang-package init
```

Creates a `claude.json` manifest file.

### Install Dependencies

```bash
# Install all dependencies
claudelang-package install

# Add a dependency
claudelang-package install math-utils

# Add a dev dependency
claudelang-package install --dev test-framework
```

### Publish a Package

```bash
claudelang-package publish
```

### Search for Packages

```bash
claudelang-package search math
```

## Manifest Format

`claude.json`:

```json
{
  "name": "my-package",
  "version": "1.0.0",
  "description": "My awesome package",
  "author": "Your Name <you@example.com>",
  "license": "MIT",
  "dependencies": {
    "math-utils": "^2.0.0",
    "string-helpers": "~1.5.0"
  },
  "devDependencies": {
    "test-framework": "^3.0.0"
  }
}
```

## Version Specifications

- **Exact**: `1.2.3`
- **Range**: `>=1.0.0 <2.0.0`
- **Caret**: `^1.2.3` (compatible)
- **Tilde**: `~1.2.3` (approximately)
- **Latest**: `*` or `latest`

## Architecture

### Core Components

- **Manifest**: Package metadata and dependencies
- **DependencyResolver**: Resolves version constraints
- **Registry**: Package storage and retrieval
- **Installer**: Downloads and extracts packages
- **Lockfile**: Reproducible dependency tree

### Dependency Resolution

1. Parse manifest dependencies
2. Query registry for available versions
3. Resolve version constraints
4. Build dependency graph
5. Check for conflicts and cycles
6. Generate lock file

## Registry API

### LocalRegistry

```rust
use claudelang_package::registry::LocalRegistry;

let registry = LocalRegistry::new(PathBuf::from("./registry"));
```

### HttpRegistry

```rust
use claudelang_package::registry::HttpRegistry;

let registry = HttpRegistry::new("https://registry.claudelang.org")?;
```

## Configuration

```rust
pub struct PackageConfig {
    /// Default registry URL
    pub registry: String,
    
    /// Alternative registries
    pub registries: Vec<RegistryConfig>,
    
    /// Global packages directory
    pub global_dir: PathBuf,
    
    /// Cache directory
    pub cache_dir: PathBuf,
    
    /// HTTP timeout in seconds
    pub timeout: u64,
}
```

## Error Handling

```rust
use claudelang_package::PackageError;

match installer.install(&manifest) {
    Err(PackageError::PackageNotFound { name, version }) => {
        println!("Package {}@{} not found", name, version);
    }
    Err(PackageError::VersionConflict { package, required, found }) => {
        println!("Version conflict in {}: required {}, found {}", 
                 package, required, found);
    }
    _ => {}
}
```

## Testing

```bash
cargo test -p claudelang-package
```

## CLI Commands

- `init`: Initialize a new package
- `install`: Install dependencies
- `publish`: Publish to registry
- `search`: Search packages
- `info`: Show package details
- `config`: Manage configuration

## License

Part of the ClaudeLang project.