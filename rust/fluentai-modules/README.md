# fluentai-modules

Module system implementation for FluentAi, providing code organization, encapsulation, and dependency management.

## Features

- **Module Loading**: File-based module loading with configurable search paths
- **Import/Export**: Control visibility with explicit exports and flexible import syntax
- **Caching**: LRU cache for loaded modules with configurable size limits
- **Circular Dependency Detection**: Prevents circular module dependencies
- **Module Environments**: Isolated environments for each module
- **Qualified Names**: Support for module.variable syntax

## Usage

```rust
use fluentai_modules::{ModuleLoader, ModuleConfig};

// Configure module system
let config = ModuleConfig {
    search_paths: vec![PathBuf::from("./lib")],
    enable_cache: true,
    max_cache_size: 100,
    allow_circular: false,
};

// Create module loader
let mut loader = ModuleLoader::new(config);

// Load a module
let module = loader.load_module("math").unwrap();
```

## Module Syntax

### Defining a Module

```flc
mod math {
  public const pi = 3.14159;
  
  public function add(a, b) {
    a + b
  }
  
  public function multiply(a, b) {
    a * b
  }
  
  private function internal() {
    "private"  // Not exported
  }
}
```

### Importing from Modules

```flc
// Import specific symbols
use math::{add, multiply};

// Import with aliases  
// TODO: Aliasing not yet implemented
// use math::{pi as PI};

// Import all exports
// TODO: Import all not yet implemented
// use math::*;

// Qualified access
math::add(2, 3);
```

## Architecture

### Core Components

- **ModuleLoader**: Handles finding and loading modules from disk
- **ModuleResolver**: Resolves imports and builds dependency graphs
- **ModuleCache**: LRU cache for loaded modules
- **ModuleEnvironment**: Manages module-level bindings and imports
- **DependencyGraph**: Tracks module dependencies and detects cycles

### Module Resolution

1. Check if module is already cached
2. Search for module file in configured paths
3. Parse module file
4. Extract exports and dependencies
5. Cache the loaded module
6. Resolve imports recursively

## Error Handling

```rust
use fluentai_modules::ModuleError;

match loader.load_module("missing") {
    Err(ModuleError::ModuleNotFound { path }) => {
        println!("Module not found: {}", path);
    }
    Err(ModuleError::CircularDependency { cycle }) => {
        println!("Circular dependency: {}", cycle);
    }
    Err(ModuleError::ExportNotFound { name, module }) => {
        println!("Export '{}' not found in module '{}'", name, module);
    }
    _ => {}
}
```

## Configuration

```rust
pub struct ModuleConfig {
    /// Directories to search for modules
    pub search_paths: Vec<PathBuf>,
    
    /// Enable module caching
    pub enable_cache: bool,
    
    /// Maximum number of cached modules
    pub max_cache_size: usize,
    
    /// Allow circular dependencies (not recommended)
    pub allow_circular: bool,
}
```

## Testing

```bash
cargo test -p fluentai-modules
```

## License

Part of the FluentAi project.