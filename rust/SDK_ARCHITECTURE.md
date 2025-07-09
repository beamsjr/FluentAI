# FluentAI SDK Architecture

FluentAI compiles to native executables with an optional core library for advanced features.

## 1. FluentAI SDK (Development Kit)
Complete development environment for building FluentAI applications.

### Components:
- **FluentAI CLI** (`fluentai` command)
  - `fluentai new` - Create new projects from templates
  - `fluentai build` - Compile .ai files to native executables
  - `fluentai run` - Build and run applications
  - `fluentai test` - Run tests
  - `fluentai publish` - Package for distribution
  - `fluentai add package` - Add dependencies
  - `fluentai restore` - Restore dependencies

- **Project System**
  - Project file format (`.aiproj`)
  - Templates (console app, library, web service)
  - Package management

- **Build System**
  - AOT (Ahead-of-Time) compilation to native code
  - Optimization levels (O0-O3)
  - Target platforms (x86_64, ARM64, WASM)
  - Debug/Release configurations
  - Static linking of core library

- **Development Tools**
  - REPL (interpreter mode for development)
  - Debugger integration
  - Profiler
  - Language server (LSP)

## 2. FluentAI Core Library (Statically Linked)
Core functionality that gets compiled into FluentAI applications.

### Components:
- **Core Functions**
  - Effect handlers
  - Standard library functions
  - Memory management (optional GC)
  - Module loading (for dynamic imports)

- **No Separate Runtime Required**
  - Applications are self-contained native executables
  - Core library is statically linked during compilation
  - Zero runtime dependencies for basic applications

## 3. Distribution Model

### SDK Installation
```bash
# Install FluentAI SDK (includes runtime)
curl -sSL https://fluentai.dev/install.sh | sh

# Or via package managers
brew install fluentai-sdk        # macOS
apt install fluentai-sdk         # Ubuntu  
choco install fluentai-sdk       # Windows
```

### Runtime-Only Installation
```bash
# For deployment machines
apt install fluentai-runtime
```

## 4. Project Structure

### Console Application
```
MyApp/
├── MyApp.aiproj          # Project file
├── Program.ai            # Entry point
├── src/
│   └── Utils.ai         # Source files
├── tests/
│   └── Utils.test.ai    # Tests
└── packages.lock        # Lock file
```

### Project File (MyApp.aiproj)
```xml
<Project Sdk="FluentAI.Sdk/1.0">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>fluentai1.0</TargetFramework>
    <RootNamespace>MyApp</RootNamespace>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="FluentAI.Http" Version="1.0.0" />
    <PackageReference Include="FluentAI.Json" Version="1.0.0" />
  </ItemGroup>
</Project>
```

## 5. Compilation Modes

### 1. JIT (Default for Development)
```bash
fluentai run MyApp.ai
# Compiles to bytecode, runs with JIT
```

### 2. AOT (For Production)
```bash
fluentai publish -c Release --self-contained
# Produces native executable with embedded runtime
```

### 3. Library
```bash
fluentai build --target library
# Produces .ailib file for use by other projects
```

## 6. Package Management

### Package Format
- `.aipkg` files (zip archives)
- Contains compiled bytecode + metadata
- Can include native dependencies

### Package Repository
- Central repository at packages.fluentai.dev
- Support for private repositories
- Version resolution and dependency management

## 7. Development Workflow

```bash
# Create new console app
fluentai new console -n MyApp
cd MyApp

# Add dependencies
fluentai add package FluentAI.Http

# Run in development
fluentai run

# Run tests
fluentai test

# Build for release
fluentai build -c Release

# Publish self-contained app
fluentai publish -c Release --self-contained -r linux-x64
```

This architecture provides a familiar, productive development experience while maintaining the flexibility to deploy lightweight runtime-only environments.