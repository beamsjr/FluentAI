# FluentAI SDK User Guide

## Installation

### Install the SDK (includes everything needed for development)
```bash
# macOS/Linux
curl -sSL https://get.fluentai.dev | sh

# Windows
iwr -useb https://get.fluentai.dev/install.ps1 | iex

# Or via package managers
brew install fluentai         # macOS
apt install fluentai-sdk      # Ubuntu/Debian
choco install fluentai        # Windows
```

### Install Runtime Only (for running compiled applications)
```bash
# For deployment servers that only need to run FluentAI apps
curl -sSL https://get.fluentai.dev/runtime | sh
```

## Creating Your First Application

### 1. Create a new console application
```bash
fluentai new console MyApp
cd MyApp
```

This creates:
```
MyApp/
├── MyApp.aiproj          # Project file
├── Program.ai            # Entry point
├── src/
│   └── Utils.ai          # Sample module
├── tests/
│   └── Utils.test.ai     # Sample test
├── .gitignore
└── README.md
```

### 2. Restore dependencies
```bash
fluentai restore
```

### 3. Run the application
```bash
fluentai run
```

## Common Commands

### Development Workflow
```bash
# Create new projects
fluentai new console MyApp        # Console application
fluentai new library MyLib        # Reusable library
fluentai new webservice MyApi     # Web service

# Build
fluentai build                    # Debug build
fluentai build -c Release         # Release build

# Run
fluentai run                      # Run main program
fluentai run Program.ai           # Run specific file
fluentai run -- arg1 arg2         # Pass arguments

# Test
fluentai test                     # Run all tests
fluentai test --filter Utils      # Run specific tests
fluentai test --coverage          # Generate coverage report

# REPL
fluentai repl                     # Start interactive REPL
```

### Package Management
```bash
# Add packages
fluentai add package FluentAI.Http
fluentai add package FluentAI.Json --version 2.0.0

# Restore packages
fluentai restore

# List packages
fluentai package list
fluentai package list --tree      # Show dependency tree

# Search packages
fluentai package search http
```

### Publishing and Deployment
```bash
# Publish for production
fluentai publish -c Release --self-contained
fluentai publish -c Release --self-contained -r linux-x64
fluentai publish -c Release --self-contained --single-file

# Framework-dependent deployment (smaller, requires runtime)
fluentai publish -c Release

# Platform targets
fluentai publish -r win-x64       # Windows 64-bit
fluentai publish -r linux-x64     # Linux 64-bit
fluentai publish -r osx-x64       # macOS Intel
fluentai publish -r osx-arm64     # macOS Apple Silicon
fluentai publish -r wasm          # WebAssembly
```

## Project File Reference

### Console Application (.aiproj)
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

### Library Project
```xml
<Project Sdk="FluentAI.Sdk/1.0">
  <PropertyGroup>
    <OutputType>Library</OutputType>
    <TargetFramework>fluentai1.0</TargetFramework>
    <GeneratePackageOnBuild>true</GeneratePackageOnBuild>
    <PackageId>MyCompany.MyLibrary</PackageId>
    <Version>1.0.0</Version>
    <Authors>Your Name</Authors>
  </PropertyGroup>
</Project>
```

## Code Examples

### Hello World (Program.ai)
```scheme
;; Entry point
(define main (args)
  (println "Hello, FluentAI!")
  0) ;; Exit code

;; Run main if this is the entry module
(when (= __name__ "__main__")
  (exit (main (command-line-args))))
```

### Web Service
```scheme
(import "fluentai/http" :as http)
(import "fluentai/json" :as json)

(define routes
  [(http/get "/" home-handler)
   (http/get "/api/users" list-users)
   (http/post "/api/users" create-user)])

(define home-handler (req)
  (http/html-response "<h1>Welcome to FluentAI!</h1>"))

(define list-users (req)
  (http/json-response 
    [{:id 1 :name "Alice"}
     {:id 2 :name "Bob"}]))

(define main (args)
  (let ([port (or (env "PORT") 8080)])
    (println (format "Starting server on port {}" port))
    (http/serve routes :port port)))
```

### Unit Testing
```scheme
(import "../src/Utils.ai" :as utils)
(import "fluentai/test" :as test)

(test/describe "Utils"
  (test/it "calculates correctly"
    (test/expect (utils/add 2 3) :to-equal 5))
  
  (test/it "handles edge cases"
    (test/expect (utils/add 0 0) :to-equal 0)))
```

## Configuration

### Global Settings (~/.fluentai/settings.json)
```json
{
  "defaultFramework": "fluentai1.0",
  "packageSources": [
    "https://packages.fluentai.dev/v1"
  ],
  "telemetry": {
    "enabled": false
  }
}
```

### Environment Variables
```bash
FLUENTAI_HOME          # SDK installation directory
FLUENTAI_PACKAGES      # Global packages directory
FLUENTAI_RUNTIME_VERSION # Default runtime version
```

## Troubleshooting

### Check SDK version
```bash
fluentai --version
```

### Update SDK
```bash
fluentai sdk update
```

### Clear package cache
```bash
fluentai package clean
```

### Verbose output
```bash
fluentai build -v
fluentai run -v
```

## Advanced Features

### Cross-compilation
```bash
# Build for different platforms from any OS
fluentai publish -r linux-arm64 --self-contained
fluentai publish -r win-arm64 --self-contained
```

### Docker Support
```dockerfile
# Multi-stage build
FROM fluentai/sdk:1.0 AS build
WORKDIR /app
COPY . .
RUN fluentai publish -c Release

FROM fluentai/runtime:1.0
WORKDIR /app
COPY --from=build /app/publish .
ENTRYPOINT ["./MyApp"]
```

### CI/CD Integration
```yaml
# GitHub Actions
- name: Setup FluentAI
  uses: fluentai/setup-fluentai@v1
  with:
    sdk-version: '1.0.x'

- name: Build
  run: fluentai build -c Release

- name: Test
  run: fluentai test

- name: Publish
  run: fluentai publish -c Release --self-contained
```

## Migration from Other Languages

### From .NET
- Project files are similar to .csproj
- `fluentai` CLI works like `dotnet` CLI
- Package management via NuGet-like system

### From Node.js
- `packages.lock` similar to `package-lock.json`
- `fluentai run` like `npm start`
- `fluentai test` like `npm test`

### From Python
- Virtual environments not needed (built-in isolation)
- `fluentai add package` like `pip install`
- REPL available with `fluentai repl`

## Learn More

- Official Documentation: https://docs.fluentai.dev
- Package Repository: https://packages.fluentai.dev
- Community Forum: https://forum.fluentai.dev
- GitHub: https://github.com/fluentai/fluentai