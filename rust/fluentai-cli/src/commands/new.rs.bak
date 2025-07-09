//! Create new FluentAI projects from templates

use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};
use colored::*;

/// Project template types
#[derive(Debug, Clone, Copy)]
pub enum Template {
    Console,
    Library,
    WebService,
}

impl Template {
    /// Parse template from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "console" => Some(Template::Console),
            "library" | "lib" => Some(Template::Library),
            "webservice" | "web" => Some(Template::WebService),
            _ => None,
        }
    }
}

/// Create a new FluentAI project
pub async fn new_project(
    template: &str,
    name: &str,
    path: Option<PathBuf>,
) -> Result<()> {
    let template = Template::from_str(template)
        .context("Invalid template. Use: console, library, or webservice")?;
    
    let project_path = path.unwrap_or_else(|| PathBuf::from(name));
    
    // Check if directory already exists
    if project_path.exists() {
        anyhow::bail!("Directory '{}' already exists", project_path.display());
    }
    
    // Create project directory
    fs::create_dir_all(&project_path)?;
    
    // Create project structure based on template
    match template {
        Template::Console => create_console_project(&project_path, name)?,
        Template::Library => create_library_project(&project_path, name)?,
        Template::WebService => create_webservice_project(&project_path, name)?,
    }
    
    println!("{} Created new {} project '{}'", 
             "✓".green().bold(),
             template_name(template),
             name);
    println!("\nNext steps:");
    println!("  cd {}", project_path.display());
    println!("  fluentai restore");
    println!("  fluentai run");
    
    Ok(())
}

fn template_name(template: Template) -> &'static str {
    match template {
        Template::Console => "console",
        Template::Library => "library",
        Template::WebService => "web service",
    }
}

fn create_console_project(path: &Path, name: &str) -> Result<()> {
    // Create project file
    let project_file = format!(
        r#"<Project Sdk="FluentAI.Sdk/1.0">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>fluentai1.0</TargetFramework>
    <RootNamespace>{}</RootNamespace>
  </PropertyGroup>
</Project>
"#,
        name
    );
    fs::write(path.join(format!("{}.aiproj", name)), project_file)?;
    
    // Create main program file
    let program_content = r#";; FluentAI Console Application
;; Entry point for the application

(define main (args)
  (println "Hello, FluentAI!")
  0) ;; Return exit code

;; Run main if this is the entry module
(when (= __name__ "__main__")
  (exit (main (command-line-args))))
"#;
    fs::write(path.join("Program.ai"), program_content)?;
    
    // Create src directory
    fs::create_dir(path.join("src"))?;
    
    // Create a sample module
    let utils_content = r#";; Utility functions

(define greet (name)
  (format "Hello, {}!" name))

(export greet)
"#;
    fs::write(path.join("src/Utils.ai"), utils_content)?;
    
    // Create tests directory
    fs::create_dir(path.join("tests"))?;
    
    // Create a sample test
    let test_content = r#";; Tests for Utils module

(import "../src/Utils.ai" :as utils)
(import "fluentai/test" :as test)

(test/describe "Utils"
  (test/it "greets correctly"
    (test/expect (utils/greet "World") 
                 :to-equal "Hello, World!")))
"#;
    fs::write(path.join("tests/Utils.test.ai"), test_content)?;
    
    // Create .gitignore
    let gitignore = r#"# FluentAI
*.ailib
*.aipkg
/target/
/packages/
packages.lock

# IDE
.vscode/
.idea/
*.swp
*.swo

# OS
.DS_Store
Thumbs.db
"#;
    fs::write(path.join(".gitignore"), gitignore)?;
    
    // Create README
    let readme = format!(
        r#"# {}

A FluentAI console application.

## Getting Started

```bash
fluentai restore  # Restore dependencies
fluentai run      # Run the application
fluentai test     # Run tests
```

## Building

```bash
fluentai build -c Release
```

## Publishing

```bash
fluentai publish -c Release --self-contained
```
"#,
        name
    );
    fs::write(path.join("README.md"), readme)?;
    
    Ok(())
}

fn create_library_project(path: &Path, name: &str) -> Result<()> {
    // Create project file
    let project_file = format!(
        r#"<Project Sdk="FluentAI.Sdk/1.0">
  <PropertyGroup>
    <OutputType>Library</OutputType>
    <TargetFramework>fluentai1.0</TargetFramework>
    <RootNamespace>{}</RootNamespace>
    <GeneratePackageOnBuild>true</GeneratePackageOnBuild>
    <PackageId>{}</PackageId>
    <Version>1.0.0</Version>
    <Authors>Your Name</Authors>
    <Description>A FluentAI library</Description>
  </PropertyGroup>
</Project>
"#,
        name, name
    );
    fs::write(path.join(format!("{}.aiproj", name)), project_file)?;
    
    // Create main library file
    let lib_content = format!(
        r#";; {} Library
;; Main entry point for the library

(module {}
  
  ;; Public API
  (define version "1.0.0")
  
  ;; Example function
  (define hello ()
    "Hello from {}!")
  
  ;; Export public API
  (export version hello))
"#,
        name, name, name
    );
    fs::write(path.join("lib.ai"), lib_content)?;
    
    // Create src directory with additional modules
    fs::create_dir(path.join("src"))?;
    
    Ok(())
}

fn create_webservice_project(path: &Path, name: &str) -> Result<()> {
    // Create project file
    let project_file = format!(
        r#"<Project Sdk="FluentAI.Sdk/1.0">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>fluentai1.0</TargetFramework>
    <RootNamespace>{}</RootNamespace>
  </PropertyGroup>
  
  <ItemGroup>
    <PackageReference Include="FluentAI.Http" Version="1.0.0" />
    <PackageReference Include="FluentAI.Json" Version="1.0.0" />
  </ItemGroup>
</Project>
"#,
        name
    );
    fs::write(path.join(format!("{}.aiproj", name)), project_file)?;
    
    // Create main program
    let program_content = r#";; FluentAI Web Service

(import "fluentai/http" :as http)
(import "fluentai/json" :as json)

;; Define routes
(define routes
  [(http/get "/" home-handler)
   (http/get "/api/hello" hello-handler)
   (http/post "/api/echo" echo-handler)])

;; Handlers
(define home-handler (req)
  (http/response 200 
    :body "Welcome to FluentAI Web Service!"
    :content-type "text/plain"))

(define hello-handler (req)
  (http/json-response 
    {:message "Hello, World!"
     :timestamp (current-time)}))

(define echo-handler (req)
  (let ([body (http/request-body req)])
    (http/json-response body)))

;; Main entry point
(define main (args)
  (let ([port (or (env "PORT") 8080)])
    (println (format "Starting server on port {}" port))
    (http/serve routes :port port)))

;; Run if main module
(when (= __name__ "__main__")
  (main (command-line-args)))
"#;
    fs::write(path.join("Program.ai"), program_content)?;
    
    // Create directories
    fs::create_dir(path.join("src"))?;
    fs::create_dir(path.join("tests"))?;
    
    Ok(())
}