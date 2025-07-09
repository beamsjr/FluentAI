//! Library template

use super::{Template, TemplateCategory, TemplateOptions, helpers};
use anyhow::Result;
use std::fs;
use std::path::Path;

pub struct LibraryTemplate;

impl Template for LibraryTemplate {
    fn name(&self) -> &'static str {
        "library"
    }
    
    fn description(&self) -> &'static str {
        "Reusable library package for sharing code"
    }
    
    fn aliases(&self) -> Vec<&'static str> {
        vec!["lib", "package"]
    }
    
    fn category(&self) -> TemplateCategory {
        TemplateCategory::Library
    }
    
    fn create(&self, path: &Path, name: &str, _options: &TemplateOptions) -> Result<()> {
        // Create project file with library-specific settings
        let project_content = format!(
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
        fs::write(path.join(format!("{}.aiproj", name)), project_content)?;
        
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
  
  ;; Example function with parameters
  (define greet (name)
    (format "Hello, {{}}!" name))
  
  ;; Example higher-order function
  (define map-twice (f lst)
    (map f (map f lst)))
  
  ;; Export public API
  (export version hello greet map-twice))
"#,
            name, name, name
        );
        fs::write(path.join("lib.ai"), lib_content)?;
        
        // Create directories
        helpers::create_directories(path, &["src", "tests", "docs", "examples"])?;
        
        // Create additional source file
        let math_content = r#";; Math utilities

(module math
  
  ;; Calculate factorial
  (define factorial (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))
  
  ;; Calculate fibonacci number
  (define fibonacci (n)
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (else (+ (fibonacci (- n 1))
               (fibonacci (- n 2))))))
  
  ;; Check if number is prime
  (define prime? (n)
    (define (check-divisor d)
      (cond
        ((> (* d d) n) true)
        ((= (mod n d) 0) false)
        (else (check-divisor (+ d 1)))))
    (and (> n 1) (check-divisor 2)))
  
  (export factorial fibonacci prime?))
"#;
        fs::write(path.join("src/math.ai"), math_content)?;
        
        // Create tests
        let test_content = format!(
            r#";; Tests for {} library

(import "../lib.ai" :as {})
(import "../src/math.ai" :as math)
(import "fluentai/test" :as test)

(test/describe "{} Library"
  
  (test/describe "Core functions"
    (test/it "returns correct version"
      (test/expect {}/version :to-equal "1.0.0"))
    
    (test/it "hello returns greeting"
      (test/expect ({}/hello) :to-equal "Hello from {}!"))
    
    (test/it "greet formats name correctly"
      (test/expect ({}/greet "Alice") :to-equal "Hello, Alice!")))
  
  (test/describe "Math utilities"
    (test/it "calculates factorial correctly"
      (test/expect (math/factorial 5) :to-equal 120))
    
    (test/it "generates fibonacci sequence"
      (test/expect (map math/fibonacci (range 0 7))
                   :to-equal (list 0 1 1 2 3 5 8)))
    
    (test/it "identifies prime numbers"
      (test/expect (filter math/prime? (range 1 20))
                   :to-equal (list 2 3 5 7 11 13 17 19)))))
"#,
            name, name.to_lowercase(), name, name.to_lowercase(), name, name.to_lowercase(), name.to_lowercase()
        );
        fs::write(path.join("tests/lib.test.ai"), test_content)?;
        
        // Create example
        let example_content = format!(
            r#";; Example usage of {} library

(import "{}" :as lib)
(import "{}/math" :as math)

;; Basic usage
(println (lib/hello))
(println (lib/greet "Developer"))

;; Using math utilities
(println "Factorial of 10:" (math/factorial 10))
(println "First 10 Fibonacci numbers:" 
         (map math/fibonacci (range 0 10)))
(println "Prime numbers under 50:"
         (filter math/prime? (range 2 50)))

;; Using higher-order functions
(define double (x) (* x 2))
(println "Map twice example:"
         (lib/map-twice double (list 1 2 3 4 5)))
"#,
            name, name.to_lowercase(), name.to_lowercase()
        );
        fs::write(path.join("examples/usage.ai"), example_content)?;
        
        // Create documentation
        let docs_content = format!(
            r#"# {} API Documentation

## Overview

{} is a FluentAI library that provides...

## Installation

```bash
fluentai add package {}
```

## Quick Start

```scheme
(import "{}" :as lib)

(println (lib/hello))
```

## API Reference

### Core Functions

#### `version`
- **Type**: String
- **Description**: The library version

#### `hello()`
- **Returns**: String
- **Description**: Returns a greeting message

#### `greet(name)`
- **Parameters**: 
  - `name` (String): The name to greet
- **Returns**: String
- **Description**: Returns a personalized greeting

#### `map-twice(f, lst)`
- **Parameters**: 
  - `f` (Function): Function to apply
  - `lst` (List): List to process
- **Returns**: List
- **Description**: Applies function f twice to each element

### Math Module

#### `math/factorial(n)`
- **Parameters**: 
  - `n` (Number): Non-negative integer
- **Returns**: Number
- **Description**: Calculates n!

#### `math/fibonacci(n)`
- **Parameters**: 
  - `n` (Number): Non-negative integer
- **Returns**: Number
- **Description**: Returns nth Fibonacci number

#### `math/prime?(n)`
- **Parameters**: 
  - `n` (Number): Integer to test
- **Returns**: Boolean
- **Description**: Tests if n is prime
"#,
            name, name, name, name.to_lowercase()
        );
        fs::write(path.join("docs/API.md"), docs_content)?;
        
        // Create .gitignore
        helpers::create_gitignore(path)?;
        
        // Create README
        helpers::create_readme(path, name, "A FluentAI library package.")?;
        
        Ok(())
    }
}