//! Library template

use super::{helpers, Template, TemplateCategory, TemplateOptions};
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
            r#"// lib.flc
// {} Library - Main entry point

module {};

// Public API
public const version = "1.0.0";

// Example function
public function hello() -> string {{
    "Hello from {}!"
}}

// Example function with parameters
public function greet(name: string) -> string {{
    f"Hello, {{name}}!"
}}

// Example higher-order function
public function map_twice<T>(f: (T) -> T, lst: List<T>) -> List<T> {{
    lst.map(f).map(f)
}}
"#,
            name, name, name
        );
        fs::write(path.join("lib.flc"), lib_content)?;

        // Create directories
        helpers::create_directories(path, &["src", "tests", "docs", "examples"])?;

        // Create additional source file
        let math_content = r#"// math.flc
module math;

// Calculate factorial
public function factorial(n: int) -> int {
    if (n <= 1) { 1 }
    else { n * factorial(n - 1) }
}

// Calculate fibonacci number
public function fibonacci(n: int) -> int {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci(n - 1) + fibonacci(n - 2)
    }
}

// Check if number is prime
public function is_prime(n: int) -> bool {
    let check_divisor = (d) => {
        if (d * d > n) { true }
        else if (n % d == 0) { false }
        else { check_divisor(d + 1) }
    };
    
    n > 1 && check_divisor(2)
}
"#;
        fs::write(path.join("src/math.flc"), math_content)?;

        // Create tests
        let test_content = format!(
            r#"// lib.test.flc
use ../lib;
use ../src/math;
use std::test::{{describe, it, expect}};

describe("{} Library", () => {{
    describe("Core functions", () => {{
        it("returns correct version", () => {{
            expect(lib.version).to_equal("1.0.0");
        }});
        
        it("hello returns greeting", () => {{
            expect(lib.hello()).to_equal("Hello from {}!");
        }});
        
        it("greet formats name correctly", () => {{
            expect(lib.greet("Alice")).to_equal("Hello, Alice!");
        }});
    }});
    
    describe("Math utilities", () => {{
        it("calculates factorial correctly", () => {{
            expect(math.factorial(5)).to_equal(120);
        }});
        
        it("generates fibonacci sequence", () => {{
            let fib_seq = [0, 1, 2, 3, 4, 5, 6].map(n => math.fibonacci(n));
            expect(fib_seq).to_equal([0, 1, 1, 2, 3, 5, 8]);
        }});
        
        it("identifies prime numbers", () => {{
            let primes = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
                .filter(n => math.is_prime(n));
            expect(primes).to_equal([2, 3, 5, 7, 11, 13, 17, 19]);
        }});
    }});
}});
"#,
            name,
            name
        );
        fs::write(path.join("tests/lib.test.flc"), test_content)?;

        // Create example
        let example_content = format!(
            r#"// usage.flc
// Example usage of {} library

use {};
use {}/math;

// Basic usage
$(lib.hello()).print();
$(lib.greet("Developer")).print();

// Using math utilities
$(f"Factorial of 10: {{math.factorial(10)}}").print();

let fib_numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].map(n => math.fibonacci(n));
$(f"First 10 Fibonacci numbers: {{fib_numbers}}").print();

let primes = (2..50).filter(n => math.is_prime(n));
$(f"Prime numbers under 50: {{primes}}").print();

// Using higher-order functions
let double = (x) => x * 2;
let result = lib.map_twice(double, [1, 2, 3, 4, 5]);
$(f"Map twice example: {{result}}").print();
"#,
            name,
            name,
            name
        );
        fs::write(path.join("examples/usage.flc"), example_content)?;

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

```flc
use {};

$(lib.hello()).print();
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

#### `map_twice(f, lst)`
- **Parameters**: 
  - `f` (Function): Function to apply
  - `lst` (List): List to process
- **Returns**: List
- **Description**: Applies function f twice to each element

### Math Module

#### `math.factorial(n)`
- **Parameters**: 
  - `n` (Number): Non-negative integer
- **Returns**: Number
- **Description**: Calculates n!

#### `math.fibonacci(n)`
- **Parameters**: 
  - `n` (Number): Non-negative integer
- **Returns**: Number
- **Description**: Returns nth Fibonacci number

#### `math.is_prime(n)`
- **Parameters**: 
  - `n` (Number): Integer to test
- **Returns**: Boolean
- **Description**: Tests if n is prime
"#,
            name,
            name,
            name,
            name
        );
        fs::write(path.join("docs/API.md"), docs_content)?;

        // Create .gitignore
        helpers::create_gitignore(path)?;

        // Create README
        helpers::create_readme(path, name, "A FluentAI library package.")?;

        Ok(())
    }
}
