//! Integration test for the module system
//! 
//! This test demonstrates:
//! 1. Loading modules from the file system
//! 2. Importing functions from modules
//! 3. Using qualified variable access (Module.function)
//! 4. Symbol resolution for imported modules

use fluentai_vm::{VM, Compiler, CompilerOptions, ModuleLoader, ModuleLoaderConfig};
use fluentai_parser::parse_flc;
use fluentai_optimizer::OptimizationLevel;
use fluentai_core::value::Value;
use anyhow::Result;
use tempfile::TempDir;
use std::fs;
use std::path::PathBuf;

fn create_test_module_loader(module_path: PathBuf) -> ModuleLoader {
    let config = ModuleLoaderConfig {
        module_paths: vec![module_path],
        module_extension: "flc".to_string(),
        enable_cache: true,
        optimization_level: OptimizationLevel::None,
    };
    ModuleLoader::with_config(config)
}

#[test]
fn test_module_import_and_use() -> Result<()> {
    // Create a temporary directory for our test modules
    let temp_dir = TempDir::new()?;
    let module_path = temp_dir.path();
    
    // Create a math module
    let math_module = r#"
module Math;

public function add(a: int, b: int) -> int {
    a + b
}

public function multiply(a: int, b: int) -> int {
    a * b
}

public const PI = 3.14159;
"#;
    
    fs::write(module_path.join("Math.flc"), math_module)?;
    
    // Create a utils module
    let utils_module = r#"
module Utils;

public function double(x: int) -> int {
    x * 2
}

public function greet(name: string) -> string {
    "Hello, " + name + "!"
}
"#;
    
    fs::write(module_path.join("Utils.flc"), utils_module)?;
    
    // Create main code that uses the modules
    let main_code = r#"
use Math;
use Utils;

// Test direct imports
let result1 = add(5, 3);
let result2 = multiply(4, 7);
let result3 = double(21);

// Test qualified access
let pi = Math.PI;
let sum = Math.add(10, 20);
let greeting = Utils.greet("World");

// Return a tuple of results for testing
[result1, result2, result3, pi, sum, greeting]
"#;
    
    // Parse and compile the main code
    let graph = parse_flc(main_code)?;
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM with module loader
    let mut vm = VM::new(bytecode);
    
    // Create and configure module loader
    let loader = create_test_module_loader(module_path.to_path_buf());
    
    // Pre-load the modules using the module loader
    loader.load_module(&mut vm, "Math")?;
    loader.load_module(&mut vm, "Utils")?;
    
    // Import the functions into globals so the main code can use them
    // This simulates what the ImportBinding opcode would do
    if let Ok(add_fn) = vm.get_module_export("Math", "add") {
        vm.set_global("add".to_string(), add_fn);
    }
    if let Ok(multiply_fn) = vm.get_module_export("Math", "multiply") {
        vm.set_global("multiply".to_string(), multiply_fn);
    }
    if let Ok(double_fn) = vm.get_module_export("Utils", "double") {
        vm.set_global("double".to_string(), double_fn);
    }
    
    // Run the VM
    let result = vm.run()?;
    
    // Verify the results
    match result {
        Value::List(values) => {
            assert_eq!(values.len(), 6);
            
            // Check result1: add(5, 3) = 8
            assert_eq!(values[0], Value::Integer(8));
            
            // Check result2: multiply(4, 7) = 28
            assert_eq!(values[1], Value::Integer(28));
            
            // Check result3: double(21) = 42
            assert_eq!(values[2], Value::Integer(42));
            
            // Check pi: Math.PI = 3.14159
            assert_eq!(values[3], Value::Float(3.14159));
            
            // Check sum: Math.add(10, 20) = 30
            assert_eq!(values[4], Value::Integer(30));
            
            // Check greeting: Utils.greet("World") = "Hello, World!"
            assert_eq!(values[5], Value::String("Hello, World!".to_string()));
        }
        _ => panic!("Expected list result, got {:?}", result),
    }
    
    Ok(())
}

#[test]
fn test_module_not_found_error() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let module_path = temp_dir.path();
    
    // Try to use a non-existent module
    let code = r#"
use NonExistentModule;

42
"#;
    
    // Parse and compile
    let graph = parse_flc(code)?;
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM with module loader
    let mut vm = VM::new(bytecode);
    let loader = create_test_module_loader(module_path.to_path_buf());
    
    // Try to load the non-existent module
    let result = loader.load_module(&mut vm, "NonExistentModule");
    
    // Should fail with module not found error
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));
    
    Ok(())
}

#[test]
fn test_nested_module_path() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let module_path = temp_dir.path();
    
    // Create nested directory structure
    fs::create_dir_all(module_path.join("utils"))?;
    
    // Create a nested module
    let string_utils = r#"
module utils.strings;

public function uppercase(s: string) -> string {
    // For now, just return the string with " (uppercase)" appended
    // since we don't have real string manipulation yet
    s + " (UPPERCASE)"
}

public function lowercase(s: string) -> string {
    s + " (lowercase)"
}
"#;
    
    fs::write(module_path.join("utils").join("strings.flc"), string_utils)?;
    
    // Create code that uses the nested module
    let code = r#"
use utils.strings;

let upper = uppercase("hello");
let lower = lowercase("WORLD");

[upper, lower]
"#;
    
    // Parse and compile
    let graph = parse_flc(code)?;
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM and load module
    let mut vm = VM::new(bytecode);
    let loader = create_test_module_loader(module_path.to_path_buf());
    
    loader.load_module(&mut vm, "utils.strings")?;
    
    // Import the functions
    if let Ok(uppercase_fn) = vm.get_module_export("utils.strings", "uppercase") {
        vm.set_global("uppercase".to_string(), uppercase_fn);
    }
    if let Ok(lowercase_fn) = vm.get_module_export("utils.strings", "lowercase") {
        vm.set_global("lowercase".to_string(), lowercase_fn);
    }
    
    // Run and verify
    let result = vm.run()?;
    match result {
        Value::List(values) => {
            assert_eq!(values.len(), 2);
            assert_eq!(values[0], Value::String("hello (UPPERCASE)".to_string()));
            assert_eq!(values[1], Value::String("WORLD (lowercase)".to_string()));
        }
        _ => panic!("Expected list result"),
    }
    
    Ok(())
}