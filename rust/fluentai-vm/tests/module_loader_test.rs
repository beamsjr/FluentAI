//! Tests for the module loader functionality

use fluentai_vm::{ModuleLoader, ModuleLoaderConfig, VM, Compiler, CompilerOptions};
use fluentai_parser::parse_flc;
use fluentai_optimizer::OptimizationLevel;
use fluentai_core::value::Value;
use anyhow::Result;
use tempfile::TempDir;
use std::fs;
use std::path::PathBuf;

#[test]
fn test_module_loader_basic() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let module_path = temp_dir.path();
    
    // Create a simple module
    let math_module = r#"
// A simple math module

public function add(a: int, b: int) -> int {
    a + b
}

public function multiply(a: int, b: int) -> int {
    a * b
}

public const PI = 3.14159;
"#;
    
    fs::write(module_path.join("Math.flc"), math_module)?;
    
    // Create loader with test path
    let config = ModuleLoaderConfig {
        module_paths: vec![module_path.to_path_buf()],
        module_extension: "flc".to_string(),
        enable_cache: true,
        optimization_level: OptimizationLevel::None,
    };
    let loader = ModuleLoader::with_config(config);
    
    // Create a simple test program
    let main_code = "42";
    let graph = parse_flc(main_code)?;
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM and load module
    let mut vm = VM::new(bytecode);
    let result = loader.load_module(&mut vm, "Math");
    
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "Math");
    
    // Verify module is loaded
    assert!(vm.has_module("Math"));
    
    // Verify exports are available
    assert!(vm.get_module_export("Math", "add").is_ok());
    assert!(vm.get_module_export("Math", "multiply").is_ok());
    assert!(vm.get_module_export("Math", "PI").is_ok());
    
    Ok(())
}

#[test]
fn test_module_loader_nested_path() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let module_path = temp_dir.path();
    
    // Create nested directory
    fs::create_dir_all(module_path.join("utils"))?;
    
    // Create a nested module
    let string_utils = r#"
// String utility module

public function concat(a: string, b: string) -> string {
    a + b
}

public function length(s: string) -> int {
    s.length()
}
"#;
    
    fs::write(module_path.join("utils").join("strings.flc"), string_utils)?;
    
    // Create loader
    let config = ModuleLoaderConfig {
        module_paths: vec![module_path.to_path_buf()],
        module_extension: "flc".to_string(),
        enable_cache: true,
        optimization_level: OptimizationLevel::None,
    };
    let loader = ModuleLoader::with_config(config);
    
    // Create VM
    let main_code = "42";
    let graph = parse_flc(main_code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    // Load nested module
    let result = loader.load_module(&mut vm, "utils.strings");
    
    assert!(result.is_ok());
    assert!(vm.has_module("utils.strings"));
    
    Ok(())
}

#[test]
fn test_module_not_found() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let module_path = temp_dir.path();
    
    let config = ModuleLoaderConfig {
        module_paths: vec![module_path.to_path_buf()],
        module_extension: "flc".to_string(),
        enable_cache: true,
        optimization_level: OptimizationLevel::None,
    };
    let loader = ModuleLoader::with_config(config);
    
    // Create VM
    let main_code = "42";
    let graph = parse_flc(main_code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    // Try to load non-existent module
    let result = loader.load_module(&mut vm, "NonExistent");
    
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));
    
    Ok(())
}

#[test]
fn test_module_caching() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let module_path = temp_dir.path();
    
    // Create a module
    let test_module = r#"
public function test() -> string {
    "Hello from module"
}
"#;
    
    fs::write(module_path.join("Test.flc"), test_module)?;
    
    let config = ModuleLoaderConfig {
        module_paths: vec![module_path.to_path_buf()],
        module_extension: "flc".to_string(),
        enable_cache: true,
        optimization_level: OptimizationLevel::None,
    };
    let loader = ModuleLoader::with_config(config);
    
    // Create VM
    let main_code = "42";
    let graph = parse_flc(main_code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    // Load module twice
    let result1 = loader.load_module(&mut vm, "Test");
    let result2 = loader.load_module(&mut vm, "Test");
    
    assert!(result1.is_ok());
    assert!(result2.is_ok());
    
    // Both should return the same module name
    assert_eq!(result1.unwrap(), "Test");
    assert_eq!(result2.unwrap(), "Test");
    
    Ok(())
}

#[test]
fn test_module_with_syntax_error() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let module_path = temp_dir.path();
    
    // Create a module with syntax error
    let bad_module = r#"
public function test() -> string {
    // Missing closing brace
    "Hello"
"#;
    
    fs::write(module_path.join("Bad.flc"), bad_module)?;
    
    let config = ModuleLoaderConfig {
        module_paths: vec![module_path.to_path_buf()],
        module_extension: "flc".to_string(),
        enable_cache: true,
        optimization_level: OptimizationLevel::None,
    };
    let loader = ModuleLoader::with_config(config);
    
    // Create VM
    let main_code = "42";
    let graph = parse_flc(main_code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    // Try to load module with syntax error
    let result = loader.load_module(&mut vm, "Bad");
    
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("parse"));
    
    Ok(())
}

#[test]
fn test_load_and_get_export() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let module_path = temp_dir.path();
    
    // Create a module with a specific export
    let module_content = r#"
public function greet(name: string) -> string {
    "Hello, " + name
}

public const VERSION = "1.0.0";
"#;
    
    fs::write(module_path.join("Greeter.flc"), module_content)?;
    
    let config = ModuleLoaderConfig {
        module_paths: vec![module_path.to_path_buf()],
        module_extension: "flc".to_string(),
        enable_cache: true,
        optimization_level: OptimizationLevel::None,
    };
    let loader = ModuleLoader::with_config(config);
    
    // Create VM
    let main_code = "42";
    let graph = parse_flc(main_code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    // Load and get specific export
    let greet_fn = loader.load_and_get_export(&mut vm, "Greeter", "greet")?;
    assert!(matches!(greet_fn, Value::Function { .. } | Value::NativeFunction { .. }));
    
    let version = loader.load_and_get_export(&mut vm, "Greeter", "VERSION")?;
    assert_eq!(version, Value::String("1.0.0".to_string()));
    
    // Try to get non-existent export
    let result = loader.load_and_get_export(&mut vm, "Greeter", "nonexistent");
    assert!(result.is_err());
    
    Ok(())
}

#[test]
fn test_multiple_module_paths() -> Result<()> {
    let temp_dir1 = TempDir::new()?;
    let temp_dir2 = TempDir::new()?;
    
    // Create modules in different directories
    let module1 = r#"
public function from_dir1() -> string {
    "Module from directory 1"
}
"#;
    
    let module2 = r#"
public function from_dir2() -> string {
    "Module from directory 2"
}
"#;
    
    fs::write(temp_dir1.path().join("Module1.flc"), module1)?;
    fs::write(temp_dir2.path().join("Module2.flc"), module2)?;
    
    // Create loader with multiple paths
    let config = ModuleLoaderConfig {
        module_paths: vec![
            temp_dir1.path().to_path_buf(),
            temp_dir2.path().to_path_buf(),
        ],
        module_extension: "flc".to_string(),
        enable_cache: true,
        optimization_level: OptimizationLevel::None,
    };
    let loader = ModuleLoader::with_config(config);
    
    // Create VM
    let main_code = "42";
    let graph = parse_flc(main_code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    // Load modules from different directories
    assert!(loader.load_module(&mut vm, "Module1").is_ok());
    assert!(loader.load_module(&mut vm, "Module2").is_ok());
    
    assert!(vm.has_module("Module1"));
    assert!(vm.has_module("Module2"));
    
    Ok(())
}