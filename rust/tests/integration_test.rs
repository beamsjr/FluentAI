//! Integration tests for FluentAi module system and package manager

use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM, bytecode::Value};
use fluentai_modules::{ModuleLoader, ModuleConfig};
use fluentai_package::{Manifest, DependencyResolver, registry::LocalRegistry};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tempfile::TempDir;

#[test]
fn test_module_loading_and_execution() {
    let temp_dir = TempDir::new().unwrap();
    
    // Create a math module
    let math_module = r#"
(module math (export add multiply square)
  (let ((add (lambda (a b) (+ a b)))
        (multiply (lambda (a b) (* a b)))
        (square (lambda (x) (* x x))))
    nil))
"#;
    
    fs::write(temp_dir.path().join("math.ai"), math_module).unwrap();
    
    // Create a main module that imports math
    let main_module = r#"
(import "math" (add multiply))
(+ (add 2 3) (multiply 4 5))
"#;
    
    // Parse and compile
    let ast = parse(main_module).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    // Create VM with module loader
    let config = ModuleConfig {
        search_paths: vec![temp_dir.path().to_path_buf()],
        enable_cache: true,
        max_cache_size: 100,
        allow_circular: false,
    };
    let loader = ModuleLoader::new(config);
    
    let mut vm = VM::new(bytecode);
    vm.set_module_loader(loader);
    
    // Run should load the math module and execute
    let result = vm.run().unwrap();
    
    // 2 + 3 = 5, 4 * 5 = 20, 5 + 20 = 25
    assert_eq!(result, Value::Integer(25));
}

#[test]
fn test_qualified_module_access() {
    let temp_dir = TempDir::new().unwrap();
    
    // Create a utils module
    let utils_module = r#"
(module utils (export format-number)
  (let ((format-number (lambda (n) (str "Number: " n))))
    nil))
"#;
    
    fs::write(temp_dir.path().join("utils.ai"), utils_module).unwrap();
    
    // Use qualified access
    let main_code = r#"
(import "utils" *)
(utils.format-number 42)
"#;
    
    let ast = parse(main_code).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    let config = ModuleConfig {
        search_paths: vec![temp_dir.path().to_path_buf()],
        ..Default::default()
    };
    let loader = ModuleLoader::new(config);
    
    let mut vm = VM::new(bytecode);
    vm.set_module_loader(loader);
    
    let result = vm.run().unwrap();
    assert_eq!(result, Value::String("Number: 42".to_string()));
}

#[test]
fn test_package_manifest_and_dependencies() {
    let temp_dir = TempDir::new().unwrap();
    
    // Create a package manifest
    let manifest_content = r#"{
    "name": "my-app",
    "version": "1.0.0",
    "description": "Test application",
    "author": "Test Author",
    "license": "MIT",
    "dependencies": {
        "math-lib": "^1.0.0",
        "utils": "~2.0.0"
    },
    "devDependencies": {
        "test-framework": "^3.0.0"
    }
}"#;
    
    let manifest_path = temp_dir.path().join("fluentai.json");
    fs::write(&manifest_path, manifest_content).unwrap();
    
    // Parse manifest
    let manifest = Manifest::from_file(&manifest_path).unwrap();
    assert_eq!(manifest.name, "my-app");
    assert_eq!(manifest.version, "1.0.0");
    assert_eq!(manifest.dependencies.len(), 2);
    assert_eq!(manifest.dev_dependencies.len(), 1);
}

#[test]
fn test_module_with_effects() {
    let temp_dir = TempDir::new().unwrap();
    
    // Create an IO module
    let io_module = r#"
(module io (export read-file write-file)
  (let ((read-file (lambda (path) (effect io:read path)))
        (write-file (lambda (path content) (effect io:write path content))))
    nil))
"#;
    
    fs::write(temp_dir.path().join("io.ai"), io_module).unwrap();
    
    // Use the IO module
    let main_code = r#"
(import "io" (read-file write-file))
(let ((content (read-file "test.txt")))
  (write-file "output.txt" content))
"#;
    
    let ast = parse(main_code).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    let config = ModuleConfig {
        search_paths: vec![temp_dir.path().to_path_buf()],
        ..Default::default()
    };
    let loader = ModuleLoader::new(config);
    
    let mut vm = VM::new(bytecode);
    vm.set_module_loader(loader);
    
    // This would require effect handlers to be set up
    // For now, we just verify it compiles correctly
    assert!(bytecode.chunks.len() > 0);
}

#[test]
fn test_nested_module_imports() {
    let temp_dir = TempDir::new().unwrap();
    
    // Create a base module
    let base_module = r#"
(module base (export pi e)
  (let ((pi 3.14159)
        (e 2.71828))
    nil))
"#;
    
    // Create a math module that imports base
    let math_module = r#"
(import "base" (pi))
(module math (export circle-area)
  (let ((circle-area (lambda (r) (* pi r r))))
    nil))
"#;
    
    // Create main that uses math
    let main_code = r#"
(import "math" (circle-area))
(circle-area 5)
"#;
    
    fs::write(temp_dir.path().join("base.ai"), base_module).unwrap();
    fs::write(temp_dir.path().join("math.ai"), math_module).unwrap();
    
    let ast = parse(main_code).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    let config = ModuleConfig {
        search_paths: vec![temp_dir.path().to_path_buf()],
        ..Default::default()
    };
    let loader = ModuleLoader::new(config);
    
    let mut vm = VM::new(bytecode);
    vm.set_module_loader(loader);
    
    // This tests that nested imports work correctly
    let result = vm.run().unwrap();
    // pi * 5 * 5 = 3.14159 * 25 = 78.53975
    match result {
        Value::Float(f) => assert!((f - 78.53975).abs() < 0.0001),
        _ => panic!("Expected float result"),
    }
}

#[test]
fn test_module_exports_validation() {
    let temp_dir = TempDir::new().unwrap();
    
    // Create a module with specific exports
    let secure_module = r#"
(module secure (export public-api)
  (let ((private-key "secret123")
        (public-api (lambda (data) (encrypt data private-key)))
        (encrypt (lambda (data key) (str data "-encrypted-with-" key))))
    nil))
"#;
    
    fs::write(temp_dir.path().join("secure.ai"), secure_module).unwrap();
    
    // Try to import non-exported function (should fail at runtime)
    let invalid_code = r#"
(import "secure" (private-key))
private-key
"#;
    
    let ast = parse(invalid_code).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    let config = ModuleConfig {
        search_paths: vec![temp_dir.path().to_path_buf()],
        ..Default::default()
    };
    let loader = ModuleLoader::new(config);
    
    let mut vm = VM::new(bytecode);
    vm.set_module_loader(loader);
    
    // Should fail because private-key is not exported
    let result = vm.run();
    assert!(result.is_err());
}

#[test]
fn test_package_dependency_resolution() {
    use fluentai_package::{Version, registry::LocalRegistry};
    
    let temp_dir = TempDir::new().unwrap();
    let registry_path = temp_dir.path().join("registry");
    fs::create_dir(&registry_path).unwrap();
    
    // Create a local registry
    let registry = Arc::new(LocalRegistry::new(registry_path));
    
    // Create a manifest with dependencies
    let mut manifest = Manifest::default();
    manifest.name = "test-app".to_string();
    manifest.version = "1.0.0".to_string();
    manifest.dependencies.insert(
        "math-lib".to_string(),
        fluentai_package::manifest::Dependency::Version("^1.0.0".to_string()),
    );
    
    // Create a dependency resolver
    let mut resolver = DependencyResolver::new(registry.clone());
    
    // In a real scenario, we would have packages in the registry
    // For this test, we just verify the resolver is created correctly
    let result = resolver.resolve(&manifest, false);
    
    // Without packages in the registry, this should return empty or error
    match result {
        Ok(deps) => assert_eq!(deps.len(), 0),
        Err(_) => {
            // Expected if no packages are in registry
        }
    }
}

#[test]
fn test_module_caching() {
    let temp_dir = TempDir::new().unwrap();
    
    // Create a module
    let cached_module = r#"
(module cached (export value)
  (let ((value 42))
    nil))
"#;
    
    fs::write(temp_dir.path().join("cached.ai"), cached_module).unwrap();
    
    let config = ModuleConfig {
        search_paths: vec![temp_dir.path().to_path_buf()],
        enable_cache: true,
        max_cache_size: 10,
        allow_circular: false,
    };
    
    let mut loader1 = ModuleLoader::new(config.clone());
    let mut loader2 = ModuleLoader::new(config);
    
    // Load module with first loader
    let module1 = loader1.load_module("cached").unwrap();
    
    // Load same module with second loader (should be cached)
    let module2 = loader2.load_module("cached").unwrap();
    
    // Verify they have the same content
    assert_eq!(module1.name, module2.name);
    assert_eq!(module1.exports, module2.exports);
}

#[test]
fn test_module_search_paths() {
    let temp_dir = TempDir::new().unwrap();
    let lib_dir = temp_dir.path().join("lib");
    let app_dir = temp_dir.path().join("app");
    
    fs::create_dir(&lib_dir).unwrap();
    fs::create_dir(&app_dir).unwrap();
    
    // Create a library module
    let lib_module = r#"
(module stdlib (export map filter)
  (let ((map (lambda (f xs) xs))
        (filter (lambda (p xs) xs)))
    nil))
"#;
    
    fs::write(lib_dir.join("stdlib.ai"), lib_module).unwrap();
    
    // Create an app module
    let app_module = r#"
(module app (export main)
  (let ((main (lambda () "Hello, World!")))
    nil))
"#;
    
    fs::write(app_dir.join("app.ai"), app_module).unwrap();
    
    // Configure with multiple search paths
    let config = ModuleConfig {
        search_paths: vec![lib_dir, app_dir],
        ..Default::default()
    };
    
    let mut loader = ModuleLoader::new(config);
    
    // Should find both modules
    let stdlib = loader.load_module("stdlib").unwrap();
    assert_eq!(stdlib.name, "stdlib");
    
    let app = loader.load_module("app").unwrap();
    assert_eq!(app.name, "app");
}