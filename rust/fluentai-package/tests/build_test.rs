//! Tests for the build system

use fluentai_package::{BuildConfig, Builder, Manifest, PackageConfig};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_build_simple_project() {
    // Create a temporary directory for our test project
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path();
    
    // Create src directory
    let src_dir = project_dir.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    
    // Create a simple main.flc file
    let main_content = r#"
// Main entry point
public function main() {
    $("Hello from FluentAI!").print()
}
"#;
    fs::write(src_dir.join("main.flc"), main_content).unwrap();
    
    // Create manifest
    let mut manifest = Manifest::default();
    manifest.name = "test-project".to_string();
    manifest.version = "0.1.0".to_string();
    manifest.main = Some("main.flc".to_string());
    
    // Create build configuration
    let build_config = BuildConfig {
        src_dir: src_dir.clone(),
        out_dir: project_dir.join("dist"),
        source_maps: true,
        opt_level: 0,
        bundle_deps: false,
        include_paths: vec![],
    };
    
    let package_config = PackageConfig::default();
    
    // Create builder and build
    let mut builder = Builder::new(manifest, build_config, package_config);
    let result = builder.build();
    
    // Check that build succeeded
    assert!(result.is_ok(), "Build failed: {:?}", result);
    
    // Check that output files were created
    let dist_dir = project_dir.join("dist");
    assert!(dist_dir.exists());
    assert!(dist_dir.join("main.json").exists());
    assert!(dist_dir.join("main.flc").exists()); // source map
    assert!(dist_dir.join("package.json").exists());
}

#[test]
fn test_build_with_imports() {
    // Create a temporary directory for our test project
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path();
    
    // Create src directory
    let src_dir = project_dir.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    
    // Create a library module
    let utils_content = r#"
// Utility functions
public function greet(name: string) {
    $(f"Hello, {name}!").print()
}
"#;
    fs::write(src_dir.join("utils.flc"), utils_content).unwrap();
    
    // Create main.flc that imports utils
    let main_content = r#"
use utils::{greet};

public function main() {
    greet("FluentAI")
}
"#;
    fs::write(src_dir.join("main.flc"), main_content).unwrap();
    
    // Create manifest
    let mut manifest = Manifest::default();
    manifest.name = "test-imports".to_string();
    manifest.version = "0.1.0".to_string();
    
    // Create build configuration
    let build_config = BuildConfig {
        src_dir: src_dir.clone(),
        out_dir: project_dir.join("dist"),
        source_maps: false,
        opt_level: 1,
        bundle_deps: false,
        include_paths: vec![],
    };
    
    let package_config = PackageConfig::default();
    
    // Create builder and build
    let mut builder = Builder::new(manifest, build_config, package_config);
    let result = builder.build();
    
    // Check that build succeeded
    assert!(result.is_ok(), "Build failed: {:?}", result);
    
    // Check that both modules were compiled
    let dist_dir = project_dir.join("dist");
    assert!(dist_dir.join("main.json").exists());
    assert!(dist_dir.join("utils.json").exists());
    assert!(!dist_dir.join("main.flc").exists()); // no source maps
}

#[test]
fn test_build_bundle() {
    // Create a temporary directory for our test project
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path();
    
    // Create src directory
    let src_dir = project_dir.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    
    // Create multiple modules
    let math_content = r#"
public function add(a: float, b: float) -> float {
    a + b
}

public function multiply(a: float, b: float) -> float {
    a * b
}
"#;
    fs::write(src_dir.join("math.flc"), math_content).unwrap();
    
    let main_content = r#"
use math::{add, multiply};

public function main() {
    let result = add(2, 3);
    let product = multiply(result, 4);
    $(f"Result: {product}").print()
}
"#;
    fs::write(src_dir.join("main.flc"), main_content).unwrap();
    
    // Create manifest
    let mut manifest = Manifest::default();
    manifest.name = "test-bundle".to_string();
    manifest.version = "0.1.0".to_string();
    
    // Create build configuration with bundling enabled
    let build_config = BuildConfig {
        src_dir: src_dir.clone(),
        out_dir: project_dir.join("dist"),
        source_maps: false,
        opt_level: 2,
        bundle_deps: true,
        include_paths: vec![],
    };
    
    let package_config = PackageConfig::default();
    
    // Create builder and build
    let mut builder = Builder::new(manifest.clone(), build_config, package_config);
    let result = builder.build();
    
    // Check that build succeeded
    assert!(result.is_ok(), "Build failed: {:?}", result);
    
    // Check that bundle was created
    let dist_dir = project_dir.join("dist");
    let bundle_path = dist_dir.join(format!("{}.bundle.flc", manifest.name));
    assert!(bundle_path.exists());
    
    // Check bundle content
    let bundle_content = fs::read_to_string(bundle_path).unwrap();
    assert!(bundle_content.contains("Module: math"));
    assert!(bundle_content.contains("Module: main"));
}

#[test]
fn test_build_no_entry_point() {
    // Create a temporary directory for our test project
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path();
    
    // Create src directory but no main file
    let src_dir = project_dir.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    
    // Create manifest without main field
    let manifest = Manifest {
        name: "test-no-entry".to_string(),
        version: "0.1.0".to_string(),
        main: None,
        ..Default::default()
    };
    
    // Create build configuration
    let build_config = BuildConfig {
        src_dir: src_dir.clone(),
        out_dir: project_dir.join("dist"),
        source_maps: true,
        opt_level: 0,
        bundle_deps: false,
        include_paths: vec![],
    };
    
    let package_config = PackageConfig::default();
    
    // Create builder and build
    let mut builder = Builder::new(manifest, build_config, package_config);
    let result = builder.build();
    
    // Check that build failed with appropriate error
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("No entry point found"));
}