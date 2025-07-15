//! Build FluentAI projects

use anyhow::{Context, Result};
use colored::*;
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

/// Build configuration
#[derive(Debug, Clone)]
pub struct BuildConfig {
    /// Build configuration type (Debug or Release)
    pub configuration: String,
    /// Optional output path for build artifacts
    pub output_path: Option<PathBuf>,
    /// Target platform for the build
    pub target: BuildTarget,
    /// Optimization level (0-3)
    pub optimization_level: u8,
    /// Enable verbose output
    pub verbose: bool,
}

/// Build target types
#[derive(Debug, Clone)]
pub enum BuildTarget {
    /// Build as executable binary
    Executable,
    /// Build as library
    Library,
    /// Build as WebAssembly module
    WebAssembly,
}

/// Build a FluentAI project
pub async fn build(project_path: Option<PathBuf>, config: BuildConfig) -> Result<()> {
    let start = Instant::now();
    let project_path = project_path.unwrap_or_else(|| PathBuf::from("."));

    println!("{} FluentAI Build", "→".blue().bold());
    println!("  Configuration: {}", config.configuration);
    println!("  Target: {:?}", config.target);

    // Find project file
    let project_file = find_project_file(&project_path)?;
    println!("  Project: {}", project_file.display());

    // Load project configuration
    let project = load_project(&project_file)?;

    // Create output directory
    let output_dir = match config.output_path.as_ref() {
        Some(path) => path.clone(),
        None => project_path
            .join("target")
            .join(&config.configuration.to_lowercase()),
    };
    fs::create_dir_all(&output_dir)?;

    // Collect source files
    let pb = ProgressBar::new_spinner();
    pb.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.green} {msg}")
            .unwrap(),
    );
    pb.set_message("Collecting source files...");

    let source_files = collect_source_files(&project_path, &project)?;
    pb.finish_with_message(format!("Found {} source files", source_files.len()));

    // Compile each file
    let pb = ProgressBar::new(source_files.len() as u64);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{bar:40.cyan/blue} {pos}/{len} {msg}")
            .unwrap(),
    );

    let mut compiled_modules = Vec::new();
    for (i, source_file) in source_files.iter().enumerate() {
        pb.set_position(i as u64);
        pb.set_message(format!(
            "Compiling {}",
            source_file.file_name().unwrap().to_string_lossy()
        ));

        let module = compile_file(source_file, &config)?;
        compiled_modules.push(module);
    }
    pb.finish_with_message("Compilation complete");

    // Link modules
    let pb = ProgressBar::new_spinner();
    pb.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.green} {msg}")
            .unwrap(),
    );
    pb.set_message("Linking...");

    let output_file = link_modules(&compiled_modules, &output_dir, &project.name, &config)?;

    pb.finish_and_clear();

    let elapsed = start.elapsed();
    println!(
        "\n{} Build succeeded in {:.2}s",
        "✓".green().bold(),
        elapsed.as_secs_f64()
    );
    println!("  Output: {}", output_file.display());

    Ok(())
}

/// Project configuration
#[derive(Debug)]
struct Project {
    /// Project name
    name: String,
    /// Output type (Exe, Library, etc.)
    output_type: String,
    /// Target framework version
    target_framework: String,
    /// Project dependencies
    dependencies: Vec<Dependency>,
}

/// Project dependency information
#[derive(Debug)]
struct Dependency {
    /// Dependency name
    name: String,
    /// Dependency version
    version: String,
}

/// Find project file in directory
fn find_project_file(path: &Path) -> Result<PathBuf> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("aiproj") {
            return Ok(path);
        }
    }
    anyhow::bail!("No .aiproj file found in {}", path.display())
}

/// Load project configuration
fn load_project(project_file: &Path) -> Result<Project> {
    let content = fs::read_to_string(project_file)?;

    // Simple XML parsing (in real implementation, use proper XML parser)
    let name = project_file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Unknown")
        .to_string();

    Ok(Project {
        name,
        output_type: "Exe".to_string(),
        target_framework: "fluentai1.0".to_string(),
        dependencies: Vec::new(),
    })
}

/// Collect all source files using glob patterns
fn collect_source_files(project_path: &Path, _project: &Project) -> Result<Vec<PathBuf>> {
    use glob::glob;
    use std::collections::HashSet;
    
    let mut files = HashSet::new();
    
    // Default patterns for FluentAI projects
    let patterns = vec![
        "Program.flc",
        "main.flc",
        "lib.flc",
        "src/**/*.flc",
        "lib/**/*.flc",
        "tests/**/*.test.flc",
    ];
    
    // Process each pattern
    for pattern in patterns {
        let full_pattern = project_path.join(pattern);
        let pattern_str = full_pattern.to_str()
            .ok_or_else(|| anyhow::anyhow!("Invalid path"))?;
        
        // Use glob to find matching files
        for entry in glob(pattern_str).map_err(|e| anyhow::anyhow!("Glob pattern error: {}", e))? {
            match entry {
                Ok(path) => {
                    // Only include .flc files and exclude test files for regular builds
                    if path.extension().and_then(|s| s.to_str()) == Some("flc") &&
                       !path.to_str().unwrap_or("").contains(".test.flc") {
                        files.insert(path);
                    }
                }
                Err(e) => {
                    // Log glob errors but continue
                    eprintln!("Warning: Error reading path: {}", e);
                }
            }
        }
    }
    
    // Convert to sorted vector for consistent ordering
    let mut files_vec: Vec<PathBuf> = files.into_iter().collect();
    files_vec.sort();
    
    if files_vec.is_empty() {
        return Err(anyhow::anyhow!("No source files found. Make sure you have .flc files in your project."));
    }
    
    Ok(files_vec)
}

// Note: The recursive collect_flc_files function has been replaced by glob pattern matching

/// Compiled module representation
struct CompiledModule {
    /// Module name
    name: String,
    /// Compiled bytecode
    bytecode: Vec<u8>,
    /// Module metadata
    metadata: ModuleMetadata,
}

/// Module metadata information
struct ModuleMetadata {
    /// Exported symbols
    exports: Vec<String>,
    /// Imported symbols
    imports: Vec<String>,
}

/// Compile a single file
fn compile_file(source_file: &Path, config: &BuildConfig) -> Result<CompiledModule> {
    let source = fs::read_to_string(source_file)?;

    // Parse
    let ast = fluentai_parser::parse_flc(&source).context("Failed to parse source file")?;

    // Optimize
    let ast = if config.optimization_level > 0 {
        let mut optimizer = fluentai_optimizer::GraphOptimizer::new();
        optimizer.optimize(&ast).context("Failed to optimize")?
    } else {
        ast
    };

    // Compile to bytecode
    let compiler = fluentai_vm::compiler::Compiler::new();
    let bytecode = compiler.compile(&ast).context("Failed to compile")?;

    // Extract metadata
    let module_name = source_file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string();

    // Extract exports and imports from AST
    let (exports, imports) = extract_module_metadata(&ast);

    Ok(CompiledModule {
        name: module_name,
        bytecode: serialize_bytecode(&bytecode),
        metadata: ModuleMetadata {
            exports,
            imports,
        },
    })
}

/// Extract module metadata (exports and imports) from AST
fn extract_module_metadata(ast: &fluentai_core::ast::Graph) -> (Vec<String>, Vec<String>) {
    use fluentai_core::ast::Node;
    use std::collections::HashSet;
    
    let mut exports = HashSet::new();
    let mut imports = HashSet::new();
    
    // Traverse all nodes in the graph
    for (node_id, node) in ast.nodes() {
        match node {
            Node::Export { export_list } => {
                // Collect all exported names
                for export_item in export_list {
                    // Use the alias if provided, otherwise the original name
                    let exported_name = export_item.alias.as_ref().unwrap_or(&export_item.name);
                    exports.insert(exported_name.clone());
                }
            }
            Node::Import { module_path, import_list, import_all } => {
                // Collect imported module paths
                imports.insert(module_path.clone());
                
                // If importing specific items, we could track them individually
                // For now, we just track the module path
                if *import_all {
                    imports.insert(format!("{}::*", module_path));
                }
            }
            _ => {}
        }
    }
    
    // Convert to sorted vectors for consistent output
    let mut exports_vec: Vec<String> = exports.into_iter().collect();
    let mut imports_vec: Vec<String> = imports.into_iter().collect();
    exports_vec.sort();
    imports_vec.sort();
    
    (exports_vec, imports_vec)
}

/// Serialize bytecode for storage
fn serialize_bytecode(bytecode: &fluentai_bytecode::Bytecode) -> Vec<u8> {
    use serde::{Serialize, Deserialize};
    
    // Create a serializable representation of the bytecode
    #[derive(Serialize, Deserialize)]
    struct SerializedBytecode {
        chunks: Vec<SerializedChunk>,
        main_chunk: usize,
    }
    
    #[derive(Serialize, Deserialize)]
    struct SerializedChunk {
        instructions: Vec<SerializedInstruction>,
        constants: Vec<SerializedValue>,
        name: Option<String>,
    }
    
    #[derive(Serialize, Deserialize)]
    struct SerializedInstruction {
        opcode: String,
        operand: u32,
    }
    
    #[derive(Serialize, Deserialize)]
    enum SerializedValue {
        Integer(i64),
        Float(f64),
        String(String),
        Boolean(bool),
        Nil,
    }
    
    // Convert bytecode to serializable format
    let serialized = SerializedBytecode {
        chunks: bytecode.chunks.iter().map(|chunk| {
            SerializedChunk {
                instructions: chunk.instructions.iter().map(|inst| {
                    SerializedInstruction {
                        opcode: format!("{:?}", inst.opcode),
                        operand: inst.arg,
                    }
                }).collect(),
                constants: chunk.constants.iter().map(|val| {
                    match val {
                        fluentai_core::value::Value::Integer(i) => SerializedValue::Integer(*i),
                        fluentai_core::value::Value::Float(f) => SerializedValue::Float(*f),
                        fluentai_core::value::Value::String(s) => SerializedValue::String(s.clone()),
                        fluentai_core::value::Value::Boolean(b) => SerializedValue::Boolean(*b),
                        fluentai_core::value::Value::Nil => SerializedValue::Nil,
                        _ => SerializedValue::Nil, // Handle other types as needed
                    }
                }).collect(),
                name: chunk.name.clone(),
            }
        }).collect(),
        main_chunk: bytecode.main_chunk,
    };
    
    // Serialize to JSON bytes
    serde_json::to_vec(&serialized).unwrap_or_else(|_| vec![])
}

/// Link compiled modules into final output
fn link_modules(
    modules: &[CompiledModule],
    output_dir: &Path,
    project_name: &str,
    config: &BuildConfig,
) -> Result<PathBuf> {
    

    let output_file = match config.target {
        BuildTarget::Executable => {
            let exe_name = if cfg!(windows) {
                format!("{}.exe", project_name)
            } else {
                project_name.to_string()
            };
            output_dir.join(exe_name)
        }
        BuildTarget::Library => output_dir.join(format!("lib{}.a", project_name)),
        BuildTarget::WebAssembly => output_dir.join(format!("{}.wasm", project_name)),
    };

    match config.target {
        BuildTarget::Executable => {
            // Create an embedded application
            create_embedded_executable(modules, project_name, &output_file, config)?;
        }
        BuildTarget::Library => {
            // Create a static library
            create_static_library(modules, project_name, &output_file, config)?;
        }
        BuildTarget::WebAssembly => {
            // Create WASM module
            compile_to_wasm(modules, &output_file, config)?;
        }
    }

    Ok(output_file)
}

/// Create a self-contained executable with embedded runtime
fn create_embedded_executable(
    modules: &[CompiledModule],
    project_name: &str,
    output_file: &Path,
    _config: &BuildConfig,
) -> Result<()> {
    // For now, create a placeholder
    // In a real implementation, this would:
    // 1. Create a Rust project that embeds the bytecode
    // 2. Link with fluentai-core-lib
    // 3. Compile to native executable

    // Temporary: just write a marker file
    let content = format!(
        "#!/usr/bin/env fluentai\n# FluentAI Embedded Application: {}\n# Modules: {}\n",
        project_name,
        modules.len()
    );
    fs::write(output_file, content)?;

    // Make it executable on Unix
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(output_file)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(output_file, perms)?;
    }

    Ok(())
}

/// Create a static library
fn create_static_library(
    modules: &[CompiledModule],
    project_name: &str,
    output_file: &Path,
    _config: &BuildConfig,
) -> Result<()> {
    // TODO: Implement static library creation
    let content = format!("FluentAI Static Library: {}\n", project_name);
    fs::write(output_file, content)?;
    Ok(())
}

/// Compile to WebAssembly
fn compile_to_wasm(
    modules: &[CompiledModule],
    output_file: &Path,
    config: &BuildConfig,
) -> Result<PathBuf> {
    use std::process::Command;
    
    println!("Building WebAssembly module...");
    
    // Create a temporary directory for the WASM build
    let temp_dir = tempfile::tempdir()?;
    let wasm_project_dir = temp_dir.path();
    
    // Generate a Rust project that wraps the FluentAI runtime
    generate_wasm_wrapper(wasm_project_dir, modules)?;
    
    // Build the WASM module using wasm-pack
    let wasm_pack_available = Command::new("wasm-pack")
        .arg("--version")
        .output()
        .is_ok();
    
    if wasm_pack_available {
        // Use wasm-pack for a full-featured build
        println!("Building with wasm-pack...");
        
        let mut cmd = Command::new("wasm-pack");
        cmd.arg("build")
            .arg("--target").arg("web")
            .arg("--out-dir").arg(output_file.parent().unwrap_or(Path::new(".")))
            .arg("--out-name").arg(output_file.file_stem().unwrap_or_default())
            .current_dir(wasm_project_dir);
        
        if config.optimization_level > 0 {
            cmd.arg("--release");
        } else {
            cmd.arg("--dev");
        }
        
        let status = cmd.status()?;
        
        if !status.success() {
            return Err(anyhow::anyhow!("wasm-pack build failed"));
        }
    } else {
        // Fallback: Create a basic WASM loader
        println!("wasm-pack not found. Creating basic WASM loader...");
        generate_basic_wasm_loader(output_file, modules)?;
    }
    
    println!("✓ WebAssembly module created: {}", output_file.display());
    Ok(output_file.to_path_buf())
}

/// Generate a Rust wrapper project for WASM compilation
fn generate_wasm_wrapper(project_dir: &Path, modules: &[CompiledModule]) -> Result<()> {
    fs::create_dir_all(project_dir.join("src"))?;
    
    // Create Cargo.toml
    let cargo_toml = r#"[package]
name = "fluentai-wasm"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
wasm-bindgen = "0.2"
fluentai-core = { path = "../../../../../fluentai-core" }
fluentai-vm = { path = "../../../../../fluentai-vm" }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
console_error_panic_hook = "0.1"

[dependencies.web-sys]
version = "0.3"
features = ["console"]
"#;
    fs::write(project_dir.join("Cargo.toml"), cargo_toml)?;
    
    // Create lib.rs with WASM bindings
    let embedded_modules_data = serde_json::to_string(&modules.len()).unwrap_or_default();
    let lib_rs = format!(r#"use wasm_bindgen::prelude::*;
use fluentai_vm::VM;
use fluentai_core::value::Value;

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() {{
    console_error_panic_hook::set_once();
}}

#[wasm_bindgen]
pub struct FluentAIRuntime {{
    vm: VM,
}}

#[wasm_bindgen]
impl FluentAIRuntime {{
    #[wasm_bindgen(constructor)]
    pub fn new() -> Result<FluentAIRuntime, JsValue> {{
        let vm = VM::new();
        Ok(FluentAIRuntime {{ vm }})
    }}
    
    /// Execute FluentAI code
    #[wasm_bindgen]
    pub fn execute(&mut self, code: &str) -> Result<String, JsValue> {{
        // In a real implementation, this would:
        // 1. Parse the code
        // 2. Compile to bytecode
        // 3. Execute in the VM
        // For now, return a placeholder
        Ok(format!("Executed: {{}}", code))
    }}
    
    /// Load pre-compiled bytecode
    #[wasm_bindgen]
    pub fn load_bytecode(&mut self, bytecode_json: &str) -> Result<(), JsValue> {{
        // Deserialize and load bytecode
        Ok(())
    }}
}}

// Embedded bytecode from build
const EMBEDDED_MODULES: &str = "{}";
"#, embedded_modules_data);
    
    let lib_rs_path = project_dir.join("src").join("lib.rs");
    fs::write(lib_rs_path, lib_rs)?;
    
    Ok(())
}

/// Generate a basic WASM loader without wasm-pack
fn generate_basic_wasm_loader(output_file: &Path, modules: &[CompiledModule]) -> Result<()> {
    // Create an HTML file that demonstrates loading
    let html_content = format!(r#"<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>FluentAI WebAssembly Module</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        #output {{ border: 1px solid #ccc; padding: 10px; margin-top: 10px; }}
    </style>
</head>
<body>
    <h1>FluentAI WebAssembly Module</h1>
    <p>This is a placeholder for the FluentAI WASM runtime.</p>
    <p>Modules included: {}</p>
    
    <h2>To use this module:</h2>
    <ol>
        <li>Install wasm-pack: <code>cargo install wasm-pack</code></li>
        <li>Rebuild with: <code>fluentai build --target wasm</code></li>
        <li>Include the generated .wasm and .js files in your web project</li>
    </ol>
    
    <h2>Module Information:</h2>
    <div id="output">
        {}
    </div>
    
    <script>
        // Placeholder for WASM loading code
        console.log("FluentAI WASM Module Loader");
        
        // In a real implementation, this would:
        // 1. Load the .wasm file
        // 2. Instantiate the module
        // 3. Provide JavaScript API bindings
    </script>
</body>
</html>"#, 
        modules.len(),
        modules.iter()
            .map(|m| format!("<div>Module: {} ({} bytes)</div>", m.name, m.bytecode.len()))
            .collect::<Vec<_>>()
            .join("\\n")
    );
    
    // Write the HTML loader
    let html_path = output_file.with_extension("html");
    fs::write(&html_path, html_content)?;
    
    // Write a JavaScript module stub
    let js_content = format!(r#"// FluentAI WebAssembly Module Stub
export class FluentAIRuntime {{
    constructor() {{
        console.log("FluentAI Runtime initialized (stub)");
        this.modules = {{}};
    }}
    
    async execute(code) {{
        console.log("Executing:", code);
        // In a real implementation, this would execute FluentAI code
        return "Result placeholder";
    }}
    
    getModuleInfo() {{
        return {{
            moduleCount: {},
            modules: {}
        }};
    }}
}}

// Module metadata
export const FLUENTAI_MODULES = {};
"#, 
        modules.len(),
        serde_json::to_string(&modules.iter().map(|m| &m.name).collect::<Vec<_>>())?,
        modules.len()
    );
    
    fs::write(output_file, js_content)?;
    
    Ok(())
}
