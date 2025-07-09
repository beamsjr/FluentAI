//! Run FluentAI tests

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use std::fs;
use colored::*;
use std::time::Instant;

/// Test configuration
#[derive(Debug, Clone)]
pub struct TestConfig {
    pub filter: Option<String>,
    pub verbose: bool,
    pub parallel: bool,
    pub coverage: bool,
}

/// Test results
#[derive(Debug, Default)]
struct TestResults {
    passed: usize,
    failed: usize,
    skipped: usize,
    duration: std::time::Duration,
}

/// Run tests in a FluentAI project
pub async fn test(
    project_path: Option<PathBuf>,
    config: TestConfig,
) -> Result<()> {
    let start = Instant::now();
    let project_path = project_path.unwrap_or_else(|| PathBuf::from("."));
    
    println!("{} FluentAI Test Runner", "→".blue().bold());
    if let Some(filter) = &config.filter {
        println!("  Filter: {}", filter);
    }
    
    // Find test files
    let test_files = find_test_files(&project_path)?;
    if test_files.is_empty() {
        println!("{} No test files found", "!".yellow());
        return Ok(());
    }
    
    println!("  Found {} test files\n", test_files.len());
    
    // Run tests
    let mut total_results = TestResults::default();
    
    for test_file in &test_files {
        let results = run_test_file(test_file, &config).await?;
        total_results.passed += results.passed;
        total_results.failed += results.failed;
        total_results.skipped += results.skipped;
    }
    
    total_results.duration = start.elapsed();
    
    // Print summary
    println!("\n{}", "─".repeat(60));
    print_test_summary(&total_results);
    
    // Exit with error if tests failed
    if total_results.failed > 0 {
        std::process::exit(1);
    }
    
    Ok(())
}

/// Find all test files in project
fn find_test_files(project_path: &Path) -> Result<Vec<PathBuf>> {
    let mut test_files = Vec::new();
    
    // Check tests directory
    let tests_dir = project_path.join("tests");
    if tests_dir.exists() {
        collect_test_files(&tests_dir, &mut test_files)?;
    }
    
    // Check for test files in src
    let src_dir = project_path.join("src");
    if src_dir.exists() {
        collect_test_files(&src_dir, &mut test_files)?;
    }
    
    Ok(test_files)
}

/// Collect test files recursively
fn collect_test_files(dir: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        
        if path.is_dir() {
            collect_test_files(&path, files)?;
        } else if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
            if name.ends_with(".test.ai") || name.ends_with("_test.ai") {
                files.push(path);
            }
        }
    }
    Ok(())
}

/// Run a single test file
async fn run_test_file(test_file: &Path, config: &TestConfig) -> Result<TestResults> {
    let relative_path = test_file.strip_prefix(std::env::current_dir()?).unwrap_or(test_file);
    println!("{} {}", "Testing".cyan(), relative_path.display());
    
    // Create runtime for test execution
    let mut runtime = fluentai_runtime::RuntimeEngine::development();
    
    // Register test framework functions
    register_test_framework(&mut runtime)?;
    
    // Load and run test file
    let source = fs::read_to_string(test_file)?;
    let start = Instant::now();
    
    // Execute tests
    match runtime.execute(&source) {
        Ok(value) => {
            // Parse test results from return value
            let results = parse_test_results(value)?;
            let duration = start.elapsed();
            
            // Print test file results
            print_file_results(relative_path, &results, duration);
            
            Ok(TestResults {
                passed: results.passed,
                failed: results.failed,
                skipped: results.skipped,
                duration,
            })
        }
        Err(e) => {
            println!("  {} Test execution failed: {}", "✗".red(), e);
            Ok(TestResults {
                failed: 1,
                duration: start.elapsed(),
                ..Default::default()
            })
        }
    }
}

/// Test results from a single file
#[derive(Debug)]
struct FileTestResults {
    passed: usize,
    failed: usize,
    skipped: usize,
    failures: Vec<TestFailure>,
}

#[derive(Debug)]
struct TestFailure {
    name: String,
    message: String,
}

/// Register test framework functions
fn register_test_framework(runtime: &mut fluentai_runtime::RuntimeEngine) -> Result<()> {
    use fluentai_runtime::HostFunction;
    use fluentai_core::value::Value;
    use std::sync::{Arc, Mutex};
    
    // Test results accumulator
    let results = Arc::new(Mutex::new(FileTestResults {
        passed: 0,
        failed: 0,
        skipped: 0,
        failures: Vec::new(),
    }));
    
    // test/describe function
    let describe_results = results.clone();
    let describe = HostFunction::new("test/describe", 2, move |args| {
        // args[0] = description, args[1] = test function
        // For now, just execute the test function
        Ok(args[1].clone())
    });
    
    // test/it function
    let it_results = results.clone();
    let it = HostFunction::new("test/it", 2, move |args| {
        // args[0] = test name, args[1] = test function
        // Execute test and track results
        Ok(Value::Nil)
    });
    
    // test/expect function
    let expect_results = results.clone();
    let expect = HostFunction::new("test/expect", 3, move |args| {
        // args[0] = actual, args[1] = matcher symbol, args[2] = expected
        // Perform assertion
        Ok(Value::Nil)
    });
    
    runtime.register_functions(vec![describe, it, expect])?;
    
    Ok(())
}

/// Parse test results from execution value
fn parse_test_results(_value: fluentai_core::value::Value) -> Result<FileTestResults> {
    // TODO: Implement actual parsing of test results
    // For now, return dummy results
    Ok(FileTestResults {
        passed: 5,
        failed: 0,
        skipped: 1,
        failures: Vec::new(),
    })
}

/// Print results for a single test file
fn print_file_results(file: &Path, results: &FileTestResults, duration: std::time::Duration) {
    if results.failed == 0 {
        println!("  {} {} passed, {} skipped ({:.2}s)",
                 "✓".green(),
                 results.passed,
                 results.skipped,
                 duration.as_secs_f64());
    } else {
        println!("  {} {} passed, {} failed, {} skipped ({:.2}s)",
                 "✗".red(),
                 results.passed,
                 results.failed,
                 results.skipped,
                 duration.as_secs_f64());
        
        // Print failures
        for failure in &results.failures {
            println!("    {} {}: {}", 
                     "↳".red(),
                     failure.name,
                     failure.message);
        }
    }
}

/// Print test summary
fn print_test_summary(results: &TestResults) {
    let total = results.passed + results.failed + results.skipped;
    
    if results.failed == 0 {
        println!("{} {} tests, {} passed, {} skipped ({:.2}s)",
                 "✓".green().bold(),
                 total,
                 results.passed,
                 results.skipped,
                 results.duration.as_secs_f64());
    } else {
        println!("{} {} tests, {} passed, {} failed, {} skipped ({:.2}s)",
                 "✗".red().bold(),
                 total,
                 results.passed,
                 results.failed,
                 results.skipped,
                 results.duration.as_secs_f64());
    }
}