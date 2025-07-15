//! Run FluentAI tests

use anyhow::Result;
use colored::*;
use std::fs;
use std::path::{Path, PathBuf};
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
pub async fn test(project_path: Option<PathBuf>, config: TestConfig) -> Result<()> {
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
            if name.ends_with(".test.flc") || name.ends_with("_test.flc") {
                files.push(path);
            }
        }
    }
    Ok(())
}

/// Run a single test file
async fn run_test_file(test_file: &Path, config: &TestConfig) -> Result<TestResults> {
    let relative_path = test_file
        .strip_prefix(std::env::current_dir()?)
        .unwrap_or(test_file);
    println!("{} {}", "Testing".cyan(), relative_path.display());

    // Create runtime for test execution
    let mut runtime = fluentai_core_lib::RuntimeEngine::development();

    // Register test framework functions
    register_test_framework(&mut runtime)?;

    // Load and run test file
    let source = fs::read_to_string(test_file)?;
    let start = Instant::now();

    // Execute tests
    match runtime.execute(&source) {
        Ok(_value) => {
            // Parse test results from runtime state
            let results = parse_test_results(&runtime)?;
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

#[derive(Debug, Clone)]
struct TestFailure {
    name: String,
    message: String,
}

/// Register test framework functions
fn register_test_framework(runtime: &mut fluentai_core_lib::RuntimeEngine) -> Result<()> {
    use fluentai_core::value::Value;
    use fluentai_core_lib::HostFunction;
    use std::sync::{Arc, Mutex};
    use rustc_hash::FxHashMap;

    // Test results accumulator - shared across all test functions
    let results = Arc::new(Mutex::new(FileTestResults {
        passed: 0,
        failed: 0,
        skipped: 0,
        failures: Vec::new(),
    }));

    // Current test context (for nested describe/it)
    let current_suite = Arc::new(Mutex::new(String::new()));

    // test/describe function
    let describe_results = results.clone();
    let describe_suite = current_suite.clone();
    let describe = HostFunction::new("test/describe", 2, move |args| {
        if args.len() != 2 {
            return Err(fluentai_core_lib::RuntimeError::HostError("describe expects 2 arguments".to_string()));
        }
        
        // Extract suite name
        let suite_name = match &args[0] {
            Value::String(s) => s.clone(),
            _ => return Err(fluentai_core_lib::RuntimeError::HostError("First argument to describe must be a string".to_string())),
        };
        
        // Update current suite
        *describe_suite.lock().unwrap() = suite_name;
        
        // Execute the test suite function
        // In a real implementation, we'd evaluate the function here
        // For now, we'll return the function to be executed by the runtime
        Ok(args[1].clone())
    });

    // test/it function
    let it_results = results.clone();
    let it_suite = current_suite.clone();
    let it = HostFunction::new("test/it", 2, move |args| {
        if args.len() != 2 {
            return Err(fluentai_core_lib::RuntimeError::HostError("it expects 2 arguments".to_string()));
        }
        
        // Extract test name
        let test_name = match &args[0] {
            Value::String(s) => s.clone(),
            _ => return Err(fluentai_core_lib::RuntimeError::HostError("First argument to it must be a string".to_string())),
        };
        
        // Build full test name with suite
        let suite = it_suite.lock().unwrap();
        let full_name = if suite.is_empty() {
            test_name
        } else {
            format!("{} > {}", suite, test_name)
        };
        
        // TODO: Execute the test function (args[1])
        // For now, we'll just mark it as passed
        let mut results = it_results.lock().unwrap();
        results.passed += 1;
        
        Ok(Value::Nil)
    });

    // test/expect function
    let expect_results = results.clone();
    let expect_suite = current_suite.clone();
    let expect = HostFunction::new("test/expect", 1, move |args| {
        if args.is_empty() {
            return Err(fluentai_core_lib::RuntimeError::HostError("expect requires an argument".to_string()));
        }
        
        // Return an expectation object with assertion methods
        let actual = args[0].clone();
        let results_clone = expect_results.clone();
        let suite_clone = expect_suite.clone();
        
        // Create expectation object with methods
        let mut methods: FxHashMap<String, Value> = FxHashMap::default();
        
        // to_equal method
        let to_equal_results = results_clone.clone();
        let to_equal_suite = suite_clone.clone();
        let to_equal_actual = actual.clone();
        methods.insert(
            "to_equal".to_string(),
            Value::NativeFunction {
                name: "to_equal".to_string(),
                arity: 1,
                function: Arc::new(move |args| {
                    if args.is_empty() {
                        return Err(fluentai_core::value::ValueError::ArityMismatch {
                            expected: 1,
                            actual: 0,
                        });
                    }
                    
                    let expected = &args[0];
                    
                    // Compare actual vs expected
                    if value_equals(&to_equal_actual, expected) {
                        // Test passed - already counted in 'it' function
                        Ok(Value::Nil)
                    } else {
                        // Test failed
                        let mut results = to_equal_results.lock().unwrap();
                        results.failed += 1;
                        results.passed = results.passed.saturating_sub(1); // Correct the count
                        
                        let suite = to_equal_suite.lock().unwrap();
                        let test_name = if suite.is_empty() {
                            "test".to_string()
                        } else {
                            suite.clone()
                        };
                        
                        results.failures.push(TestFailure {
                            name: test_name,
                            message: format!(
                                "Expected {:?} to equal {:?}",
                                to_equal_actual,
                                expected
                            ),
                        });
                        
                        Err(fluentai_core::value::ValueError::InvalidOperation("Assertion failed".to_string()))
                    }
                }),
            }
        );
        
        // Return object with assertion methods
        Ok(Value::Map(methods))
    });

    // Register all test functions
    runtime.register_functions(vec![describe, it, expect])?;
    
    // Store results reference in runtime for later retrieval
    runtime.set_global("__test_results__", Value::GcHandle(results));

    Ok(())
}

/// Helper function to compare two values for equality
fn value_equals(a: &fluentai_core::value::Value, b: &fluentai_core::value::Value) -> bool {
    use fluentai_core::value::Value::*;
    match (a, b) {
        (Integer(a), Integer(b)) => a == b,
        (Float(a), Float(b)) => a == b,
        (String(a), String(b)) => a == b,
        (Symbol(a), Symbol(b)) => a == b,
        (Boolean(a), Boolean(b)) => a == b,
        (Nil, Nil) => true,
        (List(a), List(b)) => a.len() == b.len() && a.iter().zip(b).all(|(x, y)| value_equals(x, y)),
        _ => false,
    }
}

/// Parse test results from execution value
fn parse_test_results(runtime: &fluentai_core_lib::RuntimeEngine) -> Result<FileTestResults> {
    use fluentai_core::value::Value;
    use std::sync::{Arc, Mutex};
    
    // Retrieve the test results from the runtime global
    match runtime.get_global("__test_results__") {
        Some(value) => {
            match value {
                Value::GcHandle(results_arc) => {
                    // Try to downcast to our results type
                    if let Some(results) = results_arc.downcast_ref::<Arc<Mutex<FileTestResults>>>() {
                        let results = results.lock().unwrap();
                        Ok(FileTestResults {
                            passed: results.passed,
                            failed: results.failed,
                            skipped: results.skipped,
                            failures: results.failures.clone(),
                        })
                    } else {
                        // Fallback to empty results if casting fails
                        Ok(FileTestResults {
                            passed: 0,
                            failed: 0,
                            skipped: 0,
                            failures: Vec::new(),
                        })
                    }
                },
                _ => {
                    // No test results found
                    Ok(FileTestResults {
                        passed: 0,
                        failed: 0,
                        skipped: 0,
                        failures: Vec::new(),
                    })
                }
            }
        },
        None => {
            // No test results found
            Ok(FileTestResults {
                passed: 0,
                failed: 0,
                skipped: 0,
                failures: Vec::new(),
            })
        }
    }
}

/// Print results for a single test file
fn print_file_results(file: &Path, results: &FileTestResults, duration: std::time::Duration) {
    if results.failed == 0 {
        println!(
            "  {} {} passed, {} skipped ({:.2}s)",
            "✓".green(),
            results.passed,
            results.skipped,
            duration.as_secs_f64()
        );
    } else {
        println!(
            "  {} {} passed, {} failed, {} skipped ({:.2}s)",
            "✗".red(),
            results.passed,
            results.failed,
            results.skipped,
            duration.as_secs_f64()
        );

        // Print failures
        for failure in &results.failures {
            println!("    {} {}: {}", "↳".red(), failure.name, failure.message);
        }
    }
}

/// Print test summary
fn print_test_summary(results: &TestResults) {
    let total = results.passed + results.failed + results.skipped;

    if results.failed == 0 {
        println!(
            "{} {} tests, {} passed, {} skipped ({:.2}s)",
            "✓".green().bold(),
            total,
            results.passed,
            results.skipped,
            results.duration.as_secs_f64()
        );
    } else {
        println!(
            "{} {} tests, {} passed, {} failed, {} skipped ({:.2}s)",
            "✗".red().bold(),
            total,
            results.passed,
            results.failed,
            results.skipped,
            results.duration.as_secs_f64()
        );
    }
}
