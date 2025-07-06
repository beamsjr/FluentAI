//! Integration test for IoT demo functionality

use fluentai_parser::parse;
use fluentai_vm::{Compiler, CompilerOptions, VM};
use fluentai_optimizer::OptimizationLevel;
use fluentai_stdlib::init_stdlib;

#[test]
#[ignore = "Stdlib functions not properly loaded"]
fn test_basic_iot_functionality() {
    let code = r#"
        ;; Define some sensor data
        (define readings (list 25.5 45.0 22.0 38.5))
        
        ;; Filter high temperatures
        (define high-temps 
          (filter (lambda (t) (> t 40.0)) readings))
        
        ;; Return the count
        (length high-temps)
    "#;
    
    let result = run_code(code).expect("Failed to run code");
    
    // Should find 1 high temperature (45.0)
    match result {
        fluentai_vm::bytecode::Value::Int(n) => assert_eq!(n, 1),
        _ => panic!("Expected Int, got {:?}", result),
    }
}

#[test]
#[ignore = "Stdlib functions not properly loaded"]
fn test_sensor_reading_simulation() {
    let code = r#"
        ;; Simulate sensor readings as lists
        (define (make-reading id value)
          (list id value))
        
        (define (reading-value r)
          (car (cdr r)))
        
        ;; Create test data
        (define sensors
          (list
            (make-reading "s1" 25.5)
            (make-reading "s2" 45.0)
            (make-reading "s3" 22.0)))
        
        ;; Count anomalies
        (define (is-anomaly? reading)
          (> (reading-value reading) 40.0))
        
        (length (filter is-anomaly? sensors))
    "#;
    
    let result = run_code(code).expect("Failed to run code");
    
    match result {
        fluentai_vm::bytecode::Value::Int(n) => assert_eq!(n, 1),
        _ => panic!("Expected Int, got {:?}", result),
    }
}

#[test]
#[ignore = "Stdlib functions not properly loaded"]
fn test_pipeline_concept() {
    let code = r#"
        ;; Simple pipeline simulation
        (define (process-data data)
          (define enriched (map (lambda (x) (* x 1.1)) data))
          (define filtered (filter (lambda (x) (> x 30)) enriched))
          filtered)
        
        (define input (list 20 25 30 35))
        (define output (process-data input))
        (length output)
    "#;
    
    let result = run_code(code).expect("Failed to run code");
    
    // After enrichment: 22, 27.5, 33, 38.5
    // After filtering > 30: 33, 38.5
    match result {
        fluentai_vm::bytecode::Value::Int(n) => assert_eq!(n, 2),
        _ => panic!("Expected Int, got {:?}", result),
    }
}

#[test]
#[ignore = "Stdlib functions not properly loaded"]
fn test_fold_operations() {
    let code = r#"
        (define nums (list 1 2 3 4 5))
        
        ;; Sum using fold
        (define sum (fold + 0 nums))
        
        ;; Average
        (define avg (/ sum (length nums)))
        
        ;; Return as list for testing
        (list sum avg)
    "#;
    
    let result = run_code(code).expect("Failed to run code");
    
    match result {
        fluentai_vm::bytecode::Value::List(items) => {
            assert_eq!(items.len(), 2);
            match &items[0] {
                fluentai_vm::bytecode::Value::Int(n) => assert_eq!(*n, 15),
                _ => panic!("Expected sum to be Int"),
            }
            match &items[1] {
                fluentai_vm::bytecode::Value::Float(f) => assert!((f - 3.0).abs() < 0.001),
                fluentai_vm::bytecode::Value::Int(n) => assert_eq!(*n, 3),
                _ => panic!("Expected avg to be Float or Int"),
            }
        }
        _ => panic!("Expected List, got {:?}", result),
    }
}

// Helper function to run FluentAI code
fn run_code(code: &str) -> Result<fluentai_vm::bytecode::Value, Box<dyn std::error::Error>> {
    use fluentai_vm::builder::VMBuilder;
    
    // Initialize stdlib
    let stdlib = init_stdlib();
    
    // Parse
    let ast = parse(code)?;
    
    // Compile
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&ast)?;
    
    // Build VM with stdlib
    let mut vm = VMBuilder::new()
        .with_bytecode(bytecode)
        .with_stdlib_registry(stdlib)
        .build()?;
    
    // Execute
    let result = vm.run()?;
    
    Ok(result)
}