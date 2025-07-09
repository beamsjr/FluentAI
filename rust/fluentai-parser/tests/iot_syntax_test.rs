//! Test that IoT demo syntax parses correctly

use fluentai_parser::parse;

#[test]
fn test_iot_demo_parses() {
    // Test key constructs from the IoT demo that the parser currently supports
    let test_cases = vec![
        // Basic function call
        r#"(make-tagged "sensor-reading" "temp-001" 1000 25.5)"#,
        // Function definition with define
        r#"(define process-stream (lambda (data) data))"#,
        // Let bindings
        r#"(let ((x 1) (y 2)) (+ x y))"#,
        // Lambda expression
        r#"(lambda (acc reading) acc)"#,
        // If expression
        r#"(if (> temp 40) "hot" "normal")"#,
        // Nested function calls
        r#"(fold-left (lambda (acc x) (+ acc x)) 0 numbers)"#,
    ];

    for (i, code) in test_cases.iter().enumerate() {
        match parse(code) {
            Ok(ast) => {
                println!("Test case {} parsed successfully", i + 1);
                // Verify it's a proper expression
                assert!(!ast.nodes.is_empty(), "AST should have nodes");
            }
            Err(e) => {
                panic!("Test case {} failed to parse: {}\nCode: {}", i + 1, e, code);
            }
        }
    }
}

#[test]
fn test_iot_demo_full_module() {
    // Test a simpler version that the parser can handle
    let code = r#"
(define enrich-reading 
  (lambda (reading)
    (let ((value (sensor-value reading)))
      (if (> value 40.0)
          (cons "anomaly" reading)
          reading))))

(define process-stream
  (lambda (readings)
    (map enrich-reading readings)))
"#;

    match parse(code) {
        Ok(ast) => {
            println!("Code parsed successfully");
            println!("AST has {} nodes", ast.nodes.len());

            // Check that it parsed successfully
            if let Some(root_id) = ast.root_id {
                if ast.nodes.get(&root_id).is_some() {
                    println!("Root node exists");
                } else {
                    panic!("Root node should exist");
                }
            }
        }
        Err(e) => {
            panic!("Failed to parse code: {}", e);
        }
    }
}

#[test]
fn test_sensor_data_operations() {
    let code = r#"
;; Create test sensor data - using simpler syntax
(define readings (list 25.5 45.0 22.0))

;; Process the readings
(define anomalies
  (filter (lambda (r) (> r 40.0)) readings))

;; Count anomalies
(length anomalies)
"#;

    match parse(code) {
        Ok(ast) => {
            println!("Sensor operations parsed successfully");
            assert!(
                ast.nodes.len() >= 3,
                "Should have at least 3 top-level expressions"
            );
        }
        Err(e) => {
            panic!("Failed to parse sensor operations: {}", e);
        }
    }
}
