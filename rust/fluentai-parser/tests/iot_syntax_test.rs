//! Test that IoT demo syntax parses correctly

use fluentai_parser::parse;
use fluentai_core::ast::Node;

#[test]
fn test_iot_demo_parses() {
    // Test key constructs from the IoT demo
    let test_cases = vec![
        // Basic sensor reading creation
        r#"(make-tagged "sensor-reading" "temp-001" 1000 25.5 (make-map "type" "temperature"))"#,
        
        // Module definition
        r#"(module iot-types (export sensor-reading?))"#,
        
        // Function with let bindings
        r#"(define (process-stream data)
             (let ((enriched (map enrich data)))
               (filter anomaly? enriched)))"#,
        
        // Effect syntax
        r#"(effect io print-line "Hello")"#,
        
        // Cond expression
        r#"(cond
             ((> temp 40) "hot")
             ((< temp 0) "cold")
             (else "normal"))"#,
        
        // Lambda with multiple operations
        r#"(fold-left 
             (lambda (acc reading)
               (if (anomaly? reading)
                   (cons reading acc)
                   acc))
             []
             readings)"#,
        
        // Stream operations (pipe syntax)
        r#"(|> stream
             (stream-map enrich)
             (stream-filter anomaly?)
             (stream-collect))"#,
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
    let module_code = r#"
(module iot-pipeline
  (import iot-types)
  (export process-stream)
  
  (define (enrich-reading reading)
    (let ((metadata (sensor-metadata reading)))
      (make-sensor-reading
        (sensor-id reading)
        (sensor-timestamp reading)
        (sensor-value reading)
        (map-merge metadata (make-map "enriched" true)))))
  
  (define (anomaly? reading)
    (> (sensor-value reading) 40.0))
  
  (define (process-stream readings)
    (|> readings
        (map enrich-reading)
        (filter anomaly?)
        (map log-anomaly))))
"#;
    
    match parse(module_code) {
        Ok(ast) => {
            println!("Full module parsed successfully");
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
            panic!("Failed to parse module: {}", e);
        }
    }
}

#[test]
fn test_sensor_data_operations() {
    let code = r#"
;; Create test sensor data
(define readings
  [(make-sensor-reading "temp-001" 1000 25.5 (make-map "type" "temperature"))
   (make-sensor-reading "temp-002" 1100 45.0 (make-map "type" "temperature"))
   (make-sensor-reading "temp-003" 1200 22.0 (make-map "type" "temperature"))])

;; Process the readings
(define anomalies
  (filter (lambda (r) (> (sensor-value r) 40.0)) readings))

;; Count anomalies
(length anomalies)
"#;
    
    match parse(code) {
        Ok(ast) => {
            println!("Sensor operations parsed successfully");
            assert!(ast.nodes.len() >= 3, "Should have at least 3 top-level expressions");
        }
        Err(e) => {
            panic!("Failed to parse sensor operations: {}", e);
        }
    }
}