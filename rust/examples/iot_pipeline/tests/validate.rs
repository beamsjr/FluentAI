// Validation test for IoT demo syntax

fn main() {
    println!("Validating IoT demo FluentAI syntax...\n");
    
    // Test 1: Basic expressions
    test_parse("(+ 1 2)", "Basic addition");
    test_parse("(define x 42)", "Define variable");
    test_parse("[1 2 3 4 5]", "List literal");
    
    // Test 2: Functions
    test_parse("(lambda (x) (* x 2))", "Lambda function");
    test_parse("(map inc [1 2 3])", "Map function");
    test_parse("(filter (lambda (x) (> x 0)) lst)", "Filter function");
    
    // Test 3: Tagged values (as used in demo)
    test_parse(r#"(make-tagged "sensor-reading" "temp-001" 1000 25.5 (make-map "type" "temperature"))"#, 
               "Tagged value creation");
    
    // Test 4: Effect syntax
    test_parse(r#"(effect io print-line "Hello")"#, "Effect syntax");
    
    // Test 5: Module syntax
    test_parse(r#"(module iot-types (export foo))"#, "Module definition");
    
    // Test 6: Complex expression from demo
    test_parse(r#"
        (define (process-stream data)
          (let ((enriched (map enrich data)))
            (filter anomaly? enriched)))
    "#, "Function with let binding");
    
    println!("\nValidation complete!");
}

fn test_parse(code: &str, description: &str) {
    print!("Testing {}: ", description);
    match parse(code) {
        Ok(_) => println!("✓ OK"),
        Err(e) => println!("✗ FAILED: {}", e),
    }
}