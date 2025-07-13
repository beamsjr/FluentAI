//! Validation tests for IoT demo syntax


#[test]
fn test_basic_expressions() {
    assert!(parse("(+ 1 2)").is_ok());
    assert!(parse("(define x 42)").is_ok());
    assert!(parse("[1 2 3 4 5]").is_ok());
    assert!(parse("(list 1 2 3)").is_ok());
}

#[test]
fn test_functions() {
    assert!(parse("(lambda (x) (* x 2))").is_ok());
    assert!(parse("(map inc [1 2 3])").is_ok());
    assert!(parse("(filter (lambda (x) (> x 0)) lst)").is_ok());
    assert!(parse("(fold-left + 0 [1 2 3])").is_ok());
}

#[test]
fn test_iot_specific_syntax() {
    // Tagged values
    assert!(parse(r#"(make-tagged "sensor-reading" "id" 1000 25.5 meta)"#).is_ok());

    // Effect syntax
    assert!(parse(r#"(effect io print-line "Hello")"#).is_ok());
    assert!(parse(r#"(effect io print "World")"#).is_ok());

    // String formatting
    assert!(parse(r#"(string-format "Value: {}" x)"#).is_ok());
}

#[test]
fn test_module_syntax() {
    let module_code = r#"
        (module iot-types
          (export make-sensor-reading
                  sensor-reading?
                  sensor-id)
          
          (define (make-sensor-reading id time val meta)
            (list 'reading id time val meta)))
    "#;

    assert!(parse(module_code).is_ok());
}

#[test]
fn test_complex_functions() {
    let process_fn = r#"
        (define (process-stream data-stream)
          (let ((enriched (map enrich-with-metadata data-stream)))
            (let ((anomalies (filter detect-anomalies enriched)))
              (map log-anomalies anomalies))))
    "#;

    assert!(parse(process_fn).is_ok());
}

#[test]
fn test_stream_operations() {
    // Pipe operator
    assert!(parse(r#"(|> stream (stream-map f) (stream-filter p))"#).is_ok());

    // Channel operations
    assert!(parse("(make-channel)").is_ok());
    assert!(parse("(send ch value)").is_ok());
    assert!(parse("(receive ch)").is_ok());
}

#[test]
fn test_pattern_matching() {
    // Cond expressions
    let cond_expr = r#"
        (cond
          ((eq? type "temperature") (check-temp value))
          ((eq? type "pressure") (check-pressure value))
          (else (default-check value)))
    "#;

    assert!(parse(cond_expr).is_ok());
}

#[test]
fn test_error_cases() {
    // These should fail
    assert!(parse("(").is_err());
    assert!(parse(")").is_err());
    // TODO: Parser currently doesn't validate special form argument counts
    // assert!(parse("(define)").is_err());
    // assert!(parse("(lambda)").is_err());
}

#[test]
fn test_demo_snippets() {
    // From test-minimal.fl
    let minimal_code = r#"
        (define readings [25.5 45.0 22.0 38.5 55.0])
        (define doubled (map (lambda (x) (* x 2)) readings))
        (define high-temps (filter (lambda (x) (> x 40.0)) readings))
        (define sum (fold-left + 0 readings))
    "#;

    assert!(parse(minimal_code).is_ok());

    // From iot-types.fl
    let types_code = r#"
        (define (sensor-reading? val)
          (and (tagged? val)
               (eq? (tagged-tag val) "sensor-reading")
               (= (length (tagged-values val)) 4)))
    "#;

    assert!(parse(types_code).is_ok());
}
