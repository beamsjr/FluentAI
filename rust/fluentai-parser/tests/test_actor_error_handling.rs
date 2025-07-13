//! Tests for actor definition error handling

use fluentai_parser::parse_flc;

#[test]
fn test_state_field_after_handler_error() {
    let source = r#"
        private actor BadActor {
            private handle process() {
                42
            }
            
            // This should fail - state field after handler
            count: int = 0;
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_err());
    let error = result.unwrap_err().to_string();
    assert!(error.contains("cannot be declared after handlers"), "Error: {}", error);
}

#[test]
fn test_duplicate_handler_names_error() {
    let source = r#"
        private actor Counter {
            count: int = 0;
            
            private handle inc() {
                count + 1
            }
            
            // This should fail - duplicate handler name
            private handle inc() {
                count + 2
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_err());
    let error = result.unwrap_err().to_string();
    assert!(error.contains("Duplicate handler"), "Error: {}", error);
}

#[test]
fn test_duplicate_state_field_error() {
    let source = r#"
        private actor BadActor {
            count: int = 0;
            count: float = 0.0;  // This should fail - duplicate field
            
            private handle get() {
                count
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_err());
    let error = result.unwrap_err().to_string();
    assert!(error.contains("Duplicate state field"), "Error: {}", error);
}

#[test]
fn test_field_handler_name_conflict_error() {
    let source = r#"
        private actor Conflicted {
            process: string = "data";
            
            // This should fail - handler name conflicts with field
            private handle process() {
                "processing"
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_err());
    let error = result.unwrap_err().to_string();
    assert!(error.contains("conflicts with state field"), "Error: {}", error);
}

#[test]
fn test_actor_without_handlers_error() {
    let source = r#"
        private actor EmptyActor {
            count: int = 0;
            // No handlers - should fail
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_err());
    let error = result.unwrap_err().to_string();
    assert!(error.contains("must have at least one handler"), "Error: {}", error);
}

#[test]
fn test_duplicate_parameter_names_error() {
    let source = r#"
        private actor Calculator {
            private handle calculate(x: int, x: float) {  // Duplicate param name
                x
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_err());
    let error = result.unwrap_err().to_string();
    assert!(error.contains("Duplicate parameter name"), "Error: {}", error);
}

#[test]
fn test_empty_handler_name_error() {
    // This test would require a malformed token stream, which is harder to test
    // through the parser interface. The validation is there but triggering it
    // requires lexer-level manipulation.
}

#[test]
fn test_mixed_state_and_handlers_error() {
    let source = r#"
        private actor Mixed {
            x: int = 0;
            
            private handle process() {
                x
            }
            
            y: int = 0;  // State field after handler
            
            private handle get() {
                y
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_err());
    let error = result.unwrap_err().to_string();
    assert!(error.contains("cannot be declared after handlers"), "Error: {}", error);
}

#[test]
fn test_valid_actor_with_multiple_handlers() {
    let source = r#"
        private actor ValidActor {
            x: int = 0;
            y: float = 0.0;
            
            private handle inc_x(n: int) {
                x + n
            }
            
            private handle inc_y(n: float) {
                y + n
            }
            
            private handle get_sum() -> float {
                x + y
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Valid actor should parse successfully: {:?}", result);
}

#[test]
fn test_handler_with_no_params() {
    let source = r#"
        private actor Simple {
            private handle ping() {
                "pong"
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Handler with no params should be valid: {:?}", result);
}

#[test]
fn test_handler_with_typed_params() {
    let source = r#"
        private actor Typed {
            private handle process(msg: string, priority: int) -> string {
                f"Processing {msg} with priority {priority}"
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Handler with typed params should be valid: {:?}", result);
}

#[test]
fn test_actor_with_functions() {
    let source = r#"
        private actor WithFunctions {
            data: string = "";
            
            private handle set(value: string) {
                data = value
            }
            
            private function helper(x: int) -> int {
                x * 2
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Actor with helper functions should be valid: {:?}", result);
}