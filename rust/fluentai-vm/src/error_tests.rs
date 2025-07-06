//! Tests for VM error handling

#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::error::{VMError, StackTrace, StackFrame, SourceLocation, value_type_name};
    use crate::bytecode::Value;
    
    #[test]
    fn test_stack_overflow_error() {
        let error = VMError::StackOverflow {
            current_depth: 1001,
            max_depth: 1000,
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Stack overflow"));
        assert!(msg.contains("1001"));
        assert!(msg.contains("1000"));
    }
    
    #[test]
    fn test_stack_overflow_with_trace() {
        let mut trace = StackTrace::new();
        trace.push_frame(StackFrame {
            function_name: "recursive_fn".to_string(),
            chunk_id: 1,
            ip: 42,
            location: Some(SourceLocation {
                file: Some("test.fl".to_string()),
                line: 10,
                column: 5,
                function: Some("recursive_fn".to_string()),
            }),
        });
        
        let error = VMError::StackOverflow {
            current_depth: 1001,
            max_depth: 1000,
            stack_trace: Some(trace),
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Stack overflow"));
        assert!(msg.contains("Stack trace:"));
        assert!(msg.contains("recursive_fn"));
        assert!(msg.contains("test.fl:10:5"));
    }
    
    #[test]
    fn test_stack_underflow_error() {
        let error = VMError::StackUnderflow {
            operation: "pop".to_string(),
            stack_size: 0,
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Stack underflow"));
        assert!(msg.contains("pop"));
        assert!(msg.contains("0"));
    }
    
    #[test]
    fn test_call_stack_overflow() {
        let error = VMError::CallStackOverflow {
            current_depth: 256,
            max_depth: 255,
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Call stack overflow"));
        assert!(msg.contains("256"));
        assert!(msg.contains("255"));
    }
    
    #[test]
    fn test_type_error() {
        let error = VMError::TypeError {
            operation: "add".to_string(),
            expected: "number".to_string(),
            got: "string".to_string(),
            location: Some(SourceLocation {
                file: Some("math.fl".to_string()),
                line: 5,
                column: 10,
                function: Some("calculate".to_string()),
            }),
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Type error"));
        assert!(msg.contains("add"));
        assert!(msg.contains("expected number"));
        assert!(msg.contains("got string"));
        assert!(msg.contains("math.fl:5:10"));
    }
    
    #[test]
    fn test_arithmetic_error() {
        let error = VMError::ArithmeticError {
            operation: "sqrt".to_string(),
            message: "cannot take square root of negative number".to_string(),
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Arithmetic error"));
        assert!(msg.contains("sqrt"));
        assert!(msg.contains("cannot take square root"));
    }
    
    #[test]
    fn test_division_by_zero() {
        let error = VMError::DivisionByZero {
            location: Some(SourceLocation {
                file: None,
                line: 3,
                column: 7,
                function: None,
            }),
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Division by zero"));
        assert!(msg.contains("<unknown>:3:7"));
    }
    
    #[test]
    fn test_integer_overflow() {
        let error = VMError::IntegerOverflow {
            operation: "mul".to_string(),
            operands: (i64::MAX, 2),
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Integer overflow"));
        assert!(msg.contains("mul"));
        assert!(msg.contains(&i64::MAX.to_string()));
        assert!(msg.contains("2"));
    }
    
    #[test]
    fn test_invalid_opcode() {
        let error = VMError::InvalidOpcode {
            opcode: 0xFF,
            location: Some(SourceLocation {
                file: Some("bytecode.fl".to_string()),
                line: 1,
                column: 1,
                function: None,
            }),
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Invalid opcode"));
        assert!(msg.contains("0xff"));
        assert!(msg.contains("bytecode.fl:1:1"));
    }
    
    #[test]
    fn test_invalid_constant_index() {
        let error = VMError::InvalidConstantIndex {
            index: 42,
            max_index: 10,
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Invalid constant index"));
        assert!(msg.contains("42"));
        assert!(msg.contains("10"));
    }
    
    #[test]
    fn test_invalid_local_index() {
        let error = VMError::InvalidLocalIndex {
            index: 5,
            frame_size: 3,
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Invalid local variable index"));
        assert!(msg.contains("5"));
        assert!(msg.contains("3"));
    }
    
    #[test]
    fn test_invalid_jump_target() {
        let error = VMError::InvalidJumpTarget {
            target: 1000,
            chunk_size: 500,
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Invalid jump target"));
        assert!(msg.contains("1000"));
        assert!(msg.contains("500"));
    }
    
    #[test]
    fn test_resource_limit_exceeded() {
        let error = VMError::ResourceLimitExceeded {
            resource: "memory".to_string(),
            limit: 1_000_000,
            requested: 2_000_000,
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Resource limit exceeded"));
        assert!(msg.contains("memory"));
        assert!(msg.contains("2000000"));
        assert!(msg.contains("1000000"));
    }
    
    #[test]
    fn test_module_error() {
        let error = VMError::ModuleError {
            module_name: "network".to_string(),
            message: "connection refused".to_string(),
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Module error"));
        assert!(msg.contains("network"));
        assert!(msg.contains("connection refused"));
    }
    
    #[test]
    fn test_async_error() {
        let error = VMError::AsyncError {
            message: "channel closed".to_string(),
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Async error"));
        assert!(msg.contains("channel closed"));
    }
    
    #[test]
    fn test_cell_error() {
        let error = VMError::CellError {
            index: 3,
            message: "cell is not initialized".to_string(),
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Cell error"));
        assert!(msg.contains("3"));
        assert!(msg.contains("not initialized"));
    }
    
    #[test]
    fn test_unknown_identifier() {
        let error = VMError::UnknownIdentifier {
            name: "undefined_var".to_string(),
            location: Some(SourceLocation {
                file: Some("script.fl".to_string()),
                line: 15,
                column: 8,
                function: Some("main".to_string()),
            }),
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Unknown identifier"));
        assert!(msg.contains("undefined_var"));
        assert!(msg.contains("script.fl:15:8"));
    }
    
    #[test]
    fn test_runtime_error() {
        let error = VMError::RuntimeError {
            message: "general runtime failure".to_string(),
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Runtime error"));
        assert!(msg.contains("general runtime failure"));
    }
    
    #[test]
    fn test_stack_trace_formatting() {
        let mut trace = StackTrace::new();
        
        // Add multiple frames
        trace.push_frame(StackFrame {
            function_name: "outer".to_string(),
            chunk_id: 1,
            ip: 10,
            location: Some(SourceLocation {
                file: Some("main.fl".to_string()),
                line: 5,
                column: 3,
                function: Some("main".to_string()),
            }),
        });
        
        trace.push_frame(StackFrame {
            function_name: "middle".to_string(),
            chunk_id: 2,
            ip: 25,
            location: Some(SourceLocation {
                file: Some("lib.fl".to_string()),
                line: 20,
                column: 15,
                function: Some("process".to_string()),
            }),
        });
        
        trace.push_frame(StackFrame {
            function_name: "inner".to_string(),
            chunk_id: 3,
            ip: 42,
            location: None,
        });
        
        let error = VMError::RuntimeError {
            message: "test error".to_string(),
            stack_trace: Some(trace),
        };
        
        let msg = error.to_string();
        assert!(msg.contains("Stack trace:"));
        assert!(msg.contains("0: outer at chunk 1 ip 10 (main.fl:5:3)"));
        assert!(msg.contains("1: middle at chunk 2 ip 25 (lib.fl:20:15)"));
        assert!(msg.contains("2: inner at chunk 3 ip 42"));
    }
    
    #[test]
    fn test_value_type_names() {
        assert_eq!(value_type_name(&Value::Nil), "nil");
        assert_eq!(value_type_name(&Value::Bool(true)), "bool");
        assert_eq!(value_type_name(&Value::Int(42)), "int");
        assert_eq!(value_type_name(&Value::Float(3.14)), "float");
        assert_eq!(value_type_name(&Value::String("test".into())), "string");
        assert_eq!(value_type_name(&Value::List(vec![])), "list");
        assert_eq!(value_type_name(&Value::Map(Default::default())), "map");
        assert_eq!(value_type_name(&Value::Function { 
            chunk_id: 0, 
            env: vec![]
        }), "function");
    }
    
    #[test]
    fn test_from_anyhow_error() {
        let anyhow_err = anyhow::anyhow!("custom error message");
        let vm_err: VMError = anyhow_err.into();
        
        match vm_err {
            VMError::RuntimeError { message, stack_trace } => {
                assert_eq!(message, "custom error message");
                assert!(stack_trace.is_none());
            }
            _ => panic!("Expected RuntimeError variant"),
        }
    }
    
    #[test]
    fn test_error_clone() {
        let original = VMError::TypeError {
            operation: "concat".to_string(),
            expected: "string".to_string(),
            got: "number".to_string(),
            location: Some(SourceLocation {
                file: Some("test.fl".to_string()),
                line: 1,
                column: 1,
                function: None,
            }),
            stack_trace: None,
        };
        
        let cloned = original.clone();
        assert_eq!(original.to_string(), cloned.to_string());
    }
    
    #[test]
    fn test_source_location_no_file() {
        let location = SourceLocation {
            file: None,
            line: 10,
            column: 5,
            function: Some("anonymous".to_string()),
        };
        
        let error = VMError::TypeError {
            operation: "call".to_string(),
            expected: "function".to_string(),
            got: "nil".to_string(),
            location: Some(location),
            stack_trace: None,
        };
        
        let msg = error.to_string();
        assert!(msg.contains("<unknown>:10:5"));
    }
    
    #[test]
    fn test_complex_error_scenario() {
        // Create a complex stack trace
        let mut trace = StackTrace::new();
        
        for i in 0..5 {
            trace.push_frame(StackFrame {
                function_name: format!("function_{}", i),
                chunk_id: i,
                ip: i * 10,
                location: if i % 2 == 0 {
                    Some(SourceLocation {
                        file: Some(format!("file_{}.fl", i)),
                        line: i * 5 + 1,
                        column: i * 2 + 3,
                        function: Some(format!("fn_{}", i)),
                    })
                } else {
                    None
                },
            });
        }
        
        let error = VMError::StackOverflow {
            current_depth: 1000,
            max_depth: 999,
            stack_trace: Some(trace),
        };
        
        let msg = error.to_string();
        
        // Verify all frames are present
        for i in 0..5 {
            assert!(msg.contains(&format!("function_{}", i)));
            assert!(msg.contains(&format!("chunk {}", i)));
            if i % 2 == 0 {
                assert!(msg.contains(&format!("file_{}.fl", i)));
            }
        }
    }
    
    #[test]
    fn test_error_is_std_error() {
        let error = VMError::RuntimeError {
            message: "test".to_string(),
            stack_trace: None,
        };
        
        // Verify it implements std::error::Error
        let _err: &dyn std::error::Error = &error;
    }
}