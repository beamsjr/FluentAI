#[cfg(test)]
mod tests {
    use crate::error::*;
    use std::io;

    // ===== Error Creation Tests =====

    #[test]
    fn test_parse_error() {
        let err = Error::Parse("unexpected token".to_string());
        assert_eq!(err.to_string(), "Parse error: unexpected token");
    }

    #[test]
    fn test_type_error() {
        let err = Error::Type("expected Integer, got String".to_string());
        assert_eq!(err.to_string(), "Type error: expected Integer, got String");
    }

    #[test]
    fn test_runtime_error() {
        let err = Error::Runtime("division by zero".to_string());
        assert_eq!(err.to_string(), "Runtime error: division by zero");
    }

    #[test]
    fn test_undefined_variable_error() {
        let err = Error::UndefinedVariable("foo".to_string());
        assert_eq!(err.to_string(), "Undefined variable: foo");
    }

    #[test]
    fn test_invalid_arity_error() {
        let err = Error::InvalidArity {
            expected: 2,
            got: 3,
        };
        assert_eq!(err.to_string(), "Invalid arity: expected 2, got 3");
    }

    #[test]
    fn test_unknown_effect_error() {
        let err = Error::UnknownEffect("CustomEffect".to_string());
        assert_eq!(err.to_string(), "Unknown effect: CustomEffect");
    }

    #[test]
    fn test_contract_violation_error() {
        let err = Error::ContractViolation("precondition failed: x > 0".to_string());
        assert_eq!(
            err.to_string(),
            "Contract violation: precondition failed: x > 0"
        );
    }

    #[test]
    fn test_io_error() {
        let io_err = io::Error::new(io::ErrorKind::NotFound, "file not found");
        let err = Error::Io(io_err);
        assert!(err.to_string().contains("IO error:"));
    }

    #[test]
    fn test_other_error() {
        let anyhow_err = anyhow::anyhow!("custom error");
        let err = Error::Other(anyhow_err);
        assert_eq!(err.to_string(), "custom error");
    }

    // ===== Error Conversion Tests =====

    #[test]
    fn test_from_io_error() {
        let io_err = io::Error::new(io::ErrorKind::PermissionDenied, "access denied");
        let err: Error = io_err.into();
        assert!(matches!(err, Error::Io(_)));
        assert!(err.to_string().contains("IO error:"));
    }

    #[test]
    fn test_from_anyhow_error() {
        let anyhow_err = anyhow::anyhow!("something went wrong");
        let err: Error = anyhow_err.into();
        assert!(matches!(err, Error::Other(_)));
        assert_eq!(err.to_string(), "something went wrong");
    }

    // ===== Result Type Tests =====

    #[test]
    fn test_result_ok() {
        let result: Result<i32> = Ok(42);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[test]
    fn test_result_err() {
        let result: Result<i32> = Err(Error::Runtime("failed".to_string()));
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), Error::Runtime(_)));
    }

    // ===== Error Pattern Matching Tests =====

    #[test]
    fn test_error_matching() {
        let errors = vec![
            Error::Parse("test".to_string()),
            Error::Type("test".to_string()),
            Error::Runtime("test".to_string()),
            Error::UndefinedVariable("test".to_string()),
            Error::InvalidArity {
                expected: 1,
                got: 2,
            },
            Error::UnknownEffect("test".to_string()),
            Error::ContractViolation("test".to_string()),
        ];

        let mut counts = (0, 0, 0, 0, 0, 0, 0);

        for err in errors {
            match err {
                Error::Parse(_) => counts.0 += 1,
                Error::Type(_) => counts.1 += 1,
                Error::Runtime(_) => counts.2 += 1,
                Error::UndefinedVariable(_) => counts.3 += 1,
                Error::InvalidArity { .. } => counts.4 += 1,
                Error::UnknownEffect(_) => counts.5 += 1,
                Error::ContractViolation(_) => counts.6 += 1,
                _ => {}
            }
        }

        assert_eq!(counts, (1, 1, 1, 1, 1, 1, 1));
    }

    // ===== Error Display Formatting Tests =====

    #[test]
    fn test_error_display_formatting() {
        // Test with empty strings
        assert_eq!(Error::Parse("".to_string()).to_string(), "Parse error: ");
        assert_eq!(Error::Type("".to_string()).to_string(), "Type error: ");
        assert_eq!(
            Error::Runtime("".to_string()).to_string(),
            "Runtime error: "
        );

        // Test with special characters
        let special = "Error with 'quotes' and \"double quotes\"";
        assert_eq!(
            Error::Parse(special.to_string()).to_string(),
            format!("Parse error: {}", special)
        );
    }

    #[test]
    fn test_invalid_arity_formatting() {
        // Test various arity combinations
        assert_eq!(
            Error::InvalidArity {
                expected: 0,
                got: 1
            }
            .to_string(),
            "Invalid arity: expected 0, got 1"
        );

        assert_eq!(
            Error::InvalidArity {
                expected: 10,
                got: 5
            }
            .to_string(),
            "Invalid arity: expected 10, got 5"
        );

        assert_eq!(
            Error::InvalidArity {
                expected: 1,
                got: 1
            }
            .to_string(),
            "Invalid arity: expected 1, got 1"
        );
    }

    // ===== Error Chain Tests =====

    #[test]
    fn test_error_chain_with_io() {
        fn may_fail_io() -> Result<String> {
            let io_err = io::Error::new(io::ErrorKind::Other, "disk full");
            Err(io_err.into())
        }

        let result = may_fail_io();
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, Error::Io(_)));
    }

    #[test]
    fn test_error_chain_with_anyhow() {
        fn may_fail_anyhow() -> Result<String> {
            let err = anyhow::anyhow!("operation failed");
            Err(err.into())
        }

        let result = may_fail_anyhow();
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, Error::Other(_)));
        // The transparent attribute means we see the anyhow error directly
        let err_str = err.to_string();
        assert_eq!(err_str, "operation failed");
    }

    // ===== Error Propagation Tests =====

    #[test]
    fn test_error_propagation() {
        fn inner_function(fail: bool) -> Result<i32> {
            if fail {
                Err(Error::Runtime("inner failed".to_string()))
            } else {
                Ok(42)
            }
        }

        fn outer_function(fail: bool) -> Result<i32> {
            let value = inner_function(fail)?;
            Ok(value * 2)
        }

        // Test success case
        assert_eq!(outer_function(false).unwrap(), 84);

        // Test failure case
        let err = outer_function(true).unwrap_err();
        match err {
            Error::Runtime(msg) => assert_eq!(msg, "inner failed"),
            _ => panic!("Wrong error type"),
        }
    }

    // ===== Debug Implementation Tests =====

    #[test]
    fn test_error_debug() {
        let err = Error::Parse("test".to_string());
        let debug_str = format!("{:?}", err);
        assert!(debug_str.contains("Parse"));
        assert!(debug_str.contains("test"));

        let err = Error::InvalidArity {
            expected: 2,
            got: 3,
        };
        let debug_str = format!("{:?}", err);
        assert!(debug_str.contains("InvalidArity"));
        assert!(debug_str.contains("expected"));
        assert!(debug_str.contains("got"));
    }

    // ===== Real-world Usage Tests =====

    #[test]
    fn test_parser_error_scenario() {
        fn parse_number(s: &str) -> Result<i32> {
            s.parse::<i32>()
                .map_err(|_| Error::Parse(format!("invalid number: '{}'", s)))
        }

        assert_eq!(parse_number("42").unwrap(), 42);

        let err = parse_number("abc").unwrap_err();
        match err {
            Error::Parse(msg) => assert!(msg.contains("invalid number: 'abc'")),
            _ => panic!("Wrong error type"),
        }
    }

    #[test]
    fn test_type_checking_scenario() {
        fn check_type(expected: &str, actual: &str) -> Result<()> {
            if expected != actual {
                Err(Error::Type(format!(
                    "expected {}, got {}",
                    expected, actual
                )))
            } else {
                Ok(())
            }
        }

        assert!(check_type("int", "int").is_ok());

        let err = check_type("int", "string").unwrap_err();
        match err {
            Error::Type(msg) => assert_eq!(msg, "expected int, got string"),
            _ => panic!("Wrong error type"),
        }
    }

    #[test]
    fn test_runtime_error_scenario() {
        fn divide(a: i32, b: i32) -> Result<i32> {
            if b == 0 {
                Err(Error::Runtime("division by zero".to_string()))
            } else {
                Ok(a / b)
            }
        }

        assert_eq!(divide(10, 2).unwrap(), 5);

        let err = divide(10, 0).unwrap_err();
        match err {
            Error::Runtime(msg) => assert_eq!(msg, "division by zero"),
            _ => panic!("Wrong error type"),
        }
    }
}
