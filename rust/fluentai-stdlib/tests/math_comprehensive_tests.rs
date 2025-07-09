//! Comprehensive tests for math functions to improve coverage

use fluentai_stdlib::init_stdlib;
use fluentai_stdlib::value::Value;

#[test]
fn test_variadic_arithmetic() {
    let stdlib = init_stdlib();

    // Test add with multiple arguments
    let add = stdlib.get("+").unwrap();
    assert_eq!(
        add.call(&[
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4)
        ])
        .unwrap(),
        Value::Integer(10)
    );
    // Add with no args - might not be supported
    // assert_eq!(
    //     add.call(&[]).unwrap(),
    //     Value::Integer(0)  // Empty sum is 0
    // );
    assert_eq!(
        add.call(&[Value::Integer(42)]).unwrap(),
        Value::Integer(42) // Single arg
    );

    // Test multiply with multiple arguments
    let mul = stdlib.get("*").unwrap();
    assert_eq!(
        mul.call(&[Value::Integer(2), Value::Integer(3), Value::Integer(4)])
            .unwrap(),
        Value::Integer(24)
    );
    // Multiply with no args - might not be supported
    // assert_eq!(
    //     mul.call(&[]).unwrap(),
    //     Value::Integer(1)  // Empty product is 1
    // );
    assert_eq!(
        mul.call(&[Value::Integer(42)]).unwrap(),
        Value::Integer(42) // Single arg
    );

    // Test max with multiple arguments
    let max = stdlib.get("max").unwrap();
    assert_eq!(
        max.call(&[
            Value::Integer(3),
            Value::Integer(7),
            Value::Integer(2),
            Value::Integer(9),
            Value::Integer(5)
        ])
        .unwrap(),
        Value::Integer(9)
    );
    assert_eq!(
        max.call(&[Value::Float(3.5), Value::Float(7.2), Value::Float(2.1)])
            .unwrap(),
        Value::Float(7.2)
    );

    // Test min with multiple arguments
    let min = stdlib.get("min").unwrap();
    assert_eq!(
        min.call(&[
            Value::Integer(3),
            Value::Integer(7),
            Value::Integer(2),
            Value::Integer(9),
            Value::Integer(5)
        ])
        .unwrap(),
        Value::Integer(2)
    );
    assert_eq!(
        min.call(&[Value::Float(3.5), Value::Float(7.2), Value::Float(2.1)])
            .unwrap(),
        Value::Float(2.1)
    );
}

#[test]
fn test_advanced_math_functions() {
    let stdlib = init_stdlib();

    // Test gcd
    let gcd = stdlib.get("gcd").unwrap();
    assert_eq!(
        gcd.call(&[Value::Integer(48), Value::Integer(18)]).unwrap(),
        Value::Integer(6)
    );
    assert_eq!(
        gcd.call(&[Value::Integer(17), Value::Integer(19)]).unwrap(),
        Value::Integer(1) // Coprime
    );
    assert_eq!(
        gcd.call(&[Value::Integer(0), Value::Integer(5)]).unwrap(),
        Value::Integer(5)
    );

    // Test lcm
    let lcm = stdlib.get("lcm").unwrap();
    assert_eq!(
        lcm.call(&[Value::Integer(12), Value::Integer(18)]).unwrap(),
        Value::Integer(36)
    );
    assert_eq!(
        lcm.call(&[Value::Integer(7), Value::Integer(5)]).unwrap(),
        Value::Integer(35)
    );

    // Test factorial
    let factorial = stdlib.get("factorial").unwrap();
    assert_eq!(
        factorial.call(&[Value::Integer(5)]).unwrap(),
        Value::Integer(120)
    );
    assert_eq!(
        factorial.call(&[Value::Integer(0)]).unwrap(),
        Value::Integer(1)
    );
    assert_eq!(
        factorial.call(&[Value::Integer(1)]).unwrap(),
        Value::Integer(1)
    );
}

#[test]
fn test_logarithm_functions() {
    let stdlib = init_stdlib();

    // Test log with different bases
    let log = stdlib.get("log").unwrap();

    // log base e (natural log) - single argument
    let result = log.call(&[Value::Float(2.718281828)]).unwrap();
    match result {
        Value::Float(f) => assert!((f - 1.0).abs() < 0.0001),
        _ => panic!("Expected float"),
    }

    // log with explicit base
    let result = log
        .call(&[Value::Float(100.0), Value::Float(10.0)])
        .unwrap();
    match result {
        Value::Float(f) => assert!((f - 2.0).abs() < 0.0001),
        _ => panic!("Expected float"),
    }

    // Test log2
    let log2 = stdlib.get("log2").unwrap();
    assert_eq!(log2.call(&[Value::Float(8.0)]).unwrap(), Value::Float(3.0));
    assert_eq!(log2.call(&[Value::Float(1.0)]).unwrap(), Value::Float(0.0));

    // Test log10
    let log10 = stdlib.get("log10").unwrap();
    assert_eq!(
        log10.call(&[Value::Float(1000.0)]).unwrap(),
        Value::Float(3.0)
    );
    assert_eq!(log10.call(&[Value::Float(1.0)]).unwrap(), Value::Float(0.0));
}

#[test]
fn test_degree_radian_conversion() {
    let stdlib = init_stdlib();

    // Test radians (converts degrees to radians)
    let deg_to_rad = stdlib.get("radians").unwrap();
    let result = deg_to_rad.call(&[Value::Float(180.0)]).unwrap();
    match result {
        Value::Float(f) => assert!((f - std::f64::consts::PI).abs() < 0.0001),
        _ => panic!("Expected float"),
    }

    let result = deg_to_rad.call(&[Value::Float(90.0)]).unwrap();
    match result {
        Value::Float(f) => assert!((f - std::f64::consts::FRAC_PI_2).abs() < 0.0001),
        _ => panic!("Expected float"),
    }

    // Test degrees (converts radians to degrees)
    let rad_to_deg = stdlib.get("degrees").unwrap();
    let result = rad_to_deg
        .call(&[Value::Float(std::f64::consts::PI)])
        .unwrap();
    match result {
        Value::Float(f) => assert!((f - 180.0).abs() < 0.0001),
        _ => panic!("Expected float"),
    }
}

#[test]
fn test_hyperbolic_edge_cases() {
    let stdlib = init_stdlib();

    // Test sinh(0) = 0
    let sinh = stdlib.get("sinh").unwrap();
    assert_eq!(sinh.call(&[Value::Float(0.0)]).unwrap(), Value::Float(0.0));

    // Test cosh(0) = 1
    let cosh = stdlib.get("cosh").unwrap();
    assert_eq!(cosh.call(&[Value::Float(0.0)]).unwrap(), Value::Float(1.0));

    // Test tanh(0) = 0
    let tanh = stdlib.get("tanh").unwrap();
    assert_eq!(tanh.call(&[Value::Float(0.0)]).unwrap(), Value::Float(0.0));
}

// Test for infinite? and nan? predicates - these don't exist in stdlib yet
// #[test]
// fn test_inf_and_nan_predicates() {
//     let stdlib = init_stdlib();
//
//     // Test infinite?
//     let infinite = stdlib.get("infinite?").unwrap();
//     assert_eq!(
//         infinite.call(&[Value::Float(f64::INFINITY)]).unwrap(),
//         Value::Boolean(true)
//     );
//     assert_eq!(
//         infinite.call(&[Value::Float(f64::NEG_INFINITY)]).unwrap(),
//         Value::Boolean(true)
//     );
//     assert_eq!(
//         infinite.call(&[Value::Float(42.0)]).unwrap(),
//         Value::Boolean(false)
//     );
//     assert_eq!(
//         infinite.call(&[Value::Float(f64::NAN)]).unwrap(),
//         Value::Boolean(false)
//     );
//
//     // Test nan?
//     let nan = stdlib.get("nan?").unwrap();
//     assert_eq!(
//         nan.call(&[Value::Float(f64::NAN)]).unwrap(),
//         Value::Boolean(true)
//     );
//     assert_eq!(
//         nan.call(&[Value::Float(42.0)]).unwrap(),
//         Value::Boolean(false)
//     );
//     assert_eq!(
//         nan.call(&[Value::Float(f64::INFINITY)]).unwrap(),
//         Value::Boolean(false)
//     );
//
//     // Test on non-float values
//     assert_eq!(
//         infinite.call(&[Value::Integer(42)]).unwrap(),
//         Value::Boolean(false)
//     );
//     assert_eq!(
//         nan.call(&[Value::Integer(42)]).unwrap(),
//         Value::Boolean(false)
//     );
// }

#[test]
fn test_error_cases() {
    let stdlib = init_stdlib();

    // Test division by zero
    let div = stdlib.get("/").unwrap();
    assert!(div.call(&[Value::Integer(5), Value::Integer(0)]).is_err());

    // Test log of negative number - should return an error or NaN
    let log = stdlib.get("log").unwrap();
    let result = log.call(&[Value::Float(-1.0)]);
    // The implementation returns an error for negative inputs
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("input must be positive"));

    // Test sqrt of negative number
    let sqrt = stdlib.get("sqrt").unwrap();
    let result = sqrt.call(&[Value::Float(-1.0)]).unwrap();
    match result {
        Value::Float(f) => assert!(f.is_nan()),
        _ => panic!("Expected float"),
    }

    // Test asin/acos out of range
    let asin = stdlib.get("asin").unwrap();
    let result = asin.call(&[Value::Float(2.0)]).unwrap();
    match result {
        Value::Float(f) => assert!(f.is_nan()),
        _ => panic!("Expected float"),
    }

    // Test modulo/quotient/remainder functions - may not exist
    // let modulo = stdlib.get("modulo").unwrap();
    // assert!(modulo.call(&[Value::Integer(5), Value::Integer(0)]).is_err());
    //
    // let quotient = stdlib.get("quotient").unwrap();
    // assert!(quotient.call(&[Value::Integer(5), Value::Integer(0)]).is_err());
    //
    // let remainder = stdlib.get("remainder").unwrap();
    // assert!(remainder.call(&[Value::Integer(5), Value::Integer(0)]).is_err());
}

#[test]
fn test_mixed_numeric_operations() {
    let stdlib = init_stdlib();

    // Test operations with mixed int/float
    let pow = stdlib.get("pow").unwrap();

    // int^int -> int when possible
    assert_eq!(
        pow.call(&[Value::Integer(2), Value::Integer(3)]).unwrap(),
        Value::Integer(8)
    );

    // int^float -> float
    let result = pow.call(&[Value::Integer(2), Value::Float(3.5)]).unwrap();
    match result {
        Value::Float(f) => assert!((f - 11.313708498984761).abs() < 0.0001),
        _ => panic!("Expected float"),
    }

    // float^int -> float
    assert_eq!(
        pow.call(&[Value::Float(2.5), Value::Integer(2)]).unwrap(),
        Value::Float(6.25)
    );

    // Test negative powers
    assert_eq!(
        pow.call(&[Value::Float(2.0), Value::Integer(-2)]).unwrap(),
        Value::Float(0.25)
    );
}
