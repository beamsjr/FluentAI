//! Tests for math stdlib functions

use fluentai_stdlib::init_stdlib;
use fluentai_stdlib::value::Value;

#[test]
fn test_basic_arithmetic() {
    let stdlib = init_stdlib();

    // Test add
    let add = stdlib.get("+").unwrap();
    assert_eq!(
        add.call(&[Value::Integer(5), Value::Integer(3)]).unwrap(),
        Value::Integer(8)
    );
    assert_eq!(
        add.call(&[Value::Float(2.5), Value::Float(1.5)]).unwrap(),
        Value::Float(4.0)
    );
    assert_eq!(
        add.call(&[Value::Integer(5), Value::Float(2.5)]).unwrap(),
        Value::Float(7.5)
    );

    // Test subtract
    let sub = stdlib.get("-").unwrap();
    assert_eq!(
        sub.call(&[Value::Integer(10), Value::Integer(3)]).unwrap(),
        Value::Integer(7)
    );
    assert_eq!(
        sub.call(&[Value::Float(5.5), Value::Float(2.5)]).unwrap(),
        Value::Float(3.0)
    );

    // Test multiply
    let mul = stdlib.get("*").unwrap();
    assert_eq!(
        mul.call(&[Value::Integer(4), Value::Integer(5)]).unwrap(),
        Value::Integer(20)
    );
    assert_eq!(
        mul.call(&[Value::Float(2.5), Value::Float(4.0)]).unwrap(),
        Value::Float(10.0)
    );

    // Test divide
    let div = stdlib.get("/").unwrap();
    assert_eq!(
        div.call(&[Value::Integer(20), Value::Integer(4)]).unwrap(),
        Value::Integer(5)
    );
    assert_eq!(
        div.call(&[Value::Float(10.0), Value::Float(4.0)]).unwrap(),
        Value::Float(2.5)
    );
    assert_eq!(
        div.call(&[Value::Integer(7), Value::Integer(2)]).unwrap(),
        Value::Integer(3) // Integer division
    );

    // Test division by zero
    assert!(div.call(&[Value::Integer(10), Value::Integer(0)]).is_err());
    assert!(div.call(&[Value::Float(10.0), Value::Float(0.0)]).is_err());
}

#[test]
fn test_power_functions() {
    let stdlib = init_stdlib();

    // Test pow
    let pow = stdlib.get("pow").unwrap();
    assert_eq!(
        pow.call(&[Value::Integer(2), Value::Integer(3)]).unwrap(),
        Value::Integer(8)
    );
    assert_eq!(
        pow.call(&[Value::Float(2.0), Value::Float(3.0)]).unwrap(),
        Value::Float(8.0)
    );
    assert_eq!(
        pow.call(&[Value::Integer(5), Value::Integer(0)]).unwrap(),
        Value::Integer(1)
    );
    assert_eq!(
        pow.call(&[Value::Integer(10), Value::Integer(-1)]).unwrap(),
        Value::Float(0.1)
    );

    // Test ** operator (alias for pow)
    let exp_op = stdlib.get("**").unwrap();
    assert_eq!(
        exp_op.call(&[Value::Integer(2), Value::Integer(8)]).unwrap(),
        Value::Integer(256)
    );
    assert_eq!(
        exp_op.call(&[Value::Float(2.0), Value::Float(8.0)]).unwrap(),
        Value::Float(256.0)
    );
    assert_eq!(
        exp_op.call(&[Value::Integer(3), Value::Integer(3)]).unwrap(),
        Value::Integer(27)
    );

    // Test sqrt
    let sqrt = stdlib.get("sqrt").unwrap();
    assert_eq!(sqrt.call(&[Value::Integer(16)]).unwrap(), Value::Float(4.0));
    assert_eq!(sqrt.call(&[Value::Float(25.0)]).unwrap(), Value::Float(5.0));
    assert_eq!(
        sqrt.call(&[Value::Float(2.0)]).unwrap(),
        Value::Float(1.4142135623730951)
    );

    // Test sqrt of negative number returns NaN
    let sqrt_neg = sqrt.call(&[Value::Integer(-1)]).unwrap();
    match sqrt_neg {
        Value::Float(f) => assert!(f.is_nan()),
        _ => panic!("Expected float"),
    }
}

#[test]
fn test_exponential_logarithm() {
    let stdlib = init_stdlib();

    // Test exp
    let exp = stdlib.get("exp").unwrap();
    let exp_1 = exp.call(&[Value::Integer(1)]).unwrap();
    match exp_1 {
        Value::Float(f) => assert!((f - 2.718281828459045).abs() < 1e-10),
        _ => panic!("Expected float"),
    }

    assert_eq!(exp.call(&[Value::Integer(0)]).unwrap(), Value::Float(1.0));

    // Test ln
    let ln = stdlib.get("ln").unwrap();
    assert_eq!(
        ln.call(&[Value::Float(2.718281828459045)]).unwrap(),
        Value::Float(1.0)
    );
    assert_eq!(ln.call(&[Value::Integer(1)]).unwrap(), Value::Float(0.0));

    // Test ln of non-positive
    assert!(ln.call(&[Value::Integer(0)]).is_err());
    assert!(ln.call(&[Value::Integer(-1)]).is_err());

    // Test log
    let log = stdlib.get("log").unwrap();
    assert_eq!(
        log.call(&[Value::Integer(100), Value::Integer(10)])
            .unwrap(),
        Value::Float(2.0)
    );
    assert_eq!(
        log.call(&[Value::Integer(8), Value::Integer(2)]).unwrap(),
        Value::Float(3.0)
    );
}

#[test]
fn test_trigonometric() {
    let stdlib = init_stdlib();
    let pi = std::f64::consts::PI;

    // Test sin
    let sin = stdlib.get("sin").unwrap();
    assert_eq!(sin.call(&[Value::Float(0.0)]).unwrap(), Value::Float(0.0));
    let sin_half_pi = sin.call(&[Value::Float(pi / 2.0)]).unwrap();
    match sin_half_pi {
        Value::Float(f) => assert!((f - 1.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }

    // Test cos
    let cos = stdlib.get("cos").unwrap();
    assert_eq!(cos.call(&[Value::Float(0.0)]).unwrap(), Value::Float(1.0));
    let cos_half_pi = cos.call(&[Value::Float(pi / 2.0)]).unwrap();
    match cos_half_pi {
        Value::Float(f) => assert!(f.abs() < 1e-10),
        _ => panic!("Expected float"),
    }

    // Test tan
    let tan = stdlib.get("tan").unwrap();
    assert_eq!(tan.call(&[Value::Float(0.0)]).unwrap(), Value::Float(0.0));
    let tan_quarter_pi = tan.call(&[Value::Float(pi / 4.0)]).unwrap();
    match tan_quarter_pi {
        Value::Float(f) => assert!((f - 1.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }

    // Test asin
    let asin = stdlib.get("asin").unwrap();
    assert_eq!(asin.call(&[Value::Float(0.0)]).unwrap(), Value::Float(0.0));
    let asin_1 = asin.call(&[Value::Float(1.0)]).unwrap();
    match asin_1 {
        Value::Float(f) => assert!((f - pi / 2.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }

    // Test asin out of domain - returns NaN
    let asin_2 = asin.call(&[Value::Float(2.0)]).unwrap();
    match asin_2 {
        Value::Float(f) => assert!(f.is_nan()),
        _ => panic!("Expected float"),
    }
    let asin_neg2 = asin.call(&[Value::Float(-2.0)]).unwrap();
    match asin_neg2 {
        Value::Float(f) => assert!(f.is_nan()),
        _ => panic!("Expected float"),
    }

    // Test acos
    let acos = stdlib.get("acos").unwrap();
    let acos_0 = acos.call(&[Value::Float(0.0)]).unwrap();
    match acos_0 {
        Value::Float(f) => assert!((f - pi / 2.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }

    // Test atan
    let atan = stdlib.get("atan").unwrap();
    assert_eq!(atan.call(&[Value::Float(0.0)]).unwrap(), Value::Float(0.0));
    let atan_1 = atan.call(&[Value::Float(1.0)]).unwrap();
    match atan_1 {
        Value::Float(f) => assert!((f - pi / 4.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
}

#[test]
fn test_hyperbolic() {
    let stdlib = init_stdlib();

    // Test sinh
    let sinh = stdlib.get("sinh").unwrap();
    assert_eq!(sinh.call(&[Value::Float(0.0)]).unwrap(), Value::Float(0.0));

    // Test cosh
    let cosh = stdlib.get("cosh").unwrap();
    assert_eq!(cosh.call(&[Value::Float(0.0)]).unwrap(), Value::Float(1.0));

    // Test tanh
    let tanh = stdlib.get("tanh").unwrap();
    assert_eq!(tanh.call(&[Value::Float(0.0)]).unwrap(), Value::Float(0.0));

    // Test large values approach ±1
    let tanh_large = tanh.call(&[Value::Float(100.0)]).unwrap();
    match tanh_large {
        Value::Float(f) => assert!((f - 1.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }

    let tanh_neg_large = tanh.call(&[Value::Float(-100.0)]).unwrap();
    match tanh_neg_large {
        Value::Float(f) => assert!((f + 1.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
}

#[test]
fn test_rounding_functions() {
    let stdlib = init_stdlib();

    // Test floor
    let floor = stdlib.get("floor").unwrap();
    assert_eq!(floor.call(&[Value::Float(3.7)]).unwrap(), Value::Float(3.0));
    assert_eq!(
        floor.call(&[Value::Float(-2.3)]).unwrap(),
        Value::Float(-3.0)
    );
    assert_eq!(floor.call(&[Value::Integer(5)]).unwrap(), Value::Float(5.0));

    // Test ceil
    let ceil = stdlib.get("ceil").unwrap();
    assert_eq!(ceil.call(&[Value::Float(3.2)]).unwrap(), Value::Float(4.0));
    assert_eq!(
        ceil.call(&[Value::Float(-2.7)]).unwrap(),
        Value::Float(-2.0)
    );
    assert_eq!(ceil.call(&[Value::Integer(5)]).unwrap(), Value::Float(5.0));

    // Test round
    let round = stdlib.get("round").unwrap();
    assert_eq!(round.call(&[Value::Float(3.5)]).unwrap(), Value::Float(4.0));
    assert_eq!(round.call(&[Value::Float(3.4)]).unwrap(), Value::Float(3.0));
    assert_eq!(
        round.call(&[Value::Float(-2.5)]).unwrap(),
        Value::Float(-3.0)
    );
    assert_eq!(
        round.call(&[Value::Float(-2.4)]).unwrap(),
        Value::Float(-2.0)
    );
}

#[test]
fn test_sign_and_even_odd() {
    let stdlib = init_stdlib();

    // Test sign
    let sign = stdlib.get("sign").unwrap();
    assert_eq!(sign.call(&[Value::Integer(42)]).unwrap(), Value::Integer(1));
    assert_eq!(
        sign.call(&[Value::Integer(-42)]).unwrap(),
        Value::Integer(-1)
    );
    assert_eq!(sign.call(&[Value::Integer(0)]).unwrap(), Value::Integer(0));
    assert_eq!(sign.call(&[Value::Float(3.14)]).unwrap(), Value::Integer(1));
    assert_eq!(
        sign.call(&[Value::Float(-3.14)]).unwrap(),
        Value::Integer(-1)
    );

    // Test even?
    let even = stdlib.get("even?").unwrap();
    assert_eq!(
        even.call(&[Value::Integer(4)]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        even.call(&[Value::Integer(5)]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        even.call(&[Value::Integer(0)]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        even.call(&[Value::Integer(-4)]).unwrap(),
        Value::Boolean(true)
    );

    // Test odd?
    let odd = stdlib.get("odd?").unwrap();
    assert_eq!(
        odd.call(&[Value::Integer(4)]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        odd.call(&[Value::Integer(5)]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        odd.call(&[Value::Integer(0)]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        odd.call(&[Value::Integer(-3)]).unwrap(),
        Value::Boolean(true)
    );
}

#[test]
fn test_constants() {
    let stdlib = init_stdlib();

    // Test pi
    let pi = stdlib.get("pi").unwrap();
    let pi_val = pi.call(&[]).unwrap();
    match pi_val {
        Value::Float(f) => assert!((f - std::f64::consts::PI).abs() < 1e-10),
        _ => panic!("Expected float"),
    }

    // Test e
    let e = stdlib.get("e").unwrap();
    let e_val = e.call(&[]).unwrap();
    match e_val {
        Value::Float(f) => assert!((f - std::f64::consts::E).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
}

#[test]
fn test_zero_predicate() {
    let stdlib = init_stdlib();
    let zero = stdlib.get("zero?").unwrap();

    assert_eq!(
        zero.call(&[Value::Integer(0)]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        zero.call(&[Value::Integer(1)]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        zero.call(&[Value::Integer(-1)]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        zero.call(&[Value::Float(0.0)]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        zero.call(&[Value::Float(0.1)]).unwrap(),
        Value::Boolean(false)
    );
}
