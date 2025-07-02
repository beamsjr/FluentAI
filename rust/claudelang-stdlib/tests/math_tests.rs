//! Tests for math stdlib functions

use claudelang_stdlib::value::Value;
use claudelang_stdlib::init_stdlib;

#[test]
fn test_basic_arithmetic() {
    let stdlib = init_stdlib();
    
    // Test add
    let add = stdlib.get("+").unwrap();
    assert_eq!(
        add.call(&[Value::Int(5), Value::Int(3)]).unwrap(),
        Value::Int(8)
    );
    assert_eq!(
        add.call(&[Value::Float(2.5), Value::Float(1.5)]).unwrap(),
        Value::Float(4.0)
    );
    assert_eq!(
        add.call(&[Value::Int(5), Value::Float(2.5)]).unwrap(),
        Value::Float(7.5)
    );
    
    // Test subtract
    let sub = stdlib.get("-").unwrap();
    assert_eq!(
        sub.call(&[Value::Int(10), Value::Int(3)]).unwrap(),
        Value::Int(7)
    );
    assert_eq!(
        sub.call(&[Value::Float(5.5), Value::Float(2.5)]).unwrap(),
        Value::Float(3.0)
    );
    
    // Test multiply
    let mul = stdlib.get("*").unwrap();
    assert_eq!(
        mul.call(&[Value::Int(4), Value::Int(5)]).unwrap(),
        Value::Int(20)
    );
    assert_eq!(
        mul.call(&[Value::Float(2.5), Value::Float(4.0)]).unwrap(),
        Value::Float(10.0)
    );
    
    // Test divide
    let div = stdlib.get("/").unwrap();
    assert_eq!(
        div.call(&[Value::Int(20), Value::Int(4)]).unwrap(),
        Value::Int(5)
    );
    assert_eq!(
        div.call(&[Value::Float(10.0), Value::Float(4.0)]).unwrap(),
        Value::Float(2.5)
    );
    assert_eq!(
        div.call(&[Value::Int(7), Value::Int(2)]).unwrap(),
        Value::Int(3) // Integer division
    );
    
    // Test division by zero
    assert!(div.call(&[Value::Int(10), Value::Int(0)]).is_err());
    assert!(div.call(&[Value::Float(10.0), Value::Float(0.0)]).is_err());
}

#[test]
fn test_power_functions() {
    let stdlib = init_stdlib();
    
    // Test pow
    let pow = stdlib.get("pow").unwrap();
    assert_eq!(
        pow.call(&[Value::Int(2), Value::Int(3)]).unwrap(),
        Value::Float(8.0)
    );
    assert_eq!(
        pow.call(&[Value::Float(2.0), Value::Float(3.0)]).unwrap(),
        Value::Float(8.0)
    );
    assert_eq!(
        pow.call(&[Value::Int(5), Value::Int(0)]).unwrap(),
        Value::Float(1.0)
    );
    assert_eq!(
        pow.call(&[Value::Int(10), Value::Int(-1)]).unwrap(),
        Value::Float(0.1)
    );
    
    // Test sqrt
    let sqrt = stdlib.get("sqrt").unwrap();
    assert_eq!(
        sqrt.call(&[Value::Int(16)]).unwrap(),
        Value::Float(4.0)
    );
    assert_eq!(
        sqrt.call(&[Value::Float(25.0)]).unwrap(),
        Value::Float(5.0)
    );
    assert_eq!(
        sqrt.call(&[Value::Float(2.0)]).unwrap(),
        Value::Float(1.4142135623730951)
    );
    
    // Test sqrt of negative number
    assert!(sqrt.call(&[Value::Int(-1)]).is_err());
}

#[test]
fn test_exponential_logarithm() {
    let stdlib = init_stdlib();
    
    // Test exp
    let exp = stdlib.get("exp").unwrap();
    let exp_1 = exp.call(&[Value::Int(1)]).unwrap();
    match exp_1 {
        Value::Float(f) => assert!((f - 2.718281828459045).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
    
    assert_eq!(
        exp.call(&[Value::Int(0)]).unwrap(),
        Value::Float(1.0)
    );
    
    // Test ln
    let ln = stdlib.get("ln").unwrap();
    assert_eq!(
        ln.call(&[Value::Float(2.718281828459045)]).unwrap(),
        Value::Float(1.0)
    );
    assert_eq!(
        ln.call(&[Value::Int(1)]).unwrap(),
        Value::Float(0.0)
    );
    
    // Test ln of non-positive
    assert!(ln.call(&[Value::Int(0)]).is_err());
    assert!(ln.call(&[Value::Int(-1)]).is_err());
    
    // Test log
    let log = stdlib.get("log").unwrap();
    assert_eq!(
        log.call(&[Value::Int(100), Value::Int(10)]).unwrap(),
        Value::Float(2.0)
    );
    assert_eq!(
        log.call(&[Value::Int(8), Value::Int(2)]).unwrap(),
        Value::Float(3.0)
    );
}

#[test]
fn test_trigonometric() {
    let stdlib = init_stdlib();
    let pi = std::f64::consts::PI;
    
    // Test sin
    let sin = stdlib.get("sin").unwrap();
    assert_eq!(
        sin.call(&[Value::Float(0.0)]).unwrap(),
        Value::Float(0.0)
    );
    let sin_half_pi = sin.call(&[Value::Float(pi / 2.0)]).unwrap();
    match sin_half_pi {
        Value::Float(f) => assert!((f - 1.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
    
    // Test cos
    let cos = stdlib.get("cos").unwrap();
    assert_eq!(
        cos.call(&[Value::Float(0.0)]).unwrap(),
        Value::Float(1.0)
    );
    let cos_half_pi = cos.call(&[Value::Float(pi / 2.0)]).unwrap();
    match cos_half_pi {
        Value::Float(f) => assert!(f.abs() < 1e-10),
        _ => panic!("Expected float"),
    }
    
    // Test tan
    let tan = stdlib.get("tan").unwrap();
    assert_eq!(
        tan.call(&[Value::Float(0.0)]).unwrap(),
        Value::Float(0.0)
    );
    let tan_quarter_pi = tan.call(&[Value::Float(pi / 4.0)]).unwrap();
    match tan_quarter_pi {
        Value::Float(f) => assert!((f - 1.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
    
    // Test asin
    let asin = stdlib.get("asin").unwrap();
    assert_eq!(
        asin.call(&[Value::Float(0.0)]).unwrap(),
        Value::Float(0.0)
    );
    let asin_1 = asin.call(&[Value::Float(1.0)]).unwrap();
    match asin_1 {
        Value::Float(f) => assert!((f - pi / 2.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
    
    // Test asin out of domain
    assert!(asin.call(&[Value::Float(2.0)]).is_err());
    assert!(asin.call(&[Value::Float(-2.0)]).is_err());
    
    // Test acos
    let acos = stdlib.get("acos").unwrap();
    let acos_0 = acos.call(&[Value::Float(0.0)]).unwrap();
    match acos_0 {
        Value::Float(f) => assert!((f - pi / 2.0).abs() < 1e-10),
        _ => panic!("Expected float"),
    }
    
    // Test atan
    let atan = stdlib.get("atan").unwrap();
    assert_eq!(
        atan.call(&[Value::Float(0.0)]).unwrap(),
        Value::Float(0.0)
    );
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
    assert_eq!(
        sinh.call(&[Value::Float(0.0)]).unwrap(),
        Value::Float(0.0)
    );
    
    // Test cosh
    let cosh = stdlib.get("cosh").unwrap();
    assert_eq!(
        cosh.call(&[Value::Float(0.0)]).unwrap(),
        Value::Float(1.0)
    );
    
    // Test tanh
    let tanh = stdlib.get("tanh").unwrap();
    assert_eq!(
        tanh.call(&[Value::Float(0.0)]).unwrap(),
        Value::Float(0.0)
    );
    
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
    assert_eq!(
        floor.call(&[Value::Float(3.7)]).unwrap(),
        Value::Float(3.0)
    );
    assert_eq!(
        floor.call(&[Value::Float(-2.3)]).unwrap(),
        Value::Float(-3.0)
    );
    assert_eq!(
        floor.call(&[Value::Int(5)]).unwrap(),
        Value::Float(5.0)
    );
    
    // Test ceil
    let ceil = stdlib.get("ceil").unwrap();
    assert_eq!(
        ceil.call(&[Value::Float(3.2)]).unwrap(),
        Value::Float(4.0)
    );
    assert_eq!(
        ceil.call(&[Value::Float(-2.7)]).unwrap(),
        Value::Float(-2.0)
    );
    assert_eq!(
        ceil.call(&[Value::Int(5)]).unwrap(),
        Value::Float(5.0)
    );
    
    // Test round
    let round = stdlib.get("round").unwrap();
    assert_eq!(
        round.call(&[Value::Float(3.5)]).unwrap(),
        Value::Float(4.0)
    );
    assert_eq!(
        round.call(&[Value::Float(3.4)]).unwrap(),
        Value::Float(3.0)
    );
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
    assert_eq!(
        sign.call(&[Value::Int(42)]).unwrap(),
        Value::Int(1)
    );
    assert_eq!(
        sign.call(&[Value::Int(-42)]).unwrap(),
        Value::Int(-1)
    );
    assert_eq!(
        sign.call(&[Value::Int(0)]).unwrap(),
        Value::Int(0)
    );
    assert_eq!(
        sign.call(&[Value::Float(3.14)]).unwrap(),
        Value::Int(1)
    );
    assert_eq!(
        sign.call(&[Value::Float(-3.14)]).unwrap(),
        Value::Int(-1)
    );
    
    // Test even?
    let even = stdlib.get("even?").unwrap();
    assert_eq!(
        even.call(&[Value::Int(4)]).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        even.call(&[Value::Int(5)]).unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        even.call(&[Value::Int(0)]).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        even.call(&[Value::Int(-4)]).unwrap(),
        Value::Bool(true)
    );
    
    // Test odd?
    let odd = stdlib.get("odd?").unwrap();
    assert_eq!(
        odd.call(&[Value::Int(4)]).unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        odd.call(&[Value::Int(5)]).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        odd.call(&[Value::Int(0)]).unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        odd.call(&[Value::Int(-3)]).unwrap(),
        Value::Bool(true)
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
        zero.call(&[Value::Int(0)]).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        zero.call(&[Value::Int(1)]).unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        zero.call(&[Value::Int(-1)]).unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        zero.call(&[Value::Float(0.0)]).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        zero.call(&[Value::Float(0.1)]).unwrap(),
        Value::Bool(false)
    );
}