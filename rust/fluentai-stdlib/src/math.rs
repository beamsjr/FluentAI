//! Mathematical functions

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use anyhow::{anyhow, Result};
use std::f64::consts;

/// Register all math functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // Basic arithmetic
        StdlibFunction::pure("+", add, 0, None, "Addition"),
        StdlibFunction::pure("-", subtract, 1, Some(2), "Subtraction"),
        StdlibFunction::pure("*", multiply, 0, None, "Multiplication"),
        StdlibFunction::pure("/", divide, 2, Some(2), "Division"),
        // Number predicates
        StdlibFunction::pure("zero?", is_zero, 1, Some(1), "Check if number is zero"),
        StdlibFunction::pure("even?", is_even, 1, Some(1), "Check if integer is even"),
        StdlibFunction::pure("odd?", is_odd, 1, Some(1), "Check if integer is odd"),
        StdlibFunction::pure("sign", sign, 1, Some(1), "Get sign of number (-1, 0, 1)"),
        // Logarithm aliases
        StdlibFunction::pure("ln", ln, 1, Some(1), "Natural logarithm"),
        // Trigonometric functions
        StdlibFunction::pure("sin", sin, 1, Some(1), "Sine function"),
        StdlibFunction::pure("cos", cos, 1, Some(1), "Cosine function"),
        StdlibFunction::pure("tan", tan, 1, Some(1), "Tangent function"),
        StdlibFunction::pure("asin", asin, 1, Some(1), "Arcsine function"),
        StdlibFunction::pure("acos", acos, 1, Some(1), "Arccosine function"),
        StdlibFunction::pure("atan", atan, 1, Some(1), "Arctangent function"),
        StdlibFunction::pure("atan2", atan2, 2, Some(2), "Two-argument arctangent"),
        // Hyperbolic functions
        StdlibFunction::pure("sinh", sinh, 1, Some(1), "Hyperbolic sine"),
        StdlibFunction::pure("cosh", cosh, 1, Some(1), "Hyperbolic cosine"),
        StdlibFunction::pure("tanh", tanh, 1, Some(1), "Hyperbolic tangent"),
        // Exponential and logarithmic
        StdlibFunction::pure("exp", exp, 1, Some(1), "Exponential function"),
        StdlibFunction::pure(
            "log",
            log,
            1,
            Some(2),
            "Natural logarithm or logarithm with base",
        ),
        StdlibFunction::pure("log10", log10, 1, Some(1), "Base 10 logarithm"),
        StdlibFunction::pure("log2", log2, 1, Some(1), "Base 2 logarithm"),
        StdlibFunction::pure("pow", pow, 2, Some(2), "Power function"),
        StdlibFunction::pure("**", pow, 2, Some(2), "Exponentiation operator"),
        StdlibFunction::pure("sqrt", sqrt, 1, Some(1), "Square root"),
        // Rounding functions
        StdlibFunction::pure("ceil", ceil, 1, Some(1), "Ceiling function"),
        StdlibFunction::pure("floor", floor, 1, Some(1), "Floor function"),
        StdlibFunction::pure("round", round, 1, Some(1), "Round to nearest integer"),
        StdlibFunction::pure(
            "round-to",
            round_to,
            2,
            Some(2),
            "Round to specific decimal places",
        ),
        StdlibFunction::pure("trunc", trunc, 1, Some(1), "Truncate to integer"),
        // Constants
        StdlibFunction::pure("pi", pi, 0, Some(0), "Mathematical constant pi"),
        StdlibFunction::pure("e", e, 0, Some(0), "Mathematical constant e"),
        StdlibFunction::pure("tau", tau, 0, Some(0), "Mathematical constant tau (2*pi)"),
        // Utility functions
        StdlibFunction::pure("degrees", degrees, 1, Some(1), "Convert radians to degrees"),
        StdlibFunction::pure("radians", radians, 1, Some(1), "Convert degrees to radians"),
        StdlibFunction::pure("hypot", hypot, 2, Some(2), "Euclidean distance"),
        StdlibFunction::pure("factorial", factorial, 1, Some(1), "Factorial function"),
        StdlibFunction::pure("gcd", gcd, 2, Some(2), "Greatest common divisor"),
        StdlibFunction::pure("lcm", lcm, 2, Some(2), "Least common multiple"),
        // Comparison functions
        StdlibFunction::pure("max", max, 1, None, "Maximum of numbers"),
        StdlibFunction::pure("min", min, 1, None, "Minimum of numbers"),
        // Statistical functions
        StdlibFunction::pure("sum", sum, 1, Some(1), "Sum of numbers in a list"),
        StdlibFunction::pure(
            "product",
            product,
            1,
            Some(1),
            "Product of numbers in a list",
        ),
        StdlibFunction::pure("mean", mean, 1, Some(1), "Arithmetic mean"),
        StdlibFunction::pure(
            "clamp",
            clamp,
            3,
            Some(3),
            "Clamp value between min and max",
        ),
    ]);
}

// Basic arithmetic

fn add(args: &[Value]) -> Result<Value> {
    let mut result = 0.0;
    let mut all_ints = true;

    for arg in args {
        match arg {
            Value::Integer(i) => result += *i as f64,
            Value::Float(f) => {
                result += f;
                all_ints = false;
            }
            _ => return Err(anyhow!("+: expected number")),
        }
    }

    if all_ints && result.fract() == 0.0 {
        Ok(Value::Integer(result as i64))
    } else {
        Ok(Value::Float(result))
    }
}

fn subtract(args: &[Value]) -> Result<Value> {
    if args.len() == 1 {
        // Unary minus (negation)
        match &args[0] {
            Value::Integer(n) => Ok(Value::Integer(-n)),
            Value::Float(f) => Ok(Value::Float(-f)),
            _ => Err(anyhow!("-: expected number")),
        }
    } else {
        // Binary subtraction
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a - *b as f64)),
            _ => Err(anyhow!("-: expected numbers")),
        }
    }
}

fn multiply(args: &[Value]) -> Result<Value> {
    let mut result = 1.0;
    let mut all_ints = true;

    for arg in args {
        match arg {
            Value::Integer(i) => result *= *i as f64,
            Value::Float(f) => {
                result *= f;
                all_ints = false;
            }
            _ => return Err(anyhow!("*: expected number")),
        }
    }

    if all_ints && result.fract() == 0.0 {
        Ok(Value::Integer(result as i64))
    } else {
        Ok(Value::Float(result))
    }
}

// Comparison functions

fn max(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("max: expected at least one argument"));
    }

    let mut result = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Float(f) => *f,
        _ => return Err(anyhow!("max: expected numbers")),
    };
    let mut is_float = matches!(&args[0], Value::Float(_));

    for arg in &args[1..] {
        match arg {
            Value::Integer(i) => result = result.max(*i as f64),
            Value::Float(f) => {
                result = result.max(*f);
                is_float = true;
            }
            _ => return Err(anyhow!("max: expected numbers")),
        }
    }

    if is_float || result.fract() != 0.0 {
        Ok(Value::Float(result))
    } else {
        Ok(Value::Integer(result as i64))
    }
}

fn min(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("min: expected at least one argument"));
    }

    let mut result = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Float(f) => *f,
        _ => return Err(anyhow!("min: expected numbers")),
    };
    let mut is_float = matches!(&args[0], Value::Float(_));

    for arg in &args[1..] {
        match arg {
            Value::Integer(i) => result = result.min(*i as f64),
            Value::Float(f) => {
                result = result.min(*f);
                is_float = true;
            }
            _ => return Err(anyhow!("min: expected numbers")),
        }
    }

    if is_float || result.fract() != 0.0 {
        Ok(Value::Float(result))
    } else {
        Ok(Value::Integer(result as i64))
    }
}

fn divide(args: &[Value]) -> Result<Value> {
    let divisor = match &args[1] {
        Value::Integer(i) => *i as f64,
        Value::Float(f) => *f,
        _ => return Err(anyhow!("/: expected number")),
    };

    if divisor == 0.0 {
        return Err(anyhow!("/: division by zero"));
    }

    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => {
            // Integer division for int/int
            Ok(Value::Integer(a / b))
        }
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
        (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 / b)),
        (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a / *b as f64)),
        _ => Err(anyhow!("/: expected numbers")),
    }
}

// Number predicates

fn is_zero(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Integer(i) => Ok(Value::Boolean(*i == 0)),
        Value::Float(f) => Ok(Value::Boolean(*f == 0.0)),
        _ => Err(anyhow!("zero?: expected number")),
    }
}

fn is_even(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Integer(i) => Ok(Value::Boolean(i % 2 == 0)),
        _ => Err(anyhow!("even?: expected integer")),
    }
}

fn is_odd(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Integer(i) => Ok(Value::Boolean(i % 2 != 0)),
        _ => Err(anyhow!("odd?: expected integer")),
    }
}

fn sign(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Integer(i) => Ok(Value::Integer(i.signum())),
        Value::Float(f) => {
            if f > &0.0 {
                Ok(Value::Integer(1))
            } else if f < &0.0 {
                Ok(Value::Integer(-1))
            } else {
                Ok(Value::Integer(0))
            }
        }
        _ => Err(anyhow!("sign: expected number")),
    }
}

fn ln(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    if x <= 0.0 {
        Err(anyhow!("ln: argument must be positive"))
    } else {
        Ok(Value::Float(x.ln()))
    }
}

// Helper to convert Value to f64
fn to_float(value: &Value) -> Result<f64> {
    match value {
        Value::Integer(i) => Ok(*i as f64),
        Value::Float(f) => Ok(*f),
        _ => Err(anyhow!("expected number")),
    }
}

// Helper to convert f64 result back to appropriate Value
fn from_float(f: f64) -> Value {
    if f.fract() == 0.0 && f.is_finite() && f >= i64::MIN as f64 && f <= i64::MAX as f64 {
        Value::Integer(f as i64)
    } else {
        Value::Float(f)
    }
}

// Trigonometric functions

fn sin(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.sin()))
}

fn cos(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.cos()))
}

fn tan(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.tan()))
}

fn asin(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.asin()))
}

fn acos(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.acos()))
}

fn atan(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.atan()))
}

fn atan2(args: &[Value]) -> Result<Value> {
    let y = to_float(&args[0])?;
    let x = to_float(&args[1])?;
    Ok(Value::Float(y.atan2(x)))
}

// Hyperbolic functions

fn sinh(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.sinh()))
}

fn cosh(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.cosh()))
}

fn tanh(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.tanh()))
}

// Exponential and logarithmic

fn exp(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.exp()))
}

fn log(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    if x <= 0.0 {
        return Err(anyhow!("log: input must be positive"));
    }

    if args.len() == 2 {
        let base = to_float(&args[1])?;
        if base <= 0.0 || base == 1.0 {
            return Err(anyhow!("log: base must be positive and not equal to 1"));
        }
        Ok(Value::Float(x.log(base)))
    } else {
        Ok(Value::Float(x.ln()))
    }
}

fn log10(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    if x <= 0.0 {
        return Err(anyhow!("log10: input must be positive"));
    }
    Ok(Value::Float(x.log10()))
}

fn log2(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    if x <= 0.0 {
        return Err(anyhow!("log2: input must be positive"));
    }
    Ok(Value::Float(x.log2()))
}

fn pow(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Integer(base), Value::Integer(exp)) if *exp >= 0 => {
            // For positive integer exponents, we can use integer arithmetic
            let result = (*base as f64).powi(*exp as i32);
            if result.fract() == 0.0 && result >= i64::MIN as f64 && result <= i64::MAX as f64 {
                Ok(Value::Integer(result as i64))
            } else {
                Ok(Value::Float(result))
            }
        }
        _ => {
            // For all other cases, use floating point
            let base = to_float(&args[0])?;
            let exp = to_float(&args[1])?;
            Ok(Value::Float(base.powf(exp)))
        }
    }
}

fn sqrt(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.sqrt()))
}

// Rounding functions

fn ceil(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.ceil()))
}

fn floor(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.floor()))
}

fn round(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(Value::Float(x.round()))
}

fn round_to(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    let places = match &args[1] {
        Value::Integer(i) => *i,
        _ => return Err(anyhow!("round-to: expected integer for decimal places")),
    };

    let multiplier = 10.0_f64.powi(places as i32);
    Ok(Value::Float((x * multiplier).round() / multiplier))
}

fn trunc(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    Ok(from_float(x.trunc()))
}

// Constants

fn pi(_args: &[Value]) -> Result<Value> {
    Ok(Value::Float(consts::PI))
}

fn e(_args: &[Value]) -> Result<Value> {
    Ok(Value::Float(consts::E))
}

fn tau(_args: &[Value]) -> Result<Value> {
    Ok(Value::Float(consts::TAU))
}

// Utility functions

fn degrees(args: &[Value]) -> Result<Value> {
    let radians = to_float(&args[0])?;
    Ok(Value::Float(radians.to_degrees()))
}

fn radians(args: &[Value]) -> Result<Value> {
    let degrees = to_float(&args[0])?;
    Ok(Value::Float(degrees.to_radians()))
}

fn hypot(args: &[Value]) -> Result<Value> {
    let x = to_float(&args[0])?;
    let y = to_float(&args[1])?;
    Ok(Value::Float(x.hypot(y)))
}

fn factorial(args: &[Value]) -> Result<Value> {
    let n = match &args[0] {
        Value::Integer(i) => *i,
        _ => return Err(anyhow!("factorial: expected integer")),
    };

    if n < 0 {
        return Err(anyhow!("factorial: input must be non-negative"));
    }

    if n > 20 {
        // Factorial grows very quickly, use float for large values
        let mut result = 1.0;
        for i in 2..=n {
            result *= i as f64;
        }
        Ok(Value::Float(result))
    } else {
        let mut result = 1i64;
        for i in 2..=n {
            result *= i;
        }
        Ok(Value::Integer(result))
    }
}

fn gcd(args: &[Value]) -> Result<Value> {
    let a = match &args[0] {
        Value::Integer(i) => i.abs(),
        _ => return Err(anyhow!("gcd: expected integer")),
    };

    let b = match &args[1] {
        Value::Integer(i) => i.abs(),
        _ => return Err(anyhow!("gcd: expected integer")),
    };

    fn gcd_helper(a: i64, b: i64) -> i64 {
        if b == 0 {
            a
        } else {
            gcd_helper(b, a % b)
        }
    }

    Ok(Value::Integer(gcd_helper(a, b)))
}

fn lcm(args: &[Value]) -> Result<Value> {
    let a = match &args[0] {
        Value::Integer(i) => i.abs(),
        _ => return Err(anyhow!("lcm: expected integer")),
    };

    let b = match &args[1] {
        Value::Integer(i) => i.abs(),
        _ => return Err(anyhow!("lcm: expected integer")),
    };

    if a == 0 || b == 0 {
        Ok(Value::Integer(0))
    } else {
        // Use GCD to calculate LCM
        let gcd_val = match gcd(args)? {
            Value::Integer(g) => g,
            _ => unreachable!(),
        };
        Ok(Value::Integer(a * b / gcd_val))
    }
}

// Statistical functions

fn sum(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut int_sum = 0i64;
            let mut float_sum = 0.0;
            let mut has_float = false;

            for item in items {
                match item {
                    Value::Integer(i) => int_sum += i,
                    Value::Float(f) => {
                        has_float = true;
                        float_sum += f;
                    }
                    _ => return Err(anyhow!("sum: list must contain only numbers")),
                }
            }

            if has_float {
                Ok(Value::Float(int_sum as f64 + float_sum))
            } else {
                Ok(Value::Integer(int_sum))
            }
        }
        _ => Err(anyhow!("sum: expected list")),
    }
}

fn product(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut int_product = 1i64;
            let mut float_product = 1.0;
            let mut has_float = false;

            for item in items {
                match item {
                    Value::Integer(i) => int_product *= i,
                    Value::Float(f) => {
                        has_float = true;
                        float_product *= f;
                    }
                    _ => return Err(anyhow!("product: list must contain only numbers")),
                }
            }

            if has_float {
                Ok(Value::Float(int_product as f64 * float_product))
            } else {
                Ok(Value::Integer(int_product))
            }
        }
        _ => Err(anyhow!("product: expected list")),
    }
}

fn mean(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            if items.is_empty() {
                return Err(anyhow!("mean: empty list"));
            }

            let sum_val = sum(args)?;
            match sum_val {
                Value::Integer(i) => Ok(Value::Float(i as f64 / items.len() as f64)),
                Value::Float(f) => Ok(Value::Float(f / items.len() as f64)),
                _ => unreachable!(),
            }
        }
        _ => Err(anyhow!("mean: expected list")),
    }
}

fn clamp(args: &[Value]) -> Result<Value> {
    let value = to_float(&args[0])?;
    let min = to_float(&args[1])?;
    let max = to_float(&args[2])?;

    if min > max {
        return Err(anyhow!("clamp: min must be less than or equal to max"));
    }

    let clamped = value.max(min).min(max);

    // Try to preserve integer type if possible
    match &args[0] {
        Value::Integer(_) if clamped == clamped.trunc() => Ok(Value::Integer(clamped as i64)),
        _ => Ok(Value::Float(clamped)),
    }
}
