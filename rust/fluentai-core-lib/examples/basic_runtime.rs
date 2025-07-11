//! Basic runtime usage example

use fluentai_core_lib::{HostFunction, RuntimeEngine, RuntimeConfig, Value};
use fluentai_core_lib::config::{DebugConfig, SecurityConfig};

fn main() -> anyhow::Result<()> {
    // Create a runtime engine with custom configuration
    let config = RuntimeConfig {
        debug: DebugConfig {
            enabled: true,
            ..Default::default()
        },
        security: SecurityConfig {
            max_execution_time: 5000, // 5 second timeout
            ..Default::default()
        },
        ..Default::default()
    };
    let mut engine = RuntimeEngine::new(config);

    // Register some host functions
    register_math_functions(&engine)?;
    register_io_functions(&engine)?;

    // Set some global values
    engine.set_global("pi", Value::Float(std::f64::consts::PI));
    engine.set_global("version", Value::String("1.0.0".into()));

    // Execute some code
    println!("=== Basic Arithmetic ===");
    let result = engine.execute("(+ 1 2 3)")?;
    println!("Result: {:?}", result);

    println!("\n=== Using Host Functions ===");
    let result = engine.execute("(sqrt 16)")?;
    println!("sqrt(16) = {:?}", result);

    println!("\n=== Using Globals ===");
    let result = engine.execute("(* 2 pi)")?;
    println!("2 * pi = {:?}", result);

    println!("\n=== Define and Call Function ===");
    let code = r#"
        (define square (lambda (x) (* x x)))
        (square 5)
    "#;
    let result = engine.execute(code)?;
    println!("square(5) = {:?}", result);

    println!("\n=== Complex Expression ===");
    let code = r#"
        (define factorial
          (lambda (n)
            (if (<= n 1)
                1
                (* n (factorial (- n 1))))))
        (factorial 5)
    "#;
    let result = engine.execute(code)?;
    println!("factorial(5) = {:?}", result);

    // Print execution statistics
    let stats = engine.stats();
    println!("\n=== Execution Statistics ===");
    println!("Total time: {:?}", stats.total_time);
    println!("Instructions executed: {}", stats.instructions_executed);
    println!("Function calls: {}", stats.function_calls);

    Ok(())
}

fn register_math_functions(engine: &RuntimeEngine) -> anyhow::Result<()> {
    // Square root
    let sqrt = HostFunction::new("sqrt", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.sqrt())),
        Value::Integer(n) => Ok(Value::Float((*n as f64).sqrt())),
        _ => Err(fluentai_core_lib::RuntimeError::host(
            "sqrt expects a number",
        )),
    })
    .with_doc("Calculate square root");

    // Power
    let pow = HostFunction::new("pow", 2, |args| match (&args[0], &args[1]) {
        (Value::Float(base), Value::Float(exp)) => Ok(Value::Float(base.powf(*exp))),
        (Value::Integer(base), Value::Integer(exp)) => {
            if *exp >= 0 && *exp <= u32::MAX as i64 {
                Ok(Value::Integer(base.pow(*exp as u32)))
            } else {
                Ok(Value::Float((*base as f64).powf(*exp as f64)))
            }
        }
        (Value::Float(base), Value::Integer(exp)) => Ok(Value::Float(base.powi(*exp as i32))),
        (Value::Integer(base), Value::Float(exp)) => Ok(Value::Float((*base as f64).powf(*exp))),
        _ => Err(fluentai_core_lib::RuntimeError::host(
            "pow expects two numbers",
        )),
    })
    .with_doc("Calculate power (base^exp)");

    // Absolute value
    let abs = HostFunction::new("abs", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.abs())),
        Value::Integer(n) => Ok(Value::Integer(n.abs())),
        _ => Err(fluentai_core_lib::RuntimeError::host("abs expects a number")),
    })
    .with_doc("Calculate absolute value");

    engine.register_functions(vec![sqrt, pow, abs])?;
    Ok(())
}

fn register_io_functions(engine: &RuntimeEngine) -> anyhow::Result<()> {
    // Print function
    let print = HostFunction::new("print", 0, |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{:?}", arg);
        }
        println!();
        Ok(Value::Nil)
    })
    .variadic()
    .with_doc("Print values to stdout");

    // Format function
    let format = HostFunction::new("format", 0, |args| {
        let mut result = String::new();
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                result.push(' ');
            }
            result.push_str(&format!("{:?}", arg));
        }
        Ok(Value::String(result))
    })
    .variadic()
    .with_doc("Format values as string");

    engine.register_functions(vec![print, format])?;
    Ok(())
}
