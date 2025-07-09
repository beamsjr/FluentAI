//! FluentAI SDK demonstration

use fluentai_sdk::prelude::*;
use fluentai_sdk::new_session;
use fluentai_sdk::{FluentAIBuilder, HostFunction};
use std::collections::HashMap;

fn main() -> anyhow::Result<()> {
    // Initialize the SDK
    fluentai_sdk::init()?;

    println!("=== FluentAI SDK Demo ===\n");

    // Example 1: Simple evaluation
    simple_eval_example()?;

    // Example 2: Session with custom functions
    custom_functions_example()?;

    // Example 3: Script execution
    script_execution_example()?;

    // Example 4: Sandboxed execution
    sandboxed_example()?;

    // Example 5: Value conversion
    value_conversion_example()?;

    Ok(())
}

fn simple_eval_example() -> anyhow::Result<()> {
    println!("1. Simple Evaluation Example:");

    // Quick evaluation
    let result = eval("(* 7 6)")?;
    println!("   7 * 6 = {:?}", result);

    // Using a session
    let mut session = new_session()?;
    let result = session.eval("(+ 1 2 3 4 5)")?;
    println!("   1 + 2 + 3 + 4 + 5 = {:?}", result);

    println!();
    Ok(())
}

fn custom_functions_example() -> anyhow::Result<()> {
    println!("2. Custom Functions Example:");

    // Define custom host functions
    let greet = HostFunction::new("greet", 1, |args| match &args[0] {
        Value::String(name) => Ok(Value::String(std::sync::Arc::new(format!(
            "Hello, {}!",
            name
        )))),
        _ => Err(fluentai_sdk::Error::Runtime(
            "greet expects a string".to_string(),
        )),
    });

    let add_tax = HostFunction::new("add-tax", 2, |args| match (&args[0], &args[1]) {
        (Value::Float(amount), Value::Float(rate)) => {
            let tax = amount * (rate / 100.0);
            Ok(Value::Float(amount + tax))
        }
        (Value::Integer(amount), Value::Float(rate)) => {
            let amount_f = *amount as f64;
            let tax = amount_f * (rate / 100.0);
            Ok(Value::Float(amount_f + tax))
        }
        (Value::Float(amount), Value::Integer(rate)) => {
            let rate_f = *rate as f64;
            let tax = amount * (rate_f / 100.0);
            Ok(Value::Float(amount + tax))
        }
        (Value::Integer(amount), Value::Integer(rate)) => {
            let amount_f = *amount as f64;
            let rate_f = *rate as f64;
            let tax = amount_f * (rate_f / 100.0);
            Ok(Value::Float(amount_f + tax))
        }
        _ => Err(fluentai_sdk::Error::Runtime(
            "add-tax expects two numbers".to_string(),
        )),
    });

    // Create session with custom functions
    let mut session = FluentAIBuilder::new()
        .add_function(greet)
        .add_function(add_tax)
        .build()?;

    // Use custom functions
    let result = session.eval(r#"(greet "World")"#)?;
    println!("   Greeting: {:?}", result);

    let result = session.eval("(add-tax 100 8.5)")?;
    println!("   $100 + 8.5% tax = ${:?}", result);

    println!();
    Ok(())
}

fn script_execution_example() -> anyhow::Result<()> {
    println!("3. Script Execution Example:");

    // Create a script
    let script = Script::new(
        "fibonacci",
        r#"
        ;; @description Calculate Fibonacci numbers
        ;; @version 1.0.0
        
        (define fib
          (lambda (n)
            (if (< n 2)
                n
                (+ (fib (- n 1)) (fib (- n 2))))))
        
        ;; Calculate first 10 Fibonacci numbers
        (define fib-list
          (lambda (n)
            (if (= n 0)
                '()
                (append (fib-list (- n 1)) (list (fib (- n 1)))))))
        
        (list (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) 
              (fib 5) (fib 6) (fib 7) (fib 8) (fib 9))
    "#,
    );

    let mut session = new_session()?;
    let result = session.run_script(&script)?;
    println!("   First 10 Fibonacci numbers: {:?}", result);

    println!();
    Ok(())
}

fn sandboxed_example() -> anyhow::Result<()> {
    println!("4. Sandboxed Execution Example:");

    // Create a sandboxed session with timeout
    let mut session = FluentAIBuilder::sandboxed()
        .timeout(1000) // 1 second timeout
        .allow_fs(false)
        .allow_network(false)
        .build()?;

    // Safe computation
    let result = session.eval("(* 2 3 4 5)")?;
    println!("   Safe computation: 2 * 3 * 4 * 5 = {:?}", result);

    // This would timeout if it took too long
    let result = session.eval(
        r#"
        (define factorial
          (lambda (n)
            (if (<= n 1) 1 (* n (factorial (- n 1))))))
        (factorial 10)
    "#,
    )?;
    println!("   10! = {:?}", result);

    println!();
    Ok(())
}

fn value_conversion_example() -> anyhow::Result<()> {
    println!("5. Value Conversion Example:");

    // Create a session with a custom data function
    let get_data = HostFunction::new("get-data", 0, |_| {
        let mut data = HashMap::new();
        data.insert("name".to_string(), Value::String("Alice".into()));
        data.insert("age".to_string(), Value::Float(30.0));
        data.insert(
            "scores".to_string(),
            Value::List(
                vec![
                    Value::Float(95.0),
                    Value::Float(87.0),
                    Value::Float(92.0),
                ]
                .into(),
            ),
        );

        // Convert HashMap to association list
        let pairs: Vec<Value> = data
            .into_iter()
            .map(|(k, v)| Value::List(vec![Value::Symbol(k.into()), v].into()))
            .collect();

        Ok(Value::List(pairs.into()))
    });

    let mut session = FluentAIBuilder::new().add_function(get_data).build()?;

    // Get data from FluentAI
    let data = session.eval("(get-data)")?;
    println!("   Data from FluentAI: {:?}", data);

    // Convert FluentAI values to Rust types
    session.set_global("numbers", vec![1, 2, 3, 4, 5].into_value()?);
    let result = session.eval("(apply + numbers)")?;
    let sum: f64 = result.from_value()?;
    println!("   Sum of [1,2,3,4,5] = {}", sum);

    // String conversion
    session.set_global("message", "Hello from Rust!".into_value()?);
    let result = session.eval("(string-upcase message)")?;
    let upper: String = result.from_value()?;
    println!("   Uppercase: {}", upper);

    println!();
    Ok(())
}

// Extension trait for Value conversions
trait ValueExt {
    fn from_value<T: FromValue>(&self) -> Result<T>;
}

impl ValueExt for Value {
    fn from_value<T: FromValue>(&self) -> Result<T> {
        T::from_value(self)
    }
}
