//! Test the complete effect system integration

use fluentai_parser::parse;
use fluentai_types::TypeChecker;
use fluentai_vm::{compile, VM, Bytecode};
use fluentai_effects::{EffectContext, handlers::*};
use fluentai_core::value::Value;
use std::sync::Arc;
use std::sync::Mutex;

// Custom IO handler for testing
struct TestIOHandler {
    output: Arc<Mutex<Vec<String>>>,
}

impl TestIOHandler {
    fn new() -> Self {
        Self {
            output: Arc::new(Mutex::new(Vec::new())),
        }
    }
    
    fn get_output(&self) -> Vec<String> {
        self.output.lock().unwrap().clone()
    }
}

impl fluentai_effects::EffectHandler for TestIOHandler {
    fn effect_type(&self) -> fluentai_core::ast::EffectType {
        fluentai_core::ast::EffectType::IO
    }
    
    fn handle_sync(&self, operation: &str, args: &[Value]) -> fluentai_effects::EffectResult {
        match operation {
            "print_line" => {
                let output = args.iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<_>>()
                    .join(" ");
                self.output.lock().unwrap().push(output);
                Ok(Value::Nil)
            }
            _ => Err(fluentai_core::error::Error::Runtime(format!(
                "Test IO handler: unknown operation '{}'", operation
            )))
        }
    }
}

fn main() {
    println!("Testing Effect System Integration\n");
    
    // Test 1: Basic IO effect through stdlib
    test_io_effect();
    
    // Test 2: Higher-order functions with effects
    test_higher_order_with_effects();
    
    // Test 3: Custom effect handler
    test_custom_effect_handler();
    
    println!("\nAll tests passed!");
}

fn test_io_effect() {
    println!("Test 1: Basic IO effect through stdlib");
    
    let code = r#"
        (print-line "Hello from effect system!")
    "#;
    
    let ast = parse(code).expect("Failed to parse");
    let mut type_checker = TypeChecker::new();
    let typed_ast = type_checker.check(ast).expect("Type check failed");
    let bytecode = compile(typed_ast).expect("Compilation failed");
    
    // Create VM with custom IO handler
    let test_handler = Arc::new(TestIOHandler::new());
    let effect_context = EffectContext::new();
    effect_context.register_handler(test_handler.clone());
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(Arc::new(effect_context));
    
    let result = vm.run().expect("VM execution failed");
    println!("  Result: {:?}", result);
    
    let output = test_handler.get_output();
    assert_eq!(output, vec!["Hello from effect system!"]);
    println!("  Output captured: {:?}", output);
    println!("  ✓ IO effect worked correctly\n");
}

fn test_higher_order_with_effects() {
    println!("Test 2: Higher-order functions with effects");
    
    let code = r#"
        (define numbers (list 1 2 3 4 5))
        (map (lambda (x) 
               (begin 
                 (print-line (str "Processing: " x))
                 (* x x)))
             numbers)
    "#;
    
    let ast = parse(code).expect("Failed to parse");
    let mut type_checker = TypeChecker::new();
    let typed_ast = type_checker.check(ast).expect("Type check failed");
    let bytecode = compile(typed_ast).expect("Compilation failed");
    
    // Create VM with custom IO handler
    let test_handler = Arc::new(TestIOHandler::new());
    let effect_context = EffectContext::new();
    effect_context.register_handler(test_handler.clone());
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(Arc::new(effect_context));
    
    let result = vm.run().expect("VM execution failed");
    println!("  Result: {:?}", result);
    
    let output = test_handler.get_output();
    assert_eq!(output.len(), 5);
    assert_eq!(output[0], "Processing: 1");
    assert_eq!(output[4], "Processing: 5");
    println!("  Output captured: {:?}", output);
    
    // Check the result is squared numbers
    match result {
        Value::List(values) => {
            assert_eq!(values.len(), 5);
            assert_eq!(values[0], Value::Integer(1));
            assert_eq!(values[1], Value::Integer(4));
            assert_eq!(values[2], Value::Integer(9));
            assert_eq!(values[3], Value::Integer(16));
            assert_eq!(values[4], Value::Integer(25));
        }
        _ => panic!("Expected list result"),
    }
    println!("  ✓ Higher-order functions propagated effects correctly\n");
}

fn test_custom_effect_handler() {
    println!("Test 3: Custom effect handler");
    
    // Create a custom state handler that tracks function calls
    struct CallTracker {
        calls: Arc<Mutex<Vec<String>>>,
    }
    
    impl CallTracker {
        fn new() -> Self {
            Self {
                calls: Arc::new(Mutex::new(Vec::new())),
            }
        }
        
        fn get_calls(&self) -> Vec<String> {
            self.calls.lock().unwrap().clone()
        }
    }
    
    impl fluentai_effects::EffectHandler for CallTracker {
        fn effect_type(&self) -> fluentai_core::ast::EffectType {
            fluentai_core::ast::EffectType::State
        }
        
        fn handle_sync(&self, operation: &str, args: &[Value]) -> fluentai_effects::EffectResult {
            match operation {
                "track" => {
                    if let Some(Value::String(s)) = args.get(0) {
                        self.calls.lock().unwrap().push(s.clone());
                    }
                    Ok(Value::Nil)
                }
                _ => Ok(Value::Nil)
            }
        }
    }
    
    let code = r#"
        (define track-call (lambda (name) 
          (begin 
            (state:track name)
            name)))
        
        (filter (lambda (x) 
                  (begin
                    (track-call (str "checking " x))
                    (> x 2)))
                (list 1 2 3 4 5))
    "#;
    
    let ast = parse(code).expect("Failed to parse");
    let mut type_checker = TypeChecker::new();
    let typed_ast = type_checker.check(ast).expect("Type check failed");
    let bytecode = compile(typed_ast).expect("Compilation failed");
    
    // Create VM with custom effect handler
    let tracker = Arc::new(CallTracker::new());
    let effect_context = EffectContext::new();
    effect_context.register_handler(tracker.clone());
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(Arc::new(effect_context));
    
    let result = vm.run().expect("VM execution failed");
    println!("  Result: {:?}", result);
    
    let calls = tracker.get_calls();
    assert_eq!(calls.len(), 5);
    println!("  Tracked calls: {:?}", calls);
    
    match result {
        Value::List(values) => {
            assert_eq!(values.len(), 3);
            assert_eq!(values, vec![Value::Integer(3), Value::Integer(4), Value::Integer(5)]);
        }
        _ => panic!("Expected list result"),
    }
    println!("  ✓ Custom effect handler worked correctly");
}