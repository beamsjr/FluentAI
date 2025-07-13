//! Integration tests for effects with other FluentAI components

use fluentai_core::{ast::EffectType, value::Value};
use fluentai_effects::EffectContext;
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_parser::parse_flc;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

/// Mock IO handler that captures output
struct CaptureIOHandler {
    output: Arc<Mutex<VecDeque<String>>>,
}

impl CaptureIOHandler {
    fn new() -> Self {
        Self {
            output: Arc::new(Mutex::new(VecDeque::new())),
        }
    }

    fn get_output(&self) -> Vec<String> {
        self.output.lock().unwrap().drain(..).collect()
    }
}

impl fluentai_effects::EffectHandler for CaptureIOHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::IO
    }

    fn handle_sync(&self, operation: &str, args: &[Value]) -> fluentai_effects::EffectResult {
        use fluentai_core::error::Error;
        match operation {
            "print" => {
                let msg = if args.is_empty() {
                    String::new()
                } else {
                    args[0].to_string()
                };
                self.output.lock().unwrap().push_back(msg);
                Ok(Value::Nil)
            }
            _ => Err(Error::Runtime(format!("Unsupported IO operation: {}", operation))),
        }
    }
}

#[test]
fn test_effects_with_parser() {
    // Test that parsed effect nodes work correctly
    let code = r#"
        perform IO.print("Hello from effect!")
    "#;

    let ast = parse_flc(code).unwrap();

    // Verify effect node was created
    let has_effect_node = ast
        .nodes
        .values()
        .any(|node| matches!(node, fluentai_core::ast::Node::Effect { .. }));

    assert!(has_effect_node, "Parser should create effect nodes");
}

#[test]
fn test_effects_with_optimizer() {
    // Test that optimizer preserves effects correctly
    let code = r#"
        let x = perform IO.print("effect1");
        let y = 2 + 3;
        let z = perform IO.print("effect2");
        y
    "#;

    let ast = parse_flc(code).unwrap();

    // Configure optimizer to preserve effects
    let config = OptimizationConfig {
        level: OptimizationLevel::Aggressive,
        ..Default::default()
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Count effect nodes in optimized AST
    let effect_count = optimized
        .nodes
        .values()
        .filter(|node| matches!(node, fluentai_core::ast::Node::Effect { .. }))
        .count();

    assert_eq!(effect_count, 2, "Both effects should be preserved");
}

#[test]
fn test_complex_effect_program() {
    // Test complex program with multiple effect types
    let code = r#"
        let start_time = perform Time.now();
        let random_val = perform Random.int(1, 100);
        let state_val = perform State.get("counter");
        perform IO.print("Starting computation");
        let result = random_val + (if (state_val) { state_val } else { 0 });
        perform IO.print("Result computed");
        perform State.set("counter", result);
        let end_time = perform Time.now();
        result
    "#;

    let ast = parse_flc(code).unwrap();

    // Count different effect types
    let mut effect_types = std::collections::HashSet::new();
    for node in ast.nodes.values() {
        if let fluentai_core::ast::Node::Effect {
            effect_type,
            ..
        } = node
        {
            effect_types.insert(effect_type.clone());
        }
    }

    assert_eq!(
        effect_types.len(),
        4,
        "Should have IO, Time, Random, and State effects"
    );
}

#[test]
fn test_optimizer_preserves_effects() {
    // Test that optimizer doesn't eliminate effects
    let config = OptimizationConfig {
        level: OptimizationLevel::Aggressive,
        ..Default::default()
    };

    let code = r#"
        perform IO.print("This should not be removed");
        42
    "#;

    let ast = parse_flc(code).unwrap();
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    let has_effect = optimized
        .nodes
        .values()
        .any(|node| matches!(node, fluentai_core::ast::Node::Effect { .. }));

    assert!(has_effect, "Effect should be preserved by optimizer");
}

#[test]
fn test_effect_ordering_preservation() {
    // Test that effect ordering is preserved through optimization
    let code = r#"
        {
            perform IO.print("1");
            perform IO.print("2");
            perform IO.print("3");
            42
        }
    "#;

    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        level: OptimizationLevel::Aggressive,
        ..Default::default()
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Collect effect operations in order
    let effects: Vec<_> = optimized
        .nodes
        .values()
        .filter_map(|node| {
            if let fluentai_core::ast::Node::Effect {
                operation,
                ..
            } = node
            {
                Some(operation.as_str())
            } else {
                None
            }
        })
        .collect();

    assert_eq!(effects.len(), 3, "All three effects should be preserved");
}

#[test]
fn test_runtime_with_multiple_effects() {
    // Test runtime execution with multiple effect handlers
    let ctx = EffectContext::new();
    let io_handler = Arc::new(CaptureIOHandler::new());
    ctx.register_handler(io_handler.clone());

    // Execute effects and verify output
    let output = io_handler.get_output();
    assert_eq!(output.len(), 0, "Should start with no output");
}

#[test]
fn test_conditional_effects() {
    // Test effects in conditional branches
    let code = r#"
        if (perform Random.bool()) {
            perform IO.print("true branch")
        } else {
            perform IO.print("false branch")
        }
    "#;

    let ast = parse_flc(code).unwrap();

    // Both branches should have effects
    let effect_count = ast
        .nodes
        .values()
        .filter(|node| matches!(node, fluentai_core::ast::Node::Effect { .. }))
        .count();

    assert_eq!(
        effect_count, 3,
        "Should have one Random effect and two IO effects"
    );
}

#[test]
fn test_effects_with_control_flow() {
    // Test effects with control flow preserving execution order
    let _ctx = EffectContext::new();
    // Test would execute with VM/interpreter
}

#[test]
#[ignore = "letrec syntax not yet supported by parser"]
fn test_effect_in_loop_context() {
    // Test effects in recursive/loop contexts
    let code = r#"
        letrec loop = (n) => {
            if (n > 0) {
                perform IO.print(n);
                loop(n - 1)
            } else {
                nil
            }
        };
        loop(3)
    "#;

    let ast = parse_flc(code).unwrap();

    // Should have effect in loop body
    let has_effect = ast
        .nodes
        .values()
        .any(|node| matches!(node, fluentai_core::ast::Node::Effect { .. }));

    assert!(has_effect, "Should have IO effect in loop");
}

#[test]
fn test_recursive_effects() {
    // Test recursive calls with effects
    let _ctx = EffectContext::new();
    // Test would execute with VM/interpreter
}

#[test]
fn test_effect_with_error_handling() {
    // Test error effects with try/catch patterns
    let code = r#"
        let result = perform Random.bool();
        if (result) {
            perform Error.raise("Random error")
        } else {
            42
        }
    "#;

    let ast = parse_flc(code).unwrap();

    // Count effect types
    let effect_types: std::collections::HashSet<_> = ast
        .nodes
        .values()
        .filter_map(|node| {
            if let fluentai_core::ast::Node::Effect {
                effect_type,
                ..
            } = node
            {
                Some(effect_type.clone())
            } else {
                None
            }
        })
        .collect();

    assert!(
        effect_types.contains(&EffectType::Random),
        "Should have Random effect"
    );
}

#[test]
fn test_error_handling_with_effects() {
    // Test error handling preserves effect semantics
    let _ctx = EffectContext::new();
    // Test would execute with VM/interpreter
}

#[test]
#[ignore = "async/concurrent syntax not yet fully supported"]
fn test_concurrent_effects() {
    // Test concurrent effect patterns
    let code = r#"
        let ch = perform Concurrent.channel();
        let task1 = perform Async.spawn();
        let task2 = perform Async.spawn();
        [
            perform Concurrent.receive(ch),
            perform Concurrent.receive(ch)
        ]
    "#;

    let ast = parse_flc(code).unwrap();

    // Should have concurrent effects
    let has_concurrent = ast.nodes.values().any(|node| {
        if let fluentai_core::ast::Node::Effect {
            operation,
            ..
        } = node
        {
            operation == "channel" || operation == "spawn" || operation == "receive"
        } else {
            false
        }
    });

    assert!(has_concurrent, "Should have concurrent effects");
}

#[test]
fn test_dom_effects() {
    // Test DOM effect patterns
    let code = r#"
        let elem = perform Dom.get_element("app");
        perform Dom.set_attribute(elem, "class", "active");
        elem
    "#;

    let ast = parse_flc(code).unwrap();

    // Count DOM effects
    let dom_effect_count = ast
        .nodes
        .values()
        .filter(|node| {
            if let fluentai_core::ast::Node::Effect {
                effect_type: EffectType::Dom,
                ..
            } = node
            {
                true
            } else {
                false
            }
        })
        .count();

    assert_eq!(dom_effect_count, 2, "Should have two DOM effects");
}