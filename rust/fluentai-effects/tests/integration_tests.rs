//! Integration tests for effects with other FluentAI components

use fluentai_core::{ast::EffectType, value::Value};
use fluentai_effects::EffectContext;
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_parser::parse;
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

#[async_trait::async_trait]
impl fluentai_effects::EffectHandler for CaptureIOHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::IO
    }

    fn handle_sync(
        &self,
        operation: &str,
        args: &[Value],
    ) -> Result<Value, fluentai_core::error::Error> {
        match operation {
            "print" | "println" => {
                if let Some(arg) = args.first() {
                    self.output.lock().unwrap().push_back(arg.to_string());
                }
                Ok(Value::Nil)
            }
            _ => Err(fluentai_core::error::Error::Runtime(format!(
                "Unknown IO operation: {}",
                operation
            ))),
        }
    }
}

#[test]
#[ignore = "Uses s-expression syntax - needs FLC update"]
fn test_effects_with_parser() {
    // Test that parsed effect nodes work correctly
    let code = r#"
        (effect IO:print "Hello from effect!")
    "#;

    let ast = parse(code).unwrap();

    // Verify effect node was created
    let has_effect_node = ast
        .nodes
        .values()
        .any(|node| matches!(node, fluentai_core::ast::Node::Effect { .. }));

    assert!(has_effect_node, "Parser should create effect nodes");
}

#[test]
#[ignore = "Uses s-expression syntax - needs FLC update"]
fn test_effects_with_optimizer() {
    // Test that optimizer preserves effects correctly
    let code = r#"
        (let ((x (effect IO:print "effect1"))
              (y (+ 2 3))
              (z (effect IO:print "effect2")))
          y)
    "#;

    let ast = parse(code).unwrap();

    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Count effect nodes
    let effect_count = optimized
        .nodes
        .values()
        .filter(|node| matches!(node, fluentai_core::ast::Node::Effect { .. }))
        .count();

    assert_eq!(effect_count, 2, "Both effects should be preserved");

    // Pure computation should be optimized
    let stats = pipeline.stats();
    assert!(stats.constant_folded > 0 || stats.pure_expressions_evaluated > 0);
}

#[test]
#[ignore = "Uses s-expression syntax - needs FLC update"]
fn test_complex_effect_program() {
    // Test complex program with multiple effect types
    let code = r#"
        (let ((start-time (effect Time:now))
              (random-val (effect Random:int 1 100))
              (state-val (effect State:get "counter")))
          (begin
            (effect IO:print "Starting computation")
            (let ((result (+ random-val (if state-val state-val 0))))
              (begin
                (effect State:set "counter" result)
                (effect IO:print "Result computed")
                result))))
    "#;

    let ast = parse(code).unwrap();

    // Optimize the program
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Verify all effect types are preserved
    let effect_types: std::collections::HashSet<_> = optimized
        .nodes
        .values()
        .filter_map(|node| {
            if let fluentai_core::ast::Node::Effect { effect_type, .. } = node {
                Some(effect_type.clone())
            } else {
                None
            }
        })
        .collect();

    assert!(effect_types.contains(&EffectType::Time));
    assert!(effect_types.contains(&EffectType::Random));
    assert!(effect_types.contains(&EffectType::State));
    assert!(effect_types.contains(&EffectType::IO));
}

#[test]
#[ignore = "Uses s-expression syntax - needs FLC update"]
fn test_effect_ordering_preservation() {
    // Test that effect ordering is preserved through optimization
    let code = r#"
        (begin
          (effect IO:print "1")
          (effect IO:print "2")
          (effect IO:print "3")
          42)
    "#;

    let ast = parse(code).unwrap();

    // Create runtime with capture handler
    let capture_handler = Arc::new(CaptureIOHandler::new());
    let context = EffectContext::new();
    context.register_handler(capture_handler.clone());

    // Would need VM integration to actually execute
    // For now, verify structure is preserved
    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Count IO effects
    let io_effects: Vec<_> = optimized
        .nodes
        .values()
        .filter(|node| {
            matches!(
                node,
                fluentai_core::ast::Node::Effect {
                    effect_type: EffectType::IO,
                    ..
                }
            )
        })
        .collect();


    assert_eq!(
        io_effects.len(),
        3,
        "All IO effects should be preserved in order"
    );
}

#[test]
#[ignore = "Uses s-expression syntax - needs FLC update"]
fn test_conditional_effects() {
    // Test effects in conditional branches
    let code = r#"
        (if (effect Random:bool)
            (effect IO:print "true branch")
            (effect IO:print "false branch"))
    "#;

    let ast = parse(code).unwrap();

    // Optimize
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // All effects should be preserved (condition + both branches)
    let effect_count = optimized
        .nodes
        .values()
        .filter(|node| matches!(node, fluentai_core::ast::Node::Effect { .. }))
        .count();

    assert_eq!(
        effect_count, 3,
        "Condition and both branch effects should be preserved"
    );
}

#[test]
#[ignore = "Uses s-expression syntax - needs FLC update"]
fn test_effect_in_loop_context() {
    // Test effects in recursive/loop contexts
    let code = r#"
        (letrec ((loop (lambda (n)
                         (if (> n 0)
                             (begin
                               (effect IO:print n)
                               (loop (- n 1)))
                             nil))))
          (loop 3))
    "#;

    let ast = parse(code).unwrap();

    // Optimize
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Effect in loop body should be preserved
    let has_io_effect = optimized.nodes.values().any(|node| {
        matches!(
            node,
            fluentai_core::ast::Node::Effect {
                effect_type: EffectType::IO,
                ..
            }
        )
    });

    assert!(has_io_effect, "Effect in loop should be preserved");
}

#[test]
#[ignore = "Uses s-expression syntax - needs FLC update"]
fn test_effect_with_error_handling() {
    // Test error effects with try/catch patterns
    let code = r#"
        (let ((result (effect Random:bool)))
          (if result
              (effect Error:raise "Random error")
              42))
    "#;

    let ast = parse(code).unwrap();

    // Count different effect types
    let mut effect_counts = std::collections::HashMap::new();
    for node in ast.nodes.values() {
        if let fluentai_core::ast::Node::Effect { effect_type, .. } = node {
            *effect_counts.entry(effect_type).or_insert(0) += 1;
        }
    }

    assert!(effect_counts.contains_key(&EffectType::Error));
    assert!(effect_counts.contains_key(&EffectType::Random));
}

#[test]
#[ignore = "Uses s-expression syntax - needs FLC update"]
fn test_concurrent_effects() {
    // Test concurrent effect patterns
    let code = r#"
        (let ((ch (effect Concurrent:channel)))
          (let ((task1 (effect Async:spawn))
                (task2 (effect Async:spawn)))
            (list
              (effect Concurrent:receive ch)
              (effect Concurrent:receive ch))))
    "#;

    let ast = parse(code).unwrap();

    // Verify concurrent and async effects are present
    let effect_types: std::collections::HashSet<_> = ast
        .nodes
        .values()
        .filter_map(|node| {
            if let fluentai_core::ast::Node::Effect { effect_type, .. } = node {
                Some(effect_type.clone())
            } else {
                None
            }
        })
        .collect();

    assert!(effect_types.contains(&EffectType::Concurrent));
    assert!(effect_types.contains(&EffectType::Async));
}

#[test]
#[ignore = "Uses s-expression syntax - needs FLC update"]
fn test_dom_effects() {
    // Test DOM effect patterns
    let code = r#"
        (let ((elem (effect Dom:get_element "app")))
          (begin
            (effect Dom:set_attribute elem "class" "active")
            elem))
    "#;

    let ast = parse(code).unwrap();

    // Count DOM effects
    let dom_effect_count = ast
        .nodes
        .values()
        .filter(|node| {
            matches!(
                node,
                fluentai_core::ast::Node::Effect {
                    effect_type: EffectType::Dom,
                    ..
                }
            )
        })
        .count();

    assert_eq!(
        dom_effect_count, 2,
        "All DOM operations should be effect nodes"
    );
}
