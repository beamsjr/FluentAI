//! Example demonstrating dynamic optimization pipeline composition with DI

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use fluentai_di::prelude::*;
use fluentai_optimizer::di::*;
use fluentai_optimizer::*;
use fluentai_parser::parse_flc;
use std::sync::Arc;

fn main() -> Result<()> {
    println!("=== Dynamic Optimization Pipeline Demo ===\n");

    // 1. Basic pipeline builder usage
    basic_pipeline_demo()?;

    // 2. Custom pass registration
    custom_pass_demo()?;

    // 3. DI integration demo
    di_integration_demo()?;

    // 4. Runtime pipeline composition
    runtime_composition_demo()?;

    Ok(())
}

fn basic_pipeline_demo() -> Result<()> {
    println!("1. Basic Pipeline Builder Demo");
    println!("-----------------------------");

    // Create a sample program
    let program = r#"
        (let ((x (+ 10 20))
              (y (* x 2))
              (z 30)
              (unused 100))
          (if #t
              (+ y z)
              0))
    "#;

    let graph = parse_flc(program)?;

    println!("Original program: {} nodes", graph.nodes.len());

    // Create pipeline with standard optimization level
    let mut pipeline = OptimizationPipelineBuilder::with_level(OptimizationLevel::Standard)
        .with_default_passes()
        .build();

    let optimized = pipeline.optimize(&graph)?;

    println!("Optimized program: {} nodes", optimized.nodes.len());
    println!(
        "Optimization time: {} µs",
        pipeline.stats().optimization_time_us
    );
    println!();

    Ok(())
}

fn custom_pass_demo() -> Result<()> {
    println!("2. Custom Pass Registration Demo");
    println!("-------------------------------");

    // Create a custom optimization pass
    struct CustomConstantPass {
        replacements: usize,
    }

    impl CustomConstantPass {
        fn new() -> Self {
            Self { replacements: 0 }
        }
    }

    impl fluentai_optimizer::passes::OptimizationPass for CustomConstantPass {
        fn name(&self) -> &str {
            "custom_constant_replacer"
        }

        fn run(&mut self, graph: &Graph) -> Result<Graph> {
            let mut new_graph = graph.clone();

            // Simple example: replace all integer literals with 42
            for (id, node) in &mut new_graph.nodes {
                if let Node::Literal(Literal::Integer(_)) = node {
                    *node = Node::Literal(Literal::Integer(42));
                    self.replacements += 1;
                }
            }

            println!("  Custom pass replaced {} constants", self.replacements);
            Ok(new_graph)
        }

        fn is_applicable(&self, _graph: &Graph) -> bool {
            true
        }
    }

    // Create program with some integers
    let program = r#"
        (let ((a 1)
              (b 2)
              (c 3))
          (+ (+ a b) c))
    "#;

    let graph = parse_flc(program)?;

    // Build pipeline with custom pass
    let mut pipeline = OptimizationPipelineBuilder::new()
        .add_custom_pass(|| Box::new(CustomConstantPass::new()))
        .build();

    let optimized = pipeline.optimize(&graph)?;
    println!("  Pipeline completed\n");

    Ok(())
}

fn di_integration_demo() -> Result<()> {
    println!("3. DI Integration Demo");
    println!("---------------------");

    // Create DI container with optimizer services
    let mut builder = ContainerBuilder::new();

    // Register optimizer with custom configuration
    let config = OptimizationConfig {
        level: OptimizationLevel::Aggressive,
        constant_folding: true,
        dead_code_elimination: true,
        cse: true,
        inline: true,
        inline_threshold: 15,
        tail_call_optimization: true,
        loop_optimization: true,
        beta_reduction: true,
        partial_evaluation: false, // Disable partial eval for this example
        max_iterations: 3,
        debug_mode: true,
    };

    builder.register_optimizer(config);

    let container = Arc::new(builder.build());

    // Resolve services
    let resolved_config = container.resolve::<OptimizationConfig>()?;
    println!("  Resolved config: {:?}", resolved_config.level);

    let pipeline_builder = container.resolve::<OptimizationPipelineBuilder>()?;
    println!("  Resolved pipeline builder");

    // Create provider
    let provider = ContainerOptimizationProvider::new(container);
    let mut pipeline = provider.create_pipeline()?;

    // Test with a program
    let program = r#"
        (letrec ((factorial 
                  (lambda (n)
                    (if (<= n 1)
                        1
                        (* n (factorial (- n 1)))))))
          (factorial 5))
    "#;

    let graph = parse_flc(program)?;

    println!("  Original: {} nodes", graph.nodes.len());

    let optimized = pipeline.optimize(&graph)?;

    println!("  Optimized: {} nodes", optimized.nodes.len());
    println!("  Time: {} µs\n", pipeline.stats().optimization_time_us);

    Ok(())
}

fn runtime_composition_demo() -> Result<()> {
    println!("4. Runtime Pipeline Composition Demo");
    println!("-----------------------------------");

    // Create a monitoring pass that logs what it sees
    struct MonitoringPass {
        pass_name: String,
        nodes_seen: usize,
    }

    impl MonitoringPass {
        fn new(name: impl Into<String>) -> Self {
            Self {
                pass_name: name.into(),
                nodes_seen: 0,
            }
        }
    }

    impl fluentai_optimizer::passes::OptimizationPass for MonitoringPass {
        fn name(&self) -> &str {
            &self.pass_name
        }

        fn run(&mut self, graph: &Graph) -> Result<Graph> {
            self.nodes_seen = graph.nodes.len();
            println!(
                "  [{}] Processing {} nodes",
                self.pass_name, self.nodes_seen
            );
            Ok(graph.clone())
        }

        fn is_applicable(&self, _graph: &Graph) -> bool {
            true
        }
    }

    // Build pipeline dynamically based on "user input"
    let enable_aggressive = true;
    let custom_threshold = 25;

    let mut builder = OptimizationPipelineBuilder::new();

    // Add monitoring at the start
    builder = builder.add_custom_pass(|| Box::new(MonitoringPass::new("pre_optimization")));

    // Conditionally add passes
    if enable_aggressive {
        builder = builder
            .with_config(OptimizationConfig {
                level: OptimizationLevel::Aggressive,
                inline_threshold: custom_threshold,
                ..OptimizationConfig::for_level(OptimizationLevel::Aggressive)
            })
            .with_default_passes();
    } else {
        builder = builder
            .with_config(OptimizationConfig::for_level(OptimizationLevel::Basic))
            .with_default_passes();
    }

    // Add monitoring at the end
    builder = builder.add_custom_pass(|| Box::new(MonitoringPass::new("post_optimization")));

    let mut pipeline = builder.build();

    // Test with a complex program
    let program = r#"
        (let ((sum (lambda (lst)
                     (if (null? lst)
                         0
                         (+ (car lst) (sum (cdr lst))))))
              (data (list 1 2 3 4 5)))
          (let ((total (sum data))
                (unused (sum (list 10 20 30))))
            (let ((double (* total 2)))
              double)))
    "#;

    let graph = parse_flc(program)?;

    println!("  Running dynamically composed pipeline:");
    let optimized = pipeline.optimize(&graph)?;

    println!("\n  Final result: {} nodes", optimized.nodes.len());
    println!("  Total time: {} µs", pipeline.stats().optimization_time_us);

    Ok(())
}

// Helper function to create a simple test graph
fn create_test_graph() -> Graph {
    let mut graph = Graph::new();

    // Add some nodes
    let n1 = graph
        .add_node(Node::Literal(Literal::Integer(10)))
        .expect("Failed to add node");
    let n2 = graph
        .add_node(Node::Literal(Literal::Integer(20)))
        .expect("Failed to add node");
    let n3 = graph
        .add_node(Node::Variable {
            name: "result".to_string(),
        })
        .expect("Failed to add node");
    graph.root_id = Some(n3);

    graph
}
