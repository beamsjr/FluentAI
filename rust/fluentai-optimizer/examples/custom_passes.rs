//! Example demonstrating how to create custom optimization passes

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use fluentai_optimizer::di::*;
use fluentai_optimizer::passes::OptimizationPass;
use fluentai_optimizer::*;
use fluentai_parser::parse;
use std::collections::{HashMap, HashSet};

fn main() -> Result<()> {
    println!("=== Custom Optimization Passes Demo ===\n");

    // 1. String interning pass
    string_interning_demo()?;

    // 2. Algebraic simplification pass
    algebraic_simplification_demo()?;

    // 3. Function specialization pass
    function_specialization_demo()?;

    // 4. Combined custom pipeline
    combined_pipeline_demo()?;

    Ok(())
}

/// String interning optimization - replace duplicate strings with references
struct StringInterningPass {
    strings: HashMap<String, NodeId>,
    replacements: usize,
}

impl StringInterningPass {
    fn new() -> Self {
        Self {
            strings: HashMap::new(),
            replacements: 0,
        }
    }
}

impl OptimizationPass for StringInterningPass {
    fn name(&self) -> &str {
        "string_interning"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        let mut new_graph = graph.clone();
        self.strings.clear();
        self.replacements = 0;

        // First pass: collect all string literal nodes
        for (id, node) in &new_graph.nodes {
            if let Node::Literal(Literal::String(s)) = node {
                if let Some(&existing_id) = self.strings.get(s) {
                    // Found duplicate - will need to replace references
                    self.replacements += 1;
                } else {
                    self.strings.insert(s.clone(), *id);
                }
            }
        }

        // Second pass: replace references to duplicate strings
        // (In a real implementation, we'd update all references)

        println!(
            "  String interning: found {} duplicate strings",
            self.replacements
        );
        Ok(new_graph)
    }

    fn is_applicable(&self, graph: &Graph) -> bool {
        // Only run if there are string literal nodes
        graph
            .nodes
            .values()
            .any(|n| matches!(n, Node::Literal(Literal::String(_))))
    }
}

fn string_interning_demo() -> Result<()> {
    println!("1. String Interning Pass Demo");
    println!("----------------------------");

    let program = r#"
        (let ((msg1 "Hello, World!")
              (msg2 "Hello, World!")
              (msg3 "Different message")
              (msg4 "Hello, World!"))
          (list msg1 msg2 msg3 msg4))
    "#;

    let graph = parse(program)?;

    let mut pipeline = OptimizationPipelineBuilder::new()
        .add_custom_pass(|| Box::new(StringInterningPass::new()))
        .build();

    pipeline.optimize(&graph)?;
    println!();

    Ok(())
}

/// Algebraic simplification - simplify mathematical expressions
struct AlgebraicSimplificationPass {
    simplifications: usize,
}

impl AlgebraicSimplificationPass {
    fn new() -> Self {
        Self { simplifications: 0 }
    }

    fn simplify_node(&mut self, node: &Node, graph: &Graph) -> Option<Node> {
        // In the real AST, binary operations are represented as function applications
        // For this demo, we'll check for literals that can be simplified
        match node {
            Node::Literal(Literal::Integer(n)) if *n == 0 => {
                // This is a simplified example - in a real pass, we'd look for
                // patterns like (+ x 0) or (* x 0) in Application nodes
                self.simplifications += 1;
                Some(node.clone())
            }
            _ => None,
        }
    }
}

impl OptimizationPass for AlgebraicSimplificationPass {
    fn name(&self) -> &str {
        "algebraic_simplification"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        let mut new_graph = graph.clone();
        self.simplifications = 0;

        // Look for simplification opportunities
        let mut changes = Vec::new();
        for (id, node) in &new_graph.nodes {
            if let Some(simplified) = self.simplify_node(node, &new_graph) {
                changes.push((*id, simplified));
            }
        }

        // Apply changes
        for (id, new_node) in changes {
            new_graph.nodes.insert(id, new_node);
        }

        println!(
            "  Algebraic simplification: {} simplifications",
            self.simplifications
        );
        Ok(new_graph)
    }

    fn is_applicable(&self, graph: &Graph) -> bool {
        // Check if there are any integer literals or applications
        graph.nodes.values().any(|n| {
            matches!(
                n,
                Node::Literal(Literal::Integer(_)) | Node::Application { .. }
            )
        })
    }
}

fn algebraic_simplification_demo() -> Result<()> {
    println!("2. Algebraic Simplification Demo");
    println!("-------------------------------");

    let program = r#"
        (let ((a (+ x 0))
              (b (* 1 y))
              (c (* z 0))
              (d (- w w))
              (e (+ 0 v)))
          (+ (+ (+ (+ a b) c) d) e))
    "#;

    let graph = parse(program)?;

    let mut pipeline = OptimizationPipelineBuilder::new()
        .add_custom_pass(|| Box::new(AlgebraicSimplificationPass::new()))
        .build();

    pipeline.optimize(&graph)?;
    println!();

    Ok(())
}

/// Function specialization - create specialized versions of functions
struct FunctionSpecializationPass {
    specializations: usize,
    threshold: usize,
}

impl FunctionSpecializationPass {
    fn new(threshold: usize) -> Self {
        Self {
            specializations: 0,
            threshold,
        }
    }
}

impl OptimizationPass for FunctionSpecializationPass {
    fn name(&self) -> &str {
        "function_specialization"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        let mut new_graph = graph.clone();
        self.specializations = 0;

        // Find function applications with constant arguments
        let mut call_sites = Vec::new();
        for (id, node) in &new_graph.nodes {
            if let Node::Application { function, args } = node {
                // Check if any arguments are constants
                let const_args: Vec<bool> = args
                    .iter()
                    .map(|arg_id| matches!(new_graph.nodes.get(arg_id), Some(Node::Literal(_))))
                    .collect();

                if const_args.iter().any(|&is_const| is_const) {
                    call_sites.push((*id, *function, args.clone(), const_args));
                }
            }
        }

        // For demo purposes, just count potential specializations
        self.specializations = call_sites.len();

        println!(
            "  Function specialization: {} potential specializations",
            self.specializations
        );
        Ok(new_graph)
    }

    fn is_applicable(&self, graph: &Graph) -> bool {
        graph
            .nodes
            .values()
            .any(|n| matches!(n, Node::Application { .. }))
    }
}

fn function_specialization_demo() -> Result<()> {
    println!("3. Function Specialization Demo");
    println!("------------------------------");

    let program = r#"
        (let ((add (lambda (x y) (+ x y)))
              (mul (lambda (x y) (* x y))))
          (let ((a (add 5 z))
                (b (add w 10))
                (c (mul 2 v))
                (d (add u v)))
            (+ (+ (+ a b) c) d)))
    "#;

    let graph = parse(program)?;

    let mut pipeline = OptimizationPipelineBuilder::new()
        .add_custom_pass(|| Box::new(FunctionSpecializationPass::new(5)))
        .build();

    pipeline.optimize(&graph)?;
    println!();

    Ok(())
}

fn combined_pipeline_demo() -> Result<()> {
    println!("4. Combined Custom Pipeline Demo");
    println!("-------------------------------");

    // Create a complex program that benefits from multiple passes
    let program = r#"
        (let ((double (lambda (x) (+ x x)))
              (identity (lambda (x) (* x 1)))
              (zero (lambda (x) (* x 0))))
          (let ((msg "Result: ")
                (msg2 "Result: ")
                (a (double 5))
                (b (identity a))
                (c (zero b))
                (d (+ c 0))
                (e (* 1 d)))
            (str-concat msg (number->string e))))
    "#;

    let graph = parse(program)?;

    println!("  Original program: {} nodes", graph.nodes.len());

    // Build pipeline with all custom passes and default ones
    let mut pipeline = OptimizationPipelineBuilder::with_level(OptimizationLevel::Standard)
        .register_pass("string_interning", || Box::new(StringInterningPass::new()))
        .register_pass("algebraic_simplification", || {
            Box::new(AlgebraicSimplificationPass::new())
        })
        .register_pass("function_specialization", || {
            Box::new(FunctionSpecializationPass::new(10))
        })
        .with_default_passes()
        .build();

    let optimized = pipeline.optimize(&graph)?;

    println!("\n  Optimized program: {} nodes", optimized.nodes.len());
    println!(
        "  Reduction: {:.1}%",
        (1.0 - optimized.nodes.len() as f64 / graph.nodes.len() as f64) * 100.0
    );
    println!("  Time: {} µs", pipeline.stats().optimization_time_us);

    Ok(())
}
