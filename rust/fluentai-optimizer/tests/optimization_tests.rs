use fluentai_optimizer::*;
use fluentai_parser::parse;
use fluentai_vm::{Compiler, VM, CompilerOptions};
use fluentai_core::ast::NodeId;

#[test]
fn test_constant_folding() {
    let code = "(+ 2 3)";
    let ast = parse(code).unwrap();
    
    let config = OptimizationConfig {
        constant_folding: true,
        dead_code_elimination: false,
        cse: false,
        inline: false,
        inline_threshold: 0,
        tail_call_optimization: false,
        loop_optimization: false,
        beta_reduction: false,
        partial_evaluation: false,
        max_iterations: 1,
        debug_mode: false,
        level: OptimizationLevel::Basic,
    };
    
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();
    
    // The optimized graph should have fewer nodes (constant folded)
    assert!(optimized.nodes.len() < ast.nodes.len());
}

#[test]
fn test_dead_code_elimination() {
    let code = "(let ((x 10) (y 20)) x)"; // y is dead code
    let ast = parse(code).unwrap();
    
    let config = OptimizationConfig {
        constant_folding: false,
        dead_code_elimination: true,
        cse: false,
        inline: false,
        inline_threshold: 0,
        tail_call_optimization: false,
        loop_optimization: false,
        beta_reduction: false,
        partial_evaluation: false,
        max_iterations: 1,
        debug_mode: false,
        level: OptimizationLevel::Basic,
    };
    
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();
    
    // Should have eliminated the dead binding
    assert!(optimized.nodes.len() < ast.nodes.len());
}

#[test]
fn test_inline_optimization() {
    let code = "((lambda (x) (+ x 1)) 5)";
    let ast = parse(code).unwrap();
    
    let config = OptimizationConfig {
        constant_folding: false,
        dead_code_elimination: false,
        cse: false,
        inline: true,
        inline_threshold: 10,
        tail_call_optimization: false,
        loop_optimization: false,
        beta_reduction: false,
        partial_evaluation: false,
        max_iterations: 1,
        debug_mode: false,
        level: OptimizationLevel::Standard,
    };
    
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();
    
    // Function should be inlined
    // Resulting code should be equivalent to (+ 5 1)
    assert!(optimized.nodes.len() < ast.nodes.len());
}

#[test]
fn test_performance_improvement() {
    // Test complex expression with multiple optimization opportunities
    let code = r#"
        (let ((add (lambda (x y) (+ x y)))
              (mul (lambda (x y) (* x y))))
          (+ (add 2 3) (mul 4 5)))
    "#;
    
    let ast = parse(code).unwrap();
    
    // Compile without optimization
    let unopt_compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let unopt_bytecode = unopt_compiler.compile(&ast).unwrap();
    
    // First optimize the AST
    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized_ast = pipeline.optimize(&ast).unwrap();
    
    // Debug: print AST sizes
    println!("Original AST nodes: {}", ast.nodes.len());
    println!("Optimized AST nodes: {}", optimized_ast.nodes.len());
    
    // Debug: print all nodes in optimized AST
    println!("\nOptimized AST nodes:");
    for (id, node) in &optimized_ast.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}", optimized_ast.root_id);
    
    // Debug: check for invalid references
    for (id, node) in &optimized_ast.nodes {
        let check_ref = |ref_id: NodeId, context: &str| {
            if !optimized_ast.nodes.contains_key(&ref_id) {
                println!("ERROR: Node {:?} has invalid {} reference to {:?}", id, context, ref_id);
            }
        };
        
        match node {
            fluentai_core::ast::Node::Application { function, args } => {
                check_ref(*function, "function");
                for arg in args {
                    check_ref(*arg, "arg");
                }
            }
            fluentai_core::ast::Node::Let { bindings, body } => {
                for (name, value_id) in bindings {
                    check_ref(*value_id, &format!("binding '{}'", name));
                }
                check_ref(*body, "body");
            }
            fluentai_core::ast::Node::Lambda { body, .. } => {
                check_ref(*body, "body");
            }
            _ => {}
        }
    }
    
    // Compile with optimization
    let opt_compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::Aggressive,
        debug_info: false,
    });
    let opt_bytecode = opt_compiler.compile(&optimized_ast).unwrap();
    
    // Optimized bytecode should be smaller
    let unopt_size: usize = unopt_bytecode.chunks.iter()
        .map(|chunk| chunk.instructions.len())
        .sum();
    let opt_size: usize = opt_bytecode.chunks.iter()
        .map(|chunk| chunk.instructions.len())
        .sum();
    
    println!("Unoptimized bytecode size: {}", unopt_size);
    println!("Optimized bytecode size: {}", opt_size);
    assert!(opt_size <= unopt_size, "Optimized bytecode should not be larger");
    
    // Both should produce the same result
    let mut unopt_vm = VM::new(unopt_bytecode);
    let unopt_result = unopt_vm.run().unwrap();
    
    let mut opt_vm = VM::new(opt_bytecode);
    let opt_result = opt_vm.run().unwrap();
    
    assert_eq!(format!("{}", unopt_result), format!("{}", opt_result));
}

#[test]
fn test_partial_evaluation() {
    let code = "(if #t (+ 1 2) (- 4 5))";
    let ast = parse(code).unwrap();
    
    let config = OptimizationConfig {
        constant_folding: true,
        dead_code_elimination: false,
        cse: false,
        inline: false,
        inline_threshold: 0,
        tail_call_optimization: false,
        loop_optimization: false,
        beta_reduction: false,
        partial_evaluation: true,
        max_iterations: 1,
        debug_mode: false,
        level: OptimizationLevel::Standard,
    };
    
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();
    
    // Should have evaluated the if with constant condition
    assert!(optimized.nodes.len() < ast.nodes.len());
}

#[test]
fn test_optimization_levels() {
    let code = r#"
        (let ((x 10)
              (y 20)
              (add (lambda (a b) (+ a b))))
          (add x y))
    "#;
    
    let ast = parse(code).unwrap();
    
    // Test each optimization level
    for level in [
        OptimizationLevel::None,
        OptimizationLevel::Basic,
        OptimizationLevel::Standard,
        OptimizationLevel::Aggressive,
    ] {
        let config = OptimizationConfig::for_level(level);
        let mut pipeline = OptimizationPipeline::new(config);
        let optimized = pipeline.optimize(&ast).unwrap();
        
        // Higher levels should optimize more
        match level {
            OptimizationLevel::None => {
                assert_eq!(optimized.nodes.len(), ast.nodes.len());
            }
            _ => {
                assert!(optimized.nodes.len() <= ast.nodes.len());
            }
        }
        
        // All levels should preserve correctness
        let compiler = Compiler::with_options(CompilerOptions {
            optimization_level: level,
            debug_info: false,
        });
        let bytecode = compiler.compile(&ast).unwrap();
        let mut vm = VM::new(bytecode);
        let result = vm.run().unwrap();
        assert_eq!(format!("{}", result), "30");
    }
}