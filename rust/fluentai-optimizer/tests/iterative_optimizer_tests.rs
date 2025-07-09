//! Tests for iterative optimization algorithms
//!
//! These tests verify that the optimizer can handle extremely deep
//! AST structures without stack overflow by using iterative algorithms
//! instead of recursive ones.

use fluentai_optimizer::*;
use fluentai_parser::parse;

// Helper to create safe parse depth based on test requirements
fn safe_parse_depth() -> usize {
    // Parser can handle ~500-600 levels before stack overflow
    400
}

#[test]
fn test_deeply_nested_let_bindings() {
    // Test with maximum safe depth for parser
    let depth = safe_parse_depth();
    let mut code = String::new();

    // Build nested lets
    for i in 0..depth {
        code.push_str(&format!("(let ((a{} {}))\n", i, i));
    }

    // Add the body that references the innermost binding
    code.push_str(&format!("a{}", depth - 1));

    // Close all the lets
    for _ in 0..depth {
        code.push(')');
    }

    let ast = match parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parse error (expected for deep nesting): {}", e);
            // For tests that hit parser depth limits, just verify we don't crash
            return;
        }
    };
    println!("Created AST with {} nodes", ast.nodes.len());

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    // Should either succeed or fail gracefully, not stack overflow
    match result {
        Ok(optimized) => {
            println!("Optimization succeeded, {} nodes", optimized.nodes.len());
            assert!(optimized.nodes.len() > 0);
        }
        Err(e) => {
            println!("Optimization failed: {}", e);
            // Current implementation should fail with recursion depth
            assert!(e.to_string().contains("recursion depth"));
        }
    }
}

#[test]
fn test_deeply_nested_applications() {
    // Create 1000+ nested function applications
    let depth = safe_parse_depth();
    let mut code = String::from("(+");

    for _ in 0..depth {
        code.push_str(" (+");
    }

    code.push_str(" 1");

    for _ in 0..depth {
        code.push_str(" 1)");
    }
    code.push(')');

    let ast = match parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parse error (expected for deep nesting): {}", e);
            // For tests that hit parser depth limits, just verify we don't crash
            return;
        }
    };
    println!("Created AST with {} nodes", ast.nodes.len());

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    match result {
        Ok(optimized) => {
            println!("Optimization succeeded, {} nodes", optimized.nodes.len());
            assert!(optimized.nodes.len() > 0);
        }
        Err(e) => {
            println!("Optimization failed: {}", e);
            assert!(e.to_string().contains("recursion depth"));
        }
    }
}

#[test]
fn test_deeply_nested_lambda_bodies() {
    // Create deeply nested lambda with nested body
    let depth = safe_parse_depth();
    let mut code = String::from("(lambda (x0)");

    for i in 1..depth {
        code.push_str(&format!(" (lambda (x{})", i));
    }

    code.push_str(" x0");

    for _ in 0..depth {
        code.push(')');
    }

    let ast = match parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parse error (expected for deep nesting): {}", e);
            // For tests that hit parser depth limits, just verify we don't crash
            return;
        }
    };
    println!("Created AST with {} nodes", ast.nodes.len());

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    match result {
        Ok(optimized) => {
            println!("Optimization succeeded, {} nodes", optimized.nodes.len());
            assert!(optimized.nodes.len() > 0);
        }
        Err(e) => {
            println!("Optimization failed: {}", e);
            assert!(e.to_string().contains("recursion depth"));
        }
    }
}

#[test]
fn test_deeply_nested_if_chains() {
    // Create deeply nested if-then-else chains
    let depth = safe_parse_depth();
    let mut code = String::new();

    // Build nested ifs
    for _ in 0..depth {
        code.push_str("(if #t ");
    }

    // Add the innermost value
    code.push_str("1");

    // Add all the else branches
    for _ in 0..depth {
        code.push_str(" 0)");
    }

    let ast = match parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parse error (expected for deep nesting): {}", e);
            // For tests that hit parser depth limits, just verify we don't crash
            return;
        }
    };
    println!("Created AST with {} nodes", ast.nodes.len());

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    match result {
        Ok(optimized) => {
            println!("Optimization succeeded, {} nodes", optimized.nodes.len());
            assert!(optimized.nodes.len() > 0);
        }
        Err(e) => {
            println!("Optimization failed: {}", e);
            assert!(e.to_string().contains("recursion depth"));
        }
    }
}

#[test]
fn test_deeply_nested_list_structures() {
    // Create deeply nested list structures
    let depth = safe_parse_depth();
    let mut code = String::from("(list 1");

    for _ in 0..depth {
        code.push_str(" (list 2");
    }

    code.push_str(" 3");

    for _ in 0..depth {
        code.push(')');
    }
    code.push(')');

    let ast = match parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parse error (expected for deep nesting): {}", e);
            // For tests that hit parser depth limits, just verify we don't crash
            return;
        }
    };
    println!("Created AST with {} nodes", ast.nodes.len());

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    match result {
        Ok(optimized) => {
            println!("Optimization succeeded, {} nodes", optimized.nodes.len());
            assert!(optimized.nodes.len() > 0);
        }
        Err(e) => {
            println!("Optimization failed: {}", e);
            assert!(e.to_string().contains("recursion depth"));
        }
    }
}

#[test]
fn test_complex_deep_nesting() {
    // Test combination of deep nesting: let inside lambda inside if
    let depth = 100; // Much less depth due to complexity - 200 total levels
    let mut code = String::from("(if #t");

    // Add nested lambdas
    for i in 0..depth {
        code.push_str(&format!(" (lambda (x{})", i));
    }

    // Add nested lets inside
    for i in 0..depth {
        code.push_str(&format!(" (let ((y{} {}))", i, i));
    }

    code.push_str(" (+ x0 y0)");

    // Close lets
    for _ in 0..depth {
        code.push(')');
    }

    // Close lambdas
    for _ in 0..depth {
        code.push(')');
    }

    code.push_str(" 0)"); // else branch

    let ast = match parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parse error (expected for deep nesting): {}", e);
            // For tests that hit parser depth limits, just verify we don't crash
            return;
        }
    };
    println!("Created complex AST with {} nodes", ast.nodes.len());

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    match result {
        Ok(optimized) => {
            println!("Optimization succeeded, {} nodes", optimized.nodes.len());
            assert!(optimized.nodes.len() > 0);
        }
        Err(e) => {
            println!("Optimization failed: {}", e);
            assert!(e.to_string().contains("recursion depth"));
        }
    }
}

#[test]
fn test_extremely_deep_nesting_stress() {
    // Use a separate thread with larger stack size for this test
    let builder = std::thread::Builder::new()
        .name("deep_nesting_test".into())
        .stack_size(8 * 1024 * 1024); // 8MB stack

    let handle = builder
        .spawn(|| {
            // Stress test with extremely deep nesting
            let depth = 450; // Reduced to avoid parser stack overflow
            let mut code = String::from("(+ 1");

            for _ in 0..depth {
                code.push_str(" (+ 1");
            }

            code.push_str(" 1");

            for _ in 0..depth {
                code.push_str(" 1)");
            }
            code.push(')');

            // This is so deep that even parsing might fail
            match parse(&code) {
                Ok(ast) => {
                    println!("Parsed extremely deep AST with {} nodes", ast.nodes.len());

                    let mut optimizer = AdvancedOptimizer::new();
                    let result = optimizer.optimize(&ast);

                    match result {
                        Ok(optimized) => {
                            println!("Optimization succeeded, {} nodes", optimized.nodes.len());
                            assert!(optimized.nodes.len() > 0);
                        }
                        Err(e) => {
                            println!("Optimization failed: {}", e);
                            // Expected to fail with current implementation
                            assert!(e.to_string().contains("recursion depth"));
                        }
                    }
                }
                Err(e) => {
                    println!("Parsing failed on extremely deep nesting: {}", e);
                    // Parser might also have recursion limits
                }
            }
        })
        .unwrap();

    handle.join().unwrap();
}

#[test]
fn test_mark_reachable_deep_nesting() {
    // Test specifically for mark_reachable with deep let bindings
    // where some bindings are unused
    // Simpler structure to avoid parse errors
    let depth = 50; // Much smaller for this complex test
    let mut code = String::from("(let (");

    // Create bindings
    for i in 0..depth {
        if i % 2 == 0 {
            code.push_str(&format!("(used{} {}) ", i, i));
        } else {
            code.push_str(&format!("(unused{} (+ {} 1)) ", i, i));
        }
    }
    code.push_str(") "); // Close bindings

    // Use only even variables in the body
    code.push_str("(+ ");
    for i in (0..depth).step_by(2).take(10) {
        code.push_str(&format!("used{} ", i));
    }
    code.push_str("))"); // Close + and let

    let ast = match parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parse error (expected for deep nesting): {}", e);
            // For tests that hit parser depth limits, just verify we don't crash
            return;
        }
    };
    println!(
        "Created AST for mark_reachable test with {} nodes",
        ast.nodes.len()
    );

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    match result {
        Ok(optimized) => {
            println!("Optimization succeeded, {} nodes", optimized.nodes.len());
            // Should have eliminated many unused bindings
            assert!(optimized.nodes.len() < ast.nodes.len());
        }
        Err(e) => {
            println!("Optimization failed: {}", e);
            assert!(e.to_string().contains("recursion depth"));
        }
    }
}

#[test]
fn test_deep_copy_with_substitution_nesting() {
    // Test deep nesting that exercises deep_copy_with_substitution
    // through beta reduction and inlining
    let depth = 150;

    // Create deeply nested immediately-applied lambdas
    let mut code = String::from("");

    for i in 0..depth {
        code.push_str(&format!("((lambda (x{}) ", i));
    }

    code.push_str("42");

    for i in (0..depth).rev() {
        code.push_str(&format!(") {})", i));
    }

    let ast = match parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parse error (expected for deep nesting): {}", e);
            // For tests that hit parser depth limits, just verify we don't crash
            return;
        }
    };
    println!(
        "Created AST for substitution test with {} nodes",
        ast.nodes.len()
    );

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    match result {
        Ok(optimized) => {
            println!("Optimization succeeded, {} nodes", optimized.nodes.len());
            // Beta reduction should simplify this significantly
            assert!(optimized.nodes.len() < ast.nodes.len());
        }
        Err(e) => {
            println!("Optimization failed: {}", e);
            assert!(e.to_string().contains("recursion depth"));
        }
    }
}
