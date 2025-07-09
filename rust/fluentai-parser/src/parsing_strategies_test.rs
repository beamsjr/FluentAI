//! Integration tests demonstrating different parsing strategies
//!
//! This module shows when to use each parsing approach:
//! 1. Standard recursive parser - for normal code
//! 2. Recursive parser with depth limit - for untrusted input
//! 3. Iterative parser - for extreme nesting
//! 4. Threaded parser - for custom stack requirements

#[cfg(test)]
mod tests {
    use crate::{
        parse, parse_iterative, parse_with_depth_limit, parse_with_stack_and_depth,
        parse_with_stack_size, ParseError,
    };

    /// Helper to create deeply nested expressions
    fn create_nested_expr(depth: usize) -> String {
        let mut expr = String::new();
        for _ in 0..depth {
            expr.push_str("(+ ");
        }
        expr.push('1');
        for _ in 0..depth {
            expr.push_str(" 2)");
        }
        expr
    }

    #[test]
    fn test_parsing_strategy_comparison() {
        println!("\n=== Parsing Strategy Comparison ===\n");

        // Test different depths
        let test_depths = vec![10, 50, 100, 200];

        for depth in test_depths {
            println!("Testing depth: {}", depth);
            let expr = create_nested_expr(depth);

            // Standard parser
            let standard_result = parse(&expr);
            match standard_result {
                Ok(_) => println!("  ✓ Standard parser: Success"),
                Err(e) => println!("  ✗ Standard parser: {:?}", e),
            }

            // Parser with custom depth limit
            let depth_limited_result = parse_with_depth_limit(&expr, depth * 3);
            match depth_limited_result {
                Ok(_) => println!("  ✓ Depth-limited parser: Success"),
                Err(e) => println!("  ✗ Depth-limited parser: {:?}", e),
            }

            // Iterative parser
            let iterative_result = parse_iterative(&expr);
            match iterative_result {
                Ok(_) => println!("  ✓ Iterative parser: Success"),
                Err(e) => println!("  ✗ Iterative parser: {:?}", e),
            }

            // Threaded parser with large stack
            let threaded_result = parse_with_stack_size(&expr, 16 * 1024 * 1024);
            match threaded_result {
                Ok(_) => println!("  ✓ Threaded parser: Success"),
                Err(e) => println!("  ✗ Threaded parser: {:?}", e),
            }

            println!();
        }
    }

    #[test]
    fn test_embedded_environment_simulation() {
        println!("\n=== Embedded Environment Simulation ===\n");

        // Simulate limited stack environment (256KB)
        let small_stack = 256 * 1024;
        let expr = create_nested_expr(50);

        println!(
            "Simulating embedded environment with {}KB stack",
            small_stack / 1024
        );

        // Standard parser would likely fail in real embedded environment
        println!("Standard parser: Would likely fail with stack overflow");

        // Threaded parser with small stack
        let threaded_result = parse_with_stack_and_depth(&expr, small_stack, 200);
        match threaded_result {
            Ok(_) => println!("✓ Threaded parser with small stack: Success"),
            Err(e) => println!("✗ Threaded parser with small stack: {:?}", e),
        }

        // Iterative parser (stack-independent)
        let iterative_result = parse_iterative(&expr);
        match iterative_result {
            Ok(_) => println!("✓ Iterative parser: Success (stack-independent)"),
            Err(e) => println!("✗ Iterative parser: {:?}", e),
        }
    }

    #[test]
    fn test_untrusted_input_handling() {
        println!("\n=== Untrusted Input Handling ===\n");

        // Simulate malicious input with extreme nesting
        let malicious_depth = 10000;
        let expr = create_nested_expr(malicious_depth);

        println!("Testing malicious input with depth: {}", malicious_depth);

        // Standard parser - would crash
        println!("Standard parser: Would crash with stack overflow");

        // Depth-limited parser - safe rejection
        let safe_limit = 100;
        let depth_result = parse_with_depth_limit(&expr, safe_limit);
        match depth_result {
            Ok(_) => println!("✗ Depth-limited parser: Unexpected success"),
            Err(ParseError::MaxDepthExceeded { depth, max_depth }) => {
                println!(
                    "✓ Depth-limited parser: Safely rejected at depth {} (limit: {})",
                    depth, max_depth
                );
            }
            Err(e) => println!("✗ Depth-limited parser: Wrong error: {:?}", e),
        }

        // Iterative parser with limit - also safe
        let iterative_result = crate::parse_iterative_with_depth(&expr, safe_limit);
        match iterative_result {
            Ok(_) => println!("✗ Iterative parser: Unexpected success"),
            Err(ParseError::MaxDepthExceeded { .. }) => {
                println!("✓ Iterative parser: Safely rejected");
            }
            Err(e) => println!("✗ Iterative parser: Wrong error: {:?}", e),
        }
    }

    #[test]
    fn test_performance_characteristics() {
        use std::time::Instant;

        println!("\n=== Performance Characteristics ===\n");

        let depths = vec![50, 100, 200];

        for depth in depths {
            println!("Depth: {}", depth);
            let expr = create_nested_expr(depth);

            // Measure standard parser
            let start = Instant::now();
            let _ = parse_with_depth_limit(&expr, depth * 5);
            let standard_time = start.elapsed();

            // Measure iterative parser
            let start = Instant::now();
            let _ = parse_iterative(&expr);
            let iterative_time = start.elapsed();

            // Measure threaded parser
            let start = Instant::now();
            let _ = parse_with_stack_and_depth(&expr, 8 * 1024 * 1024, depth * 5);
            let threaded_time = start.elapsed();

            println!("  Standard:  {:?}", standard_time);
            println!("  Iterative: {:?}", iterative_time);
            println!("  Threaded:  {:?}", threaded_time);
            println!();
        }
    }
}
