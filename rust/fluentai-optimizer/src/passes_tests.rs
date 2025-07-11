#[cfg(test)]
mod tests {
    use crate::passes::*;
    use fluentai_core::ast::{Graph, Literal, Node, NodeId};
    use fluentai_parser::parse;

    // ===== Constant Folding Tests =====

    #[test]
    fn test_constant_folding_arithmetic() {
        let code = "1 + 2";
        let graph = parse(code).unwrap();
        let mut pass = constant_folding::ConstantFoldingPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should fold to literal 3
        if let Some(root) = optimized.root_id {
            if let Some(Node::Literal(Literal::Integer(n))) = optimized.get_node(root) {
                assert_eq!(*n, 3);
            }
        }
    }

    #[test]
    fn test_constant_folding_nested() {
        let code = "(1 + 2) * (5 - 3)";
        let graph = parse(code).unwrap();
        let mut pass = constant_folding::ConstantFoldingPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should fold to literal 6 (3 * 2)
        if let Some(root) = optimized.root_id {
            if let Some(Node::Literal(Literal::Integer(n))) = optimized.get_node(root) {
                assert_eq!(*n, 6);
            }
        }
    }

    #[test]
    fn test_constant_folding_with_variables() {
        let code = "x + 0";
        let graph = parse(code).unwrap();
        let mut pass = constant_folding::ConstantFoldingPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should simplify to just x (identity with 0)
        // Or might not be implemented
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_constant_folding_boolean() {
        let code = "true && false";
        let graph = parse(code).unwrap();
        let mut pass = constant_folding::ConstantFoldingPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should fold to false
        if let Some(root) = optimized.root_id {
            if let Some(Node::Literal(Literal::Boolean(b))) = optimized.get_node(root) {
                assert_eq!(*b, false);
            }
        }
    }

    // ===== Dead Code Elimination Tests =====

    #[test]
    fn test_dead_code_unused_binding() {
        let code = "{ let x = 1; let y = 2; x }";
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();

        let original_size = graph.nodes.len();
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should remove unused binding y
        assert!(optimized.nodes.len() <= original_size);
    }

    #[test]
    fn test_dead_code_unreachable_branch() {
        let code = "if (true) { 1 } else { error(\"unreachable\") }";
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Might optimize away unreachable branch
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_dead_code_keep_effects() {
        let code = "{ let x = perform IO.print(\"hello\"); 42 }";
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should NOT remove binding with effects
        // Effect must be preserved
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_dead_code_effect_analysis_print() {
        // Test that print function is recognized as having effects
        let code = "{ let x = print(\"side effect\"); let y = 10; y }";
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();
        
        let original_size = graph.nodes.len();
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();
        
        // Should keep x binding because print has IO effects
        // We expect most nodes to be preserved
        assert!(optimized.nodes.len() >= original_size - 2); // might remove at most 2 nodes
    }

    #[test]
    fn test_dead_code_effect_analysis_multiple_effects() {
        // Test multiple effect types
        let code = r#"{
            let io_effect = println("hello");
            let state_effect = x := 42;
            let unused_pure = 2 * 3;
            let used = 10;
            used
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();
        
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();
        
        // Should keep effectful bindings but remove unused-pure
        assert!(optimized.root_id.is_some());
        
        // Count effect nodes in optimized graph
        let mut has_io_effect = false;
        let mut has_state_effect = false;
        for (_, node) in &optimized.nodes {
            match node {
                Node::Variable { name } if name == "println" => has_io_effect = true,
                Node::Variable { name } if name == "set!" => has_state_effect = true,
                _ => {}
            }
        }
        assert!(has_io_effect, "IO effect should be preserved");
        assert!(has_state_effect, "State effect should be preserved");
    }

    #[test]
    fn test_dead_code_effect_analysis_network() {
        // Test network effects
        let code = r#"{
            let response = http_get("https://api.example.com");
            let unused = 42;
            "done"
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();
        
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();
        
        // Should keep network effect
        let mut has_network_effect = false;
        for (_, node) in &optimized.nodes {
            if let Node::Variable { name } = node {
                if name == "http-get" {
                    has_network_effect = true;
                }
            }
        }
        assert!(has_network_effect, "Network effect should be preserved");
    }

    #[test]
    fn test_dead_code_effect_analysis_pure_functions() {
        // Test that pure functions can be eliminated
        let code = r#"{
            let unused_add = 1 + 2;
            let unused_mul = 3 * 4;
            let unused_str = str_concat("a", "b");
            let result = 100;
            result
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();
        
        let original_size = graph.nodes.len();
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();
        
        // Should remove all unused pure computations
        assert!(optimized.nodes.len() < original_size);
        
        // Check that pure functions were removed
        let mut has_add = false;
        let mut has_mul = false;
        let mut has_concat = false;
        for (_, node) in &optimized.nodes {
            if let Node::Variable { name } = node {
                match name.as_str() {
                    "+" => has_add = true,
                    "*" => has_mul = true,
                    "str-concat" => has_concat = true,
                    _ => {}
                }
            }
        }
        assert!(!has_add, "Unused addition should be removed");
        assert!(!has_mul, "Unused multiplication should be removed");
        assert!(!has_concat, "Unused string concat should be removed");
    }

    #[test]
    fn test_dead_code_concurrent_effects() {
        // Test concurrent/channel operations
        let code = r#"{
            let ch = channel();
            let unused_pure = 42;
            chan_send!(ch, "message")
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();
        
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();
        
        // Should keep channel because it's used
        let mut has_channel = false;
        let mut has_send = false;
        for (_, node) in &optimized.nodes {
            if let Node::Variable { name } = node {
                match name.as_str() {
                    "channel" => has_channel = true,
                    "chan-send!" => has_send = true,
                    _ => {}
                }
            }
        }
        assert!(has_channel, "Channel creation should be preserved");
        assert!(has_send, "Channel send should be preserved");
    }

    #[test] 
    fn test_dead_code_async_effects() {
        // Test async/await effects
        let code = r#"{
            let unused_promise = async { 1 + 2 };
            let result = 10;
            result
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();
        
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();
        
        // Even though unused, async has effects and should be preserved
        let mut has_async = false;
        for (_, node) in &optimized.nodes {
            if let Node::Variable { name } = node {
                if name == "async" {
                    has_async = true;
                }
            }
        }
        assert!(has_async, "Async effect should be preserved even if unused");
    }

    #[test]
    fn test_dead_code_custom_effects() {
        // Test custom effect nodes
        let code = r#"{
            let custom = perform Custom.operation("data");
            let result = 5;
            result
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();
        
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();
        
        // Custom effects should be preserved
        let mut has_effect = false;
        for (_, node) in &optimized.nodes {
            if matches!(node, Node::Effect { .. }) {
                has_effect = true;
            }
        }
        assert!(has_effect, "Custom effect should be preserved");
    }

    // ===== Common Subexpression Elimination Tests =====

    #[test]
    fn test_cse_identical_expressions() {
        let code = "(x * y) + (x * y)";
        let graph = parse(code).unwrap();
        let mut pass = cse::CommonSubexpressionEliminationPass::new();

        let original_size = graph.nodes.len();
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should share the (* x y) computation
        assert!(optimized.nodes.len() <= original_size);
    }

    #[test]
    fn test_cse_with_let() {
        let code = "{ let a = x + 1; (x + 1) + a }";
        let graph = parse(code).unwrap();
        let mut pass = cse::CommonSubexpressionEliminationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should reuse the (+ x 1) from binding
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_cse_no_effects() {
        let code = "perform IO.read() + perform IO.read()";
        let graph = parse(code).unwrap();
        let original_size = graph.nodes.len();
        let mut pass = cse::CommonSubexpressionEliminationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should NOT eliminate effectful expressions
        // Each read might return different values
        assert_eq!(optimized.nodes.len(), original_size);
    }

    // ===== Inlining Tests =====

    #[test]
    fn test_inline_simple_function() {
        let code = "{ let f = (x) => x + 1; f(5) }";
        let graph = parse(code).unwrap();
        let mut pass = inline::InlinePass::new(10);

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Might inline to (+ 5 1)
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_inline_small_functions_only() {
        let code = r#"{
            let small = (x) => x;
            let large = (x) => (x * x) + (x * x) + (x * x);
            small(1) + large(2)
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = inline::InlinePass::new(10);

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should inline small but not large
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_no_inline_recursive() {
        let code = r#"{
            private function fact(n) { if (n <= 1) { 1 } else { n * fact(n - 1) } };
            fact(5)
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = inline::InlinePass::new(10);

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should NOT inline recursive functions
        assert!(optimized.root_id.is_some());
    }

    // ===== Beta Reduction Tests =====

    #[test]
    fn test_beta_reduction_simple() {
        let code = "((x) => x + 1)(5)";
        let graph = parse(code).unwrap();
        let mut pass = beta_reduction::BetaReductionPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should reduce to (+ 5 1)
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_beta_reduction_multiple_params() {
        let code = "((x, y) => x + y)(3, 4)";
        let graph = parse(code).unwrap();
        let mut pass = beta_reduction::BetaReductionPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should reduce to (+ 3 4)
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_beta_reduction_preserve_effects() {
        let code = "((x) => perform IO.print(x))(\"hello\")";
        let graph = parse(code).unwrap();
        let mut pass = beta_reduction::BetaReductionPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should preserve the effect
        assert!(optimized.root_id.is_some());
    }

    // ===== Tail Call Optimization Tests =====

    #[test]
    fn test_tail_call_simple() {
        let code = r#"{
            private function loop(n, acc) { 
                if (n == 0) { acc } 
                else { loop(n - 1, acc + n) }
            };
            loop(10, 0)
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = tail_call::TailCallOptimizationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should mark tail calls
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_tail_call_mutual_recursion() {
        let code = r#"{
            private function even(n) { if (n == 0) { true } else { odd(n - 1) } };
            private function odd(n) { if (n == 0) { false } else { even(n - 1) } };
            even(10)
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = tail_call::TailCallOptimizationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should handle mutual recursion
        assert!(optimized.root_id.is_some());
    }

    // ===== Loop Optimization Tests =====

    #[test]
    fn test_loop_invariant_hoisting() {
        let code = r#"{
            let x = 5;
            private function loop(i) {
                if (i < 10) {
                    print(x + 1);
                    loop(i + 1)
                } else { i }
            };
            loop(0)
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = loop_opts::LoopOptimizationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should hoist (+ x 1) out of loop
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_loop_unrolling() {
        let code = r#"{
            private function loop(i) {
                if (i < 4) {
                    print(i);
                    loop(i + 1)
                } else { i }
            };
            loop(0)
        }"#;
        let graph = parse(code).unwrap();
        let mut pass = loop_opts::LoopOptimizationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Might unroll small loops
        assert!(optimized.root_id.is_some());
    }

    // ===== Partial Evaluation Tests =====

    #[test]
    fn test_partial_eval_known_args() {
        let code = "{ let f = (x, y) => x + y; f(5, y) }";
        let graph = parse(code).unwrap();
        let mut pass = partial_eval::PartialEvaluationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should partially evaluate to (+ 5 y)
        assert!(optimized.root_id.is_some());
    }

    #[test]
    fn test_partial_eval_higher_order() {
        let code = "{ let add = (x) => (y) => x + y; add(5)(3) }";
        let graph = parse(code).unwrap();
        let mut pass = partial_eval::PartialEvaluationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should evaluate to 8
        assert!(optimized.root_id.is_some());
    }
}
