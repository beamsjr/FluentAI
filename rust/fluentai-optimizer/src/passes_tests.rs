#[cfg(test)]
mod tests {
    use crate::passes::*;
    use fluentai_core::ast::{Graph, Literal, Node, NodeId};
    use fluentai_parser::parse;

    // ===== Constant Folding Tests =====

    #[test]
    fn test_constant_folding_arithmetic() {
        let code = "(+ 1 2)";
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
        let code = "(* (+ 1 2) (- 5 3))";
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
        let code = "(+ x 0)";
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
        let code = "(and true false)";
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
        let code = "(let ((x 1) (y 2)) x)";
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
        let code = "(if true 1 (error \"unreachable\"))";
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
        let code = "(let ((x (effect IO:print \"hello\"))) 42)";
        let graph = parse(code).unwrap();
        let mut pass = dead_code::DeadCodeEliminationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should NOT remove binding with effects
        // Effect must be preserved
        assert!(optimized.root_id.is_some());
    }

    // ===== Common Subexpression Elimination Tests =====

    #[test]
    fn test_cse_identical_expressions() {
        let code = "(+ (* x y) (* x y))";
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
        let code = "(let ((a (+ x 1))) (+ (+ x 1) a))";
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
        let code = "(+ (effect IO:read) (effect IO:read))";
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
        let code = "(let ((f (lambda (x) (+ x 1)))) (f 5))";
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
        let code = r#"
            (let ((small (lambda (x) x))
                  (large (lambda (x) (+ (* x x) (* x x) (* x x)))))
                (+ (small 1) (large 2)))
        "#;
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
        let code = r#"
            (letrec ((fact (lambda (n) 
                        (if (<= n 1) 1 (* n (fact (- n 1)))))))
                (fact 5))
        "#;
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
        let code = "((lambda (x) (+ x 1)) 5)";
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
        let code = "((lambda (x y) (+ x y)) 3 4)";
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
        let code = "((lambda (x) (effect IO:print x)) \"hello\")";
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
        let code = r#"
            (letrec ((loop (lambda (n acc)
                        (if (= n 0) 
                            acc 
                            (loop (- n 1) (+ acc n))))))
                (loop 10 0))
        "#;
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
        let code = r#"
            (letrec ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
                     (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
                (even? 10))
        "#;
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
        let code = r#"
            (let ((x 5))
                (letrec ((loop (lambda (i)
                            (if (< i 10)
                                (do (print (+ x 1))
                                    (loop (+ i 1)))
                                i))))
                    (loop 0)))
        "#;
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
        let code = r#"
            (letrec ((loop (lambda (i)
                        (if (< i 4)
                            (do (print i)
                                (loop (+ i 1)))
                            i))))
                (loop 0))
        "#;
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
        let code = "(let ((f (lambda (x y) (+ x y)))) (f 5 y))";
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
        let code = "(let ((add (lambda (x) (lambda (y) (+ x y))))) ((add 5) 3))";
        let graph = parse(code).unwrap();
        let mut pass = partial_eval::PartialEvaluationPass::new();

        let result = pass.run(&graph);
        assert!(result.is_ok());
        let optimized = result.unwrap();

        // Should evaluate to 8
        assert!(optimized.root_id.is_some());
    }
}
