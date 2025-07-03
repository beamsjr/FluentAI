//! Tests for contract verification

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::collections::HashMap;
    
    use fluentai_core::{
        ast::{Graph, Node, NodeId, Literal},
        value::Value,
    };
    use fluentai_parser::parse;
    
    use crate::{
        contract::{Contract, ContractCondition, ContractKind},
        runtime::{RuntimeVerifier, VerificationContext},
        evaluator::ConditionEvaluator,
    };
    
    fn create_test_graph() -> Graph {
        let mut graph = Graph::new();
        
        // Add some test expressions
        // (>= x 0)
        let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
        let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
        let gte = graph.add_node(Node::Variable { name: ">=".to_string() });
        let cond1 = graph.add_node(Node::Application {
            function: gte,
            args: vec![x_var, zero],
        });
        
        graph.root_id = Some(cond1);
        graph
    }
    
    #[test]
    fn test_evaluate_simple_condition() {
        let graph = create_test_graph();
        let mut evaluator = ConditionEvaluator::new(&graph);
        
        // Set x = 5
        evaluator.bind("x".to_string(), Value::Integer(5));
        
        let result = evaluator.evaluate_condition(graph.root_id.unwrap());
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), true);
        
        // Set x = -1
        evaluator.bind("x".to_string(), Value::Integer(-1));
        let result = evaluator.evaluate_condition(graph.root_id.unwrap());
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), false);
    }
    
    #[test]
    fn test_runtime_verifier_precondition() {
        let mut graph = Graph::new();
        
        // Create condition: (>= x 0)
        let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
        let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
        let gte = graph.add_node(Node::Variable { name: ">=".to_string() });
        let cond = graph.add_node(Node::Application {
            function: gte,
            args: vec![x_var, zero],
        });
        
        // Create contract
        let mut contract = Contract::new("test_func".to_string(), NodeId(999));
        contract.add_precondition(ContractCondition::new(cond, ContractKind::Precondition));
        
        // Create verifier
        let mut verifier = RuntimeVerifier::new();
        verifier.set_ast_graph(Arc::new(graph));
        verifier.register_contract(contract);
        
        // Test with valid input
        let mut ctx = VerificationContext::pre("test_func".to_string(), vec![Value::Integer(5)]);
        ctx.add_binding("x".to_string(), Value::Integer(5));
        
        let result = verifier.verify_preconditions(&ctx);
        assert!(result.is_ok());
        
        // Test with invalid input
        let mut ctx = VerificationContext::pre("test_func".to_string(), vec![Value::Integer(-1)]);
        ctx.add_binding("x".to_string(), Value::Integer(-1));
        
        let result = verifier.verify_preconditions(&ctx);
        assert!(result.is_err());
    }
    
    #[test]
    fn test_postcondition_with_result() {
        let mut graph = Graph::new();
        
        // Create condition: (>= result 0)
        let result_var = graph.add_node(Node::Variable { name: "result".to_string() });
        let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
        let gte = graph.add_node(Node::Variable { name: ">=".to_string() });
        let cond = graph.add_node(Node::Application {
            function: gte,
            args: vec![result_var, zero],
        });
        
        // Create contract
        let mut contract = Contract::new("abs".to_string(), NodeId(999));
        contract.add_postcondition(ContractCondition::new(cond, ContractKind::Postcondition));
        
        // Create verifier
        let mut verifier = RuntimeVerifier::new();
        verifier.set_ast_graph(Arc::new(graph));
        verifier.register_contract(contract);
        
        // Test with valid result
        let ctx = VerificationContext::post(
            "abs".to_string(),
            vec![Value::Integer(-5)],
            Value::Integer(5)
        );
        
        let result = verifier.verify_postconditions(&ctx);
        assert!(result.is_ok());
        
        // Test with invalid result (bug in implementation)
        let ctx = VerificationContext::post(
            "abs".to_string(),
            vec![Value::Integer(-5)],
            Value::Integer(-5) // Wrong!
        );
        
        let result = verifier.verify_postconditions(&ctx);
        assert!(result.is_err());
    }
    
    #[test]
    fn test_type_predicates() {
        let graph = parse("(and (int? x) (string? y))").unwrap();
        let evaluator = ConditionEvaluator::new(&graph);
        
        // Test with correct types
        let eval = evaluator.with_bindings(HashMap::from([
            ("x".to_string(), Value::Integer(42)),
            ("y".to_string(), Value::String("hello".to_string())),
        ]));
        
        let result = eval.evaluate_condition(graph.root_id.unwrap());
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), true);
        
        // Test with wrong types
        let eval2 = ConditionEvaluator::new(&graph).with_bindings(HashMap::from([
            ("x".to_string(), Value::String("not an int".to_string())),
            ("y".to_string(), Value::String("hello".to_string())),
        ]));
        
        let result = eval2.evaluate_condition(graph.root_id.unwrap());
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), false);
    }
    
    #[test]
    fn test_complex_contract_from_parser() {
        // Parse a contract and a condition
        let contract_code = r#"(spec:contract factorial
            :requires [(>= n 0)]
            :ensures [(>= result 1)]
            :complexity "O(n)"
            :pure true)"#;
        
        let graph = parse(contract_code).unwrap();
        
        // Extract the contract node
        let contract_node = graph.nodes.values()
            .find(|n| matches!(n, Node::Contract { .. }))
            .expect("Contract node not found");
        
        if let Node::Contract { preconditions, postconditions, .. } = contract_node {
            assert_eq!(preconditions.len(), 1);
            assert_eq!(postconditions.len(), 1);
            
            // Create evaluator and test precondition
            let mut evaluator = ConditionEvaluator::new(&graph);
            evaluator.bind("n".to_string(), Value::Integer(5));
            
            let result = evaluator.evaluate_condition(preconditions[0]);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), true);
            
            // Test with negative n
            evaluator.bind("n".to_string(), Value::Integer(-1));
            let result = evaluator.evaluate_condition(preconditions[0]);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), false);
        } else {
            panic!("Expected Contract node");
        }
    }
}