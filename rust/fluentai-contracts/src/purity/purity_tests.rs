//! Comprehensive tests for purity analysis

use super::*;
use crate::contract::{Contract, ContractCondition, ContractKind};
use fluentai_core::ast::{EffectType, Graph, Literal, Node, NodeId};

// ===== Test Helper Functions =====

struct TestGraphBuilder {
    graph: Graph,
}

impl TestGraphBuilder {
    fn new() -> Self {
        Self {
            graph: Graph::new(),
        }
    }

    fn add_literal(&mut self, lit: Literal) -> NodeId {
        self.graph
            .add_node(Node::Literal(lit))
            .expect("Failed to add literal")
    }

    fn add_variable(&mut self, name: &str) -> NodeId {
        self.graph
            .add_node(Node::Variable {
                name: name.to_string(),
            })
            .expect("Failed to add variable")
    }

    fn add_application(&mut self, func_name: &str, args: Vec<NodeId>) -> NodeId {
        let func_id = self.add_variable(func_name);
        self.graph
            .add_node(Node::Application {
                function: func_id,
                args,
            })
            .expect("Failed to add application")
    }

    fn add_lambda(&mut self, params: Vec<String>, body: NodeId) -> NodeId {
        self.graph
            .add_node(Node::Lambda { params, body })
            .expect("Failed to add lambda")
    }

    fn add_if(&mut self, condition: NodeId, then_branch: NodeId, else_branch: NodeId) -> NodeId {
        self.graph
            .add_node(Node::If {
                condition,
                then_branch,
                else_branch,
            })
            .expect("Failed to add if")
    }

    fn add_let(&mut self, bindings: Vec<(String, NodeId)>, body: NodeId) -> NodeId {
        self.graph
            .add_node(Node::Let { bindings, body })
            .expect("Failed to add let")
    }

    fn add_list(&mut self, elements: Vec<NodeId>) -> NodeId {
        self.graph
            .add_node(Node::List(elements))
            .expect("Failed to add list")
    }

    // Note: We're simplifying and not using Match nodes since they require Pattern types

    fn add_effect(
        &mut self,
        effect_type: EffectType,
        operation: String,
        args: Vec<NodeId>,
    ) -> NodeId {
        self.graph
            .add_node(Node::Effect {
                effect_type,
                operation,
                args,
            })
            .expect("Failed to add effect")
    }

    fn add_send(&mut self, channel: NodeId, value: NodeId) -> NodeId {
        self.graph
            .add_node(Node::Send { channel, value })
            .expect("Failed to add send")
    }

    fn add_receive(&mut self, channel: NodeId) -> NodeId {
        self.graph
            .add_node(Node::Receive { channel })
            .expect("Failed to add receive")
    }

    fn build(self) -> Graph {
        self.graph
    }
}

// ===== PurityChecker Tests =====

#[test]
fn test_purity_checker_new() {
    let graph = Graph::new();
    let checker = PurityChecker::new(&graph);
    assert!(checker.pure_functions.contains("+"));
    assert!(checker.pure_functions.contains("map"));
    assert!(!checker.impure_functions.contains("+"));
}

#[test]
fn test_register_pure_builtins() {
    let graph = Graph::new();
    let checker = PurityChecker::new(&graph);

    // Check arithmetic operators
    assert!(checker.pure_functions.contains("+"));
    assert!(checker.pure_functions.contains("-"));
    assert!(checker.pure_functions.contains("*"));
    assert!(checker.pure_functions.contains("/"));
    assert!(checker.pure_functions.contains("sqrt"));

    // Check comparison operators
    assert!(checker.pure_functions.contains("="));
    assert!(checker.pure_functions.contains("<"));
    assert!(checker.pure_functions.contains("<="));
    assert!(checker.pure_functions.contains("equal?"));

    // Check logical operators
    assert!(checker.pure_functions.contains("and"));
    assert!(checker.pure_functions.contains("or"));
    assert!(checker.pure_functions.contains("not"));

    // Check list operations
    assert!(checker.pure_functions.contains("cons"));
    assert!(checker.pure_functions.contains("car"));
    assert!(checker.pure_functions.contains("cdr"));
    assert!(checker.pure_functions.contains("map"));
    assert!(checker.pure_functions.contains("filter"));

    // Check type predicates
    assert!(checker.pure_functions.contains("number?"));
    assert!(checker.pure_functions.contains("list?"));
    assert!(checker.pure_functions.contains("string?"));

    // Check contract-specific functions
    assert!(checker.pure_functions.contains("old"));
    assert!(checker.pure_functions.contains("result"));
    assert!(checker.pure_functions.contains("forall"));
}

#[test]
fn test_register_impure_builtins() {
    let graph = Graph::new();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(checker.impure_functions.contains("set!"));
    assert!(checker.impure_functions.contains("print"));
    assert!(checker.impure_functions.contains("random"));
    assert!(checker.impure_functions.contains("vector-set!"));
}

#[test]
fn test_mark_function_pure() {
    let graph = Graph::new();
    let mut checker = PurityChecker::new(&graph);

    // Initially mark as impure
    checker.mark_function_impure("custom_func".to_string());
    assert!(checker.impure_functions.contains("custom_func"));
    assert!(!checker.pure_functions.contains("custom_func"));

    // Then mark as pure
    checker.mark_function_pure("custom_func".to_string());
    assert!(!checker.impure_functions.contains("custom_func"));
    assert!(checker.pure_functions.contains("custom_func"));
}

#[test]
fn test_mark_function_impure() {
    let graph = Graph::new();
    let mut checker = PurityChecker::new(&graph);

    // Initially pure
    checker.mark_function_pure("my_func".to_string());

    // Mark as impure
    checker.mark_function_impure("my_func".to_string());
    assert!(checker.impure_functions.contains("my_func"));
    assert!(!checker.pure_functions.contains("my_func"));
}

// ===== Literal Purity Tests =====

#[test]
fn test_literal_purity() {
    let mut builder = TestGraphBuilder::new();
    let int_lit = builder.add_literal(Literal::Integer(42));
    let float_lit = builder.add_literal(Literal::Float(3.14));
    let string_lit = builder.add_literal(Literal::String("hello".to_string()));
    let bool_lit = builder.add_literal(Literal::Boolean(true));
    let nil_lit = builder.add_literal(Literal::Nil);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(int_lit).unwrap());
    assert!(checker.is_pure(float_lit).unwrap());
    assert!(checker.is_pure(string_lit).unwrap());
    assert!(checker.is_pure(bool_lit).unwrap());
    assert!(checker.is_pure(nil_lit).unwrap());
}

// ===== Variable Purity Tests =====

#[test]
fn test_variable_purity() {
    let mut builder = TestGraphBuilder::new();
    let var1 = builder.add_variable("x");
    let var2 = builder.add_variable("y");
    let var3 = builder.add_variable("complex_name_123");

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(var1).unwrap());
    assert!(checker.is_pure(var2).unwrap());
    assert!(checker.is_pure(var3).unwrap());
}

// ===== Lambda Purity Tests =====

#[test]
fn test_lambda_purity() {
    let mut builder = TestGraphBuilder::new();
    let body = builder.add_literal(Literal::Integer(42));
    let lambda = builder.add_lambda(vec!["x".to_string()], body);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(lambda).unwrap());
}

// ===== Application Purity Tests =====

#[test]
fn test_pure_arithmetic_application() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(10));
    let arg2 = builder.add_literal(Literal::Integer(20));
    let app = builder.add_application("+", vec![arg1, arg2]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(app).unwrap());
}

#[test]
fn test_impure_print_application() {
    let mut builder = TestGraphBuilder::new();
    let arg = builder.add_literal(Literal::String("Hello".to_string()));
    let app = builder.add_application("print", vec![arg]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(!checker.is_pure(app).unwrap());
}

#[test]
fn test_pure_list_operations() {
    let mut builder = TestGraphBuilder::new();
    let elem1 = builder.add_literal(Literal::Integer(1));
    let elem2 = builder.add_literal(Literal::Integer(2));
    let cons_app = builder.add_application("cons", vec![elem1, elem2]);

    let list = builder.add_list(vec![elem1, elem2]);
    let car_app = builder.add_application("car", vec![list]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(cons_app).unwrap());
    assert!(checker.is_pure(car_app).unwrap());
}

#[test]
fn test_impure_mutation() {
    let mut builder = TestGraphBuilder::new();
    let var = builder.add_variable("x");
    let val = builder.add_literal(Literal::Integer(42));
    let set_app = builder.add_application("set!", vec![var, val]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(!checker.is_pure(set_app).unwrap());
}

#[test]
fn test_unknown_function_application() {
    let mut builder = TestGraphBuilder::new();
    let arg = builder.add_literal(Literal::Integer(42));
    let app = builder.add_application("unknown_func", vec![arg]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    // Unknown functions are assumed impure
    assert!(!checker.is_pure(app).unwrap());
}

#[test]
fn test_lambda_definition_purity() {
    let mut builder = TestGraphBuilder::new();

    // Pure lambda: (lambda (x) (+ x 1))
    let x_var = builder.add_variable("x");
    let one = builder.add_literal(Literal::Integer(1));
    let add_app = builder.add_application("+", vec![x_var, one]);
    let pure_lambda = builder.add_lambda(vec!["x".to_string()], add_app);

    // Impure lambda: (lambda (x) (print x))
    let x_var2 = builder.add_variable("x");
    let print_app = builder.add_application("print", vec![x_var2]);
    let impure_lambda = builder.add_lambda(vec!["x".to_string()], print_app);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    // Lambda definitions are always pure - the body is only checked when applied
    assert!(checker.is_pure(pure_lambda).unwrap());
    assert!(checker.is_pure(impure_lambda).unwrap());
}

#[test]
fn test_lambda_application_purity() {
    let mut builder = TestGraphBuilder::new();

    // Application of pure lambda
    let x_var = builder.add_variable("x");
    let one = builder.add_literal(Literal::Integer(1));
    let add_body = builder.add_application("+", vec![x_var, one]);
    let pure_lambda = builder.add_lambda(vec!["x".to_string()], add_body);

    // Note: To test lambda application purity, we would need to create an application
    // where the function position is a lambda. The current implementation only checks
    // the body when it's a direct lambda in function position.

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    // Just test that the lambda definition itself is pure
    assert!(checker.is_pure(pure_lambda).unwrap());
}

#[test]
fn test_application_with_impure_arg() {
    let mut builder = TestGraphBuilder::new();

    // Even if function is pure, impure arguments make it impure
    let impure_arg = builder.add_application("random", vec![]);
    let pure_arg = builder.add_literal(Literal::Integer(10));
    let app = builder.add_application("+", vec![impure_arg, pure_arg]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(!checker.is_pure(app).unwrap());
}

// ===== If Expression Tests =====

#[test]
fn test_if_pure() {
    let mut builder = TestGraphBuilder::new();
    let cond = builder.add_literal(Literal::Boolean(true));
    let then_br = builder.add_literal(Literal::Integer(1));
    let else_br = builder.add_literal(Literal::Integer(2));
    let if_expr = builder.add_if(cond, then_br, else_br);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(if_expr).unwrap());
}

#[test]
fn test_if_impure_condition() {
    let mut builder = TestGraphBuilder::new();
    let cond = builder.add_application("random", vec![]);
    let then_br = builder.add_literal(Literal::Integer(1));
    let else_br = builder.add_literal(Literal::Integer(2));
    let if_expr = builder.add_if(cond, then_br, else_br);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(!checker.is_pure(if_expr).unwrap());
}

#[test]
fn test_if_impure_branch() {
    let mut builder = TestGraphBuilder::new();
    let cond = builder.add_literal(Literal::Boolean(true));
    let then_br = builder.add_literal(Literal::Integer(1));
    let msg = builder.add_literal(Literal::String("else".to_string()));
    let else_br = builder.add_application("print", vec![msg]);
    let if_expr = builder.add_if(cond, then_br, else_br);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(!checker.is_pure(if_expr).unwrap());
}

// ===== Let Expression Tests =====

#[test]
fn test_let_pure() {
    let mut builder = TestGraphBuilder::new();
    let val1 = builder.add_literal(Literal::Integer(10));
    let val2 = builder.add_literal(Literal::Integer(20));
    let x_var = builder.add_variable("x");
    let y_var = builder.add_variable("y");
    let body = builder.add_application("+", vec![x_var, y_var]);

    let let_expr = builder.add_let(vec![("x".to_string(), val1), ("y".to_string(), val2)], body);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(let_expr).unwrap());
}

#[test]
fn test_let_impure_binding() {
    let mut builder = TestGraphBuilder::new();
    let val1 = builder.add_application("random", vec![]);
    let val2 = builder.add_literal(Literal::Integer(20));
    let body = builder.add_literal(Literal::Integer(42));

    let let_expr = builder.add_let(vec![("x".to_string(), val1), ("y".to_string(), val2)], body);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(!checker.is_pure(let_expr).unwrap());
}

#[test]
fn test_let_impure_body() {
    let mut builder = TestGraphBuilder::new();
    let val1 = builder.add_literal(Literal::Integer(10));
    let msg = builder.add_literal(Literal::String("hello".to_string()));
    let body = builder.add_application("print", vec![msg]);

    let let_expr = builder.add_let(vec![("x".to_string(), val1)], body);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(!checker.is_pure(let_expr).unwrap());
}

// ===== List Tests =====

#[test]
fn test_list_pure() {
    let mut builder = TestGraphBuilder::new();
    let elem1 = builder.add_literal(Literal::Integer(1));
    let elem2 = builder.add_literal(Literal::Integer(2));
    let elem3 = builder.add_literal(Literal::Integer(3));
    let list = builder.add_list(vec![elem1, elem2, elem3]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(list).unwrap());
}

#[test]
fn test_list_impure_element() {
    let mut builder = TestGraphBuilder::new();
    let elem1 = builder.add_literal(Literal::Integer(1));
    let elem2 = builder.add_application("random", vec![]);
    let elem3 = builder.add_literal(Literal::Integer(3));
    let list = builder.add_list(vec![elem1, elem2, elem3]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(!checker.is_pure(list).unwrap());
}

// Note: Match expression tests removed as they require Pattern types which are complex to construct in tests

// ===== Effect Node Tests =====

#[test]
fn test_effect_pure() {
    let mut builder = TestGraphBuilder::new();
    let arg = builder.add_literal(Literal::Integer(42));
    let effect = builder.add_effect(EffectType::Pure, "pure_op".to_string(), vec![arg]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(effect).unwrap());
}

#[test]
fn test_effect_io() {
    let mut builder = TestGraphBuilder::new();
    let arg = builder.add_literal(Literal::String("test".to_string()));
    let effect = builder.add_effect(EffectType::IO, "read_file".to_string(), vec![arg]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(!checker.is_pure(effect).unwrap());
}

#[test]
fn test_effect_state() {
    let mut builder = TestGraphBuilder::new();
    let arg = builder.add_literal(Literal::Integer(42));
    let effect = builder.add_effect(EffectType::State, "set_state".to_string(), vec![arg]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(!checker.is_pure(effect).unwrap());
}

// ===== Channel Operation Tests =====

#[test]
fn test_send_impure() {
    let mut builder = TestGraphBuilder::new();
    let channel = builder.add_variable("ch");
    let msg = builder.add_literal(Literal::Integer(42));
    let send = builder.add_send(channel, msg);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(!checker.is_pure(send).unwrap());
}

#[test]
fn test_receive_impure() {
    let mut builder = TestGraphBuilder::new();
    let channel = builder.add_variable("ch");
    let receive = builder.add_receive(channel);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(!checker.is_pure(receive).unwrap());
}

// ===== Contract Node Tests =====

#[test]
fn test_contract_node_pure() {
    let mut graph = Graph::new();
    let contract_node = graph
        .add_node(Node::Contract {
            function_name: "test".to_string(),
            preconditions: vec![],
            postconditions: vec![],
            invariants: vec![],
            complexity: None,
            pure: true,
        })
        .expect("Failed to add contract node");

    let mut checker = PurityChecker::new(&graph);
    assert!(checker.is_pure(contract_node).unwrap());
}

// ===== Cache Tests =====

#[test]
fn test_purity_cache() {
    let mut builder = TestGraphBuilder::new();
    let complex_expr = {
        let a = builder.add_literal(Literal::Integer(1));
        let b = builder.add_literal(Literal::Integer(2));
        let sum = builder.add_application("+", vec![a, b]);
        let c = builder.add_literal(Literal::Integer(3));
        builder.add_application("*", vec![sum, c])
    };

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    // First call should compute and cache
    assert!(checker.is_pure(complex_expr).unwrap());
    assert!(checker.purity_cache.contains_key(&complex_expr));

    // Second call should use cache
    assert!(checker.is_pure(complex_expr).unwrap());
}

// ===== Contract Validation Tests =====

#[test]
fn test_validate_contract_purity_pure() {
    let mut builder = TestGraphBuilder::new();

    // Pure precondition: x > 0
    let x = builder.add_variable("x");
    let zero = builder.add_literal(Literal::Integer(0));
    let pre_expr = builder.add_application(">", vec![x, zero]);

    // Pure postcondition: result > x
    let result = builder.add_variable("result");
    let x2 = builder.add_variable("x");
    let post_expr = builder.add_application(">", vec![result, x2]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    let mut contract = Contract::new("test_func".to_string(), NodeId::new(1).unwrap());
    contract.add_precondition(
        ContractCondition::new(pre_expr, ContractKind::Precondition)
            .with_message("x must be positive".to_string()),
    );
    contract.add_postcondition(
        ContractCondition::new(post_expr, ContractKind::Postcondition)
            .with_message("result must be greater than x".to_string()),
    );

    assert!(checker.validate_contract_purity(&contract).is_ok());
}

#[test]
fn test_validate_contract_purity_impure_precondition() {
    let mut builder = TestGraphBuilder::new();

    // Impure precondition: random() > 0.5
    let rand = builder.add_application("random", vec![]);
    let half = builder.add_literal(Literal::Float(0.5));
    let pre_expr = builder.add_application(">", vec![rand, half]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    let mut contract = Contract::new("test_func".to_string(), NodeId::new(1).unwrap());
    contract.add_precondition(
        ContractCondition::new(pre_expr, ContractKind::Precondition)
            .with_message("random check".to_string()),
    );

    let result = checker.validate_contract_purity(&contract);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Precondition contains impure expression"));
}

#[test]
fn test_validate_contract_purity_impure_postcondition() {
    let mut builder = TestGraphBuilder::new();

    // Pure precondition
    let x = builder.add_variable("x");
    let zero = builder.add_literal(Literal::Integer(0));
    let pre_expr = builder.add_application(">", vec![x, zero]);

    // Impure postcondition: print(result)
    let result = builder.add_variable("result");
    let post_expr = builder.add_application("print", vec![result]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    let mut contract = Contract::new("test_func".to_string(), NodeId::new(1).unwrap());
    contract.add_precondition(ContractCondition::new(pre_expr, ContractKind::Precondition));
    contract.add_postcondition(
        ContractCondition::new(post_expr, ContractKind::Postcondition)
            .with_message("print result".to_string()),
    );

    let result = checker.validate_contract_purity(&contract);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Postcondition contains impure expression"));
}

#[test]
fn test_validate_contract_purity_impure_invariant() {
    let mut builder = TestGraphBuilder::new();

    // Impure invariant: set!(global, 1)
    let global = builder.add_variable("global");
    let one = builder.add_literal(Literal::Integer(1));
    let inv_expr = builder.add_application("set!", vec![global, one]);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    let mut contract = Contract::new("test_func".to_string(), NodeId::new(1).unwrap());
    contract.add_invariant(
        ContractCondition::new(inv_expr, ContractKind::Invariant)
            .with_message("set global".to_string()),
    );

    let result = checker.validate_contract_purity(&contract);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Invariant contains impure expression"));
}

// ===== Complex Expression Tests =====

#[test]
fn test_nested_pure_expression() {
    let mut builder = TestGraphBuilder::new();

    // (if (> (+ x 1) 0) (* y 2) (- z 3))
    let x = builder.add_variable("x");
    let one = builder.add_literal(Literal::Integer(1));
    let x_plus_1 = builder.add_application("+", vec![x, one]);
    let zero = builder.add_literal(Literal::Integer(0));
    let cond = builder.add_application(">", vec![x_plus_1, zero]);

    let y = builder.add_variable("y");
    let two = builder.add_literal(Literal::Integer(2));
    let then_br = builder.add_application("*", vec![y, two]);

    let z = builder.add_variable("z");
    let three = builder.add_literal(Literal::Integer(3));
    let else_br = builder.add_application("-", vec![z, three]);

    let if_expr = builder.add_if(cond, then_br, else_br);

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    assert!(checker.is_pure(if_expr).unwrap());
}

#[test]
fn test_mixed_purity_expression() {
    let mut builder = TestGraphBuilder::new();

    // (let ((x (+ 1 2))
    //       (y (random)))
    //   (if (> x y) x y))
    let one = builder.add_literal(Literal::Integer(1));
    let two = builder.add_literal(Literal::Integer(2));
    let x_val = builder.add_application("+", vec![one, two]);
    let y_val = builder.add_application("random", vec![]);

    let x_var = builder.add_variable("x");
    let y_var = builder.add_variable("y");
    let cond = builder.add_application(">", vec![x_var, y_var]);
    let body = builder.add_if(cond, x_var, y_var);

    let let_expr = builder.add_let(
        vec![("x".to_string(), x_val), ("y".to_string(), y_val)],
        body,
    );

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();

    assert!(!checker.is_pure(let_expr).unwrap());
}

// ===== Error Handling Tests =====

#[test]
fn test_invalid_node_id() {
    let graph = Graph::new();
    let mut checker = PurityChecker::new(&graph);

    let invalid_id = NodeId::new(9999).unwrap();
    let result = checker.is_pure(invalid_id);
    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("Node"));
    assert!(err_msg.contains("not found"));
}

#[test]
fn test_invalid_function_node() {
    let mut builder = TestGraphBuilder::new();

    // Create an application where function node doesn't exist
    let arg = builder.add_literal(Literal::Integer(42));
    let invalid_func_id = NodeId::new(9999).unwrap();
    let app_node = builder
        .graph
        .add_node(Node::Application {
            function: invalid_func_id,
            args: vec![arg],
        })
        .expect("Failed to add application node");

    let graph = builder.build();
    let mut checker = PurityChecker::new(&graph);

    let result = checker.is_pure(app_node);
    assert!(result.is_err());
}
