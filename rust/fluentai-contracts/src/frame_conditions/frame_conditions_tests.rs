//! Comprehensive tests for frame conditions

use super::*;
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use crate::contract::{Contract, ContractCondition, ContractKind};
use rustc_hash::FxHashSet;

// ===== Test Helper Functions =====

fn create_test_graph() -> Graph {
    Graph::new()
}

fn create_test_contract(name: &str) -> Contract {
    Contract::new(name.to_string(), NodeId::new(1).unwrap())
}

// ===== FrameCondition Tests =====

#[test]
fn test_frame_condition_new() {
    let frame = FrameCondition::new();
    assert!(frame.modifies.is_empty());
    assert!(frame.modifies_fields.is_empty());
    assert!(frame.modifies_indices.is_empty());
    assert!(frame.modifies_regions.is_empty());
    assert!(!frame.may_allocate);
    assert!(!frame.may_deallocate);
}

#[test]
fn test_frame_condition_pure() {
    let frame = FrameCondition::pure();
    assert!(frame.is_pure());
    assert!(frame.modifies.is_empty());
    assert!(frame.modifies_fields.is_empty());
    assert!(frame.modifies_indices.is_empty());
    assert!(frame.modifies_regions.is_empty());
    assert!(!frame.may_allocate);
    assert!(!frame.may_deallocate);
}

#[test]
fn test_frame_condition_is_pure() {
    let mut frame = FrameCondition::new();
    assert!(frame.is_pure());
    
    frame.add_modifies_var("x".to_string());
    assert!(!frame.is_pure());
    
    let mut frame2 = FrameCondition::new();
    frame2.may_allocate = true;
    assert!(!frame2.is_pure());
    
    let mut frame3 = FrameCondition::new();
    frame3.add_modifies_field("obj".to_string(), "field".to_string());
    assert!(!frame3.is_pure());
}

#[test]
fn test_add_modifies_var() {
    let mut frame = FrameCondition::new();
    
    frame.add_modifies_var("x".to_string());
    frame.add_modifies_var("y".to_string());
    frame.add_modifies_var("x".to_string()); // Duplicate
    
    assert_eq!(frame.modifies.len(), 2);
    assert!(frame.modifies.contains("x"));
    assert!(frame.modifies.contains("y"));
}

#[test]
fn test_add_modifies_field() {
    let mut frame = FrameCondition::new();
    
    frame.add_modifies_field("obj1".to_string(), "field1".to_string());
    frame.add_modifies_field("obj2".to_string(), "field2".to_string());
    frame.add_modifies_field("obj1".to_string(), "field1".to_string()); // Duplicate
    
    assert_eq!(frame.modifies_fields.len(), 2);
    assert!(frame.modifies_fields.contains(&FieldAccess {
        object: "obj1".to_string(),
        field: "field1".to_string(),
    }));
}

#[test]
fn test_add_modifies_index() {
    let mut frame = FrameCondition::new();
    
    frame.add_modifies_index("arr1".to_string(), IndexExpr::Constant(0));
    frame.add_modifies_index("arr1".to_string(), IndexExpr::Variable("i".to_string()));
    frame.add_modifies_index("arr2".to_string(), IndexExpr::All);
    
    assert_eq!(frame.modifies_indices.len(), 3);
}

#[test]
fn test_add_modifies_region() {
    let mut frame = FrameCondition::new();
    
    frame.add_modifies_region(HeapRegion::Global);
    frame.add_modifies_region(HeapRegion::Named("region1".to_string()));
    frame.add_modifies_region(HeapRegion::ReachableFrom("ptr".to_string()));
    frame.add_modifies_region(HeapRegion::Fresh);
    
    assert_eq!(frame.modifies_regions.len(), 4);
    assert!(frame.modifies_regions.contains(&HeapRegion::Global));
}

// ===== FieldAccess Tests =====

#[test]
fn test_field_access_equality() {
    let access1 = FieldAccess {
        object: "obj".to_string(),
        field: "field".to_string(),
    };
    
    let access2 = FieldAccess {
        object: "obj".to_string(),
        field: "field".to_string(),
    };
    
    let access3 = FieldAccess {
        object: "other".to_string(),
        field: "field".to_string(),
    };
    
    assert_eq!(access1, access2);
    assert_ne!(access1, access3);
}

#[test]
fn test_field_access_hash() {
    let mut set = FxHashSet::default();
    
    let access1 = FieldAccess {
        object: "obj".to_string(),
        field: "field".to_string(),
    };
    
    let access2 = FieldAccess {
        object: "obj".to_string(),
        field: "field".to_string(),
    };
    
    set.insert(access1);
    set.insert(access2); // Should not increase size
    
    assert_eq!(set.len(), 1);
}

// ===== IndexAccess Tests =====

#[test]
fn test_index_access_constant() {
    let access = IndexAccess {
        array: "arr".to_string(),
        index: IndexExpr::Constant(42),
    };
    
    match access.index {
        IndexExpr::Constant(n) => assert_eq!(n, 42),
        _ => panic!("Wrong index type"),
    }
}

#[test]
fn test_index_access_variable() {
    let access = IndexAccess {
        array: "arr".to_string(),
        index: IndexExpr::Variable("i".to_string()),
    };
    
    match access.index {
        IndexExpr::Variable(ref v) => assert_eq!(v, "i"),
        _ => panic!("Wrong index type"),
    }
}

#[test]
fn test_index_access_range() {
    let access = IndexAccess {
        array: "arr".to_string(),
        index: IndexExpr::Range(
            Box::new(IndexExpr::Constant(0)),
            Box::new(IndexExpr::Constant(10))
        ),
    };
    
    match access.index {
        IndexExpr::Range(ref start, ref end) => {
            assert_eq!(**start, IndexExpr::Constant(0));
            assert_eq!(**end, IndexExpr::Constant(10));
        }
        _ => panic!("Wrong index type"),
    }
}

#[test]
fn test_index_access_all() {
    let access = IndexAccess {
        array: "arr".to_string(),
        index: IndexExpr::All,
    };
    
    assert_eq!(access.index, IndexExpr::All);
}

// ===== HeapRegion Tests =====

#[test]
fn test_heap_region_variants() {
    let region1 = HeapRegion::Named("heap1".to_string());
    let region2 = HeapRegion::ReachableFrom("ptr".to_string());
    let region3 = HeapRegion::Fresh;
    let region4 = HeapRegion::Global;
    
    // Test equality
    assert_eq!(HeapRegion::Fresh, HeapRegion::Fresh);
    assert_eq!(HeapRegion::Global, HeapRegion::Global);
    assert_ne!(region1, region2);
    assert_ne!(region3, region4);
}

#[test]
fn test_heap_region_hash() {
    let mut set = FxHashSet::default();
    
    set.insert(HeapRegion::Global);
    set.insert(HeapRegion::Global); // Duplicate
    set.insert(HeapRegion::Fresh);
    set.insert(HeapRegion::Named("r1".to_string()));
    set.insert(HeapRegion::Named("r2".to_string()));
    
    assert_eq!(set.len(), 4);
}

// ===== FrameConditionBuilder Tests =====

#[test]
fn test_frame_condition_builder_empty() {
    let frame = FrameConditionBuilder::new().build();
    assert!(frame.is_pure());
}

#[test]
fn test_frame_condition_builder_vars() {
    let frame = FrameConditionBuilder::new()
        .modifies_vars(vec!["x".to_string(), "y".to_string(), "z".to_string()])
        .build();
    
    assert_eq!(frame.modifies.len(), 3);
    assert!(frame.modifies.contains("x"));
    assert!(frame.modifies.contains("y"));
    assert!(frame.modifies.contains("z"));
}

#[test]
fn test_frame_condition_builder_fields() {
    let frame = FrameConditionBuilder::new()
        .modifies_fields(vec![
            ("obj1".to_string(), "field1".to_string()),
            ("obj2".to_string(), "field2".to_string()),
        ])
        .build();
    
    assert_eq!(frame.modifies_fields.len(), 2);
}

#[test]
fn test_frame_condition_builder_allocation() {
    let frame = FrameConditionBuilder::new()
        .allows_allocation()
        .build();
    
    assert!(frame.may_allocate);
    assert!(!frame.may_deallocate);
    assert!(!frame.is_pure());
}

#[test]
fn test_frame_condition_builder_complex() {
    let frame = FrameConditionBuilder::new()
        .modifies_vars(vec!["x".to_string()])
        .modifies_fields(vec![("obj".to_string(), "f".to_string())])
        .allows_allocation()
        .build();
    
    assert!(!frame.is_pure());
    assert_eq!(frame.modifies.len(), 1);
    assert_eq!(frame.modifies_fields.len(), 1);
    assert!(frame.may_allocate);
}

// ===== FrameConditionManager Tests =====

#[test]
fn test_frame_condition_manager_new() {
    let graph = create_test_graph();
    let manager = FrameConditionManager::new(&graph);
    // Just test creation
}

#[test]
fn test_add_frame_condition() {
    let graph = create_test_graph();
    let mut manager = FrameConditionManager::new(&graph);
    
    let frame1 = FrameCondition::pure();
    let mut frame2 = FrameCondition::new();
    frame2.add_modifies_var("x".to_string());
    
    manager.add_frame_condition("func1".to_string(), frame1);
    manager.add_frame_condition("func2".to_string(), frame2);
    
    assert!(manager.get_frame_condition("func1").is_some());
    assert!(manager.get_frame_condition("func2").is_some());
    assert!(manager.get_frame_condition("func3").is_none());
}

#[test]
fn test_get_frame_condition() {
    let graph = create_test_graph();
    let mut manager = FrameConditionManager::new(&graph);
    
    let mut frame = FrameCondition::new();
    frame.add_modifies_var("test".to_string());
    manager.add_frame_condition("test_func".to_string(), frame);
    
    let retrieved = manager.get_frame_condition("test_func");
    assert!(retrieved.is_some());
    assert!(retrieved.unwrap().modifies.contains("test"));
}

#[test]
fn test_check_modification_allowed_no_frame() {
    let graph = create_test_graph();
    let manager = FrameConditionManager::new(&graph);
    
    // No frame condition means all modifications allowed
    let result = manager.check_modification_allowed(
        "unknown_func",
        &Modification::Variable("x".to_string())
    );
    assert!(result.is_ok());
    assert!(result.unwrap());
}

#[test]
fn test_check_modification_allowed_variable() {
    let graph = create_test_graph();
    let mut manager = FrameConditionManager::new(&graph);
    
    let mut frame = FrameCondition::new();
    frame.add_modifies_var("x".to_string());
    frame.add_modifies_var("y".to_string());
    manager.add_frame_condition("func".to_string(), frame);
    
    // Allowed modification
    let result = manager.check_modification_allowed(
        "func",
        &Modification::Variable("x".to_string())
    );
    assert!(result.is_ok());
    assert!(result.unwrap());
    
    // Disallowed modification
    let result = manager.check_modification_allowed(
        "func",
        &Modification::Variable("z".to_string())
    );
    assert!(result.is_ok());
    assert!(!result.unwrap());
}

#[test]
fn test_check_modification_allowed_field() {
    let graph = create_test_graph();
    let mut manager = FrameConditionManager::new(&graph);
    
    let mut frame = FrameCondition::new();
    frame.add_modifies_field("obj".to_string(), "field1".to_string());
    manager.add_frame_condition("func".to_string(), frame);
    
    let allowed = FieldAccess {
        object: "obj".to_string(),
        field: "field1".to_string(),
    };
    
    let disallowed = FieldAccess {
        object: "obj".to_string(),
        field: "field2".to_string(),
    };
    
    assert!(manager.check_modification_allowed("func", &Modification::Field(allowed)).unwrap());
    assert!(!manager.check_modification_allowed("func", &Modification::Field(disallowed)).unwrap());
}

#[test]
fn test_check_modification_allowed_allocation() {
    let graph = create_test_graph();
    let mut manager = FrameConditionManager::new(&graph);
    
    let mut frame = FrameCondition::new();
    frame.may_allocate = true;
    frame.may_deallocate = false;
    manager.add_frame_condition("func".to_string(), frame);
    
    assert!(manager.check_modification_allowed("func", &Modification::Allocation).unwrap());
    assert!(!manager.check_modification_allowed("func", &Modification::Deallocation).unwrap());
}

#[test]
fn test_extract_from_contract_empty() {
    let mut graph = create_test_graph();
    
    // Create a simple function body
    let body = graph.add_node(Node::Literal(Literal::Integer(42)));
    
    let mut contract = create_test_contract("pure_func");
    contract.node_id = body;
    
    let mut manager = FrameConditionManager::new(&graph);
    let result = manager.extract_from_contract(&contract);
    
    assert!(result.is_ok());
    let frame = result.unwrap();
    assert!(frame.is_pure());
}

#[test]
fn test_extract_index_constant() {
    let mut graph = create_test_graph();
    let node = graph.add_node(Node::Literal(Literal::Integer(5)));
    
    let manager = FrameConditionManager::new(&graph);
    let index = manager.extract_index(node);
    
    assert_eq!(index, IndexExpr::Constant(5));
}

#[test]
fn test_extract_index_variable() {
    let mut graph = create_test_graph();
    let node = graph.add_node(Node::Variable { name: "i".to_string() });
    
    let manager = FrameConditionManager::new(&graph);
    let index = manager.extract_index(node);
    
    assert_eq!(index, IndexExpr::Variable("i".to_string()));
}

#[test]
fn test_extract_index_default() {
    let mut graph = create_test_graph();
    let node = graph.add_node(Node::Literal(Literal::Float(3.14)));
    
    let manager = FrameConditionManager::new(&graph);
    let index = manager.extract_index(node);
    
    assert_eq!(index, IndexExpr::All);
}

#[test]
fn test_extract_index_invalid_node() {
    let graph = create_test_graph();
    let manager = FrameConditionManager::new(&graph);
    
    let invalid_id = NodeId::new(9999).unwrap();
    let index = manager.extract_index(invalid_id);
    
    assert_eq!(index, IndexExpr::All);
}

// ===== Modification Tests =====

#[test]
fn test_modification_variants() {
    let mod1 = Modification::Variable("x".to_string());
    let mod2 = Modification::Field(FieldAccess {
        object: "obj".to_string(),
        field: "f".to_string(),
    });
    let mod3 = Modification::Index(IndexAccess {
        array: "arr".to_string(),
        index: IndexExpr::Constant(0),
    });
    let mod4 = Modification::Allocation;
    let mod5 = Modification::Deallocation;
    
    // Just test that they can be created and matched
    match mod1 {
        Modification::Variable(ref v) => assert_eq!(v, "x"),
        _ => panic!("Wrong variant"),
    }
    
    match mod4 {
        Modification::Allocation => {},
        _ => panic!("Wrong variant"),
    }
}

// ===== Integration Tests =====

#[test]
fn test_analyze_modifications_set() {
    let mut graph = create_test_graph();
    
    // Create AST for: (set! x 42)
    let set_func = graph.add_node(Node::Variable { name: "set!".to_string() });
    let var_x = graph.add_node(Node::Variable { name: "x".to_string() });
    let val = graph.add_node(Node::Literal(Literal::Integer(42)));
    let set_app = graph.add_node(Node::Application {
        function: set_func,
        args: vec![var_x, val],
    });
    
    let mut contract = create_test_contract("mutating_func");
    contract.node_id = set_app;
    
    let mut manager = FrameConditionManager::new(&graph);
    let result = manager.extract_from_contract(&contract);
    
    assert!(result.is_ok());
    let frame = result.unwrap();
    assert!(frame.modifies.contains("x"));
    assert!(!frame.is_pure());
}

#[test]
fn test_analyze_modifications_array_set() {
    let mut graph = create_test_graph();
    
    // Create AST for: (vector-set! arr 0 value)
    let set_func = graph.add_node(Node::Variable { name: "vector-set!".to_string() });
    let arr = graph.add_node(Node::Variable { name: "arr".to_string() });
    let idx = graph.add_node(Node::Literal(Literal::Integer(0)));
    let val = graph.add_node(Node::Literal(Literal::Integer(99)));
    let set_app = graph.add_node(Node::Application {
        function: set_func,
        args: vec![arr, idx, val],
    });
    
    let mut contract = create_test_contract("array_mutate");
    contract.node_id = set_app;
    
    let mut manager = FrameConditionManager::new(&graph);
    let result = manager.extract_from_contract(&contract);
    
    assert!(result.is_ok());
    let frame = result.unwrap();
    assert!(!frame.modifies_indices.is_empty());
}

#[test]
fn test_analyze_modifications_allocation() {
    let mut graph = create_test_graph();
    
    // Create AST for: (cons 1 2)
    let cons_func = graph.add_node(Node::Variable { name: "cons".to_string() });
    let arg1 = graph.add_node(Node::Literal(Literal::Integer(1)));
    let arg2 = graph.add_node(Node::Literal(Literal::Integer(2)));
    let cons_app = graph.add_node(Node::Application {
        function: cons_func,
        args: vec![arg1, arg2],
    });
    
    let mut contract = create_test_contract("allocating_func");
    contract.node_id = cons_app;
    
    let mut manager = FrameConditionManager::new(&graph);
    let result = manager.extract_from_contract(&contract);
    
    assert!(result.is_ok());
    let frame = result.unwrap();
    assert!(frame.may_allocate);
    assert!(!frame.is_pure());
}

#[test]
fn test_analyze_modifications_let() {
    let mut graph = create_test_graph();
    
    // Create AST for: (let ((x 1)) (set! x 2))
    let set_func = graph.add_node(Node::Variable { name: "set!".to_string() });
    let var_x = graph.add_node(Node::Variable { name: "x".to_string() });
    let val1 = graph.add_node(Node::Literal(Literal::Integer(1)));
    let val2 = graph.add_node(Node::Literal(Literal::Integer(2)));
    let set_app = graph.add_node(Node::Application {
        function: set_func,
        args: vec![var_x, val2],
    });
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), val1)],
        body: set_app,
    });
    
    let mut contract = create_test_contract("let_func");
    contract.node_id = let_node;
    
    let mut manager = FrameConditionManager::new(&graph);
    let result = manager.extract_from_contract(&contract);
    
    assert!(result.is_ok());
    let frame = result.unwrap();
    assert!(frame.modifies.contains("x"));
}

#[test]
fn test_analyze_modifications_if() {
    let mut graph = create_test_graph();
    
    // Create AST for: (if cond (set! x 1) (set! y 2))
    let cond = graph.add_node(Node::Literal(Literal::Boolean(true)));
    
    let set_func1 = graph.add_node(Node::Variable { name: "set!".to_string() });
    let var_x = graph.add_node(Node::Variable { name: "x".to_string() });
    let val1 = graph.add_node(Node::Literal(Literal::Integer(1)));
    let then_branch = graph.add_node(Node::Application {
        function: set_func1,
        args: vec![var_x, val1],
    });
    
    let set_func2 = graph.add_node(Node::Variable { name: "set!".to_string() });
    let var_y = graph.add_node(Node::Variable { name: "y".to_string() });
    let val2 = graph.add_node(Node::Literal(Literal::Integer(2)));
    let else_branch = graph.add_node(Node::Application {
        function: set_func2,
        args: vec![var_y, val2],
    });
    
    let if_node = graph.add_node(Node::If {
        condition: cond,
        then_branch,
        else_branch,
    });
    
    let mut contract = create_test_contract("if_func");
    contract.node_id = if_node;
    
    let mut manager = FrameConditionManager::new(&graph);
    let result = manager.extract_from_contract(&contract);
    
    assert!(result.is_ok());
    let frame = result.unwrap();
    // Both branches modify variables
    assert!(frame.modifies.contains("x"));
    assert!(frame.modifies.contains("y"));
}

// ===== Serialization Tests =====

#[test]
fn test_frame_condition_serialization() {
    let mut frame = FrameCondition::new();
    frame.add_modifies_var("x".to_string());
    frame.add_modifies_field("obj".to_string(), "field".to_string());
    frame.add_modifies_index("arr".to_string(), IndexExpr::Constant(0));
    frame.add_modifies_region(HeapRegion::Global);
    frame.may_allocate = true;
    
    // Serialize
    let json = serde_json::to_string(&frame).unwrap();
    
    // Deserialize
    let deserialized: FrameCondition = serde_json::from_str(&json).unwrap();
    
    assert!(deserialized.modifies.contains("x"));
    assert_eq!(deserialized.modifies_fields.len(), 1);
    assert_eq!(deserialized.modifies_indices.len(), 1);
    assert_eq!(deserialized.modifies_regions.len(), 1);
    assert!(deserialized.may_allocate);
}

#[test]
fn test_field_access_serialization() {
    let access = FieldAccess {
        object: "myObj".to_string(),
        field: "myField".to_string(),
    };
    
    let json = serde_json::to_string(&access).unwrap();
    let deserialized: FieldAccess = serde_json::from_str(&json).unwrap();
    
    assert_eq!(access, deserialized);
}

#[test]
fn test_index_expr_serialization() {
    let exprs = vec![
        IndexExpr::Constant(42),
        IndexExpr::Variable("i".to_string()),
        IndexExpr::Range(
            Box::new(IndexExpr::Constant(0)),
            Box::new(IndexExpr::Variable("n".to_string()))
        ),
        IndexExpr::All,
    ];
    
    for expr in exprs {
        let json = serde_json::to_string(&expr).unwrap();
        let deserialized: IndexExpr = serde_json::from_str(&json).unwrap();
        assert_eq!(expr, deserialized);
    }
}

#[test]
fn test_heap_region_serialization() {
    let regions = vec![
        HeapRegion::Named("heap1".to_string()),
        HeapRegion::ReachableFrom("ptr".to_string()),
        HeapRegion::Fresh,
        HeapRegion::Global,
    ];
    
    for region in regions {
        let json = serde_json::to_string(&region).unwrap();
        let deserialized: HeapRegion = serde_json::from_str(&json).unwrap();
        assert_eq!(region, deserialized);
    }
}