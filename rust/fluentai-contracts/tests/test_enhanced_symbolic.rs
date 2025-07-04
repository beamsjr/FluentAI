//! Test for enhanced symbolic value types

use fluentai_contracts::symbolic_execution::{SymbolicValue, SymbolicType, SymbolicState, ListOperation};
use fluentai_core::ast::Literal;

#[test]
fn test_enhanced_symbolic_types() {
    let mut state = SymbolicState::new();
    
    // Test creating typed symbolic values
    let int_sym = state.fresh_symbol_typed("x", SymbolicType::Integer);
    let float_sym = state.fresh_symbol_typed("y", SymbolicType::Float);
    let string_sym = state.fresh_symbol_typed("s", SymbolicType::String);
    
    // Test string concatenation
    let concat = SymbolicValue::StringConcat(vec![
        string_sym.clone(),
        SymbolicValue::Concrete(Literal::String(" world".to_string())),
    ]);
    
    // Test list operations
    let list_op = SymbolicValue::ListOp {
        op: ListOperation::Cons,
        args: vec![
            int_sym.clone(),
            SymbolicValue::List(vec![]),
        ],
    };
    
    // Test map value
    let map_val = SymbolicValue::Map(vec![
        (SymbolicValue::Concrete(Literal::String("key".to_string())), int_sym.clone()),
    ]);
    
    // Verify they can be created and used
    assert!(matches!(int_sym, SymbolicValue::Symbolic { .. }));
    assert!(matches!(concat, SymbolicValue::StringConcat(_)));
    assert!(matches!(list_op, SymbolicValue::ListOp { .. }));
    assert!(matches!(map_val, SymbolicValue::Map(_)));
}