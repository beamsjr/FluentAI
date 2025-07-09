//! Tests for symbolic execution functionality

use fluentai_contracts::symbolic_execution::{
    ListOperation, SymbolicState, SymbolicType, SymbolicValue,
};
use fluentai_core::ast::Literal;

#[test]
fn test_symbolic_value_variants() {
    // Test concrete value
    let concrete = SymbolicValue::Concrete(Literal::Integer(42));
    match concrete {
        SymbolicValue::Concrete(Literal::Integer(n)) => assert_eq!(n, 42),
        _ => panic!("Expected concrete integer"),
    }

    // Test symbolic value
    let symbolic = SymbolicValue::Symbolic {
        name: "x".to_string(),
        ty: Some(SymbolicType::Integer),
    };
    match symbolic {
        SymbolicValue::Symbolic { name, ty } => {
            assert_eq!(name, "x");
            assert_eq!(ty, Some(SymbolicType::Integer));
        }
        _ => panic!("Expected symbolic value"),
    }

    // Test unknown value
    let unknown = SymbolicValue::Unknown;
    assert!(matches!(unknown, SymbolicValue::Unknown));
}

#[test]
fn test_symbolic_state() {
    let mut state = SymbolicState::new();

    // Test fresh symbol generation
    let sym1 = state.fresh_symbol("x");
    let sym2 = state.fresh_symbol("x");

    match (&sym1, &sym2) {
        (SymbolicValue::Symbolic { name: n1, .. }, SymbolicValue::Symbolic { name: n2, .. }) => {
            assert_ne!(n1, n2);
            assert!(n1.starts_with("x_"));
            assert!(n2.starts_with("x_"));
        }
        _ => panic!("Expected symbolic values"),
    }

    // Test symbol counter increments
    assert_eq!(state.symbol_counter, 2);
}

#[test]
fn test_symbolic_state_bindings() {
    let mut state = SymbolicState::new();

    let value = SymbolicValue::Concrete(Literal::Boolean(true));
    state.bindings.insert("flag".to_string(), value.clone());

    assert_eq!(state.bindings.len(), 1);
    assert_eq!(state.bindings.get("flag"), Some(&value));
}

#[test]
fn test_path_constraints() {
    let mut state = SymbolicState::new();

    let constraint = SymbolicValue::BinOp {
        op: ">".to_string(),
        left: Box::new(SymbolicValue::Symbolic {
            name: "x".to_string(),
            ty: Some(SymbolicType::Integer),
        }),
        right: Box::new(SymbolicValue::Concrete(Literal::Integer(0))),
    };

    state.add_constraint(constraint, true);

    assert_eq!(state.path_constraints.len(), 1);
    assert!(state.path_constraints[0].expected);
}

#[test]
fn test_symbolic_binary_operations() {
    let left = SymbolicValue::Concrete(Literal::Integer(10));
    let right = SymbolicValue::Concrete(Literal::Integer(5));

    let add = SymbolicValue::BinOp {
        op: "+".to_string(),
        left: Box::new(left.clone()),
        right: Box::new(right.clone()),
    };

    let sub = SymbolicValue::BinOp {
        op: "-".to_string(),
        left: Box::new(left.clone()),
        right: Box::new(right.clone()),
    };

    match add {
        SymbolicValue::BinOp { op, .. } => assert_eq!(op, "+"),
        _ => panic!("Expected addition"),
    }

    match sub {
        SymbolicValue::BinOp { op, .. } => assert_eq!(op, "-"),
        _ => panic!("Expected subtraction"),
    }
}

#[test]
fn test_symbolic_unary_operations() {
    let operand = SymbolicValue::Concrete(Literal::Boolean(false));

    let not_op = SymbolicValue::UnaryOp {
        op: "not".to_string(),
        operand: Box::new(operand.clone()),
    };

    let neg_op = SymbolicValue::UnaryOp {
        op: "-".to_string(),
        operand: Box::new(SymbolicValue::Concrete(Literal::Integer(42))),
    };

    match not_op {
        SymbolicValue::UnaryOp { op, .. } => assert_eq!(op, "not"),
        _ => panic!("Expected not operation"),
    }

    match neg_op {
        SymbolicValue::UnaryOp { op, .. } => assert_eq!(op, "-"),
        _ => panic!("Expected negation"),
    }
}

#[test]
fn test_symbolic_conditional() {
    let cond = SymbolicValue::Concrete(Literal::Boolean(true));
    let then_val = SymbolicValue::Concrete(Literal::String("yes".to_string()));
    let else_val = SymbolicValue::Concrete(Literal::String("no".to_string()));

    let conditional = SymbolicValue::Conditional {
        condition: Box::new(cond),
        then_val: Box::new(then_val),
        else_val: Box::new(else_val),
    };

    match &conditional {
        SymbolicValue::Conditional {
            condition,
            then_val,
            else_val,
        } => {
            assert!(matches!(
                condition.as_ref(),
                SymbolicValue::Concrete(Literal::Boolean(true))
            ));
            assert!(matches!(
                then_val.as_ref(),
                SymbolicValue::Concrete(Literal::String(_))
            ));
            assert!(matches!(
                else_val.as_ref(),
                SymbolicValue::Concrete(Literal::String(_))
            ));
        }
        _ => panic!("Expected conditional"),
    }
}

#[test]
fn test_symbolic_lists() {
    let elements = vec![
        SymbolicValue::Concrete(Literal::Integer(1)),
        SymbolicValue::Concrete(Literal::Integer(2)),
        SymbolicValue::Concrete(Literal::Integer(3)),
    ];

    let list = SymbolicValue::List(elements.clone());

    match &list {
        SymbolicValue::List(elems) => {
            assert_eq!(elems.len(), 3);
            match &elems[0] {
                SymbolicValue::Concrete(Literal::Integer(n)) => assert_eq!(*n, 1),
                _ => panic!("Expected integer"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_list_operations() {
    let list = SymbolicValue::List(vec![
        SymbolicValue::Concrete(Literal::Integer(10)),
        SymbolicValue::Concrete(Literal::Integer(20)),
    ]);

    let head = SymbolicValue::ListOp {
        op: ListOperation::Head,
        args: vec![list.clone()],
    };

    let tail = SymbolicValue::ListOp {
        op: ListOperation::Tail,
        args: vec![list.clone()],
    };

    let cons = SymbolicValue::ListOp {
        op: ListOperation::Cons,
        args: vec![SymbolicValue::Concrete(Literal::Integer(5)), list.clone()],
    };

    let length = SymbolicValue::ListOp {
        op: ListOperation::Length,
        args: vec![list.clone()],
    };

    assert!(matches!(
        head,
        SymbolicValue::ListOp {
            op: ListOperation::Head,
            ..
        }
    ));
    assert!(matches!(
        tail,
        SymbolicValue::ListOp {
            op: ListOperation::Tail,
            ..
        }
    ));
    assert!(matches!(
        cons,
        SymbolicValue::ListOp {
            op: ListOperation::Cons,
            ..
        }
    ));
    assert!(matches!(
        length,
        SymbolicValue::ListOp {
            op: ListOperation::Length,
            ..
        }
    ));
}

#[test]
fn test_symbolic_map() {
    let map_entries = vec![
        (
            SymbolicValue::Concrete(Literal::String("key1".to_string())),
            SymbolicValue::Concrete(Literal::Integer(100)),
        ),
        (
            SymbolicValue::Concrete(Literal::String("key2".to_string())),
            SymbolicValue::Concrete(Literal::Integer(200)),
        ),
    ];

    let map = SymbolicValue::Map(map_entries);

    match map {
        SymbolicValue::Map(entries) => {
            assert_eq!(entries.len(), 2);
            match &entries[0] {
                (
                    SymbolicValue::Concrete(Literal::String(k)),
                    SymbolicValue::Concrete(Literal::Integer(v)),
                ) => {
                    assert_eq!(k, "key1");
                    assert_eq!(*v, 100);
                }
                _ => panic!("Expected string key and integer value"),
            }
        }
        _ => panic!("Expected map"),
    }
}

#[test]
fn test_symbolic_application() {
    let args = vec![
        SymbolicValue::Concrete(Literal::Integer(5)),
        SymbolicValue::Concrete(Literal::Integer(3)),
    ];

    let app = SymbolicValue::Application {
        function: "max".to_string(),
        args: args.clone(),
    };

    match app {
        SymbolicValue::Application { function, args } => {
            assert_eq!(function, "max");
            assert_eq!(args.len(), 2);
        }
        _ => panic!("Expected application"),
    }
}

#[test]
fn test_string_concat() {
    let parts = vec![
        SymbolicValue::Concrete(Literal::String("Hello".to_string())),
        SymbolicValue::Concrete(Literal::String(", ".to_string())),
        SymbolicValue::Concrete(Literal::String("World!".to_string())),
    ];

    let concat = SymbolicValue::StringConcat(parts);

    match concat {
        SymbolicValue::StringConcat(parts) => {
            assert_eq!(parts.len(), 3);
            match &parts[0] {
                SymbolicValue::Concrete(Literal::String(s)) => assert_eq!(s, "Hello"),
                _ => panic!("Expected string"),
            }
        }
        _ => panic!("Expected string concatenation"),
    }
}

#[test]
fn test_symbolic_state_fork() {
    let mut state = SymbolicState::new();

    // Add some state
    state.bindings.insert(
        "x".to_string(),
        SymbolicValue::Concrete(Literal::Integer(42)),
    );
    state.symbol_counter = 10;
    state.add_constraint(SymbolicValue::Concrete(Literal::Boolean(true)), true);

    // Fork the state
    let forked = state.fork();

    // Verify the fork is a deep copy
    assert_eq!(forked.bindings.len(), state.bindings.len());
    assert_eq!(forked.symbol_counter, state.symbol_counter);
    assert_eq!(forked.path_constraints.len(), state.path_constraints.len());

    // Verify they're independent
    assert_eq!(forked.bindings.get("x"), state.bindings.get("x"));
}

#[test]
fn test_symbolic_types() {
    // Just verify all types exist
    let _ = SymbolicType::Integer;
    let _ = SymbolicType::Float;
    let _ = SymbolicType::Boolean;
    let _ = SymbolicType::String;
    let _ = SymbolicType::List;
    let _ = SymbolicType::Map;
}

#[test]
fn test_fresh_symbol_typed() {
    let mut state = SymbolicState::new();

    let int_sym = state.fresh_symbol_typed("n", SymbolicType::Integer);
    let bool_sym = state.fresh_symbol_typed("b", SymbolicType::Boolean);

    match int_sym {
        SymbolicValue::Symbolic { name, ty } => {
            assert!(name.starts_with("n_"));
            assert_eq!(ty, Some(SymbolicType::Integer));
        }
        _ => panic!("Expected symbolic integer"),
    }

    match bool_sym {
        SymbolicValue::Symbolic { name, ty } => {
            assert!(name.starts_with("b_"));
            assert_eq!(ty, Some(SymbolicType::Boolean));
        }
        _ => panic!("Expected symbolic boolean"),
    }
}
