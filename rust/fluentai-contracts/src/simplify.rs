//! Symbolic value simplification
//!
//! This module provides simplification of symbolic expressions to reduce
//! the complexity of constraints sent to the SMT solver.

use crate::symbolic_execution::{ListOperation, SymbolicValue};
use fluentai_core::ast::Literal;

/// Simplify a symbolic value by applying constant folding and algebraic identities
pub fn simplify(value: &SymbolicValue) -> SymbolicValue {
    match value {
        // Already simplified
        SymbolicValue::Concrete(_) | SymbolicValue::Symbolic { .. } | SymbolicValue::Unknown => {
            value.clone()
        }

        // Simplify binary operations
        SymbolicValue::BinOp { op, left, right } => {
            let left_simplified = simplify(left);
            let right_simplified = simplify(right);
            simplify_binop(op, left_simplified, right_simplified)
        }

        // Simplify unary operations
        SymbolicValue::UnaryOp { op, operand } => {
            let operand_simplified = simplify(operand);
            simplify_unaryop(op, operand_simplified)
        }

        // Simplify conditionals
        SymbolicValue::Conditional {
            condition,
            then_val,
            else_val,
        } => {
            let cond_simplified = simplify(condition);

            // If condition is concrete, select branch
            if let SymbolicValue::Concrete(Literal::Boolean(b)) = &cond_simplified {
                if *b {
                    simplify(then_val)
                } else {
                    simplify(else_val)
                }
            } else {
                // If both branches are identical, return one of them
                let then_simplified = simplify(then_val);
                let else_simplified = simplify(else_val);

                if then_simplified == else_simplified {
                    then_simplified
                } else {
                    SymbolicValue::Conditional {
                        condition: Box::new(cond_simplified),
                        then_val: Box::new(then_simplified),
                        else_val: Box::new(else_simplified),
                    }
                }
            }
        }

        // Simplify lists
        SymbolicValue::List(elements) => {
            SymbolicValue::List(elements.iter().map(simplify).collect())
        }

        // Simplify string concatenation
        SymbolicValue::StringConcat(parts) => simplify_string_concat(parts),

        // Simplify list operations
        SymbolicValue::ListOp { op, args } => {
            let simplified_args: Vec<_> = args.iter().map(simplify).collect();
            simplify_list_op(op, simplified_args)
        }

        // Other cases
        _ => value.clone(),
    }
}

/// Simplify binary operations
fn simplify_binop(op: &str, left: SymbolicValue, right: SymbolicValue) -> SymbolicValue {
    // Constant folding
    if let (SymbolicValue::Concrete(l), SymbolicValue::Concrete(r)) = (&left, &right) {
        if let Some(result) = evaluate_binop(op, l, r) {
            return SymbolicValue::Concrete(result);
        }
    }

    // Algebraic identities
    match op {
        "+" => {
            // x + 0 = x
            if let SymbolicValue::Concrete(Literal::Integer(0)) = right {
                return left;
            }
            // 0 + x = x
            if let SymbolicValue::Concrete(Literal::Integer(0)) = left {
                return right;
            }
        }
        "-" => {
            // x - 0 = x
            if let SymbolicValue::Concrete(Literal::Integer(0)) = right {
                return left;
            }
            // x - x = 0
            if left == right {
                return SymbolicValue::Concrete(Literal::Integer(0));
            }
        }
        "*" => {
            // x * 1 = x
            if let SymbolicValue::Concrete(Literal::Integer(1)) = right {
                return left;
            }
            // 1 * x = x
            if let SymbolicValue::Concrete(Literal::Integer(1)) = left {
                return right;
            }
            // x * 0 = 0
            if let SymbolicValue::Concrete(Literal::Integer(0)) = right {
                return right;
            }
            // 0 * x = 0
            if let SymbolicValue::Concrete(Literal::Integer(0)) = left {
                return left;
            }
        }
        "/" => {
            // x / 1 = x
            if let SymbolicValue::Concrete(Literal::Integer(1)) = right {
                return left;
            }
        }
        "and" => {
            // x and true = x
            if let SymbolicValue::Concrete(Literal::Boolean(true)) = right {
                return left;
            }
            // true and x = x
            if let SymbolicValue::Concrete(Literal::Boolean(true)) = left {
                return right;
            }
            // x and false = false
            if let SymbolicValue::Concrete(Literal::Boolean(false)) = right {
                return right;
            }
            // false and x = false
            if let SymbolicValue::Concrete(Literal::Boolean(false)) = left {
                return left;
            }
            // x and x = x
            if left == right {
                return left;
            }
        }
        "or" => {
            // x or false = x
            if let SymbolicValue::Concrete(Literal::Boolean(false)) = right {
                return left;
            }
            // false or x = x
            if let SymbolicValue::Concrete(Literal::Boolean(false)) = left {
                return right;
            }
            // x or true = true
            if let SymbolicValue::Concrete(Literal::Boolean(true)) = right {
                return right;
            }
            // true or x = true
            if let SymbolicValue::Concrete(Literal::Boolean(true)) = left {
                return left;
            }
            // x or x = x
            if left == right {
                return left;
            }
        }
        "=" => {
            // x = x => true
            if left == right {
                return SymbolicValue::Concrete(Literal::Boolean(true));
            }
        }
        _ => {}
    }

    // No simplification possible
    SymbolicValue::BinOp {
        op: op.to_string(),
        left: Box::new(left),
        right: Box::new(right),
    }
}

/// Simplify unary operations
fn simplify_unaryop(op: &str, operand: SymbolicValue) -> SymbolicValue {
    // Constant folding
    if let SymbolicValue::Concrete(lit) = &operand {
        if let Some(result) = evaluate_unaryop(op, lit) {
            return SymbolicValue::Concrete(result);
        }
    }

    // Special cases
    match op {
        "not" => {
            // not (not x) = x
            if let SymbolicValue::UnaryOp {
                op: inner_op,
                operand: inner,
            } = &operand
            {
                if inner_op == "not" {
                    return (**inner).clone();
                }
            }
        }
        "-" => {
            // -(-x) = x
            if let SymbolicValue::UnaryOp {
                op: inner_op,
                operand: inner,
            } = &operand
            {
                if inner_op == "-" {
                    return (**inner).clone();
                }
            }
        }
        _ => {}
    }

    SymbolicValue::UnaryOp {
        op: op.to_string(),
        operand: Box::new(operand),
    }
}

/// Simplify string concatenation
fn simplify_string_concat(parts: &[SymbolicValue]) -> SymbolicValue {
    let mut simplified_parts = Vec::new();
    let mut current_string = String::new();

    for part in parts {
        let simplified = simplify(part);
        if let SymbolicValue::Concrete(Literal::String(s)) = &simplified {
            current_string.push_str(s);
        } else {
            if !current_string.is_empty() {
                simplified_parts.push(SymbolicValue::Concrete(Literal::String(
                    current_string.clone(),
                )));
                current_string.clear();
            }
            simplified_parts.push(simplified);
        }
    }

    if !current_string.is_empty() {
        simplified_parts.push(SymbolicValue::Concrete(Literal::String(current_string)));
    }

    match simplified_parts.len() {
        0 => SymbolicValue::Concrete(Literal::String(String::new())),
        1 => simplified_parts.into_iter().next().unwrap(),
        _ => SymbolicValue::StringConcat(simplified_parts),
    }
}

/// Simplify list operations
fn simplify_list_op(op: &ListOperation, args: Vec<SymbolicValue>) -> SymbolicValue {
    match op {
        ListOperation::Length => {
            if args.len() == 1 {
                if let SymbolicValue::List(elements) = &args[0] {
                    return SymbolicValue::Concrete(Literal::Integer(elements.len() as i64));
                }
            }
        }
        ListOperation::Head => {
            if args.len() == 1 {
                if let SymbolicValue::List(elements) = &args[0] {
                    if !elements.is_empty() {
                        return elements[0].clone();
                    }
                }
            }
        }
        ListOperation::Tail => {
            if args.len() == 1 {
                if let SymbolicValue::List(elements) = &args[0] {
                    if !elements.is_empty() {
                        return SymbolicValue::List(elements[1..].to_vec());
                    }
                }
            }
        }
        ListOperation::Cons => {
            if args.len() == 2 {
                if let SymbolicValue::List(tail) = &args[1] {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.clone());
                    return SymbolicValue::List(new_list);
                }
            }
        }
        ListOperation::Append => {
            let mut all_concrete = true;
            let mut result = Vec::new();

            for arg in &args {
                if let SymbolicValue::List(elements) = arg {
                    result.extend(elements.clone());
                } else {
                    all_concrete = false;
                    break;
                }
            }

            if all_concrete {
                return SymbolicValue::List(result);
            }
        }
        _ => {}
    }

    SymbolicValue::ListOp {
        op: op.clone(),
        args,
    }
}

/// Evaluate binary operation on concrete values
fn evaluate_binop(op: &str, left: &Literal, right: &Literal) -> Option<Literal> {
    match (op, left, right) {
        // Arithmetic
        ("+", Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Integer(a + b)),
        ("-", Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Integer(a - b)),
        ("*", Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Integer(a * b)),
        ("/", Literal::Integer(a), Literal::Integer(b)) if *b != 0 => Some(Literal::Integer(a / b)),
        ("%", Literal::Integer(a), Literal::Integer(b)) if *b != 0 => Some(Literal::Integer(a % b)),

        // Comparison
        ("=", a, b) => Some(Literal::Boolean(a == b)),
        ("!=", a, b) => Some(Literal::Boolean(a != b)),
        ("<", Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Boolean(a < b)),
        (">", Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Boolean(a > b)),
        ("<=", Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Boolean(a <= b)),
        (">=", Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Boolean(a >= b)),

        // Boolean
        ("and", Literal::Boolean(a), Literal::Boolean(b)) => Some(Literal::Boolean(*a && *b)),
        ("or", Literal::Boolean(a), Literal::Boolean(b)) => Some(Literal::Boolean(*a || *b)),

        _ => None,
    }
}

/// Evaluate unary operation on concrete value
fn evaluate_unaryop(op: &str, operand: &Literal) -> Option<Literal> {
    match (op, operand) {
        ("not", Literal::Boolean(b)) => Some(Literal::Boolean(!b)),
        ("-", Literal::Integer(n)) => Some(Literal::Integer(-n)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic_simplification() {
        // x + 0 = x
        let x = SymbolicValue::Symbolic {
            name: "x".to_string(),
            ty: None,
        };
        let zero = SymbolicValue::Concrete(Literal::Integer(0));
        let expr = SymbolicValue::BinOp {
            op: "+".to_string(),
            left: Box::new(x.clone()),
            right: Box::new(zero),
        };
        assert_eq!(simplify(&expr), x);

        // 2 + 3 = 5
        let two = SymbolicValue::Concrete(Literal::Integer(2));
        let three = SymbolicValue::Concrete(Literal::Integer(3));
        let expr = SymbolicValue::BinOp {
            op: "+".to_string(),
            left: Box::new(two),
            right: Box::new(three),
        };
        assert_eq!(
            simplify(&expr),
            SymbolicValue::Concrete(Literal::Integer(5))
        );
    }

    #[test]
    fn test_boolean_simplification() {
        // x and true = x
        let x = SymbolicValue::Symbolic {
            name: "x".to_string(),
            ty: None,
        };
        let true_val = SymbolicValue::Concrete(Literal::Boolean(true));
        let expr = SymbolicValue::BinOp {
            op: "and".to_string(),
            left: Box::new(x.clone()),
            right: Box::new(true_val),
        };
        assert_eq!(simplify(&expr), x);

        // not (not x) = x
        let not_not_x = SymbolicValue::UnaryOp {
            op: "not".to_string(),
            operand: Box::new(SymbolicValue::UnaryOp {
                op: "not".to_string(),
                operand: Box::new(x.clone()),
            }),
        };
        assert_eq!(simplify(&not_not_x), x);
    }

    #[test]
    fn test_conditional_simplification() {
        let x = SymbolicValue::Symbolic {
            name: "x".to_string(),
            ty: None,
        };
        let y = SymbolicValue::Symbolic {
            name: "y".to_string(),
            ty: None,
        };

        // if true then x else y = x
        let cond = SymbolicValue::Conditional {
            condition: Box::new(SymbolicValue::Concrete(Literal::Boolean(true))),
            then_val: Box::new(x.clone()),
            else_val: Box::new(y.clone()),
        };
        assert_eq!(simplify(&cond), x);

        // if c then x else x = x
        let cond = SymbolicValue::Conditional {
            condition: Box::new(SymbolicValue::Symbolic {
                name: "c".to_string(),
                ty: None,
            }),
            then_val: Box::new(x.clone()),
            else_val: Box::new(x.clone()),
        };
        assert_eq!(simplify(&cond), x);
    }
}
