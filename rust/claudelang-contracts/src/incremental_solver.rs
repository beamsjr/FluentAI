//! Incremental Z3 solver with push/pop support
//! 
//! This module provides an incremental solver that can efficiently
//! check multiple related constraint sets using Z3's push/pop mechanism.

#[cfg(feature = "static")]
use z3::{Context, Solver, ast::{Ast, Bool as Z3Bool, Int as Z3Int, Dynamic as Z3Expr}};

use crate::symbolic_execution::{SymbolicValue, PathConstraint, SymbolicState, SymbolicType};
use crate::simplify::simplify;
use crate::errors::{ContractError, ContractResult};
use std::collections::HashMap;

#[cfg(feature = "static")]
/// Incremental symbolic constraint solver using Z3
pub struct IncrementalSolver<'ctx> {
    context: &'ctx Context,
    solver: Solver<'ctx>,
    /// Mapping from symbolic variable names to Z3 expressions
    variables: HashMap<String, Z3Expr<'ctx>>,
    /// Stack of push operations for backtracking
    push_stack: Vec<usize>,
    /// Cache of previously solved states
    cache: HashMap<u64, bool>,
}

#[cfg(feature = "static")]
impl<'ctx> IncrementalSolver<'ctx> {
    /// Create a new incremental solver
    pub fn new(context: &'ctx Context) -> Self {
        Self {
            context,
            solver: Solver::new(context),
            variables: HashMap::new(),
            push_stack: Vec::new(),
            cache: HashMap::new(),
        }
    }
    
    /// Push a new scope onto the solver stack
    pub fn push(&mut self) {
        self.solver.push();
        self.push_stack.push(self.variables.len());
    }
    
    /// Pop a scope from the solver stack
    pub fn pop(&mut self) -> ContractResult<()> {
        if self.push_stack.is_empty() {
            return Err(ContractError::Other("No scope to pop".to_string()));
        }
        
        self.solver.pop(1);
        
        // Restore variable count
        let prev_var_count = self.push_stack.pop().unwrap();
        let current_var_count = self.variables.len();
        
        // Remove variables added in this scope
        if current_var_count > prev_var_count {
            let vars_to_remove: Vec<String> = self.variables
                .keys()
                .skip(prev_var_count)
                .cloned()
                .collect();
            
            for var in vars_to_remove {
                self.variables.remove(&var);
            }
        }
        
        Ok(())
    }
    
    /// Get the current scope depth
    pub fn scope_depth(&self) -> usize {
        self.push_stack.len()
    }
    
    /// Add a single constraint to the current scope
    pub fn add_constraint(&mut self, constraint: &PathConstraint) -> ContractResult<()> {
        // Simplify constraint before converting to Z3
        let simplified = simplify(&constraint.constraint);
        let z3_constraint = self.symbolic_to_z3(&simplified)?;
        let bool_constraint = z3_constraint.as_bool()
            .ok_or_else(|| ContractError::Other("Expected boolean constraint".to_string()))?;
        
        if constraint.expected {
            self.solver.assert(&bool_constraint);
        } else {
            self.solver.assert(&bool_constraint.not());
        }
        
        Ok(())
    }
    
    /// Check satisfiability of current constraints
    pub fn check(&self) -> ContractResult<bool> {
        match self.solver.check() {
            z3::SatResult::Sat => Ok(true),
            z3::SatResult::Unsat => Ok(false),
            z3::SatResult::Unknown => Err(ContractError::Other(
                "Z3 returned unknown".to_string()
            )),
        }
    }
    
    /// Check if a symbolic state is satisfiable incrementally
    pub fn check_state_incremental(&mut self, state: &SymbolicState) -> ContractResult<bool> {
        // Check cache first
        let state_hash = self.hash_state(state);
        if let Some(&result) = self.cache.get(&state_hash) {
            return Ok(result);
        }
        
        // Push a new scope for this state
        self.push();
        
        // Add all constraints
        for constraint in &state.path_constraints {
            self.add_constraint(constraint)?;
        }
        
        // Check satisfiability
        let result = self.check()?;
        
        // Cache the result
        self.cache.insert(state_hash, result);
        
        // Pop the scope
        self.pop()?;
        
        Ok(result)
    }
    
    /// Check multiple states efficiently using incremental solving
    pub fn check_states_incremental(
        &mut self,
        states: &[SymbolicState],
    ) -> ContractResult<Vec<bool>> {
        let mut results = Vec::new();
        
        // Find common prefix of constraints
        let common_prefix_len = self.find_common_prefix_length(states);
        
        if common_prefix_len > 0 {
            // Push common constraints once
            self.push();
            
            for i in 0..common_prefix_len {
                self.add_constraint(&states[0].path_constraints[i])?;
            }
            
            // Check each state with only unique constraints
            for state in states {
                self.push();
                
                // Add unique constraints for this state
                for constraint in &state.path_constraints[common_prefix_len..] {
                    self.add_constraint(constraint)?;
                }
                
                results.push(self.check()?);
                
                self.pop()?;
            }
            
            // Pop common constraints
            self.pop()?;
        } else {
            // No common prefix, check each state independently
            for state in states {
                results.push(self.check_state_incremental(state)?);
            }
        }
        
        Ok(results)
    }
    
    /// Find the length of common constraint prefix among states
    fn find_common_prefix_length(&self, states: &[SymbolicState]) -> usize {
        if states.is_empty() {
            return 0;
        }
        
        let first_state = &states[0];
        let mut common_len = 0;
        
        'outer: for i in 0..first_state.path_constraints.len() {
            for state in &states[1..] {
                if i >= state.path_constraints.len() ||
                   state.path_constraints[i] != first_state.path_constraints[i] {
                    break 'outer;
                }
            }
            common_len = i + 1;
        }
        
        common_len
    }
    
    /// Hash a symbolic state for caching
    fn hash_state(&self, state: &SymbolicState) -> u64 {
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;
        
        let mut hasher = DefaultHasher::new();
        
        // Hash constraints
        for constraint in &state.path_constraints {
            format!("{:?}", constraint).hash(&mut hasher);
        }
        
        hasher.finish()
    }
    
    /// Convert symbolic value to Z3 expression (reused from symbolic_z3.rs)
    pub fn symbolic_to_z3(&mut self, value: &SymbolicValue) -> ContractResult<Z3Expr<'ctx>> {
        match value {
            SymbolicValue::Concrete(lit) => {
                use claudelang_core::ast::Literal;
                match lit {
                    Literal::Integer(n) => Ok(Z3Int::from_i64(self.context, *n).into()),
                    Literal::Boolean(b) => Ok(Z3Bool::from_bool(self.context, *b).into()),
                    _ => Err(ContractError::Other(
                        format!("Unsupported literal type in Z3: {:?}", lit)
                    )),
                }
            }
            
            SymbolicValue::Symbolic { name, ty } => {
                // Get or create Z3 variable
                if let Some(var) = self.variables.get(name) {
                    Ok(var.clone())
                } else {
                    // Create Z3 variable based on type hint
                    let dynamic_var: Z3Expr = match ty {
                        Some(SymbolicType::Boolean) => {
                            Z3Bool::new_const(self.context, name.clone()).into()
                        }
                        Some(SymbolicType::Float) => {
                            use z3::ast::Real as Z3Real;
                            Z3Real::new_const(self.context, name.clone()).into()
                        }
                        _ => {
                            // Default to integer for unknown types
                            Z3Int::new_const(self.context, name.clone()).into()
                        }
                    };
                    self.variables.insert(name.clone(), dynamic_var.clone());
                    Ok(dynamic_var)
                }
            }
            
            SymbolicValue::BinOp { op, left, right } => {
                let left_expr = self.symbolic_to_z3(left)?;
                let right_expr = self.symbolic_to_z3(right)?;
                
                match op.as_str() {
                    // Arithmetic operations
                    "+" => {
                        let l = left_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        let r = right_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        Ok((l + r).into())
                    }
                    "-" => {
                        let l = left_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        let r = right_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        Ok((l - r).into())
                    }
                    "*" => {
                        let l = left_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        let r = right_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        Ok((l * r).into())
                    }
                    "/" => {
                        let l = left_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        let r = right_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        Ok((l / r).into())
                    }
                    
                    // Comparison operations
                    "=" => Ok(left_expr._eq(&right_expr).into()),
                    "!=" => Ok(left_expr._eq(&right_expr).not().into()),
                    "<" => {
                        let l = left_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        let r = right_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        Ok(l.lt(&r).into())
                    }
                    ">" => {
                        let l = left_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        let r = right_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        Ok(l.gt(&r).into())
                    }
                    "<=" => {
                        let l = left_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        let r = right_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        Ok(l.le(&r).into())
                    }
                    ">=" => {
                        let l = left_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        let r = right_expr.as_int().ok_or_else(|| ContractError::Other("Expected int".to_string()))?;
                        Ok(l.ge(&r).into())
                    }
                    
                    // Logical operations
                    "and" => {
                        let l = left_expr.as_bool().ok_or_else(|| ContractError::Other("Expected bool".to_string()))?;
                        let r = right_expr.as_bool().ok_or_else(|| ContractError::Other("Expected bool".to_string()))?;
                        Ok((l & r).into())
                    }
                    "or" => {
                        let l = left_expr.as_bool().ok_or_else(|| ContractError::Other("Expected bool".to_string()))?;
                        let r = right_expr.as_bool().ok_or_else(|| ContractError::Other("Expected bool".to_string()))?;
                        Ok((l | r).into())
                    }
                    
                    _ => Err(ContractError::Other(format!("Unknown binary operator: {}", op))),
                }
            }
            
            SymbolicValue::UnaryOp { op, operand } => {
                let operand_expr = self.symbolic_to_z3(operand)?;
                
                match op.as_str() {
                    "not" => {
                        let bool_expr = operand_expr.as_bool()
                            .ok_or_else(|| ContractError::Other("Expected bool for not".to_string()))?;
                        Ok(bool_expr.not().into())
                    }
                    "-" => {
                        let int_expr = operand_expr.as_int()
                            .ok_or_else(|| ContractError::Other("Expected int for negation".to_string()))?;
                        Ok((-int_expr).into())
                    }
                    _ => Err(ContractError::Other(format!("Unknown unary operator: {}", op))),
                }
            }
            
            SymbolicValue::Conditional { condition, then_val, else_val } => {
                let cond_expr = self.symbolic_to_z3(condition)?;
                let then_expr = self.symbolic_to_z3(then_val)?;
                let else_expr = self.symbolic_to_z3(else_val)?;
                
                let bool_cond = cond_expr.as_bool()
                    .ok_or_else(|| ContractError::Other("Expected bool condition".to_string()))?;
                
                Ok(bool_cond.ite(&then_expr, &else_expr))
            }
            
            _ => Err(ContractError::Other(
                format!("Unsupported symbolic value for Z3: {:?}", value)
            )),
        }
    }
    
    /// Get a model for the current constraints
    pub fn get_model(&self) -> ContractResult<HashMap<String, i64>> {
        let model = self.solver.get_model()
            .ok_or_else(|| ContractError::Other("No model available".to_string()))?;
        
        let mut values = HashMap::new();
        
        for (name, var) in &self.variables {
            if let Some(int_var) = var.as_int() {
                if let Some(val) = model.eval(&int_var, true) {
                    if let Some(n) = val.as_i64() {
                        values.insert(name.clone(), n);
                    }
                }
            }
        }
        
        Ok(values)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbolic_execution::{SymbolicState, PathConstraint};
    use claudelang_core::ast::Literal;
    
    #[test]
    #[cfg(feature = "static")]
    fn test_incremental_solving() {
        let context = Context::new(&z3::Config::new());
        let mut solver = IncrementalSolver::new(&context);
        
        // Create a simple constraint: x > 0
        let x = SymbolicValue::Symbolic {
            name: "x".to_string(),
            ty: Some(SymbolicType::Integer),
        };
        let zero = SymbolicValue::Concrete(Literal::Integer(0));
        let constraint = PathConstraint {
            constraint: SymbolicValue::BinOp {
                op: ">".to_string(),
                left: Box::new(x.clone()),
                right: Box::new(zero.clone()),
            },
            expected: true,
        };
        
        // Add constraint and check
        solver.add_constraint(&constraint).unwrap();
        assert!(solver.check().unwrap());
        
        // Push a new scope and add conflicting constraint: x < 0
        solver.push();
        let neg_constraint = PathConstraint {
            constraint: SymbolicValue::BinOp {
                op: "<".to_string(),
                left: Box::new(x.clone()),
                right: Box::new(zero.clone()),
            },
            expected: true,
        };
        solver.add_constraint(&neg_constraint).unwrap();
        
        // Should be unsatisfiable
        assert!(!solver.check().unwrap());
        
        // Pop the scope
        solver.pop().unwrap();
        
        // Should be satisfiable again
        assert!(solver.check().unwrap());
    }
    
    #[test]
    #[cfg(feature = "static")]
    fn test_common_prefix_optimization() {
        let context = Context::new(&z3::Config::new());
        let mut solver = IncrementalSolver::new(&context);
        
        // Create states with common prefix
        let x = SymbolicValue::Symbolic {
            name: "x".to_string(),
            ty: Some(SymbolicType::Integer),
        };
        let y = SymbolicValue::Symbolic {
            name: "y".to_string(),
            ty: Some(SymbolicType::Integer),
        };
        let zero = SymbolicValue::Concrete(Literal::Integer(0));
        let one = SymbolicValue::Concrete(Literal::Integer(1));
        
        // Common constraint: x > 0
        let common_constraint = PathConstraint {
            constraint: SymbolicValue::BinOp {
                op: ">".to_string(),
                left: Box::new(x.clone()),
                right: Box::new(zero.clone()),
            },
            expected: true,
        };
        
        // State 1: x > 0 && y > 0
        let mut state1 = SymbolicState::new();
        state1.path_constraints.push(common_constraint.clone());
        state1.path_constraints.push(PathConstraint {
            constraint: SymbolicValue::BinOp {
                op: ">".to_string(),
                left: Box::new(y.clone()),
                right: Box::new(zero.clone()),
            },
            expected: true,
        });
        
        // State 2: x > 0 && y < 1
        let mut state2 = SymbolicState::new();
        state2.path_constraints.push(common_constraint.clone());
        state2.path_constraints.push(PathConstraint {
            constraint: SymbolicValue::BinOp {
                op: "<".to_string(),
                left: Box::new(y.clone()),
                right: Box::new(one.clone()),
            },
            expected: true,
        });
        
        // Check both states incrementally
        let results = solver.check_states_incremental(&[state1, state2]).unwrap();
        assert_eq!(results.len(), 2);
        assert!(results[0]); // Both constraints are satisfiable
        assert!(results[1]); // Both constraints are satisfiable
    }
}