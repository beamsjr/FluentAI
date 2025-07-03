//! Convert ClaudeLang AST expressions to Z3 formulas

#[cfg(feature = "static")]
pub mod implementation {
    use z3::{ast::{Ast, Bool, Int, Real, forall_const, exists_const}, Context, Pattern};
    use claudelang_core::ast::{Graph, Node, NodeId, Literal};
    use crate::{
        errors::{ContractError, ContractResult},
        quantifiers::{QuantifierParser, QuantifierDomain, Quantifier},
    };
    use std::collections::HashMap;
    
    /// Converts ClaudeLang AST expressions to Z3 formulas
    pub struct Z3Converter<'ctx> {
        context: &'ctx Context,
        graph: &'ctx Graph,
        /// Variable bindings from names to Z3 expressions
        variables: HashMap<String, Z3Expr<'ctx>>,
    }
    
    /// Represents a Z3 expression that could be of different sorts
    pub enum Z3Expr<'ctx> {
        Bool(Bool<'ctx>),
        Int(Int<'ctx>),
        Real(Real<'ctx>),
        Array(z3::ast::Array<'ctx>),
        String(z3::ast::String<'ctx>),
    }
    
    impl<'ctx> Z3Converter<'ctx> {
        /// Create a new converter
        pub fn new(context: &'ctx Context, graph: &'ctx Graph) -> Self {
            Self {
                context,
                graph,
                variables: HashMap::new(),
            }
        }
        
        /// Declare a variable with the given name and type
        pub fn declare_var(&mut self, name: &str, sort: Z3Sort) -> Z3Expr<'ctx> {
            let expr = match sort {
                Z3Sort::Bool => Z3Expr::Bool(Bool::new_const(self.context, name)),
                Z3Sort::Int => Z3Expr::Int(Int::new_const(self.context, name)),
                Z3Sort::Real => Z3Expr::Real(Real::new_const(self.context, name)),
                Z3Sort::String => Z3Expr::String(z3::ast::String::new_const(self.context, name)),
            };
            self.variables.insert(name.to_string(), expr.clone());
            expr
        }
        
        /// Convert a ClaudeLang AST node to a Z3 expression
        pub fn convert_node(&self, node_id: NodeId) -> ContractResult<Z3Expr<'ctx>> {
            let node = self.graph.get_node(node_id)
                .ok_or_else(|| ContractError::InvalidExpression(
                    format!("Node {} not found in graph", node_id)
                ))?;
            
            match node {
                Node::Literal(lit) => self.convert_literal(lit),
                Node::Variable { name } => self.convert_variable(name),
                Node::Application { function, args } => {
                    self.convert_application(*function, args)
                }
                _ => Err(ContractError::InvalidExpression(
                    format!("Cannot convert node type {:?} to Z3 expression", node)
                ))
            }
        }
        
        /// Convert a literal to Z3
        fn convert_literal(&self, literal: &Literal) -> ContractResult<Z3Expr<'ctx>> {
            match literal {
                Literal::Integer(n) => Ok(Z3Expr::Int(Int::from_i64(self.context, *n))),
                Literal::Float(f) => Ok(Z3Expr::Real(Real::from_real(self.context, *f as i32, 1))),
                Literal::Bool(b) => Ok(Z3Expr::Bool(Bool::from_bool(self.context, *b))),
                _ => Err(ContractError::InvalidExpression(
                    format!("Cannot convert literal {:?} to Z3", literal)
                ))
            }
        }
        
        /// Convert a variable reference to Z3
        fn convert_variable(&self, name: &str) -> ContractResult<Z3Expr<'ctx>> {
            self.variables.get(name)
                .cloned()
                .ok_or_else(|| ContractError::InvalidExpression(
                    format!("Variable {} not declared", name)
                ))
        }
        
        /// Convert a function application to Z3
        fn convert_application(&self, func_id: NodeId, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            // Get the function node
            let func_node = self.graph.get_node(func_id)
                .ok_or_else(|| ContractError::InvalidExpression(
                    format!("Function node {} not found", func_id)
                ))?;
            
            // Check if it's a built-in operator
            if let Node::Variable { name } = func_node {
                match name.as_str() {
                    // Arithmetic operators
                    "+" => self.convert_binary_arith(args, |a, b| a + b),
                    "-" => self.convert_binary_arith(args, |a, b| a - b),
                    "*" => self.convert_binary_arith(args, |a, b| a * b),
                    "/" => self.convert_binary_arith(args, |a, b| a / b),
                    "mod" | "modulo" | "%" => self.convert_modulo(args),
                    "abs" => self.convert_abs(args),
                    "min" => self.convert_min_max(args, true),
                    "max" => self.convert_min_max(args, false),
                    
                    // Comparison operators
                    "=" | "==" | "eq?" => self.convert_comparison(args, |a, b| a._eq(&b)),
                    "!=" | "<>" | "not=" => self.convert_not_equal(args),
                    "<" => self.convert_comparison(args, |a, b| a.lt(&b)),
                    "<=" => self.convert_comparison(args, |a, b| a.le(&b)),
                    ">" => self.convert_comparison(args, |a, b| a.gt(&b)),
                    ">=" => self.convert_comparison(args, |a, b| a.ge(&b)),
                    
                    // Logical operators
                    "and" => self.convert_logical(args, |a, b| a & b),
                    "or" => self.convert_logical(args, |a, b| a | b),
                    "not" => self.convert_not(args),
                    "xor" => self.convert_logical(args, |a, b| a.xor(&b)),
                    "implies" | "=>" => self.convert_logical(args, |a, b| a.implies(&b)),
                    
                    // Predicates
                    "zero?" => self.convert_zero_pred(args),
                    "positive?" => self.convert_positive_pred(args),
                    "negative?" => self.convert_negative_pred(args),
                    "even?" => self.convert_even_pred(args),
                    "odd?" => self.convert_odd_pred(args),
                    
                    // List/Array operations
                    "length" | "list-length" => self.convert_length(args),
                    "nth" | "list-ref" => self.convert_nth(args),
                    "member?" | "member" => self.convert_member(args),
                    "null?" | "empty?" => self.convert_null_pred(args),
                    
                    // Contract-specific functions
                    "old" => self.convert_old(args),
                    
                    // Quantifiers
                    "forall" | "∀" => self.convert_forall(args),
                    "exists" | "∃" => self.convert_exists(args),
                    
                    _ => Err(ContractError::InvalidExpression(
                        format!("Unknown operator: {}", name)
                    ))
                }
            } else {
                Err(ContractError::InvalidExpression(
                    "Function application must be to a named operator".to_string()
                ))
            }
        }
        
        /// Convert binary arithmetic operation
        fn convert_binary_arith<F>(&self, args: &[NodeId], op: F) -> ContractResult<Z3Expr<'ctx>>
        where
            F: Fn(&Int<'ctx>, &Int<'ctx>) -> Int<'ctx>,
        {
            if args.len() != 2 {
                return Err(ContractError::InvalidExpression(
                    format!("Binary operator expects 2 arguments, got {}", args.len())
                ));
            }
            
            let left = self.convert_node(args[0])?;
            let right = self.convert_node(args[1])?;
            
            match (left, right) {
                (Z3Expr::Int(l), Z3Expr::Int(r)) => Ok(Z3Expr::Int(op(&l, &r))),
                _ => Err(ContractError::InvalidExpression(
                    "Arithmetic operators require integer arguments".to_string()
                ))
            }
        }
        
        /// Convert comparison operation
        fn convert_comparison<F>(&self, args: &[NodeId], op: F) -> ContractResult<Z3Expr<'ctx>>
        where
            F: Fn(&Int<'ctx>, &Int<'ctx>) -> Bool<'ctx>,
        {
            if args.len() != 2 {
                return Err(ContractError::InvalidExpression(
                    format!("Comparison operator expects 2 arguments, got {}", args.len())
                ));
            }
            
            let left = self.convert_node(args[0])?;
            let right = self.convert_node(args[1])?;
            
            match (left, right) {
                (Z3Expr::Int(l), Z3Expr::Int(r)) => Ok(Z3Expr::Bool(op(&l, &r))),
                _ => Err(ContractError::InvalidExpression(
                    "Comparison operators require integer arguments".to_string()
                ))
            }
        }
        
        /// Convert logical operation
        fn convert_logical<F>(&self, args: &[NodeId], op: F) -> ContractResult<Z3Expr<'ctx>>
        where
            F: Fn(&Bool<'ctx>, &Bool<'ctx>) -> Bool<'ctx>,
        {
            if args.len() != 2 {
                return Err(ContractError::InvalidExpression(
                    format!("Logical operator expects 2 arguments, got {}", args.len())
                ));
            }
            
            let left = self.convert_node(args[0])?;
            let right = self.convert_node(args[1])?;
            
            match (left, right) {
                (Z3Expr::Bool(l), Z3Expr::Bool(r)) => Ok(Z3Expr::Bool(op(&l, &r))),
                _ => Err(ContractError::InvalidExpression(
                    "Logical operators require boolean arguments".to_string()
                ))
            }
        }
        
        /// Convert logical not
        fn convert_not(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("Not operator expects 1 argument, got {}", args.len())
                ));
            }
            
            let arg = self.convert_node(args[0])?;
            
            match arg {
                Z3Expr::Bool(b) => Ok(Z3Expr::Bool(b.not())),
                _ => Err(ContractError::InvalidExpression(
                    "Not operator requires boolean argument".to_string()
                ))
            }
        }
        
        /// Convert modulo operation
        fn convert_modulo(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 2 {
                return Err(ContractError::InvalidExpression(
                    format!("Modulo operator expects 2 arguments, got {}", args.len())
                ));
            }
            
            let left = self.convert_node(args[0])?;
            let right = self.convert_node(args[1])?;
            
            match (left, right) {
                (Z3Expr::Int(l), Z3Expr::Int(r)) => Ok(Z3Expr::Int(l.rem(&r))),
                _ => Err(ContractError::InvalidExpression(
                    "Modulo operator requires integer arguments".to_string()
                ))
            }
        }
        
        /// Convert absolute value
        fn convert_abs(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("Abs function expects 1 argument, got {}", args.len())
                ));
            }
            
            let arg = self.convert_node(args[0])?;
            
            match arg {
                Z3Expr::Int(n) => {
                    let zero = Int::from_i64(self.context, 0);
                    let neg_n = -n.clone();
                    let cond = n.ge(&zero);
                    Ok(Z3Expr::Int(cond.ite(&n, &neg_n)))
                }
                Z3Expr::Real(r) => {
                    let zero = Real::from_real(self.context, 0, 1);
                    let neg_r = -r.clone();
                    let cond = r.ge(&zero);
                    Ok(Z3Expr::Real(cond.ite(&r, &neg_r)))
                }
                _ => Err(ContractError::InvalidExpression(
                    "Abs function requires numeric argument".to_string()
                ))
            }
        }
        
        /// Convert min/max functions
        fn convert_min_max(&self, args: &[NodeId], is_min: bool) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 2 {
                return Err(ContractError::InvalidExpression(
                    format!("{} function expects 2 arguments, got {}", 
                        if is_min { "Min" } else { "Max" }, args.len())
                ));
            }
            
            let left = self.convert_node(args[0])?;
            let right = self.convert_node(args[1])?;
            
            match (left, right) {
                (Z3Expr::Int(l), Z3Expr::Int(r)) => {
                    let cond = if is_min { l.le(&r) } else { l.ge(&r) };
                    Ok(Z3Expr::Int(cond.ite(&l, &r)))
                }
                _ => Err(ContractError::InvalidExpression(
                    format!("{} function requires integer arguments", 
                        if is_min { "Min" } else { "Max" })
                ))
            }
        }
        
        /// Convert not-equal comparison
        fn convert_not_equal(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 2 {
                return Err(ContractError::InvalidExpression(
                    format!("Not-equal operator expects 2 arguments, got {}", args.len())
                ));
            }
            
            let left = self.convert_node(args[0])?;
            let right = self.convert_node(args[1])?;
            
            match (left, right) {
                (Z3Expr::Int(l), Z3Expr::Int(r)) => Ok(Z3Expr::Bool(l._eq(&r).not())),
                (Z3Expr::Bool(l), Z3Expr::Bool(r)) => Ok(Z3Expr::Bool(l._eq(&r).not())),
                _ => Err(ContractError::InvalidExpression(
                    "Not-equal operator requires matching argument types".to_string()
                ))
            }
        }
        
        /// Convert zero predicate
        fn convert_zero_pred(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("zero? predicate expects 1 argument, got {}", args.len())
                ));
            }
            
            let arg = self.convert_node(args[0])?;
            
            match arg {
                Z3Expr::Int(n) => {
                    let zero = Int::from_i64(self.context, 0);
                    Ok(Z3Expr::Bool(n._eq(&zero)))
                }
                _ => Err(ContractError::InvalidExpression(
                    "zero? predicate requires integer argument".to_string()
                ))
            }
        }
        
        /// Convert positive predicate
        fn convert_positive_pred(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("positive? predicate expects 1 argument, got {}", args.len())
                ));
            }
            
            let arg = self.convert_node(args[0])?;
            
            match arg {
                Z3Expr::Int(n) => {
                    let zero = Int::from_i64(self.context, 0);
                    Ok(Z3Expr::Bool(n.gt(&zero)))
                }
                _ => Err(ContractError::InvalidExpression(
                    "positive? predicate requires integer argument".to_string()
                ))
            }
        }
        
        /// Convert negative predicate
        fn convert_negative_pred(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("negative? predicate expects 1 argument, got {}", args.len())
                ));
            }
            
            let arg = self.convert_node(args[0])?;
            
            match arg {
                Z3Expr::Int(n) => {
                    let zero = Int::from_i64(self.context, 0);
                    Ok(Z3Expr::Bool(n.lt(&zero)))
                }
                _ => Err(ContractError::InvalidExpression(
                    "negative? predicate requires integer argument".to_string()
                ))
            }
        }
        
        /// Convert even predicate
        fn convert_even_pred(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("even? predicate expects 1 argument, got {}", args.len())
                ));
            }
            
            let arg = self.convert_node(args[0])?;
            
            match arg {
                Z3Expr::Int(n) => {
                    let two = Int::from_i64(self.context, 2);
                    let zero = Int::from_i64(self.context, 0);
                    let remainder = n.rem(&two);
                    Ok(Z3Expr::Bool(remainder._eq(&zero)))
                }
                _ => Err(ContractError::InvalidExpression(
                    "even? predicate requires integer argument".to_string()
                ))
            }
        }
        
        /// Convert odd predicate
        fn convert_odd_pred(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("odd? predicate expects 1 argument, got {}", args.len())
                ));
            }
            
            let arg = self.convert_node(args[0])?;
            
            match arg {
                Z3Expr::Int(n) => {
                    let two = Int::from_i64(self.context, 2);
                    let zero = Int::from_i64(self.context, 0);
                    let remainder = n.rem(&two);
                    Ok(Z3Expr::Bool(remainder._eq(&zero).not()))
                }
                _ => Err(ContractError::InvalidExpression(
                    "odd? predicate requires integer argument".to_string()
                ))
            }
        }
        
        /// Convert old() function (placeholder - needs proper implementation)
        fn convert_old(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("old() function expects 1 argument, got {}", args.len())
                ));
            }
            
            // For now, we'll just convert the argument normally
            // In a full implementation, this would look up the pre-state value
            self.convert_node(args[0])
        }
        
        /// Convert length function (simplified - returns symbolic length)
        fn convert_length(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("length function expects 1 argument, got {}", args.len())
                ));
            }
            
            // For now, we'll create a symbolic integer representing the length
            // In a full implementation, this would track array/list lengths
            let length_var = Int::new_const(self.context, "list_length");
            let zero = Int::from_i64(self.context, 0);
            
            // Add constraint that length >= 0
            // This would be asserted in the solver
            Ok(Z3Expr::Int(length_var))
        }
        
        /// Convert nth/list-ref function (simplified)
        fn convert_nth(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 2 {
                return Err(ContractError::InvalidExpression(
                    format!("nth function expects 2 arguments, got {}", args.len())
                ));
            }
            
            // For now, return a symbolic value
            // In a full implementation, this would use Z3 array theory
            Ok(Z3Expr::Int(Int::new_const(self.context, "list_element")))
        }
        
        /// Convert member predicate (simplified)
        fn convert_member(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 2 {
                return Err(ContractError::InvalidExpression(
                    format!("member? predicate expects 2 arguments, got {}", args.len())
                ));
            }
            
            // For now, return a symbolic boolean
            // In a full implementation, this would use quantifiers
            Ok(Z3Expr::Bool(Bool::new_const(self.context, "is_member")))
        }
        
        /// Convert null/empty predicate
        fn convert_null_pred(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 1 {
                return Err(ContractError::InvalidExpression(
                    format!("null? predicate expects 1 argument, got {}", args.len())
                ));
            }
            
            // For now, check if length is zero
            // In a full implementation, this would check the actual list
            let length = self.convert_length(args)?;
            match length {
                Z3Expr::Int(len) => {
                    let zero = Int::from_i64(self.context, 0);
                    Ok(Z3Expr::Bool(len._eq(&zero)))
                }
                _ => Err(ContractError::InvalidExpression(
                    "Invalid list for null? check".to_string()
                ))
            }
        }
        
        /// Convert forall quantifier
        fn convert_forall(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            self.convert_quantifier(args, Quantifier::ForAll)
        }
        
        /// Convert exists quantifier
        fn convert_exists(&self, args: &[NodeId]) -> ContractResult<Z3Expr<'ctx>> {
            self.convert_quantifier(args, Quantifier::Exists)
        }
        
        /// Convert quantified expression to Z3
        fn convert_quantifier(&self, args: &[NodeId], quantifier_type: Quantifier) -> ContractResult<Z3Expr<'ctx>> {
            if args.len() != 2 {
                return Err(ContractError::InvalidExpression(
                    format!("Quantifier expects 2 arguments (bindings and body), got {}", args.len())
                ));
            }
            
            // Parse the quantifier structure
            let parser = QuantifierParser::new(self.graph);
            
            // We need to manually construct the quantified expression since we already have it parsed
            let bindings_node = self.graph.get_node(args[0])
                .ok_or_else(|| ContractError::InvalidExpression("Bindings not found".to_string()))?;
            
            let body_node_id = args[1];
            
            // Parse bindings
            let bound_vars = match bindings_node {
                Node::List(bindings) => {
                    let mut vars = Vec::new();
                    for binding_id in bindings {
                        let binding = self.graph.get_node(*binding_id)
                            .ok_or_else(|| ContractError::InvalidExpression("Invalid binding".to_string()))?;
                        
                        match binding {
                            Node::List(pair) if pair.len() == 2 => {
                                // Get variable name
                                let var_name = match self.graph.get_node(pair[0]) {
                                    Some(Node::Variable { name }) => name.clone(),
                                    _ => return Err(ContractError::InvalidExpression(
                                        "Expected variable name in binding".to_string()
                                    )),
                                };
                                
                                // For now, assume integer domain
                                vars.push((var_name, Z3Sort::Int));
                            }
                            _ => return Err(ContractError::InvalidExpression(
                                "Invalid binding format".to_string()
                            )),
                        }
                    }
                    vars
                }
                _ => return Err(ContractError::InvalidExpression(
                    "Bindings must be a list".to_string()
                )),
            };
            
            // Create a new converter with bound variables
            let mut new_converter = Z3Converter::new(self.context, self.graph);
            
            // Copy existing variables
            new_converter.variables = self.variables.clone();
            
            // Create Z3 constants for bound variables
            let mut z3_bound_vars: Vec<z3::ast::Dynamic<'ctx>> = Vec::new();
            for (var_name, sort) in &bound_vars {
                let z3_var = match sort {
                    Z3Sort::Bool => Bool::new_const(self.context, var_name).into(),
                    Z3Sort::Int => Int::new_const(self.context, var_name).into(),
                    Z3Sort::Real => Real::new_const(self.context, var_name).into(),
                    _ => return Err(ContractError::InvalidExpression(
                        format!("Unsupported sort for quantified variable: {:?}", sort)
                    )),
                };
                z3_bound_vars.push(z3_var.clone());
                
                // Add to converter's variable map
                match sort {
                    Z3Sort::Bool => new_converter.variables.insert(
                        var_name.clone(), 
                        Z3Expr::Bool(Bool::new_const(self.context, var_name))
                    ),
                    Z3Sort::Int => new_converter.variables.insert(
                        var_name.clone(), 
                        Z3Expr::Int(Int::new_const(self.context, var_name))
                    ),
                    Z3Sort::Real => new_converter.variables.insert(
                        var_name.clone(), 
                        Z3Expr::Real(Real::new_const(self.context, var_name))
                    ),
                    _ => None,
                };
            }
            
            // Convert the body with the new converter
            let body = new_converter.convert_node(body_node_id)?;
            
            // Body must be boolean
            match body {
                Z3Expr::Bool(bool_expr) => {
                    let quantified = match quantifier_type {
                        Quantifier::ForAll => forall_const(
                            &z3_bound_vars.iter().collect::<Vec<_>>(),
                            &[],  // No patterns
                            &bool_expr
                        ),
                        Quantifier::Exists => exists_const(
                            &z3_bound_vars.iter().collect::<Vec<_>>(),
                            &[],  // No patterns  
                            &bool_expr
                        ),
                    };
                    Ok(Z3Expr::Bool(quantified))
                }
                _ => Err(ContractError::InvalidExpression(
                    "Quantifier body must be a boolean expression".to_string()
                ))
            }
        }
    }
    
    /// Z3 sort types
    #[derive(Debug, Clone, Copy)]
    pub enum Z3Sort {
        Bool,
        Int,
        Real,
        String,
        // Note: Array sorts would need to store index and element sorts
        // For now we keep it simple
    }
    
    impl<'ctx> Clone for Z3Expr<'ctx> {
        fn clone(&self) -> Self {
            match self {
                Z3Expr::Bool(b) => Z3Expr::Bool(b.clone()),
                Z3Expr::Int(i) => Z3Expr::Int(i.clone()),
                Z3Expr::Real(r) => Z3Expr::Real(r.clone()),
                Z3Expr::Array(a) => Z3Expr::Array(a.clone()),
                Z3Expr::String(s) => Z3Expr::String(s.clone()),
            }
        }
    }
    
    impl<'ctx> Z3Expr<'ctx> {
        /// Convert to boolean if possible
        pub fn as_bool(&self) -> Option<&Bool<'ctx>> {
            match self {
                Z3Expr::Bool(b) => Some(b),
                _ => None,
            }
        }
        
        /// Convert to integer if possible
        pub fn as_int(&self) -> Option<&Int<'ctx>> {
            match self {
                Z3Expr::Int(i) => Some(i),
                _ => None,
            }
        }
    }
}

#[cfg(feature = "static")]
pub use implementation::*;