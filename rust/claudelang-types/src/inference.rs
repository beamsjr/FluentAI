//! Hindley-Milner type inference implementation

use crate::{
    environment::TypeEnvironment,
    types::*,
    unification::{Substitution, Unifier},
};
use claudelang_core::ast::{Graph, Literal, Node, NodeId, Pattern};
use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;

/// Type inference engine
pub struct TypeInferencer {
    /// Type environment
    env: TypeEnvironment,
    /// Current substitution
    subst: Substitution,
    /// Unifier for type unification
    unifier: Unifier,
    /// Inferred types for nodes
    node_types: FxHashMap<NodeId, TypedValue>,
    /// Errors collected during inference
    errors: Vec<TypeError>,
}

/// Type errors that can occur during inference
#[derive(Debug, Clone, thiserror::Error)]
pub enum TypeError {
    #[error("Unbound variable: {0}")]
    UnboundVariable(String),
    
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
    },
    
    #[error("Cannot unify types: {0} and {1}")]
    UnificationFailure(String, String),
    
    #[error("Wrong number of arguments: expected {expected}, found {found}")]
    ArityMismatch {
        expected: usize,
        found: usize,
    },
    
    #[error("Invalid literal type")]
    InvalidLiteral,
    
    #[error("Pattern match failure: {0}")]
    PatternMatchFailure(String),
    
    #[error("Effect constraint violation: {0}")]
    EffectConstraintViolation(String),
}

impl TypeInferencer {
    /// Create a new type inferencer
    pub fn new() -> Self {
        Self {
            env: TypeEnvironment::new(),
            subst: Substitution::new(),
            unifier: Unifier::new(),
            node_types: FxHashMap::default(),
            errors: Vec::new(),
        }
    }

    /// Create with a specific environment
    pub fn with_env(env: TypeEnvironment) -> Self {
        Self {
            env,
            subst: Substitution::new(),
            unifier: Unifier::new(),
            node_types: FxHashMap::default(),
            errors: Vec::new(),
        }
    }

    /// Infer types for an entire graph
    pub fn infer_graph(&mut self, graph: &Graph) -> Result<FxHashMap<NodeId, TypedValue>> {
        self.node_types.clear();
        self.errors.clear();

        // Infer type of root node
        if let Some(root_id) = graph.root_id {
            self.infer_node(graph, root_id)?;
        }

        // Apply final substitution to all inferred types
        for (_, ty) in self.node_types.iter_mut() {
            *ty = self.subst.apply_type(ty);
        }

        Ok(self.node_types.clone())
    }

    /// Infer the type of a single node
    fn infer_node(&mut self, graph: &Graph, node_id: NodeId) -> Result<TypedValue> {
        // Check if already inferred
        if let Some(ty) = self.node_types.get(&node_id) {
            return Ok(ty.clone());
        }

        let node = graph.get_node(node_id)
            .ok_or_else(|| anyhow!("Invalid node ID: {:?}", node_id))?;

        let inferred_type = match node {
            Node::Literal(lit) => self.infer_literal(lit)?,
            Node::Variable { name } => self.infer_variable(name)?,
            Node::Lambda { params, body } => self.infer_lambda(graph, params, *body)?,
            Node::Application { function, args } => {
                self.infer_application(graph, *function, args)?
            }
            Node::Let { bindings, body } => self.infer_let(graph, bindings, *body)?,
            Node::Letrec { bindings, body } => self.infer_letrec(graph, bindings, *body)?,
            Node::If { condition, then_branch, else_branch } => {
                self.infer_if(graph, *condition, *then_branch, *else_branch)?
            }
            Node::List(elements) => self.infer_list(graph, elements)?,
            Node::Match { expr, branches } => self.infer_match(graph, *expr, branches)?,
            Node::Effect { effect_type, operation, args } => {
                self.infer_effect(*effect_type, operation, graph, args)?
            }
            Node::Async { body } => self.infer_async(graph, *body)?,
            Node::Await { expr } => self.infer_await(graph, *expr)?,
            Node::Spawn { expr } => self.infer_spawn(graph, *expr)?,
            Node::Channel => self.infer_channel()?,
            Node::Send { channel, value } => self.infer_send(graph, *channel, *value)?,
            Node::Receive { channel } => self.infer_receive(graph, *channel)?,
        };

        // Store the inferred type
        self.node_types.insert(node_id, inferred_type.clone());
        Ok(inferred_type)
    }

    /// Infer type of a literal
    fn infer_literal(&mut self, lit: &Literal) -> Result<TypedValue> {
        Ok(match lit {
            Literal::Integer(_) => TypedValue::primitive(PrimitiveType::int()),
            Literal::Float(_) => TypedValue::primitive(PrimitiveType::float()),
            Literal::String(_) => TypedValue::primitive(PrimitiveType::string()),
            Literal::Boolean(_) => TypedValue::primitive(PrimitiveType::bool()),
            Literal::Nil => TypedValue::primitive(PrimitiveType::unit()),
        })
    }

    /// Infer type of a variable
    fn infer_variable(&mut self, name: &str) -> Result<TypedValue> {
        // Check for built-in functions first
        if let Some(builtin_type) = self.get_builtin_type(name) {
            return Ok(builtin_type);
        }

        // Look up in environment
        if let Some(ty) = self.env.lookup(name) {
            // Instantiate type scheme (handle polymorphism)
            Ok(self.instantiate(ty.clone()))
        } else {
            self.errors.push(TypeError::UnboundVariable(name.to_string()));
            Err(anyhow!("Unbound variable: {}", name))
        }
    }

    /// Infer type of a lambda
    fn infer_lambda(
        &mut self,
        graph: &Graph,
        params: &[String],
        body: NodeId,
    ) -> Result<TypedValue> {
        // Create fresh type variables for parameters
        let mut param_types = Vec::new();
        
        self.env.push_scope();
        
        for param in params {
            let param_type = self.env.fresh_type("t");
            param_types.push(param_type.clone());
            self.env.bind(param, param_type);
        }

        // Infer body type
        let body_type = self.infer_node(graph, body)?;
        
        self.env.pop_scope();

        // Create function type
        Ok(TypedValue::function(FunctionType::new(param_types, body_type)))
    }

    /// Infer type of function application
    fn infer_application(
        &mut self,
        graph: &Graph,
        function: NodeId,
        args: &[NodeId],
    ) -> Result<TypedValue> {
        // Special case for list constructor
        if let Some(node) = graph.get_node(function) {
            if let Node::Variable { name } = node {
                if name == "list" {
                    return self.infer_list(graph, args);
                }
            }
        }
        
        // Infer function type
        let func_type = self.infer_node(graph, function)?;

        // Infer argument types
        let mut arg_types = Vec::new();
        for &arg in args {
            arg_types.push(self.infer_node(graph, arg)?);
        }

        // Create fresh result type
        let result_type = self.env.fresh_type("r");

        // Create expected function type
        let expected_func_type = TypedValue::function(
            FunctionType::new(arg_types.clone(), result_type.clone())
        );

        // Unify with actual function type
        match self.unifier.unify(&func_type, &expected_func_type) {
            Ok(new_subst) => {
                self.subst.compose(&new_subst);
                let mut result = self.subst.apply_type(&result_type);
                // Preserve effects from the function
                result.effects.extend(func_type.effects.iter().copied());
                Ok(result)
            }
            Err(e) => {
                self.errors.push(TypeError::UnificationFailure(
                    func_type.to_string(),
                    expected_func_type.to_string(),
                ));
                Err(anyhow!("Type unification failed: {}", e))
            }
        }
    }

    /// Infer type of let expression
    fn infer_let(
        &mut self,
        graph: &Graph,
        bindings: &[(String, NodeId)],
        body: NodeId,
    ) -> Result<TypedValue> {
        self.env.push_scope();

        // Process bindings
        for (name, value_id) in bindings {
            let value_type = self.infer_node(graph, *value_id)?;
            // Generalize the type (let-polymorphism)
            let generalized = self.generalize(&value_type);
            self.env.bind(name, generalized);
        }

        // Infer body type
        let body_type = self.infer_node(graph, body)?;
        
        self.env.pop_scope();
        
        Ok(body_type)
    }

    /// Infer type of letrec expression
    fn infer_letrec(
        &mut self,
        graph: &Graph,
        bindings: &[(String, NodeId)],
        body: NodeId,
    ) -> Result<TypedValue> {
        self.env.push_scope();

        // First pass: bind names to fresh type variables
        let mut binding_vars = Vec::new();
        for (name, _) in bindings {
            let fresh_var = self.env.fresh_type("rec");
            binding_vars.push(fresh_var.clone());
            self.env.bind(name, fresh_var);
        }

        // Second pass: infer types and unify with variables
        for ((_, value_id), var_type) in bindings.iter().zip(&binding_vars) {
            let value_type = self.infer_node(graph, *value_id)?;
            match self.unifier.unify(var_type, &value_type) {
                Ok(new_subst) => self.subst.compose(&new_subst),
                Err(e) => {
                    self.errors.push(TypeError::UnificationFailure(
                        var_type.to_string(),
                        value_type.to_string(),
                    ));
                    return Err(anyhow!("Letrec type unification failed: {}", e));
                }
            }
        }

        // Infer body type
        let body_type = self.infer_node(graph, body)?;
        
        self.env.pop_scope();
        
        Ok(body_type)
    }

    /// Infer type of if expression
    fn infer_if(
        &mut self,
        graph: &Graph,
        condition: NodeId,
        then_branch: NodeId,
        else_branch: NodeId,
    ) -> Result<TypedValue> {
        // Condition must be boolean
        let cond_type = self.infer_node(graph, condition)?;
        let bool_type = TypedValue::primitive(PrimitiveType::bool());
        
        match self.unifier.unify(&cond_type, &bool_type) {
            Ok(new_subst) => self.subst.compose(&new_subst),
            Err(_) => {
                self.errors.push(TypeError::TypeMismatch {
                    expected: "Bool".to_string(),
                    found: cond_type.to_string(),
                });
            }
        }

        // Then and else branches must have same type
        let then_type = self.infer_node(graph, then_branch)?;
        let else_type = self.infer_node(graph, else_branch)?;

        match self.unifier.unify(&then_type, &else_type) {
            Ok(new_subst) => {
                self.subst.compose(&new_subst);
                Ok(self.subst.apply_type(&then_type))
            }
            Err(e) => {
                self.errors.push(TypeError::TypeMismatch {
                    expected: then_type.to_string(),
                    found: else_type.to_string(),
                });
                Err(anyhow!("If branches have different types: {}", e))
            }
        }
    }

    /// Infer type of list
    fn infer_list(&mut self, graph: &Graph, elements: &[NodeId]) -> Result<TypedValue> {
        if elements.is_empty() {
            // Empty list with fresh element type
            let elem_type = self.env.fresh_type("a");
            Ok(TypedValue::list(ListType::new(elem_type)))
        } else {
            // Infer first element type
            let first_type = self.infer_node(graph, elements[0])?;
            
            // All elements must have same type
            for &elem_id in &elements[1..] {
                let elem_type = self.infer_node(graph, elem_id)?;
                match self.unifier.unify(&first_type, &elem_type) {
                    Ok(new_subst) => self.subst.compose(&new_subst),
                    Err(_) => {
                        self.errors.push(TypeError::TypeMismatch {
                            expected: first_type.to_string(),
                            found: elem_type.to_string(),
                        });
                    }
                }
            }
            
            Ok(TypedValue::list(ListType::new(self.subst.apply_type(&first_type))))
        }
    }

    /// Infer type of pattern match
    fn infer_match(
        &mut self,
        graph: &Graph,
        expr: NodeId,
        branches: &[(Pattern, NodeId)],
    ) -> Result<TypedValue> {
        // Infer type of matched expression
        let expr_type = self.infer_node(graph, expr)?;
        
        // Create fresh result type
        let result_type = self.env.fresh_type("match_result");
        
        // Check each branch
        for (pattern, body_id) in branches {
            self.env.push_scope();
            
            // Pattern matching adds bindings
            self.check_pattern(pattern, &expr_type)?;
            
            // Infer body type
            let body_type = self.infer_node(graph, *body_id)?;
            
            // Unify with result type
            match self.unifier.unify(&result_type, &body_type) {
                Ok(new_subst) => self.subst.compose(&new_subst),
                Err(_) => {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: result_type.to_string(),
                        found: body_type.to_string(),
                    });
                }
            }
            
            self.env.pop_scope();
        }
        
        Ok(self.subst.apply_type(&result_type))
    }

    /// Check pattern and add bindings
    fn check_pattern(&mut self, pattern: &Pattern, expected_type: &TypedValue) -> Result<()> {
        match pattern {
            Pattern::Variable(name) => {
                self.env.bind(name, expected_type.clone());
                Ok(())
            }
            Pattern::Literal(lit) => {
                let lit_type = self.infer_literal(lit)?;
                match self.unifier.unify(&lit_type, expected_type) {
                    Ok(new_subst) => {
                        self.subst.compose(&new_subst);
                        Ok(())
                    }
                    Err(_) => {
                        self.errors.push(TypeError::PatternMatchFailure(
                            format!("Pattern type {} doesn't match expected {}", 
                                    lit_type, expected_type)
                        ));
                        Err(anyhow!("Pattern type mismatch"))
                    }
                }
            }
            Pattern::Constructor { name: _, patterns: _ } => {
                // TODO: Implement constructor pattern matching
                self.errors.push(TypeError::PatternMatchFailure(
                    "Constructor patterns not yet implemented".to_string()
                ));
                Err(anyhow!("Constructor patterns not yet implemented"))
            }
            Pattern::Wildcard => Ok(()),
        }
    }

    /// Infer type of effect expression
    fn infer_effect(
        &mut self,
        effect_type: claudelang_core::ast::EffectType,
        operation: &str,
        graph: &Graph,
        args: &[NodeId],
    ) -> Result<TypedValue> {
        // Infer argument types
        let mut arg_types = Vec::new();
        for &arg in args {
            arg_types.push(self.infer_node(graph, arg)?);
        }

        // Get effect handler type
        let effect_ty = self.get_effect_type(effect_type, operation, &arg_types)?;
        
        // Add effect to the result
        Ok(effect_ty.add_effect(effect_type))
    }

    /// Infer type of async expression
    fn infer_async(&mut self, graph: &Graph, body: NodeId) -> Result<TypedValue> {
        let body_type = self.infer_node(graph, body)?;
        Ok(body_type.add_effect(claudelang_core::ast::EffectType::Async))
    }

    /// Infer type of await expression
    fn infer_await(&mut self, graph: &Graph, expr: NodeId) -> Result<TypedValue> {
        let expr_type = self.infer_node(graph, expr)?;
        
        // Remove async effect if present
        let mut result = expr_type.clone();
        result.effects.remove(&claudelang_core::ast::EffectType::Async);
        Ok(result)
    }

    /// Infer type of spawn expression
    fn infer_spawn(&mut self, graph: &Graph, expr: NodeId) -> Result<TypedValue> {
        let expr_type = self.infer_node(graph, expr)?;
        Ok(expr_type.add_effect(claudelang_core::ast::EffectType::Concurrent))
    }

    /// Infer type of channel creation
    fn infer_channel(&mut self) -> Result<TypedValue> {
        // Channel[T] where T is a fresh type variable
        let elem_type = self.env.fresh_type("chan");
        let channel_type = TypedValue::variant(
            VariantType::new()
                .with_variant("Channel", Some(elem_type))
        );
        Ok(channel_type.add_effect(claudelang_core::ast::EffectType::Concurrent))
    }

    /// Infer type of send operation
    fn infer_send(&mut self, graph: &Graph, channel: NodeId, value: NodeId) -> Result<TypedValue> {
        let _channel_type = self.infer_node(graph, channel)?;
        let _value_type = self.infer_node(graph, value)?;
        
        // TODO: Validate channel and value types match
        
        Ok(TypedValue::primitive(PrimitiveType::unit())
            .add_effect(claudelang_core::ast::EffectType::Concurrent))
    }

    /// Infer type of receive operation
    fn infer_receive(&mut self, graph: &Graph, channel: NodeId) -> Result<TypedValue> {
        let _channel_type = self.infer_node(graph, channel)?;
        
        // Extract element type from channel
        // TODO: Proper channel type extraction
        let elem_type = self.env.fresh_type("recv");
        
        Ok(elem_type.add_effect(claudelang_core::ast::EffectType::Concurrent))
    }

    /// Get built-in function type
    fn get_builtin_type(&mut self, name: &str) -> Option<TypedValue> {
        use PrimitiveType as P;
        
        let int = || TypedValue::primitive(P::int());
        let float = || TypedValue::primitive(P::float());
        let bool = || TypedValue::primitive(P::bool());
        let string = || TypedValue::primitive(P::string());
        let unit = || TypedValue::primitive(P::unit());
        
        let binary_int = || TypedValue::function(FunctionType::new(
            vec![int(), int()],
            int()
        ));
        
        let binary_float = || TypedValue::function(FunctionType::new(
            vec![float(), float()],
            float()
        ));
        
        let comparison = || TypedValue::function(FunctionType::new(
            vec![int(), int()],
            bool()
        ));
        
        match name {
            // Arithmetic
            "+" => Some(binary_int()),
            "-" => Some(binary_int()),
            "*" => Some(binary_int()),
            "/" => Some(binary_int()),
            "mod" => Some(binary_int()),
            
            // Float arithmetic
            "+." => Some(binary_float()),
            "-." => Some(binary_float()),
            "*." => Some(binary_float()),
            "/." => Some(binary_float()),
            
            // Comparison
            "<" => Some(comparison()),
            ">" => Some(comparison()),
            "<=" => Some(comparison()),
            ">=" => Some(comparison()),
            "=" | "==" => Some(comparison()),
            "!=" | "<>" => Some(comparison()),
            
            // Boolean
            "and" => Some(TypedValue::function(FunctionType::new(
                vec![bool(), bool()],
                bool()
            ))),
            "or" => Some(TypedValue::function(FunctionType::new(
                vec![bool(), bool()],
                bool()
            ))),
            "not" => Some(TypedValue::function(FunctionType::new(
                vec![bool()],
                bool()
            ))),
            
            // List operations
            "car" | "head" | "first" => {
                let a = self.env.fresh_type("a");
                Some(TypedValue::function(FunctionType::new(
                    vec![TypedValue::list(ListType::new(a.clone()))],
                    a
                )))
            }
            "cdr" | "tail" | "rest" => {
                let a = self.env.fresh_type("a");
                let list_a = TypedValue::list(ListType::new(a));
                Some(TypedValue::function(FunctionType::new(
                    vec![list_a.clone()],
                    list_a
                )))
            }
            "cons" => {
                let a = self.env.fresh_type("a");
                let list_a = TypedValue::list(ListType::new(a.clone()));
                Some(TypedValue::function(FunctionType::new(
                    vec![a, list_a.clone()],
                    list_a
                )))
            }
            "list-len" | "length" => {
                let a = self.env.fresh_type("a");
                Some(TypedValue::function(FunctionType::new(
                    vec![TypedValue::list(ListType::new(a))],
                    int()
                )))
            }
            "list-empty?" | "empty?" => {
                let a = self.env.fresh_type("a");
                Some(TypedValue::function(FunctionType::new(
                    vec![TypedValue::list(ListType::new(a))],
                    bool()
                )))
            }
            
            // String operations
            "str-len" | "string-length" => Some(TypedValue::function(FunctionType::new(
                vec![string()],
                int()
            ))),
            "str-concat" | "string-append" => Some(TypedValue::function(FunctionType::new(
                vec![string(), string()],
                string()
            ))),
            "str-upper" | "string-upcase" => Some(TypedValue::function(FunctionType::new(
                vec![string()],
                string()
            ))),
            "str-lower" | "string-downcase" => Some(TypedValue::function(FunctionType::new(
                vec![string()],
                string()
            ))),
            
            // IO operations
            "print" => Some(TypedValue::function(FunctionType::new(
                vec![self.env.fresh_type("a")],
                unit()
            )).add_effect(claudelang_core::ast::EffectType::IO)),
            
            _ => None,
        }
    }

    /// Get effect handler type
    fn get_effect_type(
        &mut self,
        effect: claudelang_core::ast::EffectType,
        operation: &str,
        _arg_types: &[TypedValue],
    ) -> Result<TypedValue> {
        use PrimitiveType as P;
        
        match (effect, operation) {
            (claudelang_core::ast::EffectType::IO, "print") => {
                Ok(TypedValue::primitive(P::unit()))
            }
            (claudelang_core::ast::EffectType::IO, "read") => {
                Ok(TypedValue::primitive(P::string()))
            }
            _ => {
                let result = self.env.fresh_type("effect");
                Ok(result)
            }
        }
    }

    /// Generalize a type (for let-polymorphism)
    fn generalize(&self, ty: &TypedValue) -> TypedValue {
        // For now, just return the type as-is
        // Full implementation would find free type variables
        // and create a type scheme
        ty.clone()
    }

    /// Instantiate a type scheme
    fn instantiate(&mut self, ty: TypedValue) -> TypedValue {
        // For now, just return the type as-is
        // Full implementation would replace bound type variables
        // with fresh ones
        ty
    }

    /// Get collected errors
    pub fn errors(&self) -> &[TypeError] {
        &self.errors
    }

    /// Check if inference succeeded without errors
    pub fn is_success(&self) -> bool {
        self.errors.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use claudelang_parser::parse;

    fn infer_code(code: &str) -> Result<TypedValue> {
        let graph = parse(code)?;
        let mut inferencer = TypeInferencer::new();
        let types = inferencer.infer_graph(&graph)?;
        
        if let Some(root_id) = graph.root_id {
            Ok(types.get(&root_id).unwrap().clone())
        } else {
            Err(anyhow!("No root node"))
        }
    }

    #[test]
    fn test_literal_inference() {
        assert_eq!(infer_code("42").unwrap().to_string(), "Int");
        assert_eq!(infer_code("3.14").unwrap().to_string(), "Float");
        assert_eq!(infer_code("\"hello\"").unwrap().to_string(), "String");
        assert_eq!(infer_code("#t").unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_arithmetic_inference() {
        assert_eq!(infer_code("(+ 1 2)").unwrap().to_string(), "Int");
        assert_eq!(infer_code("(* 3 4)").unwrap().to_string(), "Int");
    }

    #[test]
    fn test_comparison_inference() {
        assert_eq!(infer_code("(< 1 2)").unwrap().to_string(), "Bool");
        assert_eq!(infer_code("(= 5 5)").unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_if_inference() {
        assert_eq!(infer_code("(if #t 1 2)").unwrap().to_string(), "Int");
        assert_eq!(
            infer_code("(if (< 1 2) \"yes\" \"no\")").unwrap().to_string(),
            "String"
        );
    }

    #[test]
    fn test_let_inference() {
        assert_eq!(
            infer_code("(let ((x 42)) x)").unwrap().to_string(),
            "Int"
        );
        assert_eq!(
            infer_code("(let ((x 42) (y 3)) (+ x y))").unwrap().to_string(),
            "Int"
        );
    }

    #[test]
    fn test_lambda_inference() {
        let result = infer_code("(lambda (x) x)").unwrap();
        assert!(result.to_string().contains("→"));
        
        assert_eq!(
            infer_code("((lambda (x) (+ x 1)) 5)").unwrap().to_string(),
            "Int"
        );
    }

    #[test]
    fn test_list_inference() {
        let result = infer_code("(list 1 2 3)").unwrap();
        assert_eq!(result.to_string(), "[Int]");
        
        let result = infer_code("(car (list 1 2 3))").unwrap();
        assert_eq!(result.to_string(), "Int");
    }
}