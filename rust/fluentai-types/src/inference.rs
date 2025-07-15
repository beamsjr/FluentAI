//! Hindley-Milner type inference implementation

use crate::{
    environment::TypeEnvironment,
    types::*,
    unification::{Substitution, Unifier},
};
use anyhow::{anyhow, Result};
use fluentai_core::ast::{Graph, Literal, Node, NodeId, Pattern};
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
    /// Variable is not bound in the current scope
    #[error("Unbound variable: {0}")]
    UnboundVariable(String),

    /// Type mismatch between expected and found types
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        /// The expected type
        expected: String,
        /// The type that was found
        found: String,
    },

    /// Type unification failed between two types
    #[error("Cannot unify types: {0} and {1}")]
    UnificationFailure(String, String),

    /// Wrong number of arguments provided to a function
    #[error("Wrong number of arguments: expected {expected}, found {found}")]
    ArityMismatch {
        /// The expected number of arguments
        expected: usize,
        /// The actual number of arguments found
        found: usize,
    },

    /// Invalid literal type encountered
    #[error("Invalid literal type")]
    InvalidLiteral,

    /// Pattern matching failed
    #[error("Pattern match failure: {0}")]
    PatternMatchFailure(String),

    /// Effect constraint was violated
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

        let node = graph
            .get_node(node_id)
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
            Node::If {
                condition,
                then_branch,
                else_branch,
            } => self.infer_if(graph, *condition, *then_branch, *else_branch)?,
            Node::List(elements) => self.infer_list(graph, elements)?,
            Node::Match { expr, branches } => self.infer_match(graph, *expr, branches)?,
            Node::Effect {
                effect_type,
                operation,
                args,
            } => self.infer_effect(*effect_type, operation, graph, args)?,
            Node::Async { body } => self.infer_async(graph, *body)?,
            Node::Await { expr } => self.infer_await(graph, *expr)?,
            Node::Spawn { expr } => self.infer_spawn(graph, *expr)?,
            Node::Channel { .. } => self.infer_channel()?,
            Node::Send { channel, value } => self.infer_send(graph, *channel, *value)?,
            Node::Receive { channel } => self.infer_receive(graph, *channel)?,
            Node::TrySend { channel, value } => self.infer_try_send(graph, *channel, *value)?,
            Node::TryReceive { channel } => self.infer_try_receive(graph, *channel)?,
            Node::Select { branches, default } => self.infer_select(graph, branches, default.as_ref())?,
            // Module-related nodes - for now, return unit type
            Node::Module { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::Import { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::Export { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::QualifiedVariable { .. } => {
                // TODO: Implement proper module type inference
                self.env.fresh_type("T")
            }
            Node::Contract { .. } => {
                // Contracts are metadata, not runtime values
                TypedValue::primitive(PrimitiveType::unit())
            }
            Node::Handler { handlers: _, body } => {
                // Handler has the same type as its body expression
                self.infer_node(graph, *body)?
            }
            Node::Define { name: _, value } => {
                // Define creates a binding and returns unit
                self.infer_node(graph, *value)?;
                TypedValue::primitive(PrimitiveType::unit())
            }
            Node::Begin { exprs } => {
                // Begin evaluates all expressions and returns the type of the last one
                if exprs.is_empty() {
                    TypedValue::primitive(PrimitiveType::unit())
                } else {
                    // Infer types for all expressions (for side effects/error checking)
                    for expr in &exprs[..exprs.len() - 1] {
                        self.infer_node(graph, *expr)?;
                    }
                    // Return the type of the last expression
                    self.infer_node(graph, exprs[exprs.len() - 1])?
                }
            }
            Node::Actor { .. } => {
                // Actor type - for now just a variant with no payload
                TypedValue::variant(VariantType::new().with_variant("Actor", None))
            }
            Node::ActorSend { .. } => TypedValue::primitive(PrimitiveType::unit()), // Send returns nil
            Node::ActorReceive { .. } => {
                // Depends on message pattern - for now return a type variable
                let var = self.env.fresh_type("T");
                var
            }
            Node::Become { .. } => TypedValue::primitive(PrimitiveType::unit()), // Become returns nil
            Node::Try { body, .. } => {
                // Try returns the type of its body or catch handler
                self.infer_node(graph, *body)?
            }
            Node::Throw { .. } => {
                // Throw doesn't return normally
                TypedValue::primitive(PrimitiveType::unit())
            }
            Node::Promise { body } => {
                // Promise wraps the body type
                let body_type = self.infer_node(graph, *body)?;
                TypedValue::variant(VariantType::new().with_variant("Promise", Some(body_type)))
            }
            Node::PromiseAll { promises } => {
                // Returns a list of all promise results
                if promises.is_empty() {
                    TypedValue::list(ListType::new(TypedValue::primitive(PrimitiveType::unit())))
                } else {
                    // For simplicity, assume all promises have the same type
                    let first_type = self.infer_node(graph, promises[0])?;
                    TypedValue::list(ListType::new(first_type))
                }
            }
            Node::PromiseRace { promises } => {
                // Returns the type of the first promise to resolve
                if promises.is_empty() {
                    TypedValue::primitive(PrimitiveType::unit())
                } else {
                    self.infer_node(graph, promises[0])?
                }
            }
            Node::Timeout { promise, default, .. } => {
                // Returns promise result or default type
                let promise_type = self.infer_node(graph, *promise)?;
                if let Some(def) = default {
                    let default_type = self.infer_node(graph, *def)?;
                    // Should unify types, but for now just return promise type
                    promise_type
                } else {
                    promise_type
                }
            }
            Node::Assignment { target: _, value } => {
                // Assignment returns unit type
                self.infer_node(graph, *value)?;
                TypedValue::primitive(PrimitiveType::unit())
            }
            // Continuum UI nodes - these will be compiled away
            Node::Surface { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::Space { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::Element { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::StateField { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::When { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::Disturb { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::Extern { .. } => TypedValue::primitive(PrimitiveType::unit()),
            Node::Map(pairs) => self.infer_map(graph, pairs)?,
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
            Literal::Symbol(_) => TypedValue::primitive(PrimitiveType::symbol()),
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
            self.errors
                .push(TypeError::UnboundVariable(name.to_string()));
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
        Ok(TypedValue::function(FunctionType::new(
            param_types,
            body_type,
        )))
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
        let expected_func_type =
            TypedValue::function(FunctionType::new(arg_types.clone(), result_type.clone()));

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

            Ok(TypedValue::list(ListType::new(
                self.subst.apply_type(&first_type),
            )))
        }
    }

    /// Infer type of map/dictionary
    fn infer_map(&mut self, graph: &Graph, pairs: &[(NodeId, NodeId)]) -> Result<TypedValue> {
        // For now, treat maps as records with string keys
        // In the future, we could have a proper Map type
        let mut fields = FxHashMap::default();
        
        for &(key_id, value_id) in pairs {
            // Get the key - should be a string literal for record syntax
            if let Some(Node::Literal(Literal::String(key))) = graph.get_node(key_id) {
                let value_type = self.infer_node(graph, value_id)?;
                fields.insert(key.clone(), value_type);
            } else {
                // For dynamic keys, we'd need a different approach
                // For now, just use a generic record type
                let value_type = if pairs.is_empty() {
                    self.env.fresh_type("v")
                } else {
                    self.infer_node(graph, pairs[0].1)?
                };
                
                // Ensure all values have the same type for dynamic maps
                for &(_, value_id) in &pairs[1..] {
                    let v_type = self.infer_node(graph, value_id)?;
                    match self.unifier.unify(&value_type, &v_type) {
                        Ok(new_subst) => self.subst.compose(&new_subst),
                        Err(_) => {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: value_type.to_string(),
                                found: v_type.to_string(),
                            });
                        }
                    }
                }
                
                // Return a generic "map-like" record type
                let mut record = RecordType::new();
                record.fields = fields;
                return Ok(TypedValue::record(record));
            }
        }
        
        let mut record = RecordType::new();
        record.fields = fields;
        Ok(TypedValue::record(record))
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
                        self.errors.push(TypeError::PatternMatchFailure(format!(
                            "Pattern type {} doesn't match expected {}",
                            lit_type, expected_type
                        )));
                        Err(anyhow!("Pattern type mismatch"))
                    }
                }
            }
            Pattern::Constructor {
                name: _,
                patterns: _,
            } => {
                // TODO: Implement constructor pattern matching
                self.errors.push(TypeError::PatternMatchFailure(
                    "Constructor patterns not yet implemented".to_string(),
                ));
                Err(anyhow!("Constructor patterns not yet implemented"))
            }
            Pattern::Wildcard => Ok(()),
            Pattern::Guard {
                pattern,
                condition: _,
            } => {
                // For guard patterns, check the inner pattern
                // The condition will be checked separately during evaluation
                self.check_pattern(pattern, expected_type)
            }
            Pattern::As { binding, pattern } => {
                // For as-patterns, bind the name and check the inner pattern
                self.env.bind(binding, expected_type.clone());
                self.check_pattern(pattern, expected_type)
            }
            Pattern::Or(patterns) => {
                // For or-patterns, check that all alternatives have the same type
                for p in patterns {
                    self.check_pattern(p, expected_type)?;
                }
                Ok(())
            }
            Pattern::Range(_) => {
                // Range patterns only work with numeric types
                match &expected_type.inner {
                    TypedValueInner::Primitive(prim) => match prim.name.as_str() {
                        "Int" | "Float" => Ok(()),
                        _ => {
                            self.errors.push(TypeError::PatternMatchFailure(format!(
                                "Range patterns can only match numeric types, not {}",
                                expected_type
                            )));
                            Err(anyhow!("Range pattern type mismatch"))
                        }
                    },
                    _ => {
                        self.errors.push(TypeError::PatternMatchFailure(format!(
                            "Range patterns can only match numeric types, not {}",
                            expected_type
                        )));
                        Err(anyhow!("Range pattern type mismatch"))
                    }
                }
            }
            Pattern::View {
                function: _,
                pattern,
            } => {
                // For view patterns, we would need to infer the function type
                // and check that the pattern matches the function's return type
                // For now, just check the inner pattern
                self.check_pattern(pattern, expected_type)
            }
        }
    }

    /// Infer type of effect expression
    fn infer_effect(
        &mut self,
        effect_type: fluentai_core::ast::EffectType,
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
        Ok(body_type.add_effect(fluentai_core::ast::EffectType::Async))
    }

    /// Infer type of await expression
    fn infer_await(&mut self, graph: &Graph, expr: NodeId) -> Result<TypedValue> {
        let expr_type = self.infer_node(graph, expr)?;

        // Remove async effect if present
        let mut result = expr_type.clone();
        result
            .effects
            .remove(&fluentai_core::ast::EffectType::Async);
        Ok(result)
    }

    /// Infer type of spawn expression
    fn infer_spawn(&mut self, graph: &Graph, expr: NodeId) -> Result<TypedValue> {
        let expr_type = self.infer_node(graph, expr)?;
        Ok(expr_type.add_effect(fluentai_core::ast::EffectType::Concurrent))
    }

    /// Infer type of channel creation
    fn infer_channel(&mut self) -> Result<TypedValue> {
        // Channel[T] where T is a fresh type variable
        let elem_type = self.env.fresh_type("chan");
        let channel_type =
            TypedValue::variant(VariantType::new().with_variant("Channel", Some(elem_type)));
        Ok(channel_type.add_effect(fluentai_core::ast::EffectType::Concurrent))
    }

    /// Infer type of send operation
    fn infer_send(&mut self, graph: &Graph, channel: NodeId, value: NodeId) -> Result<TypedValue> {
        let _channel_type = self.infer_node(graph, channel)?;
        let _value_type = self.infer_node(graph, value)?;

        // TODO: Validate channel and value types match

        Ok(TypedValue::primitive(PrimitiveType::unit())
            .add_effect(fluentai_core::ast::EffectType::Concurrent))
    }

    /// Infer type of receive operation
    fn infer_receive(&mut self, graph: &Graph, channel: NodeId) -> Result<TypedValue> {
        let _channel_type = self.infer_node(graph, channel)?;

        // Extract element type from channel
        // TODO: Proper channel type extraction
        let elem_type = self.env.fresh_type("recv");

        Ok(elem_type.add_effect(fluentai_core::ast::EffectType::Concurrent))
    }
    
    /// Infer type of try-send operation
    fn infer_try_send(&mut self, graph: &Graph, channel: NodeId, value: NodeId) -> Result<TypedValue> {
        let _channel_type = self.infer_node(graph, channel)?;
        let _value_type = self.infer_node(graph, value)?;
        
        // TODO: Verify value type matches channel element type
        // Returns a boolean indicating success/failure
        Ok(TypedValue::primitive(PrimitiveType::bool())
            .add_effect(fluentai_core::ast::EffectType::Concurrent))
    }
    
    /// Infer type of try-receive operation  
    fn infer_try_receive(&mut self, graph: &Graph, channel: NodeId) -> Result<TypedValue> {
        let _channel_type = self.infer_node(graph, channel)?;
        
        // Extract element type from channel
        // TODO: Proper channel type extraction
        let elem_type = self.env.fresh_type("recv");
        
        // Returns a list [bool, T] where bool indicates success
        let _bool_type = TypedValue::primitive(PrimitiveType::bool());
        let list_type = TypedValue::list(ListType::new(elem_type));
        
        Ok(list_type.add_effect(fluentai_core::ast::EffectType::Concurrent))
    }
    
    /// Infer type of select operation
    fn infer_select(&mut self, graph: &Graph, branches: &[(NodeId, NodeId)], default: Option<&NodeId>) -> Result<TypedValue> {
        // All handlers must have the same type
        let result_type = self.env.fresh_type("select_result");
        
        for (channel_op, handler) in branches {
            // Infer channel operation type
            let _op_type = self.infer_node(graph, *channel_op)?;
            
            // Infer handler type
            let handler_type = self.infer_node(graph, *handler)?;
            
            // Unify with result type
            match self.unifier.unify(&result_type, &handler_type) {
                Ok(new_subst) => self.subst.compose(&new_subst),
                Err(_) => {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: result_type.to_string(),
                        found: handler_type.to_string(),
                    });
                }
            }
        }
        
        // Check default branch if present
        if let Some(def) = default {
            let default_type = self.infer_node(graph, *def)?;
            match self.unifier.unify(&result_type, &default_type) {
                Ok(new_subst) => self.subst.compose(&new_subst),
                Err(_) => {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: result_type.to_string(),
                        found: default_type.to_string(),
                    });
                }
            }
        }
        
        Ok(self.subst.apply_type(&result_type).add_effect(fluentai_core::ast::EffectType::Concurrent))
    }

    /// Get built-in function type
    fn get_builtin_type(&mut self, name: &str) -> Option<TypedValue> {
        use PrimitiveType as P;

        let int = || TypedValue::primitive(P::int());
        let float = || TypedValue::primitive(P::float());
        let bool = || TypedValue::primitive(P::bool());
        let string = || TypedValue::primitive(P::string());
        let unit = || TypedValue::primitive(P::unit());

        let binary_int = || TypedValue::function(FunctionType::new(vec![int(), int()], int()));

        let binary_float =
            || TypedValue::function(FunctionType::new(vec![float(), float()], float()));

        let comparison = || TypedValue::function(FunctionType::new(vec![int(), int()], bool()));

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
                bool(),
            ))),
            "or" => Some(TypedValue::function(FunctionType::new(
                vec![bool(), bool()],
                bool(),
            ))),
            "not" => Some(TypedValue::function(FunctionType::new(
                vec![bool()],
                bool(),
            ))),

            // List operations
            "car" | "head" | "first" => {
                let a = self.env.fresh_type("a");
                Some(TypedValue::function(FunctionType::new(
                    vec![TypedValue::list(ListType::new(a.clone()))],
                    a,
                )))
            }
            "cdr" | "tail" | "rest" => {
                let a = self.env.fresh_type("a");
                let list_a = TypedValue::list(ListType::new(a));
                Some(TypedValue::function(FunctionType::new(
                    vec![list_a.clone()],
                    list_a,
                )))
            }
            "cons" => {
                let a = self.env.fresh_type("a");
                let list_a = TypedValue::list(ListType::new(a.clone()));
                Some(TypedValue::function(FunctionType::new(
                    vec![a, list_a.clone()],
                    list_a,
                )))
            }
            "list-len" | "length" => {
                let a = self.env.fresh_type("a");
                Some(TypedValue::function(FunctionType::new(
                    vec![TypedValue::list(ListType::new(a))],
                    int(),
                )))
            }
            "list-empty?" | "empty?" => {
                let a = self.env.fresh_type("a");
                Some(TypedValue::function(FunctionType::new(
                    vec![TypedValue::list(ListType::new(a))],
                    bool(),
                )))
            }

            // String operations
            "str-len" | "string-length" => Some(TypedValue::function(FunctionType::new(
                vec![string()],
                int(),
            ))),
            "str-concat" | "string-append" => Some(TypedValue::function(FunctionType::new(
                vec![string(), string()],
                string(),
            ))),
            "str-upper" | "string-upcase" => Some(TypedValue::function(FunctionType::new(
                vec![string()],
                string(),
            ))),
            "str-lower" | "string-downcase" => Some(TypedValue::function(FunctionType::new(
                vec![string()],
                string(),
            ))),

            // IO operations
            "print" => Some(
                TypedValue::function(FunctionType::new(vec![self.env.fresh_type("a")], unit()))
                    .add_effect(fluentai_core::ast::EffectType::IO),
            ),

            _ => None,
        }
    }

    /// Get effect handler type
    fn get_effect_type(
        &mut self,
        effect: fluentai_core::ast::EffectType,
        operation: &str,
        _arg_types: &[TypedValue],
    ) -> Result<TypedValue> {
        use PrimitiveType as P;

        match (effect, operation) {
            (fluentai_core::ast::EffectType::IO, "print") => Ok(TypedValue::primitive(P::unit())),
            (fluentai_core::ast::EffectType::IO, "read") => Ok(TypedValue::primitive(P::string())),
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
    use fluentai_parser::parse_flc;

    fn infer_code(code: &str) -> Result<TypedValue> {
        let graph = parse_flc(code)?;
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
        assert_eq!(infer_code("true").unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_arithmetic_inference() {
        assert_eq!(infer_code("1 + 2").unwrap().to_string(), "Int");
        assert_eq!(infer_code("3 * 4").unwrap().to_string(), "Int");
    }

    #[test]
    fn test_comparison_inference() {
        assert_eq!(infer_code("1 < 2").unwrap().to_string(), "Bool");
        assert_eq!(infer_code("5 == 5").unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_if_inference() {
        assert_eq!(infer_code("if (true) { 1 } else { 2 }").unwrap().to_string(), "Int");
        assert_eq!(
            infer_code("if (1 < 2) { \"yes\" } else { \"no\" }")
                .unwrap()
                .to_string(),
            "String"
        );
    }

    #[test]
    fn test_let_inference() {
        assert_eq!(infer_code("{ let x = 42; x }").unwrap().to_string(), "Int");
        assert_eq!(
            infer_code("{ let x = 42; let y = 3; x + y }")
                .unwrap()
                .to_string(),
            "Int"
        );
    }

    #[test]
    fn test_lambda_inference() {
        let result = infer_code("(x) => x").unwrap();
        assert!(result.to_string().contains("→"));

        assert_eq!(
            infer_code("((x) => x + 1)(5)").unwrap().to_string(),
            "Int"
        );
    }

    #[test]
    fn test_list_inference() {
        let result = infer_code("list(1, 2, 3)").unwrap();
        assert_eq!(result.to_string(), "[Int]");

        let result = infer_code("car(list(1, 2, 3))").unwrap();
        assert_eq!(result.to_string(), "Int");
    }
}
