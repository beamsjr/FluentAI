//! Runtime contract verification

use std::collections::HashMap;
use std::sync::Arc;

use claudelang_core::{
    ast::Graph,
    value::Value,
};
use tracing::{debug, trace};

use crate::{
    contract::{Contract, ContractCondition},
    errors::{ContractError, ContractResult, ContractViolation},
    evaluator::ConditionEvaluator,
    purity::PurityChecker,
};

/// Runtime contract verifier
pub struct RuntimeVerifier {
    /// Whether runtime verification is enabled
    enabled: bool,
    
    /// Contracts indexed by function name
    contracts: HashMap<String, Contract>,
    
    /// Performance mode - skip expensive checks
    performance_mode: bool,
    
    /// AST graph for evaluating contract conditions
    ast_graph: Option<Arc<Graph>>,
}

/// Context for verifying a single function call
pub struct VerificationContext {
    /// Function being verified
    pub function_name: String,
    
    /// Arguments passed to the function
    pub arguments: Vec<Value>,
    
    /// Return value (for postcondition checking)
    pub return_value: Option<Value>,
    
    /// Local bindings available during verification
    pub bindings: HashMap<String, Value>,
    
    /// Whether this is a pre or post check
    pub phase: VerificationPhase,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerificationPhase {
    /// Checking preconditions before execution
    Pre,
    /// Checking postconditions after execution
    Post,
    /// Checking invariants
    Invariant,
}

impl RuntimeVerifier {
    /// Create a new runtime verifier
    pub fn new() -> Self {
        Self {
            enabled: true,
            contracts: HashMap::new(),
            performance_mode: false,
            ast_graph: None,
        }
    }
    
    /// Set the AST graph for condition evaluation
    pub fn set_ast_graph(&mut self, graph: Arc<Graph>) {
        self.ast_graph = Some(graph);
    }
    
    /// Enable or disable runtime verification
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }
    
    /// Enable performance mode (skip expensive checks)
    pub fn set_performance_mode(&mut self, enabled: bool) {
        self.performance_mode = enabled;
    }
    
    /// Register a contract for a function
    pub fn register_contract(&mut self, contract: Contract) {
        debug!("Registering contract for function: {}", contract.function_name);
        self.contracts.insert(contract.function_name.clone(), contract);
    }
    
    /// Register a contract with purity validation
    pub fn register_contract_validated(&mut self, contract: Contract) -> ContractResult<()> {
        debug!("Registering and validating contract for function: {}", contract.function_name);
        
        // Validate purity of contract expressions if we have an AST graph
        if let Some(ref graph) = self.ast_graph {
            let mut purity_checker = PurityChecker::new(graph);
            purity_checker.validate_contract_purity(&contract)?;
        } else {
            debug!("Skipping purity validation - no AST graph available");
        }
        
        self.contracts.insert(contract.function_name.clone(), contract);
        Ok(())
    }
    
    /// Check if a function has a contract
    pub fn has_contract(&self, function_name: &str) -> bool {
        self.contracts.contains_key(function_name)
    }
    
    /// Get a contract by function name
    pub fn get_contract(&self, function_name: &str) -> Option<&Contract> {
        self.contracts.get(function_name)
    }
    
    /// Verify preconditions before function execution
    pub fn verify_preconditions(&self, ctx: &VerificationContext) -> ContractResult<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let Some(contract) = self.contracts.get(&ctx.function_name) else {
            return Ok(());
        };
        
        trace!("Verifying preconditions for {}", ctx.function_name);
        
        for condition in &contract.preconditions {
            self.verify_condition(condition, ctx, contract)?;
        }
        
        Ok(())
    }
    
    /// Verify postconditions after function execution
    pub fn verify_postconditions(&self, ctx: &VerificationContext) -> ContractResult<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let Some(contract) = self.contracts.get(&ctx.function_name) else {
            return Ok(());
        };
        
        trace!("Verifying postconditions for {}", ctx.function_name);
        
        for condition in &contract.postconditions {
            self.verify_condition(condition, ctx, contract)?;
        }
        
        Ok(())
    }
    
    /// Verify invariants during execution
    pub fn verify_invariants(&self, ctx: &VerificationContext) -> ContractResult<()> {
        if !self.enabled || self.performance_mode {
            return Ok(());
        }
        
        let Some(contract) = self.contracts.get(&ctx.function_name) else {
            return Ok(());
        };
        
        trace!("Verifying invariants for {}", ctx.function_name);
        
        for condition in &contract.invariants {
            self.verify_condition(condition, ctx, contract)?;
        }
        
        Ok(())
    }
    
    /// Verify a single condition
    fn verify_condition(
        &self,
        condition: &ContractCondition,
        ctx: &VerificationContext,
        contract: &Contract,
    ) -> ContractResult<()> {
        let graph = self.ast_graph.as_ref()
            .ok_or_else(|| ContractError::VerificationError(
                "AST graph not set for contract verification".to_string()
            ))?;
        
        // Create evaluator with context bindings
        let mut evaluator = ConditionEvaluator::new(graph);
        
        // Set up bindings from context
        for (name, value) in &ctx.bindings {
            evaluator.bind(name.clone(), value.clone());
        }
        
        // Bind function arguments
        // TODO: This assumes parameter names are available
        // In a real implementation, we'd need to get parameter names from the function definition
        for (i, arg) in ctx.arguments.iter().enumerate() {
            evaluator.bind(format!("arg{}", i), arg.clone());
        }
        
        // Bind return value for postconditions
        if let Some(ref return_value) = ctx.return_value {
            evaluator.bind("result".to_string(), return_value.clone());
        }
        
        // Evaluate the condition
        match evaluator.evaluate_condition(condition.expression) {
            Ok(true) => Ok(()),
            Ok(false) => {
                let message = condition.message.clone()
                    .unwrap_or_else(|| format!("Contract {:?} violated", condition.kind));
                
                Err(ContractError::Violation(ContractViolation::with_details(
                    condition.kind,
                    Some(contract.function_name.clone()),
                    message,
                    condition.expression,
                    condition.span,
                    condition.blame_label.clone(),
                )))
            }
            Err(e) => Err(e),
        }
    }
    
    /// Check purity constraints
    pub fn check_purity(
        &self,
        function_name: &str,
        had_side_effects: bool,
    ) -> ContractResult<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let Some(contract) = self.contracts.get(function_name) else {
            return Ok(());
        };
        
        if contract.pure && had_side_effects {
            return Err(ContractError::Violation(ContractViolation::Purity {
                function: function_name.to_string(),
                message: "Function marked as pure but had side effects".to_string(),
                node_id: contract.node_id,
                span: None,
                blame_label: None,
            }));
        }
        
        Ok(())
    }
}

impl Default for RuntimeVerifier {
    fn default() -> Self {
        Self::new()
    }
}

impl VerificationContext {
    /// Create a new verification context for precondition checking
    pub fn pre(function_name: String, arguments: Vec<Value>) -> Self {
        Self {
            function_name,
            arguments,
            return_value: None,
            bindings: HashMap::new(),
            phase: VerificationPhase::Pre,
        }
    }
    
    /// Create a new verification context for postcondition checking
    pub fn post(
        function_name: String,
        arguments: Vec<Value>,
        return_value: Value,
    ) -> Self {
        Self {
            function_name,
            arguments,
            return_value: Some(return_value),
            bindings: HashMap::new(),
            phase: VerificationPhase::Post,
        }
    }
    
    /// Add a binding to the context
    pub fn add_binding(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }
}