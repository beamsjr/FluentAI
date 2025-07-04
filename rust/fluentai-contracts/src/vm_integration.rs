//! VM integration for contract verification

use std::sync::Arc;
use std::collections::HashMap;

use fluentai_core::{
    ast::{Graph, Node},
    value::Value,
};
use tracing::{debug, trace};

use crate::{
    contract::Contract,
    runtime::{RuntimeVerifier, VerificationContext},
    errors::ContractResult,
};

/// Contract-aware VM extensions
pub trait ContractVM {
    /// Set up contract verification
    fn enable_contracts(&mut self, graph: Arc<Graph>, contracts: HashMap<String, Contract>);
    
    /// Check preconditions before function call
    fn check_preconditions(&self, function_name: &str, args: &[Value]) -> ContractResult<()>;
    
    /// Check postconditions after function return
    fn check_postconditions(&self, function_name: &str, args: &[Value], result: &Value) -> ContractResult<()>;
    
    /// Check if a function has purity constraints
    fn is_pure_function(&self, function_name: &str) -> bool;
}

/// Contract registry for the VM
pub struct ContractRegistry {
    /// Runtime verifier
    verifier: RuntimeVerifier,
    
    /// Function parameter names (function_name -> param_names)
    /// This would be populated from function definitions
    param_names: HashMap<String, Vec<String>>,
    
    /// Track if contracts are enabled
    enabled: bool,
}

impl ContractRegistry {
    /// Create a new contract registry
    pub fn new() -> Self {
        Self {
            verifier: RuntimeVerifier::new(),
            param_names: HashMap::new(),
            enabled: false,
        }
    }
    
    /// Enable contract checking with AST graph
    pub fn enable(&mut self, graph: Arc<Graph>) {
        self.verifier.set_ast_graph(graph);
        self.enabled = true;
    }
    
    /// Register contracts from the AST
    pub fn register_contracts_from_ast(&mut self, graph: &Graph) {
        debug!("Registering contracts from AST");
        
        for (node_id, node) in &graph.nodes {
            if let Node::Contract { 
                function_name,
                preconditions,
                postconditions,
                invariants,
                complexity,
                pure,
            } = node {
                trace!("Found contract for function: {}", function_name);
                
                // Convert AST contract to our Contract type
                let mut contract = Contract::new(function_name.clone(), *node_id);
                
                // Add preconditions
                for &pre_id in preconditions {
                    contract.add_precondition(
                        crate::contract::ContractCondition::new(
                            pre_id,
                            crate::contract::ContractKind::Precondition
                        )
                    );
                }
                
                // Add postconditions
                for &post_id in postconditions {
                    contract.add_postcondition(
                        crate::contract::ContractCondition::new(
                            post_id,
                            crate::contract::ContractKind::Postcondition
                        )
                    );
                }
                
                // Add invariants
                for &inv_id in invariants {
                    contract.add_invariant(
                        crate::contract::ContractCondition::new(
                            inv_id,
                            crate::contract::ContractKind::Invariant
                        )
                    );
                }
                
                // Set other properties
                contract.complexity = complexity.clone();
                contract.pure = *pure;
                
                self.verifier.register_contract(contract);
            }
        }
    }
    
    /// Register parameter names for a function
    pub fn register_function_params(&mut self, function_name: String, param_names: Vec<String>) {
        self.param_names.insert(function_name, param_names);
    }
    
    /// Check preconditions
    pub fn check_preconditions(&self, function_name: &str, args: &[Value]) -> ContractResult<()> {
        if !self.enabled || !self.verifier.has_contract(function_name) {
            return Ok(());
        }
        
        let mut ctx = VerificationContext::pre(function_name.to_string(), args.to_vec());
        
        // Bind arguments to parameter names if available
        if let Some(param_names) = self.param_names.get(function_name) {
            for (i, (param_name, arg_value)) in param_names.iter().zip(args.iter()).enumerate() {
                ctx.add_binding(param_name.clone(), arg_value.clone());
                // Also bind by position for fallback
                ctx.add_binding(format!("arg{}", i), arg_value.clone());
            }
        } else {
            // Fall back to positional binding
            for (i, arg) in args.iter().enumerate() {
                ctx.add_binding(format!("arg{}", i), arg.clone());
            }
        }
        
        self.verifier.verify_preconditions(&ctx)
    }
    
    /// Check postconditions
    pub fn check_postconditions(
        &self,
        function_name: &str,
        args: &[Value],
        result: &Value
    ) -> ContractResult<()> {
        if !self.enabled || !self.verifier.has_contract(function_name) {
            return Ok(());
        }
        
        let mut ctx = VerificationContext::post(
            function_name.to_string(),
            args.to_vec(),
            result.clone()
        );
        
        // Bind arguments to parameter names if available
        if let Some(param_names) = self.param_names.get(function_name) {
            for (i, (param_name, arg_value)) in param_names.iter().zip(args.iter()).enumerate() {
                ctx.add_binding(param_name.clone(), arg_value.clone());
                ctx.add_binding(format!("arg{}", i), arg_value.clone());
            }
        } else {
            for (i, arg) in args.iter().enumerate() {
                ctx.add_binding(format!("arg{}", i), arg.clone());
            }
        }
        
        self.verifier.verify_postconditions(&ctx)
    }
    
    /// Check if a function is marked as pure
    pub fn is_pure_function(&self, function_name: &str) -> bool {
        self.verifier.get_contract(function_name)
            .map(|c| c.pure)
            .unwrap_or(false)
    }
    
    /// Check purity violation
    pub fn check_purity(&self, function_name: &str, had_side_effects: bool) -> ContractResult<()> {
        self.verifier.check_purity(function_name, had_side_effects)
    }
}

/// Example of how to integrate with the VM
/// This would be implemented in the fluentai-vm crate
#[allow(dead_code)]
pub mod example {
    use super::*;
    
    /// Example VM struct showing integration points
    pub struct VMWithContracts {
        // ... existing VM fields ...
        
        /// Contract registry
        contracts: ContractRegistry,
    }
    
    impl VMWithContracts {
        /// Example: Hook into function call
        fn call_function(&mut self, name: &str, args: Vec<Value>) -> ContractResult<Value> {
            // 1. Check preconditions
            self.contracts.check_preconditions(name, &args)?;
            
            // 2. Execute function (placeholder)
            let result = self.execute_function(name, args.clone())?;
            
            // 3. Check postconditions
            self.contracts.check_postconditions(name, &args, &result)?;
            
            Ok(result)
        }
        
        /// Placeholder for actual function execution
        fn execute_function(&mut self, _name: &str, _args: Vec<Value>) -> ContractResult<Value> {
            // This would be the actual VM execution
            Ok(Value::Nil)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_parser::parse;
    
    #[test]
    fn test_contract_registry() {
        let code = r#"
            (spec:contract add
                :requires [(>= a 0) (>= b 0)]
                :ensures [(>= result 0)]
                :pure true)
            
            (define (add a b) (+ a b))
        "#;
        
        let graph = parse(code).unwrap();
        let mut registry = ContractRegistry::new();
        registry.enable(Arc::new(graph.clone()));
        registry.register_contracts_from_ast(&graph);
        
        // Register parameter names
        registry.register_function_params("add".to_string(), vec!["a".to_string(), "b".to_string()]);
        
        // Test valid preconditions
        let result = registry.check_preconditions("add", &[Value::Integer(5), Value::Integer(3)]);
        assert!(result.is_ok());
        
        // Test invalid preconditions
        let result = registry.check_preconditions("add", &[Value::Integer(-1), Value::Integer(3)]);
        assert!(result.is_err());
    }
}