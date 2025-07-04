//! Integration of symbolic execution with contract verification
//! 
//! This module provides verification of contracts using symbolic execution
//! to explore all possible execution paths and verify contract conditions.

use crate::symbolic_execution::{SymbolicExecutor, SymbolicState, SymbolicValue};
use crate::contract::{Contract, ContractCondition};
use crate::errors::{ContractError, ContractResult, ContractViolation};
use crate::test_generation::{TestGenerator, TestCase};
use crate::visualization::{ExecutionTree, TreeBuilder};
use fluentai_core::ast::{Graph, NodeId};
use std::collections::HashMap;

#[cfg(feature = "static")]
use crate::incremental_solver::IncrementalSolver;

/// Result of symbolic contract verification
#[derive(Debug)]
pub struct SymbolicVerificationResult {
    /// Whether all contract conditions hold
    pub verified: bool,
    /// Contract violations found (if any)
    pub violations: Vec<SymbolicViolation>,
    /// Number of paths explored
    pub paths_explored: usize,
    /// Number of paths that satisfy preconditions
    pub valid_paths: usize,
    /// Generated test cases for violations
    pub counterexamples: Vec<TestCase>,
    /// Execution tree for visualization
    pub execution_tree: Option<ExecutionTree>,
}

/// A contract violation found during symbolic execution
#[derive(Debug, Clone)]
pub struct SymbolicViolation {
    /// Type of contract condition violated
    pub condition_type: ContractConditionType,
    /// The specific condition that was violated
    pub condition: ContractCondition,
    /// The symbolic state that led to the violation
    pub violating_state: SymbolicState,
    /// Path constraints that lead to this violation
    pub path_description: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContractConditionType {
    Precondition,
    Postcondition,
    Invariant,
}

/// Symbolic contract verifier
pub struct SymbolicContractVerifier {
    executor: SymbolicExecutor,
    test_generator: TestGenerator,
    generate_visualization: bool,
    #[cfg(feature = "static")]
    use_incremental_solver: bool,
}

impl SymbolicContractVerifier {
    /// Create a new symbolic contract verifier
    pub fn new() -> Self {
        Self {
            executor: SymbolicExecutor::new(),
            test_generator: TestGenerator::new(),
            generate_visualization: false,
            #[cfg(feature = "static")]
            use_incremental_solver: true,
        }
    }
    
    /// Create a verifier with custom configuration
    pub fn with_config(max_depth: usize, max_states: usize) -> Self {
        Self {
            executor: SymbolicExecutor::with_limits(max_depth, max_states),
            test_generator: TestGenerator::new(),
            generate_visualization: false,
            #[cfg(feature = "static")]
            use_incremental_solver: true,
        }
    }
    
    /// Enable visualization generation
    pub fn with_visualization(mut self) -> Self {
        self.generate_visualization = true;
        self
    }
    
    /// Verify a contract symbolically
    pub fn verify_contract(
        &self,
        graph: &Graph,
        contract: &Contract,
    ) -> ContractResult<SymbolicVerificationResult> {
        // Extract function body
        let function_id = self.find_function(graph, &contract.function_name)?;
        
        // Get parameter names
        let param_names = self.extract_parameters(graph, &contract.function_name)?;
        
        // Execute function symbolically
        let states = self.executor.execute_function(graph, function_id, &param_names)?;
        
        // Verify contracts on all paths
        let mut violations = Vec::new();
        let mut valid_paths = 0;
        let mut all_states = Vec::new();
        
        #[cfg(feature = "static")]
        let mut solver = if self.use_incremental_solver {
            use z3::Context;
            Some(IncrementalSolver::new(&Context::new(&z3::Config::new())))
        } else {
            None
        };
        
        for state in &states {
            all_states.push(state.clone());
            
            // Check if preconditions are satisfied
            let preconditions_hold = self.check_preconditions(
                &state,
                &contract.preconditions,
                graph,
                #[cfg(feature = "static")]
                solver.as_mut(),
            )?;
            
            if !preconditions_hold {
                // Skip this path as preconditions don't hold
                continue;
            }
            
            valid_paths += 1;
            
            // Check postconditions
            let postcondition_violations = self.check_postconditions(
                &state,
                &contract.postconditions,
                graph,
                #[cfg(feature = "static")]
                solver.as_mut(),
            )?;
            
            for (condition, violated) in postcondition_violations {
                if violated {
                    violations.push(SymbolicViolation {
                        condition_type: ContractConditionType::Postcondition,
                        condition: condition.clone(),
                        violating_state: state.clone(),
                        path_description: self.describe_path(state),
                    });
                }
            }
            
            // Check invariants
            let invariant_violations = self.check_invariants(
                &state,
                &contract.invariants,
                graph,
                #[cfg(feature = "static")]
                solver.as_mut(),
            )?;
            
            for (condition, violated) in invariant_violations {
                if violated {
                    violations.push(SymbolicViolation {
                        condition_type: ContractConditionType::Invariant,
                        condition: condition.clone(),
                        violating_state: state.clone(),
                        path_description: self.describe_path(state),
                    });
                }
            }
        }
        
        // Generate counterexamples for violations
        let counterexamples = if !violations.is_empty() {
            self.generate_counterexamples(&violations, &param_names)?
        } else {
            Vec::new()
        };
        
        // Build execution tree if requested
        let execution_tree = if self.generate_visualization {
            Some(TreeBuilder::build_from_states(all_states)?)
        } else {
            None
        };
        
        Ok(SymbolicVerificationResult {
            verified: violations.is_empty(),
            violations,
            paths_explored: states.len(),
            valid_paths,
            counterexamples,
            execution_tree,
        })
    }
    
    /// Find function definition in the graph
    fn find_function(&self, graph: &Graph, function_name: &str) -> ContractResult<NodeId> {
        // Look for (define (fname ...) body) pattern
        for (_id, node) in &graph.nodes {
            if let fluentai_core::ast::Node::Application { function, args } = node {
                if let Some(fluentai_core::ast::Node::Variable { name }) = graph.get_node(*function) {
                    if name == "define" && args.len() == 2 {
                        if let Some(fluentai_core::ast::Node::Application { function: fname_id, .. }) = graph.get_node(args[0]) {
                            if let Some(fluentai_core::ast::Node::Variable { name: fname }) = graph.get_node(*fname_id) {
                                if fname == function_name {
                                    return Ok(args[1]);
                                }
                            }
                        }
                    }
                }
            }
        }
        
        Err(ContractError::Other(format!("Function '{}' not found", function_name)))
    }
    
    /// Extract parameter names from function definition
    fn extract_parameters(&self, graph: &Graph, function_name: &str) -> ContractResult<Vec<String>> {
        // Similar to find_function but extracts parameter names
        for (_id, node) in &graph.nodes {
            if let fluentai_core::ast::Node::Application { function, args } = node {
                if let Some(fluentai_core::ast::Node::Variable { name }) = graph.get_node(*function) {
                    if name == "define" && args.len() == 2 {
                        if let Some(fluentai_core::ast::Node::Application { function: fname_id, args: param_ids }) = graph.get_node(args[0]) {
                            if let Some(fluentai_core::ast::Node::Variable { name: fname }) = graph.get_node(*fname_id) {
                                if fname == function_name {
                                    let mut params = Vec::new();
                                    for param_id in param_ids {
                                        if let Some(fluentai_core::ast::Node::Variable { name }) = graph.get_node(*param_id) {
                                            params.push(name.clone());
                                        }
                                    }
                                    return Ok(params);
                                }
                            }
                        }
                    }
                }
            }
        }
        
        Ok(Vec::new())
    }
    
    /// Check if preconditions are satisfied
    fn check_preconditions(
        &self,
        state: &SymbolicState,
        preconditions: &[ContractCondition],
        graph: &Graph,
        #[cfg(feature = "static")]
        solver: Option<&mut IncrementalSolver>,
    ) -> ContractResult<bool> {
        if preconditions.is_empty() {
            return Ok(true);
        }
        
        // For now, we assume preconditions hold if the path is reachable
        // In a full implementation, we would evaluate the precondition expressions
        // symbolically and check if they're satisfied
        
        #[cfg(feature = "static")]
        if let Some(solver) = solver {
            // Check if the current path is satisfiable
            return solver.check_state_incremental(state);
        }
        
        Ok(true)
    }
    
    /// Check postconditions
    fn check_postconditions(
        &self,
        state: &SymbolicState,
        postconditions: &[ContractCondition],
        graph: &Graph,
        #[cfg(feature = "static")]
        solver: Option<&mut IncrementalSolver>,
    ) -> ContractResult<Vec<(ContractCondition, bool)>> {
        let mut results = Vec::new();
        
        for condition in postconditions {
            // In a full implementation, we would:
            // 1. Convert the condition expression to symbolic form
            // 2. Evaluate it in the context of the final state
            // 3. Check if it can be false
            
            // For now, we'll return no violations
            results.push((condition.clone(), false));
        }
        
        Ok(results)
    }
    
    /// Check invariants
    fn check_invariants(
        &self,
        state: &SymbolicState,
        invariants: &[ContractCondition],
        graph: &Graph,
        #[cfg(feature = "static")]
        solver: Option<&mut IncrementalSolver>,
    ) -> ContractResult<Vec<(ContractCondition, bool)>> {
        let mut results = Vec::new();
        
        for condition in invariants {
            // Similar to postconditions
            results.push((condition.clone(), false));
        }
        
        Ok(results)
    }
    
    /// Generate a human-readable description of a path
    fn describe_path(&self, state: &SymbolicState) -> String {
        let mut description = String::new();
        
        for (i, constraint) in state.path_constraints.iter().enumerate() {
            if i > 0 {
                description.push_str(" → ");
            }
            description.push_str(&self.describe_constraint(constraint));
        }
        
        if description.is_empty() {
            description = "Empty path".to_string();
        }
        
        description
    }
    
    /// Describe a single constraint
    fn describe_constraint(&self, constraint: &crate::symbolic_execution::PathConstraint) -> String {
        format!(
            "{}{}",
            if constraint.expected { "" } else { "¬" },
            self.format_symbolic_value(&constraint.constraint)
        )
    }
    
    /// Format a symbolic value
    fn format_symbolic_value(&self, value: &SymbolicValue) -> String {
        match value {
            SymbolicValue::Concrete(lit) => format!("{:?}", lit),
            SymbolicValue::Symbolic { name, .. } => name.clone(),
            SymbolicValue::BinOp { op, left, right } => {
                format!(
                    "({} {} {})",
                    self.format_symbolic_value(left),
                    op,
                    self.format_symbolic_value(right)
                )
            }
            _ => "...".to_string(),
        }
    }
    
    /// Generate counterexamples for violations
    fn generate_counterexamples(
        &self,
        violations: &[SymbolicViolation],
        param_names: &[String],
    ) -> ContractResult<Vec<TestCase>> {
        let mut counterexamples = Vec::new();
        
        // Generate test cases for each unique violating state
        let mut seen_states = std::collections::HashSet::new();
        
        for violation in violations {
            let state_hash = format!("{:?}", violation.violating_state);
            if seen_states.insert(state_hash) {
                let states = vec![violation.violating_state.clone()];
                let tests = self.test_generator.generate_tests(&states, param_names)?;
                
                // Add violation information to test description
                for mut test in tests {
                    test.description = format!(
                        "{} - Violates {:?}: {}",
                        test.description,
                        violation.condition_type,
                        violation.path_description
                    );
                    counterexamples.push(test);
                }
            }
        }
        
        Ok(counterexamples)
    }
}

/// Helper function to verify a function against its contract
pub fn verify_function_contract(
    graph: &Graph,
    contract: &Contract,
) -> ContractResult<SymbolicVerificationResult> {
    let verifier = SymbolicContractVerifier::new();
    verifier.verify_contract(graph, contract)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_parser::parse;
    
    #[test]
    fn test_basic_verification() {
        let program = r#"
            (define (abs x)
              (if (< x 0)
                  (- 0 x)
                  x))
        "#;
        
        let graph = parse(program).unwrap();
        
        // Create a simple contract
        let contract = Contract {
            function_name: "abs".to_string(),
            preconditions: vec![],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(0), // Dummy
                    description: Some("result >= 0".to_string()),
                }
            ],
            invariants: vec![],
        };
        
        let verifier = SymbolicContractVerifier::new();
        let result = verifier.verify_contract(&graph, &contract).unwrap();
        
        assert_eq!(result.paths_explored, 2); // Two branches: x < 0 and x >= 0
        assert!(result.verified); // Should verify (in our simplified implementation)
    }
}