//! Counterexample generation for contract failures
//! 
//! This module provides detailed counterexample generation when contracts fail,
//! including minimal test cases, execution traces, and debugging information.

use crate::symbolic_execution::{SymbolicValue, SymbolicState, PathConstraint, SymbolicType};
use crate::symbolic_verification::SymbolicViolation;
use crate::test_generation::TestCase;
use crate::errors::{ContractError, ContractResult};
use fluentai_core::ast::{Graph, NodeId, Node, Literal};
use std::num::NonZeroU32;
use std::collections::HashMap;

#[cfg(feature = "static")]
use crate::incremental_solver::IncrementalSolver;

/// A detailed counterexample for a contract violation
#[derive(Debug, Clone)]
pub struct Counterexample {
    /// The concrete inputs that cause the violation
    pub inputs: HashMap<String, Literal>,
    /// Step-by-step execution trace
    pub execution_trace: Vec<ExecutionStep>,
    /// The specific assertion that failed
    pub failed_assertion: String,
    /// Minimal path constraints that lead to failure
    pub minimal_constraints: Vec<PathConstraint>,
    /// Additional debugging information
    pub debug_info: DebugInfo,
}

/// A single step in the execution trace
#[derive(Debug, Clone)]
pub struct ExecutionStep {
    /// Step number
    pub step: usize,
    /// Source location (if available)
    pub location: Option<String>,
    /// Operation performed
    pub operation: String,
    /// State after this step
    pub state: HashMap<String, String>,
    /// Whether this step contributed to the failure
    pub is_critical: bool,
}

/// Additional debugging information
#[derive(Debug, Clone)]
pub struct DebugInfo {
    /// Simplified path condition
    pub path_condition: String,
    /// Variables that influenced the failure
    pub relevant_variables: Vec<String>,
    /// Suggested fixes (if any)
    pub suggestions: Vec<String>,
    /// Complexity metrics
    pub complexity: ComplexityMetrics,
}

/// Metrics about the counterexample complexity
#[derive(Debug, Clone)]
pub struct ComplexityMetrics {
    /// Number of branches taken
    pub branch_count: usize,
    /// Depth of recursion (if any)
    pub recursion_depth: usize,
    /// Number of loop iterations
    pub loop_iterations: usize,
    /// Size of the minimal constraint set
    pub constraint_count: usize,
}

/// Counterexample generator
pub struct CounterexampleGenerator {
    /// Whether to minimize counterexamples
    minimize: bool,
    /// Whether to generate execution traces
    generate_traces: bool,
    /// Maximum trace length
    max_trace_length: usize,
}

impl CounterexampleGenerator {
    /// Create a new counterexample generator
    pub fn new() -> Self {
        Self {
            minimize: true,
            generate_traces: true,
            max_trace_length: 100,
        }
    }
    
    /// Create a generator with custom configuration
    pub fn with_config(minimize: bool, generate_traces: bool) -> Self {
        Self {
            minimize,
            generate_traces,
            max_trace_length: 100,
        }
    }
    
    /// Generate a counterexample from a contract violation
    pub fn generate_counterexample(
        &self,
        violation: &SymbolicViolation,
        param_names: &[String],
        graph: &Graph,
    ) -> ContractResult<Counterexample> {
        // Generate concrete inputs
        let inputs = self.generate_concrete_inputs(&violation.violating_state, param_names)?;
        
        // Minimize constraints if requested
        let minimal_constraints = if self.minimize {
            self.minimize_constraints(&violation.violating_state.path_constraints)?
        } else {
            violation.violating_state.path_constraints.clone()
        };
        
        // Generate execution trace if requested
        let execution_trace = if self.generate_traces {
            self.generate_execution_trace(&violation.violating_state, &inputs, graph)?
        } else {
            Vec::new()
        };
        
        // Extract failed assertion
        let failed_assertion = format!("{:?}: {}", 
            violation.condition_type,
            violation.condition.message.as_ref().unwrap_or(&"Unknown".to_string())
        );
        
        // Generate debug info
        let debug_info = self.generate_debug_info(
            &violation.violating_state,
            &minimal_constraints,
            &execution_trace,
        )?;
        
        Ok(Counterexample {
            inputs,
            execution_trace,
            failed_assertion,
            minimal_constraints,
            debug_info,
        })
    }
    
    /// Generate concrete inputs from symbolic state
    fn generate_concrete_inputs(
        &self,
        state: &SymbolicState,
        param_names: &[String],
    ) -> ContractResult<HashMap<String, Literal>> {
        let mut inputs = HashMap::new();
        
        #[cfg(feature = "static")]
        {
            // Try to use Z3 to generate concrete values
            use z3::Context;
            let context = Context::new(&z3::Config::new());
            let mut solver = IncrementalSolver::new(&context);
            
            if solver.check_state_incremental(state)? {
                let model = solver.get_model()?;
                
                for param in param_names {
                    if let Some(&value) = model.get(param) {
                        inputs.insert(param.clone(), Literal::Integer(value));
                    } else {
                        // Use binding if available
                        if let Some(sym_value) = state.bindings.get(param) {
                            if let Some(lit) = self.extract_literal(sym_value) {
                                inputs.insert(param.clone(), lit);
                            } else {
                                inputs.insert(param.clone(), Literal::Integer(0));
                            }
                        } else {
                            inputs.insert(param.clone(), Literal::Integer(0));
                        }
                    }
                }
                
                return Ok(inputs);
            }
        }
        
        // Fallback: use heuristics
        for param in param_names {
            if let Some(sym_value) = state.bindings.get(param) {
                if let Some(lit) = self.extract_literal(sym_value) {
                    inputs.insert(param.clone(), lit);
                } else {
                    // Analyze constraints to infer value
                    let value = self.infer_value_from_constraints(param, &state.path_constraints)?;
                    inputs.insert(param.clone(), value);
                }
            } else {
                inputs.insert(param.clone(), Literal::Integer(0));
            }
        }
        
        Ok(inputs)
    }
    
    /// Extract literal from symbolic value if possible
    fn extract_literal(&self, value: &SymbolicValue) -> Option<Literal> {
        match value {
            SymbolicValue::Concrete(lit) => Some(lit.clone()),
            _ => None,
        }
    }
    
    /// Infer a value from constraints
    fn infer_value_from_constraints(
        &self,
        param: &str,
        constraints: &[PathConstraint],
    ) -> ContractResult<Literal> {
        // Look for constraints involving this parameter
        for constraint in constraints {
            if let SymbolicValue::BinOp { op, left, right } = &constraint.constraint {
                // Check if this constraint involves our parameter
                if self.involves_parameter(left, param) || self.involves_parameter(right, param) {
                    // Try to extract a value
                    match (op.as_str(), constraint.expected) {
                        ("=", true) => {
                            // x = value
                            if let (SymbolicValue::Symbolic { name, .. }, SymbolicValue::Concrete(lit)) = (left.as_ref(), right.as_ref()) {
                                if name == param {
                                    return Ok(lit.clone());
                                }
                            }
                        }
                        ("<", true) => {
                            // x < value, so use value - 1
                            if let (SymbolicValue::Symbolic { name, .. }, SymbolicValue::Concrete(Literal::Integer(n))) = (left.as_ref(), right.as_ref()) {
                                if name == param {
                                    return Ok(Literal::Integer(n - 1));
                                }
                            }
                        }
                        (">", true) => {
                            // x > value, so use value + 1
                            if let (SymbolicValue::Symbolic { name, .. }, SymbolicValue::Concrete(Literal::Integer(n))) = (left.as_ref(), right.as_ref()) {
                                if name == param {
                                    return Ok(Literal::Integer(n + 1));
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        
        // Default value
        Ok(Literal::Integer(0))
    }
    
    /// Check if a symbolic value involves a parameter
    fn involves_parameter(&self, value: &SymbolicValue, param: &str) -> bool {
        match value {
            SymbolicValue::Symbolic { name, .. } => name == param,
            SymbolicValue::BinOp { left, right, .. } => {
                self.involves_parameter(left, param) || self.involves_parameter(right, param)
            }
            SymbolicValue::UnaryOp { operand, .. } => self.involves_parameter(operand, param),
            _ => false,
        }
    }
    
    /// Minimize the constraint set
    fn minimize_constraints(
        &self,
        constraints: &[PathConstraint],
    ) -> ContractResult<Vec<PathConstraint>> {
        // Delta debugging approach: try to find minimal failing set
        let mut minimal = constraints.to_vec();
        
        // Try removing each constraint
        let mut i = 0;
        while i < minimal.len() {
            let mut test_set = minimal.clone();
            test_set.remove(i);
            
            // Check if the reduced set still leads to the failure
            // For now, we'll keep all constraints
            // In a full implementation, we'd re-run symbolic execution
            i += 1;
        }
        
        Ok(minimal)
    }
    
    /// Generate execution trace
    fn generate_execution_trace(
        &self,
        state: &SymbolicState,
        inputs: &HashMap<String, Literal>,
        graph: &Graph,
    ) -> ContractResult<Vec<ExecutionStep>> {
        let mut trace = Vec::new();
        let mut step_count = 0;
        
        // Initial state
        let mut current_state: HashMap<String, String> = inputs.iter()
            .map(|(k, v)| (k.clone(), format!("{:?}", v)))
            .collect();
        
        trace.push(ExecutionStep {
            step: step_count,
            location: Some("Function entry".to_string()),
            operation: "Initialize parameters".to_string(),
            state: current_state.clone(),
            is_critical: true,
        });
        
        // Trace through constraints
        for (i, constraint) in state.path_constraints.iter().enumerate() {
            step_count += 1;
            
            let operation = self.describe_constraint(constraint);
            
            // Update state based on constraint
            // This is simplified - in reality we'd trace actual execution
            if let SymbolicValue::BinOp { op, left, right } = &constraint.constraint {
                if op == "=" && constraint.expected {
                    if let (SymbolicValue::Symbolic { name, .. }, SymbolicValue::Concrete(lit)) = (left.as_ref(), right.as_ref()) {
                        current_state.insert(name.clone(), format!("{:?}", lit));
                    }
                }
            }
            
            trace.push(ExecutionStep {
                step: step_count,
                location: Some(format!("Branch {}", i + 1)),
                operation,
                state: current_state.clone(),
                is_critical: i >= state.path_constraints.len().saturating_sub(3), // Last few are often critical
            });
            
            if trace.len() >= self.max_trace_length {
                break;
            }
        }
        
        Ok(trace)
    }
    
    /// Describe a constraint in human-readable form
    fn describe_constraint(&self, constraint: &PathConstraint) -> String {
        let base = self.format_symbolic_value(&constraint.constraint);
        if constraint.expected {
            format!("Assert: {}", base)
        } else {
            format!("Assert: not ({})", base)
        }
    }
    
    /// Format symbolic value
    fn format_symbolic_value(&self, value: &SymbolicValue) -> String {
        match value {
            SymbolicValue::Concrete(lit) => format!("{:?}", lit),
            SymbolicValue::Symbolic { name, .. } => name.clone(),
            SymbolicValue::BinOp { op, left, right } => {
                format!("{} {} {}", 
                    self.format_symbolic_value(left),
                    op,
                    self.format_symbolic_value(right)
                )
            }
            SymbolicValue::UnaryOp { op, operand } => {
                format!("{} {}", op, self.format_symbolic_value(operand))
            }
            _ => "...".to_string(),
        }
    }
    
    /// Generate debugging information
    fn generate_debug_info(
        &self,
        state: &SymbolicState,
        minimal_constraints: &[PathConstraint],
        trace: &[ExecutionStep],
    ) -> ContractResult<DebugInfo> {
        // Simplify path condition
        let path_condition = minimal_constraints.iter()
            .map(|c| self.describe_constraint(c))
            .collect::<Vec<_>>()
            .join(" AND ");
        
        // Find relevant variables
        let mut relevant_variables = Vec::new();
        for constraint in minimal_constraints {
            self.collect_variables(&constraint.constraint, &mut relevant_variables);
        }
        relevant_variables.sort();
        relevant_variables.dedup();
        
        // Generate suggestions
        let suggestions = self.generate_suggestions(state, minimal_constraints)?;
        
        // Calculate complexity metrics
        let complexity = ComplexityMetrics {
            branch_count: minimal_constraints.len(),
            recursion_depth: self.estimate_recursion_depth(trace),
            loop_iterations: self.estimate_loop_iterations(trace),
            constraint_count: minimal_constraints.len(),
        };
        
        Ok(DebugInfo {
            path_condition,
            relevant_variables,
            suggestions,
            complexity,
        })
    }
    
    /// Collect variables from symbolic value
    fn collect_variables(&self, value: &SymbolicValue, vars: &mut Vec<String>) {
        match value {
            SymbolicValue::Symbolic { name, .. } => vars.push(name.clone()),
            SymbolicValue::BinOp { left, right, .. } => {
                self.collect_variables(left, vars);
                self.collect_variables(right, vars);
            }
            SymbolicValue::UnaryOp { operand, .. } => {
                self.collect_variables(operand, vars);
            }
            _ => {}
        }
    }
    
    /// Generate suggestions for fixing the contract violation
    fn generate_suggestions(
        &self,
        state: &SymbolicState,
        constraints: &[PathConstraint],
    ) -> ContractResult<Vec<String>> {
        let mut suggestions = Vec::new();
        
        // Look for common patterns
        for constraint in constraints {
            if let SymbolicValue::BinOp { op, left, right } = &constraint.constraint {
                match op.as_str() {
                    "/" => {
                        // Division - check for division by zero
                        if let SymbolicValue::Concrete(Literal::Integer(0)) = right.as_ref() {
                            suggestions.push("Add check for division by zero".to_string());
                        }
                    }
                    "<" | ">" | "<=" | ">=" => {
                        // Bounds check
                        suggestions.push("Consider adding bounds validation".to_string());
                    }
                    _ => {}
                }
            }
        }
        
        // Generic suggestions
        if constraints.len() > 5 {
            suggestions.push("Complex path - consider simplifying logic".to_string());
        }
        
        Ok(suggestions)
    }
    
    /// Estimate recursion depth from trace
    fn estimate_recursion_depth(&self, trace: &[ExecutionStep]) -> usize {
        // Simple heuristic: count repeated operations
        let mut max_depth: usize = 0;
        let mut current_depth: usize = 0;
        
        for i in 1..trace.len() {
            if trace[i].operation.contains("recursive call") {
                current_depth += 1;
                max_depth = max_depth.max(current_depth);
            } else if trace[i].operation.contains("return") {
                current_depth = current_depth.saturating_sub(1);
            }
        }
        
        max_depth
    }
    
    /// Estimate loop iterations from trace
    fn estimate_loop_iterations(&self, trace: &[ExecutionStep]) -> usize {
        // Count branch repetitions
        let mut iterations = 0;
        let mut seen_branches = std::collections::HashSet::new();
        
        for step in trace {
            if let Some(loc) = &step.location {
                if loc.contains("Branch") {
                    if !seen_branches.insert(loc.clone()) {
                        iterations += 1;
                    }
                }
            }
        }
        
        iterations
    }
}

/// Format a counterexample for display
pub fn format_counterexample(counterexample: &Counterexample) -> String {
    let mut output = String::new();
    
    // Header
    output.push_str("=== CONTRACT VIOLATION COUNTEREXAMPLE ===\n\n");
    
    // Failed assertion
    output.push_str(&format!("Failed Assertion: {}\n\n", counterexample.failed_assertion));
    
    // Inputs
    output.push_str("Inputs that cause failure:\n");
    for (param, value) in &counterexample.inputs {
        output.push_str(&format!("  {} = {:?}\n", param, value));
    }
    output.push('\n');
    
    // Path condition
    output.push_str("Path Condition:\n");
    output.push_str(&format!("  {}\n\n", counterexample.debug_info.path_condition));
    
    // Execution trace (abbreviated)
    if !counterexample.execution_trace.is_empty() {
        output.push_str("Execution Trace:\n");
        for step in counterexample.execution_trace.iter().filter(|s| s.is_critical).take(10) {
            output.push_str(&format!("  Step {}: {}\n", step.step, step.operation));
            if step.state.len() <= 3 {
                for (var, val) in &step.state {
                    output.push_str(&format!("    {} = {}\n", var, val));
                }
            }
        }
        if counterexample.execution_trace.len() > 10 {
            output.push_str("  ...\n");
        }
        output.push('\n');
    }
    
    // Relevant variables
    output.push_str("Relevant Variables:\n");
    output.push_str(&format!("  {}\n\n", counterexample.debug_info.relevant_variables.join(", ")));
    
    // Complexity metrics
    output.push_str("Complexity Metrics:\n");
    let metrics = &counterexample.debug_info.complexity;
    output.push_str(&format!("  Branches: {}\n", metrics.branch_count));
    output.push_str(&format!("  Constraints: {}\n", metrics.constraint_count));
    if metrics.recursion_depth > 0 {
        output.push_str(&format!("  Recursion depth: {}\n", metrics.recursion_depth));
    }
    if metrics.loop_iterations > 0 {
        output.push_str(&format!("  Loop iterations: {}\n", metrics.loop_iterations));
    }
    output.push('\n');
    
    // Suggestions
    if !counterexample.debug_info.suggestions.is_empty() {
        output.push_str("Suggestions:\n");
        for suggestion in &counterexample.debug_info.suggestions {
            output.push_str(&format!("  - {}\n", suggestion));
        }
    }
    
    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbolic_execution::SymbolicState;
    use crate::symbolic_verification::{SymbolicViolation, ContractConditionType};
    
    #[test]
    fn test_counterexample_generation() {
        let mut state = SymbolicState::new();
        
        // Add some constraints
        let x = SymbolicValue::Symbolic { name: "x".to_string(), ty: Some(SymbolicType::Integer) };
        let zero = SymbolicValue::Concrete(Literal::Integer(0));
        
        state.add_constraint(
            SymbolicValue::BinOp {
                op: ">".to_string(),
                left: Box::new(x.clone()),
                right: Box::new(zero),
            },
            true
        );
        
        let violation = SymbolicViolation {
            condition_type: ContractConditionType::Postcondition,
            condition: crate::contract::ContractCondition {
                expression: NodeId(NonZeroU32::new(1).unwrap()),
                message: Some("Result should be positive".to_string()),
                kind: crate::contract::ContractKind::Postcondition,
                span: None,
                blame_label: None,
            },
            violating_state: state,
            path_description: "x > 0".to_string(),
        };
        
        // Create a graph with a node for the condition
        let mut graph = Graph::new();
        let condition_node = graph.add_node(Node::Literal(Literal::Boolean(true)));
        
        // Update the violation to use the actual node
        let mut updated_violation = violation;
        updated_violation.condition.expression = condition_node;
        
        let generator = CounterexampleGenerator::new();
        let counterexample = generator.generate_counterexample(
            &updated_violation,
            &["x".to_string()],
            &graph,
        ).unwrap();
        
        // Check that we generated inputs
        assert!(counterexample.inputs.contains_key("x"));
        
        // Check debug info
        assert!(!counterexample.debug_info.relevant_variables.is_empty());
        assert!(counterexample.debug_info.relevant_variables.contains(&"x".to_string()));
    }
}