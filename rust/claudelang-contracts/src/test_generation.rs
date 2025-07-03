//! Test case generation from symbolic execution paths
//! 
//! This module generates concrete test cases from symbolic execution paths
//! that can be used for property-based testing and coverage analysis.

use crate::symbolic_execution::{SymbolicValue, SymbolicState, PathConstraint, SymbolicType};
use crate::errors::{ContractError, ContractResult};
use claudelang_core::ast::Literal;
use std::collections::HashMap;

#[cfg(feature = "static")]
use crate::symbolic_z3::SymbolicSolver;

/// A concrete test case generated from a symbolic path
#[derive(Debug, Clone)]
pub struct TestCase {
    /// Input values for function parameters
    pub inputs: HashMap<String, Literal>,
    /// Expected output value (if deterministic)
    pub expected_output: Option<Literal>,
    /// Path constraints that led to this test case
    pub path_constraints: Vec<PathConstraint>,
    /// Description of what this test case covers
    pub description: String,
}

/// Test case generator from symbolic execution results
pub struct TestGenerator {
    /// Maximum number of test cases to generate per path
    max_tests_per_path: usize,
    /// Random seed for non-deterministic generation
    seed: u64,
}

impl TestGenerator {
    /// Create a new test generator
    pub fn new() -> Self {
        Self {
            max_tests_per_path: 5,
            seed: 42,
        }
    }
    
    /// Create a test generator with custom configuration
    pub fn with_config(max_tests_per_path: usize, seed: u64) -> Self {
        Self {
            max_tests_per_path,
            seed,
        }
    }
    
    /// Generate test cases from symbolic execution states
    pub fn generate_tests(
        &self,
        states: &[SymbolicState],
        param_names: &[String],
    ) -> ContractResult<Vec<TestCase>> {
        let mut test_cases = Vec::new();
        
        for (i, state) in states.iter().enumerate() {
            // Generate test cases for this path
            let path_tests = self.generate_tests_for_path(state, param_names, i)?;
            test_cases.extend(path_tests);
        }
        
        Ok(test_cases)
    }
    
    /// Generate test cases for a single symbolic execution path
    fn generate_tests_for_path(
        &self,
        state: &SymbolicState,
        param_names: &[String],
        path_index: usize,
    ) -> ContractResult<Vec<TestCase>> {
        let mut test_cases = Vec::new();
        
        // Try to generate tests using Z3 if available
        #[cfg(feature = "static")]
        {
            if let Ok(z3_tests) = self.generate_z3_tests(state, param_names, path_index) {
                test_cases.extend(z3_tests);
            }
        }
        
        // If Z3 is not available or didn't generate enough tests, use heuristic generation
        if test_cases.len() < self.max_tests_per_path {
            let heuristic_tests = self.generate_heuristic_tests(
                state, 
                param_names, 
                path_index,
                self.max_tests_per_path - test_cases.len()
            )?;
            test_cases.extend(heuristic_tests);
        }
        
        Ok(test_cases)
    }
    
    /// Generate test cases using Z3 solver
    #[cfg(feature = "static")]
    fn generate_z3_tests(
        &self,
        state: &SymbolicState,
        param_names: &[String],
        path_index: usize,
    ) -> ContractResult<Vec<TestCase>> {
        use z3::Context;
        
        let mut test_cases = Vec::new();
        let context = Context::new(&z3::Config::new());
        let mut solver = SymbolicSolver::new(&context);
        
        // Check if the path is satisfiable
        if !solver.check_state(state)? {
            // Path is unsatisfiable, no test cases
            return Ok(test_cases);
        }
        
        // Get model from Z3
        let model = solver.get_model()?;
        
        // Convert model to test case
        let mut inputs = HashMap::new();
        for param in param_names {
            if let Some(&value) = model.get(param) {
                inputs.insert(param.clone(), Literal::Integer(value));
            } else {
                // Default value if not in model
                inputs.insert(param.clone(), Literal::Integer(0));
            }
        }
        
        let test_case = TestCase {
            inputs,
            expected_output: None,
            path_constraints: state.path_constraints.clone(),
            description: format!("Path {} (Z3 generated)", path_index + 1),
        };
        
        test_cases.push(test_case);
        
        // Try to generate diverse test cases by adding constraints
        for i in 1..self.max_tests_per_path {
            // Add constraint to make next solution different
            // This is a simplified approach; a real implementation would be more sophisticated
            if let Some((first_param, first_value)) = inputs.iter().next() {
                if let Literal::Integer(n) = first_value {
                    // Try to get a different value for the first parameter
                    let different_value = if *n == 0 { 1 } else { 0 };
                    let mut new_inputs = inputs.clone();
                    new_inputs.insert(first_param.clone(), Literal::Integer(different_value));
                    
                    let test_case = TestCase {
                        inputs: new_inputs,
                        expected_output: None,
                        path_constraints: state.path_constraints.clone(),
                        description: format!("Path {} (Z3 variant {})", path_index + 1, i + 1),
                    };
                    
                    test_cases.push(test_case);
                }
            }
        }
        
        Ok(test_cases)
    }
    
    /// Generate test cases using heuristics
    fn generate_heuristic_tests(
        &self,
        state: &SymbolicState,
        param_names: &[String],
        path_index: usize,
        count: usize,
    ) -> ContractResult<Vec<TestCase>> {
        let mut test_cases = Vec::new();
        
        // Analyze constraints to infer reasonable values
        let param_info = self.analyze_constraints(state, param_names)?;
        
        for i in 0..count {
            let mut inputs = HashMap::new();
            
            for param in param_names {
                let value = if let Some(info) = param_info.get(param) {
                    // Generate value based on constraint analysis
                    self.generate_value_from_info(info, i)
                } else {
                    // Default heuristic values
                    match i {
                        0 => Literal::Integer(0),        // Zero
                        1 => Literal::Integer(1),        // One
                        2 => Literal::Integer(-1),       // Negative
                        3 => Literal::Integer(42),       // Positive
                        4 => Literal::Integer(i64::MAX), // Max value
                        _ => Literal::Integer((i as i64) * 10), // Multiples of 10
                    }
                };
                inputs.insert(param.clone(), value);
            }
            
            let test_case = TestCase {
                inputs,
                expected_output: None,
                path_constraints: state.path_constraints.clone(),
                description: format!("Path {} (heuristic {})", path_index + 1, i + 1),
            };
            
            test_cases.push(test_case);
        }
        
        Ok(test_cases)
    }
    
    /// Analyze constraints to infer information about parameters
    fn analyze_constraints(
        &self,
        state: &SymbolicState,
        param_names: &[String],
    ) -> ContractResult<HashMap<String, ParameterInfo>> {
        let mut param_info = HashMap::new();
        
        for constraint in &state.path_constraints {
            self.analyze_constraint(&constraint.constraint, constraint.expected, &mut param_info);
        }
        
        Ok(param_info)
    }
    
    /// Analyze a single constraint
    fn analyze_constraint(
        &self,
        constraint: &SymbolicValue,
        expected: bool,
        param_info: &mut HashMap<String, ParameterInfo>,
    ) {
        match constraint {
            SymbolicValue::BinOp { op, left, right } => {
                // Extract bounds from comparison operators
                match (op.as_str(), left.as_ref(), right.as_ref()) {
                    ("<", SymbolicValue::Symbolic { name, .. }, SymbolicValue::Concrete(Literal::Integer(n))) => {
                        if expected {
                            let info = param_info.entry(name.clone()).or_default();
                            info.upper_bound = Some(info.upper_bound.map_or(*n - 1, |b| b.min(*n - 1)));
                        }
                    }
                    (">", SymbolicValue::Symbolic { name, .. }, SymbolicValue::Concrete(Literal::Integer(n))) => {
                        if expected {
                            let info = param_info.entry(name.clone()).or_default();
                            info.lower_bound = Some(info.lower_bound.map_or(*n + 1, |b| b.max(*n + 1)));
                        }
                    }
                    ("=", SymbolicValue::Symbolic { name, .. }, SymbolicValue::Concrete(lit)) => {
                        if expected {
                            let info = param_info.entry(name.clone()).or_default();
                            info.exact_values.push(lit.clone());
                        }
                    }
                    _ => {
                        // Recursively analyze nested expressions
                        self.analyze_constraint(left, expected, param_info);
                        self.analyze_constraint(right, expected, param_info);
                    }
                }
            }
            _ => {}
        }
    }
    
    /// Generate a value based on parameter information
    fn generate_value_from_info(&self, info: &ParameterInfo, variant: usize) -> Literal {
        // If we have exact values, use them
        if !info.exact_values.is_empty() {
            return info.exact_values[variant % info.exact_values.len()].clone();
        }
        
        // Generate values within bounds
        match (info.lower_bound, info.upper_bound) {
            (Some(lower), Some(upper)) => {
                // Generate values within range
                let range = upper - lower;
                if range > 0 {
                    Literal::Integer(lower + (variant as i64 % range))
                } else {
                    Literal::Integer(lower)
                }
            }
            (Some(lower), None) => {
                // Values above lower bound
                Literal::Integer(lower + variant as i64)
            }
            (None, Some(upper)) => {
                // Values below upper bound
                Literal::Integer(upper - variant as i64)
            }
            (None, None) => {
                // No bounds, use default heuristics
                Literal::Integer(variant as i64)
            }
        }
    }
}

/// Information about a parameter extracted from constraints
#[derive(Debug, Default)]
struct ParameterInfo {
    lower_bound: Option<i64>,
    upper_bound: Option<i64>,
    exact_values: Vec<Literal>,
    excluded_values: Vec<Literal>,
}

/// Format test cases as executable code
pub fn format_test_cases(
    test_cases: &[TestCase],
    function_name: &str,
    language: TestLanguage,
) -> String {
    match language {
        TestLanguage::ClaudeLang => format_claudelang_tests(test_cases, function_name),
        TestLanguage::Rust => format_rust_tests(test_cases, function_name),
    }
}

/// Supported test output languages
pub enum TestLanguage {
    ClaudeLang,
    Rust,
}

/// Format test cases as ClaudeLang code
fn format_claudelang_tests(test_cases: &[TestCase], function_name: &str) -> String {
    let mut output = String::new();
    output.push_str(";; Generated test cases for ");
    output.push_str(function_name);
    output.push_str("\n\n");
    
    for (i, test) in test_cases.iter().enumerate() {
        output.push_str(&format!(";; Test case {}: {}\n", i + 1, test.description));
        
        // Format the function call
        output.push_str(&format!("({}", function_name));
        for (param, value) in &test.inputs {
            output.push_str(&format!(" {}", format_literal(value)));
        }
        output.push_str(")\n");
        
        if let Some(expected) = &test.expected_output {
            output.push_str(&format!(";; Expected: {}\n", format_literal(expected)));
        }
        
        output.push_str("\n");
    }
    
    output
}

/// Format test cases as Rust unit tests
fn format_rust_tests(test_cases: &[TestCase], function_name: &str) -> String {
    let mut output = String::new();
    output.push_str("#[cfg(test)]\n");
    output.push_str("mod generated_tests {\n");
    output.push_str("    use super::*;\n\n");
    
    for (i, test) in test_cases.iter().enumerate() {
        output.push_str(&format!("    #[test]\n"));
        output.push_str(&format!("    fn test_{}_{}_generated() {{\n", function_name, i + 1));
        output.push_str(&format!("        // {}\n", test.description));
        
        // Format inputs
        for (param, value) in &test.inputs {
            output.push_str(&format!("        let {} = {};\n", param, format_literal_rust(value)));
        }
        
        // Format function call
        output.push_str(&format!("        let result = {}(", function_name));
        let params: Vec<String> = test.inputs.keys().cloned().collect();
        output.push_str(&params.join(", "));
        output.push_str(");\n");
        
        if let Some(expected) = &test.expected_output {
            output.push_str(&format!("        assert_eq!(result, {});\n", format_literal_rust(expected)));
        }
        
        output.push_str("    }\n\n");
    }
    
    output.push_str("}\n");
    output
}

/// Format a literal value for ClaudeLang
fn format_literal(lit: &Literal) -> String {
    match lit {
        Literal::Integer(n) => n.to_string(),
        Literal::Float(f) => f.to_string(),
        Literal::String(s) => format!("\"{}\"", s),
        Literal::Boolean(b) => if *b { "#t" } else { "#f" }.to_string(),
        Literal::Nil => "nil".to_string(),
    }
}

/// Format a literal value for Rust
fn format_literal_rust(lit: &Literal) -> String {
    match lit {
        Literal::Integer(n) => format!("{}i64", n),
        Literal::Float(f) => format!("{}f64", f),
        Literal::String(s) => format!("\"{}\"", s),
        Literal::Boolean(b) => b.to_string(),
        Literal::Nil => "None".to_string(),
    }
}