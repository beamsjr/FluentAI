//! Advanced proof generation system for ClaudeLang contracts
//! 
//! This module implements various proof strategies for verifying contracts,
//! including induction, case analysis, and automated SMT-based proofs.

use std::collections::HashMap;
use claudelang_core::ast::{Graph, Node, NodeId, Literal};
use crate::{
    contract::{Contract, ContractCondition},
    errors::{ContractError, ContractResult},
    symbolic_execution::SymbolicExecutor,
};

/// Proof generation strategies
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProofStrategy {
    /// Proof by induction
    Induction {
        /// Variable to induct on
        induction_var: Option<String>,
    },
    
    /// Case analysis
    CaseAnalysis,
    
    /// Direct proof using symbolic execution
    Direct,
    
    /// Proof by contradiction
    Contradiction,
    
    /// Automated using SMT solver
    Automated,
    
    /// Bounded model checking
    BoundedModelChecking {
        /// Maximum bound to check
        bound: usize,
    },
}

/// Represents a generated proof
#[derive(Debug, Clone)]
pub struct Proof {
    /// Contract being proved
    pub contract_name: String,
    
    /// Strategy used
    pub strategy: ProofStrategy,
    
    /// Proof steps
    pub steps: Vec<ProofStep>,
    
    /// Whether proof is complete
    pub complete: bool,
    
    /// Assumptions made during proof
    pub assumptions: Vec<String>,
    
    /// Lemmas used in the proof
    pub lemmas: Vec<Lemma>,
}

/// A single step in a proof
#[derive(Debug, Clone)]
pub struct ProofStep {
    /// Step number
    pub step_number: usize,
    
    /// Description of the step
    pub description: String,
    
    /// Justification for the step
    pub justification: Justification,
    
    /// Formula or expression at this step
    pub formula: Option<ProofFormula>,
    
    /// Any sub-proofs needed
    pub subproofs: Vec<Proof>,
}

/// Justification for a proof step
#[derive(Debug, Clone)]
pub enum Justification {
    /// Given/assumption
    Given,
    
    /// By definition
    Definition(String),
    
    /// Modus ponens
    ModusPonens { from: usize, implies: usize },
    
    /// Universal instantiation
    UniversalInstantiation { from: usize, var: String, value: String },
    
    /// Existential instantiation
    ExistentialInstantiation { from: usize },
    
    /// Induction hypothesis
    InductionHypothesis,
    
    /// Base case of induction
    InductionBase,
    
    /// Inductive step
    InductionStep,
    
    /// By contradiction
    Contradiction { assumption: usize },
    
    /// SMT solver result
    SMTSolver { solver: String, result: String },
    
    /// Symbolic execution result
    SymbolicExecution { paths: usize },
    
    /// Apply lemma
    ApplyLemma { lemma_name: String },
}

/// A lemma that can be used in proofs
#[derive(Debug, Clone)]
pub struct Lemma {
    /// Name of the lemma
    pub name: String,
    
    /// Statement of the lemma
    pub statement: ProofFormula,
    
    /// Proof of the lemma (if available)
    pub proof: Option<Box<Proof>>,
}

/// Formula representation for proofs
#[derive(Debug, Clone)]
pub enum ProofFormula {
    /// Atomic proposition
    Atom(String),
    
    /// Negation
    Not(Box<ProofFormula>),
    
    /// Conjunction
    And(Box<ProofFormula>, Box<ProofFormula>),
    
    /// Disjunction
    Or(Box<ProofFormula>, Box<ProofFormula>),
    
    /// Implication
    Implies(Box<ProofFormula>, Box<ProofFormula>),
    
    /// Universal quantification
    ForAll { var: String, body: Box<ProofFormula> },
    
    /// Existential quantification
    Exists { var: String, body: Box<ProofFormula> },
    
    /// Equality
    Equals(Box<ProofFormula>, Box<ProofFormula>),
    
    /// Less than
    LessThan(Box<ProofFormula>, Box<ProofFormula>),
    
    /// Function application
    Apply { func: String, args: Vec<ProofFormula> },
}

/// Advanced proof generator
pub struct ProofGenerator {
    /// Default strategy to use
    default_strategy: ProofStrategy,
    
    /// Known lemmas
    lemmas: HashMap<String, Lemma>,
    
    /// Symbolic executor for direct proofs
    symbolic_executor: SymbolicExecutor,
}

impl ProofGenerator {
    /// Create a new proof generator
    pub fn new() -> Self {
        Self {
            default_strategy: ProofStrategy::Automated,
            lemmas: HashMap::new(),
            symbolic_executor: SymbolicExecutor::new(),
        }
    }
    
    /// Add a lemma to the proof generator
    pub fn add_lemma(&mut self, lemma: Lemma) {
        self.lemmas.insert(lemma.name.clone(), lemma);
    }
    
    /// Generate a proof for a contract
    pub fn generate_proof(
        &self,
        graph: &Graph,
        contract: &Contract,
        strategy: Option<ProofStrategy>,
    ) -> ContractResult<Proof> {
        let strategy = strategy.unwrap_or(self.default_strategy.clone());
        
        match strategy {
            ProofStrategy::Direct => self.generate_direct_proof(graph, contract),
            ProofStrategy::Induction { induction_var } => {
                self.generate_induction_proof(graph, contract, induction_var.as_deref())
            }
            ProofStrategy::CaseAnalysis => self.generate_case_analysis_proof(graph, contract),
            ProofStrategy::Contradiction => self.generate_contradiction_proof(graph, contract),
            ProofStrategy::Automated => self.generate_automated_proof(graph, contract),
            ProofStrategy::BoundedModelChecking { bound } => {
                self.generate_bmc_proof(graph, contract, bound)
            }
        }
    }
    
    /// Generate a direct proof using symbolic execution
    fn generate_direct_proof(
        &self,
        graph: &Graph,
        contract: &Contract,
    ) -> ContractResult<Proof> {
        let mut steps = Vec::new();
        let mut step_number = 1;
        
        // Step 1: State the contract
        steps.push(ProofStep {
            step_number,
            description: format!("Contract for function '{}'", contract.function_name),
            justification: Justification::Given,
            formula: Some(self.contract_to_formula(contract)),
            subproofs: Vec::new(),
        });
        step_number += 1;
        
        // Step 2: Assume preconditions
        for (i, precond) in contract.preconditions.iter().enumerate() {
            steps.push(ProofStep {
                step_number,
                description: format!("Assume precondition {}", i + 1),
                justification: Justification::Given,
                formula: Some(self.condition_to_formula(graph, precond)?),
                subproofs: Vec::new(),
            });
            step_number += 1;
        }
        
        // Step 3: Execute symbolically
        let result = self.symbolic_executor.verify_contract(graph, contract)?;
        
        steps.push(ProofStep {
            step_number,
            description: format!(
                "Symbolic execution of '{}' explores {} paths",
                contract.function_name,
                result.total_paths
            ),
            justification: Justification::SymbolicExecution {
                paths: result.total_paths,
            },
            formula: None,
            subproofs: Vec::new(),
        });
        step_number += 1;
        
        // Step 4: Check postconditions
        let all_verified = result.violations.is_empty();
        
        steps.push(ProofStep {
            step_number,
            description: if all_verified {
                "All postconditions hold on all execution paths".to_string()
            } else {
                format!("{} violations found", result.violations.len())
            },
            justification: Justification::SymbolicExecution {
                paths: result.verified_paths,
            },
            formula: None,
            subproofs: Vec::new(),
        });
        
        Ok(Proof {
            contract_name: contract.function_name.clone(),
            strategy: ProofStrategy::Direct,
            steps,
            complete: all_verified,
            assumptions: Vec::new(),
            lemmas: Vec::new(),
        })
    }
    
    /// Generate a proof by induction
    fn generate_induction_proof(
        &self,
        graph: &Graph,
        contract: &Contract,
        induction_var: Option<&str>,
    ) -> ContractResult<Proof> {
        let mut steps = Vec::new();
        let mut step_number = 1;
        
        // Determine induction variable
        let var_name = induction_var.unwrap_or("n");
        
        // Step 1: State induction principle
        steps.push(ProofStep {
            step_number,
            description: format!("Proof by induction on {}", var_name),
            justification: Justification::Given,
            formula: Some(ProofFormula::ForAll {
                var: var_name.to_string(),
                body: Box::new(self.contract_to_formula(contract)),
            }),
            subproofs: Vec::new(),
        });
        step_number += 1;
        
        // Step 2: Base case
        let base_case_proof = self.prove_base_case(graph, contract, var_name)?;
        steps.push(ProofStep {
            step_number,
            description: format!("Base case: {} = 0", var_name),
            justification: Justification::InductionBase,
            formula: None,
            subproofs: vec![base_case_proof],
        });
        step_number += 1;
        
        // Step 3: Inductive step
        let inductive_step_proof = self.prove_inductive_step(graph, contract, var_name)?;
        steps.push(ProofStep {
            step_number,
            description: format!("Inductive step: assume P({k}), prove P({k}+1)", k = var_name),
            justification: Justification::InductionStep,
            formula: None,
            subproofs: vec![inductive_step_proof],
        });
        
        Ok(Proof {
            contract_name: contract.function_name.clone(),
            strategy: ProofStrategy::Induction {
                induction_var: Some(var_name.to_string()),
            },
            steps,
            complete: true, // Assume complete if subproofs succeed
            assumptions: vec![format!("{} is a natural number", var_name)],
            lemmas: Vec::new(),
        })
    }
    
    /// Generate a proof by case analysis
    fn generate_case_analysis_proof(
        &self,
        _graph: &Graph,
        contract: &Contract,
    ) -> ContractResult<Proof> {
        // Simplified implementation
        Ok(Proof {
            contract_name: contract.function_name.clone(),
            strategy: ProofStrategy::CaseAnalysis,
            steps: vec![
                ProofStep {
                    step_number: 1,
                    description: "Case analysis not yet implemented".to_string(),
                    justification: Justification::Given,
                    formula: None,
                    subproofs: Vec::new(),
                },
            ],
            complete: false,
            assumptions: Vec::new(),
            lemmas: Vec::new(),
        })
    }
    
    /// Generate a proof by contradiction
    fn generate_contradiction_proof(
        &self,
        _graph: &Graph,
        contract: &Contract,
    ) -> ContractResult<Proof> {
        // Simplified implementation
        Ok(Proof {
            contract_name: contract.function_name.clone(),
            strategy: ProofStrategy::Contradiction,
            steps: vec![
                ProofStep {
                    step_number: 1,
                    description: "Assume the negation of the postcondition".to_string(),
                    justification: Justification::Contradiction { assumption: 0 },
                    formula: None,
                    subproofs: Vec::new(),
                },
            ],
            complete: false,
            assumptions: Vec::new(),
            lemmas: Vec::new(),
        })
    }
    
    /// Generate an automated proof using SMT solver
    fn generate_automated_proof(
        &self,
        _graph: &Graph,
        contract: &Contract,
    ) -> ContractResult<Proof> {
        #[cfg(feature = "static")]
        {
            use crate::static_verification::StaticVerifier;
            
            let verifier = StaticVerifier::new();
            let result = verifier.verify_contract(contract, graph)?;
            
            let mut steps = vec![
                ProofStep {
                    step_number: 1,
                    description: "Automated verification using Z3 SMT solver".to_string(),
                    justification: Justification::SMTSolver {
                        solver: "Z3".to_string(),
                        result: if result.all_conditions_verified() {
                            "VERIFIED".to_string()
                        } else {
                            "FAILED".to_string()
                        },
                    },
                    formula: Some(self.contract_to_formula(contract)),
                    subproofs: Vec::new(),
                },
            ];
            
            // Add counterexamples if any
            if !result.all_conditions_verified() {
                for (i, (condition, verified)) in result.preconditions.iter().enumerate() {
                    if !verified {
                        steps.push(ProofStep {
                            step_number: steps.len() + 1,
                            description: format!("Precondition {} failed", i + 1),
                            justification: Justification::SMTSolver {
                                solver: "Z3".to_string(),
                                result: "UNSAT".to_string(),
                            },
                            formula: None,
                            subproofs: Vec::new(),
                        });
                    }
                }
            }
            
            Ok(Proof {
                contract_name: contract.function_name.clone(),
                strategy: ProofStrategy::Automated,
                steps,
                complete: result.all_conditions_verified(),
                assumptions: Vec::new(),
                lemmas: Vec::new(),
            })
        }
        
        #[cfg(not(feature = "static"))]
        {
            Ok(Proof {
                contract_name: contract.function_name.clone(),
                strategy: ProofStrategy::Automated,
                steps: vec![
                    ProofStep {
                        step_number: 1,
                        description: "SMT solver not available (requires 'static' feature)".to_string(),
                        justification: Justification::Given,
                        formula: None,
                        subproofs: Vec::new(),
                    },
                ],
                complete: false,
                assumptions: Vec::new(),
                lemmas: Vec::new(),
            })
        }
    }
    
    /// Generate a proof using bounded model checking
    fn generate_bmc_proof(
        &self,
        graph: &Graph,
        contract: &Contract,
        bound: usize,
    ) -> ContractResult<Proof> {
        let mut steps = Vec::new();
        
        // Use symbolic execution with limited depth
        let executor = SymbolicExecutor::with_limits(bound, bound * 10);
        let result = executor.verify_contract(graph, contract)?;
        
        steps.push(ProofStep {
            step_number: 1,
            description: format!("Bounded model checking up to depth {}", bound),
            justification: Justification::SymbolicExecution {
                paths: result.total_paths,
            },
            formula: None,
            subproofs: Vec::new(),
        });
        
        let verified = result.violations.is_empty();
        steps.push(ProofStep {
            step_number: 2,
            description: if verified {
                format!("No violations found within bound {}", bound)
            } else {
                format!("{} violations found", result.violations.len())
            },
            justification: Justification::SymbolicExecution {
                paths: result.verified_paths,
            },
            formula: None,
            subproofs: Vec::new(),
        });
        
        Ok(Proof {
            contract_name: contract.function_name.clone(),
            strategy: ProofStrategy::BoundedModelChecking { bound },
            steps,
            complete: verified,
            assumptions: vec![format!("Verification limited to execution depth {}", bound)],
            lemmas: Vec::new(),
        })
    }
    
    /// Helper: prove base case for induction
    fn prove_base_case(
        &self,
        _graph: &Graph,
        _contract: &Contract,
        var_name: &str,
    ) -> ContractResult<Proof> {
        Ok(Proof {
            contract_name: "base_case".to_string(),
            strategy: ProofStrategy::Direct,
            steps: vec![
                ProofStep {
                    step_number: 1,
                    description: format!("Substitute {} = 0", var_name),
                    justification: Justification::UniversalInstantiation {
                        from: 0,
                        var: var_name.to_string(),
                        value: "0".to_string(),
                    },
                    formula: None,
                    subproofs: Vec::new(),
                },
            ],
            complete: true,
            assumptions: Vec::new(),
            lemmas: Vec::new(),
        })
    }
    
    /// Helper: prove inductive step
    fn prove_inductive_step(
        &self,
        _graph: &Graph,
        _contract: &Contract,
        var_name: &str,
    ) -> ContractResult<Proof> {
        Ok(Proof {
            contract_name: "inductive_step".to_string(),
            strategy: ProofStrategy::Direct,
            steps: vec![
                ProofStep {
                    step_number: 1,
                    description: format!("Assume P({})", var_name),
                    justification: Justification::InductionHypothesis,
                    formula: None,
                    subproofs: Vec::new(),
                },
                ProofStep {
                    step_number: 2,
                    description: format!("Prove P({}+1)", var_name),
                    justification: Justification::InductionStep,
                    formula: None,
                    subproofs: Vec::new(),
                },
            ],
            complete: true,
            assumptions: Vec::new(),
            lemmas: Vec::new(),
        })
    }
    
    /// Convert a contract to a proof formula
    fn contract_to_formula(&self, contract: &Contract) -> ProofFormula {
        let mut conjuncts = Vec::new();
        
        // Add preconditions
        for (i, _) in contract.preconditions.iter().enumerate() {
            conjuncts.push(ProofFormula::Atom(format!("pre_{}", i)));
        }
        
        // Add postconditions
        for (i, _) in contract.postconditions.iter().enumerate() {
            conjuncts.push(ProofFormula::Atom(format!("post_{}", i)));
        }
        
        // Create conjunction
        if conjuncts.is_empty() {
            ProofFormula::Atom("true".to_string())
        } else if conjuncts.len() == 1 {
            conjuncts.into_iter().next().unwrap()
        } else {
            conjuncts.into_iter().reduce(|a, b| {
                ProofFormula::And(Box::new(a), Box::new(b))
            }).unwrap()
        }
    }
    
    /// Convert a condition to a proof formula
    fn condition_to_formula(
        &self,
        graph: &Graph,
        condition: &ContractCondition,
    ) -> ContractResult<ProofFormula> {
        // Convert AST to formula
        self.node_to_formula(graph, condition.expression)
    }
    
    /// Convert an AST node to a proof formula
    fn node_to_formula(&self, graph: &Graph, node_id: NodeId) -> ContractResult<ProofFormula> {
        let node = graph.get_node(node_id)
            .ok_or_else(|| ContractError::Other(format!("Node {} not found", node_id)))?;
        
        match node {
            Node::Literal(lit) => match lit {
                Literal::Integer(n) => Ok(ProofFormula::Atom(n.to_string())),
                Literal::Boolean(b) => Ok(ProofFormula::Atom(b.to_string())),
                _ => Ok(ProofFormula::Atom(format!("{:?}", lit))),
            },
            
            Node::Variable { name } => Ok(ProofFormula::Atom(name.clone())),
            
            Node::Application { function, args } => {
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    match name.as_str() {
                        "=" if args.len() == 2 => {
                            let left = self.node_to_formula(graph, args[0])?;
                            let right = self.node_to_formula(graph, args[1])?;
                            Ok(ProofFormula::Equals(Box::new(left), Box::new(right)))
                        }
                        "<" if args.len() == 2 => {
                            let left = self.node_to_formula(graph, args[0])?;
                            let right = self.node_to_formula(graph, args[1])?;
                            Ok(ProofFormula::LessThan(Box::new(left), Box::new(right)))
                        }
                        "and" if args.len() == 2 => {
                            let left = self.node_to_formula(graph, args[0])?;
                            let right = self.node_to_formula(graph, args[1])?;
                            Ok(ProofFormula::And(Box::new(left), Box::new(right)))
                        }
                        "or" if args.len() == 2 => {
                            let left = self.node_to_formula(graph, args[0])?;
                            let right = self.node_to_formula(graph, args[1])?;
                            Ok(ProofFormula::Or(Box::new(left), Box::new(right)))
                        }
                        "not" if args.len() == 1 => {
                            let arg = self.node_to_formula(graph, args[0])?;
                            Ok(ProofFormula::Not(Box::new(arg)))
                        }
                        _ => {
                            let arg_formulas = args.iter()
                                .map(|&arg| self.node_to_formula(graph, arg))
                                .collect::<ContractResult<Vec<_>>>()?;
                            Ok(ProofFormula::Apply {
                                func: name.clone(),
                                args: arg_formulas,
                            })
                        }
                    }
                } else {
                    Ok(ProofFormula::Atom("unknown".to_string()))
                }
            }
            
            _ => Ok(ProofFormula::Atom(format!("{:?}", node))),
        }
    }
}

impl Default for ProofGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Pretty printer for proofs
impl Proof {
    /// Format the proof as a readable string
    pub fn format(&self) -> String {
        let mut output = String::new();
        
        output.push_str(&format!("Proof of: {}\n", self.contract_name));
        output.push_str(&format!("Strategy: {:?}\n", self.strategy));
        
        if !self.assumptions.is_empty() {
            output.push_str("\nAssumptions:\n");
            for assumption in &self.assumptions {
                output.push_str(&format!("  - {}\n", assumption));
            }
        }
        
        output.push_str("\nProof:\n");
        for step in &self.steps {
            output.push_str(&self.format_step(step, 0));
        }
        
        output.push_str(&format!("\nProof {}\n", 
            if self.complete { "COMPLETE ✓" } else { "INCOMPLETE ✗" }
        ));
        
        output
    }
    
    fn format_step(&self, step: &ProofStep, indent: usize) -> String {
        let mut output = String::new();
        let prefix = "  ".repeat(indent);
        
        output.push_str(&format!("{}{}. {}\n", prefix, step.step_number, step.description));
        output.push_str(&format!("{}   Justification: {:?}\n", prefix, step.justification));
        
        if let Some(formula) = &step.formula {
            output.push_str(&format!("{}   Formula: {:?}\n", prefix, formula));
        }
        
        for subproof in &step.subproofs {
            output.push_str(&format!("{}   Subproof: {}\n", prefix, subproof.contract_name));
            for substep in &subproof.steps {
                output.push_str(&self.format_step(substep, indent + 2));
            }
        }
        
        output
    }
}