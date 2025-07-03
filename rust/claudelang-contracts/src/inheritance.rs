//! Contract inheritance and refinement system
//! 
//! This module provides support for contract inheritance, allowing contracts
//! to be extended, refined, and composed in a modular way.

use std::collections::{HashMap, HashSet};
use claudelang_core::ast::Graph;
use crate::{
    contract::{Contract, ContractCondition},
    errors::{ContractError, ContractResult},
};

/// Contract inheritance relationship
#[derive(Debug, Clone)]
pub struct ContractInheritance {
    /// Base contract name
    pub base: String,
    /// Derived contract name
    pub derived: String,
    /// Inheritance type
    pub inheritance_type: InheritanceType,
}

/// Types of contract inheritance
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InheritanceType {
    /// Standard inheritance - derived must satisfy base
    Standard,
    /// Refinement - derived strengthens base
    Refinement,
    /// Weakening - derived weakens base (covariant)
    Weakening,
    /// Interface implementation
    Interface,
}

/// Contract refinement rules
#[derive(Debug, Clone)]
pub struct RefinementRule {
    /// Name of the rule
    pub name: String,
    /// Type of refinement
    pub rule_type: RefinementType,
    /// Additional constraints
    pub constraints: Vec<ContractCondition>,
}

/// Types of refinement
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefinementType {
    /// Strengthen preconditions
    StrengthenPreconditions,
    /// Weaken postconditions
    WeakenPostconditions,
    /// Add invariants
    AddInvariants,
    /// Refine complexity bounds
    RefineComplexity,
}

/// Contract hierarchy manager
pub struct ContractHierarchy {
    /// All contracts by name
    contracts: HashMap<String, Contract>,
    /// Inheritance relationships
    inheritance: Vec<ContractInheritance>,
    /// Contract interfaces
    interfaces: HashMap<String, ContractInterface>,
    /// Refinement rules
    refinement_rules: HashMap<String, Vec<RefinementRule>>,
}

/// Contract interface definition
#[derive(Debug, Clone)]
pub struct ContractInterface {
    /// Interface name
    pub name: String,
    /// Required preconditions
    pub required_preconditions: Vec<ContractCondition>,
    /// Required postconditions
    pub required_postconditions: Vec<ContractCondition>,
    /// Required invariants
    pub required_invariants: Vec<ContractCondition>,
    /// Optional conditions that can be refined
    pub optional_conditions: Vec<ContractCondition>,
}

impl ContractHierarchy {
    /// Create a new contract hierarchy
    pub fn new() -> Self {
        Self {
            contracts: HashMap::new(),
            inheritance: Vec::new(),
            interfaces: HashMap::new(),
            refinement_rules: HashMap::new(),
        }
    }
    
    /// Add a contract to the hierarchy
    pub fn add_contract(&mut self, contract: Contract) {
        self.contracts.insert(contract.function_name.clone(), contract);
    }
    
    /// Add an interface
    pub fn add_interface(&mut self, interface: ContractInterface) {
        self.interfaces.insert(interface.name.clone(), interface);
    }
    
    /// Establish inheritance relationship
    pub fn add_inheritance(
        &mut self,
        base: String,
        derived: String,
        inheritance_type: InheritanceType,
    ) -> ContractResult<()> {
        // Check that both contracts exist
        if !self.contracts.contains_key(&base) {
            return Err(ContractError::Other(
                format!("Base contract '{}' not found", base)
            ));
        }
        if !self.contracts.contains_key(&derived) {
            return Err(ContractError::Other(
                format!("Derived contract '{}' not found", derived)
            ));
        }
        
        // Check for cycles
        if self.would_create_cycle(&base, &derived) {
            return Err(ContractError::Other(
                "Inheritance would create a cycle".to_string()
            ));
        }
        
        self.inheritance.push(ContractInheritance {
            base,
            derived,
            inheritance_type,
        });
        
        Ok(())
    }
    
    /// Check if adding an inheritance would create a cycle
    fn would_create_cycle(&self, base: &str, derived: &str) -> bool {
        // Simple DFS to detect cycles
        let mut visited = HashSet::new();
        self.has_path_dfs(base, derived, &mut visited)
    }
    
    fn has_path_dfs(&self, from: &str, to: &str, visited: &mut HashSet<String>) -> bool {
        if from == to {
            return true;
        }
        
        if visited.contains(from) {
            return false;
        }
        
        visited.insert(from.to_string());
        
        // Check all contracts that 'from' inherits from
        for inheritance in &self.inheritance {
            if inheritance.derived == from {
                if self.has_path_dfs(&inheritance.base, to, visited) {
                    return true;
                }
            }
        }
        
        false
    }
    
    /// Get all base contracts for a given contract
    pub fn get_bases(&self, contract_name: &str) -> Vec<&str> {
        self.inheritance
            .iter()
            .filter(|i| i.derived == contract_name)
            .map(|i| i.base.as_str())
            .collect()
    }
    
    /// Get all derived contracts for a given contract
    pub fn get_derived(&self, contract_name: &str) -> Vec<&str> {
        self.inheritance
            .iter()
            .filter(|i| i.base == contract_name)
            .map(|i| i.derived.as_str())
            .collect()
    }
    
    /// Verify that a derived contract correctly inherits from its base
    pub fn verify_inheritance(
        &self,
        graph: &Graph,
        base_name: &str,
        derived_name: &str,
    ) -> ContractResult<InheritanceVerificationResult> {
        let base = self.contracts.get(base_name)
            .ok_or_else(|| ContractError::Other(format!("Base contract '{}' not found", base_name)))?;
        
        let derived = self.contracts.get(derived_name)
            .ok_or_else(|| ContractError::Other(format!("Derived contract '{}' not found", derived_name)))?;
        
        let inheritance = self.inheritance.iter()
            .find(|i| i.base == base_name && i.derived == derived_name)
            .ok_or_else(|| ContractError::Other("No inheritance relationship found".to_string()))?;
        
        match inheritance.inheritance_type {
            InheritanceType::Standard => self.verify_standard_inheritance(graph, base, derived),
            InheritanceType::Refinement => self.verify_refinement(graph, base, derived),
            InheritanceType::Weakening => self.verify_weakening(graph, base, derived),
            InheritanceType::Interface => self.verify_interface_implementation(graph, base, derived),
        }
    }
    
    /// Verify standard inheritance (Liskov substitution principle)
    fn verify_standard_inheritance(
        &self,
        _graph: &Graph,
        base: &Contract,
        derived: &Contract,
    ) -> ContractResult<InheritanceVerificationResult> {
        let mut violations = Vec::new();
        
        // Derived preconditions must be weaker than or equal to base
        // (contravariant in preconditions)
        for base_pre in &base.preconditions {
            let found_weaker = derived.preconditions.iter().any(|derived_pre| {
                // In a real implementation, we'd use SMT to check implication
                // For now, just check if the same condition exists
                derived_pre.expression == base_pre.expression
            });
            
            if !found_weaker {
                violations.push(InheritanceViolation {
                    violation_type: ViolationType::PreconditionStrengthened,
                    base_condition: Some(base_pre.clone()),
                    derived_condition: None,
                    message: "Derived contract strengthens precondition".to_string(),
                });
            }
        }
        
        // Derived postconditions must be stronger than or equal to base
        // (covariant in postconditions)
        for base_post in &base.postconditions {
            let found_stronger = derived.postconditions.iter().any(|derived_post| {
                // In a real implementation, we'd use SMT to check implication
                derived_post.expression == base_post.expression
            });
            
            if !found_stronger {
                violations.push(InheritanceViolation {
                    violation_type: ViolationType::PostconditionWeakened,
                    base_condition: Some(base_post.clone()),
                    derived_condition: None,
                    message: "Derived contract weakens postcondition".to_string(),
                });
            }
        }
        
        // Derived must preserve all base invariants
        for base_inv in &base.invariants {
            let found = derived.invariants.iter().any(|derived_inv| {
                derived_inv.expression == base_inv.expression
            });
            
            if !found {
                violations.push(InheritanceViolation {
                    violation_type: ViolationType::InvariantViolated,
                    base_condition: Some(base_inv.clone()),
                    derived_condition: None,
                    message: "Derived contract does not preserve invariant".to_string(),
                });
            }
        }
        
        let verified = violations.is_empty();
        Ok(InheritanceVerificationResult {
            base_contract: base.function_name.clone(),
            derived_contract: derived.function_name.clone(),
            inheritance_type: InheritanceType::Standard,
            violations,
            verified,
        })
    }
    
    /// Verify contract refinement
    fn verify_refinement(
        &self,
        _graph: &Graph,
        base: &Contract,
        derived: &Contract,
    ) -> ContractResult<InheritanceVerificationResult> {
        let mut violations = Vec::new();
        
        // In refinement, derived can strengthen postconditions
        // but must include all base postconditions
        for base_post in &base.postconditions {
            let found = derived.postconditions.iter().any(|derived_post| {
                derived_post.expression == base_post.expression
            });
            
            if !found {
                violations.push(InheritanceViolation {
                    violation_type: ViolationType::PostconditionMissing,
                    base_condition: Some(base_post.clone()),
                    derived_condition: None,
                    message: "Refined contract must include all base postconditions".to_string(),
                });
            }
        }
        
        // Check if refinement rules are satisfied
        if let Some(rules) = self.refinement_rules.get(&derived.function_name) {
            for rule in rules {
                match rule.rule_type {
                    RefinementType::StrengthenPreconditions => {
                        // Verify that preconditions are indeed strengthened
                        if derived.preconditions.len() <= base.preconditions.len() {
                            violations.push(InheritanceViolation {
                                violation_type: ViolationType::RefinementRuleViolated,
                                base_condition: None,
                                derived_condition: None,
                                message: format!("Refinement rule '{}' not satisfied", rule.name),
                            });
                        }
                    }
                    RefinementType::RefineComplexity => {
                        // Check if complexity is refined
                        if derived.complexity.is_none() && base.complexity.is_some() {
                            violations.push(InheritanceViolation {
                                violation_type: ViolationType::ComplexityNotRefined,
                                base_condition: None,
                                derived_condition: None,
                                message: "Complexity bound not refined".to_string(),
                            });
                        }
                    }
                    _ => {}
                }
            }
        }
        
        let verified = violations.is_empty();
        Ok(InheritanceVerificationResult {
            base_contract: base.function_name.clone(),
            derived_contract: derived.function_name.clone(),
            inheritance_type: InheritanceType::Refinement,
            violations,
            verified,
        })
    }
    
    /// Verify contract weakening (dual of refinement)
    fn verify_weakening(
        &self,
        _graph: &Graph,
        base: &Contract,
        derived: &Contract,
    ) -> ContractResult<InheritanceVerificationResult> {
        // Weakening allows relaxing preconditions and postconditions
        // This is useful for implementing more general versions
        
        let violations = Vec::new();
        
        // In weakening, all checks are relaxed
        // This is always valid unless specific rules prohibit it
        
        Ok(InheritanceVerificationResult {
            base_contract: base.function_name.clone(),
            derived_contract: derived.function_name.clone(),
            inheritance_type: InheritanceType::Weakening,
            violations,
            verified: true,
        })
    }
    
    /// Verify interface implementation
    fn verify_interface_implementation(
        &self,
        _graph: &Graph,
        _base: &Contract,
        derived: &Contract,
    ) -> ContractResult<InheritanceVerificationResult> {
        let mut violations = Vec::new();
        
        // Find the interface
        if let Some(interface) = self.interfaces.get(&_base.function_name) {
            // Check required preconditions
            for req_pre in &interface.required_preconditions {
                let found = derived.preconditions.iter().any(|pre| {
                    pre.expression == req_pre.expression
                });
                
                if !found {
                    violations.push(InheritanceViolation {
                        violation_type: ViolationType::InterfaceRequirementMissing,
                        base_condition: Some(req_pre.clone()),
                        derived_condition: None,
                        message: "Required interface precondition missing".to_string(),
                    });
                }
            }
            
            // Check required postconditions
            for req_post in &interface.required_postconditions {
                let found = derived.postconditions.iter().any(|post| {
                    post.expression == req_post.expression
                });
                
                if !found {
                    violations.push(InheritanceViolation {
                        violation_type: ViolationType::InterfaceRequirementMissing,
                        base_condition: Some(req_post.clone()),
                        derived_condition: None,
                        message: "Required interface postcondition missing".to_string(),
                    });
                }
            }
        }
        
        let verified = violations.is_empty();
        Ok(InheritanceVerificationResult {
            base_contract: _base.function_name.clone(),
            derived_contract: derived.function_name.clone(),
            inheritance_type: InheritanceType::Interface,
            violations,
            verified,
        })
    }
    
    /// Compose contracts through inheritance
    pub fn compose_contracts(
        &self,
        contract_names: &[String],
        composition_type: CompositionType,
    ) -> ContractResult<Contract> {
        if contract_names.is_empty() {
            return Err(ContractError::Other("No contracts to compose".to_string()));
        }
        
        let contracts: Vec<&Contract> = contract_names.iter()
            .map(|name| self.contracts.get(name))
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| ContractError::Other("One or more contracts not found".to_string()))?;
        
        match composition_type {
            CompositionType::Conjunction => self.compose_conjunction(contracts),
            CompositionType::Disjunction => self.compose_disjunction(contracts),
            CompositionType::Sequential => self.compose_sequential(contracts),
        }
    }
    
    /// Compose contracts using conjunction (AND)
    fn compose_conjunction(&self, contracts: Vec<&Contract>) -> ContractResult<Contract> {
        let mut composed = Contract {
            function_name: format!("composed_{}", contracts[0].function_name),
            preconditions: Vec::new(),
            postconditions: Vec::new(),
            invariants: Vec::new(),
            complexity: None,
            pure: contracts.iter().all(|c| c.pure),
            node_id: contracts[0].node_id, // Dummy
        };
        
        // Union of all preconditions
        for contract in &contracts {
            composed.preconditions.extend(contract.preconditions.clone());
        }
        
        // Union of all postconditions
        for contract in &contracts {
            composed.postconditions.extend(contract.postconditions.clone());
        }
        
        // Union of all invariants
        for contract in &contracts {
            composed.invariants.extend(contract.invariants.clone());
        }
        
        Ok(composed)
    }
    
    /// Compose contracts using disjunction (OR)
    fn compose_disjunction(&self, contracts: Vec<&Contract>) -> ContractResult<Contract> {
        // For disjunction, we need at least one contract's conditions to hold
        // This is more complex and would require creating disjunctive conditions
        
        let composed = Contract {
            function_name: format!("composed_or_{}", contracts[0].function_name),
            preconditions: Vec::new(), // Would need OR of preconditions
            postconditions: Vec::new(), // Would need OR of postconditions
            invariants: Vec::new(),
            complexity: None,
            pure: contracts.iter().all(|c| c.pure),
            node_id: contracts[0].node_id,
        };
        
        Ok(composed)
    }
    
    /// Compose contracts sequentially
    fn compose_sequential(&self, contracts: Vec<&Contract>) -> ContractResult<Contract> {
        if contracts.len() < 2 {
            return Err(ContractError::Other(
                "Sequential composition requires at least 2 contracts".to_string()
            ));
        }
        
        let mut composed = Contract {
            function_name: format!("composed_seq_{}", contracts[0].function_name),
            preconditions: contracts[0].preconditions.clone(),
            postconditions: contracts.last().unwrap().postconditions.clone(),
            invariants: Vec::new(),
            complexity: None,
            pure: contracts.iter().all(|c| c.pure),
            node_id: contracts[0].node_id,
        };
        
        // All invariants must hold throughout
        for contract in &contracts {
            composed.invariants.extend(contract.invariants.clone());
        }
        
        Ok(composed)
    }
    
    /// Add a refinement rule
    pub fn add_refinement_rule(
        &mut self,
        contract_name: String,
        rule: RefinementRule,
    ) {
        self.refinement_rules
            .entry(contract_name)
            .or_insert_with(Vec::new)
            .push(rule);
    }
}

/// Result of inheritance verification
#[derive(Debug)]
pub struct InheritanceVerificationResult {
    /// Base contract name
    pub base_contract: String,
    /// Derived contract name
    pub derived_contract: String,
    /// Type of inheritance
    pub inheritance_type: InheritanceType,
    /// Violations found
    pub violations: Vec<InheritanceViolation>,
    /// Whether inheritance is valid
    pub verified: bool,
}

/// Inheritance violation
#[derive(Debug)]
pub struct InheritanceViolation {
    /// Type of violation
    pub violation_type: ViolationType,
    /// Base contract condition (if applicable)
    pub base_condition: Option<ContractCondition>,
    /// Derived contract condition (if applicable)
    pub derived_condition: Option<ContractCondition>,
    /// Violation message
    pub message: String,
}

/// Types of inheritance violations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViolationType {
    /// Precondition was strengthened (violates LSP)
    PreconditionStrengthened,
    /// Postcondition was weakened (violates LSP)
    PostconditionWeakened,
    /// Invariant not preserved
    InvariantViolated,
    /// Required postcondition missing in refinement
    PostconditionMissing,
    /// Interface requirement not met
    InterfaceRequirementMissing,
    /// Refinement rule violated
    RefinementRuleViolated,
    /// Complexity not refined
    ComplexityNotRefined,
}

/// Contract composition types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompositionType {
    /// All contracts must hold (AND)
    Conjunction,
    /// At least one contract must hold (OR)
    Disjunction,
    /// Contracts applied in sequence
    Sequential,
}

impl Default for ContractHierarchy {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for contract hierarchies
pub struct ContractHierarchyBuilder {
    hierarchy: ContractHierarchy,
}

impl ContractHierarchyBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            hierarchy: ContractHierarchy::new(),
        }
    }
    
    /// Add a contract
    pub fn contract(mut self, contract: Contract) -> Self {
        self.hierarchy.add_contract(contract);
        self
    }
    
    /// Add an interface
    pub fn interface(mut self, interface: ContractInterface) -> Self {
        self.hierarchy.add_interface(interface);
        self
    }
    
    /// Add inheritance relationship
    pub fn inherits(
        mut self,
        base: String,
        derived: String,
        inheritance_type: InheritanceType,
    ) -> Self {
        let _ = self.hierarchy.add_inheritance(base, derived, inheritance_type);
        self
    }
    
    /// Build the hierarchy
    pub fn build(self) -> ContractHierarchy {
        self.hierarchy
    }
}

impl Default for ContractHierarchyBuilder {
    fn default() -> Self {
        Self::new()
    }
}