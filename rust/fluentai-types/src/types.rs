//! Core type definitions for FluentAi
//!
//! This module implements the type system including:
//! - Algebraic data types
//! - Effect types
//! - Probabilistic types
//! - Temporal types

use fluentai_core::ast::EffectType;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fmt;

/// Kinds of types in the type system
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeKind {
    /// Basic types like integers, floats, strings, booleans
    Primitive,
    /// Function types with parameters and return type
    Function,
    /// Fixed-size collections of heterogeneous types
    Tuple,
    /// Variable-size collections of homogeneous types
    List,
    /// Named fields with associated types
    Record,
    /// Sum types with tagged alternatives
    Variant,
    /// Effect types for computational effects
    Effect,
    /// Types with uncertainty/probability information
    Uncertain,
    /// Types with temporal constraints
    Temporal,
    /// Type variables for polymorphism
    TypeVariable,
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Primitive => write!(f, "Primitive"),
            TypeKind::Function => write!(f, "Function"),
            TypeKind::Tuple => write!(f, "Tuple"),
            TypeKind::List => write!(f, "List"),
            TypeKind::Record => write!(f, "Record"),
            TypeKind::Variant => write!(f, "Variant"),
            TypeKind::Effect => write!(f, "Effect"),
            TypeKind::Uncertain => write!(f, "Uncertain"),
            TypeKind::Temporal => write!(f, "Temporal"),
            TypeKind::TypeVariable => write!(f, "TypeVariable"),
        }
    }
}

/// Base trait for all types
pub trait TypeTrait {
    /// Get the kind of this type
    fn kind(&self) -> TypeKind;
    /// Get the effects associated with this type
    fn effects(&self) -> &HashSet<EffectType>;
    /// Check if this type has no effects (is pure)
    fn is_pure(&self) -> bool {
        self.effects().is_empty() || 
        (self.effects().len() == 1 && self.effects().contains(&EffectType::Pure))
    }
}

/// Base type structure
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Type {
    /// The kind of type (primitive, function, etc.)
    pub kind: TypeKind,
    /// Set of effects this type may perform
    pub effects: HashSet<EffectType>,
    /// Additional metadata for the type
    pub metadata: FxHashMap<String, String>,
}

impl Default for Type {
    fn default() -> Self {
        Self {
            kind: TypeKind::TypeVariable,
            effects: HashSet::new(),
            metadata: FxHashMap::default(),
        }
    }
}

/// Primitive types like Int, Float, String, Bool
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PrimitiveType {
    /// Name of the primitive type (e.g., "Int", "Float", "String", "Bool")
    pub name: String,
}

impl PrimitiveType {
    /// Create a new primitive type with the given name
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }

    /// Create an Int primitive type
    pub fn int() -> Self {
        Self::new("Int")
    }

    /// Create a Float primitive type
    pub fn float() -> Self {
        Self::new("Float")
    }

    /// Create a String primitive type
    pub fn string() -> Self {
        Self::new("String")
    }

    /// Create a Bool primitive type
    pub fn bool() -> Self {
        Self::new("Bool")
    }

    /// Create a Unit primitive type
    pub fn unit() -> Self {
        Self::new("Unit")
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// Function types with explicit effects
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionType {
    /// Parameter types for the function
    pub params: Vec<TypedValue>,
    /// Return type of the function
    pub result: Box<TypedValue>,
    /// Whether this function is pure (no side effects)
    pub is_pure: bool,
}

impl FunctionType {
    /// Create a new function type with the given parameters and return type
    pub fn new(params: Vec<TypedValue>, result: TypedValue) -> Self {
        Self {
            params,
            result: Box::new(result),
            is_pure: true,
        }
    }

    /// Create a TypedValue from this function type with the given effects
    pub fn with_effects(mut self, effects: HashSet<EffectType>) -> TypedValue {
        self.is_pure = effects.is_empty() || 
            (effects.len() == 1 && effects.contains(&EffectType::Pure));
        
        TypedValue {
            inner: TypedValueInner::Function(self),
            effects,
        }
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params_str = self.params.iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(" → ");
        write!(f, "{} → {}", params_str, self.result)
    }
}

/// Product types (tuples)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TupleType {
    /// Types of each element in the tuple
    pub elements: Vec<TypedValue>,
}

impl TupleType {
    /// Create a new tuple type with the given element types
    pub fn new(elements: Vec<TypedValue>) -> Self {
        Self { elements }
    }
}

impl fmt::Display for TupleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let elements_str = self.elements.iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "({})", elements_str)
    }
}

/// Homogeneous list type
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListType {
    /// Type of elements in the list
    pub element_type: Box<TypedValue>,
}

impl ListType {
    /// Create a new list type with the given element type
    pub fn new(element_type: TypedValue) -> Self {
        Self {
            element_type: Box::new(element_type),
        }
    }
}

impl fmt::Display for ListType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self.element_type)
    }
}

/// Record types with named fields
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordType {
    /// Map of field names to their types
    pub fields: FxHashMap<String, TypedValue>,
}

impl RecordType {
    /// Create a new empty record type
    pub fn new() -> Self {
        Self {
            fields: FxHashMap::default(),
        }
    }

    /// Add a field to the record type
    pub fn with_field(mut self, name: impl Into<String>, ty: TypedValue) -> Self {
        self.fields.insert(name.into(), ty);
        self
    }
}

impl fmt::Display for RecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fields_str = self.fields.iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{{{}}}", fields_str)
    }
}

/// Sum types (variants/enums)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariantType {
    /// Map of variant names to their optional payload types
    pub variants: FxHashMap<String, Option<TypedValue>>,
}

impl VariantType {
    /// Create a new empty variant type
    pub fn new() -> Self {
        Self {
            variants: FxHashMap::default(),
        }
    }

    /// Add a variant to the type
    pub fn with_variant(mut self, tag: impl Into<String>, payload: Option<TypedValue>) -> Self {
        self.variants.insert(tag.into(), payload);
        self
    }
}

impl fmt::Display for VariantType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let variants_str = self.variants.iter()
            .map(|(tag, payload)| {
                if let Some(p) = payload {
                    format!("{}({})", tag, p)
                } else {
                    tag.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join(" | ");
        write!(f, "<{}>", variants_str)
    }
}

/// Effect types for tracking side effects
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EffectTypeWrapper {
    /// The kind of effect (IO, State, etc.)
    pub effect_kind: EffectType,
    /// Optional payload type for the effect
    pub payload_type: Option<Box<TypedValue>>,
}

impl EffectTypeWrapper {
    /// Create a new effect type with the given kind
    pub fn new(effect_kind: EffectType) -> Self {
        Self {
            effect_kind,
            payload_type: None,
        }
    }

    /// Set the payload type for this effect
    pub fn with_payload(mut self, payload: TypedValue) -> Self {
        self.payload_type = Some(Box::new(payload));
        self
    }
}

impl fmt::Display for EffectTypeWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(payload) = &self.payload_type {
            write!(f, "Effect<{}, {}>", self.effect_kind, payload)
        } else {
            write!(f, "Effect<{}>", self.effect_kind)
        }
    }
}

/// Probabilistic types with confidence
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UncertainType {
    /// The base type that this is uncertain about
    pub base_type: Box<TypedValue>,
    /// Confidence level (0.0 to 1.0)
    pub confidence: f64,
    /// Name of the probability distribution
    pub distribution: String,
}

impl UncertainType {
    /// Create a new uncertain type with the given base type and confidence
    pub fn new(base_type: TypedValue, confidence: f64) -> Self {
        Self {
            base_type: Box::new(base_type),
            confidence,
            distribution: "uniform".to_string(),
        }
    }

    /// Set the probability distribution for this uncertain type
    pub fn with_distribution(mut self, distribution: impl Into<String>) -> Self {
        self.distribution = distribution.into();
        self
    }
}

impl fmt::Display for UncertainType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Uncertain<{}, {:.2}>", self.base_type, self.confidence)
    }
}

/// Temporal types with time constraints
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TemporalType {
    /// The base type that changes over time
    pub base_type: Box<TypedValue>,
    /// Temporal constraint expression
    pub constraint: String,
}

impl TemporalType {
    /// Create a new temporal type with the given base type and constraint
    pub fn new(base_type: TypedValue, constraint: impl Into<String>) -> Self {
        Self {
            base_type: Box::new(base_type),
            constraint: constraint.into(),
        }
    }
}

impl fmt::Display for TemporalType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Temporal<{}, {}>", self.base_type, self.constraint)
    }
}

/// Type variables for polymorphism
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeVariable {
    /// Name of the type variable (e.g., "T", "a")
    pub name: String,
    /// Constraints on this type variable
    pub constraints: Vec<TypeConstraint>,
}

impl TypeVariable {
    /// Create a new type variable with the given name
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            constraints: Vec::new(),
        }
    }

    /// Add a constraint to this type variable
    pub fn with_constraint(mut self, constraint: TypeConstraint) -> Self {
        self.constraints.push(constraint);
        self
    }
}

impl fmt::Display for TypeVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.constraints.is_empty() {
            write!(f, "{}", self.name)
        } else {
            let constraints_str = self.constraints.iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(" + ");
            write!(f, "{} where {}: {}", self.name, self.name, constraints_str)
        }
    }
}

/// Type constraints for type variables
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeConstraint {
    /// Type must have this trait/capability
    HasTrait(String),
    /// Type must be numeric
    Numeric,
    /// Type must be comparable
    Comparable,
    /// Type must be a subtype of another type
    SubtypeOf(String),
}

impl fmt::Display for TypeConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeConstraint::HasTrait(trait_name) => write!(f, "{}", trait_name),
            TypeConstraint::Numeric => write!(f, "Num"),
            TypeConstraint::Comparable => write!(f, "Ord"),
            TypeConstraint::SubtypeOf(ty) => write!(f, "<: {}", ty),
        }
    }
}

/// Inner type representation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypedValueInner {
    /// Primitive type variant
    Primitive(PrimitiveType),
    /// Function type variant
    Function(FunctionType),
    /// Tuple type variant
    Tuple(TupleType),
    /// List type variant
    List(ListType),
    /// Record type variant
    Record(RecordType),
    /// Variant type variant
    Variant(VariantType),
    /// Effect type variant
    Effect(EffectTypeWrapper),
    /// Uncertain type variant
    Uncertain(UncertainType),
    /// Temporal type variant
    Temporal(TemporalType),
    /// Type variable variant
    Variable(TypeVariable),
}

/// A typed value with effect information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypedValue {
    /// The inner type representation
    pub inner: TypedValueInner,
    /// Set of effects this type may perform
    pub effects: HashSet<EffectType>,
}

impl TypedValue {
    /// Create a primitive typed value
    pub fn primitive(prim: PrimitiveType) -> Self {
        Self {
            inner: TypedValueInner::Primitive(prim),
            effects: HashSet::new(),
        }
    }

    /// Create a function typed value with inferred effects
    pub fn function(func: FunctionType) -> Self {
        let mut effects = HashSet::new();
        
        // Collect effects from parameters
        for param in &func.params {
            effects.extend(param.effects.iter().copied());
        }
        
        // Collect effects from result
        effects.extend(func.result.effects.iter().copied());
        
        // Add state effect if not pure
        if !func.is_pure {
            effects.insert(EffectType::State);
        }
        
        Self {
            inner: TypedValueInner::Function(func),
            effects,
        }
    }

    /// Create a tuple typed value with combined effects
    pub fn tuple(tuple: TupleType) -> Self {
        let mut effects = HashSet::new();
        for elem in &tuple.elements {
            effects.extend(elem.effects.iter().copied());
        }
        
        Self {
            inner: TypedValueInner::Tuple(tuple),
            effects,
        }
    }

    /// Create a list typed value with element effects
    pub fn list(list: ListType) -> Self {
        let effects = list.element_type.effects.clone();
        Self {
            inner: TypedValueInner::List(list),
            effects,
        }
    }

    /// Create a record typed value with combined field effects
    pub fn record(record: RecordType) -> Self {
        let mut effects = HashSet::new();
        for field_type in record.fields.values() {
            effects.extend(field_type.effects.iter().copied());
        }
        
        Self {
            inner: TypedValueInner::Record(record),
            effects,
        }
    }

    /// Create a variant typed value with combined payload effects
    pub fn variant(variant: VariantType) -> Self {
        let mut effects = HashSet::new();
        for payload in variant.variants.values() {
            if let Some(p) = payload {
                effects.extend(p.effects.iter().copied());
            }
        }
        
        Self {
            inner: TypedValueInner::Variant(variant),
            effects,
        }
    }

    /// Create an effect typed value
    pub fn effect(effect: EffectTypeWrapper) -> Self {
        let mut effects = HashSet::new();
        effects.insert(effect.effect_kind);
        
        if let Some(payload) = &effect.payload_type {
            effects.extend(payload.effects.iter().copied());
        }
        
        Self {
            inner: TypedValueInner::Effect(effect),
            effects,
        }
    }

    /// Create an uncertain typed value (adds Random effect)
    pub fn uncertain(uncertain: UncertainType) -> Self {
        let mut effects = uncertain.base_type.effects.clone();
        effects.insert(EffectType::Random);
        
        Self {
            inner: TypedValueInner::Uncertain(uncertain),
            effects,
        }
    }

    /// Create a temporal typed value (adds Time effect)
    pub fn temporal(temporal: TemporalType) -> Self {
        let mut effects = temporal.base_type.effects.clone();
        effects.insert(EffectType::Time);
        
        Self {
            inner: TypedValueInner::Temporal(temporal),
            effects,
        }
    }

    /// Create a type variable typed value
    pub fn variable(var: TypeVariable) -> Self {
        Self {
            inner: TypedValueInner::Variable(var),
            effects: HashSet::new(),
        }
    }

    /// Get the kind of this type
    pub fn kind(&self) -> TypeKind {
        match &self.inner {
            TypedValueInner::Primitive(_) => TypeKind::Primitive,
            TypedValueInner::Function(_) => TypeKind::Function,
            TypedValueInner::Tuple(_) => TypeKind::Tuple,
            TypedValueInner::List(_) => TypeKind::List,
            TypedValueInner::Record(_) => TypeKind::Record,
            TypedValueInner::Variant(_) => TypeKind::Variant,
            TypedValueInner::Effect(_) => TypeKind::Effect,
            TypedValueInner::Uncertain(_) => TypeKind::Uncertain,
            TypedValueInner::Temporal(_) => TypeKind::Temporal,
            TypedValueInner::Variable(_) => TypeKind::TypeVariable,
        }
    }

    /// Check if this type is pure (has no effects)
    pub fn is_pure(&self) -> bool {
        self.effects.is_empty() || 
        (self.effects.len() == 1 && self.effects.contains(&EffectType::Pure))
    }

    /// Set the effects for this type
    pub fn with_effects(mut self, effects: HashSet<EffectType>) -> Self {
        self.effects = effects;
        self
    }

    /// Add an effect to this type
    pub fn add_effect(mut self, effect: EffectType) -> Self {
        // If we add any non-pure effect, remove Pure
        if effect != EffectType::Pure && self.effects.contains(&EffectType::Pure) {
            self.effects.remove(&EffectType::Pure);
        }
        self.effects.insert(effect);
        self
    }
}

impl fmt::Display for TypedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let type_str = match &self.inner {
            TypedValueInner::Primitive(p) => p.to_string(),
            TypedValueInner::Function(func) => func.to_string(),
            TypedValueInner::Tuple(t) => t.to_string(),
            TypedValueInner::List(l) => l.to_string(),
            TypedValueInner::Record(r) => r.to_string(),
            TypedValueInner::Variant(v) => v.to_string(),
            TypedValueInner::Effect(e) => e.to_string(),
            TypedValueInner::Uncertain(u) => u.to_string(),
            TypedValueInner::Temporal(t) => t.to_string(),
            TypedValueInner::Variable(v) => v.to_string(),
        };

        if !self.is_pure() {
            let effects_str = self.effects.iter()
                .filter(|e| **e != EffectType::Pure)
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{} ~{{{}}}", type_str, effects_str)
        } else {
            write!(f, "{}", type_str)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_types() {
        let int_type = TypedValue::primitive(PrimitiveType::int());
        assert_eq!(int_type.kind(), TypeKind::Primitive);
        assert!(int_type.is_pure());
        assert_eq!(int_type.to_string(), "Int");
    }

    #[test]
    fn test_function_types() {
        let func_type = FunctionType::new(
            vec![
                TypedValue::primitive(PrimitiveType::int()),
                TypedValue::primitive(PrimitiveType::int()),
            ],
            TypedValue::primitive(PrimitiveType::int()),
        );
        
        let typed_func = TypedValue::function(func_type);
        assert_eq!(typed_func.kind(), TypeKind::Function);
        assert_eq!(typed_func.to_string(), "Int → Int → Int");
    }

    #[test]
    fn test_list_types() {
        let list_type = ListType::new(TypedValue::primitive(PrimitiveType::string()));
        let typed_list = TypedValue::list(list_type);
        
        assert_eq!(typed_list.kind(), TypeKind::List);
        assert_eq!(typed_list.to_string(), "[String]");
    }

    #[test]
    fn test_effect_types() {
        let io_func = FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::string())],
            TypedValue::primitive(PrimitiveType::unit()),
        );
        
        let typed_func = TypedValue::function(io_func)
            .add_effect(EffectType::IO);
        
        assert!(!typed_func.is_pure());
        assert_eq!(typed_func.to_string(), "String → Unit ~{IO}");
    }

    #[test]
    fn test_type_variables() {
        let type_var = TypeVariable::new("T")
            .with_constraint(TypeConstraint::Numeric);
        
        let typed_var = TypedValue::variable(type_var);
        assert_eq!(typed_var.kind(), TypeKind::TypeVariable);
        assert_eq!(typed_var.to_string(), "T where T: Num");
    }
}

#[cfg(test)]
#[path = "types_tests.rs"]
mod types_tests;

#[cfg(test)]
#[path = "types_edge_tests.rs"]
mod types_edge_tests;