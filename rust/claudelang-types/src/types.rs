//! Core type definitions for ClaudeLang
//!
//! This module implements the type system including:
//! - Algebraic data types
//! - Effect types
//! - Probabilistic types
//! - Temporal types

use claudelang_core::ast::EffectType;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fmt;

/// Kinds of types in the type system
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeKind {
    Primitive,
    Function,
    Tuple,
    List,
    Record,
    Variant,
    Effect,
    Uncertain,
    Temporal,
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
    fn kind(&self) -> TypeKind;
    fn effects(&self) -> &HashSet<EffectType>;
    fn is_pure(&self) -> bool {
        self.effects().is_empty() || 
        (self.effects().len() == 1 && self.effects().contains(&EffectType::Pure))
    }
}

/// Base type structure
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Type {
    pub kind: TypeKind,
    pub effects: HashSet<EffectType>,
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
    pub name: String,
}

impl PrimitiveType {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }

    pub fn int() -> Self {
        Self::new("Int")
    }

    pub fn float() -> Self {
        Self::new("Float")
    }

    pub fn string() -> Self {
        Self::new("String")
    }

    pub fn bool() -> Self {
        Self::new("Bool")
    }

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
    pub params: Vec<TypedValue>,
    pub result: Box<TypedValue>,
    pub is_pure: bool,
}

impl FunctionType {
    pub fn new(params: Vec<TypedValue>, result: TypedValue) -> Self {
        Self {
            params,
            result: Box::new(result),
            is_pure: true,
        }
    }

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
    pub elements: Vec<TypedValue>,
}

impl TupleType {
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
    pub element_type: Box<TypedValue>,
}

impl ListType {
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
    pub fields: FxHashMap<String, TypedValue>,
}

impl RecordType {
    pub fn new() -> Self {
        Self {
            fields: FxHashMap::default(),
        }
    }

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
    pub variants: FxHashMap<String, Option<TypedValue>>,
}

impl VariantType {
    pub fn new() -> Self {
        Self {
            variants: FxHashMap::default(),
        }
    }

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
    pub effect_kind: EffectType,
    pub payload_type: Option<Box<TypedValue>>,
}

impl EffectTypeWrapper {
    pub fn new(effect_kind: EffectType) -> Self {
        Self {
            effect_kind,
            payload_type: None,
        }
    }

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
    pub base_type: Box<TypedValue>,
    pub confidence: f64,
    pub distribution: String,
}

impl UncertainType {
    pub fn new(base_type: TypedValue, confidence: f64) -> Self {
        Self {
            base_type: Box::new(base_type),
            confidence,
            distribution: "uniform".to_string(),
        }
    }

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
    pub base_type: Box<TypedValue>,
    pub constraint: String,
}

impl TemporalType {
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
    pub name: String,
    pub constraints: Vec<TypeConstraint>,
}

impl TypeVariable {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            constraints: Vec::new(),
        }
    }

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
    Primitive(PrimitiveType),
    Function(FunctionType),
    Tuple(TupleType),
    List(ListType),
    Record(RecordType),
    Variant(VariantType),
    Effect(EffectTypeWrapper),
    Uncertain(UncertainType),
    Temporal(TemporalType),
    Variable(TypeVariable),
}

/// A typed value with effect information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypedValue {
    pub inner: TypedValueInner,
    pub effects: HashSet<EffectType>,
}

impl TypedValue {
    pub fn primitive(prim: PrimitiveType) -> Self {
        Self {
            inner: TypedValueInner::Primitive(prim),
            effects: HashSet::new(),
        }
    }

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

    pub fn list(list: ListType) -> Self {
        let effects = list.element_type.effects.clone();
        Self {
            inner: TypedValueInner::List(list),
            effects,
        }
    }

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

    pub fn uncertain(uncertain: UncertainType) -> Self {
        let mut effects = uncertain.base_type.effects.clone();
        effects.insert(EffectType::Random);
        
        Self {
            inner: TypedValueInner::Uncertain(uncertain),
            effects,
        }
    }

    pub fn temporal(temporal: TemporalType) -> Self {
        let mut effects = temporal.base_type.effects.clone();
        effects.insert(EffectType::Time);
        
        Self {
            inner: TypedValueInner::Temporal(temporal),
            effects,
        }
    }

    pub fn variable(var: TypeVariable) -> Self {
        Self {
            inner: TypedValueInner::Variable(var),
            effects: HashSet::new(),
        }
    }

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

    pub fn is_pure(&self) -> bool {
        self.effects.is_empty() || 
        (self.effects.len() == 1 && self.effects.contains(&EffectType::Pure))
    }

    pub fn with_effects(mut self, effects: HashSet<EffectType>) -> Self {
        self.effects = effects;
        self
    }

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