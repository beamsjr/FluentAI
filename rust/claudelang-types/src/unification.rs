//! Type unification for the Hindley-Milner type system

use crate::types::*;
use rustc_hash::FxHashMap;

/// Substitution mapping type variables to types
#[derive(Debug, Clone)]
pub struct Substitution {
    mapping: FxHashMap<String, TypedValue>,
}

impl Substitution {
    /// Create an empty substitution
    pub fn new() -> Self {
        Self {
            mapping: FxHashMap::default(),
        }
    }

    /// Add a mapping from variable to type
    pub fn insert(&mut self, var: String, ty: TypedValue) {
        self.mapping.insert(var, ty);
    }

    /// Apply substitution to a type
    pub fn apply_type(&self, ty: &TypedValue) -> TypedValue {
        match &ty.inner {
            TypedValueInner::Variable(var) => {
                if let Some(substituted) = self.mapping.get(&var.name) {
                    // Apply substitution recursively
                    self.apply_type(substituted)
                } else {
                    ty.clone()
                }
            }
            TypedValueInner::Function(func) => {
                let params = func.params.iter()
                    .map(|p| self.apply_type(p))
                    .collect();
                let result = self.apply_type(&func.result);
                TypedValue::function(FunctionType {
                    params,
                    result: Box::new(result),
                    is_pure: func.is_pure,
                })
            }
            TypedValueInner::Tuple(tuple) => {
                let elements = tuple.elements.iter()
                    .map(|e| self.apply_type(e))
                    .collect();
                TypedValue::tuple(TupleType { elements })
            }
            TypedValueInner::List(list) => {
                let element_type = self.apply_type(&list.element_type);
                TypedValue::list(ListType {
                    element_type: Box::new(element_type),
                })
            }
            TypedValueInner::Record(record) => {
                let fields = record.fields.iter()
                    .map(|(k, v)| (k.clone(), self.apply_type(v)))
                    .collect();
                TypedValue::record(RecordType { fields })
            }
            TypedValueInner::Variant(variant) => {
                let variants = variant.variants.iter()
                    .map(|(k, v)| {
                        let payload = v.as_ref().map(|p| self.apply_type(p));
                        (k.clone(), payload)
                    })
                    .collect();
                TypedValue::variant(VariantType { variants })
            }
            TypedValueInner::Uncertain(uncertain) => {
                let base_type = self.apply_type(&uncertain.base_type);
                TypedValue::uncertain(UncertainType {
                    base_type: Box::new(base_type),
                    confidence: uncertain.confidence,
                    distribution: uncertain.distribution.clone(),
                })
            }
            TypedValueInner::Temporal(temporal) => {
                let base_type = self.apply_type(&temporal.base_type);
                TypedValue::temporal(TemporalType {
                    base_type: Box::new(base_type),
                    constraint: temporal.constraint.clone(),
                })
            }
            TypedValueInner::Effect(effect) => {
                let payload_type = effect.payload_type.as_ref()
                    .map(|p| Box::new(self.apply_type(p)));
                TypedValue::effect(EffectTypeWrapper {
                    effect_kind: effect.effect_kind,
                    payload_type,
                })
            }
            TypedValueInner::Primitive(_) => ty.clone(),
        }
    }

    /// Compose this substitution with another
    pub fn compose(&mut self, other: &Substitution) {
        // Apply other to all values in self
        for value in self.mapping.values_mut() {
            *value = other.apply_type(value);
        }
        
        // Add mappings from other that aren't in self
        for (var, ty) in &other.mapping {
            if !self.mapping.contains_key(var) {
                self.mapping.insert(var.clone(), ty.clone());
            }
        }
    }

    /// Check if a variable occurs in the substitution
    pub fn contains_var(&self, var: &str) -> bool {
        self.mapping.contains_key(var)
    }
}

impl Default for Substitution {
    fn default() -> Self {
        Self::new()
    }
}

/// Unification errors
#[derive(Debug, Clone, thiserror::Error)]
pub enum UnificationError {
    #[error("Cannot unify {0} with {1}")]
    TypeMismatch(String, String),
    
    #[error("Occurs check failed: {0} occurs in {1}")]
    OccursCheck(String, String),
    
    #[error("Kind mismatch: expected {expected}, found {found}")]
    KindMismatch {
        expected: TypeKind,
        found: TypeKind,
    },
    
    #[error("Arity mismatch: expected {expected} arguments, found {found}")]
    ArityMismatch {
        expected: usize,
        found: usize,
    },
    
    #[error("Record field mismatch")]
    RecordFieldMismatch,
    
    #[error("Variant mismatch")]
    VariantMismatch,
}

/// Type unifier
pub struct Unifier {
    /// Current substitution
    subst: Substitution,
}

impl Unifier {
    /// Create a new unifier
    pub fn new() -> Self {
        Self {
            subst: Substitution::new(),
        }
    }

    /// Unify two types, returning the resulting substitution
    pub fn unify(&mut self, t1: &TypedValue, t2: &TypedValue) -> Result<Substitution, UnificationError> {
        // Apply current substitution to both types
        let t1 = self.subst.apply_type(t1);
        let t2 = self.subst.apply_type(t2);

        match (&t1.inner, &t2.inner) {
            // Same primitive types unify
            (TypedValueInner::Primitive(p1), TypedValueInner::Primitive(p2)) => {
                if p1 == p2 {
                    Ok(Substitution::new())
                } else {
                    Err(UnificationError::TypeMismatch(
                        t1.to_string(),
                        t2.to_string(),
                    ))
                }
            }

            // Variable unifies with anything (with occurs check)
            (TypedValueInner::Variable(v), _) => {
                if self.occurs_check(&v.name, &t2) {
                    Err(UnificationError::OccursCheck(
                        v.name.clone(),
                        t2.to_string(),
                    ))
                } else {
                    let mut subst = Substitution::new();
                    subst.insert(v.name.clone(), t2);
                    self.subst.compose(&subst);
                    Ok(subst)
                }
            }
            (_, TypedValueInner::Variable(v)) => {
                if self.occurs_check(&v.name, &t1) {
                    Err(UnificationError::OccursCheck(
                        v.name.clone(),
                        t1.to_string(),
                    ))
                } else {
                    let mut subst = Substitution::new();
                    subst.insert(v.name.clone(), t1);
                    self.subst.compose(&subst);
                    Ok(subst)
                }
            }

            // Function types
            (TypedValueInner::Function(f1), TypedValueInner::Function(f2)) => {
                if f1.params.len() != f2.params.len() {
                    return Err(UnificationError::ArityMismatch {
                        expected: f1.params.len(),
                        found: f2.params.len(),
                    });
                }

                let mut result_subst = Substitution::new();

                // Unify parameters
                for (p1, p2) in f1.params.iter().zip(&f2.params) {
                    let param_subst = self.unify(p1, p2)?;
                    result_subst.compose(&param_subst);
                }

                // Unify results
                let result_subst2 = self.unify(&f1.result, &f2.result)?;
                result_subst.compose(&result_subst2);

                Ok(result_subst)
            }

            // Tuple types
            (TypedValueInner::Tuple(t1), TypedValueInner::Tuple(t2)) => {
                if t1.elements.len() != t2.elements.len() {
                    return Err(UnificationError::ArityMismatch {
                        expected: t1.elements.len(),
                        found: t2.elements.len(),
                    });
                }

                let mut result_subst = Substitution::new();

                for (e1, e2) in t1.elements.iter().zip(&t2.elements) {
                    let elem_subst = self.unify(e1, e2)?;
                    result_subst.compose(&elem_subst);
                }

                Ok(result_subst)
            }

            // List types
            (TypedValueInner::List(l1), TypedValueInner::List(l2)) => {
                self.unify(&l1.element_type, &l2.element_type)
            }

            // Record types
            (TypedValueInner::Record(r1), TypedValueInner::Record(r2)) => {
                // Check that all fields match
                if r1.fields.len() != r2.fields.len() {
                    return Err(UnificationError::RecordFieldMismatch);
                }

                let mut result_subst = Substitution::new();

                for (name, ty1) in &r1.fields {
                    if let Some(ty2) = r2.fields.get(name) {
                        let field_subst = self.unify(ty1, ty2)?;
                        result_subst.compose(&field_subst);
                    } else {
                        return Err(UnificationError::RecordFieldMismatch);
                    }
                }

                Ok(result_subst)
            }

            // Variant types
            (TypedValueInner::Variant(v1), TypedValueInner::Variant(v2)) => {
                if v1.variants.len() != v2.variants.len() {
                    return Err(UnificationError::VariantMismatch);
                }

                let mut result_subst = Substitution::new();

                for (tag, payload1) in &v1.variants {
                    if let Some(payload2) = v2.variants.get(tag) {
                        match (payload1, payload2) {
                            (Some(p1), Some(p2)) => {
                                let payload_subst = self.unify(p1, p2)?;
                                result_subst.compose(&payload_subst);
                            }
                            (None, None) => {}
                            _ => return Err(UnificationError::VariantMismatch),
                        }
                    } else {
                        return Err(UnificationError::VariantMismatch);
                    }
                }

                Ok(result_subst)
            }

            // Uncertain types
            (TypedValueInner::Uncertain(u1), TypedValueInner::Uncertain(u2)) => {
                // For now, just unify base types
                // Could also check confidence/distribution compatibility
                self.unify(&u1.base_type, &u2.base_type)
            }

            // Temporal types
            (TypedValueInner::Temporal(t1), TypedValueInner::Temporal(t2)) => {
                // For now, just unify base types
                // Could also check constraint compatibility
                self.unify(&t1.base_type, &t2.base_type)
            }

            // Effect types
            (TypedValueInner::Effect(e1), TypedValueInner::Effect(e2)) => {
                if e1.effect_kind != e2.effect_kind {
                    return Err(UnificationError::TypeMismatch(
                        t1.to_string(),
                        t2.to_string(),
                    ));
                }

                match (&e1.payload_type, &e2.payload_type) {
                    (Some(p1), Some(p2)) => self.unify(p1, p2),
                    (None, None) => Ok(Substitution::new()),
                    _ => Err(UnificationError::TypeMismatch(
                        t1.to_string(),
                        t2.to_string(),
                    )),
                }
            }

            // Different type kinds don't unify
            _ => Err(UnificationError::KindMismatch {
                expected: t1.kind(),
                found: t2.kind(),
            }),
        }
    }

    /// Occurs check - ensure variable doesn't occur in type
    fn occurs_check(&self, var: &str, ty: &TypedValue) -> bool {
        match &ty.inner {
            TypedValueInner::Variable(v) => v.name == var,
            TypedValueInner::Function(f) => {
                f.params.iter().any(|p| self.occurs_check(var, p)) ||
                self.occurs_check(var, &f.result)
            }
            TypedValueInner::Tuple(t) => {
                t.elements.iter().any(|e| self.occurs_check(var, e))
            }
            TypedValueInner::List(l) => self.occurs_check(var, &l.element_type),
            TypedValueInner::Record(r) => {
                r.fields.values().any(|f| self.occurs_check(var, f))
            }
            TypedValueInner::Variant(v) => {
                v.variants.values().any(|p| {
                    p.as_ref().map_or(false, |ty| self.occurs_check(var, ty))
                })
            }
            TypedValueInner::Uncertain(u) => self.occurs_check(var, &u.base_type),
            TypedValueInner::Temporal(t) => self.occurs_check(var, &t.base_type),
            TypedValueInner::Effect(e) => {
                e.payload_type.as_ref()
                    .map_or(false, |p| self.occurs_check(var, p))
            }
            TypedValueInner::Primitive(_) => false,
        }
    }

    /// Get the current substitution
    pub fn substitution(&self) -> &Substitution {
        &self.subst
    }
}

impl Default for Unifier {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unify_primitives() {
        let mut unifier = Unifier::new();
        
        let int1 = TypedValue::primitive(PrimitiveType::int());
        let int2 = TypedValue::primitive(PrimitiveType::int());
        let string = TypedValue::primitive(PrimitiveType::string());
        
        // Same types unify
        assert!(unifier.unify(&int1, &int2).is_ok());
        
        // Different types don't unify
        assert!(unifier.unify(&int1, &string).is_err());
    }

    #[test]
    fn test_unify_variables() {
        let mut unifier = Unifier::new();
        
        let var = TypedValue::variable(TypeVariable::new("T"));
        let int = TypedValue::primitive(PrimitiveType::int());
        
        // Variable unifies with concrete type
        let subst = unifier.unify(&var, &int).unwrap();
        let result = subst.apply_type(&var);
        
        match &result.inner {
            TypedValueInner::Primitive(p) => assert_eq!(p.name, "Int"),
            _ => panic!("Expected primitive type"),
        }
    }

    #[test]
    fn test_unify_functions() {
        let mut unifier = Unifier::new();
        
        let int = TypedValue::primitive(PrimitiveType::int());
        let bool = TypedValue::primitive(PrimitiveType::bool());
        
        let f1 = TypedValue::function(FunctionType::new(
            vec![int.clone(), int.clone()],
            bool.clone(),
        ));
        
        let f2 = TypedValue::function(FunctionType::new(
            vec![int.clone(), int.clone()],
            bool.clone(),
        ));
        
        // Same function types unify
        assert!(unifier.unify(&f1, &f2).is_ok());
        
        let f3 = TypedValue::function(FunctionType::new(
            vec![int.clone()],
            bool.clone(),
        ));
        
        // Different arities don't unify
        assert!(unifier.unify(&f1, &f3).is_err());
    }

    #[test]
    fn test_unify_lists() {
        let mut unifier = Unifier::new();
        
        let int_list = TypedValue::list(ListType::new(
            TypedValue::primitive(PrimitiveType::int())
        ));
        
        let var = TypedValue::variable(TypeVariable::new("T"));
        let var_list = TypedValue::list(ListType::new(var));
        
        // List of variables unifies with list of ints
        let subst = unifier.unify(&var_list, &int_list).unwrap();
        let result = subst.apply_type(&var_list);
        
        assert_eq!(result.to_string(), "[Int]");
    }

    #[test]
    fn test_occurs_check() {
        let mut unifier = Unifier::new();
        
        let var = TypedValue::variable(TypeVariable::new("T"));
        let recursive = TypedValue::list(ListType::new(var.clone()));
        
        // Should fail occurs check
        assert!(unifier.unify(&var, &recursive).is_err());
    }
}