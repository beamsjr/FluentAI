//! Effect system types and tracking

use crate::ast::EffectType;
use std::collections::HashSet;

/// Set of effects that may be triggered by an expression
#[derive(Debug, Clone, Default)]
pub struct EffectSet {
    effects: HashSet<EffectType>,
}

impl EffectSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn pure() -> Self {
        let mut set = Self::new();
        set.add(EffectType::Pure);
        set
    }

    pub fn add(&mut self, effect: EffectType) {
        // If we add any non-pure effect, remove Pure
        if effect != EffectType::Pure && self.effects.contains(&EffectType::Pure) {
            self.effects.remove(&EffectType::Pure);
        }
        self.effects.insert(effect);
    }

    pub fn union(&mut self, other: &EffectSet) {
        for effect in &other.effects {
            self.add(*effect);
        }
    }

    pub fn is_pure(&self) -> bool {
        self.effects.len() == 1 && self.effects.contains(&EffectType::Pure)
    }

    pub fn contains(&self, effect: EffectType) -> bool {
        self.effects.contains(&effect)
    }

    pub fn iter(&self) -> impl Iterator<Item = &EffectType> {
        self.effects.iter()
    }
}

/// Effect handler trait
pub trait EffectHandler: Send + Sync {
    fn handle_io(&self, operation: &str, args: &[crate::value::Value]) -> crate::Result<crate::value::Value>;
    fn handle_state(&self, operation: &str, args: &[crate::value::Value]) -> crate::Result<crate::value::Value>;
    fn handle_error(&self, operation: &str, args: &[crate::value::Value]) -> crate::Result<crate::value::Value>;
}

/// Default effect handler implementation
pub struct DefaultEffectHandler;

impl EffectHandler for DefaultEffectHandler {
    fn handle_io(&self, operation: &str, args: &[crate::value::Value]) -> crate::Result<crate::value::Value> {
        match operation {
            "print" => {
                if let Some(arg) = args.first() {
                    println!("{}", arg);
                }
                Ok(crate::value::Value::Nil)
            }
            _ => Err(crate::error::Error::UnknownEffect(format!("IO:{}", operation))),
        }
    }

    fn handle_state(&self, _operation: &str, _args: &[crate::value::Value]) -> crate::Result<crate::value::Value> {
        Err(crate::error::Error::UnknownEffect("State operations not implemented".to_string()))
    }

    fn handle_error(&self, operation: &str, args: &[crate::value::Value]) -> crate::Result<crate::value::Value> {
        match operation {
            "raise" => {
                if let Some(msg) = args.first() {
                    Err(crate::error::Error::Runtime(msg.to_string()))
                } else {
                    Err(crate::error::Error::Runtime("Error raised".to_string()))
                }
            }
            _ => Err(crate::error::Error::UnknownEffect(format!("Error:{}", operation))),
        }
    }
}